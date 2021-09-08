module LanguageService exposing (..)

import Common
import CompileFullstackApp
import Elm.Syntax.Declaration
import Elm.Syntax.File
import Elm.Syntax.Module
import Elm.Syntax.Node
import Elm.Syntax.Range
import FileTree
import FileTreeInWorkspace
import FrontendWeb.MonacoEditor
import Maybe.Extra


type alias LanguageServiceState =
    { fileTreeParseCache : FileTree.FileTreeNode LanguageServiceStateFileTreeNodeBlob
    }


type alias LanguageServiceStateFileTreeNodeBlob =
    { {- Avoid bug in Elm core library as reported at https://github.com/elm/bytes/issues/15 :
         Convert to other representation before comparing.
      -}
      sourceBase64 : String
    , parsedFile : Maybe ParsedModuleCache
    , parsedFileLastSuccess : Maybe ParsedModuleCache
    }


type alias ParsedModuleCache =
    { text : String
    , syntax : Elm.Syntax.File.File
    }


initLanguageServiceState : LanguageServiceState
initLanguageServiceState =
    { fileTreeParseCache = FileTree.TreeNode [] }


provideCompletionItems :
    { filePathOpenedInEditor : List String, textUntilPosition : String }
    -> LanguageServiceState
    -> List FrontendWeb.MonacoEditor.MonacoCompletionItem
provideCompletionItems request languageServiceState =
    case languageServiceState.fileTreeParseCache |> FileTree.getBlobAtPathFromFileTree request.filePathOpenedInEditor of
        Nothing ->
            []

        Just currentFileCacheItem ->
            case currentFileCacheItem.parsedFileLastSuccess of
                Nothing ->
                    []

                Just parsedFileLastSuccess ->
                    let
                        completionPrefix =
                            request.textUntilPosition
                                |> String.lines
                                |> List.reverse
                                |> List.head
                                |> Maybe.andThen (String.trim >> String.split " " >> List.reverse >> List.head)
                                |> Maybe.map (String.split "." >> List.reverse >> List.drop 1 >> List.reverse)
                                |> Maybe.withDefault []

                        completionPrefixIsNamespace =
                            case completionPrefix of
                                [] ->
                                    True

                                prefixFirstElement :: _ ->
                                    prefixFirstElement
                                        |> String.toList
                                        |> List.head
                                        |> Maybe.map Char.isUpper
                                        |> Maybe.withDefault True

                        importedModules =
                            parsedFileLastSuccess.syntax.imports
                                |> List.map Elm.Syntax.Node.value
                                |> List.map
                                    (\importSyntax ->
                                        let
                                            canonicalName =
                                                Elm.Syntax.Node.value importSyntax.moduleName

                                            importedName =
                                                importSyntax.moduleAlias
                                                    |> Maybe.map Elm.Syntax.Node.value
                                                    |> Maybe.withDefault canonicalName

                                            parsedModule =
                                                languageServiceState.fileTreeParseCache
                                                    |> FileTree.flatListOfBlobsFromFileTreeNode
                                                    |> List.filterMap
                                                        (\( _, fileCache ) ->
                                                            case fileCache.parsedFileLastSuccess of
                                                                Nothing ->
                                                                    Nothing

                                                                Just moduleCandidate ->
                                                                    if Elm.Syntax.Module.moduleName (Elm.Syntax.Node.value moduleCandidate.syntax.moduleDefinition) == canonicalName then
                                                                        Just moduleCandidate

                                                                    else
                                                                        Nothing
                                                        )
                                                    |> List.head
                                        in
                                        { canonicalName = canonicalName
                                        , importedName = importedName
                                        , parsedModule = parsedModule
                                        }
                                    )

                        localDeclarations =
                            completionItemsFromModule parsedFileLastSuccess

                        localDeclarationsAfterPrefix =
                            if completionPrefix == [] then
                                localDeclarations

                            else
                                case
                                    importedModules
                                        |> List.filter (.importedName >> (==) completionPrefix)
                                        |> List.head
                                        |> Maybe.andThen .parsedModule
                                of
                                    Nothing ->
                                        []

                                    Just referencedModule ->
                                        completionItemsFromModule referencedModule

                        importedModulesAfterPrefix =
                            importedModules
                                |> List.filterMap
                                    (\importedModule ->
                                        if List.take (List.length completionPrefix) importedModule.importedName == completionPrefix then
                                            case List.drop (List.length completionPrefix) importedModule.importedName of
                                                [] ->
                                                    Nothing

                                                restAfterPrefix ->
                                                    Just ( restAfterPrefix, importedModule )

                                        else
                                            Nothing
                                    )

                        fromImports =
                            importedModulesAfterPrefix
                                |> List.map
                                    (\( importedModuleNameRestAfterPrefix, importedModule ) ->
                                        let
                                            insertText =
                                                String.join "." importedModuleNameRestAfterPrefix
                                        in
                                        { label =
                                            if importedModule.importedName == importedModule.canonicalName then
                                                insertText

                                            else
                                                String.join "." importedModule.canonicalName ++ " as " ++ insertText
                                        , documentation = ""
                                        , insertText = insertText
                                        , kind = FrontendWeb.MonacoEditor.ModuleCompletionItemKind
                                        }
                                    )
                    in
                    if completionPrefixIsNamespace then
                        fromImports ++ List.sortBy .insertText localDeclarationsAfterPrefix

                    else
                        []


completionItemsFromModule : ParsedModuleCache -> List FrontendWeb.MonacoEditor.MonacoCompletionItem
completionItemsFromModule moduleCache =
    let
        textLines =
            String.lines moduleCache.text

        getTextLinesFromRange range =
            textLines
                |> List.take range.end.row
                |> List.drop (range.start.row - 1)
                |> List.reverse
                |> listMapFirstElement (String.left (range.end.column - 1))
                |> List.reverse
                |> listMapFirstElement (String.dropLeft (range.start.column - 1))

        documentationMarkdownFromCodeLinesAndDocumentation codeLines maybeDocumentation =
            ([ String.join "\n"
                ("```Elm" :: codeLines ++ [ "```" ])
             ]
                ++ Maybe.withDefault [] (Maybe.map List.singleton maybeDocumentation)
            )
                |> String.join "\n\n"
    in
    moduleCache.syntax.declarations
        |> List.concatMap
            (\declaration ->
                case Elm.Syntax.Node.value declaration of
                    Elm.Syntax.Declaration.FunctionDeclaration functionDeclaration ->
                        let
                            functionName =
                                Elm.Syntax.Node.value (Elm.Syntax.Node.value functionDeclaration.declaration).name

                            documentationStringFromSyntax =
                                functionDeclaration.documentation
                                    |> Maybe.map (Elm.Syntax.Node.value >> removeWrappingFromMultilineComment)

                            codeSignatureLines =
                                functionDeclaration.signature
                                    |> Maybe.map (Elm.Syntax.Node.range >> getTextLinesFromRange)
                                    |> Maybe.withDefault []

                            codeLines =
                                codeSignatureLines
                        in
                        [ { label = functionName
                          , documentation = documentationMarkdownFromCodeLinesAndDocumentation codeLines documentationStringFromSyntax
                          , insertText = functionName
                          , kind = FrontendWeb.MonacoEditor.FunctionCompletionItemKind
                          }
                        ]

                    Elm.Syntax.Declaration.AliasDeclaration aliasDeclaration ->
                        let
                            aliasName =
                                Elm.Syntax.Node.value aliasDeclaration.name

                            documentationStringFromSyntax =
                                aliasDeclaration.documentation
                                    |> Maybe.map (Elm.Syntax.Node.value >> removeWrappingFromMultilineComment)

                            codeRange =
                                [ Elm.Syntax.Node.range aliasDeclaration.name
                                , Elm.Syntax.Node.range aliasDeclaration.typeAnnotation
                                ]
                                    |> Elm.Syntax.Range.combine
                                    |> expandRangeToLineStart

                            codeLines =
                                getTextLinesFromRange codeRange
                        in
                        [ { label = aliasName
                          , documentation = documentationMarkdownFromCodeLinesAndDocumentation codeLines documentationStringFromSyntax
                          , insertText = aliasName
                          , kind = FrontendWeb.MonacoEditor.ConstructorCompletionItemKind
                          }
                        ]

                    Elm.Syntax.Declaration.CustomTypeDeclaration customTypeDeclaration ->
                        let
                            documentationStringFromSyntax =
                                customTypeDeclaration.documentation
                                    |> Maybe.map (Elm.Syntax.Node.value >> removeWrappingFromMultilineComment)

                            codeRange =
                                Elm.Syntax.Node.range customTypeDeclaration.name
                                    :: List.map Elm.Syntax.Node.range customTypeDeclaration.constructors
                                    |> Elm.Syntax.Range.combine
                                    |> expandRangeToLineStart

                            codeLines =
                                getTextLinesFromRange codeRange

                            customTypeName =
                                Elm.Syntax.Node.value customTypeDeclaration.name

                            fromTags =
                                customTypeDeclaration.constructors
                                    |> List.map
                                        (\constructorNode ->
                                            let
                                                tagName =
                                                    Elm.Syntax.Node.value
                                                        (Elm.Syntax.Node.value constructorNode).name
                                            in
                                            { label = tagName
                                            , documentation = "`" ++ tagName ++ "` is a variant of `" ++ customTypeName ++ "`"
                                            , insertText = tagName
                                            , kind = FrontendWeb.MonacoEditor.EnumMemberCompletionItemKind
                                            }
                                        )
                        in
                        { label = customTypeName
                        , documentation = documentationMarkdownFromCodeLinesAndDocumentation codeLines documentationStringFromSyntax
                        , insertText = customTypeName
                        , kind = FrontendWeb.MonacoEditor.EnumCompletionItemKind
                        }
                            :: fromTags

                    Elm.Syntax.Declaration.Destructuring _ _ ->
                        []

                    Elm.Syntax.Declaration.PortDeclaration _ ->
                        []

                    Elm.Syntax.Declaration.InfixDeclaration _ ->
                        []
            )


updateLanguageServiceState : FileTreeInWorkspace.FileTreeNode -> LanguageServiceState -> LanguageServiceState
updateLanguageServiceState fileTree state =
    let
        compileFileCacheEntry ( blobPath, fileTreeBlob ) =
            let
                maybePreviousCached =
                    state.fileTreeParseCache |> FileTree.getBlobAtPathFromFileTree blobPath

                buildNewEntry _ =
                    let
                        parsedFile =
                            fileTreeBlob.asBytes
                                |> Common.decodeBytesToString
                                |> Maybe.andThen
                                    (\asString ->
                                        asString
                                            |> CompileFullstackApp.parseElmModuleText
                                            |> Result.toMaybe
                                            |> Maybe.map (\syntax -> { text = asString, syntax = syntax })
                                    )
                    in
                    { sourceBase64 = fileTreeBlob.asBase64
                    , parsedFile = parsedFile
                    , parsedFileLastSuccess =
                        Maybe.Extra.or parsedFile (Maybe.andThen .parsedFileLastSuccess maybePreviousCached)
                    }
            in
            case maybePreviousCached of
                Nothing ->
                    buildNewEntry ()

                Just previousCached ->
                    if previousCached.sourceBase64 == fileTreeBlob.asBase64 then
                        previousCached

                    else
                        buildNewEntry ()
    in
    { fileTreeParseCache = fileTree |> FileTree.mapBlobsWithPath compileFileCacheEntry }


expandRangeToLineStart : Elm.Syntax.Range.Range -> Elm.Syntax.Range.Range
expandRangeToLineStart range =
    let
        start =
            range.start
    in
    { range | start = { start | column = 1 } }


removeWrappingFromMultilineComment : String -> String
removeWrappingFromMultilineComment withWrapping =
    let
        trimmed =
            String.trim withWrapping

        lessPrefix =
            String.trimLeft
                (if String.startsWith "{-|" trimmed then
                    String.dropLeft 3 trimmed

                 else if String.startsWith "{-" trimmed then
                    String.dropLeft 2 trimmed

                 else
                    trimmed
                )
    in
    String.trimRight
        (if String.endsWith "-}" lessPrefix then
            String.dropRight 2 lessPrefix

         else
            lessPrefix
        )


listMapFirstElement : (a -> a) -> List a -> List a
listMapFirstElement mapElement list =
    case list of
        firstElement :: followingElements ->
            mapElement firstElement :: followingElements

        _ ->
            list
