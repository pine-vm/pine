module LanguageService exposing (..)

import Common
import CompilationInterface.SourceFiles
import CompileFullstackApp
import Elm.Syntax.Declaration
import Elm.Syntax.Exposing
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
    , coreModulesCache : List ParsedModuleCache
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
    { fileTreeParseCache = FileTree.TreeNode []
    , coreModulesCache = coreModulesParseResults
    }


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

                        implicitlyImportedModules =
                            languageServiceState.coreModulesCache
                                |> List.map
                                    (\coreModule ->
                                        let
                                            canonicalName =
                                                Elm.Syntax.Module.moduleName (Elm.Syntax.Node.value coreModule.syntax.moduleDefinition)
                                        in
                                        { canonicalName = canonicalName
                                        , importedName = canonicalName
                                        , parsedModule = Just coreModule
                                        }
                                    )

                        explicitlyImportedModules =
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

                        importedModules =
                            implicitlyImportedModules ++ explicitlyImportedModules

                        localDeclarations =
                            completionItemsFromModule parsedFileLastSuccess
                                |> List.map .completionItem

                        importExposings =
                            parsedFileLastSuccess.syntax.imports
                                |> List.map Elm.Syntax.Node.value
                                |> List.concatMap
                                    (\importSyntax ->
                                        case importSyntax.exposingList of
                                            Nothing ->
                                                []

                                            Just exposingList ->
                                                let
                                                    canonicalName =
                                                        Elm.Syntax.Node.value importSyntax.moduleName
                                                in
                                                case
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
                                                of
                                                    Nothing ->
                                                        []

                                                    Just importedParsedModule ->
                                                        let
                                                            importedModuleItems =
                                                                completionItemsFromModule importedParsedModule
                                                                    |> List.filter .isExposed
                                                                    |> List.map .completionItem
                                                        in
                                                        case Elm.Syntax.Node.value exposingList of
                                                            Elm.Syntax.Exposing.All _ ->
                                                                importedModuleItems

                                                            Elm.Syntax.Exposing.Explicit topLevelExposings ->
                                                                topLevelExposings
                                                                    |> List.concatMap
                                                                        (\topLevelExpose ->
                                                                            let
                                                                                exposedName =
                                                                                    case Elm.Syntax.Node.value topLevelExpose of
                                                                                        Elm.Syntax.Exposing.InfixExpose name ->
                                                                                            name

                                                                                        Elm.Syntax.Exposing.FunctionExpose name ->
                                                                                            name

                                                                                        Elm.Syntax.Exposing.TypeOrAliasExpose name ->
                                                                                            name

                                                                                        Elm.Syntax.Exposing.TypeExpose typeExpose ->
                                                                                            typeExpose.name
                                                                            in
                                                                            importedModuleItems
                                                                                |> List.filter (.insertText >> (==) exposedName)
                                                                        )
                                    )

                        localDeclarationsAndImportExposings =
                            localDeclarations ++ importExposings

                        localDeclarationsAfterPrefix =
                            if completionPrefix == [] then
                                localDeclarationsAndImportExposings

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
                                            |> List.filter .isExposed
                                            |> List.map .completionItem

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
                                            documentationStringFromSyntax =
                                                case importedModule.parsedModule of
                                                    Nothing ->
                                                        Nothing

                                                    Just importedParsedModule ->
                                                        let
                                                            moduleDefinitionRange =
                                                                Elm.Syntax.Node.range importedParsedModule.syntax.moduleDefinition

                                                            importsAndDeclarationsRange =
                                                                List.map Elm.Syntax.Node.range importedParsedModule.syntax.imports
                                                                    ++ List.map Elm.Syntax.Node.range importedParsedModule.syntax.declarations
                                                                    |> Elm.Syntax.Range.combine

                                                            maybeModuleComment =
                                                                importedParsedModule.syntax.comments
                                                                    |> List.filter
                                                                        (\comment ->
                                                                            (Elm.Syntax.Node.range comment).start.row
                                                                                > moduleDefinitionRange.start.row
                                                                                && (Elm.Syntax.Node.range comment).start.row
                                                                                < importsAndDeclarationsRange.start.row
                                                                        )
                                                                    |> List.sortBy (Elm.Syntax.Node.range >> .start >> .row)
                                                                    |> List.head
                                                        in
                                                        maybeModuleComment
                                                            |> Maybe.map (Elm.Syntax.Node.value >> removeWrappingFromMultilineComment)

                                            insertText =
                                                String.join "." importedModuleNameRestAfterPrefix
                                        in
                                        { label =
                                            if importedModule.importedName == importedModule.canonicalName then
                                                insertText

                                            else
                                                String.join "." importedModule.canonicalName ++ " as " ++ insertText
                                        , documentation = Maybe.withDefault "" documentationStringFromSyntax
                                        , insertText = insertText
                                        , kind = FrontendWeb.MonacoEditor.ModuleCompletionItemKind
                                        }
                                    )
                    in
                    if completionPrefixIsNamespace then
                        fromImports ++ List.sortBy .insertText localDeclarationsAfterPrefix

                    else
                        []


completionItemsFromModule : ParsedModuleCache -> List { completionItem : FrontendWeb.MonacoEditor.MonacoCompletionItem, isExposed : Bool }
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
            (String.join "\n" ("```Elm" :: codeLines ++ [ "```" ])
                :: Maybe.withDefault [] (Maybe.map List.singleton maybeDocumentation)
            )
                |> String.join "\n\n"

        exposingList =
            Elm.Syntax.Node.value
                (case Elm.Syntax.Node.value moduleCache.syntax.moduleDefinition of
                    Elm.Syntax.Module.EffectModule effectModule ->
                        effectModule.exposingList

                    Elm.Syntax.Module.NormalModule normalModule ->
                        normalModule.exposingList

                    Elm.Syntax.Module.PortModule portModule ->
                        portModule.exposingList
                )

        exposesFunction functionName =
            Elm.Syntax.Exposing.exposesFunction functionName exposingList

        exposesTypeOrAlias name =
            case exposingList of
                Elm.Syntax.Exposing.All _ ->
                    True

                Elm.Syntax.Exposing.Explicit topLovelExposings ->
                    topLovelExposings
                        |> List.map Elm.Syntax.Node.value
                        |> List.any
                            (\topLevelExpose ->
                                case topLevelExpose of
                                    Elm.Syntax.Exposing.TypeOrAliasExpose exposedName ->
                                        exposedName == name

                                    Elm.Syntax.Exposing.TypeExpose typeExpose ->
                                        typeExpose.name == name

                                    Elm.Syntax.Exposing.InfixExpose _ ->
                                        False

                                    Elm.Syntax.Exposing.FunctionExpose functionName ->
                                        name == functionName
                            )
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
                        [ { completionItem =
                                { label = functionName
                                , documentation = documentationMarkdownFromCodeLinesAndDocumentation codeLines documentationStringFromSyntax
                                , insertText = functionName
                                , kind = FrontendWeb.MonacoEditor.FunctionCompletionItemKind
                                }
                          , isExposed = exposesFunction functionName
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
                        [ { completionItem =
                                { label = aliasName
                                , documentation = documentationMarkdownFromCodeLinesAndDocumentation codeLines documentationStringFromSyntax
                                , insertText = aliasName
                                , kind = FrontendWeb.MonacoEditor.StructCompletionItemKind
                                }
                          , isExposed = exposesTypeOrAlias aliasName
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
                                            { completionItem =
                                                { label = tagName
                                                , documentation = "`" ++ tagName ++ "` is a variant of `" ++ customTypeName ++ "`"
                                                , insertText = tagName
                                                , kind = FrontendWeb.MonacoEditor.EnumMemberCompletionItemKind
                                                }
                                            , isExposed = exposesTypeOrAlias customTypeName
                                            }
                                        )
                        in
                        { completionItem =
                            { label = customTypeName
                            , documentation = documentationMarkdownFromCodeLinesAndDocumentation codeLines documentationStringFromSyntax
                            , insertText = customTypeName
                            , kind = FrontendWeb.MonacoEditor.EnumCompletionItemKind
                            }
                        , isExposed = exposesTypeOrAlias customTypeName
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
    { state
        | fileTreeParseCache = fileTree |> FileTree.mapBlobsWithPath compileFileCacheEntry
    }


coreModulesTexts : List String
coreModulesTexts =
    [ CompilationInterface.SourceFiles.file__utf8____elm_core_modules_List_elm
    , CompilationInterface.SourceFiles.file__utf8____elm_core_modules_String_elm
    , CompilationInterface.SourceFiles.file__utf8____elm_core_modules_Maybe_elm
    , CompilationInterface.SourceFiles.file__utf8____elm_core_modules_Result_elm
    , CompilationInterface.SourceFiles.file__utf8____elm_core_modules_Tuple_elm
    ]


coreModulesParseResults : List ParsedModuleCache
coreModulesParseResults =
    coreModulesTexts
        |> List.filterMap
            (\asString ->
                asString
                    |> CompileFullstackApp.parseElmModuleText
                    |> Result.toMaybe
                    |> Maybe.map (\syntax -> { text = asString, syntax = syntax })
            )


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
