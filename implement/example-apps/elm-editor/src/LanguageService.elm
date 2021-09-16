module LanguageService exposing (..)

import Common
import CompilationInterface.SourceFiles
import CompileFullstackApp
import Elm.Syntax.Declaration
import Elm.Syntax.Exposing
import Elm.Syntax.Expression
import Elm.Syntax.File
import Elm.Syntax.Module
import Elm.Syntax.Node
import Elm.Syntax.Range
import FileTree
import FileTreeInWorkspace
import Frontend.MonacoEditor
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
    { filePathOpenedInEditor : List String, cursorLineNumber : Int, textUntilPosition : String }
    -> LanguageServiceState
    -> List Frontend.MonacoEditor.MonacoCompletionItem
provideCompletionItems request languageServiceState =
    case languageServiceState.fileTreeParseCache |> FileTree.getBlobAtPathFromFileTree request.filePathOpenedInEditor of
        Nothing ->
            []

        Just currentFileCacheItem ->
            case currentFileCacheItem.parsedFileLastSuccess of
                Nothing ->
                    []

                Just fileOpenedInEditor ->
                    let
                        fileOpenedInEditorModuleName =
                            Elm.Syntax.Module.moduleName (Elm.Syntax.Node.value fileOpenedInEditor.syntax.moduleDefinition)

                        lineUntilPosition =
                            request.textUntilPosition
                                |> String.lines
                                |> List.reverse
                                |> List.head
                                |> Maybe.withDefault ""

                        lineUntilPositionWords =
                            stringSplitByChar (\c -> not (charIsAllowedInDeclarationName c || c == '.')) lineUntilPosition

                        completionPrefix =
                            lineUntilPositionWords
                                |> List.reverse
                                |> List.head
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
                            fileOpenedInEditor.syntax.imports
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

                        moduleNamesToNotSuggestForImport =
                            [ fileOpenedInEditorModuleName ]

                        modulesToSuggestForImport =
                            languageServiceState.fileTreeParseCache
                                |> FileTree.flatListOfBlobsFromFileTreeNode
                                |> List.filterMap (Tuple.second >> .parsedFileLastSuccess >> Maybe.map .syntax)
                                |> List.filter
                                    (\availableModule ->
                                        not
                                            (List.any
                                                ((==)
                                                    (Elm.Syntax.Module.moduleName (Elm.Syntax.Node.value availableModule.moduleDefinition))
                                                )
                                                moduleNamesToNotSuggestForImport
                                            )
                                    )
                                |> List.sortBy
                                    (.moduleDefinition
                                        >> Elm.Syntax.Node.value
                                        >> Elm.Syntax.Module.moduleName
                                        >> String.join "."
                                    )

                        importedModules =
                            implicitlyImportedModules ++ explicitlyImportedModules

                        currentModuleDeclarations =
                            completionItemsFromModule fileOpenedInEditor

                        fromLetBlocks =
                            currentModuleDeclarations.fromLetBlocks
                                |> List.filter
                                    (\fromLetBlock ->
                                        (fromLetBlock.scope.start.row <= request.cursorLineNumber)
                                            && (request.cursorLineNumber <= fromLetBlock.scope.end.row)
                                    )
                                |> List.map .completionItem

                        importExposings =
                            fileOpenedInEditor.syntax.imports
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
                                                                (completionItemsFromModule importedParsedModule).topLevel
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
                            List.map .completionItem currentModuleDeclarations.topLevel ++ importExposings ++ fromLetBlocks

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
                                        (completionItemsFromModule referencedModule).topLevel
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
                                |> List.filterMap
                                    (\( importedModuleNameRestAfterPrefix, importedModule ) ->
                                        importedModule.parsedModule
                                            |> Maybe.map
                                                (.syntax
                                                    >> moduleCompletionItemFromModuleSyntax
                                                        { importedName = Just importedModule.importedName
                                                        , importedModuleNameRestAfterPrefix = Just importedModuleNameRestAfterPrefix
                                                        }
                                                )
                                    )
                    in
                    if List.head lineUntilPositionWords == Just "import" then
                        modulesToSuggestForImport
                            |> List.map
                                (moduleCompletionItemFromModuleSyntax
                                    { importedModuleNameRestAfterPrefix = Nothing, importedName = Nothing }
                                )

                    else if completionPrefixIsNamespace then
                        fromImports ++ List.sortBy .insertText localDeclarationsAfterPrefix

                    else
                        []


moduleCompletionItemFromModuleSyntax :
    { importedModuleNameRestAfterPrefix : Maybe (List String), importedName : Maybe (List String) }
    -> Elm.Syntax.File.File
    -> Frontend.MonacoEditor.MonacoCompletionItem
moduleCompletionItemFromModuleSyntax { importedModuleNameRestAfterPrefix, importedName } moduleSyntax =
    let
        canonicalName =
            Elm.Syntax.Module.moduleName (Elm.Syntax.Node.value moduleSyntax.moduleDefinition)

        insertText =
            String.join "." (Maybe.withDefault canonicalName importedModuleNameRestAfterPrefix)
    in
    { label =
        if Maybe.withDefault canonicalName importedName == canonicalName then
            insertText

        else
            String.join "." canonicalName ++ " as " ++ insertText
    , documentation = Maybe.withDefault "" (documentationStringFromModuleSyntax moduleSyntax)
    , insertText = insertText
    , kind = Frontend.MonacoEditor.ModuleCompletionItemKind
    }


documentationStringFromModuleSyntax : Elm.Syntax.File.File -> Maybe String
documentationStringFromModuleSyntax parsedModule =
    let
        moduleDefinitionRange =
            Elm.Syntax.Node.range parsedModule.moduleDefinition

        importsAndDeclarationsRange =
            List.map Elm.Syntax.Node.range parsedModule.imports
                ++ List.map Elm.Syntax.Node.range parsedModule.declarations
                |> Elm.Syntax.Range.combine

        maybeModuleComment =
            parsedModule.comments
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


completionItemsFromModule :
    ParsedModuleCache
    ->
        { topLevel : List { completionItem : Frontend.MonacoEditor.MonacoCompletionItem, isExposed : Bool }
        , fromLetBlocks : List { completionItem : Frontend.MonacoEditor.MonacoCompletionItem, scope : Elm.Syntax.Range.Range }
        }
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

        fromLetBlocks =
            listLetBlocksInFile moduleCache.syntax
                |> List.concatMap
                    (\letBlock ->
                        (Elm.Syntax.Node.value letBlock).declarations
                            |> List.concatMap
                                (\letDeclaration ->
                                    case Elm.Syntax.Node.value letDeclaration of
                                        Elm.Syntax.Expression.LetFunction letFunction ->
                                            [ (completionItemFromFunctionSyntax letFunction).buildCompletionItem getTextLinesFromRange
                                            ]

                                        Elm.Syntax.Expression.LetDestructuring _ _ ->
                                            []
                                )
                            |> List.map
                                (\completionItem ->
                                    { completionItem = completionItem
                                    , scope = Elm.Syntax.Node.range letBlock
                                    }
                                )
                    )
    in
    { topLevel =
        moduleCache.syntax.declarations
            |> List.concatMap
                (\declaration ->
                    case Elm.Syntax.Node.value declaration of
                        Elm.Syntax.Declaration.FunctionDeclaration functionDeclaration ->
                            let
                                { functionName, buildCompletionItem } =
                                    completionItemFromFunctionSyntax functionDeclaration
                            in
                            [ { completionItem = buildCompletionItem getTextLinesFromRange
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
                                    , kind = Frontend.MonacoEditor.StructCompletionItemKind
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

                                                    documentation =
                                                        [ markdownElmCodeBlockFromCodeLines [ tagName ]
                                                        , "A variant of the union type `" ++ customTypeName ++ "`"
                                                        , markdownElmCodeBlockFromCodeLines codeLines
                                                        ]
                                                            |> String.join "\n\n"
                                                in
                                                { completionItem =
                                                    { label = tagName
                                                    , documentation = documentation
                                                    , insertText = tagName
                                                    , kind = Frontend.MonacoEditor.EnumMemberCompletionItemKind
                                                    }
                                                , isExposed = exposesTypeOrAlias customTypeName
                                                }
                                            )
                            in
                            { completionItem =
                                { label = customTypeName
                                , documentation = documentationMarkdownFromCodeLinesAndDocumentation codeLines documentationStringFromSyntax
                                , insertText = customTypeName
                                , kind = Frontend.MonacoEditor.EnumCompletionItemKind
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
    , fromLetBlocks = fromLetBlocks |> List.sortBy (.completionItem >> .label)
    }


completionItemFromFunctionSyntax : Elm.Syntax.Expression.Function -> { functionName : String, buildCompletionItem : (Elm.Syntax.Range.Range -> List String) -> Frontend.MonacoEditor.MonacoCompletionItem }
completionItemFromFunctionSyntax functionSyntax =
    let
        functionName =
            Elm.Syntax.Node.value (Elm.Syntax.Node.value functionSyntax.declaration).name
    in
    { functionName = functionName
    , buildCompletionItem =
        \getTextLinesFromRange ->
            let
                documentationStringFromSyntax =
                    functionSyntax.documentation
                        |> Maybe.map (Elm.Syntax.Node.value >> removeWrappingFromMultilineComment)

                codeSignatureLines =
                    functionSyntax.signature
                        |> Maybe.map (Elm.Syntax.Node.range >> getTextLinesFromRange)
                        |> Maybe.withDefault []

                codeLines =
                    codeSignatureLines
            in
            { label = functionName
            , documentation = documentationMarkdownFromCodeLinesAndDocumentation codeLines documentationStringFromSyntax
            , insertText = functionName
            , kind = Frontend.MonacoEditor.FunctionCompletionItemKind
            }
    }


documentationMarkdownFromCodeLinesAndDocumentation : List String -> Maybe String -> String
documentationMarkdownFromCodeLinesAndDocumentation codeLines maybeDocumentation =
    (markdownElmCodeBlockFromCodeLines codeLines
        :: Maybe.withDefault [] (Maybe.map List.singleton maybeDocumentation)
    )
        |> String.join "\n\n"


markdownElmCodeBlockFromCodeLines : List String -> String
markdownElmCodeBlockFromCodeLines codeLines =
    String.join "\n" ("```Elm" :: codeLines ++ [ "```" ])


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


listLetBlocksInFile : Elm.Syntax.File.File -> List (Elm.Syntax.Node.Node Elm.Syntax.Expression.LetBlock)
listLetBlocksInFile file =
    file.declarations
        |> List.concatMap (Elm.Syntax.Node.value >> listLetBlocksInDeclaration)


listLetBlocksInDeclaration : Elm.Syntax.Declaration.Declaration -> List (Elm.Syntax.Node.Node Elm.Syntax.Expression.LetBlock)
listLetBlocksInDeclaration declaration =
    case declaration of
        Elm.Syntax.Declaration.FunctionDeclaration function ->
            listLetBlocksInExpression (Elm.Syntax.Node.value function.declaration).expression

        Elm.Syntax.Declaration.AliasDeclaration _ ->
            []

        Elm.Syntax.Declaration.CustomTypeDeclaration _ ->
            []

        Elm.Syntax.Declaration.PortDeclaration _ ->
            []

        Elm.Syntax.Declaration.InfixDeclaration _ ->
            []

        Elm.Syntax.Declaration.Destructuring _ _ ->
            []


listLetBlocksInLetDeclaration : Elm.Syntax.Expression.LetDeclaration -> List (Elm.Syntax.Node.Node Elm.Syntax.Expression.LetBlock)
listLetBlocksInLetDeclaration declaration =
    case declaration of
        Elm.Syntax.Expression.LetFunction function ->
            listLetBlocksInExpression (Elm.Syntax.Node.value function.declaration).expression

        Elm.Syntax.Expression.LetDestructuring _ letDestructuring ->
            listLetBlocksInExpression letDestructuring


listLetBlocksInExpression : Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression -> List (Elm.Syntax.Node.Node Elm.Syntax.Expression.LetBlock)
listLetBlocksInExpression expressionNode =
    case Elm.Syntax.Node.value expressionNode of
        Elm.Syntax.Expression.UnitExpr ->
            []

        Elm.Syntax.Expression.Application application ->
            application |> List.concatMap listLetBlocksInExpression

        Elm.Syntax.Expression.OperatorApplication _ _ leftExpr rightExpr ->
            [ leftExpr, rightExpr ] |> List.concatMap listLetBlocksInExpression

        Elm.Syntax.Expression.FunctionOrValue _ _ ->
            []

        Elm.Syntax.Expression.IfBlock ifExpr thenExpr elseExpr ->
            [ ifExpr, thenExpr, elseExpr ] |> List.concatMap listLetBlocksInExpression

        Elm.Syntax.Expression.PrefixOperator _ ->
            []

        Elm.Syntax.Expression.Operator _ ->
            []

        Elm.Syntax.Expression.Integer _ ->
            []

        Elm.Syntax.Expression.Hex _ ->
            []

        Elm.Syntax.Expression.Floatable _ ->
            []

        Elm.Syntax.Expression.Negation negation ->
            listLetBlocksInExpression negation

        Elm.Syntax.Expression.Literal _ ->
            []

        Elm.Syntax.Expression.CharLiteral _ ->
            []

        Elm.Syntax.Expression.TupledExpression tupled ->
            tupled |> List.concatMap listLetBlocksInExpression

        Elm.Syntax.Expression.ParenthesizedExpression parenthesized ->
            listLetBlocksInExpression parenthesized

        Elm.Syntax.Expression.LetExpression letBlock ->
            List.concatMap (Elm.Syntax.Node.value >> listLetBlocksInLetDeclaration) letBlock.declarations
                ++ listLetBlocksInExpression letBlock.expression
                ++ [ Elm.Syntax.Node.Node (Elm.Syntax.Node.range expressionNode) letBlock ]

        Elm.Syntax.Expression.CaseExpression caseBlock ->
            listLetBlocksInExpression caseBlock.expression
                ++ (caseBlock.cases |> List.concatMap (Tuple.second >> listLetBlocksInExpression))

        Elm.Syntax.Expression.LambdaExpression lambda ->
            listLetBlocksInExpression lambda.expression

        Elm.Syntax.Expression.RecordExpr recordExpr ->
            recordExpr |> List.concatMap (Elm.Syntax.Node.value >> Tuple.second >> listLetBlocksInExpression)

        Elm.Syntax.Expression.ListExpr listExpr ->
            listExpr |> List.concatMap listLetBlocksInExpression

        Elm.Syntax.Expression.RecordAccess recordAccess _ ->
            listLetBlocksInExpression recordAccess

        Elm.Syntax.Expression.RecordAccessFunction _ ->
            []

        Elm.Syntax.Expression.RecordUpdateExpression _ recordUpdateExpression ->
            recordUpdateExpression |> List.concatMap (Elm.Syntax.Node.value >> Tuple.second >> listLetBlocksInExpression)

        Elm.Syntax.Expression.GLSLExpression _ ->
            []


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


charIsAllowedInDeclarationName : Char -> Bool
charIsAllowedInDeclarationName char =
    -- https://github.com/stil4m/elm-syntax/blob/8728aa02778780b1a9bba33a27ecf0a37300a4a0/src/Elm/Parser/Tokens.elm#L245
    Char.isAlphaNum char || char == '_'


listMapFirstElement : (a -> a) -> List a -> List a
listMapFirstElement mapElement list =
    case list of
        firstElement :: followingElements ->
            mapElement firstElement :: followingElements

        _ ->
            list


stringSplitByChar : (Char -> Bool) -> String -> List String
stringSplitByChar charSplits =
    String.toList >> listCharSplitByChar charSplits >> List.map String.fromList


listCharSplitByChar : (Char -> Bool) -> List Char -> List (List Char)
listCharSplitByChar charSplits =
    List.foldl
        (\char ( completed, current ) ->
            if charSplits char then
                ( List.reverse current :: completed, [] )

            else
                ( completed, char :: current )
        )
        ( [], [] )
        >> (\( completed, current ) -> List.reverse (List.reverse current :: completed))
