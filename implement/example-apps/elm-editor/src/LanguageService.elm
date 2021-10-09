module LanguageService exposing (..)

import Common
import CompilationInterface.SourceFiles
import CompileFullstackApp
import Elm.Syntax.Declaration
import Elm.Syntax.Exposing
import Elm.Syntax.Expression
import Elm.Syntax.File
import Elm.Syntax.Module
import Elm.Syntax.ModuleName
import Elm.Syntax.Node
import Elm.Syntax.Range
import Elm.Syntax.TypeAnnotation
import FileTree
import FileTreeInWorkspace
import Frontend.MonacoEditor
import Maybe.Extra


type alias LanguageServiceState =
    { fileTreeParseCache : FileTree.FileTreeNode LanguageServiceStateFileTreeNodeBlob
    , coreModulesCache : List ElmCoreModule
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


type alias ElmCoreModule =
    { parseResult : ParsedModuleCache
    , implicitImport : Bool
    }


initLanguageServiceState : LanguageServiceState
initLanguageServiceState =
    { fileTreeParseCache = FileTree.TreeNode []
    , coreModulesCache = elmCoreModulesParseResults
    }


provideHover :
    { filePathOpenedInEditor : List String, positionLineNumber : Int, positionColumn : Int, lineText : String }
    -> LanguageServiceState
    -> List String
provideHover request languageServiceState =
    case languageServiceState.fileTreeParseCache |> FileTree.getBlobAtPathFromFileTree request.filePathOpenedInEditor of
        Nothing ->
            []

        Just currentFileCacheItem ->
            case currentFileCacheItem.parsedFileLastSuccess of
                Nothing ->
                    []

                Just parsedFileLastSuccess ->
                    hoverItemsFromParsedModule parsedFileLastSuccess languageServiceState
                        |> List.filter
                            (\( hoverRange, _ ) ->
                                let
                                    hoverRangeLines =
                                        getTextLinesFromRangeAndText hoverRange parsedFileLastSuccess.text
                                            |> List.filter (String.isEmpty >> not)
                                in
                                rangeIntersectsLocation
                                    { row = request.positionLineNumber, column = request.positionColumn }
                                    hoverRange
                                    && List.all
                                        (\hoverRangeLine -> String.contains hoverRangeLine request.lineText)
                                        hoverRangeLines
                            )
                        |> List.map Tuple.second


hoverItemsFromParsedModule : ParsedModuleCache -> LanguageServiceState -> List ( Elm.Syntax.Range.Range, String )
hoverItemsFromParsedModule parsedModule languageServiceState =
    let
        importedModules =
            importedModulesFromFile parsedModule languageServiceState

        currentModuleDeclarations =
            completionItemsFromModule parsedModule

        importExposings =
            importExposingsFromFile parsedModule languageServiceState

        localDeclarationsAndImportExposings =
            List.map .completionItem currentModuleDeclarations.topLevel
                ++ importExposings

        getModuleByImportedName importedName =
            importedModules
                |> List.filter (.importedName >> (==) importedName)
                |> List.filterMap .parsedModule
                |> List.head

        fromImportSyntax =
            importedModules
                |> List.concatMap
                    (\importedModule ->
                        case importedModule.parsedModule of
                            Nothing ->
                                []

                            Just importedModuleParsed ->
                                importedModule.referencesRanges
                                    |> List.map
                                        (\range ->
                                            ( range
                                            , moduleCompletionItemFromModuleSyntax
                                                { importedModuleNameRestAfterPrefix = Nothing, importedName = Just importedModule.importedName }
                                                importedModuleParsed.syntax
                                            )
                                        )
                    )
                |> List.map (Tuple.mapSecond .documentation)

        getHoverForFunctionOrName : ( Elm.Syntax.ModuleName.ModuleName, String ) -> Maybe String
        getHoverForFunctionOrName ( moduleName, nameInModule ) =
            let
                itemsBeforeFilteringByNameInModule =
                    if moduleName == [] then
                        localDeclarationsAndImportExposings

                    else
                        case getModuleByImportedName moduleName of
                            Nothing ->
                                []

                            Just referencedModule ->
                                (completionItemsFromModule referencedModule).topLevel
                                    |> List.filter .isExposed
                                    |> List.map .completionItem
            in
            itemsBeforeFilteringByNameInModule
                |> List.filter (.label >> (==) nameInModule)
                |> List.map .documentation
                |> List.head

        getForHoversForReferenceNode functionOrNameNode =
            let
                ( moduleName, nameInModule ) =
                    functionOrNameNode
                        |> Elm.Syntax.Node.value

                wholeRange =
                    Elm.Syntax.Node.range functionOrNameNode

                wholeRangeEnd =
                    wholeRange.end

                rangeModulePart =
                    { wholeRange
                        | end = { wholeRangeEnd | column = wholeRangeEnd.column - String.length nameInModule }
                    }

                forNameInModuleRange =
                    { wholeRange
                        | start = { wholeRangeEnd | column = wholeRangeEnd.column - String.length nameInModule }
                    }

                forModule =
                    if moduleName == [] then
                        []

                    else
                        case
                            importedModules
                                |> List.filter (.importedName >> (==) moduleName)
                                |> List.head
                        of
                            Nothing ->
                                []

                            Just referencedModule ->
                                case referencedModule.parsedModule of
                                    Nothing ->
                                        []

                                    Just referencedModuleParsed ->
                                        [ ( rangeModulePart
                                          , (moduleCompletionItemFromModuleSyntax
                                                { importedModuleNameRestAfterPrefix = Nothing
                                                , importedName = Just moduleName
                                                }
                                                referencedModuleParsed.syntax
                                            ).documentation
                                          )
                                        ]

                forNameInModule =
                    functionOrNameNode
                        |> Elm.Syntax.Node.value
                        |> getHoverForFunctionOrName
                        |> Maybe.map (Tuple.pair forNameInModuleRange)
                        |> Maybe.map List.singleton
                        |> Maybe.withDefault []
            in
            forModule ++ forNameInModule

        fromFunctionOrName =
            parsedModule.syntax
                |> listFunctionOrValueExpressionsFromFile
                |> List.concatMap getForHoversForReferenceNode

        fromFunctionTypeAnnotations =
            parsedModule.syntax
                |> listTypeReferencesFromFile
                |> List.concatMap getForHoversForReferenceNode
    in
    fromImportSyntax ++ fromFunctionOrName ++ fromFunctionTypeAnnotations


listTypeReferencesFromFile : Elm.Syntax.File.File -> List (Elm.Syntax.Node.Node ( Elm.Syntax.ModuleName.ModuleName, String ))
listTypeReferencesFromFile =
    .declarations
        >> List.concatMap (Elm.Syntax.Node.value >> listTypeReferencesFromDeclaration)


listTypeReferencesFromDeclaration : Elm.Syntax.Declaration.Declaration -> List (Elm.Syntax.Node.Node ( Elm.Syntax.ModuleName.ModuleName, String ))
listTypeReferencesFromDeclaration declaration =
    case declaration of
        Elm.Syntax.Declaration.FunctionDeclaration functionDeclaration ->
            functionDeclaration.signature
                |> Maybe.map
                    (Elm.Syntax.Node.value
                        >> .typeAnnotation
                        >> listTypeReferencesFromTypeAnnotation
                    )
                |> Maybe.withDefault []

        Elm.Syntax.Declaration.AliasDeclaration aliasDeclaration ->
            aliasDeclaration.typeAnnotation
                |> listTypeReferencesFromTypeAnnotation

        Elm.Syntax.Declaration.CustomTypeDeclaration customTypeDeclaration ->
            customTypeDeclaration.constructors
                |> List.concatMap
                    (Elm.Syntax.Node.value >> .arguments >> List.concatMap listTypeReferencesFromTypeAnnotation)

        _ ->
            []


listTypeReferencesFromTypeAnnotation : Elm.Syntax.Node.Node Elm.Syntax.TypeAnnotation.TypeAnnotation -> List (Elm.Syntax.Node.Node ( Elm.Syntax.ModuleName.ModuleName, String ))
listTypeReferencesFromTypeAnnotation node =
    case Elm.Syntax.Node.value node of
        Elm.Syntax.TypeAnnotation.GenericType _ ->
            []

        Elm.Syntax.TypeAnnotation.Typed instantiated arguments ->
            instantiated :: List.concatMap listTypeReferencesFromTypeAnnotation arguments

        Elm.Syntax.TypeAnnotation.Unit ->
            []

        Elm.Syntax.TypeAnnotation.Tupled tupled ->
            List.concatMap listTypeReferencesFromTypeAnnotation tupled

        Elm.Syntax.TypeAnnotation.Record record ->
            List.concatMap (Elm.Syntax.Node.value >> Tuple.second >> listTypeReferencesFromTypeAnnotation)
                record

        Elm.Syntax.TypeAnnotation.GenericRecord _ record ->
            List.concatMap (Elm.Syntax.Node.value >> Tuple.second >> listTypeReferencesFromTypeAnnotation)
                (Elm.Syntax.Node.value record)

        Elm.Syntax.TypeAnnotation.FunctionTypeAnnotation input return ->
            List.concatMap listTypeReferencesFromTypeAnnotation [ input, return ]


listFunctionOrValueExpressionsFromFile : Elm.Syntax.File.File -> List (Elm.Syntax.Node.Node ( Elm.Syntax.ModuleName.ModuleName, String ))
listFunctionOrValueExpressionsFromFile file =
    file.declarations
        |> List.concatMap (Elm.Syntax.Node.value >> listFunctionOrValueExpressionsFromDeclaration)


listFunctionOrValueExpressionsFromDeclaration : Elm.Syntax.Declaration.Declaration -> List (Elm.Syntax.Node.Node ( Elm.Syntax.ModuleName.ModuleName, String ))
listFunctionOrValueExpressionsFromDeclaration declaration =
    case declaration of
        Elm.Syntax.Declaration.FunctionDeclaration functionDeclaration ->
            functionDeclaration.declaration
                |> Elm.Syntax.Node.value
                |> .expression
                |> listFunctionOrValueExpressionsFromExpression

        _ ->
            []


listFunctionOrValueExpressionsFromLetDeclaration : Elm.Syntax.Expression.LetDeclaration -> List (Elm.Syntax.Node.Node ( Elm.Syntax.ModuleName.ModuleName, String ))
listFunctionOrValueExpressionsFromLetDeclaration declaration =
    case declaration of
        Elm.Syntax.Expression.LetFunction function ->
            listFunctionOrValueExpressionsFromFunction function

        Elm.Syntax.Expression.LetDestructuring _ destructuredExpr ->
            listFunctionOrValueExpressionsFromExpression destructuredExpr


listFunctionOrValueExpressionsFromExpression : Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression -> List (Elm.Syntax.Node.Node ( Elm.Syntax.ModuleName.ModuleName, String ))
listFunctionOrValueExpressionsFromExpression expressionNode =
    case Elm.Syntax.Node.value expressionNode of
        Elm.Syntax.Expression.FunctionOrValue moduleName nameInModule ->
            [ Elm.Syntax.Node.Node (Elm.Syntax.Node.range expressionNode) ( moduleName, nameInModule ) ]

        Elm.Syntax.Expression.Application application ->
            application
                |> List.concatMap listFunctionOrValueExpressionsFromExpression

        Elm.Syntax.Expression.OperatorApplication _ _ left right ->
            [ left, right ]
                |> List.concatMap listFunctionOrValueExpressionsFromExpression

        Elm.Syntax.Expression.IfBlock condition thenNode elseNode ->
            [ condition, thenNode, elseNode ]
                |> List.concatMap listFunctionOrValueExpressionsFromExpression

        Elm.Syntax.Expression.Negation negation ->
            listFunctionOrValueExpressionsFromExpression negation

        Elm.Syntax.Expression.TupledExpression tupled ->
            tupled |> List.concatMap listFunctionOrValueExpressionsFromExpression

        Elm.Syntax.Expression.ParenthesizedExpression parenthesizedExpression ->
            listFunctionOrValueExpressionsFromExpression parenthesizedExpression

        Elm.Syntax.Expression.LetExpression letExpression ->
            listFunctionOrValueExpressionsFromExpression letExpression.expression
                ++ (letExpression.declarations
                        |> List.concatMap
                            (Elm.Syntax.Node.value
                                >> listFunctionOrValueExpressionsFromLetDeclaration
                            )
                   )

        Elm.Syntax.Expression.CaseExpression caseExpression ->
            (caseExpression.expression :: List.map Tuple.second caseExpression.cases)
                |> List.concatMap listFunctionOrValueExpressionsFromExpression

        Elm.Syntax.Expression.LambdaExpression lambdaExpression ->
            listFunctionOrValueExpressionsFromExpression lambdaExpression.expression

        Elm.Syntax.Expression.RecordExpr recordExpr ->
            List.concatMap
                (Elm.Syntax.Node.value
                    >> Tuple.second
                    >> listFunctionOrValueExpressionsFromExpression
                )
                recordExpr

        Elm.Syntax.Expression.ListExpr list ->
            List.concatMap listFunctionOrValueExpressionsFromExpression list

        _ ->
            []


listFunctionOrValueExpressionsFromFunction : Elm.Syntax.Expression.Function -> List (Elm.Syntax.Node.Node ( Elm.Syntax.ModuleName.ModuleName, String ))
listFunctionOrValueExpressionsFromFunction function =
    listFunctionOrValueExpressionsFromExpression
        (Elm.Syntax.Node.value function.declaration).expression


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

                        modulesAvailableForImport =
                            modulesAvailableForImportFromState languageServiceState

                        moduleNamesToNotSuggestForImport =
                            [ fileOpenedInEditorModuleName ]

                        modulesToSuggestForImport =
                            modulesAvailableForImport
                                |> List.map .syntax
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
                            importedModulesFromFile fileOpenedInEditor languageServiceState

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
                            importExposingsFromFile fileOpenedInEditor languageServiceState

                        localDeclarationsAndImportExposings =
                            List.map .completionItem currentModuleDeclarations.topLevel
                                ++ importExposings
                                ++ fromLetBlocks

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


importedModulesFromFile :
    ParsedModuleCache
    -> LanguageServiceState
    ->
        List
            { canonicalName : List String
            , importedName : Elm.Syntax.ModuleName.ModuleName
            , parsedModule : Maybe ParsedModuleCache
            , referencesRanges : List Elm.Syntax.Range.Range
            }
importedModulesFromFile fileOpenedInEditor languageServiceState =
    let
        implicitlyImportedModules =
            languageServiceState.coreModulesCache
                |> List.filter .implicitImport
                |> List.map
                    (\coreModule ->
                        let
                            canonicalName =
                                Elm.Syntax.Module.moduleName (Elm.Syntax.Node.value coreModule.parseResult.syntax.moduleDefinition)
                        in
                        { canonicalName = canonicalName
                        , importedName = canonicalName
                        , parsedModule = Just coreModule.parseResult
                        , referencesRanges = []
                        }
                    )

        parsedModuleFromModuleName canonicalModuleName =
            modulesAvailableForImportFromState languageServiceState
                |> List.filter
                    (.syntax
                        >> .moduleDefinition
                        >> Elm.Syntax.Node.value
                        >> Elm.Syntax.Module.moduleName
                        >> (==) canonicalModuleName
                    )
                |> List.head

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
                        in
                        { canonicalName = canonicalName
                        , importedName = importedName
                        , parsedModule = parsedModuleFromModuleName canonicalName
                        , referencesRanges = [ Elm.Syntax.Node.range importSyntax.moduleName ]
                        }
                    )
    in
    implicitlyImportedModules ++ explicitlyImportedModules


modulesAvailableForImportFromState : LanguageServiceState -> List ParsedModuleCache
modulesAvailableForImportFromState languageServiceState =
    (languageServiceState.fileTreeParseCache
        |> FileTree.flatListOfBlobsFromFileTreeNode
        |> List.filterMap (Tuple.second >> .parsedFileLastSuccess)
    )
        ++ List.map .parseResult languageServiceState.coreModulesCache


importExposingsFromFile :
    ParsedModuleCache
    -> LanguageServiceState
    -> List Frontend.MonacoEditor.MonacoCompletionItem
importExposingsFromFile fileOpenedInEditor languageServiceState =
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
    String.join "\n" (List.map ((++) "    ") codeLines)


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


elmCoreModules : List { moduleText : String, implicitImport : Bool }
elmCoreModules =
    [ CompilationInterface.SourceFiles.file_tree____elm_core_modules_implicit_import
        |> listAllFilesFromSourceFileTreeNode
        |> List.map (\( _, fileContent ) -> { moduleText = fileContent.utf8, implicitImport = True })
    , CompilationInterface.SourceFiles.file_tree____elm_core_modules_explicit_import
        |> listAllFilesFromSourceFileTreeNode
        |> List.map (\( _, fileContent ) -> { moduleText = fileContent.utf8, implicitImport = False })
    ]
        |> List.concat


listAllFilesFromSourceFileTreeNode : CompilationInterface.SourceFiles.FileTreeNode a -> List ( List String, a )
listAllFilesFromSourceFileTreeNode node =
    case node of
        CompilationInterface.SourceFiles.BlobNode blob ->
            [ ( [], blob ) ]

        CompilationInterface.SourceFiles.TreeNode tree ->
            tree
                |> List.concatMap
                    (\( entryName, entryNode ) ->
                        listAllFilesFromSourceFileTreeNode entryNode |> List.map (Tuple.mapFirst ((::) entryName))
                    )


elmCoreModulesParseResults : List ElmCoreModule
elmCoreModulesParseResults =
    elmCoreModules
        |> List.filterMap
            (\coreModule ->
                coreModule.moduleText
                    |> CompileFullstackApp.parseElmModuleText
                    |> Result.toMaybe
                    |> Maybe.map
                        (\syntax ->
                            { parseResult = { text = coreModule.moduleText, syntax = syntax }
                            , implicitImport = coreModule.implicitImport
                            }
                        )
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


rangeIntersectsLocation : Elm.Syntax.Range.Location -> Elm.Syntax.Range.Range -> Bool
rangeIntersectsLocation location range =
    ((range.start.row <= location.row)
        && (range.start.row < location.row || range.start.column <= location.column)
    )
        && ((range.end.row >= location.row)
                && (range.end.row > location.row || range.end.column > location.column)
           )


getTextLinesFromRangeAndText : Elm.Syntax.Range.Range -> String -> List String
getTextLinesFromRangeAndText range =
    String.lines
        >> List.take range.end.row
        >> List.drop (range.start.row - 1)
        >> List.reverse
        >> listMapFirstElement (String.left (range.end.column - 1))
        >> List.reverse
        >> listMapFirstElement (String.dropLeft (range.start.column - 1))


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
