module LanguageService exposing (..)

{-| Language services for Elm programs.
These functions enable features like completion suggestions and hover tips in the code editor.
-}

import CompilationInterface.SourceFiles
import CompileElmApp
import Elm.Syntax.Comments
import Elm.Syntax.Declaration
import Elm.Syntax.Exposing
import Elm.Syntax.Expression
import Elm.Syntax.File
import Elm.Syntax.Module
import Elm.Syntax.ModuleName
import Elm.Syntax.Node
import Elm.Syntax.Pattern
import Elm.Syntax.Range
import Elm.Syntax.Type
import Elm.Syntax.TypeAlias
import Elm.Syntax.TypeAnnotation
import FileTree
import Frontend.MonacoEditor
import LanguageServiceInterface
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
    , textContent : Maybe FileTextContent
    , parsedFileLastSuccess : Maybe ParsedModuleCache
    }


type alias FileTextContent =
    { text : String
    , parsedFile : Maybe ParsedModuleCache
    }


type alias ParsedModuleCache =
    { filePath : List String
    , text : String
    , syntax : Elm.Syntax.File.File
    }


type alias ElmCoreModule =
    { parseResult : ParsedModuleCache
    , implicitImport : Bool
    }


type DeclarationScope
    = TopLevelScope Elm.Syntax.Range.Range
    | LocalScope Elm.Syntax.Range.Range


type Declaration documentation
    = FunctionOrValueDeclaration (FunctionOrValueDeclarationStruct documentation)
    | TypeAliasDeclaration (FunctionOrValueDeclarationStruct documentation)
    | ChoiceTypeDeclaration (ChoiceTypeDeclarationStruct documentation)


type alias FunctionOrValueDeclarationStruct documentation =
    { name : String
    , documentation : documentation
    }


type alias ChoiceTypeDeclarationStruct documentation =
    { name : String
    , documentation : documentation
    , tagsDeclarations : List (FunctionOrValueDeclarationStruct documentation)
    }


type alias ParsedDeclarationsAndReferences =
    { declarations : List ( ParsedDeclaration, DeclarationScope )
    , references : List (Elm.Syntax.Node.Node ( List String, String ))
    }


type alias ParsedDeclaration =
    Declaration ParsedDocumentation


type alias ParsedDocumentation =
    { buildMarkdown : (Elm.Syntax.Range.Range -> List String) -> String }


type alias LocationUnderFilePath =
    LanguageServiceInterface.LocationUnderFilePath


initLanguageServiceState : LanguageServiceState
initLanguageServiceState =
    { fileTreeParseCache = FileTree.TreeNode []
    , coreModulesCache = elmCoreModulesParseResults
    }


{-| <https://microsoft.github.io/monaco-editor/typedoc/interfaces/languages.HoverProvider.html#provideHover>
-}
provideHover :
    LanguageServiceInterface.ProvideHoverRequestStruct
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
                    let
                        { hoverItems } =
                            hoverItemsFromParsedModule parsedFileLastSuccess languageServiceState
                    in
                    hoverItems
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


hoverItemsFromParsedModule :
    ParsedModuleCache
    -> LanguageServiceState
    ->
        { fromDeclarations : List ( Elm.Syntax.Range.Range, LocationUnderFilePath, String )
        , hoverItems : List ( Elm.Syntax.Range.Range, String )
        }
hoverItemsFromParsedModule parsedModule languageServiceState =
    let
        importedModules :
            List
                { filePath : List String
                , canonicalName : List String
                , importedName : Elm.Syntax.ModuleName.ModuleName
                , parsedModule : Maybe ParsedModuleCache
                , referencesRanges : List Elm.Syntax.Range.Range
                }
        importedModules =
            importedModulesFromFile parsedModule languageServiceState

        parsedDeclarationsAndReferences =
            listDeclarationsAndReferencesInFile parsedModule.syntax

        currentModuleDeclarations :
            { fromTopLevel :
                List
                    { completionItem : Frontend.MonacoEditor.MonacoCompletionItem
                    , isExposed : Bool
                    , range : Elm.Syntax.Range.Range
                    }
            , fromLocals :
                List
                    { completionItem : Frontend.MonacoEditor.MonacoCompletionItem
                    , range : Elm.Syntax.Range.Range
                    }
            }
        currentModuleDeclarations =
            completionItemsFromModule parsedModule

        localDeclarationsAndImportExposings : List ( DeclarationScope, Frontend.MonacoEditor.MonacoCompletionItem )
        localDeclarationsAndImportExposings =
            List.concat
                [ List.concat
                    [ List.map
                        (\fromTopLevel ->
                            ( TopLevelScope fromTopLevel.range
                            , fromTopLevel.completionItem
                            )
                        )
                        currentModuleDeclarations.fromTopLevel
                    , commonImplicitTopLevelImports languageServiceState
                        |> List.map
                            (\( range, completionItem ) ->
                                ( TopLevelScope range, completionItem )
                            )

                    {-
                       , importExposingsFromFile parsedModule languageServiceState
                           |> List.map (Tuple.pair (TopLevelScope { start = { row = 1, column = 1 }, end = { row = 11, column = 13 } }))
                    -}
                    ]
                , List.map
                    (\item -> ( LocalScope item.range, item.completionItem ))
                    currentModuleDeclarations.fromLocals
                ]

        getModuleByImportedName importedName =
            importedModules
                |> List.filter (.importedName >> (==) importedName)
                |> List.filterMap .parsedModule
                |> List.head

        fromImportSyntax : List ( Elm.Syntax.Range.Range, String )
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
                                                { importedModuleNameRestAfterPrefix = Nothing
                                                , importedName = Just importedModule.importedName
                                                }
                                                importedModuleParsed.syntax
                                            )
                                        )
                    )
                |> List.map (Tuple.mapSecond .documentation)

        getHoverForFunctionOrName :
            Elm.Syntax.Node.Node ( Elm.Syntax.ModuleName.ModuleName, String )
            -> Maybe ( LocationUnderFilePath, String )
        getHoverForFunctionOrName (Elm.Syntax.Node.Node functionOrNameNodeRange ( moduleName, nameInModule )) =
            let
                itemsBeforeFilteringByNameInModule :
                    List
                        ( LocationUnderFilePath
                        , Frontend.MonacoEditor.MonacoCompletionItem
                        )
                itemsBeforeFilteringByNameInModule =
                    if moduleName == [] then
                        localDeclarationsAndImportExposings
                            |> List.filterMap
                                (\( scope, completionItem ) ->
                                    case scope of
                                        TopLevelScope scopeRange ->
                                            Just
                                                ( { filePath = parsedModule.filePath
                                                  , range = monacoRangeFromSyntaxRange scopeRange
                                                  }
                                                , completionItem
                                                )

                                        LocalScope scopeRange ->
                                            if rangeIntersectsLocation functionOrNameNodeRange.start scopeRange then
                                                Just
                                                    ( { filePath = parsedModule.filePath
                                                      , range = monacoRangeFromSyntaxRange scopeRange
                                                      }
                                                    , completionItem
                                                    )

                                            else
                                                Nothing
                                )

                    else
                        case getModuleByImportedName moduleName of
                            Nothing ->
                                []

                            Just referencedModule ->
                                (completionItemsFromModule referencedModule).fromTopLevel
                                    |> List.filter .isExposed
                                    |> List.map
                                        (\item ->
                                            ( { filePath = referencedModule.filePath
                                              , range = monacoRangeFromSyntaxRange item.range
                                              }
                                            , item.completionItem
                                            )
                                        )
            in
            case
                itemsBeforeFilteringByNameInModule
                    |> List.filter (\( _, completionItem ) -> completionItem.label == nameInModule)
            of
                [] ->
                    Nothing

                ( range, completionItem ) :: _ ->
                    Just
                        ( range
                        , completionItem.documentation
                        )

        getForHoversForReferenceNode :
            Elm.Syntax.Node.Node ( List String, String )
            -> List ( Elm.Syntax.Range.Range, LocationUnderFilePath, String )
        getForHoversForReferenceNode functionOrNameNode =
            let
                (Elm.Syntax.Node.Node wholeRange ( moduleName, nameInModule )) =
                    functionOrNameNode

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

                forModule : List ( Elm.Syntax.Range.Range, LocationUnderFilePath, String )
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
                                          , { filePath = referencedModuleParsed.filePath
                                            , range =
                                                monacoRangeFromSyntaxRange
                                                    (CompileElmApp.syntaxRangeCoveringCompleteString parsedModule.text)
                                            }
                                          , (moduleCompletionItemFromModuleSyntax
                                                { importedModuleNameRestAfterPrefix = Nothing
                                                , importedName = Just moduleName
                                                }
                                                referencedModuleParsed.syntax
                                            ).documentation
                                          )
                                        ]

                forNameInModule : List ( Elm.Syntax.Range.Range, LocationUnderFilePath, String )
                forNameInModule =
                    case getHoverForFunctionOrName functionOrNameNode of
                        Nothing ->
                            []

                        Just ( sourceLocation, hover ) ->
                            [ ( forNameInModuleRange
                              , sourceLocation
                              , hover
                              )
                            ]
            in
            List.concat
                [ forModule
                , forNameInModule
                ]

        fromDeclarations : List ( Elm.Syntax.Range.Range, LocationUnderFilePath, String )
        fromDeclarations =
            parsedDeclarationsAndReferences.references
                |> List.concatMap getForHoversForReferenceNode

        fromDeclarationsLessSourceLocation : List ( Elm.Syntax.Range.Range, String )
        fromDeclarationsLessSourceLocation =
            fromDeclarations
                |> List.map (\( range, _, documentation ) -> ( range, documentation ))

        hoverItems : List ( Elm.Syntax.Range.Range, String )
        hoverItems =
            List.concat
                [ fromImportSyntax
                , fromDeclarationsLessSourceLocation
                ]
    in
    { fromDeclarations = fromDeclarations
    , hoverItems = hoverItems
    }


listTypeReferencesFromTypeAnnotation :
    Elm.Syntax.Node.Node Elm.Syntax.TypeAnnotation.TypeAnnotation
    -> List (Elm.Syntax.Node.Node ( Elm.Syntax.ModuleName.ModuleName, String ))
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


{-| <https://microsoft.github.io/monaco-editor/typedoc/interfaces/languages.CompletionItemProvider.html#provideCompletionItems>
-}
provideCompletionItems :
    LanguageServiceInterface.ProvideCompletionItemsRequestStruct
    -> LanguageServiceState
    -> List Frontend.MonacoEditor.MonacoCompletionItem
provideCompletionItems request languageServiceState =
    case languageServiceState.fileTreeParseCache |> FileTree.getBlobAtPathFromFileTree request.filePathOpenedInEditor of
        Nothing ->
            []

        Just currentFileCacheItem ->
            let
                cursorIsInCommentInCompleteSyntax =
                    case currentFileCacheItem.textContent of
                        Nothing ->
                            False

                        Just textContent ->
                            case textContent.parsedFile of
                                Nothing ->
                                    False

                                Just parsedFile ->
                                    locationIsInComment
                                        { row = request.cursorLineNumber
                                        , column =
                                            (request.textUntilPosition
                                                |> String.lines
                                                |> List.reverse
                                                |> List.head
                                                |> Maybe.withDefault ""
                                                |> String.length
                                            )
                                                + 1
                                        }
                                        parsedFile.syntax
            in
            if cursorIsInCommentInCompleteSyntax then
                []

            else
                case currentFileCacheItem.parsedFileLastSuccess of
                    Nothing ->
                        []

                    Just fileOpenedInEditor ->
                        provideCompletionItemsInModule
                            { fileOpenedInEditor = fileOpenedInEditor
                            , cursorLineNumber = request.cursorLineNumber
                            , textUntilPosition = request.textUntilPosition
                            }
                            languageServiceState


{-| <https://microsoft.github.io/monaco-editor/typedoc/interfaces/languages.DefinitionProvider.html#provideDefinition>
-}
provideDefinition :
    LanguageServiceInterface.ProvideDefinitionRequestStruct
    -> LanguageServiceState
    -> List LanguageServiceInterface.LocationUnderFilePath
provideDefinition request languageServiceState =
    case FileTree.getBlobAtPathFromFileTree request.filePathOpenedInEditor languageServiceState.fileTreeParseCache of
        Nothing ->
            []

        Just currentFileCacheItem ->
            case currentFileCacheItem.parsedFileLastSuccess of
                Nothing ->
                    []

                Just parsedFileLastSuccess ->
                    let
                        { fromDeclarations } =
                            hoverItemsFromParsedModule parsedFileLastSuccess languageServiceState
                    in
                    fromDeclarations
                        |> List.filterMap
                            (\( refRange, sourceLocation, _ ) ->
                                if
                                    rangeIntersectsLocation
                                        { row = request.positionLineNumber, column = request.positionColumn }
                                        refRange
                                then
                                    Just sourceLocation

                                else
                                    Nothing
                            )


monacoRangeFromSyntaxRange : Elm.Syntax.Range.Range -> Frontend.MonacoEditor.MonacoRange
monacoRangeFromSyntaxRange syntaxRange =
    { startLineNumber = syntaxRange.start.row
    , startColumn = syntaxRange.start.column
    , endLineNumber = syntaxRange.end.row
    , endColumn = syntaxRange.end.column
    }


provideCompletionItemsInModule :
    { fileOpenedInEditor : ParsedModuleCache, cursorLineNumber : Int, textUntilPosition : String }
    -> LanguageServiceState
    -> List Frontend.MonacoEditor.MonacoCompletionItem
provideCompletionItemsInModule request languageServiceState =
    let
        fileOpenedInEditorModuleName =
            Elm.Syntax.Module.moduleName (Elm.Syntax.Node.value request.fileOpenedInEditor.syntax.moduleDefinition)

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
            importedModulesFromFile request.fileOpenedInEditor languageServiceState

        currentModuleDeclarations =
            completionItemsFromModule request.fileOpenedInEditor

        fromLocals =
            currentModuleDeclarations.fromLocals
                |> List.filter
                    (.range
                        >> rangeIntersectsLocation
                            { row = request.cursorLineNumber, column = String.length lineUntilPosition }
                    )
                |> List.map .completionItem

        importExposings =
            List.concat
                [ importExposingsFromFile request.fileOpenedInEditor languageServiceState
                , List.map Tuple.second (commonImplicitTopLevelImports languageServiceState)
                ]

        localDeclarationsAndImportExposings =
            List.concat
                [ List.map .completionItem currentModuleDeclarations.fromTopLevel
                , importExposings
                , fromLocals
                ]

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
                        (completionItemsFromModule referencedModule).fromTopLevel
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
            { filePath : List String
            , canonicalName : List String
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
                                Elm.Syntax.Module.moduleName
                                    (Elm.Syntax.Node.value coreModule.parseResult.syntax.moduleDefinition)
                        in
                        { filePath = coreModule.parseResult.filePath
                        , canonicalName = canonicalName
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
                |> List.filterMap
                    (\importSyntax ->
                        let
                            canonicalName =
                                Elm.Syntax.Node.value importSyntax.moduleName

                            importedName =
                                importSyntax.moduleAlias
                                    |> Maybe.map Elm.Syntax.Node.value
                                    |> Maybe.withDefault canonicalName
                        in
                        case parsedModuleFromModuleName canonicalName of
                            Nothing ->
                                Nothing

                            Just parsedModule ->
                                Just
                                    { filePath = parsedModule.filePath
                                    , canonicalName = canonicalName
                                    , importedName = importedName
                                    , parsedModule = Just parsedModule
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
                                        (completionItemsFromModule importedParsedModule).fromTopLevel
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


commonImplicitTopLevelImports :
    LanguageServiceState
    -> List ( Elm.Syntax.Range.Range, Frontend.MonacoEditor.MonacoCompletionItem )
commonImplicitTopLevelImports languageServiceState =
    languageServiceState.coreModulesCache
        |> List.concatMap
            (\coreModule ->
                let
                    (Elm.Syntax.Node.Node _ moduleDefinition) =
                        coreModule.parseResult.syntax.moduleDefinition

                    moduleName =
                        Elm.Syntax.Module.moduleName moduleDefinition

                    moduleCompletionItems =
                        completionItemsFromModule coreModule.parseResult

                    isItemExposed : Frontend.MonacoEditor.MonacoCompletionItem -> Bool
                    isItemExposed item =
                        case moduleName of
                            [ "Basics" ] ->
                                True

                            [ "String" ] ->
                                case item.label of
                                    "String" ->
                                        True

                                    _ ->
                                        False

                            [ "Maybe" ] ->
                                case item.label of
                                    "Maybe" ->
                                        True

                                    "Just" ->
                                        True

                                    "Nothing" ->
                                        True

                                    _ ->
                                        False

                            _ ->
                                False
                in
                moduleCompletionItems.fromTopLevel
                    |> List.filterMap
                        (\item ->
                            if item.isExposed then
                                if isItemExposed item.completionItem then
                                    Just
                                        ( item.range
                                        , item.completionItem
                                        )

                                else
                                    Nothing

                            else
                                Nothing
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
        { fromTopLevel :
            List
                { completionItem : Frontend.MonacoEditor.MonacoCompletionItem
                , isExposed : Bool
                , range : Elm.Syntax.Range.Range
                }
        , fromLocals :
            List
                { completionItem : Frontend.MonacoEditor.MonacoCompletionItem
                , range : Elm.Syntax.Range.Range
                }
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

        parsedDeclarationsAndReferences =
            listDeclarationsAndReferencesInFile moduleCache.syntax

        buildCompletionItems =
            completionItemsFromParsedDeclarationOrReference getTextLinesFromRange

        fromTopLevel =
            parsedDeclarationsAndReferences.declarations
                |> List.concatMap
                    (\( declOrRef, scope ) ->
                        case scope of
                            TopLevelScope scopeRange ->
                                buildCompletionItems declOrRef
                                    |> List.map
                                        (\completionItem ->
                                            { completionItem = completionItem
                                            , isExposed =
                                                case declOrRef of
                                                    FunctionOrValueDeclaration functionOrValue ->
                                                        exposesFunction functionOrValue.name

                                                    TypeAliasDeclaration typeAlias ->
                                                        exposesTypeOrAlias typeAlias.name

                                                    ChoiceTypeDeclaration choiceType ->
                                                        exposesTypeOrAlias choiceType.name
                                            , range = scopeRange
                                            }
                                        )

                            LocalScope _ ->
                                []
                    )

        fromLocals =
            parsedDeclarationsAndReferences.declarations
                |> List.concatMap
                    (\( declOrRef, scope ) ->
                        case scope of
                            TopLevelScope _ ->
                                []

                            LocalScope range ->
                                buildCompletionItems declOrRef
                                    |> List.map (\completionItem -> { completionItem = completionItem, range = range })
                    )
    in
    { fromTopLevel = fromTopLevel
    , fromLocals = fromLocals
    }


completionItemsFromParsedDeclarationOrReference : (Elm.Syntax.Range.Range -> List String) -> ParsedDeclaration -> List Frontend.MonacoEditor.MonacoCompletionItem
completionItemsFromParsedDeclarationOrReference getTextLinesFromRange declarationOrReference =
    let
        buildCompletionItem { name, buildMarkdown, kind } =
            { label = name
            , documentation = buildMarkdown getTextLinesFromRange
            , insertText = name
            , kind = kind
            }
    in
    case declarationOrReference of
        FunctionOrValueDeclaration functionOrValueDeclaration ->
            [ buildCompletionItem
                { name = functionOrValueDeclaration.name
                , buildMarkdown = functionOrValueDeclaration.documentation.buildMarkdown
                , kind = Frontend.MonacoEditor.FunctionCompletionItemKind
                }
            ]

        TypeAliasDeclaration typeAliasDeclaration ->
            [ buildCompletionItem
                { name = typeAliasDeclaration.name
                , buildMarkdown = typeAliasDeclaration.documentation.buildMarkdown
                , kind = Frontend.MonacoEditor.StructCompletionItemKind
                }
            ]

        ChoiceTypeDeclaration choiceTypeDeclaration ->
            buildCompletionItem
                { name = choiceTypeDeclaration.name
                , buildMarkdown = choiceTypeDeclaration.documentation.buildMarkdown
                , kind = Frontend.MonacoEditor.EnumCompletionItemKind
                }
                :: List.map
                    (\tagDeclaration ->
                        buildCompletionItem
                            { name = tagDeclaration.name
                            , buildMarkdown = tagDeclaration.documentation.buildMarkdown
                            , kind = Frontend.MonacoEditor.EnumMemberCompletionItemKind
                            }
                    )
                    choiceTypeDeclaration.tagsDeclarations


documentationMarkdownFromCodeLinesAndDocumentation : List String -> Maybe String -> String
documentationMarkdownFromCodeLinesAndDocumentation codeLines maybeDocumentation =
    (markdownElmCodeBlockFromCodeLines codeLines
        :: Maybe.withDefault [] (Maybe.map List.singleton maybeDocumentation)
    )
        |> String.join "\n\n"


markdownElmCodeBlockFromCodeLines : List String -> String
markdownElmCodeBlockFromCodeLines codeLines =
    String.join "\n" (List.map ((++) "    ") codeLines)


updateLanguageServiceState : LanguageServiceInterface.FileTreeNode -> LanguageServiceState -> LanguageServiceState
updateLanguageServiceState fileTree state =
    let
        compileFileCacheEntry ( blobPath, fileTreeBlob ) =
            let
                maybePreviousCached =
                    state.fileTreeParseCache |> FileTree.getBlobAtPathFromFileTree blobPath

                buildNewEntry () =
                    let
                        textContent =
                            case fileTreeBlob.asText of
                                Nothing ->
                                    Nothing

                                Just asString ->
                                    let
                                        parsedFile =
                                            asString
                                                |> CompileElmApp.parseElmModuleText
                                                |> Result.toMaybe
                                                |> Maybe.map
                                                    (\syntax ->
                                                        { filePath = blobPath
                                                        , text = asString
                                                        , syntax = syntax
                                                        }
                                                    )
                                    in
                                    Just { text = asString, parsedFile = parsedFile }
                    in
                    { sourceBase64 = fileTreeBlob.asBase64
                    , textContent = textContent
                    , parsedFileLastSuccess =
                        Maybe.andThen .parsedFile textContent
                            |> Maybe.Extra.orElse (Maybe.andThen .parsedFileLastSuccess maybePreviousCached)
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
        | fileTreeParseCache = FileTree.mapBlobsWithPath compileFileCacheEntry fileTree
    }


elmCoreModules : List { moduleText : String, implicitImport : Bool }
elmCoreModules =
    [ CompilationInterface.SourceFiles.file_tree____elm_core_modules_implicit_import
        |> listAllFilesFromSourceFileTreeNode
        |> List.map (\( _, fileContent ) -> { moduleText = fileContent.utf8, implicitImport = True })
    , [ CompilationInterface.SourceFiles.file_tree____elm_core_modules_explicit_import
      , CompilationInterface.SourceFiles.file_tree____elm_kernel_modules_json_src
      , CompilationInterface.SourceFiles.file_tree____elm_kernel_modules_http_src
      , CompilationInterface.SourceFiles.file_tree____elm_kernel_modules_time_src
      , CompilationInterface.SourceFiles.file_tree____elm_kernel_modules_html_src
      , CompilationInterface.SourceFiles.file_tree____elm_kernel_modules_browser_src
      ]
        |> List.concatMap listAllFilesFromSourceFileTreeNode
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
                    |> CompileElmApp.parseElmModuleText
                    |> Result.toMaybe
                    |> Maybe.map
                        (\syntax ->
                            { parseResult =
                                { filePath = [ "elm-core" ]
                                , text = coreModule.moduleText
                                , syntax = syntax
                                }
                            , implicitImport = coreModule.implicitImport
                            }
                        )
            )


listDeclarationsAndReferencesInFile : Elm.Syntax.File.File -> ParsedDeclarationsAndReferences
listDeclarationsAndReferencesInFile =
    .declarations
        >> listConcatMapParsedDeclarationsAndReferences listDeclarationsAndReferencesInDeclaration


listDeclarationsAndReferencesInDeclaration :
    Elm.Syntax.Node.Node Elm.Syntax.Declaration.Declaration
    -> ParsedDeclarationsAndReferences
listDeclarationsAndReferencesInDeclaration declarationNode =
    let
        empty =
            { declarations = [], references = [] }

        (Elm.Syntax.Node.Node declarationRange declaration) =
            declarationNode
    in
    case declaration of
        Elm.Syntax.Declaration.FunctionDeclaration function ->
            listDeclarationsAndReferencesForFunction declarationRange function

        Elm.Syntax.Declaration.AliasDeclaration aliasDeclaration ->
            declarationsAndReferencesForAliasDeclaration declarationRange aliasDeclaration

        Elm.Syntax.Declaration.CustomTypeDeclaration choiceTypeDeclaration ->
            listDeclarationsAndReferencesFromTypeDeclaration declarationRange choiceTypeDeclaration

        Elm.Syntax.Declaration.PortDeclaration _ ->
            empty

        Elm.Syntax.Declaration.InfixDeclaration _ ->
            empty

        Elm.Syntax.Declaration.Destructuring _ _ ->
            empty


declarationsAndReferencesForAliasDeclaration :
    Elm.Syntax.Range.Range
    -> Elm.Syntax.TypeAlias.TypeAlias
    -> ParsedDeclarationsAndReferences
declarationsAndReferencesForAliasDeclaration declarationRange aliasDeclaration =
    { declarations =
        [ ( declarationOrReferenceForAliasDeclaration aliasDeclaration
          , TopLevelScope declarationRange
          )
        ]
    , references = listTypeReferencesFromTypeAnnotation aliasDeclaration.typeAnnotation
    }


declarationOrReferenceForAliasDeclaration : Elm.Syntax.TypeAlias.TypeAlias -> ParsedDeclaration
declarationOrReferenceForAliasDeclaration aliasDeclaration =
    let
        aliasName =
            Elm.Syntax.Node.value aliasDeclaration.name
    in
    TypeAliasDeclaration
        { name = aliasName
        , documentation =
            { buildMarkdown =
                \getTextLinesFromRange ->
                    let
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
                    documentationMarkdownFromCodeLinesAndDocumentation codeLines documentationStringFromSyntax
            }
        }


listDeclarationsAndReferencesFromTypeDeclaration :
    Elm.Syntax.Range.Range
    -> Elm.Syntax.Type.Type
    -> ParsedDeclarationsAndReferences
listDeclarationsAndReferencesFromTypeDeclaration declarationRange choiceTypeDeclaration =
    let
        documentationStringFromSyntax =
            choiceTypeDeclaration.documentation
                |> Maybe.map (Elm.Syntax.Node.value >> removeWrappingFromMultilineComment)

        codeRange =
            Elm.Syntax.Node.range choiceTypeDeclaration.name
                :: List.map Elm.Syntax.Node.range choiceTypeDeclaration.constructors
                |> Elm.Syntax.Range.combine
                |> expandRangeToLineStart

        choiceTypeName =
            Elm.Syntax.Node.value choiceTypeDeclaration.name

        tagsDeclarations : List (FunctionOrValueDeclarationStruct ParsedDocumentation)
        tagsDeclarations =
            choiceTypeDeclaration.constructors
                |> List.map
                    (\constructorNode ->
                        let
                            tagName =
                                Elm.Syntax.Node.value
                                    (Elm.Syntax.Node.value constructorNode).name
                        in
                        { name = tagName
                        , documentation =
                            { buildMarkdown =
                                \getTextLinesFromRange ->
                                    let
                                        codeLines =
                                            getTextLinesFromRange codeRange
                                    in
                                    [ markdownElmCodeBlockFromCodeLines [ tagName ]
                                    , "A variant of the choice type `" ++ choiceTypeName ++ "`"
                                    , markdownElmCodeBlockFromCodeLines codeLines
                                    ]
                                        |> String.join "\n\n"
                            }
                        }
                    )
    in
    { declarations =
        [ ( ChoiceTypeDeclaration
                { name = choiceTypeName
                , documentation =
                    { buildMarkdown =
                        \getTextLinesFromRange ->
                            let
                                codeLines =
                                    getTextLinesFromRange codeRange
                            in
                            documentationMarkdownFromCodeLinesAndDocumentation codeLines documentationStringFromSyntax
                    }
                , tagsDeclarations = tagsDeclarations
                }
          , TopLevelScope declarationRange
          )
        ]
    , references =
        choiceTypeDeclaration.constructors
            |> List.concatMap (Elm.Syntax.Node.value >> .arguments >> List.concatMap listTypeReferencesFromTypeAnnotation)
    }


listDeclarationsAndReferencesInExpression :
    Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression
    -> ParsedDeclarationsAndReferences
listDeclarationsAndReferencesInExpression expressionNode =
    let
        empty =
            { declarations = [], references = [] }
    in
    case Elm.Syntax.Node.value expressionNode of
        Elm.Syntax.Expression.UnitExpr ->
            empty

        Elm.Syntax.Expression.Application application ->
            application
                |> List.map listDeclarationsAndReferencesInExpression
                |> concatParsedDeclarationsAndReferences

        Elm.Syntax.Expression.OperatorApplication _ _ leftExpr rightExpr ->
            [ leftExpr, rightExpr ]
                |> List.map listDeclarationsAndReferencesInExpression
                |> concatParsedDeclarationsAndReferences

        Elm.Syntax.Expression.FunctionOrValue moduleName localName ->
            { declarations = []
            , references = [ Elm.Syntax.Node.Node (Elm.Syntax.Node.range expressionNode) ( moduleName, localName ) ]
            }

        Elm.Syntax.Expression.IfBlock ifExpr thenExpr elseExpr ->
            [ ifExpr, thenExpr, elseExpr ]
                |> listConcatMapParsedDeclarationsAndReferences listDeclarationsAndReferencesInExpression

        Elm.Syntax.Expression.PrefixOperator _ ->
            empty

        Elm.Syntax.Expression.Operator _ ->
            empty

        Elm.Syntax.Expression.Integer _ ->
            empty

        Elm.Syntax.Expression.Hex _ ->
            empty

        Elm.Syntax.Expression.Floatable _ ->
            empty

        Elm.Syntax.Expression.Negation negation ->
            listDeclarationsAndReferencesInExpression negation

        Elm.Syntax.Expression.Literal _ ->
            empty

        Elm.Syntax.Expression.CharLiteral _ ->
            empty

        Elm.Syntax.Expression.TupledExpression tupled ->
            tupled |> listConcatMapParsedDeclarationsAndReferences listDeclarationsAndReferencesInExpression

        Elm.Syntax.Expression.ParenthesizedExpression parenthesized ->
            listDeclarationsAndReferencesInExpression parenthesized

        Elm.Syntax.Expression.LetExpression letBlock ->
            listDeclarationsAndReferencesInLetBlock letBlock

        Elm.Syntax.Expression.CaseExpression caseBlock ->
            [ listDeclarationsAndReferencesInExpression caseBlock.expression
            , caseBlock.cases
                |> listConcatMapParsedDeclarationsAndReferences (Tuple.second >> listDeclarationsAndReferencesInExpression)
            ]
                |> concatParsedDeclarationsAndReferences

        Elm.Syntax.Expression.LambdaExpression lambda ->
            listDeclarationsAndReferencesInExpression lambda.expression

        Elm.Syntax.Expression.RecordExpr recordExpr ->
            recordExpr
                |> listConcatMapParsedDeclarationsAndReferences (Elm.Syntax.Node.value >> Tuple.second >> listDeclarationsAndReferencesInExpression)

        Elm.Syntax.Expression.ListExpr listExpr ->
            listExpr |> listConcatMapParsedDeclarationsAndReferences listDeclarationsAndReferencesInExpression

        Elm.Syntax.Expression.RecordAccess recordAccess _ ->
            listDeclarationsAndReferencesInExpression recordAccess

        Elm.Syntax.Expression.RecordAccessFunction _ ->
            empty

        Elm.Syntax.Expression.RecordUpdateExpression recordName recordUpdateExpression ->
            [ { references = [ Elm.Syntax.Node.Node (Elm.Syntax.Node.range recordName) ( [], Elm.Syntax.Node.value recordName ) ]
              , declarations = []
              }
            , recordUpdateExpression
                |> listConcatMapParsedDeclarationsAndReferences (Elm.Syntax.Node.value >> Tuple.second >> listDeclarationsAndReferencesInExpression)
            ]
                |> concatParsedDeclarationsAndReferences

        Elm.Syntax.Expression.GLSLExpression _ ->
            empty


listDeclarationsAndReferencesForFunction :
    Elm.Syntax.Range.Range
    -> Elm.Syntax.Expression.Function
    -> ParsedDeclarationsAndReferences
listDeclarationsAndReferencesForFunction declRange function =
    let
        functionName =
            Elm.Syntax.Node.value (Elm.Syntax.Node.value function.declaration).name

        functionItem =
            FunctionOrValueDeclaration
                { name = functionName
                , documentation =
                    { buildMarkdown =
                        \getTextLinesFromRange ->
                            let
                                documentationStringFromSyntax =
                                    function.documentation
                                        |> Maybe.map (Elm.Syntax.Node.value >> removeWrappingFromMultilineComment)

                                typeAnnotationText =
                                    function.signature
                                        |> Maybe.map (Elm.Syntax.Node.value >> .typeAnnotation)
                                        |> Maybe.map (Elm.Syntax.Node.range >> getTextLinesFromRange >> String.join " ")

                                codeLines =
                                    [ functionName ++ Maybe.withDefault "" (Maybe.map ((++) " : ") typeAnnotationText) ]
                            in
                            documentationMarkdownFromCodeLinesAndDocumentation codeLines documentationStringFromSyntax
                    }
                }

        signatureReferences =
            function.signature
                |> Maybe.map (Elm.Syntax.Node.value >> .typeAnnotation >> listTypeReferencesFromTypeAnnotation)
                |> Maybe.withDefault []

        expressionNode =
            (Elm.Syntax.Node.value function.declaration).expression

        getTypeAnnotationFromArgumentIndex argumentIndex =
            function.signature
                |> Maybe.andThen (Elm.Syntax.Node.value >> .typeAnnotation >> getTypeAnnotationFromFunctionArgumentIndex argumentIndex)

        arguments =
            (Elm.Syntax.Node.value function.declaration).arguments
                |> List.indexedMap
                    (\argumentIndex argument ->
                        listDeclarationsAndReferencesFromPattern
                            { typeAnnotation = getTypeAnnotationFromArgumentIndex argumentIndex }
                            argument
                    )
                |> concatParsedDeclarationsAndReferences
    in
    [ { declarations = [ ( functionItem, TopLevelScope declRange ) ]
      , references = signatureReferences
      }
    , [ arguments
      , listDeclarationsAndReferencesInExpression expressionNode
      ]
        |> concatParsedDeclarationsAndReferences
        |> constrainDeclarationsScopesToRange (Elm.Syntax.Node.range function.declaration)
    ]
        |> concatParsedDeclarationsAndReferences


getTypeAnnotationFromFunctionArgumentIndex :
    Int
    -> Elm.Syntax.Node.Node Elm.Syntax.TypeAnnotation.TypeAnnotation
    -> Maybe (Elm.Syntax.Node.Node Elm.Syntax.TypeAnnotation.TypeAnnotation)
getTypeAnnotationFromFunctionArgumentIndex argumentIndex typeAnnotation =
    case Elm.Syntax.Node.value typeAnnotation of
        Elm.Syntax.TypeAnnotation.FunctionTypeAnnotation argumentType returnType ->
            if argumentIndex < 1 then
                Just argumentType

            else
                getTypeAnnotationFromFunctionArgumentIndex (argumentIndex - 1) returnType

        _ ->
            if argumentIndex < 1 then
                Just typeAnnotation

            else
                Nothing


listDeclarationsAndReferencesFromPattern :
    { typeAnnotation : Maybe (Elm.Syntax.Node.Node Elm.Syntax.TypeAnnotation.TypeAnnotation) }
    -> Elm.Syntax.Node.Node Elm.Syntax.Pattern.Pattern
    -> ParsedDeclarationsAndReferences
listDeclarationsAndReferencesFromPattern config patternNode =
    case Elm.Syntax.Node.value patternNode of
        Elm.Syntax.Pattern.TuplePattern tuplePattern ->
            listConcatMapParsedDeclarationsAndReferences
                (listDeclarationsAndReferencesFromPattern { typeAnnotation = Nothing })
                tuplePattern

        Elm.Syntax.Pattern.UnConsPattern head tail ->
            listConcatMapParsedDeclarationsAndReferences
                (listDeclarationsAndReferencesFromPattern { typeAnnotation = Nothing })
                [ head, tail ]

        Elm.Syntax.Pattern.ListPattern listPattern ->
            listConcatMapParsedDeclarationsAndReferences
                (listDeclarationsAndReferencesFromPattern { typeAnnotation = Nothing })
                listPattern

        Elm.Syntax.Pattern.VarPattern name ->
            { declarations =
                [ ( FunctionOrValueDeclaration
                        { name = name
                        , documentation =
                            { buildMarkdown =
                                \getTextLinesFromRange ->
                                    let
                                        typeAnnotationText =
                                            config.typeAnnotation
                                                |> Maybe.map (Elm.Syntax.Node.range >> getTextLinesFromRange >> String.join " ")

                                        codeLines =
                                            [ name ++ Maybe.withDefault "" (Maybe.map ((++) " : ") typeAnnotationText) ]
                                    in
                                    documentationMarkdownFromCodeLinesAndDocumentation codeLines Nothing
                            }
                        }
                  , TopLevelScope (Elm.Syntax.Node.range patternNode)
                  )
                ]
            , references = []
            }

        Elm.Syntax.Pattern.NamedPattern named arguments ->
            [ { declarations = []
              , references = [ Elm.Syntax.Node.Node (Elm.Syntax.Node.range patternNode) ( named.moduleName, named.name ) ]
              }
            , listConcatMapParsedDeclarationsAndReferences
                (listDeclarationsAndReferencesFromPattern { typeAnnotation = Nothing })
                arguments
            ]
                |> concatParsedDeclarationsAndReferences

        Elm.Syntax.Pattern.ParenthesizedPattern parenthesized ->
            listDeclarationsAndReferencesFromPattern { typeAnnotation = Nothing }
                parenthesized

        _ ->
            { declarations = [], references = [] }


listDeclarationsAndReferencesInLetBlock : Elm.Syntax.Expression.LetBlock -> ParsedDeclarationsAndReferences
listDeclarationsAndReferencesInLetBlock letBlock =
    [ listConcatMapParsedDeclarationsAndReferences
        listDeclarationsAndReferencesInLetDeclaration
        letBlock.declarations
    , listDeclarationsAndReferencesInExpression letBlock.expression
    ]
        |> concatParsedDeclarationsAndReferences


listDeclarationsAndReferencesInLetDeclaration :
    Elm.Syntax.Node.Node Elm.Syntax.Expression.LetDeclaration
    -> ParsedDeclarationsAndReferences
listDeclarationsAndReferencesInLetDeclaration declarationNode =
    let
        (Elm.Syntax.Node.Node declarationRange declaration) =
            declarationNode
    in
    case declaration of
        Elm.Syntax.Expression.LetFunction function ->
            listDeclarationsAndReferencesForFunction declarationRange function

        Elm.Syntax.Expression.LetDestructuring _ letDestructuring ->
            listDeclarationsAndReferencesInExpression letDestructuring


constrainDeclarationsScopesToRange : Elm.Syntax.Range.Range -> ParsedDeclarationsAndReferences -> ParsedDeclarationsAndReferences
constrainDeclarationsScopesToRange range declarationsAndReferences =
    { declarationsAndReferences
        | declarations = declarationsAndReferences.declarations |> List.map (Tuple.mapSecond (constrainScopeToRange range))
    }


constrainScopeToRange : Elm.Syntax.Range.Range -> DeclarationScope -> DeclarationScope
constrainScopeToRange range scope =
    case scope of
        LocalScope _ ->
            scope

        TopLevelScope _ ->
            LocalScope range


concatParsedDeclarationsAndReferences : List ParsedDeclarationsAndReferences -> ParsedDeclarationsAndReferences
concatParsedDeclarationsAndReferences list =
    { declarations = List.concatMap .declarations list
    , references = List.concatMap .references list
    }


listConcatMapParsedDeclarationsAndReferences : (a -> ParsedDeclarationsAndReferences) -> List a -> ParsedDeclarationsAndReferences
listConcatMapParsedDeclarationsAndReferences map =
    List.map map >> concatParsedDeclarationsAndReferences


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


locationIsInComment : Elm.Syntax.Range.Location -> Elm.Syntax.File.File -> Bool
locationIsInComment location parsedModule =
    parsedModule
        |> listCommentsInFile
        |> List.map
            (\commentNode ->
                -- Map ranges of single-line comments to cover more of line in `rangeIntersectsLocation`
                let
                    originalRange =
                        Elm.Syntax.Node.range commentNode

                    range =
                        if String.startsWith "--" (Elm.Syntax.Node.value commentNode) then
                            { originalRange
                                | end = { row = originalRange.end.row, column = originalRange.end.column + 9999 }
                            }

                        else
                            originalRange
                in
                Elm.Syntax.Node.Node range (Elm.Syntax.Node.value commentNode)
            )
        |> List.any (Elm.Syntax.Node.range >> rangeIntersectsLocation location)


listCommentsInFile : Elm.Syntax.File.File -> List (Elm.Syntax.Node.Node Elm.Syntax.Comments.Comment)
listCommentsInFile parsedModule =
    let
        fromDeclarations =
            parsedModule.declarations
                |> List.concatMap (Elm.Syntax.Node.value >> listCommentsFromDeclaration)
    in
    parsedModule.comments ++ fromDeclarations


listCommentsFromDeclaration : Elm.Syntax.Declaration.Declaration -> List (Elm.Syntax.Node.Node Elm.Syntax.Comments.Comment)
listCommentsFromDeclaration declaration =
    case declaration of
        Elm.Syntax.Declaration.FunctionDeclaration function ->
            function.documentation |> Maybe.map List.singleton |> Maybe.withDefault []

        Elm.Syntax.Declaration.AliasDeclaration typeAlias ->
            typeAlias.documentation |> Maybe.map List.singleton |> Maybe.withDefault []

        Elm.Syntax.Declaration.CustomTypeDeclaration typeDeclaration ->
            typeDeclaration.documentation |> Maybe.map List.singleton |> Maybe.withDefault []

        Elm.Syntax.Declaration.PortDeclaration _ ->
            []

        Elm.Syntax.Declaration.InfixDeclaration _ ->
            []

        Elm.Syntax.Declaration.Destructuring _ _ ->
            []


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
listCharSplitByChar charSplits chars =
    case
        List.foldl
            (\char ( completed, current ) ->
                if charSplits char then
                    ( List.reverse current :: completed, [] )

                else
                    ( completed, char :: current )
            )
            ( [], [] )
            chars
    of
        ( completed, current ) ->
            List.reverse (List.reverse current :: completed)
