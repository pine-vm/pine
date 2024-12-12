module LanguageService exposing (..)

{-| Language services for Elm programs.
These functions enable features like completion suggestions and hover tips in the code editor.
-}

import Common
import Elm.Parser
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
    = FunctionOrValueDeclaration documentation
    | TypeAliasDeclaration documentation
    | ChoiceTypeDeclaration documentation (List ( String, documentation ))


type alias ParsedDeclarationsAndReferences =
    { declarations : List ( String, ( ParsedDeclaration, DeclarationScope ) )
    , references : List (Elm.Syntax.Node.Node ( List String, String ))
    }


type alias ParsedDeclaration =
    Declaration CookedDocumentation


{-| Documentation prepared as markdown string
-}
type CookedDocumentation
    = CookedDocumentation String


type LocationUnderFilePath
    = LocationUnderFilePath (List String) Range


type Range
    = Range ( Int, Int ) ( Int, Int )


initLanguageServiceState : List { moduleText : String, implicitImport : Bool } -> LanguageServiceState
initLanguageServiceState elmCoreModules =
    let
        elmCoreModulesParseResults : List ElmCoreModule
        elmCoreModulesParseResults =
            elmCoreModules
                |> List.filterMap
                    (\coreModule ->
                        coreModule.moduleText
                            |> Elm.Parser.parseToFile
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
    in
    { fileTreeParseCache = FileTree.TreeNode []
    , coreModulesCache = elmCoreModulesParseResults
    }


handleRequest :
    LanguageServiceInterface.RequestInWorkspace
    -> LanguageServiceState
    -> ( Result String LanguageServiceInterface.Response, LanguageServiceState )
handleRequest requestInWorkspace stateBefore =
    let
        languageServiceState =
            updateLanguageServiceState requestInWorkspace.workspace stateBefore
    in
    handleRequestInCurrentWorkspace
        requestInWorkspace.request
        languageServiceState


handleRequestInCurrentWorkspace :
    LanguageServiceInterface.Request
    -> LanguageServiceState
    -> ( Result String LanguageServiceInterface.Response, LanguageServiceState )
handleRequestInCurrentWorkspace request stateBefore =
    let
        ( serviceResponse, state ) =
            case request of
                LanguageServiceInterface.AddFileRequest filePath content ->
                    addFile filePath content stateBefore

                LanguageServiceInterface.DeleteFileRequest filePath ->
                    let
                        newFileTree =
                            case FileTree.removeNodeAtPath filePath stateBefore.fileTreeParseCache of
                                Nothing ->
                                    stateBefore.fileTreeParseCache

                                Just afterRemove ->
                                    afterRemove
                    in
                    ( LanguageServiceInterface.WorkspaceSummaryResponse
                    , { stateBefore | fileTreeParseCache = newFileTree }
                    )

                LanguageServiceInterface.ProvideHoverRequest provideHoverRequest ->
                    ( LanguageServiceInterface.ProvideHoverResponse
                        (provideHover
                            provideHoverRequest
                            stateBefore
                        )
                    , stateBefore
                    )

                LanguageServiceInterface.ProvideCompletionItemsRequest provideCompletionItemsRequest ->
                    ( LanguageServiceInterface.ProvideCompletionItemsResponse
                        (provideCompletionItems
                            provideCompletionItemsRequest
                            stateBefore
                        )
                    , stateBefore
                    )

                LanguageServiceInterface.ProvideDefinitionRequest provideDefinitionRequest ->
                    ( LanguageServiceInterface.ProvideDefinitionResponse
                        (provideDefinition
                            provideDefinitionRequest
                            stateBefore
                        )
                    , stateBefore
                    )
    in
    ( Ok serviceResponse
    , state
    )


addFile :
    List String
    -> LanguageServiceInterface.FileTreeBlobNode
    -> LanguageServiceState
    -> ( LanguageServiceInterface.Response, LanguageServiceState )
addFile filePath content stateBefore =
    let
        maybePreviousCached : Maybe LanguageServiceStateFileTreeNodeBlob
        maybePreviousCached =
            FileTree.getBlobAtPathFromFileTree filePath stateBefore.fileTreeParseCache

        maybeTextContent : Maybe FileTextContent
        maybeTextContent =
            case content.asText of
                Nothing ->
                    Nothing

                Just asString ->
                    let
                        parsedFile =
                            case Elm.Parser.parseToFile asString of
                                Err _ ->
                                    Nothing

                                Ok syntax ->
                                    Just
                                        { filePath = filePath
                                        , text = asString
                                        , syntax = syntax
                                        }
                    in
                    Just { text = asString, parsedFile = parsedFile }

        parsedFileFromPreviouslyCached : Maybe ParsedModuleCache
        parsedFileFromPreviouslyCached =
            case maybePreviousCached of
                Nothing ->
                    Nothing

                Just previousCached ->
                    previousCached.parsedFileLastSuccess

        parsedFileLastSuccess : Maybe ParsedModuleCache
        parsedFileLastSuccess =
            case maybeTextContent of
                Nothing ->
                    parsedFileFromPreviouslyCached

                Just textContent ->
                    case textContent.parsedFile of
                        Nothing ->
                            parsedFileFromPreviouslyCached

                        Just parsedFile ->
                            Just parsedFile

        newFileTree : FileTree.FileTreeNode LanguageServiceStateFileTreeNodeBlob
        newFileTree =
            FileTree.setNodeAtPathInSortedFileTree
                ( filePath
                , FileTree.BlobNode
                    { sourceBase64 = content.asBase64
                    , textContent = maybeTextContent
                    , parsedFileLastSuccess = parsedFileLastSuccess
                    }
                )
                stateBefore.fileTreeParseCache
    in
    ( LanguageServiceInterface.WorkspaceSummaryResponse
    , { stateBefore | fileTreeParseCache = newFileTree }
    )


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
                                rangeIntersectsLocation
                                    { row = request.positionLineNumber, column = request.positionColumn }
                                    hoverRange
                            )
                        |> List.map Tuple.second


type alias ImportedModule =
    { filePath : List String
    , canonicalName : List String
    , importedName : Elm.Syntax.ModuleName.ModuleName
    , parsedModule : Maybe ParsedModuleCache
    , referencesRanges : List Elm.Syntax.Range.Range
    }


hoverItemsFromParsedModule :
    ParsedModuleCache
    -> LanguageServiceState
    ->
        { fromDeclarations : List ( Elm.Syntax.Range.Range, LocationUnderFilePath, String )
        , hoverItems : List ( Elm.Syntax.Range.Range, String )
        }
hoverItemsFromParsedModule parsedModule languageServiceState =
    let
        textLines : List String
        textLines =
            String.lines parsedModule.text

        getTextLinesFromRange : Range -> List String
        getTextLinesFromRange range =
            sliceRangeFromTextLines textLines range

        importedModules : List ImportedModule
        importedModules =
            importedModulesFromFile parsedModule languageServiceState

        parsedDeclarationsAndReferences =
            listDeclarationsAndReferencesInFile parsedModule.syntax getTextLinesFromRange

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

        importedModulesCompletionItems :
            List
                ( List String
                , ( ImportedModule
                  , ModuleCompletionItems
                  )
                )
        importedModulesCompletionItems =
            importedModules
                |> List.map
                    (\importedModule ->
                        ( importedModule.importedName
                        , ( importedModule
                          , case importedModule.parsedModule of
                                Nothing ->
                                    { fromTopLevel = [], fromLocals = [] }

                                Just importedParsed ->
                                    completionItemsFromModule importedParsed
                          )
                        )
                    )

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

        localModueItemsBeforeFiltering :
            List
                ( String
                , ( Maybe Elm.Syntax.Range.Range
                  , LocationUnderFilePath
                  , Frontend.MonacoEditor.MonacoCompletionItem
                  )
                )
        localModueItemsBeforeFiltering =
            localDeclarationsAndImportExposings
                |> List.map
                    (\( scope, completionItem ) ->
                        let
                            ( maybeFilterRange, completionItemRange ) =
                                case scope of
                                    TopLevelScope topLevelRange ->
                                        ( Nothing, topLevelRange )

                                    LocalScope scopeRange ->
                                        ( Just scopeRange, scopeRange )
                        in
                        ( completionItem.label
                        , ( maybeFilterRange
                          , LocationUnderFilePath
                                parsedModule.filePath
                                (Range
                                    ( completionItemRange.start.row, completionItemRange.start.column )
                                    ( completionItemRange.end.row, completionItemRange.end.column )
                                )
                          , completionItem
                          )
                        )
                    )

        getHoverForFunctionOrName :
            Elm.Syntax.Node.Node ( Elm.Syntax.ModuleName.ModuleName, String )
            -> Maybe ( LocationUnderFilePath, String )
        getHoverForFunctionOrName (Elm.Syntax.Node.Node functionOrNameNodeRange ( moduleName, nameInModule )) =
            if moduleName == [] then
                case Common.assocListGet nameInModule localModueItemsBeforeFiltering of
                    Nothing ->
                        Nothing

                    Just ( maybeFilterRange, locationUnderFilePath, completionItem ) ->
                        case maybeFilterRange of
                            Nothing ->
                                Just ( locationUnderFilePath, completionItem.documentation )

                            Just filterRange ->
                                if rangeIntersectsLocation functionOrNameNodeRange.start filterRange then
                                    Just ( locationUnderFilePath, completionItem.documentation )

                                else
                                    Nothing

            else
                case Common.assocListGet moduleName importedModulesCompletionItems of
                    Nothing ->
                        Nothing

                    Just ( referencedModule, moduleCompletionItems ) ->
                        moduleCompletionItems.fromTopLevel
                            |> Common.listFind
                                (\item ->
                                    item.completionItem.label == nameInModule && item.isExposed
                                )
                            |> Maybe.map
                                (\item ->
                                    ( LocationUnderFilePath
                                        referencedModule.filePath
                                        (Range
                                            ( item.range.start.row, item.range.start.column )
                                            ( item.range.end.row, item.range.end.column )
                                        )
                                    , item.completionItem.documentation
                                    )
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
                                |> List.filter
                                    (\importedModule ->
                                        importedModule.importedName == moduleName
                                    )
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
                                          , LocationUnderFilePath
                                                referencedModule.filePath
                                                (Range
                                                    ( rangeModulePart.start.row, rangeModulePart.start.column )
                                                    ( rangeModulePart.end.row, rangeModulePart.end.column )
                                                )
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
listTypeReferencesFromTypeAnnotation (Elm.Syntax.Node.Node _ typeAnnotation) =
    case typeAnnotation of
        Elm.Syntax.TypeAnnotation.GenericType _ ->
            []

        Elm.Syntax.TypeAnnotation.Typed instantiated arguments ->
            instantiated :: List.concatMap listTypeReferencesFromTypeAnnotation arguments

        Elm.Syntax.TypeAnnotation.Unit ->
            []

        Elm.Syntax.TypeAnnotation.Tupled tupled ->
            List.concatMap listTypeReferencesFromTypeAnnotation tupled

        Elm.Syntax.TypeAnnotation.Record record ->
            List.concatMap (\(Elm.Syntax.Node.Node _ ( _, value )) -> listTypeReferencesFromTypeAnnotation value)
                record

        Elm.Syntax.TypeAnnotation.GenericRecord _ (Elm.Syntax.Node.Node _ record) ->
            List.concatMap
                (\(Elm.Syntax.Node.Node _ ( _, value )) -> listTypeReferencesFromTypeAnnotation value)
                record

        Elm.Syntax.TypeAnnotation.FunctionTypeAnnotation input return ->
            List.concatMap listTypeReferencesFromTypeAnnotation [ input, return ]


{-| <https://microsoft.github.io/monaco-editor/typedoc/interfaces/languages.CompletionItemProvider.html#provideCompletionItems>
-}
provideCompletionItems :
    LanguageServiceInterface.ProvideCompletionItemsRequestStruct
    -> LanguageServiceState
    -> List Frontend.MonacoEditor.MonacoCompletionItem
provideCompletionItems request languageServiceState =
    case
        FileTree.getBlobAtPathFromFileTree request.filePathOpenedInEditor
            languageServiceState.fileTreeParseCache
    of
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
                                        , column = request.cursorColumn
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
                        let
                            text : String
                            text =
                                case currentFileCacheItem.textContent of
                                    Nothing ->
                                        fileOpenedInEditor.text

                                    Just textContent ->
                                        textContent.text
                        in
                        provideCompletionItemsInModule
                            { fileOpenedInEditor = fileOpenedInEditor
                            , newText = text
                            , cursorLineNumber = request.cursorLineNumber
                            , cursorColumn = request.cursorColumn
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
                            (\( refRange, LocationUnderFilePath filePath (Range ( startRow, startColumn ) ( endRow, endColumn )), _ ) ->
                                if
                                    rangeIntersectsLocation
                                        { row = request.positionLineNumber, column = request.positionColumn }
                                        refRange
                                then
                                    Just
                                        { filePath = filePath
                                        , range =
                                            { startLineNumber = startRow
                                            , startColumn = startColumn
                                            , endLineNumber = endRow
                                            , endColumn = endColumn
                                            }
                                        }

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
    { fileOpenedInEditor : ParsedModuleCache, newText : String, cursorLineNumber : Int, cursorColumn : Int }
    -> LanguageServiceState
    -> List Frontend.MonacoEditor.MonacoCompletionItem
provideCompletionItemsInModule request languageServiceState =
    let
        (Elm.Syntax.Node.Node _ moduleDefinition) =
            request.fileOpenedInEditor.syntax.moduleDefinition

        fileOpenedInEditorModuleName : Elm.Syntax.ModuleName.ModuleName
        fileOpenedInEditorModuleName =
            Elm.Syntax.Module.moduleName moduleDefinition

        lineText : String
        lineText =
            request.newText
                |> String.lines
                |> List.drop (request.cursorLineNumber - 1)
                |> List.head
                |> Maybe.withDefault ""

        lineUntilPosition : String
        lineUntilPosition =
            lineText
                |> String.left (request.cursorColumn - 1)

        lineUntilPositionWords : List String
        lineUntilPositionWords =
            stringSplitByChar (\c -> not (charIsAllowedInDeclarationName c || c == '.')) lineUntilPosition

        completionPrefix : List String
        completionPrefix =
            case List.reverse lineUntilPositionWords of
                [] ->
                    []

                word :: _ ->
                    List.drop 1 (List.reverse (String.split "." word))

        completionPrefixIsNamespace : Bool
        completionPrefixIsNamespace =
            case completionPrefix of
                [] ->
                    True

                prefixFirstElement :: _ ->
                    case String.uncons prefixFirstElement of
                        Nothing ->
                            True

                        Just ( firstChar, _ ) ->
                            Char.isUpper firstChar

        modulesAvailableForImport : List ParsedModuleCache
        modulesAvailableForImport =
            modulesAvailableForImportFromState languageServiceState

        moduleNamesToNotSuggestForImport : List Elm.Syntax.ModuleName.ModuleName
        moduleNamesToNotSuggestForImport =
            [ fileOpenedInEditorModuleName ]

        modulesToSuggestForImport =
            modulesAvailableForImport
                |> List.map .syntax
                |> List.filterMap
                    (\availableModule ->
                        let
                            (Elm.Syntax.Node.Node _ availableModuleDefinition) =
                                availableModule.moduleDefinition

                            availableModuleName : Elm.Syntax.ModuleName.ModuleName
                            availableModuleName =
                                Elm.Syntax.Module.moduleName availableModuleDefinition
                        in
                        if List.member availableModuleName moduleNamesToNotSuggestForImport then
                            Nothing

                        else
                            Just ( availableModule, availableModuleName )
                    )
                |> List.sortBy
                    (\( _, availableModuleName ) ->
                        String.join "." availableModuleName
                    )

        importedModules =
            importedModulesFromFile request.fileOpenedInEditor languageServiceState

        currentModuleDeclarations =
            completionItemsFromModule request.fileOpenedInEditor

        fromLocals : List Frontend.MonacoEditor.MonacoCompletionItem
        fromLocals =
            currentModuleDeclarations.fromLocals
                |> List.filterMap
                    (\completionItem ->
                        if
                            rangeIntersectsLocation
                                { row = request.cursorLineNumber, column = String.length lineUntilPosition }
                                completionItem.range
                        then
                            Just completionItem.completionItem

                        else
                            Nothing
                    )

        importExposings : List Frontend.MonacoEditor.MonacoCompletionItem
        importExposings =
            List.concat
                [ importExposingsFromFile request.fileOpenedInEditor languageServiceState
                , List.map Tuple.second (commonImplicitTopLevelImports languageServiceState)
                ]

        localDeclarationsAndImportExposings : List Frontend.MonacoEditor.MonacoCompletionItem
        localDeclarationsAndImportExposings =
            List.concat
                [ List.map .completionItem currentModuleDeclarations.fromTopLevel
                , importExposings
                , fromLocals
                ]

        localDeclarationsAfterPrefix : List Frontend.MonacoEditor.MonacoCompletionItem
        localDeclarationsAfterPrefix =
            if completionPrefix == [] then
                localDeclarationsAndImportExposings

            else
                case
                    importedModules
                        |> List.filter
                            (\importedModule ->
                                importedModule.importedName == completionPrefix
                            )
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
                        case importedModule.parsedModule of
                            Nothing ->
                                Nothing

                            Just importedModuleParsed ->
                                Just
                                    (moduleCompletionItemFromModuleSyntax
                                        { importedName = Just importedModule.importedName
                                        , importedModuleNameRestAfterPrefix = Just importedModuleNameRestAfterPrefix
                                        }
                                        importedModuleParsed.syntax
                                    )
                    )
    in
    case lineUntilPositionWords of
        "import" :: _ ->
            modulesToSuggestForImport
                |> List.map
                    (\( availableModule, _ ) ->
                        moduleCompletionItemFromModuleSyntax
                            { importedModuleNameRestAfterPrefix = Nothing, importedName = Nothing }
                            availableModule
                    )

        _ ->
            if completionPrefixIsNamespace then
                List.concat
                    [ fromImports
                    , List.sortBy .insertText localDeclarationsAfterPrefix
                    ]

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
                            (Elm.Syntax.Node.Node _ moduleDefinition) =
                                coreModule.parseResult.syntax.moduleDefinition

                            canonicalName : Elm.Syntax.ModuleName.ModuleName
                            canonicalName =
                                Elm.Syntax.Module.moduleName moduleDefinition
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
                |> Common.listFind
                    (\moduleAvailable ->
                        let
                            (Elm.Syntax.Node.Node _ moduleDefinition) =
                                moduleAvailable.syntax.moduleDefinition
                        in
                        if Elm.Syntax.Module.moduleName moduleDefinition == canonicalModuleName then
                            True

                        else
                            False
                    )

        explicitlyImportedModules =
            fileOpenedInEditor.syntax.imports
                |> List.filterMap
                    (\(Elm.Syntax.Node.Node _ importSyntax) ->
                        let
                            (Elm.Syntax.Node.Node moduleNameRange canonicalName) =
                                importSyntax.moduleName

                            importedName : List String
                            importedName =
                                case importSyntax.moduleAlias of
                                    Nothing ->
                                        canonicalName

                                    Just (Elm.Syntax.Node.Node _ moduleAlias) ->
                                        moduleAlias
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
                                    , referencesRanges = [ moduleNameRange ]
                                    }
                    )
    in
    List.concat [ implicitlyImportedModules, explicitlyImportedModules ]


modulesAvailableForImportFromState : LanguageServiceState -> List ParsedModuleCache
modulesAvailableForImportFromState languageServiceState =
    List.concat
        [ languageServiceState.fileTreeParseCache
            |> FileTree.flatListOfBlobsFromFileTreeNode
            |> List.filterMap
                (\( _, fileCache ) ->
                    fileCache.parsedFileLastSuccess
                )
        , List.map .parseResult languageServiceState.coreModulesCache
        ]


importExposingsFromFile :
    ParsedModuleCache
    -> LanguageServiceState
    -> List Frontend.MonacoEditor.MonacoCompletionItem
importExposingsFromFile fileOpenedInEditor languageServiceState =
    fileOpenedInEditor.syntax.imports
        |> List.concatMap
            (\(Elm.Syntax.Node.Node _ importSyntax) ->
                case importSyntax.exposingList of
                    Nothing ->
                        []

                    Just (Elm.Syntax.Node.Node _ exposingList) ->
                        let
                            (Elm.Syntax.Node.Node _ canonicalName) =
                                importSyntax.moduleName
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
                                                let
                                                    (Elm.Syntax.Node.Node _ candidateModuleDefinition) =
                                                        moduleCandidate.syntax.moduleDefinition
                                                in
                                                if Elm.Syntax.Module.moduleName candidateModuleDefinition == canonicalName then
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
                                case exposingList of
                                    Elm.Syntax.Exposing.All _ ->
                                        importedModuleItems

                                    Elm.Syntax.Exposing.Explicit topLevelExposings ->
                                        topLevelExposings
                                            |> List.concatMap
                                                (\(Elm.Syntax.Node.Node _ topLevelExpose) ->
                                                    let
                                                        exposedName : String
                                                        exposedName =
                                                            case topLevelExpose of
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
                                                        |> List.filter
                                                            (\{ insertText } ->
                                                                insertText == exposedName
                                                            )
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

                    moduleName : Elm.Syntax.ModuleName.ModuleName
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
        (Elm.Syntax.Node.Node _ moduleDefinition) =
            moduleSyntax.moduleDefinition

        canonicalName : Elm.Syntax.ModuleName.ModuleName
        canonicalName =
            Elm.Syntax.Module.moduleName moduleDefinition

        insertText : String
        insertText =
            case importedModuleNameRestAfterPrefix of
                Nothing ->
                    String.join "." canonicalName

                Just moduleNameRestAfterPrefix ->
                    String.join "." moduleNameRestAfterPrefix

        label : String
        label =
            case importedName of
                Nothing ->
                    insertText

                Just importedName_ ->
                    if importedName_ == canonicalName then
                        insertText

                    else
                        String.join "." canonicalName ++ " as " ++ insertText

        documentation : String
        documentation =
            case documentationStringFromModuleSyntax moduleSyntax of
                Nothing ->
                    ""

                Just documentationString ->
                    documentationString
    in
    { label = label
    , documentation = documentation
    , insertText = insertText
    , kind = Frontend.MonacoEditor.ModuleCompletionItemKind
    }


documentationStringFromModuleSyntax : Elm.Syntax.File.File -> Maybe String
documentationStringFromModuleSyntax parsedModule =
    let
        (Elm.Syntax.Node.Node moduleDefinitionRange _) =
            parsedModule.moduleDefinition

        importsAndDeclarationsRanges : List Elm.Syntax.Range.Range
        importsAndDeclarationsRanges =
            List.concat
                [ List.map (\(Elm.Syntax.Node.Node range _) -> range) parsedModule.imports
                , List.map (\(Elm.Syntax.Node.Node range _) -> range) parsedModule.declarations
                ]

        importsAndDeclarationsRange : Elm.Syntax.Range.Range
        importsAndDeclarationsRange =
            Elm.Syntax.Range.combine importsAndDeclarationsRanges

        maybeModuleComment : Maybe (Elm.Syntax.Node.Node Elm.Syntax.Comments.Comment)
        maybeModuleComment =
            List.foldl
                (\comment maybeComment ->
                    let
                        (Elm.Syntax.Node.Node commentRange _) =
                            comment
                    in
                    case maybeComment of
                        Nothing ->
                            if
                                (commentRange.start.row > moduleDefinitionRange.start.row)
                                    && (commentRange.start.row < importsAndDeclarationsRange.start.row)
                            then
                                Just comment

                            else
                                Nothing

                        Just prevComment ->
                            let
                                (Elm.Syntax.Node.Node prevCommentRange _) =
                                    prevComment
                            in
                            if
                                (commentRange.start.row > prevCommentRange.end.row)
                                    && (commentRange.start.row < importsAndDeclarationsRange.start.row)
                            then
                                Just comment

                            else
                                Just prevComment
                )
                Nothing
                parsedModule.comments
    in
    case maybeModuleComment of
        Nothing ->
            Nothing

        Just (Elm.Syntax.Node.Node _ commentNode) ->
            Just (removeWrappingFromMultilineComment commentNode)


type alias ModuleCompletionItems =
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


completionItemsFromModule : ParsedModuleCache -> ModuleCompletionItems
completionItemsFromModule moduleCache =
    let
        textLines : List String
        textLines =
            String.lines moduleCache.text

        getTextLinesFromRange : Range -> List String
        getTextLinesFromRange range =
            sliceRangeFromTextLines textLines range

        (Elm.Syntax.Node.Node _ moduleDefinition) =
            moduleCache.syntax.moduleDefinition

        exposingListNode : Elm.Syntax.Node.Node Elm.Syntax.Exposing.Exposing
        exposingListNode =
            case moduleDefinition of
                Elm.Syntax.Module.EffectModule effectModule ->
                    effectModule.exposingList

                Elm.Syntax.Module.NormalModule normalModule ->
                    normalModule.exposingList

                Elm.Syntax.Module.PortModule portModule ->
                    portModule.exposingList

        (Elm.Syntax.Node.Node _ exposingList) =
            exposingListNode

        exposesFunction : String -> Bool
        exposesFunction functionName =
            Elm.Syntax.Exposing.exposesFunction functionName exposingList

        exposesTypeOrAlias : String -> Bool
        exposesTypeOrAlias name =
            case exposingList of
                Elm.Syntax.Exposing.All _ ->
                    True

                Elm.Syntax.Exposing.Explicit topLovelExposings ->
                    topLovelExposings
                        |> List.any
                            (\(Elm.Syntax.Node.Node _ topLevelExpose) ->
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

        parsedDeclarationsAndReferences : ParsedDeclarationsAndReferences
        parsedDeclarationsAndReferences =
            listDeclarationsAndReferencesInFile moduleCache.syntax getTextLinesFromRange

        buildCompletionItems : String -> ParsedDeclaration -> List Frontend.MonacoEditor.MonacoCompletionItem
        buildCompletionItems =
            completionItemsFromParsedDeclarationOrReference

        fromTopLevel :
            List
                { completionItem : Frontend.MonacoEditor.MonacoCompletionItem
                , isExposed : Bool
                , range : Elm.Syntax.Range.Range
                }
        fromTopLevel =
            parsedDeclarationsAndReferences.declarations
                |> List.concatMap
                    (\( declName, ( declOrRef, scope ) ) ->
                        case scope of
                            TopLevelScope scopeRange ->
                                buildCompletionItems declName declOrRef
                                    |> List.map
                                        (\completionItem ->
                                            { completionItem = completionItem
                                            , isExposed =
                                                case declOrRef of
                                                    FunctionOrValueDeclaration _ ->
                                                        exposesFunction declName

                                                    TypeAliasDeclaration _ ->
                                                        exposesTypeOrAlias declName

                                                    ChoiceTypeDeclaration _ _ ->
                                                        exposesTypeOrAlias declName
                                            , range = scopeRange
                                            }
                                        )

                            LocalScope _ ->
                                []
                    )

        fromLocals : List { completionItem : Frontend.MonacoEditor.MonacoCompletionItem, range : Elm.Syntax.Range.Range }
        fromLocals =
            parsedDeclarationsAndReferences.declarations
                |> List.concatMap
                    (\( declName, ( declOrRef, scope ) ) ->
                        case scope of
                            TopLevelScope _ ->
                                []

                            LocalScope range ->
                                buildCompletionItems declName declOrRef
                                    |> List.map (\completionItem -> { completionItem = completionItem, range = range })
                    )
    in
    { fromTopLevel = fromTopLevel
    , fromLocals = fromLocals
    }


completionItemsFromParsedDeclarationOrReference :
    String
    -> ParsedDeclaration
    -> List Frontend.MonacoEditor.MonacoCompletionItem
completionItemsFromParsedDeclarationOrReference declName declarationOrReference =
    let
        buildCompletionItem { name, markdown, kind } =
            { label = name
            , documentation = markdown
            , insertText = name
            , kind = kind
            }
    in
    case declarationOrReference of
        FunctionOrValueDeclaration (CookedDocumentation markdown) ->
            [ buildCompletionItem
                { name = declName
                , markdown = markdown
                , kind = Frontend.MonacoEditor.FunctionCompletionItemKind
                }
            ]

        TypeAliasDeclaration (CookedDocumentation markdown) ->
            [ buildCompletionItem
                { name = declName
                , markdown = markdown
                , kind = Frontend.MonacoEditor.StructCompletionItemKind
                }
            ]

        ChoiceTypeDeclaration (CookedDocumentation choiceTypeMarkdown) tags ->
            buildCompletionItem
                { name = declName
                , markdown = choiceTypeMarkdown
                , kind = Frontend.MonacoEditor.EnumCompletionItemKind
                }
                :: List.map
                    (\( tagName, CookedDocumentation tagMarkdown ) ->
                        buildCompletionItem
                            { name = tagName
                            , markdown = tagMarkdown
                            , kind = Frontend.MonacoEditor.EnumMemberCompletionItemKind
                            }
                    )
                    tags


documentationMarkdownFromCodeLinesAndDocumentation : List String -> Maybe String -> String
documentationMarkdownFromCodeLinesAndDocumentation codeLines maybeDocumentation =
    let
        lessDocumentation =
            markdownElmCodeBlockFromCodeLines codeLines
    in
    case maybeDocumentation of
        Nothing ->
            lessDocumentation

        Just documentation ->
            String.concat
                [ lessDocumentation
                , "\n\n"
                , documentation
                ]


markdownElmCodeBlockFromCodeLines : List String -> String
markdownElmCodeBlockFromCodeLines codeLines =
    String.concat
        [ "    "
        , String.join "\n    " codeLines
        ]


updateLanguageServiceState : LanguageServiceInterface.FileTreeNode -> LanguageServiceState -> LanguageServiceState
updateLanguageServiceState fileTree state =
    let
        compileFileCacheEntry ( blobPath, fileTreeBlob ) =
            let
                maybePreviousCached =
                    state.fileTreeParseCache |> FileTree.getBlobAtPathFromFileTree blobPath

                buildNewEntry () =
                    let
                        textContent : Maybe FileTextContent
                        textContent =
                            case fileTreeBlob.asText of
                                Nothing ->
                                    Nothing

                                Just asString ->
                                    let
                                        parsedFile =
                                            case Elm.Parser.parseToFile asString of
                                                Err _ ->
                                                    Nothing

                                                Ok syntax ->
                                                    Just
                                                        { filePath = blobPath
                                                        , text = asString
                                                        , syntax = syntax
                                                        }
                                    in
                                    Just { text = asString, parsedFile = parsedFile }

                        parsedFileFromPreviouslyCached =
                            case maybePreviousCached of
                                Nothing ->
                                    Nothing

                                Just previousCached ->
                                    previousCached.parsedFileLastSuccess
                    in
                    { sourceBase64 = fileTreeBlob.asBase64
                    , textContent = textContent
                    , parsedFileLastSuccess =
                        case textContent of
                            Nothing ->
                                parsedFileFromPreviouslyCached

                            Just fromTextContent ->
                                case fromTextContent.parsedFile of
                                    Nothing ->
                                        parsedFileFromPreviouslyCached

                                    Just parsedFile ->
                                        Just parsedFile
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


listDeclarationsAndReferencesInFile : Elm.Syntax.File.File -> (Range -> List String) -> ParsedDeclarationsAndReferences
listDeclarationsAndReferencesInFile fileSyntax getTextLinesFromRange =
    fileSyntax.declarations
        |> listConcatMapParsedDeclarationsAndReferences
            (listDeclarationsAndReferencesInDeclaration getTextLinesFromRange)


listDeclarationsAndReferencesInDeclaration :
    (Range -> List String)
    -> Elm.Syntax.Node.Node Elm.Syntax.Declaration.Declaration
    -> ParsedDeclarationsAndReferences
listDeclarationsAndReferencesInDeclaration getTextLinesFromRange declarationNode =
    let
        empty =
            { declarations = [], references = [] }

        (Elm.Syntax.Node.Node declarationRange declaration) =
            declarationNode
    in
    case declaration of
        Elm.Syntax.Declaration.FunctionDeclaration function ->
            listDeclarationsAndReferencesForFunction
                declarationRange
                function
                getTextLinesFromRange

        Elm.Syntax.Declaration.AliasDeclaration aliasDeclaration ->
            declarationsAndReferencesForAliasDeclaration
                declarationRange
                aliasDeclaration
                getTextLinesFromRange

        Elm.Syntax.Declaration.CustomTypeDeclaration choiceTypeDeclaration ->
            listDeclarationsAndReferencesFromTypeDeclaration
                declarationRange
                choiceTypeDeclaration
                getTextLinesFromRange

        Elm.Syntax.Declaration.PortDeclaration _ ->
            empty

        Elm.Syntax.Declaration.InfixDeclaration _ ->
            empty

        Elm.Syntax.Declaration.Destructuring _ _ ->
            empty


declarationsAndReferencesForAliasDeclaration :
    Elm.Syntax.Range.Range
    -> Elm.Syntax.TypeAlias.TypeAlias
    -> (Range -> List String)
    -> ParsedDeclarationsAndReferences
declarationsAndReferencesForAliasDeclaration declarationRange aliasDeclaration getTextLinesFromRange =
    let
        (Elm.Syntax.Node.Node _ aliasName) =
            aliasDeclaration.name
    in
    { declarations =
        [ ( aliasName
          , ( declarationOrReferenceForAliasDeclaration aliasDeclaration getTextLinesFromRange
            , TopLevelScope declarationRange
            )
          )
        ]
    , references = listTypeReferencesFromTypeAnnotation aliasDeclaration.typeAnnotation
    }


declarationOrReferenceForAliasDeclaration :
    Elm.Syntax.TypeAlias.TypeAlias
    -> (Range -> List String)
    -> ParsedDeclaration
declarationOrReferenceForAliasDeclaration aliasDeclaration getTextLinesFromRange =
    let
        (Elm.Syntax.Node.Node typeAnnotationRange _) =
            aliasDeclaration.typeAnnotation

        (Elm.Syntax.Node.Node aliasNameRange _) =
            aliasDeclaration.name

        documentationStringFromSyntax : Maybe String
        documentationStringFromSyntax =
            case aliasDeclaration.documentation of
                Nothing ->
                    Nothing

                Just (Elm.Syntax.Node.Node _ comment) ->
                    Just (removeWrappingFromMultilineComment comment)

        codeRange =
            [ aliasNameRange
            , typeAnnotationRange
            ]
                |> Elm.Syntax.Range.combine
                |> expandRangeToLineStart

        codeLines : List String
        codeLines =
            getTextLinesFromRange
                (Range
                    ( codeRange.start.row, codeRange.start.column )
                    ( codeRange.end.row, codeRange.end.column )
                )
    in
    TypeAliasDeclaration
        (CookedDocumentation
            (documentationMarkdownFromCodeLinesAndDocumentation
                codeLines
                documentationStringFromSyntax
            )
        )


listDeclarationsAndReferencesFromTypeDeclaration :
    Elm.Syntax.Range.Range
    -> Elm.Syntax.Type.Type
    -> (Range -> List String)
    -> ParsedDeclarationsAndReferences
listDeclarationsAndReferencesFromTypeDeclaration declarationRange choiceTypeDeclaration getTextLinesFromRange =
    let
        documentationStringFromSyntax : Maybe String
        documentationStringFromSyntax =
            case choiceTypeDeclaration.documentation of
                Nothing ->
                    Nothing

                Just (Elm.Syntax.Node.Node _ comment) ->
                    Just (removeWrappingFromMultilineComment comment)

        (Elm.Syntax.Node.Node nameRange _) =
            choiceTypeDeclaration.name

        constructorsRanges =
            choiceTypeDeclaration.constructors
                |> List.map (\(Elm.Syntax.Node.Node constructorRange _) -> constructorRange)

        codeRange : Elm.Syntax.Range.Range
        codeRange =
            nameRange
                :: constructorsRanges
                |> Elm.Syntax.Range.combine
                |> expandRangeToLineStart

        choiceTypeCodeLines : List String
        choiceTypeCodeLines =
            getTextLinesFromRange
                (Range
                    ( codeRange.start.row, codeRange.start.column )
                    ( codeRange.end.row, codeRange.end.column )
                )

        (Elm.Syntax.Node.Node _ choiceTypeName) =
            choiceTypeDeclaration.name

        tagsDeclarations : List ( String, CookedDocumentation )
        tagsDeclarations =
            choiceTypeDeclaration.constructors
                |> List.map
                    (\(Elm.Syntax.Node.Node _ constructor) ->
                        let
                            (Elm.Syntax.Node.Node _ tagName) =
                                constructor.name
                        in
                        ( tagName
                        , CookedDocumentation
                            ([ markdownElmCodeBlockFromCodeLines [ tagName ]
                             , "A variant of the choice type `" ++ choiceTypeName ++ "`"
                             , markdownElmCodeBlockFromCodeLines choiceTypeCodeLines
                             ]
                                |> String.join "\n\n"
                            )
                        )
                    )
    in
    { declarations =
        [ ( choiceTypeName
          , ( ChoiceTypeDeclaration
                (CookedDocumentation
                    (documentationMarkdownFromCodeLinesAndDocumentation
                        choiceTypeCodeLines
                        documentationStringFromSyntax
                    )
                )
                tagsDeclarations
            , TopLevelScope declarationRange
            )
          )
        ]
    , references =
        choiceTypeDeclaration.constructors
            |> List.concatMap
                (\(Elm.Syntax.Node.Node _ constructor) ->
                    List.concatMap listTypeReferencesFromTypeAnnotation constructor.arguments
                )
    }


listDeclarationsAndReferencesInExpression :
    (Range -> List String)
    -> Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression
    -> ParsedDeclarationsAndReferences
listDeclarationsAndReferencesInExpression getTextLinesFromRange (Elm.Syntax.Node.Node expressionRange expression) =
    let
        empty =
            { declarations = [], references = [] }
    in
    case expression of
        Elm.Syntax.Expression.UnitExpr ->
            empty

        Elm.Syntax.Expression.Application application ->
            application
                |> List.map (listDeclarationsAndReferencesInExpression getTextLinesFromRange)
                |> concatParsedDeclarationsAndReferences

        Elm.Syntax.Expression.OperatorApplication _ _ leftExpr rightExpr ->
            [ leftExpr, rightExpr ]
                |> List.map (listDeclarationsAndReferencesInExpression getTextLinesFromRange)
                |> concatParsedDeclarationsAndReferences

        Elm.Syntax.Expression.FunctionOrValue moduleName localName ->
            { declarations = []
            , references = [ Elm.Syntax.Node.Node expressionRange ( moduleName, localName ) ]
            }

        Elm.Syntax.Expression.IfBlock ifExpr thenExpr elseExpr ->
            [ ifExpr, thenExpr, elseExpr ]
                |> listConcatMapParsedDeclarationsAndReferences
                    (listDeclarationsAndReferencesInExpression getTextLinesFromRange)

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
            listDeclarationsAndReferencesInExpression getTextLinesFromRange negation

        Elm.Syntax.Expression.Literal _ ->
            empty

        Elm.Syntax.Expression.CharLiteral _ ->
            empty

        Elm.Syntax.Expression.TupledExpression tupled ->
            tupled
                |> listConcatMapParsedDeclarationsAndReferences
                    (listDeclarationsAndReferencesInExpression getTextLinesFromRange)

        Elm.Syntax.Expression.ParenthesizedExpression parenthesized ->
            listDeclarationsAndReferencesInExpression
                getTextLinesFromRange
                parenthesized

        Elm.Syntax.Expression.LetExpression letBlock ->
            listDeclarationsAndReferencesInLetBlock
                getTextLinesFromRange
                letBlock

        Elm.Syntax.Expression.CaseExpression caseBlock ->
            [ listDeclarationsAndReferencesInExpression
                getTextLinesFromRange
                caseBlock.expression
            , caseBlock.cases
                |> listConcatMapParsedDeclarationsAndReferences
                    (\( _, caseBranch ) ->
                        listDeclarationsAndReferencesInExpression getTextLinesFromRange caseBranch
                    )
            ]
                |> concatParsedDeclarationsAndReferences

        Elm.Syntax.Expression.LambdaExpression lambda ->
            listDeclarationsAndReferencesInExpression getTextLinesFromRange lambda.expression

        Elm.Syntax.Expression.RecordExpr recordExpr ->
            recordExpr
                |> listConcatMapParsedDeclarationsAndReferences
                    (\(Elm.Syntax.Node.Node _ ( _, recordField )) ->
                        listDeclarationsAndReferencesInExpression
                            getTextLinesFromRange
                            recordField
                    )

        Elm.Syntax.Expression.ListExpr listExpr ->
            listExpr
                |> listConcatMapParsedDeclarationsAndReferences
                    (listDeclarationsAndReferencesInExpression getTextLinesFromRange)

        Elm.Syntax.Expression.RecordAccess recordAccess _ ->
            listDeclarationsAndReferencesInExpression
                getTextLinesFromRange
                recordAccess

        Elm.Syntax.Expression.RecordAccessFunction _ ->
            empty

        Elm.Syntax.Expression.RecordUpdateExpression (Elm.Syntax.Node.Node recordNameRange recordName) recordUpdateExpression ->
            [ { references =
                    [ Elm.Syntax.Node.Node
                        recordNameRange
                        ( [], recordName )
                    ]
              , declarations = []
              }
            , recordUpdateExpression
                |> listConcatMapParsedDeclarationsAndReferences
                    (\(Elm.Syntax.Node.Node _ ( _, recordField )) ->
                        listDeclarationsAndReferencesInExpression
                            getTextLinesFromRange
                            recordField
                    )
            ]
                |> concatParsedDeclarationsAndReferences

        Elm.Syntax.Expression.GLSLExpression _ ->
            empty


listDeclarationsAndReferencesForFunction :
    Elm.Syntax.Range.Range
    -> Elm.Syntax.Expression.Function
    -> (Range -> List String)
    -> ParsedDeclarationsAndReferences
listDeclarationsAndReferencesForFunction declRange function getTextLinesFromRange =
    let
        (Elm.Syntax.Node.Node functionDeclarationRange functionDeclaration) =
            function.declaration

        (Elm.Syntax.Node.Node _ functionName) =
            functionDeclaration.name

        documentationStringFromSyntax : Maybe String
        documentationStringFromSyntax =
            case function.documentation of
                Nothing ->
                    Nothing

                Just (Elm.Syntax.Node.Node _ comment) ->
                    Just (removeWrappingFromMultilineComment comment)

        maybeTypeAnnotationText : Maybe String
        maybeTypeAnnotationText =
            case function.signature of
                Nothing ->
                    Nothing

                Just (Elm.Syntax.Node.Node _ signature) ->
                    let
                        (Elm.Syntax.Node.Node typeAnnotationRange _) =
                            signature.typeAnnotation
                    in
                    Just
                        (Range
                            ( typeAnnotationRange.start.row, typeAnnotationRange.start.column )
                            ( typeAnnotationRange.end.row, typeAnnotationRange.end.column )
                            |> getTextLinesFromRange
                            |> String.join " "
                        )

        codeLines : List String
        codeLines =
            case maybeTypeAnnotationText of
                Nothing ->
                    [ functionName
                    ]

                Just typeAnnotationText ->
                    [ String.concat [ functionName, " : ", typeAnnotationText ]
                    ]

        functionItem : Declaration CookedDocumentation
        functionItem =
            FunctionOrValueDeclaration
                (CookedDocumentation
                    (documentationMarkdownFromCodeLinesAndDocumentation
                        codeLines
                        documentationStringFromSyntax
                    )
                )

        signatureReferences : List (Elm.Syntax.Node.Node ( Elm.Syntax.ModuleName.ModuleName, String ))
        signatureReferences =
            case function.signature of
                Nothing ->
                    []

                Just (Elm.Syntax.Node.Node _ signature) ->
                    listTypeReferencesFromTypeAnnotation signature.typeAnnotation

        expressionNode : Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression
        expressionNode =
            functionDeclaration.expression

        getTypeAnnotationFromArgumentIndex argumentIndex =
            case function.signature of
                Nothing ->
                    Nothing

                Just (Elm.Syntax.Node.Node _ signature) ->
                    signature.typeAnnotation
                        |> getTypeAnnotationFromFunctionArgumentIndex argumentIndex

        arguments : ParsedDeclarationsAndReferences
        arguments =
            functionDeclaration.arguments
                |> List.indexedMap
                    (\argumentIndex argument ->
                        listDeclarationsAndReferencesFromPattern
                            { typeAnnotation = getTypeAnnotationFromArgumentIndex argumentIndex }
                            getTextLinesFromRange
                            argument
                    )
                |> concatParsedDeclarationsAndReferences
    in
    [ { declarations =
            [ ( functionName
              , ( functionItem, TopLevelScope declRange )
              )
            ]
      , references = signatureReferences
      }
    , [ arguments
      , listDeclarationsAndReferencesInExpression
            getTextLinesFromRange
            expressionNode
      ]
        |> concatParsedDeclarationsAndReferences
        |> constrainDeclarationsScopesToRange functionDeclarationRange
    ]
        |> concatParsedDeclarationsAndReferences


getTypeAnnotationFromFunctionArgumentIndex :
    Int
    -> Elm.Syntax.Node.Node Elm.Syntax.TypeAnnotation.TypeAnnotation
    -> Maybe (Elm.Syntax.Node.Node Elm.Syntax.TypeAnnotation.TypeAnnotation)
getTypeAnnotationFromFunctionArgumentIndex argumentIndex ((Elm.Syntax.Node.Node _ typeAnnotation) as typeAnnotationNode) =
    case typeAnnotation of
        Elm.Syntax.TypeAnnotation.FunctionTypeAnnotation argumentType returnType ->
            if argumentIndex < 1 then
                Just argumentType

            else
                getTypeAnnotationFromFunctionArgumentIndex (argumentIndex - 1) returnType

        _ ->
            if argumentIndex < 1 then
                Just typeAnnotationNode

            else
                Nothing


listDeclarationsAndReferencesFromPattern :
    { typeAnnotation : Maybe (Elm.Syntax.Node.Node Elm.Syntax.TypeAnnotation.TypeAnnotation) }
    -> (Range -> List String)
    -> Elm.Syntax.Node.Node Elm.Syntax.Pattern.Pattern
    -> ParsedDeclarationsAndReferences
listDeclarationsAndReferencesFromPattern config getTextLinesFromRange (Elm.Syntax.Node.Node patternRange pattern) =
    case pattern of
        Elm.Syntax.Pattern.TuplePattern tuplePattern ->
            listConcatMapParsedDeclarationsAndReferences
                (listDeclarationsAndReferencesFromPattern { typeAnnotation = Nothing } getTextLinesFromRange)
                tuplePattern

        Elm.Syntax.Pattern.UnConsPattern head tail ->
            listConcatMapParsedDeclarationsAndReferences
                (listDeclarationsAndReferencesFromPattern { typeAnnotation = Nothing } getTextLinesFromRange)
                [ head, tail ]

        Elm.Syntax.Pattern.ListPattern listPattern ->
            listConcatMapParsedDeclarationsAndReferences
                (listDeclarationsAndReferencesFromPattern { typeAnnotation = Nothing } getTextLinesFromRange)
                listPattern

        Elm.Syntax.Pattern.VarPattern name ->
            let
                maybeTypeAnnotationText : Maybe String
                maybeTypeAnnotationText =
                    case config.typeAnnotation of
                        Nothing ->
                            Nothing

                        Just (Elm.Syntax.Node.Node typeAnnotationRange _) ->
                            Just
                                (Range
                                    ( typeAnnotationRange.start.row, typeAnnotationRange.start.column )
                                    ( typeAnnotationRange.end.row, typeAnnotationRange.end.column )
                                    |> getTextLinesFromRange
                                    |> String.join " "
                                )

                codeLines : List String
                codeLines =
                    case maybeTypeAnnotationText of
                        Nothing ->
                            [ name
                            ]

                        Just typeAnnotationText ->
                            [ String.concat [ name, " : ", typeAnnotationText ]
                            ]
            in
            { declarations =
                [ ( name
                  , ( FunctionOrValueDeclaration
                        (CookedDocumentation
                            (documentationMarkdownFromCodeLinesAndDocumentation codeLines Nothing)
                        )
                    , TopLevelScope patternRange
                    )
                  )
                ]
            , references = []
            }

        Elm.Syntax.Pattern.NamedPattern named arguments ->
            [ { declarations = []
              , references = [ Elm.Syntax.Node.Node patternRange ( named.moduleName, named.name ) ]
              }
            , listConcatMapParsedDeclarationsAndReferences
                (listDeclarationsAndReferencesFromPattern { typeAnnotation = Nothing } getTextLinesFromRange)
                arguments
            ]
                |> concatParsedDeclarationsAndReferences

        Elm.Syntax.Pattern.ParenthesizedPattern parenthesized ->
            listDeclarationsAndReferencesFromPattern { typeAnnotation = Nothing }
                getTextLinesFromRange
                parenthesized

        _ ->
            { declarations = [], references = [] }


listDeclarationsAndReferencesInLetBlock :
    (Range -> List String)
    -> Elm.Syntax.Expression.LetBlock
    -> ParsedDeclarationsAndReferences
listDeclarationsAndReferencesInLetBlock getTextLinesFromRange letBlock =
    [ listConcatMapParsedDeclarationsAndReferences
        (listDeclarationsAndReferencesInLetDeclaration getTextLinesFromRange)
        letBlock.declarations
    , listDeclarationsAndReferencesInExpression getTextLinesFromRange letBlock.expression
    ]
        |> concatParsedDeclarationsAndReferences


listDeclarationsAndReferencesInLetDeclaration :
    (Range -> List String)
    -> Elm.Syntax.Node.Node Elm.Syntax.Expression.LetDeclaration
    -> ParsedDeclarationsAndReferences
listDeclarationsAndReferencesInLetDeclaration getTextLinesFromRange declarationNode =
    let
        (Elm.Syntax.Node.Node declarationRange declaration) =
            declarationNode
    in
    case declaration of
        Elm.Syntax.Expression.LetFunction function ->
            listDeclarationsAndReferencesForFunction declarationRange function getTextLinesFromRange

        Elm.Syntax.Expression.LetDestructuring _ letDestructuring ->
            listDeclarationsAndReferencesInExpression getTextLinesFromRange letDestructuring


constrainDeclarationsScopesToRange : Elm.Syntax.Range.Range -> ParsedDeclarationsAndReferences -> ParsedDeclarationsAndReferences
constrainDeclarationsScopesToRange range declarationsAndReferences =
    { declarationsAndReferences
        | declarations =
            declarationsAndReferences.declarations
                |> List.map
                    (\( declName, ( decl, declScope ) ) ->
                        ( declName, ( decl, constrainScopeToRange range declScope ) )
                    )
    }


constrainScopeToRange : Elm.Syntax.Range.Range -> DeclarationScope -> DeclarationScope
constrainScopeToRange range scope =
    case scope of
        LocalScope _ ->
            scope

        TopLevelScope _ ->
            LocalScope range


listConcatMapParsedDeclarationsAndReferences :
    (a -> ParsedDeclarationsAndReferences)
    -> List a
    -> ParsedDeclarationsAndReferences
listConcatMapParsedDeclarationsAndReferences map =
    List.map map >> concatParsedDeclarationsAndReferences


concatParsedDeclarationsAndReferences : List ParsedDeclarationsAndReferences -> ParsedDeclarationsAndReferences
concatParsedDeclarationsAndReferences list =
    { declarations = List.concatMap .declarations list
    , references = List.concatMap .references list
    }


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
        trimmed : String
        trimmed =
            String.trim withWrapping

        lessPrefix : String
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
        |> List.any
            (\(Elm.Syntax.Node.Node commentRange comment) ->
                -- Map ranges of single-line comments to cover more of line in `rangeIntersectsLocation`
                let
                    range =
                        if String.startsWith "--" comment then
                            { commentRange
                                | end = { row = commentRange.end.row, column = commentRange.end.column + 9999 }
                            }

                        else
                            commentRange
                in
                rangeIntersectsLocation location range
            )


listCommentsInFile : Elm.Syntax.File.File -> List (Elm.Syntax.Node.Node Elm.Syntax.Comments.Comment)
listCommentsInFile parsedModule =
    let
        fromDeclarations =
            parsedModule.declarations
                |> List.concatMap
                    (\(Elm.Syntax.Node.Node _ declaration) ->
                        listCommentsFromDeclaration declaration
                    )
    in
    List.concat
        [ parsedModule.comments
        , fromDeclarations
        ]


listCommentsFromDeclaration :
    Elm.Syntax.Declaration.Declaration
    -> List (Elm.Syntax.Node.Node Elm.Syntax.Comments.Comment)
listCommentsFromDeclaration declaration =
    case declaration of
        Elm.Syntax.Declaration.FunctionDeclaration function ->
            case function.documentation of
                Nothing ->
                    []

                Just documentation ->
                    [ documentation ]

        Elm.Syntax.Declaration.AliasDeclaration typeAlias ->
            case typeAlias.documentation of
                Nothing ->
                    []

                Just documentation ->
                    [ documentation ]

        Elm.Syntax.Declaration.CustomTypeDeclaration typeDeclaration ->
            case typeDeclaration.documentation of
                Nothing ->
                    []

                Just documentation ->
                    [ documentation ]

        Elm.Syntax.Declaration.PortDeclaration _ ->
            []

        Elm.Syntax.Declaration.InfixDeclaration _ ->
            []

        Elm.Syntax.Declaration.Destructuring _ _ ->
            []


rangeIntersectsLocation : Elm.Syntax.Range.Location -> Elm.Syntax.Range.Range -> Bool
rangeIntersectsLocation location range =
    let
        start =
            range.start

        end =
            range.end

        startRow : Int
        startRow =
            start.row

        endRow : Int
        endRow =
            end.row

        locationRow : Int
        locationRow =
            location.row

        startColumn : Int
        startColumn =
            start.column
    in
    if locationRow < startRow then
        False

    else if locationRow > endRow then
        False

    else if locationRow == startRow && locationRow == endRow then
        if location.column < startColumn then
            False

        else if location.column > end.column then
            False

        else
            True

    else if locationRow == startRow then
        if location.column < startColumn then
            False

        else
            True

    else if locationRow == endRow then
        if location.column > end.column then
            False

        else
            True

    else
        True


sliceRangeFromTextLines : List String -> Range -> List String
sliceRangeFromTextLines textLines (Range ( startRow, startColumn ) ( endRow, endColumn )) =
    let
        startRowInt : Int
        startRowInt =
            startRow - 1

        endRowInt : Int
        endRowInt =
            endRow - 1

        startColumnInt : Int
        startColumnInt =
            startColumn - 1

        endColumnInt : Int
        endColumnInt =
            endColumn - 1

        rangeRowCount : Int
        rangeRowCount =
            endRowInt - startRowInt

        linesFromStart : List String
        linesFromStart =
            List.drop startRowInt textLines
    in
    if rangeRowCount == 0 then
        case linesFromStart of
            [] ->
                []

            line :: _ ->
                [ String.slice startColumnInt endColumnInt line ]

    else
        let
            firstLine : String
            firstLine =
                case linesFromStart of
                    [] ->
                        ""

                    line :: _ ->
                        String.dropLeft startColumnInt line

            lastLine : String
            lastLine =
                case List.drop rangeRowCount linesFromStart of
                    [] ->
                        ""

                    line :: _ ->
                        String.left endColumnInt line

            middleLines : List String
            middleLines =
                List.take
                    (rangeRowCount - 1)
                    (List.drop 1 linesFromStart)
        in
        List.concat
            [ [ firstLine ]
            , middleLines
            , [ lastLine ]
            ]


syntaxRangeCoveringCompleteString : String -> Elm.Syntax.Range.Range
syntaxRangeCoveringCompleteString string =
    let
        lines =
            String.lines string
    in
    { start = { row = 1, column = 1 }
    , end =
        { row = List.length lines + 1
        , column =
            case lines |> List.reverse |> List.head of
                Nothing ->
                    1

                Just lastLine ->
                    String.length lastLine + 1
        }
    }


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
stringSplitByChar charSplits string =
    List.map String.fromList
        (listCharSplitByChar charSplits (String.toList string))


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
