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
    , parsedFileLastSuccess : Maybe ParsedCookedModuleCache
    }


type alias FileTextContent =
    { text : String
    , parsedFile : Maybe ParsedCookedModuleCache
    }


type alias ParsedModuleCache =
    { filePath : List String
    , text : String
    , syntax : Elm.Syntax.File.File
    }


type alias ParsedCookedModuleCache =
    { filePath : List String
    , text : String
    , syntax : Elm.Syntax.File.File
    , completionItems : ModuleCompletionItems
    }


type alias ElmCoreModule =
    { parseResult : ParsedCookedModuleCache
    , implicitImport : Bool
    }


type DeclarationScope
    = TopLevelScope Range
    | LocalScope Range


type Declaration documentation
    = FunctionOrValueDeclaration documentation
    | TypeAliasDeclaration documentation
    | ChoiceTypeDeclaration documentation (List ( String, documentation ))


type SourceContentForCooking
    = SourceContentForCooking
        -- Text lines of the source document
        (List String)


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


type CompletionItem
    = CompletionItem
        -- Label
        String
        -- Insert Text
        String
        -- Kind
        Frontend.MonacoEditor.CompletionItemKind
        -- Documentation
        String


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
                                        , completionItems =
                                            completionItemsFromModule
                                                { filePath = [ "elm-core" ]
                                                , text = coreModule.moduleText
                                                , syntax = syntax
                                                }
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

                LanguageServiceInterface.TextDocumentSymbolRequest filePath ->
                    ( LanguageServiceInterface.TextDocumentSymbolResponse
                        (textDocumentSymbol
                            filePath
                            stateBefore
                        )
                    , stateBefore
                    )

                LanguageServiceInterface.TextDocumentReferencesRequest referenceRequest ->
                    ( LanguageServiceInterface.TextDocumentReferencesResponse
                        (textDocumentReferences
                            referenceRequest
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
                                        , completionItems =
                                            completionItemsFromModule
                                                { filePath = filePath
                                                , text = asString
                                                , syntax = syntax
                                                }
                                        }
                    in
                    Just
                        { text = asString
                        , parsedFile = parsedFile
                        }

        parsedFileFromPreviouslyCached : Maybe ParsedCookedModuleCache
        parsedFileFromPreviouslyCached =
            case maybePreviousCached of
                Nothing ->
                    Nothing

                Just previousCached ->
                    previousCached.parsedFileLastSuccess

        parsedFileLastSuccess : Maybe ParsedCookedModuleCache
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
                        |> List.filterMap
                            (\( hoverRange, hoverText ) ->
                                if
                                    rangeContainsLocation
                                        ( request.positionLineNumber, request.positionColumn )
                                        hoverRange
                                then
                                    Just hoverText

                                else
                                    Nothing
                            )


type alias ImportedModule =
    { filePath : List String
    , canonicalName : List String
    , importedName : Elm.Syntax.ModuleName.ModuleName
    , parsedModule : Maybe ParsedCookedModuleCache
    , referencesRanges : List Elm.Syntax.Range.Range
    }


hoverItemsFromParsedModule :
    ParsedCookedModuleCache
    -> LanguageServiceState
    ->
        { fromDeclarations : List ( Range, LocationUnderFilePath, String )
        , hoverItems : List ( Range, String )
        }
hoverItemsFromParsedModule parsedModule languageServiceState =
    let
        importedModules : List ImportedModule
        importedModules =
            importedModulesFromFile parsedModule languageServiceState

        parsedReferences : List (Elm.Syntax.Node.Node ( List String, String ))
        parsedReferences =
            listReferencesInFile parsedModule.syntax

        currentModuleDeclarations :
            { fromTopLevel :
                List
                    { completionItem : CompletionItem
                    , isExposed : Bool
                    , range : Range
                    }
            , fromLocals : List ( CompletionItem, Range )
            }
        currentModuleDeclarations =
            parsedModule.completionItems

        localDeclarationsAndImportExposings : List ( DeclarationScope, CompletionItem )
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
                    (\( completionItem, range ) -> ( LocalScope range, completionItem ))
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
                                    importedParsed.completionItems
                          )
                        )
                    )

        fromImportSyntax : List ( Range, String )
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
                                            let
                                                (CompletionItem _ _ _ documentation) =
                                                    moduleCompletionItemFromModuleSyntax
                                                        { importedModuleNameRestAfterPrefix = Nothing
                                                        , importedName = Just importedModule.importedName
                                                        }
                                                        importedModuleParsed.syntax
                                            in
                                            ( rangeFromRecordRange range
                                            , documentation
                                            )
                                        )
                    )

        localModuleItemsBeforeFiltering :
            List
                ( String
                , ( Maybe Range
                  , LocationUnderFilePath
                  , CompletionItem
                  )
                )
        localModuleItemsBeforeFiltering =
            localDeclarationsAndImportExposings
                |> List.map
                    (\( scope, (CompletionItem completionItemLabel _ _ _) as completionItem ) ->
                        let
                            ( maybeFilterRange, completionItemRange ) =
                                case scope of
                                    TopLevelScope topLevelRange ->
                                        ( Nothing, topLevelRange )

                                    LocalScope scopeRange ->
                                        ( Just scopeRange, scopeRange )
                        in
                        ( completionItemLabel
                        , ( maybeFilterRange
                          , LocationUnderFilePath
                                parsedModule.filePath
                                completionItemRange
                          , completionItem
                          )
                        )
                    )

        getHoverForFunctionOrName :
            Elm.Syntax.Node.Node ( Elm.Syntax.ModuleName.ModuleName, String )
            -> Maybe ( LocationUnderFilePath, String )
        getHoverForFunctionOrName (Elm.Syntax.Node.Node functionOrNameNodeRange ( moduleName, nameInModule )) =
            if moduleName == [] then
                case Common.assocListGet nameInModule localModuleItemsBeforeFiltering of
                    Nothing ->
                        Nothing

                    Just ( maybeFilterRange, locationUnderFilePath, CompletionItem _ _ _ completionItemDocumentation ) ->
                        case maybeFilterRange of
                            Nothing ->
                                Just ( locationUnderFilePath, completionItemDocumentation )

                            Just filterRange ->
                                if
                                    rangeContainsLocation
                                        ( functionOrNameNodeRange.start.row, functionOrNameNodeRange.start.column )
                                        filterRange
                                then
                                    Just ( locationUnderFilePath, completionItemDocumentation )

                                else
                                    Nothing

            else
                case Common.assocListGet moduleName importedModulesCompletionItems of
                    Nothing ->
                        Nothing

                    Just ( referencedModule, moduleCompletionItems ) ->
                        moduleCompletionItems.fromTopLevel
                            |> Common.listMapFind
                                (\item ->
                                    let
                                        (CompletionItem _ itemInsertText _ itemDocumentation) =
                                            item.completionItem
                                    in
                                    if itemInsertText == nameInModule && item.isExposed then
                                        Just
                                            ( LocationUnderFilePath
                                                referencedModule.filePath
                                                item.range
                                            , itemDocumentation
                                            )

                                    else
                                        Nothing
                                )

        getForHoversForReferenceNode :
            Elm.Syntax.Node.Node ( List String, String )
            -> List ( Range, LocationUnderFilePath, String )
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

                forModule : List ( Range, LocationUnderFilePath, String )
                forModule =
                    if moduleName == [] then
                        []

                    else
                        case
                            importedModules
                                |> Common.listFind
                                    (\importedModule ->
                                        importedModule.importedName == moduleName
                                    )
                        of
                            Nothing ->
                                []

                            Just referencedModule ->
                                case referencedModule.parsedModule of
                                    Nothing ->
                                        []

                                    Just referencedModuleParsed ->
                                        let
                                            (CompletionItem _ _ _ documentation) =
                                                moduleCompletionItemFromModuleSyntax
                                                    { importedModuleNameRestAfterPrefix = Nothing
                                                    , importedName = Just moduleName
                                                    }
                                                    referencedModuleParsed.syntax
                                        in
                                        [ ( rangeFromRecordRange rangeModulePart
                                          , LocationUnderFilePath
                                                referencedModule.filePath
                                                (Range
                                                    ( rangeModulePart.start.row, rangeModulePart.start.column )
                                                    ( rangeModulePart.end.row, rangeModulePart.end.column )
                                                )
                                          , documentation
                                          )
                                        ]

                forNameInModule : List ( Range, LocationUnderFilePath, String )
                forNameInModule =
                    case getHoverForFunctionOrName functionOrNameNode of
                        Nothing ->
                            []

                        Just ( sourceLocation, hover ) ->
                            [ ( rangeFromRecordRange forNameInModuleRange
                              , sourceLocation
                              , hover
                              )
                            ]
            in
            List.concat
                [ forModule
                , forNameInModule
                ]

        fromDeclarations : List ( Range, LocationUnderFilePath, String )
        fromDeclarations =
            List.concatMap getForHoversForReferenceNode parsedReferences

        fromDeclarationsLessSourceLocation : List ( Range, String )
        fromDeclarationsLessSourceLocation =
            fromDeclarations
                |> List.map (\( range, _, documentation ) -> ( range, documentation ))

        hoverItems : List ( Range, String )
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
                            |> List.map monacoCompletionItemFromCompletionItem


monacoCompletionItemFromCompletionItem : CompletionItem -> Frontend.MonacoEditor.MonacoCompletionItem
monacoCompletionItemFromCompletionItem (CompletionItem label insertText kind documentation) =
    { label = label
    , kind = kind
    , documentation = documentation
    , insertText = insertText
    }


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
                                    rangeContainsLocation
                                        ( request.positionLineNumber, request.positionColumn )
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


textDocumentSymbol :
    List String
    -> LanguageServiceState
    -> List LanguageServiceInterface.DocumentSymbol
textDocumentSymbol filePath languageServiceState =
    case FileTree.getBlobAtPathFromFileTree filePath languageServiceState.fileTreeParseCache of
        Nothing ->
            []

        Just currentFileCacheItem ->
            case currentFileCacheItem.parsedFileLastSuccess of
                Nothing ->
                    []

                Just parsedFileLastSuccess ->
                    parsedFileLastSuccess.completionItems.fromTopLevel
                        |> List.map
                            (\completionItem ->
                                let
                                    (CompletionItem label _ completionItemKind _) =
                                        completionItem.completionItem

                                    monacoRange : Frontend.MonacoEditor.MonacoRange
                                    monacoRange =
                                        monacoRangeFromRange completionItem.range

                                    symbolKind : LanguageServiceInterface.SymbolKind
                                    symbolKind =
                                        case completionItemKind of
                                            Frontend.MonacoEditor.ConstructorCompletionItemKind ->
                                                LanguageServiceInterface.SymbolKind_EnumMember

                                            Frontend.MonacoEditor.EnumCompletionItemKind ->
                                                LanguageServiceInterface.SymbolKind_Enum

                                            Frontend.MonacoEditor.EnumMemberCompletionItemKind ->
                                                LanguageServiceInterface.SymbolKind_EnumMember

                                            Frontend.MonacoEditor.FunctionCompletionItemKind ->
                                                LanguageServiceInterface.SymbolKind_Function

                                            Frontend.MonacoEditor.ModuleCompletionItemKind ->
                                                LanguageServiceInterface.SymbolKind_Module

                                            Frontend.MonacoEditor.StructCompletionItemKind ->
                                                LanguageServiceInterface.SymbolKind_Struct
                                in
                                LanguageServiceInterface.DocumentSymbol
                                    { name = label
                                    , range = monacoRange
                                    , selectionRange = monacoRange
                                    , kind = symbolKind

                                    -- TODO: Move choice type tags to children
                                    , children = []
                                    }
                            )


textDocumentReferences :
    LanguageServiceInterface.ProvideReferencesRequestStruct
    -> LanguageServiceState
    -> List LanguageServiceInterface.LocationUnderFilePath
textDocumentReferences referenceRequest languageServiceState =
    let
        maybeDefinition : Maybe LanguageServiceInterface.LocationUnderFilePath
        maybeDefinition =
            provideDefinition
                { filePathOpenedInEditor = referenceRequest.filePathOpenedInEditor
                , positionLineNumber = referenceRequest.positionLineNumber
                , positionColumn = referenceRequest.positionColumn
                }
                languageServiceState
                |> List.head
    in
    case maybeDefinition of
        Just definitionLocation ->
            let
                definitionLocationRange : Range
                definitionLocationRange =
                    Range ( definitionLocation.range.startLineNumber, definitionLocation.range.startColumn )
                        ( definitionLocation.range.endLineNumber, definitionLocation.range.endColumn )
            in
            findReferences ( definitionLocation.filePath, definitionLocationRange ) languageServiceState

        Nothing ->
            -- Fallback: try to find a top-level declaration covering this position
            case
                FileTree.getBlobAtPathFromFileTree referenceRequest.filePathOpenedInEditor
                    languageServiceState.fileTreeParseCache
            of
                Nothing ->
                    []

                Just currentFileCacheItem ->
                    case currentFileCacheItem.parsedFileLastSuccess of
                        Nothing ->
                            []

                        Just parsedFile ->
                            let
                                pos : ( Int, Int )
                                pos =
                                    ( referenceRequest.positionLineNumber, referenceRequest.positionColumn )

                                maybeTopLevelDefinitionLocation =
                                    parsedFile.completionItems.fromTopLevel
                                        |> Common.listMapFind
                                            (\decl ->
                                                if rangeContainsLocation pos decl.range then
                                                    Just
                                                        ( parsedFile.filePath
                                                        , decl.range
                                                        )

                                                else
                                                    Nothing
                                            )
                            in
                            case maybeTopLevelDefinitionLocation of
                                Nothing ->
                                    []

                                Just syntheticDefinitionLocation ->
                                    findReferences syntheticDefinitionLocation languageServiceState


findReferences :
    ( List String, Range )
    -> LanguageServiceState
    -> List LanguageServiceInterface.LocationUnderFilePath
findReferences ( targetDefinitionFilePath, targetDefinitionRange ) languageServiceState =
    let
        allParsedModules : List ParsedCookedModuleCache
        allParsedModules =
            List.concat
                [ languageServiceState.fileTreeParseCache
                    |> FileTree.flatListOfBlobsFromFileTreeNode
                    |> List.filterMap (\( _, blob ) -> blob.parsedFileLastSuccess)
                , List.map .parseResult languageServiceState.coreModulesCache
                ]

        findReferencesInModule :
            ParsedCookedModuleCache
            -> List LanguageServiceInterface.LocationUnderFilePath
        findReferencesInModule parsedModule =
            let
                { fromDeclarations } =
                    hoverItemsFromParsedModule parsedModule languageServiceState
            in
            fromDeclarations
                |> List.filterMap
                    (\( Range ( startLine, startColumn ) ( endLine, endColumn ), LocationUnderFilePath defFilePath defRange, _ ) ->
                        if defFilePath == targetDefinitionFilePath && defRange == targetDefinitionRange then
                            Just
                                { filePath = parsedModule.filePath
                                , range =
                                    { startLineNumber = startLine
                                    , startColumn = startColumn
                                    , endLineNumber = endLine
                                    , endColumn = endColumn
                                    }
                                }

                        else
                            Nothing
                    )
    in
    allParsedModules
        |> List.concatMap findReferencesInModule


monacoRangeFromSyntaxRange : Elm.Syntax.Range.Range -> Frontend.MonacoEditor.MonacoRange
monacoRangeFromSyntaxRange syntaxRange =
    { startLineNumber = syntaxRange.start.row
    , startColumn = syntaxRange.start.column
    , endLineNumber = syntaxRange.end.row
    , endColumn = syntaxRange.end.column
    }


monacoRangeFromRange : Range -> Frontend.MonacoEditor.MonacoRange
monacoRangeFromRange (Range ( startRow, startColumn ) ( endRow, endColumn )) =
    { startLineNumber = startRow
    , startColumn = startColumn
    , endLineNumber = endRow
    , endColumn = endColumn
    }


provideCompletionItemsInModule :
    { fileOpenedInEditor : ParsedCookedModuleCache, newText : String, cursorLineNumber : Int, cursorColumn : Int }
    -> LanguageServiceState
    -> List CompletionItem
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

        modulesAvailableForImport : List ParsedCookedModuleCache
        modulesAvailableForImport =
            modulesAvailableForImportFromState languageServiceState

        moduleNamesToNotSuggestForImport : List Elm.Syntax.ModuleName.ModuleName
        moduleNamesToNotSuggestForImport =
            [ fileOpenedInEditorModuleName ]

        modulesToSuggestForImport =
            modulesAvailableForImport
                |> List.filterMap
                    (\availableModule ->
                        let
                            availableModuleSyntax =
                                availableModule.syntax

                            (Elm.Syntax.Node.Node _ availableModuleDefinition) =
                                availableModuleSyntax.moduleDefinition

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

        currentModuleDeclarations : ModuleCompletionItems
        currentModuleDeclarations =
            request.fileOpenedInEditor.completionItems

        fromLocals : List CompletionItem
        fromLocals =
            currentModuleDeclarations.fromLocals
                |> List.filterMap
                    (\( completionItem, completionItemRange ) ->
                        if
                            rangeContainsLocation
                                ( request.cursorLineNumber, String.length lineUntilPosition )
                                completionItemRange
                        then
                            Just completionItem

                        else
                            Nothing
                    )

        importExposings : List CompletionItem
        importExposings =
            List.concat
                [ importExposingsFromFile request.fileOpenedInEditor languageServiceState
                , List.map Tuple.second (commonImplicitTopLevelImports languageServiceState)
                ]

        localDeclarationsAndImportExposings : List CompletionItem
        localDeclarationsAndImportExposings =
            List.concat
                [ List.map .completionItem currentModuleDeclarations.fromTopLevel
                , importExposings
                , fromLocals
                ]

        localDeclarationsAfterPrefix : List CompletionItem
        localDeclarationsAfterPrefix =
            if completionPrefix == [] then
                localDeclarationsAndImportExposings

            else
                case
                    importedModules
                        |> Common.listFind
                            (\importedModule ->
                                importedModule.importedName == completionPrefix
                            )
                        |> Maybe.andThen .parsedModule
                of
                    Nothing ->
                        []

                    Just referencedModule ->
                        referencedModule.completionItems.fromTopLevel
                            |> List.filterMap
                                (\item ->
                                    if item.isExposed then
                                        Just item.completionItem

                                    else
                                        Nothing
                                )

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

        fromImports : List CompletionItem
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
                            availableModule.syntax
                    )

        _ ->
            if completionPrefixIsNamespace then
                List.concat
                    [ fromImports
                    , List.sortBy
                        (\(CompletionItem label _ _ _) -> label)
                        localDeclarationsAfterPrefix
                    ]

            else
                []


importedModulesFromFile :
    ParsedCookedModuleCache
    -> LanguageServiceState
    ->
        List
            { filePath : List String
            , canonicalName : List String
            , importedName : Elm.Syntax.ModuleName.ModuleName
            , parsedModule : Maybe ParsedCookedModuleCache
            , referencesRanges : List Elm.Syntax.Range.Range
            }
importedModulesFromFile fileOpenedInEditor languageServiceState =
    let
        implicitlyImportedModules =
            languageServiceState.coreModulesCache
                |> List.filterMap
                    (\coreModule ->
                        if coreModule.implicitImport then
                            let
                                (Elm.Syntax.Node.Node _ moduleDefinition) =
                                    coreModule.parseResult.syntax.moduleDefinition

                                canonicalName : Elm.Syntax.ModuleName.ModuleName
                                canonicalName =
                                    Elm.Syntax.Module.moduleName moduleDefinition
                            in
                            Just
                                { filePath = coreModule.parseResult.filePath
                                , canonicalName = canonicalName
                                , importedName = canonicalName
                                , parsedModule = Just coreModule.parseResult
                                , referencesRanges = []
                                }

                        else
                            Nothing
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


modulesAvailableForImportFromState : LanguageServiceState -> List ParsedCookedModuleCache
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
    ParsedCookedModuleCache
    -> LanguageServiceState
    -> List CompletionItem
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
                                |> Common.listMapFind
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
                        of
                            Nothing ->
                                []

                            Just importedParsedModule ->
                                let
                                    importedModuleItems =
                                        importedParsedModule.completionItems.fromTopLevel
                                            |> List.filterMap
                                                (\item ->
                                                    if item.isExposed then
                                                        Just item.completionItem

                                                    else
                                                        Nothing
                                                )
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
                                                            (\(CompletionItem _ insertText _ _) ->
                                                                insertText == exposedName
                                                            )
                                                )
            )


commonImplicitTopLevelImports :
    LanguageServiceState
    -> List ( Range, CompletionItem )
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
                        coreModule.parseResult.completionItems

                    isItemExposed : CompletionItem -> Bool
                    isItemExposed (CompletionItem _ itemInsertText _ _) =
                        case moduleName of
                            [ "Basics" ] ->
                                True

                            [ "String" ] ->
                                case itemInsertText of
                                    "String" ->
                                        True

                                    _ ->
                                        False

                            [ "Maybe" ] ->
                                case itemInsertText of
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
    -> CompletionItem
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
    CompletionItem
        label
        insertText
        Frontend.MonacoEditor.ModuleCompletionItemKind
        documentation


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
            { completionItem : CompletionItem
            , isExposed : Bool
            , range : Range
            }
    , fromLocals : List ( CompletionItem, Range )
    }


completionItemsFromModule : ParsedModuleCache -> ModuleCompletionItems
completionItemsFromModule moduleCache =
    let
        textLines : List String
        textLines =
            String.lines moduleCache.text

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

        cookedDeclarations : List ( String, ( ParsedDeclaration, DeclarationScope ) )
        cookedDeclarations =
            listDeclarationsInFile moduleCache.syntax (SourceContentForCooking textLines)

        fromTopLevel :
            List
                { completionItem : CompletionItem
                , isExposed : Bool
                , range : Range
                }
        fromTopLevel =
            cookedDeclarations
                |> List.concatMap
                    (\( declName, ( declOrRef, scope ) ) ->
                        case scope of
                            TopLevelScope scopeRange ->
                                let
                                    isExposed : Bool
                                    isExposed =
                                        case declOrRef of
                                            FunctionOrValueDeclaration _ ->
                                                Elm.Syntax.Exposing.exposesFunction declName exposingList

                                            TypeAliasDeclaration _ ->
                                                exposingListExposesTypeOrAlias declName exposingList

                                            ChoiceTypeDeclaration _ _ ->
                                                exposingListExposesTypeOrAlias declName exposingList
                                in
                                completionItemsFromParsedDeclaration
                                    declName
                                    declOrRef
                                    |> List.map
                                        (\completionItem ->
                                            { completionItem = completionItem
                                            , isExposed = isExposed
                                            , range = scopeRange
                                            }
                                        )

                            LocalScope _ ->
                                []
                    )

        fromLocals : List ( CompletionItem, Range )
        fromLocals =
            cookedDeclarations
                |> List.concatMap
                    (\( declName, ( declOrRef, scope ) ) ->
                        case scope of
                            TopLevelScope _ ->
                                []

                            LocalScope range ->
                                completionItemsFromParsedDeclaration
                                    declName
                                    declOrRef
                                    |> List.map (\completionItem -> ( completionItem, range ))
                    )
    in
    { fromTopLevel = fromTopLevel
    , fromLocals = fromLocals
    }


exposingListExposesTypeOrAlias : String -> Elm.Syntax.Exposing.Exposing -> Bool
exposingListExposesTypeOrAlias name exposingList =
    case exposingList of
        Elm.Syntax.Exposing.All _ ->
            True

        Elm.Syntax.Exposing.Explicit topLevelExposings ->
            topLevelExposings
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


completionItemsFromParsedDeclaration :
    String
    -> ParsedDeclaration
    -> List CompletionItem
completionItemsFromParsedDeclaration declName declarationOrReference =
    case declarationOrReference of
        FunctionOrValueDeclaration (CookedDocumentation markdown) ->
            [ CompletionItem
                declName
                declName
                Frontend.MonacoEditor.FunctionCompletionItemKind
                markdown
            ]

        TypeAliasDeclaration (CookedDocumentation markdown) ->
            [ CompletionItem
                declName
                declName
                Frontend.MonacoEditor.StructCompletionItemKind
                markdown
            ]

        ChoiceTypeDeclaration (CookedDocumentation choiceTypeMarkdown) tags ->
            CompletionItem
                declName
                declName
                Frontend.MonacoEditor.EnumCompletionItemKind
                choiceTypeMarkdown
                :: List.map
                    (\( tagName, CookedDocumentation tagMarkdown ) ->
                        CompletionItem
                            tagName
                            tagName
                            Frontend.MonacoEditor.EnumMemberCompletionItemKind
                            tagMarkdown
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
                                                        , completionItems =
                                                            completionItemsFromModule
                                                                { filePath = blobPath
                                                                , text = asString
                                                                , syntax = syntax
                                                                }
                                                        }
                                    in
                                    Just
                                        { text = asString
                                        , parsedFile = parsedFile
                                        }

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


listReferencesInFile : Elm.Syntax.File.File -> List (Elm.Syntax.Node.Node ( Elm.Syntax.ModuleName.ModuleName, String ))
listReferencesInFile fileSyntax =
    List.concatMap
        listReferencesInDeclaration
        fileSyntax.declarations


listDeclarationsInFile : Elm.Syntax.File.File -> SourceContentForCooking -> List ( String, ( ParsedDeclaration, DeclarationScope ) )
listDeclarationsInFile fileSyntax sourceContent =
    fileSyntax.declarations
        |> List.concatMap (listDeclarationsInDeclaration sourceContent)


listReferencesInDeclaration :
    Elm.Syntax.Node.Node Elm.Syntax.Declaration.Declaration
    -> List (Elm.Syntax.Node.Node ( Elm.Syntax.ModuleName.ModuleName, String ))
listReferencesInDeclaration declarationNode =
    let
        (Elm.Syntax.Node.Node _ declaration) =
            declarationNode
    in
    case declaration of
        Elm.Syntax.Declaration.FunctionDeclaration function ->
            listReferencesForFunction function

        Elm.Syntax.Declaration.AliasDeclaration aliasDeclaration ->
            referencesForAliasDeclaration aliasDeclaration

        Elm.Syntax.Declaration.CustomTypeDeclaration choiceTypeDeclaration ->
            listReferencesFromTypeDeclaration choiceTypeDeclaration

        Elm.Syntax.Declaration.PortDeclaration _ ->
            []

        Elm.Syntax.Declaration.InfixDeclaration _ ->
            []

        Elm.Syntax.Declaration.Destructuring _ _ ->
            []


listDeclarationsInDeclaration :
    SourceContentForCooking
    -> Elm.Syntax.Node.Node Elm.Syntax.Declaration.Declaration
    -> List ( String, ( ParsedDeclaration, DeclarationScope ) )
listDeclarationsInDeclaration sourceContent declarationNode =
    let
        (Elm.Syntax.Node.Node declarationRange declaration) =
            declarationNode
    in
    case declaration of
        Elm.Syntax.Declaration.FunctionDeclaration function ->
            listDeclarationsForFunction
                sourceContent
                declarationRange
                function

        Elm.Syntax.Declaration.AliasDeclaration aliasDeclaration ->
            declarationsForAliasDeclaration
                sourceContent
                declarationRange
                aliasDeclaration

        Elm.Syntax.Declaration.CustomTypeDeclaration choiceTypeDeclaration ->
            listDeclarationsFromTypeDeclaration
                sourceContent
                declarationRange
                choiceTypeDeclaration

        Elm.Syntax.Declaration.PortDeclaration _ ->
            []

        Elm.Syntax.Declaration.InfixDeclaration _ ->
            []

        Elm.Syntax.Declaration.Destructuring _ _ ->
            []


referencesForAliasDeclaration :
    Elm.Syntax.TypeAlias.TypeAlias
    -> List (Elm.Syntax.Node.Node ( Elm.Syntax.ModuleName.ModuleName, String ))
referencesForAliasDeclaration aliasDeclaration =
    listTypeReferencesFromTypeAnnotation aliasDeclaration.typeAnnotation


declarationsForAliasDeclaration :
    SourceContentForCooking
    -> Elm.Syntax.Range.Range
    -> Elm.Syntax.TypeAlias.TypeAlias
    -> List ( String, ( ParsedDeclaration, DeclarationScope ) )
declarationsForAliasDeclaration sourceContent declarationRange aliasDeclaration =
    let
        (Elm.Syntax.Node.Node _ aliasName) =
            aliasDeclaration.name
    in
    [ ( aliasName
      , ( declarationOrReferenceForAliasDeclaration sourceContent aliasDeclaration
        , TopLevelScope (rangeFromRecordRange declarationRange)
        )
      )
    ]


declarationOrReferenceForAliasDeclaration :
    SourceContentForCooking
    -> Elm.Syntax.TypeAlias.TypeAlias
    -> ParsedDeclaration
declarationOrReferenceForAliasDeclaration (SourceContentForCooking textLines) aliasDeclaration =
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

        codeRange : Range
        codeRange =
            [ aliasNameRange
            , typeAnnotationRange
            ]
                |> Elm.Syntax.Range.combine
                |> rangeFromRecordRange
                |> expandRangeToLineStart

        codeLines : List String
        codeLines =
            sliceRangeFromTextLines
                textLines
                codeRange
    in
    TypeAliasDeclaration
        (CookedDocumentation
            (documentationMarkdownFromCodeLinesAndDocumentation
                codeLines
                documentationStringFromSyntax
            )
        )


listReferencesFromTypeDeclaration :
    Elm.Syntax.Type.Type
    -> List (Elm.Syntax.Node.Node ( List String, String ))
listReferencesFromTypeDeclaration choiceTypeDeclaration =
    choiceTypeDeclaration.constructors
        |> List.concatMap
            (\(Elm.Syntax.Node.Node _ constructor) ->
                List.concatMap listTypeReferencesFromTypeAnnotation constructor.arguments
            )


listDeclarationsFromTypeDeclaration :
    SourceContentForCooking
    -> Elm.Syntax.Range.Range
    -> Elm.Syntax.Type.Type
    -> List ( String, ( ParsedDeclaration, DeclarationScope ) )
listDeclarationsFromTypeDeclaration (SourceContentForCooking textLines) declarationRange choiceTypeDeclaration =
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

        codeRange : Range
        codeRange =
            nameRange
                :: constructorsRanges
                |> Elm.Syntax.Range.combine
                |> rangeFromRecordRange
                |> expandRangeToLineStart

        choiceTypeCodeLines : List String
        choiceTypeCodeLines =
            sliceRangeFromTextLines
                textLines
                codeRange

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
    [ ( choiceTypeName
      , ( ChoiceTypeDeclaration
            (CookedDocumentation
                (documentationMarkdownFromCodeLinesAndDocumentation
                    choiceTypeCodeLines
                    documentationStringFromSyntax
                )
            )
            tagsDeclarations
        , TopLevelScope (rangeFromRecordRange declarationRange)
        )
      )
    ]


listReferencesInExpression :
    Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression
    -> List (Elm.Syntax.Node.Node ( List String, String ))
listReferencesInExpression (Elm.Syntax.Node.Node expressionRange expression) =
    case expression of
        Elm.Syntax.Expression.UnitExpr ->
            []

        Elm.Syntax.Expression.Application application ->
            application
                |> List.concatMap listReferencesInExpression

        Elm.Syntax.Expression.OperatorApplication _ _ leftExpr rightExpr ->
            [ leftExpr, rightExpr ]
                |> List.concatMap listReferencesInExpression

        Elm.Syntax.Expression.FunctionOrValue moduleName localName ->
            [ Elm.Syntax.Node.Node expressionRange ( moduleName, localName ) ]

        Elm.Syntax.Expression.IfBlock ifExpr thenExpr elseExpr ->
            [ ifExpr, thenExpr, elseExpr ]
                |> List.concatMap listReferencesInExpression

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
            listReferencesInExpression negation

        Elm.Syntax.Expression.Literal _ ->
            []

        Elm.Syntax.Expression.CharLiteral _ ->
            []

        Elm.Syntax.Expression.TupledExpression tupled ->
            tupled
                |> List.concatMap listReferencesInExpression

        Elm.Syntax.Expression.ParenthesizedExpression parenthesized ->
            listReferencesInExpression parenthesized

        Elm.Syntax.Expression.LetExpression letBlock ->
            listReferencesInLetBlock letBlock

        Elm.Syntax.Expression.CaseExpression caseBlock ->
            [ listReferencesInExpression caseBlock.expression
            , caseBlock.cases
                |> List.concatMap
                    (\( _, caseBranch ) ->
                        listReferencesInExpression caseBranch
                    )
            ]
                |> List.concat

        Elm.Syntax.Expression.LambdaExpression lambda ->
            listReferencesInExpression lambda.expression

        Elm.Syntax.Expression.RecordExpr recordExpr ->
            recordExpr
                |> List.concatMap
                    (\(Elm.Syntax.Node.Node _ ( _, recordField )) ->
                        listReferencesInExpression recordField
                    )

        Elm.Syntax.Expression.ListExpr listExpr ->
            listExpr
                |> List.concatMap listReferencesInExpression

        Elm.Syntax.Expression.RecordAccess recordAccess _ ->
            listReferencesInExpression recordAccess

        Elm.Syntax.Expression.RecordAccessFunction _ ->
            []

        Elm.Syntax.Expression.RecordUpdateExpression (Elm.Syntax.Node.Node recordNameRange recordName) recordUpdateExpression ->
            [ [ Elm.Syntax.Node.Node
                    recordNameRange
                    ( [], recordName )
              ]
            , recordUpdateExpression
                |> List.concatMap
                    (\(Elm.Syntax.Node.Node _ ( _, recordField )) ->
                        listReferencesInExpression recordField
                    )
            ]
                |> List.concat

        Elm.Syntax.Expression.GLSLExpression _ ->
            []


listDeclarationsInExpression :
    SourceContentForCooking
    -> Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression
    -> List ( String, ( ParsedDeclaration, DeclarationScope ) )
listDeclarationsInExpression sourceContent (Elm.Syntax.Node.Node rangeRecord expression) =
    case expression of
        Elm.Syntax.Expression.UnitExpr ->
            []

        Elm.Syntax.Expression.Application application ->
            application
                |> List.concatMap (listDeclarationsInExpression sourceContent)

        Elm.Syntax.Expression.OperatorApplication _ _ leftExpr rightExpr ->
            [ leftExpr, rightExpr ]
                |> List.concatMap (listDeclarationsInExpression sourceContent)

        Elm.Syntax.Expression.FunctionOrValue _ _ ->
            []

        Elm.Syntax.Expression.IfBlock ifExpr thenExpr elseExpr ->
            [ ifExpr, thenExpr, elseExpr ]
                |> List.concatMap (listDeclarationsInExpression sourceContent)

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
            listDeclarationsInExpression sourceContent negation

        Elm.Syntax.Expression.Literal _ ->
            []

        Elm.Syntax.Expression.CharLiteral _ ->
            []

        Elm.Syntax.Expression.TupledExpression tupled ->
            List.concatMap
                (listDeclarationsInExpression sourceContent)
                tupled

        Elm.Syntax.Expression.ParenthesizedExpression parenthesized ->
            listDeclarationsInExpression
                sourceContent
                parenthesized

        Elm.Syntax.Expression.LetExpression letBlock ->
            let
                blockRange : Range
                blockRange =
                    rangeFromRecordRange rangeRecord
            in
            listDeclarationsInLetBlock
                sourceContent
                letBlock
                |> List.map
                    (\( name, ( declaration, _ ) ) ->
                        ( name, ( declaration, LocalScope blockRange ) )
                    )

        Elm.Syntax.Expression.CaseExpression caseBlock ->
            [ listDeclarationsInExpression
                sourceContent
                caseBlock.expression
            , caseBlock.cases
                |> List.concatMap
                    (\( _, caseBranch ) ->
                        listDeclarationsInExpression sourceContent caseBranch
                    )
            ]
                |> List.concat

        Elm.Syntax.Expression.LambdaExpression lambda ->
            listDeclarationsInExpression sourceContent lambda.expression

        Elm.Syntax.Expression.RecordExpr recordExpr ->
            recordExpr
                |> List.concatMap
                    (\(Elm.Syntax.Node.Node _ ( _, recordField )) ->
                        listDeclarationsInExpression
                            sourceContent
                            recordField
                    )

        Elm.Syntax.Expression.ListExpr listExpr ->
            listExpr
                |> List.concatMap (listDeclarationsInExpression sourceContent)

        Elm.Syntax.Expression.RecordAccess recordAccess _ ->
            listDeclarationsInExpression
                sourceContent
                recordAccess

        Elm.Syntax.Expression.RecordAccessFunction _ ->
            []

        Elm.Syntax.Expression.RecordUpdateExpression (Elm.Syntax.Node.Node _ _) recordUpdateExpression ->
            recordUpdateExpression
                |> List.concatMap
                    (\(Elm.Syntax.Node.Node _ ( _, recordField )) ->
                        listDeclarationsInExpression
                            sourceContent
                            recordField
                    )

        Elm.Syntax.Expression.GLSLExpression _ ->
            []


listReferencesForFunction :
    Elm.Syntax.Expression.Function
    -> List (Elm.Syntax.Node.Node ( Elm.Syntax.ModuleName.ModuleName, String ))
listReferencesForFunction function =
    let
        (Elm.Syntax.Node.Node _ functionDeclaration) =
            function.declaration

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

        arguments : List (Elm.Syntax.Node.Node ( Elm.Syntax.ModuleName.ModuleName, String ))
        arguments =
            functionDeclaration.arguments
                |> List.concatMap listReferencesFromPattern
    in
    [ signatureReferences
    , arguments
    , listReferencesInExpression
        expressionNode
    ]
        |> List.concat


listDeclarationsForFunction :
    SourceContentForCooking
    -> Elm.Syntax.Range.Range
    -> Elm.Syntax.Expression.Function
    -> List ( String, ( ParsedDeclaration, DeclarationScope ) )
listDeclarationsForFunction ((SourceContentForCooking textLines) as sourceContent) declRange function =
    let
        (Elm.Syntax.Node.Node _ functionDeclaration) =
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
                            |> sliceRangeFromTextLines textLines
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

        expressionNode : Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression
        expressionNode =
            functionDeclaration.expression

        getTypeAnnotationFromArgumentIndex : Int -> Maybe (Elm.Syntax.Node.Node Elm.Syntax.TypeAnnotation.TypeAnnotation)
        getTypeAnnotationFromArgumentIndex argumentIndex =
            case function.signature of
                Nothing ->
                    Nothing

                Just (Elm.Syntax.Node.Node _ signature) ->
                    signature.typeAnnotation
                        |> getTypeAnnotationFromFunctionArgumentIndex argumentIndex

        arguments : List ( String, ( ParsedDeclaration, DeclarationScope ) )
        arguments =
            functionDeclaration.arguments
                |> List.indexedMap
                    (\argumentIndex argument ->
                        listDeclarationsFromPattern
                            sourceContent
                            { typeAnnotation = getTypeAnnotationFromArgumentIndex argumentIndex }
                            argument
                    )
                |> List.concat
    in
    List.concat
        [ [ ( functionName
            , ( functionItem, TopLevelScope (rangeFromRecordRange declRange) )
            )
          ]
        , arguments
        , listDeclarationsInExpression
            sourceContent
            expressionNode
        ]


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


listReferencesFromPattern :
    Elm.Syntax.Node.Node Elm.Syntax.Pattern.Pattern
    -> List (Elm.Syntax.Node.Node ( List String, String ))
listReferencesFromPattern (Elm.Syntax.Node.Node patternRange pattern) =
    case pattern of
        Elm.Syntax.Pattern.TuplePattern tuplePattern ->
            List.concatMap
                listReferencesFromPattern
                tuplePattern

        Elm.Syntax.Pattern.UnConsPattern head tail ->
            List.concatMap
                listReferencesFromPattern
                [ head, tail ]

        Elm.Syntax.Pattern.ListPattern listPattern ->
            List.concatMap
                listReferencesFromPattern
                listPattern

        Elm.Syntax.Pattern.VarPattern _ ->
            []

        Elm.Syntax.Pattern.NamedPattern named arguments ->
            [ [ Elm.Syntax.Node.Node patternRange ( named.moduleName, named.name ) ]
            , List.concatMap
                listReferencesFromPattern
                arguments
            ]
                |> List.concat

        Elm.Syntax.Pattern.ParenthesizedPattern parenthesized ->
            listReferencesFromPattern parenthesized

        _ ->
            []


listDeclarationsFromPattern :
    SourceContentForCooking
    -> { typeAnnotation : Maybe (Elm.Syntax.Node.Node Elm.Syntax.TypeAnnotation.TypeAnnotation) }
    -> Elm.Syntax.Node.Node Elm.Syntax.Pattern.Pattern
    -> List ( String, ( ParsedDeclaration, DeclarationScope ) )
listDeclarationsFromPattern ((SourceContentForCooking textLines) as sourceContent) config (Elm.Syntax.Node.Node patternRange pattern) =
    case pattern of
        Elm.Syntax.Pattern.TuplePattern tuplePattern ->
            List.concatMap
                (listDeclarationsFromPattern sourceContent { typeAnnotation = Nothing })
                tuplePattern

        Elm.Syntax.Pattern.UnConsPattern head tail ->
            List.concatMap
                (listDeclarationsFromPattern sourceContent { typeAnnotation = Nothing })
                [ head, tail ]

        Elm.Syntax.Pattern.ListPattern listPattern ->
            List.concatMap
                (listDeclarationsFromPattern sourceContent { typeAnnotation = Nothing })
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
                                    |> sliceRangeFromTextLines textLines
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
            [ ( name
              , ( FunctionOrValueDeclaration
                    (CookedDocumentation
                        (documentationMarkdownFromCodeLinesAndDocumentation codeLines Nothing)
                    )
                , TopLevelScope (rangeFromRecordRange patternRange)
                )
              )
            ]

        Elm.Syntax.Pattern.NamedPattern _ arguments ->
            List.concatMap
                (listDeclarationsFromPattern sourceContent { typeAnnotation = Nothing })
                arguments

        Elm.Syntax.Pattern.ParenthesizedPattern parenthesized ->
            listDeclarationsFromPattern sourceContent
                { typeAnnotation = Nothing }
                parenthesized

        _ ->
            []


listReferencesInLetBlock :
    Elm.Syntax.Expression.LetBlock
    -> List (Elm.Syntax.Node.Node ( List String, String ))
listReferencesInLetBlock letBlock =
    [ List.concatMap listReferencesInLetDeclaration letBlock.declarations
    , listReferencesInExpression letBlock.expression
    ]
        |> List.concat


listDeclarationsInLetBlock :
    SourceContentForCooking
    -> Elm.Syntax.Expression.LetBlock
    -> List ( String, ( ParsedDeclaration, DeclarationScope ) )
listDeclarationsInLetBlock sourceContent letBlock =
    [ List.concatMap
        (listDeclarationsInLetDeclaration sourceContent)
        letBlock.declarations
    , listDeclarationsInExpression sourceContent letBlock.expression
    ]
        |> List.concat


listReferencesInLetDeclaration :
    Elm.Syntax.Node.Node Elm.Syntax.Expression.LetDeclaration
    -> List (Elm.Syntax.Node.Node ( List String, String ))
listReferencesInLetDeclaration declarationNode =
    let
        (Elm.Syntax.Node.Node _ declaration) =
            declarationNode
    in
    case declaration of
        Elm.Syntax.Expression.LetFunction function ->
            listReferencesForFunction function

        Elm.Syntax.Expression.LetDestructuring _ letDestructuring ->
            listReferencesInExpression letDestructuring


listDeclarationsInLetDeclaration :
    SourceContentForCooking
    -> Elm.Syntax.Node.Node Elm.Syntax.Expression.LetDeclaration
    -> List ( String, ( ParsedDeclaration, DeclarationScope ) )
listDeclarationsInLetDeclaration sourceContent declarationNode =
    let
        (Elm.Syntax.Node.Node declarationRange declaration) =
            declarationNode
    in
    case declaration of
        Elm.Syntax.Expression.LetFunction function ->
            listDeclarationsForFunction sourceContent declarationRange function

        Elm.Syntax.Expression.LetDestructuring _ letDestructuring ->
            listDeclarationsInExpression sourceContent letDestructuring


constrainScopeToRange : Range -> DeclarationScope -> DeclarationScope
constrainScopeToRange range scope =
    case scope of
        LocalScope _ ->
            scope

        TopLevelScope _ ->
            LocalScope range


expandRangeToLineStart : Range -> Range
expandRangeToLineStart (Range ( startRow, _ ) end) =
    Range ( startRow, 1 ) end


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


rangeContainsLocation : ( Int, Int ) -> Range -> Bool
rangeContainsLocation ( row, column ) (Range ( startRow, startColumn ) ( endRow, endColumn )) =
    let
        rowInt : Int
        rowInt =
            row - 1

        columnInt : Int
        columnInt =
            column - 1

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
    in
    if rowInt < startRowInt then
        False

    else if rowInt > endRowInt then
        False

    else if rowInt == startRowInt && rowInt == endRowInt then
        if columnInt < startColumnInt then
            False

        else if columnInt > endColumnInt then
            False

        else
            True

    else if rowInt == startRowInt then
        if columnInt < startColumnInt then
            False

        else
            True

    else if rowInt == endRowInt then
        if columnInt > endColumnInt then
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


recordRangeFromRange : Range -> Elm.Syntax.Range.Range
recordRangeFromRange (Range ( startRow, startColumn ) ( endRow, endColumn )) =
    { start = { row = startRow, column = startColumn }
    , end = { row = endRow, column = endColumn }
    }


rangeFromRecordRange : Elm.Syntax.Range.Range -> Range
rangeFromRecordRange { start, end } =
    Range
        ( start.row, start.column )
        ( end.row, end.column )


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
