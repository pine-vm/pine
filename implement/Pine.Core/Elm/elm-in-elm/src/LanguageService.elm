module LanguageService exposing (..)

{-| Language services for Elm programs.
These functions enable features like completion suggestions and hover tips in the code editor.

Syntax analysis builds on `pine-elm-syntax`:

  - The concrete syntax tree retains source ranges, comments and literal
    spellings. It is only consulted by the presentation layer here, and only
    for the occurrences actually returned to the client.
  - The abstract syntax tree feeds `LanguageServiceAnalysis`, which describes
    declarations and references without any source range. Every occurrence is
    addressed by a structural `ElmSyntax.Path.Path` instead.

-}

import Common
import Dict
import ElmSyntax.Abstract.ConvertFromConcrete
import ElmSyntax.Abstract.Exposing
import ElmSyntax.Concrete.File
import ElmSyntax.Concrete.Node
import ElmSyntax.Concrete.Parser.FromString
import ElmSyntax.Concrete.Range
import ElmSyntax.Concrete.SourceLookup
import ElmSyntax.Path exposing (Path, Selection(..), Step(..))
import FileTree
import Frontend.MonacoEditor
import LanguageServiceAnalysis
import LanguageServiceInterface


type alias LanguageServiceState =
    { documentCache : Dict.Dict String LanguageServiceStateFileTreeNodeBlob
    , coreModulesCache : List ElmCoreModule
    , elmPackages :
        List
            ( LanguageServiceInterface.ElmPackageVersionIdentifer
            , List ( List String, ( List String, ParsedModuleCache ) )
            )
    }


type alias LanguageServiceStateFileTreeNodeBlob =
    { {- Avoid bug in Elm core library as reported at https://github.com/elm/bytes/issues/15 :
         Convert to other representation for equality check.
      -}
      sourceBase64 : String
    , textContent : Maybe FileTextContent
    , parsedFileLastSuccess : Maybe ParsedModuleCache
    }


type alias FileTextContent =
    { text : String
    , parsedFile : Maybe ParsedModuleCache
    }


{-| A parsed module as retained in the language service state.

The concrete syntax and the source text are only used to derive source ranges
and documentation strings on demand. All semantic questions are answered from
`analysis`, which is free of source ranges.

-}
type alias ParsedModuleCache =
    { fileUri : String
    , text : String
    , concrete : ElmSyntax.Concrete.File.File
    , analysis : LanguageServiceAnalysis.ModuleAnalysis
    , references : List LanguageServiceAnalysis.ReferenceOccurrence
    }


type alias ElmCoreModule =
    { parseResult : ParsedModuleCache
    , implicitImport : Bool
    }


type LocationInFile range
    = LocationInFile LanguageServiceInterface.FileLocation range


type DeclarationRange
    = DeclarationRange
        -- Complete declaration
        Range
        -- Instances of the own name, used for renaming
        (List Range)


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


{-| A declaration a reference resolved to, together with the module it was
declared in.
-}
type alias ResolvedDeclaration =
    { fileLocation : LanguageServiceInterface.FileLocation
    , parsedModule : ParsedModuleCache
    , occurrence : LanguageServiceAnalysis.DeclarationOccurrence
    }


{-| The subject a source location points at.

Identity of a target is `( fileLocation, path )`: A module is identified by the
path of its module definition, a declaration by the path of the complete
declaration.

-}
type ResolvedTarget
    = ResolvedDeclarationTarget ResolvedDeclaration
    | ResolvedModuleTarget LanguageServiceInterface.FileLocation ParsedModuleCache


type alias ImportedModule =
    { fileLocation : LanguageServiceInterface.FileLocation
    , canonicalName : List String
    , importedName : List String
    , exposingList : Maybe ElmSyntax.Abstract.Exposing.Exposing
    , parsedModule : ParsedModuleCache
    , moduleNamePaths : List Path
    }


initLanguageServiceState : List { moduleText : String, implicitImport : Bool } -> LanguageServiceState
initLanguageServiceState elmCoreModules =
    let
        elmCoreModulesParseResults : List ElmCoreModule
        elmCoreModulesParseResults =
            elmCoreModules
                |> List.filterMap
                    (\coreModule ->
                        case parseModuleText "elm-core" coreModule.moduleText of
                            Nothing ->
                                Nothing

                            Just parsedModule ->
                                Just
                                    { parseResult = parsedModule
                                    , implicitImport = coreModule.implicitImport
                                    }
                    )
    in
    { documentCache = Dict.empty
    , coreModulesCache = elmCoreModulesParseResults
    , elmPackages = []
    }


parseModuleText : String -> String -> Maybe ParsedModuleCache
parseModuleText fileUri text =
    case ElmSyntax.Concrete.Parser.FromString.parseFile text of
        Err _ ->
            Nothing

        Ok concrete ->
            let
                abstract =
                    ElmSyntax.Abstract.ConvertFromConcrete.fromFile concrete
            in
            Just
                { fileUri = fileUri
                , text = text
                , concrete = concrete
                , analysis = LanguageServiceAnalysis.analyzeFile abstract
                , references = LanguageServiceAnalysis.listReferencesInFile abstract
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
                LanguageServiceInterface.AddWorkspaceFileRequest fileUri fileContent ->
                    addFile ( fileUri, fileContent ) stateBefore

                LanguageServiceInterface.DeleteWorkspaceFileRequest fileUri ->
                    let
                        documentCache : Dict.Dict String LanguageServiceStateFileTreeNodeBlob
                        documentCache =
                            Dict.remove fileUri stateBefore.documentCache
                    in
                    ( LanguageServiceInterface.WorkspaceSummaryResponse
                    , { stateBefore
                        | documentCache = documentCache
                      }
                    )

                LanguageServiceInterface.AddElmPackageVersionRequest packageVersionIdentifer packageModules ->
                    handleRequestAddPackage
                        packageVersionIdentifer
                        packageModules
                        stateBefore

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

                LanguageServiceInterface.TextDocumentRenameRequest renameParams ->
                    ( LanguageServiceInterface.TextDocumentRenameResponse
                        (textDocumentRename
                            renameParams
                            stateBefore
                        )
                    , stateBefore
                    )
    in
    ( Ok serviceResponse
    , state
    )


addFile :
    ( String
    , LanguageServiceInterface.FileTreeBlobNode
    )
    -> LanguageServiceState
    -> ( LanguageServiceInterface.Response, LanguageServiceState )
addFile ( fileUri, fileContent ) stateBefore =
    let
        maybePreviousCached : Maybe LanguageServiceStateFileTreeNodeBlob
        maybePreviousCached =
            Dict.get fileUri stateBefore.documentCache

        maybeTextContent : Maybe FileTextContent
        maybeTextContent =
            case fileContent.asText of
                Nothing ->
                    Nothing

                Just asString ->
                    Just
                        { text = asString
                        , parsedFile = parseModuleText fileUri asString
                        }

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

        documentCache : Dict.Dict String LanguageServiceStateFileTreeNodeBlob
        documentCache =
            Dict.insert
                fileUri
                { sourceBase64 = fileContent.asBase64
                , textContent = maybeTextContent
                , parsedFileLastSuccess = parsedFileLastSuccess
                }
                stateBefore.documentCache
    in
    ( LanguageServiceInterface.WorkspaceSummaryResponse
    , { stateBefore
        | documentCache = documentCache
      }
    )


handleRequestAddPackage :
    LanguageServiceInterface.ElmPackageVersionIdentifer
    -> List ( List String, LanguageServiceInterface.FileTreeBlobNode )
    -> LanguageServiceState
    -> ( LanguageServiceInterface.Response, LanguageServiceState )
handleRequestAddPackage packageVersionIdentifer packageModules stateBefore =
    let
        parsedModules : List ( List String, ( List String, ParsedModuleCache ) )
        parsedModules =
            packageModules
                |> List.filterMap
                    (\( modulePath, fileContent ) ->
                        case fileContent.asText of
                            Nothing ->
                                Nothing

                            Just asString ->
                                case parseModuleText (String.join "/" modulePath) asString of
                                    Nothing ->
                                        Nothing

                                    Just parsedModule ->
                                        Just
                                            ( parsedModule.analysis.moduleName
                                            , ( modulePath, parsedModule )
                                            )
                    )

        state : LanguageServiceState
        state =
            { stateBefore
                | elmPackages =
                    ( packageVersionIdentifer, parsedModules )
                        :: stateBefore.elmPackages
            }
    in
    ( LanguageServiceInterface.WorkspaceSummaryResponse
    , state
    )



-- Deriving source ranges from structural paths


rangeFromConcreteRange : ElmSyntax.Concrete.Range.Range -> Range
rangeFromConcreteRange concreteRange =
    Range
        ( concreteRange.start.row, concreteRange.start.column )
        ( concreteRange.end.row, concreteRange.end.column )


rangeAtPathInModule : ParsedModuleCache -> Path -> Selection -> Maybe Range
rangeAtPathInModule parsedModule path selection =
    case ElmSyntax.Concrete.SourceLookup.rangeAtPath path selection parsedModule.concrete of
        Nothing ->
            Nothing

        Just concreteRange ->
            Just (rangeFromConcreteRange concreteRange)


declarationRangeOfOccurrence :
    ParsedModuleCache
    -> LanguageServiceAnalysis.DeclarationOccurrence
    -> Maybe DeclarationRange
declarationRangeOfOccurrence parsedModule occurrence =
    case rangeAtPathInModule parsedModule occurrence.declarationPath occurrence.declarationSelection of
        Nothing ->
            Nothing

        Just wholeRange ->
            Just
                (DeclarationRange
                    wholeRange
                    (List.filterMap
                        (\namePath -> rangeAtPathInModule parsedModule namePath SelectName)
                        occurrence.namePaths
                    )
                )


moduleDeclarationRange : ParsedModuleCache -> Maybe DeclarationRange
moduleDeclarationRange parsedModule =
    case rangeAtPathInModule parsedModule [ StepModuleDefinition ] SelectWhole of
        Nothing ->
            Nothing

        Just wholeRange ->
            Just (DeclarationRange wholeRange [])



-- Documentation, rendered on demand


documentationMarkdownForDeclaration :
    List String
    -> ParsedModuleCache
    -> LanguageServiceAnalysis.DeclarationOccurrence
    -> String
documentationMarkdownForDeclaration textLines parsedModule occurrence =
    let
        documentationFromComment : Maybe String
        documentationFromComment =
            case rangeAtPathInModule parsedModule occurrence.declarationPath SelectDocumentation of
                Nothing ->
                    Nothing

                Just documentationRange ->
                    Just
                        (removeWrappingFromMultilineComment
                            (String.join "\n" (sliceRangeFromTextLines textLines documentationRange))
                        )
    in
    case occurrence.documentation of
        LanguageServiceAnalysis.ValueDocumentation maybeAnnotationPath ->
            documentationMarkdownFromCodeLinesAndDocumentation
                (valueCodeLines textLines parsedModule occurrence.name maybeAnnotationPath)
                documentationFromComment

        LanguageServiceAnalysis.DeclarationCodeDocumentation ->
            documentationMarkdownFromCodeLinesAndDocumentation
                (declarationCodeLines textLines parsedModule occurrence)
                documentationFromComment

        LanguageServiceAnalysis.ChoiceTypeTagDocumentation choiceTypeName choiceTypePath ->
            String.join "\n\n"
                [ markdownElmCodeBlockFromCodeLines [ occurrence.name ]
                , String.concat [ "A variant of the choice type `", choiceTypeName, "`" ]
                , markdownElmCodeBlockFromCodeLines
                    (choiceTypeCodeLines textLines parsedModule choiceTypePath)
                ]


valueCodeLines : List String -> ParsedModuleCache -> String -> Maybe Path -> List String
valueCodeLines textLines parsedModule name maybeAnnotationPath =
    case maybeAnnotationPath of
        Nothing ->
            [ name ]

        Just annotationPath ->
            case rangeAtPathInModule parsedModule annotationPath SelectWhole of
                Nothing ->
                    [ name ]

                Just annotationRange ->
                    [ String.concat
                        [ name
                        , " : "
                        , String.join " " (sliceRangeFromTextLines textLines annotationRange)
                        ]
                    ]


declarationCodeLines :
    List String
    -> ParsedModuleCache
    -> LanguageServiceAnalysis.DeclarationOccurrence
    -> List String
declarationCodeLines textLines parsedModule occurrence =
    case rangeAtPathInModule parsedModule occurrence.declarationPath occurrence.declarationSelection of
        Nothing ->
            [ occurrence.name ]

        Just codeRange ->
            let
                lines : List String
                lines =
                    sliceRangeFromTextLines textLines codeRange
            in
            case occurrence.kind of
                LanguageServiceAnalysis.ChoiceTypeDeclarationKind ->
                    dropWhileEmpty lines

                _ ->
                    lines


dropWhileEmpty : List String -> List String
dropWhileEmpty lines =
    case lines of
        [] ->
            []

        first :: rest ->
            if String.isEmpty first then
                dropWhileEmpty rest

            else
                lines


choiceTypeCodeLines : List String -> ParsedModuleCache -> Path -> List String
choiceTypeCodeLines textLines parsedModule choiceTypePath =
    case rangeAtPathInModule parsedModule choiceTypePath SelectDeclarationWithoutDocumentation of
        Nothing ->
            []

        Just codeRange ->
            dropWhileEmpty
                (sliceRangeFromTextLines textLines codeRange)


completionItemKindFromDeclarationKind :
    LanguageServiceAnalysis.DeclarationKind
    -> Frontend.MonacoEditor.CompletionItemKind
completionItemKindFromDeclarationKind kind =
    case kind of
        LanguageServiceAnalysis.FunctionOrValueDeclarationKind ->
            Frontend.MonacoEditor.FunctionCompletionItemKind

        LanguageServiceAnalysis.TypeAliasDeclarationKind ->
            Frontend.MonacoEditor.StructCompletionItemKind

        LanguageServiceAnalysis.ChoiceTypeDeclarationKind ->
            Frontend.MonacoEditor.EnumCompletionItemKind

        LanguageServiceAnalysis.ChoiceTypeTagDeclarationKind ->
            Frontend.MonacoEditor.EnumMemberCompletionItemKind


completionItemForDeclaration :
    List String
    -> ParsedModuleCache
    -> LanguageServiceAnalysis.DeclarationOccurrence
    -> CompletionItem
completionItemForDeclaration textLines parsedModule occurrence =
    CompletionItem
        occurrence.name
        occurrence.name
        (completionItemKindFromDeclarationKind occurrence.kind)
        (documentationMarkdownForDeclaration textLines parsedModule occurrence)


{-| Build completion items for declarations of a single module, slicing the
source text into lines only once.
-}
completionItemsForDeclarations :
    ParsedModuleCache
    -> List LanguageServiceAnalysis.DeclarationOccurrence
    -> List CompletionItem
completionItemsForDeclarations parsedModule occurrences =
    case occurrences of
        [] ->
            []

        _ ->
            let
                textLines : List String
                textLines =
                    String.lines parsedModule.text
            in
            List.map (completionItemForDeclaration textLines parsedModule) occurrences


topLevelDeclarations : ParsedModuleCache -> List LanguageServiceAnalysis.DeclarationOccurrence
topLevelDeclarations parsedModule =
    List.filter declarationIsTopLevel parsedModule.analysis.declarations


declarationIsTopLevel : LanguageServiceAnalysis.DeclarationOccurrence -> Bool
declarationIsTopLevel occurrence =
    case occurrence.scope of
        LanguageServiceAnalysis.TopLevelScope ->
            True

        LanguageServiceAnalysis.LocalScope _ ->
            False


exposedTopLevelDeclarations : ParsedModuleCache -> List LanguageServiceAnalysis.DeclarationOccurrence
exposedTopLevelDeclarations parsedModule =
    List.filter
        (\occurrence -> occurrence.isExposed && declarationIsTopLevel occurrence)
        parsedModule.analysis.declarations



-- Resolving a source location to its target


{-| <https://microsoft.github.io/monaco-editor/typedoc/interfaces/languages.HoverProvider.html#provideHover>
-}
provideHover :
    LanguageServiceInterface.ProvideHoverRequestStruct
    -> LanguageServiceState
    -> List String
provideHover request languageServiceState =
    hoverItemsAtLocation
        request.fileLocation
        ( request.positionLineNumber, request.positionColumn )
        languageServiceState
        |> List.map (\( _, _, documentation ) -> documentation)


{-| Resolve the subject at the given source location and render it as hover
item: the range of the resolved occurrence in the queried file, the location of
the declaration it resolves to and the documentation to display.
-}
hoverItemsAtLocation :
    LanguageServiceInterface.FileLocation
    -> ( Int, Int )
    -> LanguageServiceState
    -> List ( Range, LocationInFile DeclarationRange, String )
hoverItemsAtLocation fileLocation location languageServiceState =
    case parsedModuleAtFileLocation fileLocation languageServiceState of
        Nothing ->
            []

        Just parsedModule ->
            let
                resolvedAtLocation : Maybe ( Range, ResolvedTarget )
                resolvedAtLocation =
                    case
                        resolveAtLocation
                            parsedModule
                            fileLocation
                            location
                            languageServiceState
                    of
                        Just resolved ->
                            Just resolved

                        Nothing ->
                            case declarationTargetAtLocation parsedModule fileLocation location of
                                Nothing ->
                                    Nothing

                                Just target ->
                                    case targetDeclarationRange target of
                                        Nothing ->
                                            Nothing

                                        Just (DeclarationRange _ nameRanges) ->
                                            case
                                                Common.listFind
                                                    (rangeContainsLocation location)
                                                    nameRanges
                                            of
                                                Nothing ->
                                                    Nothing

                                                Just nameRange ->
                                                    Just ( nameRange, target )
            in
            case resolvedAtLocation of
                Nothing ->
                    []

                Just ( occurrenceRange, target ) ->
                    case targetDeclarationRange target of
                        Nothing ->
                            []

                        Just declarationRange ->
                            [ ( occurrenceRange
                              , LocationInFile
                                    (targetFileLocation target)
                                    declarationRange
                              , targetDocumentation target
                              )
                            ]


parsedModuleAtFileLocation :
    LanguageServiceInterface.FileLocation
    -> LanguageServiceState
    -> Maybe ParsedModuleCache
parsedModuleAtFileLocation fileLocation languageServiceState =
    case fileLocation of
        LanguageServiceInterface.WorkspaceFileLocation filePath ->
            case Dict.get filePath languageServiceState.documentCache of
                Nothing ->
                    Nothing

                Just currentFileCacheItem ->
                    currentFileCacheItem.parsedFileLastSuccess

        LanguageServiceInterface.ElmPackageFileLocation packageVersionIdentifer modulePath ->
            languageServiceState.elmPackages
                |> Common.listMapFind
                    (\( candidateVersionIdentifer, packageModules ) ->
                        if candidateVersionIdentifer == packageVersionIdentifer then
                            packageModules
                                |> Common.listMapFind
                                    (\( _, ( candidateModulePath, parsedModule ) ) ->
                                        if candidateModulePath == modulePath then
                                            Just parsedModule

                                        else
                                            Nothing
                                    )

                        else
                            Nothing
                    )


targetFileLocation : ResolvedTarget -> LanguageServiceInterface.FileLocation
targetFileLocation target =
    case target of
        ResolvedDeclarationTarget resolved ->
            resolved.fileLocation

        ResolvedModuleTarget fileLocation _ ->
            fileLocation


targetDeclarationRange : ResolvedTarget -> Maybe DeclarationRange
targetDeclarationRange target =
    case target of
        ResolvedDeclarationTarget resolved ->
            declarationRangeOfOccurrence resolved.parsedModule resolved.occurrence

        ResolvedModuleTarget _ parsedModule ->
            moduleDeclarationRange parsedModule


targetDocumentation : ResolvedTarget -> String
targetDocumentation target =
    case target of
        ResolvedDeclarationTarget resolved ->
            documentationMarkdownForDeclaration
                (String.lines resolved.parsedModule.text)
                resolved.parsedModule
                resolved.occurrence

        ResolvedModuleTarget _ parsedModule ->
            case documentationStringFromModule parsedModule of
                Nothing ->
                    ""

                Just documentationString ->
                    documentationString


{-| Identity of a target, used to decide whether a reference resolves to the
subject of a references or rename request.
-}
targetIdentity : ResolvedTarget -> ( LanguageServiceInterface.FileLocation, Path )
targetIdentity target =
    case target of
        ResolvedDeclarationTarget resolved ->
            ( resolved.fileLocation, resolved.occurrence.declarationPath )

        ResolvedModuleTarget fileLocation _ ->
            ( fileLocation, [ StepModuleDefinition ] )


{-| Name of the declaration the target refers to, if any.

Resolving a reference can only yield a match for the target when the reference
uses this exact name, so it is used to skip the (otherwise dominant) cost of
resolving every reference in large modules. For module targets this is
'Nothing' and every reference is resolved.

-}
targetName : ResolvedTarget -> Maybe String
targetName target =
    case target of
        ResolvedDeclarationTarget resolved ->
            Just resolved.occurrence.name

        ResolvedModuleTarget _ _ ->
            Nothing


{-| Resolve the syntax at the given source location.

The structural path of the innermost node covering the location decides which
occurrence is queried, so no reference outside that path needs to be resolved.

-}
resolveAtLocation :
    ParsedModuleCache
    -> LanguageServiceInterface.FileLocation
    -> ( Int, Int )
    -> LanguageServiceState
    -> Maybe ( Range, ResolvedTarget )
resolveAtLocation parsedModule fileLocation ( row, column ) languageServiceState =
    let
        cursorPath : Path
        cursorPath =
            ElmSyntax.Concrete.SourceLookup.pathAtLocation
                { row = row, column = column }
                parsedModule.concrete

        importedModules : List ImportedModule
        importedModules =
            importedModulesFromModule parsedModule languageServiceState
    in
    case moduleTargetAtImportPath parsedModule cursorPath importedModules of
        Just fromImport ->
            Just fromImport

        Nothing ->
            case
                Common.listFind
                    (\reference -> reference.path == cursorPath)
                    parsedModule.references
            of
                Nothing ->
                    Nothing

                Just reference ->
                    resolveReferenceAtLocation
                        parsedModule
                        fileLocation
                        importedModules
                        reference
                        ( row, column )
                        languageServiceState


moduleTargetAtImportPath :
    ParsedModuleCache
    -> Path
    -> List ImportedModule
    -> Maybe ( Range, ResolvedTarget )
moduleTargetAtImportPath parsedModule cursorPath importedModules =
    Common.listMapFind
        (\importedModule ->
            if List.member cursorPath importedModule.moduleNamePaths then
                case rangeAtPathInModule parsedModule cursorPath SelectWhole of
                    Nothing ->
                        Nothing

                    Just range ->
                        Just
                            ( range
                            , ResolvedModuleTarget
                                importedModule.fileLocation
                                importedModule.parsedModule
                            )

            else
                Nothing
        )
        importedModules


resolveReferenceAtLocation :
    ParsedModuleCache
    -> LanguageServiceInterface.FileLocation
    -> List ImportedModule
    -> LanguageServiceAnalysis.ReferenceOccurrence
    -> ( Int, Int )
    -> LanguageServiceState
    -> Maybe ( Range, ResolvedTarget )
resolveReferenceAtLocation parsedModule fileLocation importedModules reference location languageServiceState =
    let
        qualifierMatches : Bool
        qualifierMatches =
            case rangeAtPathInModule parsedModule reference.path SelectQualifier of
                Nothing ->
                    False

                Just qualifierRange ->
                    rangeContainsLocation location qualifierRange
    in
    if qualifierMatches then
        case
            Common.listFind
                (\importedModule -> importedModule.importedName == reference.moduleName)
                importedModules
        of
            Nothing ->
                Nothing

            Just referencedModule ->
                case rangeAtPathInModule parsedModule reference.path SelectQualifier of
                    Nothing ->
                        Nothing

                    Just qualifierRange ->
                        Just
                            ( qualifierRange
                            , ResolvedModuleTarget
                                referencedModule.fileLocation
                                referencedModule.parsedModule
                            )

    else
        case rangeAtPathInModule parsedModule reference.path SelectName of
            Nothing ->
                Nothing

            Just nameRange ->
                if rangeContainsLocation location nameRange then
                    let
                        context : ModuleResolutionContext
                        context =
                            resolutionContextForModule
                                parsedModule
                                fileLocation
                                (commonImplicitTopLevelImports languageServiceState)
                                importedModules
                    in
                    case resolveReferenceInContext context reference of
                        Nothing ->
                            Nothing

                        Just resolved ->
                            Just ( nameRange, ResolvedDeclarationTarget resolved )

                else
                    Nothing



-- Name resolution within a module


type alias ModuleResolutionContext =
    { -- Declarations visible without qualification, first match wins
      localItems : List ( String, ( LanguageServiceAnalysis.DeclarationScope, ResolvedDeclaration ) )
    , importedModules : List ( List String, ImportedModule )
    }


resolutionContextForModule :
    ParsedModuleCache
    -> LanguageServiceInterface.FileLocation
    -> List ResolvedDeclaration
    -> List ImportedModule
    -> ModuleResolutionContext
resolutionContextForModule parsedModule fileLocation implicitTopLevelImports importedModules =
    let
        ownDeclarations :
            Bool
            -> List ( String, ( LanguageServiceAnalysis.DeclarationScope, ResolvedDeclaration ) )
        ownDeclarations topLevel =
            parsedModule.analysis.declarations
                |> List.filterMap
                    (\occurrence ->
                        if declarationIsTopLevel occurrence == topLevel then
                            Just
                                ( occurrence.name
                                , ( occurrence.scope
                                  , { fileLocation = fileLocation
                                    , parsedModule = parsedModule
                                    , occurrence = occurrence
                                    }
                                  )
                                )

                        else
                            Nothing
                    )

        exposedImportedDeclarations : List ( String, ( LanguageServiceAnalysis.DeclarationScope, ResolvedDeclaration ) )
        exposedImportedDeclarations =
            importedModules
                |> List.concatMap
                    (\importedModule ->
                        case importedModule.exposingList of
                            Nothing ->
                                []

                            Just exposingList ->
                                declarationsExposedByImport exposingList importedModule.parsedModule
                                    |> List.map
                                        (\occurrence ->
                                            ( occurrence.name
                                            , ( LanguageServiceAnalysis.TopLevelScope
                                              , { fileLocation = importedModule.fileLocation
                                                , parsedModule = importedModule.parsedModule
                                                , occurrence = occurrence
                                                }
                                              )
                                            )
                                        )
                    )
    in
    { localItems =
        List.concat
            [ ownDeclarations True
            , exposedImportedDeclarations
            , List.map
                (\resolved ->
                    ( resolved.occurrence.name
                    , ( LanguageServiceAnalysis.TopLevelScope, resolved )
                    )
                )
                implicitTopLevelImports
            , ownDeclarations False
            ]
    , importedModules =
        List.map
            (\importedModule -> ( importedModule.importedName, importedModule ))
            importedModules
    }


declarationsExposedByImport :
    ElmSyntax.Abstract.Exposing.Exposing
    -> ParsedModuleCache
    -> List LanguageServiceAnalysis.DeclarationOccurrence
declarationsExposedByImport exposingList parsedModule =
    let
        exposedDeclarations : List LanguageServiceAnalysis.DeclarationOccurrence
        exposedDeclarations =
            exposedTopLevelDeclarations parsedModule
    in
    case exposingList of
        ElmSyntax.Abstract.Exposing.All ->
            exposedDeclarations

        ElmSyntax.Abstract.Exposing.Explicit topLevelExposings ->
            topLevelExposings
                |> List.concatMap
                    (\topLevelExpose ->
                        let
                            exposedName : String
                            exposedName =
                                LanguageServiceAnalysis.nameOfTopLevelExpose topLevelExpose
                        in
                        List.filter
                            (\occurrence -> occurrence.name == exposedName)
                            exposedDeclarations
                    )


resolveReferenceInContext :
    ModuleResolutionContext
    -> LanguageServiceAnalysis.ReferenceOccurrence
    -> Maybe ResolvedDeclaration
resolveReferenceInContext context reference =
    if reference.moduleName == [] then
        case Common.assocListGet reference.name context.localItems of
            Nothing ->
                Nothing

            Just ( scope, resolved ) ->
                case scope of
                    LanguageServiceAnalysis.TopLevelScope ->
                        Just resolved

                    LanguageServiceAnalysis.LocalScope scopePath ->
                        if ElmSyntax.Path.isPrefixOf scopePath reference.path then
                            Just resolved

                        else
                            Nothing

    else
        case Common.assocListGet reference.moduleName context.importedModules of
            Nothing ->
                Nothing

            Just importedModule ->
                importedModule.parsedModule.analysis.declarations
                    |> Common.listMapFind
                        (\occurrence ->
                            if
                                (occurrence.name == reference.name)
                                    && occurrence.isExposed
                                    && declarationIsTopLevel occurrence
                            then
                                Just
                                    { fileLocation = importedModule.fileLocation
                                    , parsedModule = importedModule.parsedModule
                                    , occurrence = occurrence
                                    }

                            else
                                Nothing
                        )



-- Completion


provideCompletionItems :
    LanguageServiceInterface.ProvideCompletionItemsRequestStruct
    -> LanguageServiceState
    -> List Frontend.MonacoEditor.MonacoCompletionItem
provideCompletionItems request languageServiceState =
    case Dict.get request.filePathOpenedInEditor languageServiceState.documentCache of
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
                                        parsedFile.concrete
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
    -> List LanguageServiceInterface.LocationInFile
provideDefinition request languageServiceState =
    provideDefinitionInternal
        request
        languageServiceState
        |> List.map
            (\(LocationInFile fileLocation (DeclarationRange (Range ( startRow, startColumn ) ( endRow, endColumn )) _)) ->
                { fileLocation = fileLocation
                , range =
                    { startLineNumber = startRow
                    , startColumn = startColumn
                    , endLineNumber = endRow
                    , endColumn = endColumn
                    }
                }
            )


provideDefinitionInternal :
    LanguageServiceInterface.ProvideDefinitionRequestStruct
    -> LanguageServiceState
    -> List (LocationInFile DeclarationRange)
provideDefinitionInternal request languageServiceState =
    hoverItemsAtLocation
        request.fileLocation
        ( request.positionLineNumber, request.positionColumn )
        languageServiceState
        |> List.map (\( _, locationInFile, _ ) -> locationInFile)


textDocumentSymbol :
    String
    -> LanguageServiceState
    -> List LanguageServiceInterface.DocumentSymbol
textDocumentSymbol fileUri languageServiceState =
    case Dict.get fileUri languageServiceState.documentCache of
        Nothing ->
            []

        Just currentFileCacheItem ->
            case currentFileCacheItem.parsedFileLastSuccess of
                Nothing ->
                    []

                Just parsedFileLastSuccess ->
                    let
                        topLevelOccurrences : List LanguageServiceAnalysis.DeclarationOccurrence
                        topLevelOccurrences =
                            topLevelDeclarations parsedFileLastSuccess
                    in
                    topLevelOccurrences
                        |> List.filter
                            (\occurrence ->
                                occurrence.kind /= LanguageServiceAnalysis.ChoiceTypeTagDeclarationKind
                            )
                        |> List.filterMap
                            (documentSymbolFromOccurrence
                                parsedFileLastSuccess
                                topLevelOccurrences
                            )


documentSymbolFromOccurrence :
    ParsedModuleCache
    -> List LanguageServiceAnalysis.DeclarationOccurrence
    -> LanguageServiceAnalysis.DeclarationOccurrence
    -> Maybe LanguageServiceInterface.DocumentSymbol
documentSymbolFromOccurrence parsedModule allOccurrences occurrence =
    case declarationRangeOfOccurrence parsedModule occurrence of
        Nothing ->
            Nothing

        Just (DeclarationRange wholeRange _) ->
            let
                selectionRange : Range
                selectionRange =
                    case rangeAtPathInModule parsedModule occurrence.declarationPath SelectName of
                        Nothing ->
                            wholeRange

                        Just range ->
                            range

                children : List LanguageServiceInterface.DocumentSymbol
                children =
                    case occurrence.kind of
                        LanguageServiceAnalysis.ChoiceTypeDeclarationKind ->
                            allOccurrences
                                |> List.filter
                                    (\candidate ->
                                        candidate.kind == LanguageServiceAnalysis.ChoiceTypeTagDeclarationKind
                                            && ElmSyntax.Path.isPrefixOf
                                                occurrence.declarationPath
                                                candidate.declarationPath
                                    )
                                |> List.filterMap
                                    (documentSymbolFromOccurrence parsedModule allOccurrences)

                        LanguageServiceAnalysis.FunctionOrValueDeclarationKind ->
                            []

                        LanguageServiceAnalysis.TypeAliasDeclarationKind ->
                            []

                        LanguageServiceAnalysis.ChoiceTypeTagDeclarationKind ->
                            []
            in
            Just
                (LanguageServiceInterface.DocumentSymbol
                    { name = occurrence.name
                    , range = monacoRangeFromRange wholeRange
                    , selectionRange = monacoRangeFromRange selectionRange
                    , kind = symbolKindFromDeclarationKind occurrence.kind
                    , children = children
                    }
                )


symbolKindFromDeclarationKind :
    LanguageServiceAnalysis.DeclarationKind
    -> LanguageServiceInterface.SymbolKind
symbolKindFromDeclarationKind kind =
    case kind of
        LanguageServiceAnalysis.FunctionOrValueDeclarationKind ->
            LanguageServiceInterface.SymbolKind_Function

        LanguageServiceAnalysis.TypeAliasDeclarationKind ->
            LanguageServiceInterface.SymbolKind_Struct

        LanguageServiceAnalysis.ChoiceTypeDeclarationKind ->
            LanguageServiceInterface.SymbolKind_Enum

        LanguageServiceAnalysis.ChoiceTypeTagDeclarationKind ->
            LanguageServiceInterface.SymbolKind_EnumMember



-- References and rename


textDocumentReferences :
    LanguageServiceInterface.ProvideReferencesRequestStruct
    -> LanguageServiceState
    -> List LanguageServiceInterface.LocationInFile
textDocumentReferences referenceRequest languageServiceState =
    case
        textDocumentReferencesGroupedByFilePath
            referenceRequest
            languageServiceState
    of
        Nothing ->
            []

        Just ( _, references ) ->
            references
                |> List.concatMap
                    (\( fileLocation, ranges ) ->
                        ranges
                            |> List.map
                                (\(Range ( startRow, startColumn ) ( endRow, endColumn )) ->
                                    { fileLocation = fileLocation
                                    , range =
                                        { startLineNumber = startRow
                                        , startColumn = startColumn
                                        , endLineNumber = endRow
                                        , endColumn = endColumn
                                        }
                                    }
                                )
                    )


textDocumentReferencesGroupedByFilePath :
    LanguageServiceInterface.ProvideReferencesRequestStruct
    -> LanguageServiceState
    ->
        Maybe
            ( ( LanguageServiceInterface.FileLocation, DeclarationRange )
            , List ( LanguageServiceInterface.FileLocation, List Range )
            )
textDocumentReferencesGroupedByFilePath referenceRequest languageServiceState =
    case parsedModuleAtFileLocation referenceRequest.fileLocation languageServiceState of
        Nothing ->
            Nothing

        Just parsedFile ->
            let
                position : ( Int, Int )
                position =
                    ( referenceRequest.positionLineNumber
                    , referenceRequest.positionColumn
                    )

                maybeTarget : Maybe ResolvedTarget
                maybeTarget =
                    case
                        resolveAtLocation
                            parsedFile
                            referenceRequest.fileLocation
                            position
                            languageServiceState
                    of
                        Just ( _, target ) ->
                            Just target

                        Nothing ->
                            -- Fallback: the name of a declaration under the cursor
                            declarationTargetAtLocation
                                parsedFile
                                referenceRequest.fileLocation
                                position
            in
            case maybeTarget of
                Nothing ->
                    Nothing

                Just target ->
                    case targetDeclarationRange target of
                        Nothing ->
                            Nothing

                        Just declarationRange ->
                            Just
                                ( ( targetFileLocation target
                                  , declarationRange
                                  )
                                , findReferences target languageServiceState
                                )


{-| Find the declaration whose own name covers the given location. Used when the
cursor is on a declaration instead of on a reference.
-}
declarationTargetAtLocation :
    ParsedModuleCache
    -> LanguageServiceInterface.FileLocation
    -> ( Int, Int )
    -> Maybe ResolvedTarget
declarationTargetAtLocation parsedModule fileLocation location =
    let
        cursorPath : Path
        cursorPath =
            ElmSyntax.Concrete.SourceLookup.pathAtLocation
                { row = Tuple.first location, column = Tuple.second location }
                parsedModule.concrete
    in
    topLevelDeclarations parsedModule
        |> Common.listMapFind
            (\occurrence ->
                if List.member cursorPath occurrence.namePaths then
                    case rangeAtPathInModule parsedModule cursorPath SelectName of
                        Nothing ->
                            Nothing

                        Just nameRange ->
                            if rangeContainsLocation location nameRange then
                                Just
                                    (ResolvedDeclarationTarget
                                        { fileLocation = fileLocation
                                        , parsedModule = parsedModule
                                        , occurrence = occurrence
                                        }
                                    )

                            else
                                Nothing

                else
                    Nothing
            )


findReferences :
    ResolvedTarget
    -> LanguageServiceState
    -> List ( LanguageServiceInterface.FileLocation, List Range )
findReferences target languageServiceState =
    let
        allParsedModules :
            List
                ( LanguageServiceInterface.FileLocation
                , ParsedModuleCache
                )
        allParsedModules =
            List.concat
                [ languageServiceState.documentCache
                    |> Dict.toList
                    |> List.filterMap
                        (\( filePath, blob ) ->
                            Maybe.map
                                (\parsedModule ->
                                    ( LanguageServiceInterface.WorkspaceFileLocation filePath
                                    , parsedModule
                                    )
                                )
                                blob.parsedFileLastSuccess
                        )
                , languageServiceState.coreModulesCache
                    |> List.map
                        (\coreModule ->
                            ( LanguageServiceInterface.WorkspaceFileLocation coreModule.parseResult.fileUri
                            , coreModule.parseResult
                            )
                        )
                , languageServiceState.elmPackages
                    |> List.concatMap
                        (\( packageVersionIdentifer, packageModules ) ->
                            packageModules
                                |> List.map
                                    (\( _, ( modulePath, parsedModule ) ) ->
                                        ( LanguageServiceInterface.ElmPackageFileLocation
                                            packageVersionIdentifer
                                            modulePath
                                        , parsedModule
                                        )
                                    )
                        )
                ]

        -- Implicit top-level imports do not depend on the module being scanned,
        -- so compute them once instead of once per module.
        implicitTopLevelImports : List ResolvedDeclaration
        implicitTopLevelImports =
            commonImplicitTopLevelImports languageServiceState

        findReferencesInModule :
            ( LanguageServiceInterface.FileLocation, ParsedModuleCache )
            -> Maybe ( LanguageServiceInterface.FileLocation, List Range )
        findReferencesInModule ( fileLocation, parsedModule ) =
            let
                ranges : List Range
                ranges =
                    referenceRangesInModuleResolvingTo
                        target
                        implicitTopLevelImports
                        fileLocation
                        parsedModule
                        languageServiceState
            in
            if ranges == [] then
                Nothing

            else
                Just ( fileLocation, ranges )
    in
    allParsedModules
        |> List.filterMap findReferencesInModule


{-| Ranges of all references in a single module that resolve to the given
target. When the target's name is known, only references that share that name
are resolved.
-}
referenceRangesInModuleResolvingTo :
    ResolvedTarget
    -> List ResolvedDeclaration
    -> LanguageServiceInterface.FileLocation
    -> ParsedModuleCache
    -> LanguageServiceState
    -> List Range
referenceRangesInModuleResolvingTo target implicitTopLevelImports currentModuleFileLocation parsedModule languageServiceState =
    let
        importedModules : List ImportedModule
        importedModules =
            importedModulesFromModule parsedModule languageServiceState

        context : ModuleResolutionContext
        context =
            resolutionContextForModule
                parsedModule
                currentModuleFileLocation
                implicitTopLevelImports
                importedModules

        identity : ( LanguageServiceInterface.FileLocation, Path )
        identity =
            targetIdentity target

        maybeTargetName : Maybe String
        maybeTargetName =
            targetName target

        resolveReference : LanguageServiceAnalysis.ReferenceOccurrence -> Maybe Range
        resolveReference reference =
            case resolveReferenceInContext context reference of
                Nothing ->
                    Nothing

                Just resolved ->
                    if
                        ( resolved.fileLocation, resolved.occurrence.declarationPath )
                            == identity
                    then
                        rangeAtPathInModule parsedModule reference.path SelectName

                    else
                        Nothing

        nameReferenceRanges : List Range
        nameReferenceRanges =
            parsedModule.references
                |> List.filterMap
                    (\reference ->
                        case maybeTargetName of
                            Just name ->
                                if reference.name == name then
                                    resolveReference reference

                                else
                                    Nothing

                            Nothing ->
                                resolveReference reference
                    )

        -- References to imported module names, relevant when the target is a
        -- module declaration.
        importNameReferenceRanges : List Range
        importNameReferenceRanges =
            importedModules
                |> List.concatMap
                    (\importedModule ->
                        if ( importedModule.fileLocation, [ StepModuleDefinition ] ) == identity then
                            List.filterMap
                                (\moduleNamePath ->
                                    rangeAtPathInModule parsedModule moduleNamePath SelectWhole
                                )
                                importedModule.moduleNamePaths

                        else
                            []
                    )
    in
    List.concat
        [ importNameReferenceRanges
        , nameReferenceRanges
        ]


textDocumentRename :
    LanguageServiceInterface.RenameParams
    -> LanguageServiceState
    -> LanguageServiceInterface.WorkspaceEdit
textDocumentRename renameParams languageServiceState =
    case
        textDocumentReferencesGroupedByFilePath
            { fileLocation = LanguageServiceInterface.WorkspaceFileLocation renameParams.filePath
            , positionLineNumber = renameParams.positionLineNumber
            , positionColumn = renameParams.positionColumn
            }
            languageServiceState
    of
        Nothing ->
            []

        Just ( ( LanguageServiceInterface.ElmPackageFileLocation _ _, _ ), _ ) ->
            []

        Just ( ( LanguageServiceInterface.WorkspaceFileLocation declFilePath, DeclarationRange _ declNamesRanges ), referencesGroupedByFilePath ) ->
            let
                newName : String
                newName =
                    renameParams.newName

                declarationEdits : List LanguageServiceInterface.TextEdit
                declarationEdits =
                    List.map
                        (\range ->
                            { range = monacoRangeFromRange range
                            , newText = newName
                            }
                        )
                        declNamesRanges

                declarationFileReferencesEdits : List LanguageServiceInterface.TextEdit
                declarationFileReferencesEdits =
                    case
                        Common.assocListGet
                            (LanguageServiceInterface.WorkspaceFileLocation declFilePath)
                            referencesGroupedByFilePath
                    of
                        Nothing ->
                            []

                        Just ranges ->
                            List.map
                                (\range ->
                                    { range = monacoRangeFromRange range
                                    , newText = newName
                                    }
                                )
                                ranges

                otherFilesReferencesEdits : List LanguageServiceInterface.TextDocumentEdit
                otherFilesReferencesEdits =
                    referencesGroupedByFilePath
                        |> List.concatMap
                            (\( fileLocation, ranges ) ->
                                case fileLocation of
                                    LanguageServiceInterface.WorkspaceFileLocation filePath ->
                                        if filePath == declFilePath then
                                            []

                                        else
                                            [ { filePath = filePath
                                              , edits =
                                                    List.map
                                                        (\range ->
                                                            { range = monacoRangeFromRange range
                                                            , newText = newName
                                                            }
                                                        )
                                                        ranges
                                              }
                                            ]

                                    LanguageServiceInterface.ElmPackageFileLocation _ _ ->
                                        []
                            )

                workspaceEdits : List LanguageServiceInterface.TextDocumentEdit
                workspaceEdits =
                    { filePath = declFilePath
                    , edits =
                        List.concat
                            [ declarationEdits
                            , declarationFileReferencesEdits
                            ]
                    }
                        :: otherFilesReferencesEdits
            in
            workspaceEdits


monacoRangeFromRange : Range -> Frontend.MonacoEditor.MonacoRange
monacoRangeFromRange (Range ( startRow, startColumn ) ( endRow, endColumn )) =
    { startLineNumber = startRow
    , startColumn = startColumn
    , endLineNumber = endRow
    , endColumn = endColumn
    }


provideCompletionItemsInModule :
    { fileOpenedInEditor : ParsedModuleCache, newText : String, cursorLineNumber : Int, cursorColumn : Int }
    -> LanguageServiceState
    -> List CompletionItem
provideCompletionItemsInModule request languageServiceState =
    let
        fileOpenedInEditor : ParsedModuleCache
        fileOpenedInEditor =
            request.fileOpenedInEditor

        fileOpenedInEditorModuleName : List String
        fileOpenedInEditorModuleName =
            fileOpenedInEditor.analysis.moduleName

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

        moduleNamesToNotSuggestForImport : List (List String)
        moduleNamesToNotSuggestForImport =
            [ fileOpenedInEditorModuleName ]

        modulesToSuggestForImport : List ParsedModuleCache
        modulesToSuggestForImport =
            modulesAvailableForImport
                |> List.filterMap
                    (\availableModule ->
                        if List.member availableModule.analysis.moduleName moduleNamesToNotSuggestForImport then
                            Nothing

                        else
                            Just availableModule
                    )
                |> List.sortBy
                    (\availableModule ->
                        String.join "." availableModule.analysis.moduleName
                    )

        importedModules : List ImportedModule
        importedModules =
            importedModulesFromModule fileOpenedInEditor languageServiceState

        fromLocals : List CompletionItem
        fromLocals =
            completionItemsForDeclarations
                fileOpenedInEditor
                (fileOpenedInEditor.analysis.declarations
                    |> List.filter
                        (\occurrence ->
                            case occurrence.scope of
                                LanguageServiceAnalysis.TopLevelScope ->
                                    False

                                LanguageServiceAnalysis.LocalScope scopePath ->
                                    case rangeAtPathInModule fileOpenedInEditor scopePath SelectWhole of
                                        Nothing ->
                                            False

                                        Just scopeRange ->
                                            rangeContainsLocation
                                                ( request.cursorLineNumber, String.length lineUntilPosition )
                                                scopeRange
                        )
                )

        importExposings : List CompletionItem
        importExposings =
            List.concat
                [ importExposingsFromModule fileOpenedInEditor languageServiceState
                , completionItemsForResolvedDeclarations
                    (commonImplicitTopLevelImports languageServiceState)
                ]

        localDeclarationsAndImportExposings : List CompletionItem
        localDeclarationsAndImportExposings =
            List.concat
                [ completionItemsForDeclarations
                    fileOpenedInEditor
                    (topLevelDeclarations fileOpenedInEditor)
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
                of
                    Nothing ->
                        []

                    Just referencedModule ->
                        completionItemsForDeclarations
                            referencedModule.parsedModule
                            (exposedTopLevelDeclarations referencedModule.parsedModule)

        importedModulesAfterPrefix : List ( List String, ImportedModule )
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
                |> List.map
                    (\( importedModuleNameRestAfterPrefix, importedModule ) ->
                        moduleCompletionItemFromModule
                            { importedName = Just importedModule.importedName
                            , importedModuleNameRestAfterPrefix = Just importedModuleNameRestAfterPrefix
                            }
                            importedModule.parsedModule
                    )
    in
    case lineUntilPositionWords of
        "import" :: _ ->
            modulesToSuggestForImport
                |> List.map
                    (moduleCompletionItemFromModule
                        { importedModuleNameRestAfterPrefix = Nothing, importedName = Nothing }
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


completionItemsForResolvedDeclarations : List ResolvedDeclaration -> List CompletionItem
completionItemsForResolvedDeclarations resolvedDeclarations =
    List.map
        (\resolved ->
            completionItemForDeclaration
                (String.lines resolved.parsedModule.text)
                resolved.parsedModule
                resolved.occurrence
        )
        resolvedDeclarations


importedModulesFromModule :
    ParsedModuleCache
    -> LanguageServiceState
    -> List ImportedModule
importedModulesFromModule parsedModule languageServiceState =
    let
        implicitlyImportedModulesOld : List ImportedModule
        implicitlyImportedModulesOld =
            languageServiceState.coreModulesCache
                |> List.filterMap
                    (\coreModule ->
                        if coreModule.implicitImport then
                            let
                                canonicalName : List String
                                canonicalName =
                                    coreModule.parseResult.analysis.moduleName
                            in
                            Just
                                { fileLocation =
                                    LanguageServiceInterface.WorkspaceFileLocation coreModule.parseResult.fileUri
                                , canonicalName = canonicalName
                                , importedName = canonicalName
                                , exposingList = Nothing
                                , parsedModule = coreModule.parseResult
                                , moduleNamePaths = []
                                }

                        else
                            Nothing
                    )

        implicitlyImportedModules : List ImportedModule
        implicitlyImportedModules =
            languageServiceState.elmPackages
                |> List.concatMap
                    (\( packageVersionIdentifer, packageModules ) ->
                        case packageVersionIdentifer of
                            LanguageServiceInterface.ElmPackageVersion019Identifer "elm/core" _ ->
                                packageModules
                                    |> List.filterMap
                                        (\( moduleModuleName, ( modulePath, packageModule ) ) ->
                                            if elmCoreModuleIsImplicitlyImported moduleModuleName then
                                                Just
                                                    { fileLocation =
                                                        LanguageServiceInterface.ElmPackageFileLocation packageVersionIdentifer modulePath
                                                    , canonicalName = moduleModuleName
                                                    , importedName = moduleModuleName
                                                    , exposingList = Nothing
                                                    , parsedModule = packageModule
                                                    , moduleNamePaths = []
                                                    }

                                            else
                                                Nothing
                                        )

                            _ ->
                                []
                    )

        explicitlyImportedModules : List ImportedModule
        explicitlyImportedModules =
            parsedModule.analysis.imports
                |> List.filterMap
                    (\importOccurrence ->
                        case moduleByCanonicalName importOccurrence.canonicalName languageServiceState of
                            Nothing ->
                                Nothing

                            Just ( moduleFileLocation, importedParsedModule ) ->
                                Just
                                    { fileLocation = moduleFileLocation
                                    , canonicalName = importOccurrence.canonicalName
                                    , importedName = importOccurrence.importedName
                                    , exposingList = importOccurrence.exposingList
                                    , parsedModule = importedParsedModule
                                    , moduleNamePaths = [ importOccurrence.moduleNamePath ]
                                    }
                    )
    in
    List.concat
        [ implicitlyImportedModules
        , implicitlyImportedModulesOld
        , explicitlyImportedModules
        ]


moduleByCanonicalName :
    List String
    -> LanguageServiceState
    -> Maybe ( LanguageServiceInterface.FileLocation, ParsedModuleCache )
moduleByCanonicalName canonicalModuleName languageServiceState =
    case
        modulesAvailableForImportFromState languageServiceState
            |> Common.listMapFind
                (\moduleAvailable ->
                    if moduleAvailable.analysis.moduleName == canonicalModuleName then
                        Just
                            ( LanguageServiceInterface.WorkspaceFileLocation moduleAvailable.fileUri
                            , moduleAvailable
                            )

                    else
                        Nothing
                )
    of
        Just fromWorkspace ->
            Just fromWorkspace

        Nothing ->
            findModuleInPackagesByModuleName canonicalModuleName languageServiceState


findModuleInPackagesByModuleName :
    List String
    -> LanguageServiceState
    -> Maybe ( LanguageServiceInterface.FileLocation, ParsedModuleCache )
findModuleInPackagesByModuleName moduleName languageServiceState =
    languageServiceState.elmPackages
        |> Common.listMapFind
            (\( packageVersionIdentifer, packageModules ) ->
                packageModules
                    |> Common.listMapFind
                        (\( moduleModuleName, ( modulePath, packageModule ) ) ->
                            if moduleModuleName == moduleName then
                                Just
                                    ( LanguageServiceInterface.ElmPackageFileLocation packageVersionIdentifer modulePath
                                    , packageModule
                                    )

                            else
                                Nothing
                        )
            )


modulesAvailableForImportFromState : LanguageServiceState -> List ParsedModuleCache
modulesAvailableForImportFromState languageServiceState =
    List.concat
        [ languageServiceState.documentCache
            |> Dict.toList
            |> List.filterMap
                (\( _, fileCache ) ->
                    fileCache.parsedFileLastSuccess
                )
        , List.map .parseResult languageServiceState.coreModulesCache
        ]


importExposingsFromModule :
    ParsedModuleCache
    -> LanguageServiceState
    -> List CompletionItem
importExposingsFromModule fileOpenedInEditor languageServiceState =
    fileOpenedInEditor.analysis.imports
        |> List.concatMap
            (\importOccurrence ->
                case importOccurrence.exposingList of
                    Nothing ->
                        []

                    Just exposingList ->
                        case moduleByCanonicalName importOccurrence.canonicalName languageServiceState of
                            Nothing ->
                                []

                            Just ( _, importedParsedModule ) ->
                                let
                                    exposedDeclarations : List LanguageServiceAnalysis.DeclarationOccurrence
                                    exposedDeclarations =
                                        exposedTopLevelDeclarations importedParsedModule
                                in
                                completionItemsForDeclarations
                                    importedParsedModule
                                    (case exposingList of
                                        ElmSyntax.Abstract.Exposing.All ->
                                            exposedDeclarations

                                        ElmSyntax.Abstract.Exposing.Explicit topLevelExposings ->
                                            topLevelExposings
                                                |> List.concatMap
                                                    (\topLevelExpose ->
                                                        let
                                                            exposedName : String
                                                            exposedName =
                                                                LanguageServiceAnalysis.nameOfTopLevelExpose topLevelExpose
                                                        in
                                                        List.filter
                                                            (\occurrence -> occurrence.name == exposedName)
                                                            exposedDeclarations
                                                    )
                                    )
            )


commonImplicitTopLevelImports :
    LanguageServiceState
    -> List ResolvedDeclaration
commonImplicitTopLevelImports languageServiceState =
    if languageServiceState.elmPackages == [] then
        commonImplicitTopLevelImportsOld languageServiceState

    else
        commonImplicitTopLevelImportsNew languageServiceState


commonImplicitTopLevelImportsOld :
    LanguageServiceState
    -> List ResolvedDeclaration
commonImplicitTopLevelImportsOld languageServiceState =
    languageServiceState.coreModulesCache
        |> List.concatMap
            (\coreModule ->
                let
                    moduleName : List String
                    moduleName =
                        coreModule.parseResult.analysis.moduleName
                in
                coreModule.parseResult.analysis.declarations
                    |> List.filterMap
                        (\occurrence ->
                            if
                                occurrence.isExposed
                                    && declarationIsTopLevel occurrence
                                    && isItemImplicitlyExposed moduleName occurrence.name
                            then
                                Just
                                    { -- TODO: Use constant
                                      fileLocation =
                                        LanguageServiceInterface.WorkspaceFileLocation "elm/core"
                                    , parsedModule = coreModule.parseResult
                                    , occurrence = occurrence
                                    }

                            else
                                Nothing
                        )
            )


commonImplicitTopLevelImportsNew :
    LanguageServiceState
    -> List ResolvedDeclaration
commonImplicitTopLevelImportsNew languageServiceState =
    languageServiceState.elmPackages
        |> List.concatMap
            (\( packageVersionIdentifer, packageModules ) ->
                case packageVersionIdentifer of
                    LanguageServiceInterface.ElmPackageVersion019Identifer "elm/core" _ ->
                        packageModules
                            |> List.concatMap
                                (\( moduleName, ( moduleFilePath, packageModule ) ) ->
                                    packageModule.analysis.declarations
                                        |> List.filterMap
                                            (\occurrence ->
                                                if
                                                    occurrence.isExposed
                                                        && declarationIsTopLevel occurrence
                                                        && isItemImplicitlyExposed moduleName occurrence.name
                                                then
                                                    Just
                                                        { fileLocation =
                                                            LanguageServiceInterface.ElmPackageFileLocation
                                                                packageVersionIdentifer
                                                                moduleFilePath
                                                        , parsedModule = packageModule
                                                        , occurrence = occurrence
                                                        }

                                                else
                                                    Nothing
                                            )
                                )

                    _ ->
                        []
            )


elmCoreModuleIsImplicitlyImported : List String -> Bool
elmCoreModuleIsImplicitlyImported moduleName =
    case moduleName of
        [ "Basics" ] ->
            True

        [ "List" ] ->
            True

        [ "Maybe" ] ->
            True

        [ "Result" ] ->
            True

        [ "String" ] ->
            True

        [ "Platform" ] ->
            True

        _ ->
            False


isItemImplicitlyExposed : List String -> String -> Bool
isItemImplicitlyExposed moduleName itemInsertText =
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

        [ "List" ] ->
            case itemInsertText of
                "List" ->
                    True

                _ ->
                    False

        [ "Result" ] ->
            case itemInsertText of
                "Result" ->
                    True

                "Ok" ->
                    True

                "Err" ->
                    True

                _ ->
                    False

        [ "Platform", "Cmd" ] ->
            True

        _ ->
            False


moduleCompletionItemFromModule :
    { importedModuleNameRestAfterPrefix : Maybe (List String), importedName : Maybe (List String) }
    -> ParsedModuleCache
    -> CompletionItem
moduleCompletionItemFromModule { importedModuleNameRestAfterPrefix, importedName } parsedModule =
    let
        canonicalName : List String
        canonicalName =
            parsedModule.analysis.moduleName

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
            case documentationStringFromModule parsedModule of
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


documentationStringFromModule : ParsedModuleCache -> Maybe String
documentationStringFromModule parsedModule =
    let
        concrete : ElmSyntax.Concrete.File.File
        concrete =
            parsedModule.concrete

        (ElmSyntax.Concrete.Node.Node moduleDefinitionRange _) =
            concrete.moduleDefinition

        importsAndDeclarationsStartRows : List Int
        importsAndDeclarationsStartRows =
            List.concat
                [ List.map
                    (\(ElmSyntax.Concrete.Node.Node range _) -> range.start.row)
                    concrete.imports
                , List.map
                    (\(ElmSyntax.Concrete.Node.Node range _) -> range.start.row)
                    concrete.declarations
                ]

        importsAndDeclarationsStartRow : Int
        importsAndDeclarationsStartRow =
            case importsAndDeclarationsStartRows of
                [] ->
                    0

                first :: rest ->
                    List.foldl min first rest

        maybeModuleComment : Maybe (ElmSyntax.Concrete.Node.Node String)
        maybeModuleComment =
            List.foldl
                (\comment maybeComment ->
                    let
                        (ElmSyntax.Concrete.Node.Node commentRange _) =
                            comment
                    in
                    case maybeComment of
                        Nothing ->
                            if
                                (commentRange.start.row > moduleDefinitionRange.start.row)
                                    && (commentRange.start.row < importsAndDeclarationsStartRow)
                            then
                                Just comment

                            else
                                Nothing

                        Just prevComment ->
                            let
                                (ElmSyntax.Concrete.Node.Node prevCommentRange _) =
                                    prevComment
                            in
                            if
                                (commentRange.start.row > prevCommentRange.end.row)
                                    && (commentRange.start.row < importsAndDeclarationsStartRow)
                            then
                                Just comment

                            else
                                Just prevComment
                )
                Nothing
                concrete.comments
    in
    case maybeModuleComment of
        Nothing ->
            Nothing

        Just (ElmSyntax.Concrete.Node.Node _ commentText) ->
            Just (removeWrappingFromMultilineComment commentText)


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
        compileFileCacheEntry : ( String, LanguageServiceInterface.FileTreeBlobNode ) -> LanguageServiceStateFileTreeNodeBlob
        compileFileCacheEntry ( blobPath, fileTreeBlob ) =
            let
                maybePreviousCached : Maybe LanguageServiceStateFileTreeNodeBlob
                maybePreviousCached =
                    Dict.get blobPath state.documentCache

                buildNewEntry () =
                    let
                        textContent : Maybe FileTextContent
                        textContent =
                            case fileTreeBlob.asText of
                                Nothing ->
                                    Nothing

                                Just asString ->
                                    Just
                                        { text = asString
                                        , parsedFile = parseModuleText blobPath asString
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

        documentCache : Dict.Dict String LanguageServiceStateFileTreeNodeBlob
        documentCache =
            fileTree
                |> FileTree.flatListOfBlobsFromFileTreeNode
                |> List.map
                    (\( filePath, fileContent ) ->
                        let
                            filePathFlat : String
                            filePathFlat =
                                String.join "/" filePath
                        in
                        ( filePathFlat
                        , compileFileCacheEntry ( filePathFlat, fileContent )
                        )
                    )
                |> Dict.fromList
    in
    { state
        | documentCache = documentCache
    }


removeWrappingFromMultilineComment : String -> String
removeWrappingFromMultilineComment withWrapping =
    let
        trimmed : String
        trimmed =
            String.trim withWrapping

        dropLeftCount : Int
        dropLeftCount =
            if String.startsWith "{-|" trimmed then
                3

            else if String.startsWith "{-" trimmed then
                2

            else
                0

        dropRightCount : Int
        dropRightCount =
            if String.endsWith "-}" trimmed then
                2

            else
                0

        lessCommentTokens : String
        lessCommentTokens =
            String.slice
                dropLeftCount
                (String.length trimmed - dropRightCount)
                trimmed
    in
    String.trim lessCommentTokens


locationIsInComment : ElmSyntax.Concrete.Range.Location -> ElmSyntax.Concrete.File.File -> Bool
locationIsInComment location concrete =
    List.any
        (\(ElmSyntax.Concrete.Node.Node commentRange comment) ->
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
        concrete.comments
        || List.any
            (\documentationRange -> rangeIntersectsLocation location documentationRange)
            (documentationRangesInFile concrete)


documentationRangesInFile : ElmSyntax.Concrete.File.File -> List ElmSyntax.Concrete.Range.Range
documentationRangesInFile concrete =
    concrete.declarations
        |> List.indexedMap
            (\index _ ->
                ElmSyntax.Concrete.SourceLookup.rangeAtPath
                    [ StepDeclaration index ]
                    SelectDocumentation
                    concrete
            )
        |> List.filterMap identity


rangeIntersectsLocation : ElmSyntax.Concrete.Range.Location -> ElmSyntax.Concrete.Range.Range -> Bool
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


charIsAllowedInDeclarationName : Char -> Bool
charIsAllowedInDeclarationName char =
    Char.isAlphaNum char || char == '_'


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
