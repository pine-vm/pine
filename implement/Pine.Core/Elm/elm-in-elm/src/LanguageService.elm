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
import ElmSyntax.Concrete.Declaration
import ElmSyntax.Concrete.File
import ElmSyntax.Concrete.Import
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
            parseCoreModules elmCoreModules
    in
    { documentCache = Dict.empty
    , coreModulesCache = elmCoreModulesParseResults
    , elmPackages = []
    }


parseCoreModules :
    List { moduleText : String, implicitImport : Bool }
    -> List ElmCoreModule
parseCoreModules coreModules =
    case coreModules of
        coreModule :: rest ->
            case parseModuleText "elm-core" coreModule.moduleText of
                Nothing ->
                    parseCoreModules rest

                Just parsedModule ->
                    { parseResult = parsedModule
                    , implicitImport = coreModule.implicitImport
                    }
                        :: parseCoreModules rest

        [] ->
            []


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
            parsePackageModules packageModules

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


parsePackageModules :
    List ( List String, LanguageServiceInterface.FileTreeBlobNode )
    -> List ( List String, ( List String, ParsedModuleCache ) )
parsePackageModules packageModules =
    case packageModules of
        ( modulePath, fileContent ) :: rest ->
            case fileContent.asText of
                Nothing ->
                    parsePackageModules rest

                Just asString ->
                    case parseModuleText (String.join "/" modulePath) asString of
                        Nothing ->
                            parsePackageModules rest

                        Just parsedModule ->
                            ( parsedModule.analysis.moduleName
                            , ( modulePath, parsedModule )
                            )
                                :: parsePackageModules rest

        [] ->
            []



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
                    (rangesAtNamePaths parsedModule occurrence.namePaths)
                )


rangesAtNamePaths : ParsedModuleCache -> List Path -> List Range
rangesAtNamePaths parsedModule paths =
    case paths of
        path :: rest ->
            case rangeAtPathInModule parsedModule path SelectName of
                Nothing ->
                    rangesAtNamePaths parsedModule rest

                Just range ->
                    range :: rangesAtNamePaths parsedModule rest

        [] ->
            []


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
            completionItemsForDeclarationsHelp textLines parsedModule occurrences []


completionItemsForDeclarationsHelp :
    List String
    -> ParsedModuleCache
    -> List LanguageServiceAnalysis.DeclarationOccurrence
    -> List CompletionItem
    -> List CompletionItem
completionItemsForDeclarationsHelp textLines parsedModule occurrences completionItemsReversed =
    case occurrences of
        [] ->
            List.reverse completionItemsReversed

        occurrence :: remainingOccurrences ->
            completionItemsForDeclarationsHelp
                textLines
                parsedModule
                remainingOccurrences
                (completionItemForDeclaration textLines parsedModule occurrence
                    :: completionItemsReversed
                )


topLevelDeclarations : ParsedModuleCache -> List LanguageServiceAnalysis.DeclarationOccurrence
topLevelDeclarations parsedModule =
    topLevelDeclarationsHelp False parsedModule.analysis.declarations


declarationIsTopLevel : LanguageServiceAnalysis.DeclarationOccurrence -> Bool
declarationIsTopLevel occurrence =
    case occurrence.scope of
        LanguageServiceAnalysis.TopLevelScope ->
            True

        LanguageServiceAnalysis.LocalScope _ ->
            False


exposedTopLevelDeclarations : ParsedModuleCache -> List LanguageServiceAnalysis.DeclarationOccurrence
exposedTopLevelDeclarations parsedModule =
    topLevelDeclarationsHelp True parsedModule.analysis.declarations


topLevelDeclarationsHelp :
    Bool
    -> List LanguageServiceAnalysis.DeclarationOccurrence
    -> List LanguageServiceAnalysis.DeclarationOccurrence
topLevelDeclarationsHelp requireExposed occurrences =
    case occurrences of
        occurrence :: rest ->
            if declarationIsTopLevel occurrence && (not requireExposed || occurrence.isExposed) then
                occurrence :: topLevelDeclarationsHelp requireExposed rest

            else
                topLevelDeclarationsHelp requireExposed rest

        [] ->
            []



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
        |> documentationFromHoverItems


documentationFromHoverItems :
    List ( Range, LocationInFile DeclarationRange, String )
    -> List String
documentationFromHoverItems hoverItems =
    case hoverItems of
        ( _, _, documentation ) :: rest ->
            documentation :: documentationFromHoverItems rest

        [] ->
            []


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
                                            case findRangeContainingLocation location nameRanges of
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


findRangeContainingLocation : ( Int, Int ) -> List Range -> Maybe Range
findRangeContainingLocation location ranges =
    case ranges of
        [] ->
            Nothing

        range :: remainingRanges ->
            if rangeContainsLocation location range then
                Just range

            else
                findRangeContainingLocation location remainingRanges


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
            parsedModuleInPackages packageVersionIdentifer modulePath languageServiceState.elmPackages


parsedModuleInPackages :
    LanguageServiceInterface.ElmPackageVersionIdentifer
    -> List String
    -> List
        ( LanguageServiceInterface.ElmPackageVersionIdentifer
        , List ( List String, ( List String, ParsedModuleCache ) )
        )
    -> Maybe ParsedModuleCache
parsedModuleInPackages packageVersionIdentifer modulePath packages =
    case packages of
        ( candidateVersionIdentifer, packageModules ) :: rest ->
            if candidateVersionIdentifer == packageVersionIdentifer then
                parsedModuleInPackage modulePath packageModules

            else
                parsedModuleInPackages packageVersionIdentifer modulePath rest

        [] ->
            Nothing


parsedModuleInPackage :
    List String
    -> List ( List String, ( List String, ParsedModuleCache ) )
    -> Maybe ParsedModuleCache
parsedModuleInPackage modulePath packageModules =
    case packageModules of
        ( _, ( candidateModulePath, parsedModule ) ) :: rest ->
            if candidateModulePath == modulePath then
                Just parsedModule

            else
                parsedModuleInPackage modulePath rest

        [] ->
            Nothing


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
                findReferenceAtPath cursorPath parsedModule.references
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


findReferenceAtPath :
    Path
    -> List LanguageServiceAnalysis.ReferenceOccurrence
    -> Maybe LanguageServiceAnalysis.ReferenceOccurrence
findReferenceAtPath cursorPath references =
    case references of
        reference :: rest ->
            if reference.path == cursorPath then
                Just reference

            else
                findReferenceAtPath cursorPath rest

        [] ->
            Nothing


moduleTargetAtImportPath :
    ParsedModuleCache
    -> Path
    -> List ImportedModule
    -> Maybe ( Range, ResolvedTarget )
moduleTargetAtImportPath parsedModule cursorPath importedModules =
    case importedModules of
        importedModule :: rest ->
            if List.member cursorPath importedModule.moduleNamePaths then
                case rangeAtPathInModule parsedModule cursorPath SelectWhole of
                    Nothing ->
                        moduleTargetAtImportPath parsedModule cursorPath rest

                    Just range ->
                        Just
                            ( range
                            , ResolvedModuleTarget
                                importedModule.fileLocation
                                importedModule.parsedModule
                            )

            else
                moduleTargetAtImportPath parsedModule cursorPath rest

        [] ->
            Nothing


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
            findImportedModuleByImportedName reference.moduleName importedModules
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


findImportedModuleByImportedName : List String -> List ImportedModule -> Maybe ImportedModule
findImportedModuleByImportedName importedName importedModules =
    case importedModules of
        importedModule :: rest ->
            if importedModule.importedName == importedName then
                Just importedModule

            else
                findImportedModuleByImportedName importedName rest

        [] ->
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
    { localItems =
        List.concat
            [ ownDeclarationsForContext True parsedModule fileLocation parsedModule.analysis.declarations
            , exposedImportedDeclarationsForContext importedModules
            , implicitImportsForContext implicitTopLevelImports
            , ownDeclarationsForContext False parsedModule fileLocation parsedModule.analysis.declarations
            ]
    , importedModules =
        importedModulesForContext importedModules
    }


ownDeclarationsForContext :
    Bool
    -> ParsedModuleCache
    -> LanguageServiceInterface.FileLocation
    -> List LanguageServiceAnalysis.DeclarationOccurrence
    -> List ( String, ( LanguageServiceAnalysis.DeclarationScope, ResolvedDeclaration ) )
ownDeclarationsForContext topLevel parsedModule fileLocation occurrences =
    case occurrences of
        occurrence :: rest ->
            if declarationIsTopLevel occurrence == topLevel then
                ( occurrence.name
                , ( occurrence.scope
                  , { fileLocation = fileLocation
                    , parsedModule = parsedModule
                    , occurrence = occurrence
                    }
                  )
                )
                    :: ownDeclarationsForContext topLevel parsedModule fileLocation rest

            else
                ownDeclarationsForContext topLevel parsedModule fileLocation rest

        [] ->
            []


exposedImportedDeclarationsForContext :
    List ImportedModule
    -> List ( String, ( LanguageServiceAnalysis.DeclarationScope, ResolvedDeclaration ) )
exposedImportedDeclarationsForContext importedModules =
    case importedModules of
        importedModule :: rest ->
            exposedDeclarationsForContext importedModule
                ++ exposedImportedDeclarationsForContext rest

        [] ->
            []


exposedDeclarationsForContext :
    ImportedModule
    -> List ( String, ( LanguageServiceAnalysis.DeclarationScope, ResolvedDeclaration ) )
exposedDeclarationsForContext importedModule =
    case importedModule.exposingList of
        Nothing ->
            []

        Just exposingList ->
            exposedDeclarationsForContextHelp
                importedModule
                (declarationsExposedByImport exposingList importedModule.parsedModule)


exposedDeclarationsForContextHelp :
    ImportedModule
    -> List LanguageServiceAnalysis.DeclarationOccurrence
    -> List ( String, ( LanguageServiceAnalysis.DeclarationScope, ResolvedDeclaration ) )
exposedDeclarationsForContextHelp importedModule occurrences =
    case occurrences of
        occurrence :: rest ->
            ( occurrence.name
            , ( LanguageServiceAnalysis.TopLevelScope
              , { fileLocation = importedModule.fileLocation
                , parsedModule = importedModule.parsedModule
                , occurrence = occurrence
                }
              )
            )
                :: exposedDeclarationsForContextHelp importedModule rest

        [] ->
            []


implicitImportsForContext :
    List ResolvedDeclaration
    -> List ( String, ( LanguageServiceAnalysis.DeclarationScope, ResolvedDeclaration ) )
implicitImportsForContext resolvedDeclarations =
    case resolvedDeclarations of
        resolved :: rest ->
            ( resolved.occurrence.name
            , ( LanguageServiceAnalysis.TopLevelScope, resolved )
            )
                :: implicitImportsForContext rest

        [] ->
            []


importedModulesForContext : List ImportedModule -> List ( List String, ImportedModule )
importedModulesForContext importedModules =
    case importedModules of
        importedModule :: rest ->
            ( importedModule.importedName, importedModule )
                :: importedModulesForContext rest

        [] ->
            []


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
            declarationsMatchingExposings topLevelExposings exposedDeclarations


declarationsMatchingExposings :
    List ElmSyntax.Abstract.Exposing.TopLevelExpose
    -> List LanguageServiceAnalysis.DeclarationOccurrence
    -> List LanguageServiceAnalysis.DeclarationOccurrence
declarationsMatchingExposings topLevelExposings exposedDeclarations =
    case topLevelExposings of
        topLevelExpose :: rest ->
            declarationsNamed
                (LanguageServiceAnalysis.nameOfTopLevelExpose topLevelExpose)
                exposedDeclarations
                ++ declarationsMatchingExposings rest exposedDeclarations

        [] ->
            []


declarationsNamed :
    String
    -> List LanguageServiceAnalysis.DeclarationOccurrence
    -> List LanguageServiceAnalysis.DeclarationOccurrence
declarationsNamed exposedName declarations =
    case declarations of
        occurrence :: rest ->
            if occurrence.name == exposedName then
                occurrence :: declarationsNamed exposedName rest

            else
                declarationsNamed exposedName rest

        [] ->
            []


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
                resolveImportedDeclaration
                    reference.name
                    importedModule
                    importedModule.parsedModule.analysis.declarations


resolveImportedDeclaration :
    String
    -> ImportedModule
    -> List LanguageServiceAnalysis.DeclarationOccurrence
    -> Maybe ResolvedDeclaration
resolveImportedDeclaration name importedModule declarations =
    case declarations of
        occurrence :: rest ->
            if occurrence.name == name && occurrence.isExposed && declarationIsTopLevel occurrence then
                Just
                    { fileLocation = importedModule.fileLocation
                    , parsedModule = importedModule.parsedModule
                    , occurrence = occurrence
                    }

            else
                resolveImportedDeclaration name importedModule rest

        [] ->
            Nothing



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
                            |> monacoCompletionItemsFromCompletionItems


monacoCompletionItemsFromCompletionItems :
    List CompletionItem
    -> List Frontend.MonacoEditor.MonacoCompletionItem
monacoCompletionItemsFromCompletionItems completionItems =
    case completionItems of
        completionItem :: rest ->
            monacoCompletionItemFromCompletionItem completionItem
                :: monacoCompletionItemsFromCompletionItems rest

        [] ->
            []


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
        |> locationsFromDeclarationLocations


locationsFromDeclarationLocations :
    List (LocationInFile DeclarationRange)
    -> List LanguageServiceInterface.LocationInFile
locationsFromDeclarationLocations locations =
    case locations of
        (LocationInFile fileLocation (DeclarationRange (Range ( startRow, startColumn ) ( endRow, endColumn )) _)) :: rest ->
            { fileLocation = fileLocation
            , range =
                { startLineNumber = startRow
                , startColumn = startColumn
                , endLineNumber = endRow
                , endColumn = endColumn
                }
            }
                :: locationsFromDeclarationLocations rest

        [] ->
            []


provideDefinitionInternal :
    LanguageServiceInterface.ProvideDefinitionRequestStruct
    -> LanguageServiceState
    -> List (LocationInFile DeclarationRange)
provideDefinitionInternal request languageServiceState =
    hoverItemsAtLocation
        request.fileLocation
        ( request.positionLineNumber, request.positionColumn )
        languageServiceState
        |> declarationLocationsFromHoverItems


declarationLocationsFromHoverItems :
    List ( Range, LocationInFile DeclarationRange, String )
    -> List (LocationInFile DeclarationRange)
declarationLocationsFromHoverItems hoverItems =
    case hoverItems of
        ( _, locationInFile, _ ) :: rest ->
            locationInFile :: declarationLocationsFromHoverItems rest

        [] ->
            []


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
                    documentSymbolsFromOccurrences
                        parsedFileLastSuccess
                        topLevelOccurrences
                        (nonChoiceTypeTagOccurrences topLevelOccurrences)


nonChoiceTypeTagOccurrences :
    List LanguageServiceAnalysis.DeclarationOccurrence
    -> List LanguageServiceAnalysis.DeclarationOccurrence
nonChoiceTypeTagOccurrences occurrences =
    case occurrences of
        occurrence :: rest ->
            if occurrence.kind /= LanguageServiceAnalysis.ChoiceTypeTagDeclarationKind then
                occurrence :: nonChoiceTypeTagOccurrences rest

            else
                nonChoiceTypeTagOccurrences rest

        [] ->
            []


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
                            documentSymbolsFromOccurrences
                                parsedModule
                                allOccurrences
                                (choiceTypeTagChildren occurrence.declarationPath allOccurrences)

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


choiceTypeTagChildren :
    Path
    -> List LanguageServiceAnalysis.DeclarationOccurrence
    -> List LanguageServiceAnalysis.DeclarationOccurrence
choiceTypeTagChildren declarationPath occurrences =
    case occurrences of
        candidate :: rest ->
            if
                candidate.kind == LanguageServiceAnalysis.ChoiceTypeTagDeclarationKind
                    && ElmSyntax.Path.isPrefixOf declarationPath candidate.declarationPath
            then
                candidate :: choiceTypeTagChildren declarationPath rest

            else
                choiceTypeTagChildren declarationPath rest

        [] ->
            []


documentSymbolsFromOccurrences :
    ParsedModuleCache
    -> List LanguageServiceAnalysis.DeclarationOccurrence
    -> List LanguageServiceAnalysis.DeclarationOccurrence
    -> List LanguageServiceInterface.DocumentSymbol
documentSymbolsFromOccurrences parsedModule allOccurrences occurrences =
    documentSymbolsFromOccurrencesHelp parsedModule allOccurrences occurrences []


documentSymbolsFromOccurrencesHelp :
    ParsedModuleCache
    -> List LanguageServiceAnalysis.DeclarationOccurrence
    -> List LanguageServiceAnalysis.DeclarationOccurrence
    -> List LanguageServiceInterface.DocumentSymbol
    -> List LanguageServiceInterface.DocumentSymbol
documentSymbolsFromOccurrencesHelp parsedModule allOccurrences occurrences documentSymbolsReversed =
    case occurrences of
        [] ->
            List.reverse documentSymbolsReversed

        occurrence :: remainingOccurrences ->
            case documentSymbolFromOccurrence parsedModule allOccurrences occurrence of
                Nothing ->
                    documentSymbolsFromOccurrencesHelp
                        parsedModule
                        allOccurrences
                        remainingOccurrences
                        documentSymbolsReversed

                Just documentSymbol ->
                    documentSymbolsFromOccurrencesHelp
                        parsedModule
                        allOccurrences
                        remainingOccurrences
                        (documentSymbol :: documentSymbolsReversed)


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
            locationsFromReferenceGroups references


locationsFromReferenceGroups :
    List ( LanguageServiceInterface.FileLocation, List Range )
    -> List LanguageServiceInterface.LocationInFile
locationsFromReferenceGroups references =
    case references of
        ( fileLocation, ranges ) :: rest ->
            locationsFromRanges fileLocation ranges
                ++ locationsFromReferenceGroups rest

        [] ->
            []


locationsFromRanges :
    LanguageServiceInterface.FileLocation
    -> List Range
    -> List LanguageServiceInterface.LocationInFile
locationsFromRanges fileLocation ranges =
    case ranges of
        (Range ( startRow, startColumn ) ( endRow, endColumn )) :: rest ->
            { fileLocation = fileLocation
            , range =
                { startLineNumber = startRow
                , startColumn = startColumn
                , endLineNumber = endRow
                , endColumn = endColumn
                }
            }
                :: locationsFromRanges fileLocation rest

        [] ->
            []


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
    declarationTargetAtPath
        parsedModule
        fileLocation
        location
        cursorPath
        (topLevelDeclarations parsedModule)


declarationTargetAtPath :
    ParsedModuleCache
    -> LanguageServiceInterface.FileLocation
    -> ( Int, Int )
    -> Path
    -> List LanguageServiceAnalysis.DeclarationOccurrence
    -> Maybe ResolvedTarget
declarationTargetAtPath parsedModule fileLocation location cursorPath occurrences =
    case occurrences of
        occurrence :: rest ->
            if List.member cursorPath occurrence.namePaths then
                case rangeAtPathInModule parsedModule cursorPath SelectName of
                    Nothing ->
                        declarationTargetAtPath parsedModule fileLocation location cursorPath rest

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
                            declarationTargetAtPath parsedModule fileLocation location cursorPath rest

            else
                declarationTargetAtPath parsedModule fileLocation location cursorPath rest

        [] ->
            Nothing


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
                [ workspaceParsedModules (Dict.toList languageServiceState.documentCache)
                , coreParsedModules languageServiceState.coreModulesCache
                , packageParsedModules languageServiceState.elmPackages
                ]

        -- Implicit top-level imports do not depend on the module being scanned,
        -- so compute them once instead of once per module.
        implicitTopLevelImports : List ResolvedDeclaration
        implicitTopLevelImports =
            commonImplicitTopLevelImports languageServiceState
    in
    findReferencesInModules
        target
        implicitTopLevelImports
        languageServiceState
        allParsedModules


workspaceParsedModules :
    List ( String, LanguageServiceStateFileTreeNodeBlob )
    -> List ( LanguageServiceInterface.FileLocation, ParsedModuleCache )
workspaceParsedModules entries =
    case entries of
        ( filePath, blob ) :: rest ->
            case blob.parsedFileLastSuccess of
                Nothing ->
                    workspaceParsedModules rest

                Just parsedModule ->
                    ( LanguageServiceInterface.WorkspaceFileLocation filePath, parsedModule )
                        :: workspaceParsedModules rest

        [] ->
            []


coreParsedModules :
    List ElmCoreModule
    -> List ( LanguageServiceInterface.FileLocation, ParsedModuleCache )
coreParsedModules coreModules =
    case coreModules of
        coreModule :: rest ->
            ( LanguageServiceInterface.WorkspaceFileLocation coreModule.parseResult.fileUri
            , coreModule.parseResult
            )
                :: coreParsedModules rest

        [] ->
            []


packageParsedModules :
    List
        ( LanguageServiceInterface.ElmPackageVersionIdentifer
        , List ( List String, ( List String, ParsedModuleCache ) )
        )
    -> List ( LanguageServiceInterface.FileLocation, ParsedModuleCache )
packageParsedModules packages =
    case packages of
        ( packageVersionIdentifer, packageModules ) :: rest ->
            parsedModulesFromPackage packageVersionIdentifer packageModules
                ++ packageParsedModules rest

        [] ->
            []


parsedModulesFromPackage :
    LanguageServiceInterface.ElmPackageVersionIdentifer
    -> List ( List String, ( List String, ParsedModuleCache ) )
    -> List ( LanguageServiceInterface.FileLocation, ParsedModuleCache )
parsedModulesFromPackage packageVersionIdentifer packageModules =
    case packageModules of
        ( _, ( modulePath, parsedModule ) ) :: rest ->
            ( LanguageServiceInterface.ElmPackageFileLocation packageVersionIdentifer modulePath
            , parsedModule
            )
                :: parsedModulesFromPackage packageVersionIdentifer rest

        [] ->
            []


findReferencesInModules :
    ResolvedTarget
    -> List ResolvedDeclaration
    -> LanguageServiceState
    -> List ( LanguageServiceInterface.FileLocation, ParsedModuleCache )
    -> List ( LanguageServiceInterface.FileLocation, List Range )
findReferencesInModules target implicitTopLevelImports languageServiceState parsedModules =
    case parsedModules of
        ( fileLocation, parsedModule ) :: rest ->
            let
                ranges =
                    referenceRangesInModuleResolvingTo
                        target
                        implicitTopLevelImports
                        fileLocation
                        parsedModule
                        languageServiceState
            in
            if ranges == [] then
                findReferencesInModules target implicitTopLevelImports languageServiceState rest

            else
                ( fileLocation, ranges )
                    :: findReferencesInModules target implicitTopLevelImports languageServiceState rest

        [] ->
            []


resolveReferenceRange :
    ModuleResolutionContext
    -> ( LanguageServiceInterface.FileLocation, Path )
    -> ParsedModuleCache
    -> LanguageServiceAnalysis.ReferenceOccurrence
    -> Maybe Range
resolveReferenceRange context identity parsedModule reference =
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

        nameReferenceRanges : List Range
        nameReferenceRanges =
            referenceRangesByName context identity parsedModule maybeTargetName parsedModule.references

        -- References to imported module names, relevant when the target is a
        -- module declaration.
        importNameReferenceRanges : List Range
        importNameReferenceRanges =
            importNameRanges identity parsedModule importedModules
    in
    List.concat
        [ importNameReferenceRanges
        , nameReferenceRanges
        ]


referenceRangesByName :
    ModuleResolutionContext
    -> ( LanguageServiceInterface.FileLocation, Path )
    -> ParsedModuleCache
    -> Maybe String
    -> List LanguageServiceAnalysis.ReferenceOccurrence
    -> List Range
referenceRangesByName context identity parsedModule maybeTargetName references =
    case references of
        reference :: rest ->
            let
                remaining =
                    referenceRangesByName context identity parsedModule maybeTargetName rest
            in
            if Maybe.withDefault reference.name maybeTargetName /= reference.name then
                remaining

            else
                case resolveReferenceRange context identity parsedModule reference of
                    Nothing ->
                        remaining

                    Just range ->
                        range :: remaining

        [] ->
            []


importNameRanges :
    ( LanguageServiceInterface.FileLocation, Path )
    -> ParsedModuleCache
    -> List ImportedModule
    -> List Range
importNameRanges identity parsedModule importedModules =
    case importedModules of
        importedModule :: rest ->
            if ( importedModule.fileLocation, [ StepModuleDefinition ] ) == identity then
                rangesAtWholePaths parsedModule importedModule.moduleNamePaths
                    ++ importNameRanges identity parsedModule rest

            else
                importNameRanges identity parsedModule rest

        [] ->
            []


rangesAtWholePaths : ParsedModuleCache -> List Path -> List Range
rangesAtWholePaths parsedModule paths =
    case paths of
        path :: rest ->
            case rangeAtPathInModule parsedModule path SelectWhole of
                Nothing ->
                    rangesAtWholePaths parsedModule rest

                Just range ->
                    range :: rangesAtWholePaths parsedModule rest

        [] ->
            []


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
                    textEditsForRanges newName declNamesRanges

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
                            textEditsForRanges newName ranges

                otherFilesReferencesEdits : List LanguageServiceInterface.TextDocumentEdit
                otherFilesReferencesEdits =
                    textDocumentEditsForReferences
                        declFilePath
                        newName
                        referencesGroupedByFilePath

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


textEditsForRanges : String -> List Range -> List LanguageServiceInterface.TextEdit
textEditsForRanges newName ranges =
    case ranges of
        range :: rest ->
            { range = monacoRangeFromRange range
            , newText = newName
            }
                :: textEditsForRanges newName rest

        [] ->
            []


textDocumentEditsForReferences :
    String
    -> String
    -> List ( LanguageServiceInterface.FileLocation, List Range )
    -> List LanguageServiceInterface.TextDocumentEdit
textDocumentEditsForReferences declarationFilePath newName references =
    case references of
        ( fileLocation, ranges ) :: rest ->
            case fileLocation of
                LanguageServiceInterface.WorkspaceFileLocation filePath ->
                    if filePath == declarationFilePath then
                        textDocumentEditsForReferences declarationFilePath newName rest

                    else
                        { filePath = filePath
                        , edits = textEditsForRanges newName ranges
                        }
                            :: textDocumentEditsForReferences declarationFilePath newName rest

                LanguageServiceInterface.ElmPackageFileLocation _ _ ->
                    textDocumentEditsForReferences declarationFilePath newName rest

        [] ->
            []


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
            stringSplitAtCompletionSeparators lineUntilPosition

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
                |> modulesNotNamed moduleNamesToNotSuggestForImport
                |> sortModulesByName

        importedModules : List ImportedModule
        importedModules =
            importedModulesFromModule fileOpenedInEditor languageServiceState

        fromLocals : List CompletionItem
        fromLocals =
            completionItemsForDeclarations
                fileOpenedInEditor
                (localDeclarationsAtLocation
                    fileOpenedInEditor
                    ( request.cursorLineNumber, String.length lineUntilPosition )
                    fileOpenedInEditor.analysis.declarations
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
                    findImportedModuleByImportedName completionPrefix importedModules
                of
                    Nothing ->
                        []

                    Just referencedModule ->
                        completionItemsForDeclarations
                            referencedModule.parsedModule
                            (exposedTopLevelDeclarations referencedModule.parsedModule)

        importedModulesAfterPrefix : List ( List String, ImportedModule )
        importedModulesAfterPrefix =
            importedModulesMatchingPrefix completionPrefix importedModules

        fromImports : List CompletionItem
        fromImports =
            completionItemsFromImportedModules importedModulesAfterPrefix
    in
    case lineUntilPositionWords of
        "import" :: _ ->
            moduleCompletionItemsFromModules
                { importedModuleNameRestAfterPrefix = Nothing, importedName = Nothing }
                modulesToSuggestForImport

        _ ->
            if completionPrefixIsNamespace then
                List.concat
                    [ fromImports
                    , sortCompletionItemsByLabel localDeclarationsAfterPrefix
                    ]

            else
                []


completionItemsForResolvedDeclarations : List ResolvedDeclaration -> List CompletionItem
completionItemsForResolvedDeclarations resolvedDeclarations =
    case resolvedDeclarations of
        resolved :: rest ->
            completionItemForDeclaration
                (String.lines resolved.parsedModule.text)
                resolved.parsedModule
                resolved.occurrence
                :: completionItemsForResolvedDeclarations rest

        [] ->
            []


modulesNotNamed : List (List String) -> List ParsedModuleCache -> List ParsedModuleCache
modulesNotNamed excludedNames modules =
    case modules of
        availableModule :: rest ->
            if List.member availableModule.analysis.moduleName excludedNames then
                modulesNotNamed excludedNames rest

            else
                availableModule :: modulesNotNamed excludedNames rest

        [] ->
            []


sortModulesByName : List ParsedModuleCache -> List ParsedModuleCache
sortModulesByName modules =
    case modules of
        availableModule :: rest ->
            insertModuleByName availableModule (sortModulesByName rest)

        [] ->
            []


insertModuleByName : ParsedModuleCache -> List ParsedModuleCache -> List ParsedModuleCache
insertModuleByName availableModule sortedModules =
    case sortedModules of
        next :: rest ->
            if
                String.join "." availableModule.analysis.moduleName
                    <= String.join "." next.analysis.moduleName
            then
                availableModule :: sortedModules

            else
                next :: insertModuleByName availableModule rest

        [] ->
            [ availableModule ]


localDeclarationsAtLocation :
    ParsedModuleCache
    -> ( Int, Int )
    -> List LanguageServiceAnalysis.DeclarationOccurrence
    -> List LanguageServiceAnalysis.DeclarationOccurrence
localDeclarationsAtLocation parsedModule location occurrences =
    case occurrences of
        occurrence :: rest ->
            case occurrence.scope of
                LanguageServiceAnalysis.TopLevelScope ->
                    localDeclarationsAtLocation parsedModule location rest

                LanguageServiceAnalysis.LocalScope scopePath ->
                    case rangeAtPathInModule parsedModule scopePath SelectWhole of
                        Nothing ->
                            localDeclarationsAtLocation parsedModule location rest

                        Just scopeRange ->
                            if rangeContainsLocation location scopeRange then
                                occurrence :: localDeclarationsAtLocation parsedModule location rest

                            else
                                localDeclarationsAtLocation parsedModule location rest

        [] ->
            []


importedModulesMatchingPrefix :
    List String
    -> List ImportedModule
    -> List ( List String, ImportedModule )
importedModulesMatchingPrefix completionPrefix importedModules =
    case importedModules of
        importedModule :: rest ->
            if List.take (List.length completionPrefix) importedModule.importedName == completionPrefix then
                case List.drop (List.length completionPrefix) importedModule.importedName of
                    [] ->
                        importedModulesMatchingPrefix completionPrefix rest

                    restAfterPrefix ->
                        ( restAfterPrefix, importedModule )
                            :: importedModulesMatchingPrefix completionPrefix rest

            else
                importedModulesMatchingPrefix completionPrefix rest

        [] ->
            []


completionItemsFromImportedModules :
    List ( List String, ImportedModule )
    -> List CompletionItem
completionItemsFromImportedModules importedModules =
    case importedModules of
        ( importedModuleNameRestAfterPrefix, importedModule ) :: rest ->
            moduleCompletionItemFromModule
                { importedName = Just importedModule.importedName
                , importedModuleNameRestAfterPrefix = Just importedModuleNameRestAfterPrefix
                }
                importedModule.parsedModule
                :: completionItemsFromImportedModules rest

        [] ->
            []


sortCompletionItemsByLabel : List CompletionItem -> List CompletionItem
sortCompletionItemsByLabel completionItems =
    case completionItems of
        completionItem :: rest ->
            insertCompletionItemByLabel completionItem (sortCompletionItemsByLabel rest)

        [] ->
            []


insertCompletionItemByLabel : CompletionItem -> List CompletionItem -> List CompletionItem
insertCompletionItemByLabel ((CompletionItem label _ _ _) as completionItem) sortedItems =
    case sortedItems of
        ((CompletionItem nextLabel _ _ _) as next) :: rest ->
            if label <= nextLabel then
                completionItem :: sortedItems

            else
                next :: insertCompletionItemByLabel completionItem rest

        [] ->
            [ completionItem ]


importedModulesFromModule :
    ParsedModuleCache
    -> LanguageServiceState
    -> List ImportedModule
importedModulesFromModule parsedModule languageServiceState =
    let
        implicitlyImportedModulesOld : List ImportedModule
        implicitlyImportedModulesOld =
            oldImplicitImportedModules languageServiceState.coreModulesCache

        implicitlyImportedModules : List ImportedModule
        implicitlyImportedModules =
            implicitImportedModulesFromPackages languageServiceState.elmPackages

        explicitlyImportedModules : List ImportedModule
        explicitlyImportedModules =
            explicitImportedModules languageServiceState parsedModule.analysis.imports
    in
    List.concat
        [ implicitlyImportedModules
        , implicitlyImportedModulesOld
        , explicitlyImportedModules
        ]


oldImplicitImportedModules : List ElmCoreModule -> List ImportedModule
oldImplicitImportedModules coreModules =
    case coreModules of
        coreModule :: rest ->
            if coreModule.implicitImport then
                let
                    canonicalName =
                        coreModule.parseResult.analysis.moduleName
                in
                { fileLocation =
                    LanguageServiceInterface.WorkspaceFileLocation coreModule.parseResult.fileUri
                , canonicalName = canonicalName
                , importedName = canonicalName
                , exposingList = Nothing
                , parsedModule = coreModule.parseResult
                , moduleNamePaths = []
                }
                    :: oldImplicitImportedModules rest

            else
                oldImplicitImportedModules rest

        [] ->
            []


implicitImportedModulesFromPackages :
    List
        ( LanguageServiceInterface.ElmPackageVersionIdentifer
        , List ( List String, ( List String, ParsedModuleCache ) )
        )
    -> List ImportedModule
implicitImportedModulesFromPackages packages =
    case packages of
        ( packageVersionIdentifer, packageModules ) :: rest ->
            (case packageVersionIdentifer of
                LanguageServiceInterface.ElmPackageVersion019Identifer "elm/core" _ ->
                    implicitImportedModulesFromPackage packageVersionIdentifer packageModules

                _ ->
                    []
            )
                ++ implicitImportedModulesFromPackages rest

        [] ->
            []


implicitImportedModulesFromPackage :
    LanguageServiceInterface.ElmPackageVersionIdentifer
    -> List ( List String, ( List String, ParsedModuleCache ) )
    -> List ImportedModule
implicitImportedModulesFromPackage packageVersionIdentifer packageModules =
    case packageModules of
        ( moduleName, ( modulePath, packageModule ) ) :: rest ->
            if elmCoreModuleIsImplicitlyImported moduleName then
                { fileLocation =
                    LanguageServiceInterface.ElmPackageFileLocation packageVersionIdentifer modulePath
                , canonicalName = moduleName
                , importedName = moduleName
                , exposingList = Nothing
                , parsedModule = packageModule
                , moduleNamePaths = []
                }
                    :: implicitImportedModulesFromPackage packageVersionIdentifer rest

            else
                implicitImportedModulesFromPackage packageVersionIdentifer rest

        [] ->
            []


explicitImportedModules :
    LanguageServiceState
    -> List LanguageServiceAnalysis.ImportOccurrence
    -> List ImportedModule
explicitImportedModules languageServiceState imports =
    case imports of
        importOccurrence :: rest ->
            case moduleByCanonicalName importOccurrence.canonicalName languageServiceState of
                Nothing ->
                    explicitImportedModules languageServiceState rest

                Just ( moduleFileLocation, importedParsedModule ) ->
                    { fileLocation = moduleFileLocation
                    , canonicalName = importOccurrence.canonicalName
                    , importedName = importOccurrence.importedName
                    , exposingList = importOccurrence.exposingList
                    , parsedModule = importedParsedModule
                    , moduleNamePaths = [ importOccurrence.moduleNamePath ]
                    }
                        :: explicitImportedModules languageServiceState rest

        [] ->
            []


moduleByCanonicalName :
    List String
    -> LanguageServiceState
    -> Maybe ( LanguageServiceInterface.FileLocation, ParsedModuleCache )
moduleByCanonicalName canonicalModuleName languageServiceState =
    case
        findWorkspaceModuleByName
            canonicalModuleName
            (modulesAvailableForImportFromState languageServiceState)
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
    findModuleInPackages moduleName languageServiceState.elmPackages


findWorkspaceModuleByName :
    List String
    -> List ParsedModuleCache
    -> Maybe ( LanguageServiceInterface.FileLocation, ParsedModuleCache )
findWorkspaceModuleByName moduleName modules =
    case modules of
        moduleAvailable :: rest ->
            if moduleAvailable.analysis.moduleName == moduleName then
                Just
                    ( LanguageServiceInterface.WorkspaceFileLocation moduleAvailable.fileUri
                    , moduleAvailable
                    )

            else
                findWorkspaceModuleByName moduleName rest

        [] ->
            Nothing


findModuleInPackages :
    List String
    -> List
        ( LanguageServiceInterface.ElmPackageVersionIdentifer
        , List ( List String, ( List String, ParsedModuleCache ) )
        )
    -> Maybe ( LanguageServiceInterface.FileLocation, ParsedModuleCache )
findModuleInPackages moduleName packages =
    case packages of
        ( packageVersionIdentifer, packageModules ) :: rest ->
            case findModuleInPackage moduleName packageVersionIdentifer packageModules of
                Just found ->
                    Just found

                Nothing ->
                    findModuleInPackages moduleName rest

        [] ->
            Nothing


findModuleInPackage :
    List String
    -> LanguageServiceInterface.ElmPackageVersionIdentifer
    -> List ( List String, ( List String, ParsedModuleCache ) )
    -> Maybe ( LanguageServiceInterface.FileLocation, ParsedModuleCache )
findModuleInPackage moduleName packageVersionIdentifer packageModules =
    case packageModules of
        ( moduleModuleName, ( modulePath, packageModule ) ) :: rest ->
            if moduleModuleName == moduleName then
                Just
                    ( LanguageServiceInterface.ElmPackageFileLocation packageVersionIdentifer modulePath
                    , packageModule
                    )

            else
                findModuleInPackage moduleName packageVersionIdentifer rest

        [] ->
            Nothing


modulesAvailableForImportFromState : LanguageServiceState -> List ParsedModuleCache
modulesAvailableForImportFromState languageServiceState =
    List.concat
        [ parsedModulesFromDocumentCache (Dict.toList languageServiceState.documentCache)
        , parseResultsFromCoreModules languageServiceState.coreModulesCache
        ]


parsedModulesFromDocumentCache :
    List ( String, LanguageServiceStateFileTreeNodeBlob )
    -> List ParsedModuleCache
parsedModulesFromDocumentCache entries =
    case entries of
        ( _, fileCache ) :: rest ->
            case fileCache.parsedFileLastSuccess of
                Nothing ->
                    parsedModulesFromDocumentCache rest

                Just parsedModule ->
                    parsedModule :: parsedModulesFromDocumentCache rest

        [] ->
            []


parseResultsFromCoreModules : List ElmCoreModule -> List ParsedModuleCache
parseResultsFromCoreModules coreModules =
    case coreModules of
        coreModule :: rest ->
            coreModule.parseResult :: parseResultsFromCoreModules rest

        [] ->
            []


importExposingsFromModule :
    ParsedModuleCache
    -> LanguageServiceState
    -> List CompletionItem
importExposingsFromModule fileOpenedInEditor languageServiceState =
    importExposingsFromImports languageServiceState fileOpenedInEditor.analysis.imports


importExposingsFromImports :
    LanguageServiceState
    -> List LanguageServiceAnalysis.ImportOccurrence
    -> List CompletionItem
importExposingsFromImports languageServiceState imports =
    case imports of
        importOccurrence :: rest ->
            let
                remaining =
                    importExposingsFromImports languageServiceState rest
            in
            case importOccurrence.exposingList of
                Nothing ->
                    remaining

                Just exposingList ->
                    case moduleByCanonicalName importOccurrence.canonicalName languageServiceState of
                        Nothing ->
                            remaining

                        Just ( _, importedParsedModule ) ->
                            completionItemsForDeclarations
                                importedParsedModule
                                (declarationsExposedByImport exposingList importedParsedModule)
                                ++ remaining

        [] ->
            []


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
    implicitDeclarationsFromOldCoreModules languageServiceState.coreModulesCache


implicitDeclarationsFromOldCoreModules :
    List ElmCoreModule
    -> List ResolvedDeclaration
implicitDeclarationsFromOldCoreModules coreModules =
    case coreModules of
        coreModule :: rest ->
            implicitDeclarationsFromModule
                (LanguageServiceInterface.WorkspaceFileLocation "elm/core")
                coreModule.parseResult.analysis.moduleName
                coreModule.parseResult
                coreModule.parseResult.analysis.declarations
                ++ implicitDeclarationsFromOldCoreModules rest

        [] ->
            []


commonImplicitTopLevelImportsNew :
    LanguageServiceState
    -> List ResolvedDeclaration
commonImplicitTopLevelImportsNew languageServiceState =
    implicitDeclarationsFromPackages languageServiceState.elmPackages


implicitDeclarationsFromPackages :
    List
        ( LanguageServiceInterface.ElmPackageVersionIdentifer
        , List ( List String, ( List String, ParsedModuleCache ) )
        )
    -> List ResolvedDeclaration
implicitDeclarationsFromPackages packages =
    case packages of
        ( packageVersionIdentifer, packageModules ) :: rest ->
            (case packageVersionIdentifer of
                LanguageServiceInterface.ElmPackageVersion019Identifer "elm/core" _ ->
                    implicitDeclarationsFromPackage packageVersionIdentifer packageModules

                _ ->
                    []
            )
                ++ implicitDeclarationsFromPackages rest

        [] ->
            []


implicitDeclarationsFromPackage :
    LanguageServiceInterface.ElmPackageVersionIdentifer
    -> List ( List String, ( List String, ParsedModuleCache ) )
    -> List ResolvedDeclaration
implicitDeclarationsFromPackage packageVersionIdentifer packageModules =
    case packageModules of
        ( moduleName, ( moduleFilePath, packageModule ) ) :: rest ->
            implicitDeclarationsFromModule
                (LanguageServiceInterface.ElmPackageFileLocation
                    packageVersionIdentifer
                    moduleFilePath
                )
                moduleName
                packageModule
                packageModule.analysis.declarations
                ++ implicitDeclarationsFromPackage packageVersionIdentifer rest

        [] ->
            []


implicitDeclarationsFromModule :
    LanguageServiceInterface.FileLocation
    -> List String
    -> ParsedModuleCache
    -> List LanguageServiceAnalysis.DeclarationOccurrence
    -> List ResolvedDeclaration
implicitDeclarationsFromModule fileLocation moduleName parsedModule declarations =
    case declarations of
        occurrence :: rest ->
            if
                occurrence.isExposed
                    && declarationIsTopLevel occurrence
                    && isItemImplicitlyExposed moduleName occurrence.name
            then
                { fileLocation = fileLocation
                , parsedModule = parsedModule
                , occurrence = occurrence
                }
                    :: implicitDeclarationsFromModule fileLocation moduleName parsedModule rest

            else
                implicitDeclarationsFromModule fileLocation moduleName parsedModule rest

        [] ->
            []


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


moduleCompletionItemsFromModules :
    { importedModuleNameRestAfterPrefix : Maybe (List String), importedName : Maybe (List String) }
    -> List ParsedModuleCache
    -> List CompletionItem
moduleCompletionItemsFromModules config parsedModules =
    moduleCompletionItemsFromModulesHelp config parsedModules []


moduleCompletionItemsFromModulesHelp :
    { importedModuleNameRestAfterPrefix : Maybe (List String), importedName : Maybe (List String) }
    -> List ParsedModuleCache
    -> List CompletionItem
    -> List CompletionItem
moduleCompletionItemsFromModulesHelp config parsedModules completionItemsReversed =
    case parsedModules of
        [] ->
            List.reverse completionItemsReversed

        parsedModule :: remainingParsedModules ->
            moduleCompletionItemsFromModulesHelp
                config
                remainingParsedModules
                (moduleCompletionItemFromModule config parsedModule :: completionItemsReversed)


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
                [ importStartRows concrete.imports
                , declarationStartRows concrete.declarations
                ]

        importsAndDeclarationsStartRow : Int
        importsAndDeclarationsStartRow =
            case importsAndDeclarationsStartRows of
                [] ->
                    0

                first :: rest ->
                    minimumInt first rest

        maybeModuleComment : Maybe (ElmSyntax.Concrete.Node.Node String)
        maybeModuleComment =
            findModuleComment
                moduleDefinitionRange
                importsAndDeclarationsStartRow
                concrete.comments
                Nothing
    in
    case maybeModuleComment of
        Nothing ->
            Nothing

        Just (ElmSyntax.Concrete.Node.Node _ commentText) ->
            Just (removeWrappingFromMultilineComment commentText)


importStartRows : List (ElmSyntax.Concrete.Node.Node ElmSyntax.Concrete.Import.Import) -> List Int
importStartRows imports =
    case imports of
        (ElmSyntax.Concrete.Node.Node range _) :: rest ->
            range.start.row :: importStartRows rest

        [] ->
            []


declarationStartRows : List (ElmSyntax.Concrete.Node.Node ElmSyntax.Concrete.Declaration.Declaration) -> List Int
declarationStartRows declarations =
    case declarations of
        (ElmSyntax.Concrete.Node.Node range _) :: rest ->
            range.start.row :: declarationStartRows rest

        [] ->
            []


minimumInt : Int -> List Int -> Int
minimumInt minimum remaining =
    case remaining of
        value :: rest ->
            minimumInt (min minimum value) rest

        [] ->
            minimum


findModuleComment :
    ElmSyntax.Concrete.Range.Range
    -> Int
    -> List (ElmSyntax.Concrete.Node.Node String)
    -> Maybe (ElmSyntax.Concrete.Node.Node String)
    -> Maybe (ElmSyntax.Concrete.Node.Node String)
findModuleComment moduleDefinitionRange importsAndDeclarationsStartRow comments maybeComment =
    case comments of
        ((ElmSyntax.Concrete.Node.Node commentRange _) as comment) :: rest ->
            let
                nextMaybeComment =
                    case maybeComment of
                        Nothing ->
                            if
                                (commentRange.start.row > moduleDefinitionRange.start.row)
                                    && (commentRange.start.row < importsAndDeclarationsStartRow)
                            then
                                Just comment

                            else
                                Nothing

                        Just ((ElmSyntax.Concrete.Node.Node prevCommentRange _) as prevComment) ->
                            if
                                (commentRange.start.row > prevCommentRange.end.row)
                                    && (commentRange.start.row < importsAndDeclarationsStartRow)
                            then
                                Just comment

                            else
                                Just prevComment
            in
            findModuleComment moduleDefinitionRange importsAndDeclarationsStartRow rest nextMaybeComment

        [] ->
            maybeComment


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


compileFileCacheEntry :
    LanguageServiceState
    -> ( String, LanguageServiceInterface.FileTreeBlobNode )
    -> LanguageServiceStateFileTreeNodeBlob
compileFileCacheEntry state ( blobPath, fileTreeBlob ) =
    let
        maybePreviousCached : Maybe LanguageServiceStateFileTreeNodeBlob
        maybePreviousCached =
            Dict.get blobPath state.documentCache
    in
    case maybePreviousCached of
        Nothing ->
            buildNewCacheEntry maybePreviousCached blobPath fileTreeBlob

        Just previousCached ->
            if previousCached.sourceBase64 == fileTreeBlob.asBase64 then
                previousCached

            else
                buildNewCacheEntry maybePreviousCached blobPath fileTreeBlob


buildNewCacheEntry :
    Maybe LanguageServiceStateFileTreeNodeBlob
    -> String
    -> LanguageServiceInterface.FileTreeBlobNode
    -> LanguageServiceStateFileTreeNodeBlob
buildNewCacheEntry maybePreviousCached blobPath fileTreeBlob =
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


updateLanguageServiceState : LanguageServiceInterface.FileTreeNode -> LanguageServiceState -> LanguageServiceState
updateLanguageServiceState fileTree state =
    let
        documentCache : Dict.Dict String LanguageServiceStateFileTreeNodeBlob
        documentCache =
            fileTree
                |> FileTree.flatListOfBlobsFromFileTreeNode
                |> compileFileCacheEntries state
                |> Dict.fromList
    in
    { state
        | documentCache = documentCache
    }


compileFileCacheEntries :
    LanguageServiceState
    -> List ( List String, LanguageServiceInterface.FileTreeBlobNode )
    -> List ( String, LanguageServiceStateFileTreeNodeBlob )
compileFileCacheEntries state entries =
    case entries of
        ( filePath, fileContent ) :: rest ->
            let
                filePathFlat =
                    String.join "/" filePath
            in
            ( filePathFlat
            , compileFileCacheEntry state ( filePathFlat, fileContent )
            )
                :: compileFileCacheEntries state rest

        [] ->
            []


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
    locationIsInComments location concrete.comments
        || locationIsInRanges location (documentationRangesInFile concrete)


locationIsInComments :
    ElmSyntax.Concrete.Range.Location
    -> List (ElmSyntax.Concrete.Node.Node String)
    -> Bool
locationIsInComments location comments =
    case comments of
        (ElmSyntax.Concrete.Node.Node commentRange comment) :: rest ->
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
            rangeIntersectsLocation location range || locationIsInComments location rest

        [] ->
            False


locationIsInRanges :
    ElmSyntax.Concrete.Range.Location
    -> List ElmSyntax.Concrete.Range.Range
    -> Bool
locationIsInRanges location ranges =
    case ranges of
        range :: rest ->
            rangeIntersectsLocation location range || locationIsInRanges location rest

        [] ->
            False


documentationRangesInFile : ElmSyntax.Concrete.File.File -> List ElmSyntax.Concrete.Range.Range
documentationRangesInFile concrete =
    documentationRangesInDeclarations concrete 0 concrete.declarations


documentationRangesInDeclarations :
    ElmSyntax.Concrete.File.File
    -> Int
    -> List (ElmSyntax.Concrete.Node.Node ElmSyntax.Concrete.Declaration.Declaration)
    -> List ElmSyntax.Concrete.Range.Range
documentationRangesInDeclarations concrete index declarations =
    case declarations of
        _ :: rest ->
            case
                ElmSyntax.Concrete.SourceLookup.rangeAtPath
                    [ StepDeclaration index ]
                    SelectDocumentation
                    concrete
            of
                Nothing ->
                    documentationRangesInDeclarations concrete (index + 1) rest

                Just range ->
                    range :: documentationRangesInDeclarations concrete (index + 1) rest

        [] ->
            []


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


stringSplitAtCompletionSeparators : String -> List String
stringSplitAtCompletionSeparators string =
    stringsFromCharLists
        (listCharSplitAtCompletionSeparators (String.toList string))


stringsFromCharLists : List (List Char) -> List String
stringsFromCharLists charLists =
    case charLists of
        chars :: rest ->
            String.fromList chars :: stringsFromCharLists rest

        [] ->
            []


listCharSplitAtCompletionSeparators : List Char -> List (List Char)
listCharSplitAtCompletionSeparators chars =
    case listCharSplitAtCompletionSeparatorsHelp chars [] [] of
        ( completed, current ) ->
            List.reverse (List.reverse current :: completed)


listCharSplitAtCompletionSeparatorsHelp :
    List Char
    -> List (List Char)
    -> List Char
    -> ( List (List Char), List Char )
listCharSplitAtCompletionSeparatorsHelp chars completed current =
    case chars of
        char :: rest ->
            if not (charIsAllowedInDeclarationName char || char == '.') then
                listCharSplitAtCompletionSeparatorsHelp rest (List.reverse current :: completed) []

            else
                listCharSplitAtCompletionSeparatorsHelp rest completed (char :: current)

        [] ->
            ( completed, current )
