module LanguageService.WorkspaceTests exposing (tests)

import Dict
import Expect
import LanguageService
import LanguageService.TestHelpers as Helpers
import LanguageServiceInterface exposing (Request(..), Response(..))
import Test exposing (Test, describe, test)


tests : Test
tests =
    describe "Workspace and request lifecycle"
        [ test "Initial state has no documents or packages" <|
            \_ ->
                Expect.equal ( [], [], [] )
                    ( Dict.keys Helpers.emptyState.documentCache
                    , Helpers.emptyState.coreModulesCache
                    , Helpers.emptyState.elmPackages
                    )
        , test "Adding a document acknowledges it and enables symbols" <|
            \_ ->
                let
                    ( response, state ) =
                        LanguageService.handleRequestInCurrentWorkspace
                            (AddWorkspaceFileRequest "src/Main.elm" (Helpers.blob validModule))
                            Helpers.emptyState
                in
                Expect.equal
                    ( Ok WorkspaceSummaryResponse, [ "value" ] )
                    ( response, symbolNames state )
        , test "Replacing a document replaces its declarations" <|
            \_ ->
                validState
                    |> Helpers.addFile "src/Main.elm" "module Main exposing (..)\n\nreplacement = 2\n"
                    |> symbolNames
                    |> Expect.equal [ "replacement" ]
        , test "A temporary syntax error retains the last successful parse" <|
            \_ ->
                validState
                    |> Helpers.addFile "src/Main.elm" "module Main exposing (..)\n\nvalue ="
                    |> symbolNames
                    |> Expect.equal [ "value" ]
        , test "A valid edit after a syntax error refreshes the cached parse" <|
            \_ ->
                validState
                    |> Helpers.addFile "src/Main.elm" "module Main exposing (..)\n\nvalue ="
                    |> Helpers.addFile "src/Main.elm" "module Main exposing (..)\n\nrecovered = 3\n"
                    |> symbolNames
                    |> Expect.equal [ "recovered" ]
        , test "A document that never parsed has no symbols" <|
            \_ ->
                Helpers.emptyState
                    |> Helpers.addFile "src/Main.elm" "not an Elm module"
                    |> symbolNames
                    |> Expect.equal []
        , test "A binary document has no symbols" <|
            \_ ->
                LanguageService.handleRequestInCurrentWorkspace
                    (AddWorkspaceFileRequest "src/Main.elm" { asBase64 = "AA==", asText = Nothing })
                    Helpers.emptyState
                    |> Tuple.second
                    |> symbolNames
                    |> Expect.equal []
        , test "Deleting a document acknowledges it and removes its symbols" <|
            \_ ->
                let
                    ( response, state ) =
                        LanguageService.handleRequestInCurrentWorkspace
                            (DeleteWorkspaceFileRequest "src/Main.elm")
                            validState
                in
                Expect.equal ( Ok WorkspaceSummaryResponse, [] )
                    ( response, symbolNames state )
        , test "Deleting a missing document leaves other documents intact" <|
            \_ ->
                LanguageService.handleRequestInCurrentWorkspace
                    (DeleteWorkspaceFileRequest "missing.elm")
                    validState
                    |> Tuple.second
                    |> symbolNames
                    |> Expect.equal [ "value" ]
        , test "handleRequest synchronizes the workspace before answering" <|
            \_ ->
                LanguageService.handleRequest
                    { workspace = Helpers.workspace [ ( "src/Main.elm", validModule ) ]
                    , request = TextDocumentSymbolRequest "src/Main.elm"
                    }
                    Helpers.emptyState
                    |> Tuple.first
                    |> Expect.equal (Helpers.request (TextDocumentSymbolRequest "src/Main.elm") validState)
        , test "Workspace synchronization removes absent files" <|
            \_ ->
                LanguageService.updateLanguageServiceState (Helpers.workspace []) validState
                    |> symbolNames
                    |> Expect.equal []
        , test "Workspace synchronization notices changed file contents" <|
            \_ ->
                validState
                    |> LanguageService.updateLanguageServiceState
                        (Helpers.workspace [ ( "src/Main.elm", "module Main exposing (..)\n\nchanged = 2\n" ) ])
                    |> symbolNames
                    |> Expect.equal [ "changed" ]
        , test "Synchronizing an unchanged workspace preserves query results" <|
            \_ ->
                validState
                    |> LanguageService.updateLanguageServiceState (Helpers.workspace [ ( "src/Main.elm", validModule ) ])
                    |> symbolNames
                    |> Expect.equal [ "value" ]
        , test "Adding a package ignores binary and malformed modules" <|
            \_ ->
                let
                    ( response, state ) =
                        LanguageService.handleRequestInCurrentWorkspace
                            (AddElmPackageVersionRequest Helpers.packageId
                                [ ( [ "src", "Invalid.elm" ], Helpers.blob "not Elm" )
                                , ( [ "image.png" ], { asBase64 = "AA==", asText = Nothing } )
                                , ( [ "src", "Main.elm" ], Helpers.blob validModule )
                                ]
                            )
                            Helpers.emptyState
                in
                Expect.equal
                    ( Ok WorkspaceSummaryResponse, [ [ [ "Main" ] ] ] )
                    ( response, List.map (Tuple.second >> List.map Tuple.first) state.elmPackages )
        , test "Malformed core modules are skipped without losing valid ones" <|
            \_ ->
                LanguageService.initLanguageServiceState
                    [ { moduleText = "not Elm", implicitImport = True }
                    , { moduleText = validModule, implicitImport = False }
                    ]
                    |> .coreModulesCache
                    |> List.map (\core -> ( core.parseResult.analysis.moduleName, core.implicitImport ))
                    |> Expect.equal [ ( [ "Main" ], False ) ]
        ]


validModule : String
validModule =
    "module Main exposing (..)\n\nvalue = 1\n"


validState : LanguageService.LanguageServiceState
validState =
    Helpers.stateFromFiles [ ( "src/Main.elm", validModule ) ]


symbolNames : LanguageService.LanguageServiceState -> List String
symbolNames state =
    LanguageService.textDocumentSymbol "src/Main.elm" state
        |> List.map (\(LanguageServiceInterface.DocumentSymbol symbol) -> symbol.name)
