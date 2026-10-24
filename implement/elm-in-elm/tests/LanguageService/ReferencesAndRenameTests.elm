module LanguageService.ReferencesAndRenameTests exposing (tests)

import Expect
import LanguageService
import LanguageService.TestHelpers as Helpers
import LanguageServiceInterface exposing (FileLocation(..), Request(..), Response(..))
import Test exposing (Test, describe, test)


tests : Test
tests =
    describe "References and rename"
        [ test "References span files, include exports, and exclude unrelated same-named values" <|
            \_ ->
                Helpers.request (referencesRequest "src/ModuleA.elm" 4 2 False) crossModuleState
                    |> Expect.equal (Ok (TextDocumentReferencesResponse helperReferences))
        , test "includeDeclaration adds exactly the canonical declaration name" <|
            \_ ->
                Helpers.request (referencesRequest "src/ModuleA.elm" 4 2 True) crossModuleState
                    |> Expect.equal
                        (Ok
                            (TextDocumentReferencesResponse
                                (Helpers.location "src/ModuleA.elm" ( 4, 1 ) ( 4, 7 ) :: helperReferences)
                            )
                        )
        , test "References from a qualified usage resolve to the same declaration" <|
            \_ ->
                Helpers.request (referencesRequest "src/ModuleB.elm" 5 12 False) crossModuleState
                    |> Expect.equal (Ok (TextDocumentReferencesResponse helperReferences))
        , test "References from the signature resolve to the same declaration" <|
            \_ ->
                Helpers.request (referencesRequest "src/ModuleA.elm" 3 2 False) crossModuleState
                    |> Expect.equal (Ok (TextDocumentReferencesResponse helperReferences))
        , test "An unused declaration has no references without includeDeclaration" <|
            \_ ->
                Helpers.stateFromFiles [ ( "src/Main.elm", "module Main exposing (..)\n\nunused = 1\n" ) ]
                    |> Helpers.request (referencesRequest "src/Main.elm" 3 2 False)
                    |> Expect.equal (Ok (TextDocumentReferencesResponse []))
        , test "An unused declaration can include its own name" <|
            \_ ->
                Helpers.stateFromFiles [ ( "src/Main.elm", "module Main exposing (..)\n\nunused = 1\n" ) ]
                    |> Helpers.request (referencesRequest "src/Main.elm" 3 2 True)
                    |> Expect.equal
                        (Ok (TextDocumentReferencesResponse [ Helpers.location "src/Main.elm" ( 3, 1 ) ( 3, 7 ) ]))
        , test "Type references include annotations" <|
            \_ ->
                Helpers.stateFromFiles [ ( "src/Main.elm", "module Main exposing (..)\n\ntype alias Config = Int\n\nvalue : Config\nvalue = 1\n" ) ]
                    |> Helpers.request (referencesRequest "src/Main.elm" 3 13 False)
                    |> Expect.equal
                        (Ok (TextDocumentReferencesResponse [ Helpers.location "src/Main.elm" ( 5, 9 ) ( 5, 15 ) ]))
        , test "Constructor references include both expressions and case patterns" <|
            \_ ->
                Helpers.stateFromFiles
                    [ ( "src/Main.elm"
                      , "module Main exposing (..)\n\ntype Mode = Ready | Busy\n\nvalue = Ready\n\ncheck mode =\n    case mode of\n        Ready -> 1\n        Busy -> 0\n"
                      )
                    ]
                    |> Helpers.request (referencesRequest "src/Main.elm" 3 14 False)
                    |> Expect.equal
                        (Ok
                            (TextDocumentReferencesResponse
                                [ Helpers.location "src/Main.elm" ( 5, 9 ) ( 5, 14 )
                                , Helpers.location "src/Main.elm" ( 9, 9 ) ( 9, 14 )
                                ]
                            )
                        )
        , test "Rename updates signature, implementation, export, and qualified usages without changing qualifiers" <|
            \_ ->
                Helpers.request (renameRequest "src/ModuleA.elm" 4 2 "renamed") crossModuleState
                    |> Expect.equal (Ok (TextDocumentRenameResponse helperRenameEdits))
        , test "Rename from a usage edits the declaration and all usages" <|
            \_ ->
                Helpers.request (renameRequest "src/ModuleB.elm" 5 12 "renamed") crossModuleState
                    |> Expect.equal (Ok (TextDocumentRenameResponse helperRenameEdits))
        , test "Renaming an unused value still edits its declaration" <|
            \_ ->
                Helpers.stateFromFiles [ ( "src/Main.elm", "module Main exposing (..)\n\nunused = 1\n" ) ]
                    |> Helpers.request (renameRequest "src/Main.elm" 3 2 "renamed")
                    |> Expect.equal
                        (Ok
                            (TextDocumentRenameResponse
                                [ { filePath = "src/Main.elm"
                                  , edits = [ { range = Helpers.range ( 3, 1 ) ( 3, 7 ), newText = "renamed" } ]
                                  }
                                ]
                            )
                        )
        , test "Package declarations cannot be renamed from a workspace usage" <|
            \_ ->
                Helpers.packageState
                    |> Helpers.addFile "src/Main.elm" "module Main exposing (..)\n\nimport Tools\n\nmain = Tools.answer\n"
                    |> Helpers.request (renameRequest "src/Main.elm" 5 15 "renamed")
                    |> Expect.equal (Ok (TextDocumentRenameResponse []))
        , test "References to a package declaration preserve its package location" <|
            \_ ->
                Helpers.packageState
                    |> Helpers.addFile "src/Main.elm" "module Main exposing (..)\n\nimport Tools\n\nmain = Tools.answer\n"
                    |> Helpers.request (referencesRequest "src/Main.elm" 5 15 True)
                    |> Expect.equal
                        (Ok
                            (TextDocumentReferencesResponse
                                [ { fileLocation = ElmPackageFileLocation Helpers.packageId [ "src", "Tools.elm" ]
                                  , range = Helpers.range ( 5, 1 ) ( 5, 7 )
                                  }
                                , Helpers.location "src/Main.elm" ( 5, 14 ) ( 5, 20 )
                                , { fileLocation = ElmPackageFileLocation Helpers.packageId [ "src", "Tools.elm" ]
                                  , range = Helpers.range ( 1, 24 ) ( 1, 30 )
                                  }
                                ]
                            )
                        )
        , test "Deleted files no longer contribute references" <|
            \_ ->
                LanguageService.handleRequestInCurrentWorkspace (DeleteWorkspaceFileRequest "src/ModuleB.elm") crossModuleState
                    |> Tuple.second
                    |> Helpers.request (referencesRequest "src/ModuleA.elm" 4 2 False)
                    |> Expect.equal
                        (Ok (TextDocumentReferencesResponse [ Helpers.location "src/ModuleA.elm" ( 1, 26 ) ( 1, 32 ) ]))
        , test "Missing files return empty references and rename responses" <|
            \_ ->
                expectNoReferencesOrRename "missing.elm" 1 1 Helpers.emptyState
        , test "Unresolved identifiers cannot be referenced or renamed" <|
            \_ ->
                expectNoReferencesOrRename "src/Main.elm"
                    3
                    10
                    (Helpers.stateFromFiles [ ( "src/Main.elm", "module Main exposing (..)\n\nmain = unknown\n" ) ])
        , test "Comments do not produce references or rename edits" <|
            \_ ->
                expectNoReferencesOrRename "src/Main.elm"
                    4
                    5
                    (Helpers.stateFromFiles [ ( "src/Main.elm", "module Main exposing (..)\n\nhelper = 1\n-- helper\n" ) ])
        ]


referencesRequest : String -> Int -> Int -> Bool -> LanguageServiceInterface.Request
referencesRequest path row column includeDeclaration =
    TextDocumentReferencesRequest
        { fileLocation = WorkspaceFileLocation path
        , positionLineNumber = row
        , positionColumn = column
        , includeDeclaration = includeDeclaration
        }


renameRequest : String -> Int -> Int -> String -> LanguageServiceInterface.Request
renameRequest path row column newName =
    TextDocumentRenameRequest
        { filePath = path
        , positionLineNumber = row
        , positionColumn = column
        , newName = newName
        }


expectNoReferencesOrRename : String -> Int -> Int -> LanguageService.LanguageServiceState -> Expect.Expectation
expectNoReferencesOrRename path row column state =
    Expect.equal
        ( Ok (TextDocumentReferencesResponse []), Ok (TextDocumentRenameResponse []) )
        ( Helpers.request (referencesRequest path row column True) state
        , Helpers.request (renameRequest path row column "renamed") state
        )


crossModuleState : LanguageService.LanguageServiceState
crossModuleState =
    Helpers.stateFromFiles
        [ ( "src/ModuleA.elm", "module ModuleA exposing (helper)\n\nhelper : Int\nhelper = 1\n" )
        , ( "src/ModuleB.elm", "module ModuleB exposing (..)\n\nimport ModuleA as A\n\nvalue = A.helper + A.helper\n" )
        , ( "src/ModuleC.elm", "module ModuleC exposing (..)\n\nhelper = 9\nvalue = helper\n" )
        ]


helperReferences : List LanguageServiceInterface.LocationInFile
helperReferences =
    [ Helpers.location "src/ModuleA.elm" ( 1, 26 ) ( 1, 32 )
    , Helpers.location "src/ModuleB.elm" ( 5, 11 ) ( 5, 17 )
    , Helpers.location "src/ModuleB.elm" ( 5, 22 ) ( 5, 28 )
    ]


helperRenameEdits : LanguageServiceInterface.WorkspaceEdit
helperRenameEdits =
    [ { filePath = "src/ModuleA.elm"
      , edits =
            [ { range = Helpers.range ( 3, 1 ) ( 3, 7 ), newText = "renamed" }
            , { range = Helpers.range ( 4, 1 ) ( 4, 7 ), newText = "renamed" }
            , { range = Helpers.range ( 1, 26 ) ( 1, 32 ), newText = "renamed" }
            ]
      }
    , { filePath = "src/ModuleB.elm"
      , edits =
            [ { range = Helpers.range ( 5, 11 ) ( 5, 17 ), newText = "renamed" }
            , { range = Helpers.range ( 5, 22 ) ( 5, 28 ), newText = "renamed" }
            ]
      }
    ]
