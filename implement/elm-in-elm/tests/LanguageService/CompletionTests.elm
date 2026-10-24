module LanguageService.CompletionTests exposing (tests)

import Expect
import Frontend.MonacoEditor exposing (CompletionItemKind(..))
import LanguageService
import LanguageService.TestHelpers as Helpers
import LanguageServiceInterface exposing (Request(..), Response(..))
import Test exposing (Test, describe, test)


tests : Test
tests =
    describe "Completion items"
        [ test "Local completion includes kind, insert text, and documentation" <|
            \_ ->
                Helpers.atMarker "module Main exposing (..)\n\n{-| The answer. -}\nanswer : Int\nanswer = 42\n\nmain = an<caret>swer\n" <|
                    \text row column ->
                        completions (Helpers.stateFromFiles [ ( "src/Main.elm", text ) ]) row column
                            |> List.filter (\item -> item.label == "answer")
                            |> Expect.equal
                                [ { label = "answer"
                                  , insertText = "answer"
                                  , kind = FunctionCompletionItemKind
                                  , documentation = "    answer : Int\n\nThe answer."
                                  }
                                ]
        , test "Types and constructors have distinct kinds and sorted labels" <|
            \_ ->
                expectItems "module Main exposing (..)\n\ntype Mode = Ready | Busy Int\ntype alias Config = Int\nmain = Re<caret>ady\n"
                    []
                    [ ( "Busy", EnumMemberCompletionItemKind )
                    , ( "Config", StructCompletionItemKind )
                    , ( "Mode", EnumCompletionItemKind )
                    , ( "Ready", EnumMemberCompletionItemKind )
                    , ( "main", FunctionCompletionItemKind )
                    ]
        , test "Qualified completion excludes private declarations" <|
            \_ ->
                expectLabels "module Main exposing (..)\n\nimport Library\n\nmain = Library.an<caret>swer\n"
                    libraryFiles
                    [ "answer" ]
        , test "Qualified completion uses the import alias" <|
            \_ ->
                expectLabels "module Main exposing (..)\n\nimport Library as Lib\n\nmain = Lib.an<caret>swer\n"
                    libraryFiles
                    [ "answer" ]
        , test "Exposed imports are suggested unqualified along with the module" <|
            \_ ->
                expectItems "module Main exposing (..)\n\nimport Library exposing (answer)\n\nmain = an<caret>swer\n"
                    libraryFiles
                    [ ( "Library", ModuleCompletionItemKind )
                    , ( "answer", FunctionCompletionItemKind )
                    , ( "main", FunctionCompletionItemKind )
                    ]
        , test "Function arguments are suggested within their scope" <|
            \_ ->
                expectLabels "module Main exposing (..)\n\nidentity argument = ar<caret>gument\n"
                    []
                    [ "argument", "identity" ]
        , test "Function arguments do not leak into another function" <|
            \_ ->
                expectLabels "module Main exposing (..)\n\nidentity argument = argument\nmain = id<caret>entity 1\n"
                    []
                    [ "identity", "main" ]
        , test "Let bindings are suggested within the let body" <|
            \_ ->
                expectLabels "module Main exposing (..)\n\nmain =\n    let\n        local = 1\n    in\n    lo<caret>cal\n"
                    []
                    [ "local", "main" ]
        , test "Comments suppress completion" <|
            \_ ->
                expectLabels "module Main exposing (..)\n\nanswer = 42\n-- an<caret>swer\n" [] []
        , test "Documentation comments suppress completion" <|
            \_ ->
                expectLabels "module Main exposing (..)\n\n{-| an<caret>swer -}\nanswer = 42\n" [] []
        , test "Record access does not suggest module members" <|
            \_ ->
                expectLabels "module Main exposing (..)\n\nrecord = { field = 1 }\nmain = record.fi<caret>eld\n" [] []
        , test "Unknown namespaces have no completions" <|
            \_ ->
                expectLabels "module Main exposing (..)\n\nmain = Unknown.an<caret>swer\n" libraryFiles []
        , test "Import completion suggests sorted available modules but not the current module" <|
            \_ ->
                expectLabels "module Main exposing (..)\n\nimport Lib<caret>rary\n\nmain = 1\n"
                    (( "src/Alpha.elm", "module Alpha exposing (..)\n\nvalue = 0\n" ) :: libraryFiles)
                    [ "Alpha", "Library" ]
        , test "An incomplete edit uses the previous successful parse for completion" <|
            \_ ->
                Helpers.atMarker "module Main exposing (..)\n\nimport Library\n\nmain = Library.<caret>\n" <|
                    \text row column ->
                        Helpers.stateFromFiles
                            (( "src/Main.elm", "module Main exposing (..)\n\nimport Library\n\nmain = Library.answer\n" ) :: libraryFiles)
                            |> Helpers.addFile "src/Main.elm" text
                            |> (\state -> completions state row column)
                            |> List.map .label
                            |> Expect.equal [ "answer" ]
        , test "Package exports participate in completion" <|
            \_ ->
                Helpers.atMarker "module Main exposing (..)\n\nimport Tools\n\nmain = Tools.an<caret>swer\n" <|
                    \text row column ->
                        Helpers.packageState
                            |> Helpers.addFile "src/Main.elm" text
                            |> (\state -> completions state row column)
                            |> List.map .label
                            |> Expect.equal [ "answer" ]
        , test "Implicit core imports participate in unqualified completion" <|
            \_ ->
                Helpers.atMarker "module Main exposing (..)\n\nmain = id<caret>entity 1\n" <|
                    \text row column ->
                        LanguageService.initLanguageServiceState
                            [ { moduleText = "module Basics exposing (identity)\n\nidentity x = x\n"
                              , implicitImport = True
                              }
                            ]
                            |> Helpers.addFile "src/Main.elm" text
                            |> (\state -> completions state row column)
                            |> List.map .label
                            |> Expect.equal [ "Basics", "identity", "main" ]
        , test "Missing files return an empty completion response" <|
            \_ ->
                Helpers.request
                    (ProvideCompletionItemsRequest (Helpers.completionRequest "missing.elm" 1 1))
                    Helpers.emptyState
                    |> Expect.equal (Ok (ProvideCompletionItemsResponse []))
        , test "Files with no successful parse have no completions" <|
            \_ ->
                Helpers.stateFromFiles [ ( "src/Main.elm", "invalid Elm" ) ]
                    |> (\state -> completions state 1 5)
                    |> Expect.equal []
        ]


completions : LanguageService.LanguageServiceState -> Int -> Int -> List Frontend.MonacoEditor.MonacoCompletionItem
completions state row column =
    LanguageService.provideCompletionItems (Helpers.completionRequest "src/Main.elm" row column) state


expectItems : String -> List ( String, String ) -> List ( String, CompletionItemKind ) -> Expect.Expectation
expectItems markedText otherFiles expected =
    Helpers.atMarker markedText <|
        \text row column ->
            Helpers.stateFromFiles (( "src/Main.elm", text ) :: otherFiles)
                |> (\state -> completions state row column)
                |> List.map (\item -> ( item.label, item.kind ))
                |> Expect.equal expected


expectLabels : String -> List ( String, String ) -> List String -> Expect.Expectation
expectLabels markedText otherFiles expected =
    Helpers.atMarker markedText <|
        \text row column ->
            Helpers.stateFromFiles (( "src/Main.elm", text ) :: otherFiles)
                |> (\state -> completions state row column)
                |> List.map .label
                |> Expect.equal expected


libraryFiles : List ( String, String )
libraryFiles =
    [ ( "src/Library.elm", "module Library exposing (answer)\n\nanswer = 42\nhidden = 0\n" ) ]
