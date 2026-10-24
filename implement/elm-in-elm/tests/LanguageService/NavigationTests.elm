module LanguageService.NavigationTests exposing (tests)

import Expect
import LanguageService.TestHelpers as Helpers
import LanguageServiceInterface exposing (FileLocation(..), Request(..), Response(..))
import Test exposing (Test, describe, test)


tests : Test
tests =
    describe "Hover and go to definition"
        [ test "Annotated local values include their documentation" <|
            \_ ->
                expectNavigation
                    "module Main exposing (..)\n\n{-| The answer. -}\nanswer : Int\nanswer = 42\n\nmain = an<caret>swer\n"
                    []
                    [ "    answer : Int\n\nThe answer." ]
                    [ Helpers.location "src/Main.elm" ( 4, 1 ) ( 5, 12 ) ]
        , test "An unannotated value still has hover and definition" <|
            \_ ->
                expectNavigation
                    "module Main exposing (..)\n\nanswer = 42\nmain = <caret>answer\n"
                    []
                    [ "    answer" ]
                    [ Helpers.location "src/Main.elm" ( 3, 1 ) ( 3, 12 ) ]
        , test "A declaration name resolves without a usage" <|
            \_ ->
                expectNavigation
                    "module Main exposing (..)\n\nan<caret>swer : Int\nanswer = 42\n"
                    []
                    [ "    answer : Int" ]
                    [ Helpers.location "src/Main.elm" ( 3, 1 ) ( 4, 12 ) ]
        , test "Type annotations resolve local aliases" <|
            \_ ->
                expectNavigation
                    "module Main exposing (..)\n\ntype alias Config = Int\n\nvalue : Con<caret>fig\nvalue = 1\n"
                    []
                    [ "    type alias Config = Int" ]
                    [ Helpers.location "src/Main.elm" ( 3, 1 ) ( 3, 24 ) ]
        , test "Choice constructors show their parent type" <|
            \_ ->
                expectNavigation
                    "module Main exposing (..)\n\ntype Mode = Ready | Busy Int\n\nvalue = Re<caret>ady\n"
                    []
                    [ "    Ready\n\nA variant of the choice type `Mode`\n\n    type Mode = Ready | Busy Int" ]
                    [ Helpers.location "src/Main.elm" ( 3, 13 ) ( 3, 18 ) ]
        , test "Function arguments resolve to their annotated parameter" <|
            \_ ->
                expectNavigation
                    "module Main exposing (..)\n\nidentity : Int -> Int\nidentity argument = ar<caret>gument\n"
                    []
                    [ "    argument : Int" ]
                    [ Helpers.location "src/Main.elm" ( 4, 10 ) ( 4, 18 ) ]
        , test "Let bindings resolve within their body" <|
            \_ ->
                expectNavigation
                    "module Main exposing (..)\n\nmain =\n    let\n        local = 1\n    in\n    lo<caret>cal\n"
                    []
                    [ "    local" ]
                    [ Helpers.location "src/Main.elm" ( 5, 9 ) ( 5, 18 ) ]
        , test "Qualified imports resolve exposed values" <|
            \_ ->
                expectNavigation
                    "module Main exposing (..)\n\nimport Library\n\nmain = Library.an<caret>swer\n"
                    libraryFiles
                    [ "    answer : Int\n\nThe library answer." ]
                    [ Helpers.location "src/Library.elm" ( 6, 1 ) ( 7, 12 ) ]
        , test "Import aliases resolve exposed values" <|
            \_ ->
                expectNavigation
                    "module Main exposing (..)\n\nimport Library as Lib\n\nmain = Lib.an<caret>swer\n"
                    libraryFiles
                    [ "    answer : Int\n\nThe library answer." ]
                    [ Helpers.location "src/Library.elm" ( 6, 1 ) ( 7, 12 ) ]
        , test "Explicit import exposing resolves unqualified values" <|
            \_ ->
                expectNavigation
                    "module Main exposing (..)\n\nimport Library exposing (answer)\n\nmain = an<caret>swer\n"
                    libraryFiles
                    [ "    answer : Int\n\nThe library answer." ]
                    [ Helpers.location "src/Library.elm" ( 6, 1 ) ( 7, 12 ) ]
        , test "The module name in an import resolves to the module declaration" <|
            \_ ->
                expectNavigation
                    "module Main exposing (..)\n\nimport Lib<caret>rary\n\nmain = 0\n"
                    libraryFiles
                    [ "Library documentation." ]
                    [ Helpers.location "src/Library.elm" ( 1, 1 ) ( 1, 33 ) ]
        , test "The qualifier of a reference resolves to its module" <|
            \_ ->
                expectNavigation
                    "module Main exposing (..)\n\nimport Library as Lib\n\nmain = L<caret>ib.answer\n"
                    libraryFiles
                    [ "Library documentation." ]
                    [ Helpers.location "src/Library.elm" ( 1, 1 ) ( 1, 33 ) ]
        , test "Private imported values cannot be navigated to" <|
            \_ ->
                expectNavigation
                    "module Main exposing (..)\n\nimport Library\n\nmain = Library.hid<caret>den\n"
                    libraryFiles
                    []
                    []
        , test "An unknown identifier has no hover or definition" <|
            \_ ->
                expectNavigation "module Main exposing (..)\n\nmain = un<caret>known\n" [] [] []
        , test "Comment text is not treated as a reference" <|
            \_ ->
                expectNavigation "module Main exposing (..)\n\nanswer = 42\n-- an<caret>swer\n" [] [] []
        , test "String contents are not treated as references" <|
            \_ ->
                expectNavigation "module Main exposing (..)\n\nanswer = 42\nmain = \"an<caret>swer\"\n" [] [] []
        , test "Whitespace has no hover or definition" <|
            \_ ->
                expectNavigation "module Main exposing (..)\n<caret>\nanswer = 42\n" [] [] []
        , test "Missing files return empty responses" <|
            \_ ->
                Expect.equal
                    ( Ok (ProvideHoverResponse []), Ok (ProvideDefinitionResponse []) )
                    ( Helpers.request (ProvideHoverRequest (Helpers.pointRequest "missing.elm" 1 1)) Helpers.emptyState
                    , Helpers.request (ProvideDefinitionRequest (Helpers.pointRequest "missing.elm" 1 1)) Helpers.emptyState
                    )
        , test "Package navigation preserves package identity and source path" <|
            \_ ->
                Helpers.atMarker "module Main exposing (..)\n\nimport Tools\n\nmain = Tools.an<caret>swer\n" <|
                    \text row column ->
                        let
                            state =
                                Helpers.addFile "src/Main.elm" text Helpers.packageState

                            point =
                                Helpers.pointRequest "src/Main.elm" row column
                        in
                        Expect.equal
                            ( Ok (ProvideHoverResponse [ "    answer : Int\n\nThe package answer." ])
                            , Ok
                                (ProvideDefinitionResponse
                                    [ { fileLocation = ElmPackageFileLocation Helpers.packageId [ "src", "Tools.elm" ]
                                      , range = Helpers.range ( 4, 1 ) ( 5, 12 )
                                      }
                                    ]
                                )
                            )
                            ( Helpers.request (ProvideHoverRequest point) state
                            , Helpers.request (ProvideDefinitionRequest point) state
                            )
        , test "Hover requests can query package source directly" <|
            \_ ->
                Helpers.request
                    (ProvideHoverRequest
                        { fileLocation = ElmPackageFileLocation Helpers.packageId [ "src", "Tools.elm" ]
                        , positionLineNumber = 5
                        , positionColumn = 2
                        }
                    )
                    Helpers.packageState
                    |> Expect.equal (Ok (ProvideHoverResponse [ "    answer : Int\n\nThe package answer." ]))
        ]


expectNavigation :
    String
    -> List ( String, String )
    -> List String
    -> List LanguageServiceInterface.LocationInFile
    -> Expect.Expectation
expectNavigation markedText otherFiles expectedHover expectedDefinitions =
    Helpers.atMarker markedText <|
        \text row column ->
            let
                state =
                    Helpers.stateFromFiles (( "src/Main.elm", text ) :: otherFiles)

                point =
                    Helpers.pointRequest "src/Main.elm" row column
            in
            Expect.equal
                ( Ok (ProvideHoverResponse expectedHover), Ok (ProvideDefinitionResponse expectedDefinitions) )
                ( Helpers.request (ProvideHoverRequest point) state
                , Helpers.request (ProvideDefinitionRequest point) state
                )


libraryFiles : List ( String, String )
libraryFiles =
    [ ( "src/Library.elm"
      , "module Library exposing (answer)\n\n{-| Library documentation. -}\n\n{-| The library answer. -}\nanswer : Int\nanswer = 42\n\nhidden = 0\n"
      )
    ]
