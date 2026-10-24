module LanguageService.SymbolTests exposing (tests)

import Expect
import LanguageService.TestHelpers as Helpers
import LanguageServiceInterface exposing (DocumentSymbol(..), Request(..), Response(..), SymbolKind(..))
import Test exposing (Test, describe, test)


tests : Test
tests =
    describe "Document symbols"
        [ test "Function ranges include the signature but not the documentation" <|
            \_ ->
                expectSymbols "module Main exposing (..)\n\n{-| The answer. -}\nanswer : Int\nanswer = 42\n"
                    [ DocumentSymbol
                        { name = "answer"
                        , kind = SymbolKind_Function
                        , range = Helpers.range ( 4, 1 ) ( 5, 12 )
                        , selectionRange = Helpers.range ( 5, 1 ) ( 5, 7 )
                        , children = []
                        }
                    ]
        , test "Type aliases have struct kind and a name-only selection" <|
            \_ ->
                expectSymbols "module Main exposing (..)\n\ntype alias Config = Int\n"
                    [ DocumentSymbol
                        { name = "Config"
                        , kind = SymbolKind_Struct
                        , range = Helpers.range ( 3, 1 ) ( 3, 24 )
                        , selectionRange = Helpers.range ( 3, 12 ) ( 3, 18 )
                        , children = []
                        }
                    ]
        , test "Choice constructors are nested enum members with precise ranges" <|
            \_ ->
                expectSymbols "module Main exposing (..)\n\ntype Mode\n    = Ready\n    | Busy Int\n"
                    [ DocumentSymbol
                        { name = "Mode"
                        , kind = SymbolKind_Enum
                        , range = Helpers.range ( 3, 1 ) ( 5, 15 )
                        , selectionRange = Helpers.range ( 3, 6 ) ( 3, 10 )
                        , children =
                            [ DocumentSymbol
                                { name = "Ready"
                                , kind = SymbolKind_EnumMember
                                , range = Helpers.range ( 4, 7 ) ( 4, 12 )
                                , selectionRange = Helpers.range ( 4, 7 ) ( 4, 12 )
                                , children = []
                                }
                            , DocumentSymbol
                                { name = "Busy"
                                , kind = SymbolKind_EnumMember
                                , range = Helpers.range ( 5, 7 ) ( 5, 15 )
                                , selectionRange = Helpers.range ( 5, 7 ) ( 5, 11 )
                                , children = []
                                }
                            ]
                        }
                    ]
        , test "Local let declarations do not become top-level symbols" <|
            \_ ->
                expectSymbols "module Main exposing (..)\n\nmain =\n    let\n        local = 1\n    in\n    local\n"
                    [ DocumentSymbol
                        { name = "main"
                        , kind = SymbolKind_Function
                        , range = Helpers.range ( 3, 1 ) ( 7, 10 )
                        , selectionRange = Helpers.range ( 3, 1 ) ( 3, 5 )
                        , children = []
                        }
                    ]
        , test "Symbols preserve source order and include private declarations" <|
            \_ ->
                expectSymbols "module Main exposing (zeta)\n\nzeta = 1\nalpha = 2\n"
                    [ DocumentSymbol
                        { name = "zeta"
                        , kind = SymbolKind_Function
                        , range = Helpers.range ( 3, 1 ) ( 3, 9 )
                        , selectionRange = Helpers.range ( 3, 1 ) ( 3, 5 )
                        , children = []
                        }
                    , DocumentSymbol
                        { name = "alpha"
                        , kind = SymbolKind_Function
                        , range = Helpers.range ( 4, 1 ) ( 4, 10 )
                        , selectionRange = Helpers.range ( 4, 1 ) ( 4, 6 )
                        , children = []
                        }
                    ]
        , test "A module without declarations has no symbols" <|
            \_ ->
                expectSymbols "module Main exposing (..)\n" []
        , test "A missing document returns an empty symbol response" <|
            \_ ->
                Helpers.request (TextDocumentSymbolRequest "missing.elm") Helpers.emptyState
                    |> Expect.equal (Ok (TextDocumentSymbolResponse []))
        ]


expectSymbols : String -> List DocumentSymbol -> Expect.Expectation
expectSymbols text expected =
    Helpers.stateFromFiles [ ( "src/Main.elm", text ) ]
        |> Helpers.request (TextDocumentSymbolRequest "src/Main.elm")
        |> Expect.equal (Ok (TextDocumentSymbolResponse expected))
