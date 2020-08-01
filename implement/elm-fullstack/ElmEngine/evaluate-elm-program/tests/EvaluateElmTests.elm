module EvaluateElmTests exposing (..)

import Expect
import Main
import Test


suite : Test.Test
suite =
    Test.describe "Elm evaluation"
        [ Test.test "Just a literal" <|
            \_ ->
                Expect.equal (Ok "\"just a literal ✔️\"")
                    (Main.getValueFromExpressionSyntaxAsJsonString """  "just a literal ✔️"  """)
        , Test.test "Concat string literal" <|
            \_ ->
                Expect.equal (Ok "\"first literal  second literal ✔️\"")
                    (Main.getValueFromExpressionSyntaxAsJsonString """  "first literal "  ++  " second literal ✔️"  """)
        , Test.test "Concat string using let" <|
            \_ ->
                Expect.equal (Ok "\"literal from let  second literal ✔️\"")
                    (Main.getValueFromExpressionSyntaxAsJsonString """
let
    binding_from_let =
        "literal from let "
in
binding_from_let ++ " second literal ✔️"
""")
        , Test.test "Dependency within let" <|
            \_ ->
                Expect.equal (Ok "\"literal\"")
                    (Main.getValueFromExpressionSyntaxAsJsonString """
let
    a = "literal"

    b = a
in
b
""")
        , Test.test "Support any order in let" <|
            \_ ->
                Expect.equal (Ok "\"literal\"")
                    (Main.getValueFromExpressionSyntaxAsJsonString """
let
    d = c

    a = "literal"

    c = b

    b = a
in
d
""")
        ]
