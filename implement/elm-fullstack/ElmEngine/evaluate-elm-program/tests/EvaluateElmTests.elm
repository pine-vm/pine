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
        ]
