module EvaluateElmTests exposing (..)

import Expect
import Main
import Test


suite : Test.Test
suite =
    Test.test "Just a literal" <|
        \_ ->
            Expect.equal (Ok "\"just a literal ✔️\"")
                (Main.getValueFromExpressionAsJsonString """  "just a literal ✔️"  """)
