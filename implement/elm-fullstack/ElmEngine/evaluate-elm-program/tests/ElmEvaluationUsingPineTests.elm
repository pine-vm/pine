module ElmEvaluationUsingPineTests exposing (..)

import ElmEvaluationUsingPine
import Expect
import Test


suite : Test.Test
suite =
    Test.describe "Elm evaluation using common engine"
        [ Test.test "Just a literal" <|
            \_ ->
                Expect.equal (Ok { valueAsJsonString = "\"just a literal ✔️\"", typeText = "String" })
                    (ElmEvaluationUsingPine.evaluateExpressionStringWithoutModules """  "just a literal ✔️"  """)
        , Test.test "Apply String.fromInt" <|
            \_ ->
                Expect.equal (Ok { valueAsJsonString = "\"123\"", typeText = "String" })
                    (ElmEvaluationUsingPine.evaluateExpressionStringWithoutModules " String.fromInt 123 ")
        ]
