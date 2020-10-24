module ElmEvaluationUsingPineTests exposing (..)

import ElmEvaluationUsingPine
import Expect
import Json.Encode
import Test


suite : Test.Test
suite =
    Test.describe "Elm evaluation using common engine"
        [ Test.test "Just a literal" <|
            \_ ->
                expectEvalResultJsonTextEqual
                    { elmExpressionText = """  "just a literal ✔️"  """
                    , expectedValueAsJson = "\"just a literal ✔️\""
                    }
        , Test.test "Apply String.fromInt" <|
            \_ ->
                expectEvalResultJsonTextEqual
                    { elmExpressionText = " String.fromInt 123 "
                    , expectedValueAsJson = "\"123\""
                    }
        ]


expectEvalResultJsonTextEqual : { elmExpressionText : String, expectedValueAsJson : String } -> Expect.Expectation
expectEvalResultJsonTextEqual { elmExpressionText, expectedValueAsJson } =
    Expect.equal (Ok expectedValueAsJson)
        (ElmEvaluationUsingPine.evaluateExpressionText elmExpressionText
            |> Result.map (Json.Encode.encode 0)
        )
