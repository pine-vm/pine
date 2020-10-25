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
        , Test.test "Concat string literal" <|
            \_ ->
                expectEvalResultJsonTextEqual
                    { elmExpressionText = """ "first literal " ++ " second literal ✔️" """
                    , expectedValueAsJson = "\"first literal  second literal ✔️\""
                    }
        , Test.test "Apply String.fromInt" <|
            \_ ->
                expectEvalResultJsonTextEqual
                    { elmExpressionText = " String.fromInt 123 "
                    , expectedValueAsJson = "\"123\""
                    }
        , Test.test "Add and apply String.fromInt" <|
            \_ ->
                expectEvalResultJsonTextEqual
                    { elmExpressionText = " String.fromInt (1 + 3) "
                    , expectedValueAsJson = "\"4\""
                    }
        , Test.test "Multiply and apply String.fromInt" <|
            \_ ->
                expectEvalResultJsonTextEqual
                    { elmExpressionText = " String.fromInt (17 * 41) "
                    , expectedValueAsJson = "\"697\""
                    }
        , Test.test "Divide and apply String.fromInt" <|
            \_ ->
                expectEvalResultJsonTextEqual
                    { elmExpressionText = " String.fromInt (31 // 5) "
                    , expectedValueAsJson = "\"6\""
                    }
        , Test.test "Concat string via let" <|
            \_ ->
                expectEvalResultJsonTextEqual
                    { elmExpressionText = """
let
    binding_from_let =
        "literal from let "
in
binding_from_let ++ " second literal ✔️"
"""
                    , expectedValueAsJson = "\"literal from let  second literal ✔️\""
                    }
        , Test.test "Dependency within let" <|
            \_ ->
                expectEvalResultJsonTextEqual
                    { elmExpressionText = """
let
    a = "just a literal"

    b = a
in
b
"""
                    , expectedValueAsJson = "\"just a literal\""
                    }
        , Test.test "Support any order in let" <|
            \_ ->
                expectEvalResultJsonTextEqual
                    { elmExpressionText = """
let
    d = c

    a = "just a literal"

    c = b

    b = a
in
d
"""
                    , expectedValueAsJson = "\"just a literal\""
                    }
        ]


expectEvalResultJsonTextEqual : { elmExpressionText : String, expectedValueAsJson : String } -> Expect.Expectation
expectEvalResultJsonTextEqual { elmExpressionText, expectedValueAsJson } =
    Expect.equal (Ok expectedValueAsJson)
        (ElmEvaluationUsingPine.evaluateExpressionText elmExpressionText
            |> Result.map (Json.Encode.encode 0)
        )
