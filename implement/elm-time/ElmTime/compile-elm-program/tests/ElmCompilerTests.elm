module ElmCompilerTests exposing (..)

import BigInt
import ElmInteractive exposing (InteractiveContext(..))
import Expect
import Pine
import Result.Extra
import Test


type alias ReduceDecodeAndEvalTestCase =
    { original : Pine.DecodeAndEvaluateExpressionStructure
    , expected : Pine.Expression
    , additionalTestEnvironments : List Pine.Value
    }


standardTestEnvironments : List Pine.Value
standardTestEnvironments =
    [ Pine.ListValue []
    , List.range 0 4
        |> List.map (BigInt.fromInt >> Pine.blobValueFromBigInt >> Pine.BlobValue)
        |> Pine.ListValue
    , List.range 3 7
        |> List.map
            (\offsetA ->
                List.range (offsetA * 13) (offsetA * 13 + 5)
                    |> List.map
                        (\offsetB ->
                            List.range (offsetB * 91) (offsetB * 91 + 5)
                                |> List.map (BigInt.fromInt >> Pine.blobValueFromBigInt >> Pine.BlobValue)
                                |> Pine.ListValue
                        )
                    |> Pine.ListValue
            )
        |> Pine.ListValue
    ]


compiler_reduces_decode_and_eval_test_cases : List ( String, ReduceDecodeAndEvalTestCase )
compiler_reduces_decode_and_eval_test_cases =
    [ ( "simple reducible - list head"
      , { original =
            { expression =
                Pine.EnvironmentExpression
                    |> Pine.encodeExpressionAsValue
                    |> Pine.LiteralExpression
            , environment = ElmInteractive.pineKernel_ListHead_Pine Pine.EnvironmentExpression
            }
        , expected =
            ElmInteractive.pineKernel_ListHead_Pine Pine.EnvironmentExpression
        , additionalTestEnvironments = []
        }
      )
    , ( "reducible - skip 2"
      , { original =
            { expression =
                Pine.EnvironmentExpression
                    |> ElmInteractive.listSkipExpression_Pine 2
                    |> ElmInteractive.pineKernel_ListHead_Pine
                    |> Pine.encodeExpressionAsValue
                    |> Pine.LiteralExpression
            , environment =
                Pine.ListExpression
                    [ Pine.EnvironmentExpression
                        |> ElmInteractive.listSkipExpression_Pine 4
                    , Pine.EnvironmentExpression
                        |> ElmInteractive.listSkipExpression_Pine 3
                    , Pine.EnvironmentExpression
                        |> ElmInteractive.listSkipExpression_Pine 3
                    , Pine.EnvironmentExpression
                        |> ElmInteractive.listSkipExpression_Pine 2
                    ]
            }
        , expected =
            Pine.EnvironmentExpression
                |> ElmInteractive.listSkipExpression_Pine 3
        , additionalTestEnvironments = []
        }
      )
    , ( "reducible - skip 1 (skip 2)"
      , { original =
            { expression =
                Pine.EnvironmentExpression
                    |> ElmInteractive.listSkipExpression_Pine 2
                    |> ElmInteractive.pineKernel_ListHead_Pine
                    |> ElmInteractive.listSkipExpression_Pine 1
                    |> ElmInteractive.pineKernel_ListHead_Pine
                    |> Pine.encodeExpressionAsValue
                    |> Pine.LiteralExpression
            , environment =
                Pine.ListExpression
                    [ Pine.ListExpression
                        [ Pine.ListExpression []
                        , Pine.EnvironmentExpression
                            |> ElmInteractive.listSkipExpression_Pine 4
                        ]
                    , Pine.ListExpression
                        [ Pine.ListExpression []
                        , Pine.EnvironmentExpression
                            |> ElmInteractive.listSkipExpression_Pine 3
                        ]
                    , Pine.ListExpression
                        [ Pine.ListExpression []
                        , Pine.EnvironmentExpression
                            |> ElmInteractive.listSkipExpression_Pine 3
                        ]
                    , Pine.ListExpression
                        [ Pine.ListExpression []
                        , Pine.EnvironmentExpression
                            |> ElmInteractive.listSkipExpression_Pine 2
                        ]
                    ]
            }
        , expected =
            Pine.EnvironmentExpression
                |> ElmInteractive.listSkipExpression_Pine 3
        , additionalTestEnvironments = []
        }
      )
    , ( "simple irreducible"
      , { original =
            { expression = Pine.EnvironmentExpression
            , environment = Pine.EnvironmentExpression
            }
        , expected =
            { expression = Pine.EnvironmentExpression
            , environment = Pine.EnvironmentExpression
            }
                |> Pine.DecodeAndEvaluateExpression
        , additionalTestEnvironments = []
        }
      )
    ]


test_compiler_reduces_decode_and_eval_test_cases : Test.Test
test_compiler_reduces_decode_and_eval_test_cases =
    compiler_reduces_decode_and_eval_test_cases
        |> List.indexedMap
            (\testCaseIndex ( testCaseName, testCase ) ->
                let
                    allTestEnvironments =
                        standardTestEnvironments ++ testCase.additionalTestEnvironments
                in
                allTestEnvironments
                    |> List.indexedMap
                        (\envIndex enviroment ->
                            Test.test ("Environment " ++ String.fromInt envIndex) <|
                                \_ ->
                                    testCase.original
                                        |> Pine.DecodeAndEvaluateExpression
                                        |> Pine.evaluateExpression { environment = enviroment }
                                        |> Expect.equal
                                            (Pine.evaluateExpression
                                                { environment = enviroment }
                                                testCase.expected
                                            )
                        )
                    |> Test.describe ("Expression " ++ String.fromInt testCaseIndex ++ " - " ++ testCaseName)
            )
        |> Test.describe "Test cases - Compiler reduces decode and eval expression"


compilerReducesDecodeAndEvaluateExpression : Test.Test
compilerReducesDecodeAndEvaluateExpression =
    compiler_reduces_decode_and_eval_test_cases
        |> List.indexedMap
            (\testCaseIndex ( testCaseName, testCase ) ->
                Test.test ("Expression " ++ String.fromInt testCaseIndex ++ " - " ++ testCaseName) <|
                    \_ ->
                        testCase.original
                            |> ElmInteractive.attemptReduceDecodeAndEvaluateExpressionRecursive { maxDepth = 4 }
                            |> Expect.equal testCase.expected
            )
        |> Test.describe "Compiler reduces decode and eval expression"


generateTemplateEvaluatingToExpressionTests : Test.Test
generateTemplateEvaluatingToExpressionTests =
    [ ( "Empty list expression"
      , Pine.ListExpression []
      )
    , ( "List of two empty lists"
      , Pine.ListExpression
            [ Pine.ListExpression []
            , Pine.ListExpression []
            ]
      )
    , ( "String literal"
      , Pine.LiteralExpression (Pine.valueFromString "test")
      )
    , ( "Int literal"
      , Pine.LiteralExpression (Pine.valueFromBigInt (BigInt.fromInt 3))
      )
    , ( "DecodeAndEvaluateExpression"
      , Pine.DecodeAndEvaluateExpression
            { expression = Pine.LiteralExpression (Pine.valueFromString "expression-placeholder")
            , environment = Pine.LiteralExpression (Pine.valueFromString "environment-placeholder")
            }
      )
    , ( "KernelApplicationExpression"
      , Pine.KernelApplicationExpression
            { functionName = "equal"
            , argument = Pine.LiteralExpression (Pine.valueFromString "argument-placeholder")
            }
      )
    , ( "ConditionalExpression"
      , Pine.ConditionalExpression
            { condition = Pine.LiteralExpression (Pine.valueFromString "condition-placeholder")
            , ifTrue = Pine.LiteralExpression (Pine.valueFromString "ifTrue-placeholder")
            , ifFalse = Pine.LiteralExpression (Pine.valueFromString "ifFalse-placeholder")
            }
      )
    , ( "EnvironmentExpression"
      , Pine.EnvironmentExpression
      )
    , ( "StringTagExpression"
      , Pine.StringTagExpression
            "tag-placeholder"
            (Pine.LiteralExpression (Pine.valueFromString "tagged-expr-placeholder"))
      )
    ]
        |> List.indexedMap
            (\testCaseIndex ( testCaseName, testedExpression ) ->
                Test.test ("Case " ++ String.fromInt testCaseIndex ++ " - " ++ testCaseName) <|
                    \_ ->
                        let
                            template =
                                ElmInteractive.generateTemplateEvaluatingToExpression testedExpression
                        in
                        Pine.evaluateExpression
                            Pine.emptyEvalContext
                            template
                            |> Result.mapError (Pine.displayStringFromPineError >> (++) "Failed evaluate template: ")
                            |> Result.andThen
                                (\value ->
                                    value
                                        |> Pine.decodeExpressionFromValue
                                        |> Result.mapError ((++) "Failed decode expression from value: ")
                                )
                            |> Result.Extra.unpack
                                Expect.fail
                                (Expect.equal testedExpression)
            )
        |> Test.describe "generate template evaluating to expression"
