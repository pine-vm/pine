module ElmCompilerTests exposing (..)

import BigInt
import Dict
import ElmInteractive exposing (Expression(..), InteractiveContext(..))
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
    [ ( "simple reducible - literal"
      , { original =
            { expression =
                Pine.LiteralExpression (Pine.valueFromString "test")
                    |> Pine.encodeExpressionAsValue
                    |> Pine.LiteralExpression
            , environment = Pine.ListExpression []
            }
        , expected = Pine.LiteralExpression (Pine.valueFromString "test")
        , additionalTestEnvironments = []
        }
      )
    , ( "simple reducible - list head"
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


emitClosureExpressionTests : Test.Test
emitClosureExpressionTests =
    [ ( "Zero parameters"
      , { functionInnerExpr =
            ElmInteractive.LiteralExpression (Pine.valueFromString "test")
        , functionParams = []
        , arguments = []
        , environmentFunctions = []
        , expectedValue = Pine.valueFromString "test"
        }
      )
    , ( "Zero parameters - return from function with one param"
      , { functionInnerExpr =
            ElmInteractive.FunctionApplicationExpression
                (ReferenceExpression "repeat_three_times")
                [ ElmInteractive.LiteralExpression (Pine.valueFromString "argument_alfa") ]
        , functionParams = []
        , arguments = []
        , environmentFunctions =
            [ ( "repeat_three_times"
              , { functionInnerExpr =
                    ElmInteractive.ListExpression
                        [ ElmInteractive.ReferenceExpression "param_name"
                        , ElmInteractive.ReferenceExpression "param_name"
                        , ElmInteractive.ReferenceExpression "param_name"
                        ]
                , functionParams =
                    [ [ ( "param_name", [] ) ] ]
                }
              )
            ]
        , expectedValue =
            Pine.ListValue
                [ Pine.valueFromString "argument_alfa"
                , Pine.valueFromString "argument_alfa"
                , Pine.valueFromString "argument_alfa"
                ]
        }
      )
    , ( "Zero parameters - return literal from function with zero param - once"
      , { functionInnerExpr =
            ElmInteractive.FunctionApplicationExpression
                (ReferenceExpression "return_constant_literal")
                []
        , functionParams = []
        , arguments = []
        , environmentFunctions =
            [ ( "return_constant_literal"
              , { functionInnerExpr =
                    ElmInteractive.LiteralExpression (Pine.valueFromString "constant")
                , functionParams = []
                }
              )
            ]
        , expectedValue = Pine.valueFromString "constant"
        }
      )
    , ( "Zero parameters - return literal from function with zero param - twice"
      , { functionInnerExpr =
            ElmInteractive.FunctionApplicationExpression
                (ReferenceExpression "return_constant_literal_first")
                []
        , functionParams = []
        , arguments = []
        , environmentFunctions =
            [ ( "return_constant_literal_first"
              , { functionInnerExpr =
                    ElmInteractive.FunctionApplicationExpression
                        (ReferenceExpression "return_constant_literal_second")
                        []
                , functionParams = []
                }
              )
            , ( "return_constant_literal_second"
              , { functionInnerExpr =
                    ElmInteractive.LiteralExpression (Pine.valueFromString "constant")
                , functionParams = []
                }
              )
            ]
        , expectedValue = Pine.valueFromString "constant"
        }
      )
    , ( "One parameter - literal"
      , { functionInnerExpr =
            ElmInteractive.LiteralExpression (Pine.valueFromString "test-literal")
        , functionParams = [ [ ( "param-name", [] ) ] ]
        , arguments = [ Pine.valueFromString "test-123" ]
        , environmentFunctions = []
        , expectedValue = Pine.valueFromString "test-literal"
        }
      )
    , ( "One parameter - reference"
      , { functionInnerExpr =
            ElmInteractive.ReferenceExpression "param-name"
        , functionParams = [ [ ( "param-name", [] ) ] ]
        , arguments = [ Pine.valueFromString "test-345" ]
        , environmentFunctions = []
        , expectedValue = Pine.valueFromString "test-345"
        }
      )
    , ( "One parameter - reference decons tuple second"
      , { functionInnerExpr =
            ElmInteractive.ReferenceExpression "param-name"
        , functionParams =
            [ [ ( "param-name"
                , [ ElmInteractive.ListItemDeconstruction 1 ]
                )
              ]
            ]
        , arguments =
            [ Pine.ListValue
                [ Pine.ListValue []
                , Pine.valueFromString "test-456"
                ]
            ]
        , environmentFunctions = []
        , expectedValue = Pine.valueFromString "test-456"
        }
      )
    , ( "One parameter - repeat"
      , { functionInnerExpr =
            ElmInteractive.FunctionApplicationExpression
                (ReferenceExpression "repeat_help")
                [ ElmInteractive.ListExpression
                    [ ElmInteractive.LiteralExpression (Pine.ListValue [])
                    , ElmInteractive.ReferenceExpression "count"
                    , ElmInteractive.ReferenceExpression "value"
                    ]
                ]
        , functionParams =
            [ [ ( "count"
                , [ ElmInteractive.ListItemDeconstruction 0 ]
                )
              , ( "value"
                , [ ElmInteractive.ListItemDeconstruction 1 ]
                )
              ]
            ]
        , arguments =
            [ Pine.ListValue
                [ Pine.valueFromBigInt (BigInt.fromInt 3)
                , Pine.valueFromString "test_elem"
                ]
            ]
        , environmentFunctions =
            [ ( "repeat_help"
              , { functionInnerExpr =
                    ElmInteractive.ConditionalExpression
                        { condition =
                            ElmInteractive.KernelApplicationExpression
                                { functionName = "is_sorted_ascending_int"
                                , argument =
                                    ElmInteractive.ListExpression
                                        [ ElmInteractive.ReferenceExpression "remainingCount"
                                        , ElmInteractive.LiteralExpression (Pine.valueFromBigInt (BigInt.fromInt 0))
                                        ]
                                }
                        , ifTrue =
                            ElmInteractive.ReferenceExpression "result"
                        , ifFalse =
                            ElmInteractive.FunctionApplicationExpression
                                (ReferenceExpression "repeat_help")
                                [ ElmInteractive.ListExpression
                                    [ ElmInteractive.KernelApplicationExpression
                                        { functionName = "concat"
                                        , argument =
                                            ElmInteractive.ListExpression
                                                [ ElmInteractive.ListExpression
                                                    [ ElmInteractive.ReferenceExpression "value"
                                                    ]
                                                , ElmInteractive.ReferenceExpression "result"
                                                ]
                                        }
                                    , ElmInteractive.KernelApplicationExpression
                                        { functionName = "sub_int"
                                        , argument =
                                            ElmInteractive.ListExpression
                                                [ ElmInteractive.ReferenceExpression "remainingCount"
                                                , ElmInteractive.LiteralExpression (Pine.valueFromBigInt (BigInt.fromInt 1))
                                                ]
                                        }
                                    , ElmInteractive.ReferenceExpression "value"
                                    ]
                                ]
                        }
                , functionParams =
                    [ [ ( "result"
                        , [ ElmInteractive.ListItemDeconstruction 0 ]
                        )
                      , ( "remainingCount"
                        , [ ElmInteractive.ListItemDeconstruction 1 ]
                        )
                      , ( "value"
                        , [ ElmInteractive.ListItemDeconstruction 2 ]
                        )
                      ]
                    ]
                }
              )
            ]
        , expectedValue =
            Pine.valueFromString "test_elem"
                |> List.repeat 3
                |> Pine.ListValue
        }
      )
    , ( "One parameter - repeat - separate <= 0"
      , { functionInnerExpr =
            ElmInteractive.FunctionApplicationExpression
                (ReferenceExpression "repeat_help")
                [ ElmInteractive.ListExpression
                    [ ElmInteractive.LiteralExpression (Pine.ListValue [])
                    , ElmInteractive.ReferenceExpression "count"
                    , ElmInteractive.ReferenceExpression "value"
                    ]
                ]
        , functionParams =
            [ [ ( "count"
                , [ ElmInteractive.ListItemDeconstruction 0 ]
                )
              , ( "value"
                , [ ElmInteractive.ListItemDeconstruction 1 ]
                )
              ]
            ]
        , arguments =
            [ Pine.ListValue
                [ Pine.valueFromBigInt (BigInt.fromInt 3)
                , Pine.valueFromString "test_elem"
                ]
            ]
        , environmentFunctions =
            [ ( "is_less_than_or_equal_to_zero"
              , { functionInnerExpr =
                    ElmInteractive.KernelApplicationExpression
                        { functionName = "is_sorted_ascending_int"
                        , argument =
                            ElmInteractive.ListExpression
                                [ ElmInteractive.ReferenceExpression "num"
                                , ElmInteractive.LiteralExpression (Pine.valueFromBigInt (BigInt.fromInt 0))
                                ]
                        }
                , functionParams =
                    [ [ ( "num", [] )
                      ]
                    ]
                }
              )
            , ( "repeat_help"
              , { functionInnerExpr =
                    ElmInteractive.ConditionalExpression
                        { condition =
                            ElmInteractive.FunctionApplicationExpression
                                (ReferenceExpression "is_less_than_or_equal_to_zero")
                                [ ElmInteractive.ReferenceExpression "remainingCount"
                                ]
                        , ifTrue =
                            ElmInteractive.ReferenceExpression "result"
                        , ifFalse =
                            ElmInteractive.FunctionApplicationExpression
                                (ReferenceExpression "repeat_help")
                                [ ElmInteractive.ListExpression
                                    [ ElmInteractive.KernelApplicationExpression
                                        { functionName = "concat"
                                        , argument =
                                            ElmInteractive.ListExpression
                                                [ ElmInteractive.ListExpression
                                                    [ ElmInteractive.ReferenceExpression "value"
                                                    ]
                                                , ElmInteractive.ReferenceExpression "result"
                                                ]
                                        }
                                    , ElmInteractive.KernelApplicationExpression
                                        { functionName = "sub_int"
                                        , argument =
                                            ElmInteractive.ListExpression
                                                [ ElmInteractive.ReferenceExpression "remainingCount"
                                                , ElmInteractive.LiteralExpression (Pine.valueFromBigInt (BigInt.fromInt 1))
                                                ]
                                        }
                                    , ElmInteractive.ReferenceExpression "value"
                                    ]
                                ]
                        }
                , functionParams =
                    [ [ ( "result"
                        , [ ElmInteractive.ListItemDeconstruction 0 ]
                        )
                      , ( "remainingCount"
                        , [ ElmInteractive.ListItemDeconstruction 1 ]
                        )
                      , ( "value"
                        , [ ElmInteractive.ListItemDeconstruction 2 ]
                        )
                      ]
                    ]
                }
              )
            ]
        , expectedValue =
            Pine.valueFromString "test_elem"
                |> List.repeat 3
                |> Pine.ListValue
        }
      )
    , ( "Two parameters - return literal"
      , { functionInnerExpr = ElmInteractive.LiteralExpression (Pine.valueFromString "constant-literal")
        , functionParams =
            [ [ ( "param_alfa", [] )
              ]
            , [ ( "param_beta", [] )
              ]
            ]
        , arguments =
            [ Pine.valueFromString "argument_alfa"
            , Pine.valueFromString "argument_beta"
            ]
        , environmentFunctions = []
        , expectedValue = Pine.valueFromString "constant-literal"
        }
      )
    , ( "Two parameters - return second"
      , { functionInnerExpr = ElmInteractive.ReferenceExpression "param_beta"
        , functionParams =
            [ [ ( "param_alfa", [] )
              ]
            , [ ( "param_beta", [] )
              ]
            ]
        , arguments =
            [ Pine.valueFromString "argument_alfa"
            , Pine.valueFromString "argument_beta"
            ]
        , environmentFunctions = []
        , expectedValue = Pine.valueFromString "argument_beta"
        }
      )
    , ( "Two parameters - return first"
      , { functionInnerExpr = ElmInteractive.ReferenceExpression "param_alfa"
        , functionParams =
            [ [ ( "param_alfa", [] )
              ]
            , [ ( "param_beta", [] )
              ]
            ]
        , arguments =
            [ Pine.valueFromString "argument_alfa"
            , Pine.valueFromString "argument_beta"
            ]
        , environmentFunctions = []
        , expectedValue = Pine.valueFromString "argument_alfa"
        }
      )
    , ( "Three parameters - return literal"
      , { functionInnerExpr = ElmInteractive.LiteralExpression (Pine.valueFromString "constant-literal")
        , functionParams =
            [ [ ( "param_alfa", [] )
              ]
            , [ ( "param_beta", [] )
              ]
            , [ ( "param_gamma", [] )
              ]
            ]
        , arguments =
            [ Pine.valueFromString "argument_alfa"
            , Pine.valueFromString "argument_beta"
            , Pine.valueFromString "argument_gamma"
            ]
        , environmentFunctions = []
        , expectedValue = Pine.valueFromString "constant-literal"
        }
      )
    , ( "Three parameters - return third"
      , { functionInnerExpr = ElmInteractive.ReferenceExpression "param_gamma"
        , functionParams =
            [ [ ( "param_alfa", [] )
              ]
            , [ ( "param_beta", [] )
              ]
            , [ ( "param_gamma", [] )
              ]
            ]
        , arguments =
            [ Pine.valueFromString "argument_alfa"
            , Pine.valueFromString "argument_beta"
            , Pine.valueFromString "argument_gamma"
            ]
        , environmentFunctions = []
        , expectedValue = Pine.valueFromString "argument_gamma"
        }
      )
    , ( "Three parameters - return second"
      , { functionInnerExpr = ElmInteractive.ReferenceExpression "param_beta"
        , functionParams =
            [ [ ( "param_alfa", [] ) ]
            , [ ( "param_beta", [] ) ]
            , [ ( "param_gamma", [] ) ]
            ]
        , arguments =
            [ Pine.valueFromString "argument_alfa"
            , Pine.valueFromString "argument_beta"
            , Pine.valueFromString "argument_gamma"
            ]
        , environmentFunctions = []
        , expectedValue = Pine.valueFromString "argument_beta"
        }
      )
    , ( "Three parameters - return first"
      , { functionInnerExpr = ElmInteractive.ReferenceExpression "param_alfa"
        , functionParams =
            [ [ ( "param_alfa", [] ) ]
            , [ ( "param_beta", [] ) ]
            , [ ( "param_gamma", [] ) ]
            ]
        , arguments =
            [ Pine.valueFromString "argument_alfa"
            , Pine.valueFromString "argument_beta"
            , Pine.valueFromString "argument_gamma"
            ]
        , environmentFunctions = []
        , expectedValue = Pine.valueFromString "argument_alfa"
        }
      )
    , ( "Three parameters - return from function with one param"
      , { functionInnerExpr =
            ElmInteractive.FunctionApplicationExpression
                (ReferenceExpression "repeat_three_times")
                [ ElmInteractive.ReferenceExpression "param_alfa" ]
        , functionParams =
            [ [ ( "param_alfa", [] ) ]
            , [ ( "param_beta", [] ) ]
            , [ ( "param_gamma", [] ) ]
            ]
        , arguments =
            [ Pine.valueFromString "argument_alfa"
            , Pine.valueFromString "argument_beta"
            , Pine.valueFromString "argument_gamma"
            ]
        , environmentFunctions =
            [ ( "repeat_three_times"
              , { functionInnerExpr =
                    ElmInteractive.ListExpression
                        [ ElmInteractive.ReferenceExpression "param_name"
                        , ElmInteractive.ReferenceExpression "param_name"
                        , ElmInteractive.ReferenceExpression "param_name"
                        ]
                , functionParams =
                    [ [ ( "param_name", [] ) ] ]
                }
              )
            ]
        , expectedValue =
            Pine.ListValue
                [ Pine.valueFromString "argument_alfa"
                , Pine.valueFromString "argument_alfa"
                , Pine.valueFromString "argument_alfa"
                ]
        }
      )
    , ( "Three parameters - return from function with two param - first"
      , { functionInnerExpr =
            ElmInteractive.FunctionApplicationExpression
                (ReferenceExpression "repeat_three_times")
                [ ElmInteractive.ReferenceExpression "param_alfa"
                , ElmInteractive.ReferenceExpression "param_beta"
                ]
        , functionParams =
            [ [ ( "param_alfa", [] )
              ]
            , [ ( "param_beta", [] )
              ]
            , [ ( "param_gamma", [] )
              ]
            ]
        , arguments =
            [ Pine.valueFromString "argument_alfa"
            , Pine.valueFromString "argument_beta"
            , Pine.valueFromString "argument_gamma"
            ]
        , environmentFunctions =
            [ ( "repeat_three_times"
              , { functionInnerExpr =
                    ElmInteractive.ListExpression
                        [ ElmInteractive.ReferenceExpression "param_name_a"
                        , ElmInteractive.ReferenceExpression "param_name_a"
                        , ElmInteractive.ReferenceExpression "param_name_a"
                        ]
                , functionParams =
                    [ [ ( "param_name_a", [] )
                      ]
                    , [ ( "param_name_b", [] )
                      ]
                    ]
                }
              )
            ]
        , expectedValue =
            Pine.ListValue
                [ Pine.valueFromString "argument_alfa"
                , Pine.valueFromString "argument_alfa"
                , Pine.valueFromString "argument_alfa"
                ]
        }
      )
    , ( "Two parameters - repeat"
      , { functionInnerExpr =
            ElmInteractive.FunctionApplicationExpression
                (ReferenceExpression "repeat_help")
                [ ElmInteractive.LiteralExpression (Pine.ListValue [])
                , ElmInteractive.ListExpression
                    [ ElmInteractive.ReferenceExpression "count"
                    , ElmInteractive.ReferenceExpression "value"
                    ]
                ]
        , functionParams =
            [ [ ( "count", [] )
              ]
            , [ ( "value", [] )
              ]
            ]
        , arguments =
            [ Pine.valueFromBigInt (BigInt.fromInt 3)
            , Pine.valueFromString "test_elem_two"
            ]
        , environmentFunctions =
            [ ( "repeat_help"
              , { functionInnerExpr =
                    ElmInteractive.ConditionalExpression
                        { condition =
                            ElmInteractive.KernelApplicationExpression
                                { functionName = "is_sorted_ascending_int"
                                , argument =
                                    ElmInteractive.ListExpression
                                        [ ElmInteractive.ReferenceExpression "remainingCount"
                                        , ElmInteractive.LiteralExpression (Pine.valueFromBigInt (BigInt.fromInt 0))
                                        ]
                                }
                        , ifTrue = ElmInteractive.ReferenceExpression "result"
                        , ifFalse =
                            ElmInteractive.FunctionApplicationExpression
                                (ReferenceExpression "repeat_help")
                                [ ElmInteractive.KernelApplicationExpression
                                    { functionName = "concat"
                                    , argument =
                                        ElmInteractive.ListExpression
                                            [ ElmInteractive.ListExpression
                                                [ ElmInteractive.ReferenceExpression "value"
                                                ]
                                            , ElmInteractive.ReferenceExpression "result"
                                            ]
                                    }
                                , ElmInteractive.ListExpression
                                    [ ElmInteractive.KernelApplicationExpression
                                        { functionName = "sub_int"
                                        , argument =
                                            ElmInteractive.ListExpression
                                                [ ElmInteractive.ReferenceExpression "remainingCount"
                                                , ElmInteractive.LiteralExpression (Pine.valueFromBigInt (BigInt.fromInt 1))
                                                ]
                                        }
                                    , ElmInteractive.ReferenceExpression "value"
                                    ]
                                ]
                        }
                , functionParams =
                    [ [ ( "result", [] )
                      ]
                    , [ ( "remainingCount"
                        , [ ElmInteractive.ListItemDeconstruction 0 ]
                        )
                      , ( "value"
                        , [ ElmInteractive.ListItemDeconstruction 1 ]
                        )
                      ]
                    ]
                }
              )
            ]
        , expectedValue =
            Pine.valueFromString "test_elem_two"
                |> List.repeat 3
                |> Pine.ListValue
        }
      )
    , ( "Three parameters - repeat"
      , { functionInnerExpr =
            ElmInteractive.FunctionApplicationExpression
                (ReferenceExpression "repeat_help")
                (List.map ElmInteractive.LiteralExpression
                    [ Pine.ListValue []
                    , Pine.valueFromBigInt (BigInt.fromInt 3)
                    , Pine.valueFromString "test_elem"
                    ]
                )
        , functionParams = []
        , arguments = []
        , environmentFunctions =
            [ ( "repeat_help"
              , { functionInnerExpr =
                    ElmInteractive.ConditionalExpression
                        { condition =
                            ElmInteractive.KernelApplicationExpression
                                { functionName = "is_sorted_ascending_int"
                                , argument =
                                    ElmInteractive.ListExpression
                                        [ ElmInteractive.ReferenceExpression "remainingCount"
                                        , ElmInteractive.LiteralExpression (Pine.valueFromBigInt (BigInt.fromInt 0))
                                        ]
                                }
                        , ifTrue =
                            ElmInteractive.ReferenceExpression "result"
                        , ifFalse =
                            ElmInteractive.FunctionApplicationExpression
                                (ReferenceExpression "repeat_help")
                                [ ElmInteractive.KernelApplicationExpression
                                    { functionName = "concat"
                                    , argument =
                                        ElmInteractive.ListExpression
                                            [ ElmInteractive.ListExpression
                                                [ ElmInteractive.ReferenceExpression "value"
                                                ]
                                            , ElmInteractive.ReferenceExpression "result"
                                            ]
                                    }
                                , ElmInteractive.KernelApplicationExpression
                                    { functionName = "sub_int"
                                    , argument =
                                        ElmInteractive.ListExpression
                                            [ ElmInteractive.ReferenceExpression "remainingCount"
                                            , ElmInteractive.LiteralExpression (Pine.valueFromBigInt (BigInt.fromInt 1))
                                            ]
                                    }
                                , ElmInteractive.ReferenceExpression "value"
                                ]
                        }
                , functionParams =
                    [ [ ( "result", [] )
                      ]
                    , [ ( "remainingCount", [] )
                      ]
                    , [ ( "value", [] )
                      ]
                    ]
                }
              )
            ]
        , expectedValue =
            Pine.valueFromString "test_elem"
                |> List.repeat 3
                |> Pine.ListValue
        }
      )
    , ( "let block returning literal"
      , { functionInnerExpr =
            ElmInteractive.LetBlockExpression
                { declarations =
                    [ ( "decl_from_let"
                      , ElmInteractive.LiteralExpression (Pine.valueFromString "constant_in_let")
                      )
                    ]
                , expression = ElmInteractive.ReferenceExpression "decl_from_let"
                }
        , functionParams = []
        , arguments = []
        , environmentFunctions = []
        , expectedValue = Pine.valueFromString "constant_in_let"
        }
      )
    , ( "let block returning from other decl in same block"
      , { functionInnerExpr =
            ElmInteractive.LetBlockExpression
                { declarations =
                    [ ( "decl_from_let"
                      , ElmInteractive.ReferenceExpression "other_decl_from_let"
                      )
                    , ( "other_decl_from_let"
                      , ElmInteractive.LiteralExpression (Pine.valueFromString "constant_in_let")
                      )
                    ]
                , expression = ElmInteractive.ReferenceExpression "decl_from_let"
                }
        , functionParams = []
        , arguments = []
        , environmentFunctions = []
        , expectedValue = Pine.valueFromString "constant_in_let"
        }
      )
    , ( "let block returning only parent function arg"
      , { functionInnerExpr =
            ElmInteractive.LetBlockExpression
                { declarations =
                    [ ( "decl_from_let"
                      , ElmInteractive.ReferenceExpression "param_0"
                      )
                    ]
                , expression = ElmInteractive.ReferenceExpression "decl_from_let"
                }
        , functionParams =
            [ [ ( "param_0", [] ) ]
            ]
        , arguments = [ Pine.valueFromString "argument_0" ]
        , environmentFunctions = []
        , expectedValue = Pine.valueFromString "argument_0"
        }
      )
    , ( "let block in let block returning only parent function arg"
      , { functionInnerExpr =
            ElmInteractive.LetBlockExpression
                { declarations =
                    [ ( "decl_from_let"
                      , ElmInteractive.ReferenceExpression "param_0"
                      )
                    ]
                , expression =
                    ElmInteractive.LetBlockExpression
                        { declarations =
                            [ ( "decl_from_let_inner"
                              , ElmInteractive.ReferenceExpression "decl_from_let"
                              )
                            ]
                        , expression = ElmInteractive.ReferenceExpression "decl_from_let_inner"
                        }
                }
        , functionParams =
            [ [ ( "param_0", [] ) ]
            ]
        , arguments = [ Pine.valueFromString "argument_0" ]
        , environmentFunctions = []
        , expectedValue = Pine.valueFromString "argument_0"
        }
      )
    , ( "let block returning second parent function arg"
      , { functionInnerExpr =
            ElmInteractive.LetBlockExpression
                { declarations =
                    [ ( "decl_from_let"
                      , ElmInteractive.ReferenceExpression "param_1"
                      )
                    ]
                , expression = ElmInteractive.ReferenceExpression "decl_from_let"
                }
        , functionParams =
            [ [ ( "param_0", [] ) ]
            , [ ( "param_1", [] ) ]
            ]
        , arguments =
            [ Pine.valueFromString "argument_0"
            , Pine.valueFromString "argument_1"
            ]
        , environmentFunctions = []
        , expectedValue = Pine.valueFromString "argument_1"
        }
      )
    , ( "partial application only for closure - one original param"
      , { functionInnerExpr =
            ElmInteractive.LetBlockExpression
                { declarations =
                    [ ( "decl_from_let"
                      , ElmInteractive.FunctionExpression
                            [ [ ( "final_func_param_0"
                                , []
                                )
                              ]
                            ]
                            (ElmInteractive.FunctionApplicationExpression
                                (ReferenceExpression "final_func_param_0")
                                [ ElmInteractive.LiteralExpression (Pine.valueFromString "literal_0")
                                ]
                            )
                      )
                    , ( "closure_func"
                      , ElmInteractive.FunctionExpression
                            [ [ ( "closure_func_param_0"
                                , []
                                )
                              ]
                            ]
                            (ElmInteractive.ListExpression
                                [ ElmInteractive.ReferenceExpression "closure_func_param_0"
                                , ElmInteractive.ReferenceExpression "param_0"
                                ]
                            )
                      )
                    ]
                , expression =
                    ElmInteractive.FunctionApplicationExpression
                        (ReferenceExpression "decl_from_let")
                        [ ElmInteractive.ReferenceExpression "closure_func"
                        ]
                }
        , functionParams =
            [ [ ( "param_0", [] )
              ]
            ]
        , arguments =
            [ Pine.valueFromString "argument_0"
            ]
        , environmentFunctions = []
        , expectedValue =
            Pine.ListValue
                [ Pine.valueFromString "literal_0"
                , Pine.valueFromString "argument_0"
                ]
        }
      )
    , ( "let block returning from other outside decl"
      , { functionInnerExpr =
            ElmInteractive.LetBlockExpression
                { declarations =
                    [ ( "decl_from_let"
                      , ElmInteractive.ReferenceExpression "env_func"
                      )
                    ]
                , expression = ElmInteractive.ReferenceExpression "decl_from_let"
                }
        , functionParams =
            [ [ ( "param_0", [] ) ]
            ]
        , arguments = [ Pine.valueFromString "argument_0" ]
        , environmentFunctions =
            [ ( "env_func"
              , { functionInnerExpr = ElmInteractive.LiteralExpression (Pine.valueFromString "const_from_env_func")
                , functionParams = []
                }
              )
            ]
        , expectedValue = Pine.valueFromString "const_from_env_func"
        }
      )
    , ( "Partial application - two - return literal"
      , { functionInnerExpr =
            ElmInteractive.FunctionApplicationExpression
                (ElmInteractive.ReferenceExpression "second_function_partially_applied")
                [ ElmInteractive.LiteralExpression (Pine.valueFromString "second_arg")
                ]
        , functionParams = []
        , arguments = []
        , environmentFunctions =
            [ ( "second_function"
              , { functionInnerExpr = ElmInteractive.LiteralExpression (Pine.valueFromString "constant-literal")
                , functionParams =
                    [ [ ( "second_function_param_alfa", [] ) ]
                    , [ ( "second_function_param_beta", [] ) ]
                    ]
                }
              )
            , ( "second_function_partially_applied"
              , { functionInnerExpr =
                    ElmInteractive.FunctionApplicationExpression
                        (ElmInteractive.ReferenceExpression "second_function")
                        [ ElmInteractive.LiteralExpression (Pine.valueFromString "first_arg")
                        ]
                , functionParams = []
                }
              )
            ]
        , expectedValue = Pine.valueFromString "constant-literal"
        }
      )
    ]
        |> List.indexedMap
            (\testCaseIndex ( testCaseName, testCase ) ->
                Test.test ("Case " ++ String.fromInt testCaseIndex ++ " - " ++ testCaseName) <|
                    \_ ->
                        let
                            declarationBlockOuterExprFromFunctionParamsAndInnerExpr params innerExpr =
                                ElmInteractive.FunctionExpression params innerExpr

                            environmentFunctions =
                                testCase.environmentFunctions
                                    |> List.map
                                        (Tuple.mapSecond
                                            (\functionRecord ->
                                                declarationBlockOuterExprFromFunctionParamsAndInnerExpr
                                                    functionRecord.functionParams
                                                    functionRecord.functionInnerExpr
                                            )
                                        )

                            compilationConfig =
                                { moduleAliases = Dict.empty
                                , availableModules = Dict.empty
                                , availableDeclarations = Dict.empty
                                , elmValuesToExposeToGlobal = Dict.empty
                                }

                            emptyEmitStack =
                                { moduleImports = ElmInteractive.moduleImportsFromCompilationStack [] compilationConfig
                                , declarationsDependencies = Dict.empty
                                , environmentFunctions = []
                                , environmentDeconstructions = Dict.empty
                                }

                            rootAsExpression =
                                declarationBlockOuterExprFromFunctionParamsAndInnerExpr
                                    testCase.functionParams
                                    testCase.functionInnerExpr

                            emitClosureResult =
                                ElmInteractive.emitExpressionInDeclarationBlock
                                    emptyEmitStack
                                    (Dict.fromList environmentFunctions)
                                    rootAsExpression
                        in
                        emitClosureResult
                            |> Result.andThen
                                ((\partialApplicable ->
                                    ElmInteractive.partialApplicationExpressionFromListOfArguments
                                        (testCase.arguments |> List.map Pine.LiteralExpression)
                                        partialApplicable
                                        |> Pine.evaluateExpression { environment = Pine.ListValue [] }
                                        |> Result.mapError Pine.displayStringFromPineError
                                 )
                                    >> Result.map (Expect.equal testCase.expectedValue)
                                )
                            |> Result.Extra.unpack Expect.fail identity
            )
        |> Test.describe "emit closure expression"
