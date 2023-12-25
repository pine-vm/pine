module ElmCompilerTests exposing (..)

import BigInt
import Dict
import ElmCompiler
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
            , environment = ElmCompiler.pineKernel_ListHead_Pine Pine.EnvironmentExpression
            }
        , expected =
            ElmCompiler.pineKernel_ListHead_Pine Pine.EnvironmentExpression
        , additionalTestEnvironments = []
        }
      )
    , ( "reducible - skip 2"
      , { original =
            { expression =
                Pine.EnvironmentExpression
                    |> ElmCompiler.listSkipExpression_Pine 2
                    |> ElmCompiler.pineKernel_ListHead_Pine
                    |> Pine.encodeExpressionAsValue
                    |> Pine.LiteralExpression
            , environment =
                Pine.ListExpression
                    [ Pine.EnvironmentExpression
                        |> ElmCompiler.listSkipExpression_Pine 4
                    , Pine.EnvironmentExpression
                        |> ElmCompiler.listSkipExpression_Pine 3
                    , Pine.EnvironmentExpression
                        |> ElmCompiler.listSkipExpression_Pine 3
                    , Pine.EnvironmentExpression
                        |> ElmCompiler.listSkipExpression_Pine 2
                    ]
            }
        , expected =
            Pine.EnvironmentExpression
                |> ElmCompiler.listSkipExpression_Pine 3
        , additionalTestEnvironments = []
        }
      )
    , ( "reducible - skip 1 (skip 2)"
      , { original =
            { expression =
                Pine.EnvironmentExpression
                    |> ElmCompiler.listSkipExpression_Pine 2
                    |> ElmCompiler.pineKernel_ListHead_Pine
                    |> ElmCompiler.listSkipExpression_Pine 1
                    |> ElmCompiler.pineKernel_ListHead_Pine
                    |> Pine.encodeExpressionAsValue
                    |> Pine.LiteralExpression
            , environment =
                Pine.ListExpression
                    [ Pine.ListExpression
                        [ Pine.ListExpression []
                        , Pine.EnvironmentExpression
                            |> ElmCompiler.listSkipExpression_Pine 4
                        ]
                    , Pine.ListExpression
                        [ Pine.ListExpression []
                        , Pine.EnvironmentExpression
                            |> ElmCompiler.listSkipExpression_Pine 3
                        ]
                    , Pine.ListExpression
                        [ Pine.ListExpression []
                        , Pine.EnvironmentExpression
                            |> ElmCompiler.listSkipExpression_Pine 3
                        ]
                    , Pine.ListExpression
                        [ Pine.ListExpression []
                        , Pine.EnvironmentExpression
                            |> ElmCompiler.listSkipExpression_Pine 2
                        ]
                    ]
            }
        , expected =
            Pine.EnvironmentExpression
                |> ElmCompiler.listSkipExpression_Pine 3
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
                            |> ElmCompiler.attemptReduceDecodeAndEvaluateExpressionRecursive { maxDepth = 4 }
                            |> Expect.equal testCase.expected
            )
        |> Test.describe "Compiler reduces decode and eval expression"


emitClosureExpressionTests : Test.Test
emitClosureExpressionTests =
    [ ( "Zero parameters"
      , { functionInnerExpr =
            ElmCompiler.LiteralExpression (Pine.valueFromString "test")
        , functionParams = []
        , arguments = []
        , environmentFunctions = []
        , expectedValue = Pine.valueFromString "test"
        }
      )
    , ( "Zero parameters - return from function with one param"
      , { functionInnerExpr =
            ElmCompiler.FunctionApplicationExpression
                (ElmCompiler.ReferenceExpression "repeat_three_times")
                [ ElmCompiler.LiteralExpression (Pine.valueFromString "argument_alfa") ]
        , functionParams = []
        , arguments = []
        , environmentFunctions =
            [ ( "repeat_three_times"
              , { functionInnerExpr =
                    ElmCompiler.ListExpression
                        [ ElmCompiler.ReferenceExpression "param_name"
                        , ElmCompiler.ReferenceExpression "param_name"
                        , ElmCompiler.ReferenceExpression "param_name"
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
            ElmCompiler.FunctionApplicationExpression
                (ElmCompiler.ReferenceExpression "return_constant_literal")
                []
        , functionParams = []
        , arguments = []
        , environmentFunctions =
            [ ( "return_constant_literal"
              , { functionInnerExpr =
                    ElmCompiler.LiteralExpression (Pine.valueFromString "constant")
                , functionParams = []
                }
              )
            ]
        , expectedValue = Pine.valueFromString "constant"
        }
      )
    , ( "Zero parameters - return literal from function with zero param - twice"
      , { functionInnerExpr =
            ElmCompiler.FunctionApplicationExpression
                (ElmCompiler.ReferenceExpression "return_constant_literal_first")
                []
        , functionParams = []
        , arguments = []
        , environmentFunctions =
            [ ( "return_constant_literal_first"
              , { functionInnerExpr =
                    ElmCompiler.FunctionApplicationExpression
                        (ElmCompiler.ReferenceExpression "return_constant_literal_second")
                        []
                , functionParams = []
                }
              )
            , ( "return_constant_literal_second"
              , { functionInnerExpr =
                    ElmCompiler.LiteralExpression (Pine.valueFromString "constant")
                , functionParams = []
                }
              )
            ]
        , expectedValue = Pine.valueFromString "constant"
        }
      )
    , ( "One parameter - literal"
      , { functionInnerExpr =
            ElmCompiler.LiteralExpression (Pine.valueFromString "test-literal")
        , functionParams = [ [ ( "param-name", [] ) ] ]
        , arguments = [ Pine.valueFromString "test-123" ]
        , environmentFunctions = []
        , expectedValue = Pine.valueFromString "test-literal"
        }
      )
    , ( "One parameter - reference"
      , { functionInnerExpr =
            ElmCompiler.ReferenceExpression "param-name"
        , functionParams = [ [ ( "param-name", [] ) ] ]
        , arguments = [ Pine.valueFromString "test-345" ]
        , environmentFunctions = []
        , expectedValue = Pine.valueFromString "test-345"
        }
      )
    , ( "One parameter - reference decons tuple second"
      , { functionInnerExpr =
            ElmCompiler.ReferenceExpression "param-name"
        , functionParams =
            [ [ ( "param-name"
                , [ ElmCompiler.ListItemDeconstruction 1 ]
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
            ElmCompiler.FunctionApplicationExpression
                (ElmCompiler.ReferenceExpression "repeat_help")
                [ ElmCompiler.ListExpression
                    [ ElmCompiler.LiteralExpression (Pine.ListValue [])
                    , ElmCompiler.ReferenceExpression "count"
                    , ElmCompiler.ReferenceExpression "value"
                    ]
                ]
        , functionParams =
            [ [ ( "count"
                , [ ElmCompiler.ListItemDeconstruction 0 ]
                )
              , ( "value"
                , [ ElmCompiler.ListItemDeconstruction 1 ]
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
                    ElmCompiler.ConditionalExpression
                        { condition =
                            ElmCompiler.KernelApplicationExpression
                                { functionName = "is_sorted_ascending_int"
                                , argument =
                                    ElmCompiler.ListExpression
                                        [ ElmCompiler.ReferenceExpression "remainingCount"
                                        , ElmCompiler.LiteralExpression (Pine.valueFromBigInt (BigInt.fromInt 0))
                                        ]
                                }
                        , ifTrue =
                            ElmCompiler.ReferenceExpression "result"
                        , ifFalse =
                            ElmCompiler.FunctionApplicationExpression
                                (ElmCompiler.ReferenceExpression "repeat_help")
                                [ ElmCompiler.ListExpression
                                    [ ElmCompiler.KernelApplicationExpression
                                        { functionName = "concat"
                                        , argument =
                                            ElmCompiler.ListExpression
                                                [ ElmCompiler.ListExpression
                                                    [ ElmCompiler.ReferenceExpression "value"
                                                    ]
                                                , ElmCompiler.ReferenceExpression "result"
                                                ]
                                        }
                                    , ElmCompiler.KernelApplicationExpression
                                        { functionName = "add_int"
                                        , argument =
                                            ElmCompiler.ListExpression
                                                [ ElmCompiler.ReferenceExpression "remainingCount"
                                                , ElmCompiler.LiteralExpression (Pine.valueFromBigInt (BigInt.fromInt -1))
                                                ]
                                        }
                                    , ElmCompiler.ReferenceExpression "value"
                                    ]
                                ]
                        }
                , functionParams =
                    [ [ ( "result"
                        , [ ElmCompiler.ListItemDeconstruction 0 ]
                        )
                      , ( "remainingCount"
                        , [ ElmCompiler.ListItemDeconstruction 1 ]
                        )
                      , ( "value"
                        , [ ElmCompiler.ListItemDeconstruction 2 ]
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
            ElmCompiler.FunctionApplicationExpression
                (ElmCompiler.ReferenceExpression "repeat_help")
                [ ElmCompiler.ListExpression
                    [ ElmCompiler.LiteralExpression (Pine.ListValue [])
                    , ElmCompiler.ReferenceExpression "count"
                    , ElmCompiler.ReferenceExpression "value"
                    ]
                ]
        , functionParams =
            [ [ ( "count"
                , [ ElmCompiler.ListItemDeconstruction 0 ]
                )
              , ( "value"
                , [ ElmCompiler.ListItemDeconstruction 1 ]
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
                    ElmCompiler.KernelApplicationExpression
                        { functionName = "is_sorted_ascending_int"
                        , argument =
                            ElmCompiler.ListExpression
                                [ ElmCompiler.ReferenceExpression "num"
                                , ElmCompiler.LiteralExpression (Pine.valueFromBigInt (BigInt.fromInt 0))
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
                    ElmCompiler.ConditionalExpression
                        { condition =
                            ElmCompiler.FunctionApplicationExpression
                                (ElmCompiler.ReferenceExpression "is_less_than_or_equal_to_zero")
                                [ ElmCompiler.ReferenceExpression "remainingCount"
                                ]
                        , ifTrue =
                            ElmCompiler.ReferenceExpression "result"
                        , ifFalse =
                            ElmCompiler.FunctionApplicationExpression
                                (ElmCompiler.ReferenceExpression "repeat_help")
                                [ ElmCompiler.ListExpression
                                    [ ElmCompiler.KernelApplicationExpression
                                        { functionName = "concat"
                                        , argument =
                                            ElmCompiler.ListExpression
                                                [ ElmCompiler.ListExpression
                                                    [ ElmCompiler.ReferenceExpression "value"
                                                    ]
                                                , ElmCompiler.ReferenceExpression "result"
                                                ]
                                        }
                                    , ElmCompiler.KernelApplicationExpression
                                        { functionName = "add_int"
                                        , argument =
                                            ElmCompiler.ListExpression
                                                [ ElmCompiler.ReferenceExpression "remainingCount"
                                                , ElmCompiler.LiteralExpression (Pine.valueFromBigInt (BigInt.fromInt -1))
                                                ]
                                        }
                                    , ElmCompiler.ReferenceExpression "value"
                                    ]
                                ]
                        }
                , functionParams =
                    [ [ ( "result"
                        , [ ElmCompiler.ListItemDeconstruction 0 ]
                        )
                      , ( "remainingCount"
                        , [ ElmCompiler.ListItemDeconstruction 1 ]
                        )
                      , ( "value"
                        , [ ElmCompiler.ListItemDeconstruction 2 ]
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
      , { functionInnerExpr = ElmCompiler.LiteralExpression (Pine.valueFromString "constant-literal")
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
      , { functionInnerExpr = ElmCompiler.ReferenceExpression "param_beta"
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
      , { functionInnerExpr = ElmCompiler.ReferenceExpression "param_alfa"
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
      , { functionInnerExpr = ElmCompiler.LiteralExpression (Pine.valueFromString "constant-literal")
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
      , { functionInnerExpr = ElmCompiler.ReferenceExpression "param_gamma"
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
      , { functionInnerExpr = ElmCompiler.ReferenceExpression "param_beta"
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
      , { functionInnerExpr = ElmCompiler.ReferenceExpression "param_alfa"
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
            ElmCompiler.FunctionApplicationExpression
                (ElmCompiler.ReferenceExpression "repeat_three_times")
                [ ElmCompiler.ReferenceExpression "param_alfa" ]
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
                    ElmCompiler.ListExpression
                        [ ElmCompiler.ReferenceExpression "param_name"
                        , ElmCompiler.ReferenceExpression "param_name"
                        , ElmCompiler.ReferenceExpression "param_name"
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
            ElmCompiler.FunctionApplicationExpression
                (ElmCompiler.ReferenceExpression "repeat_three_times")
                [ ElmCompiler.ReferenceExpression "param_alfa"
                , ElmCompiler.ReferenceExpression "param_beta"
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
                    ElmCompiler.ListExpression
                        [ ElmCompiler.ReferenceExpression "param_name_a"
                        , ElmCompiler.ReferenceExpression "param_name_a"
                        , ElmCompiler.ReferenceExpression "param_name_a"
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
            ElmCompiler.FunctionApplicationExpression
                (ElmCompiler.ReferenceExpression "repeat_help")
                [ ElmCompiler.LiteralExpression (Pine.ListValue [])
                , ElmCompiler.ListExpression
                    [ ElmCompiler.ReferenceExpression "count"
                    , ElmCompiler.ReferenceExpression "value"
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
                    ElmCompiler.ConditionalExpression
                        { condition =
                            ElmCompiler.KernelApplicationExpression
                                { functionName = "is_sorted_ascending_int"
                                , argument =
                                    ElmCompiler.ListExpression
                                        [ ElmCompiler.ReferenceExpression "remainingCount"
                                        , ElmCompiler.LiteralExpression (Pine.valueFromBigInt (BigInt.fromInt 0))
                                        ]
                                }
                        , ifTrue = ElmCompiler.ReferenceExpression "result"
                        , ifFalse =
                            ElmCompiler.FunctionApplicationExpression
                                (ElmCompiler.ReferenceExpression "repeat_help")
                                [ ElmCompiler.KernelApplicationExpression
                                    { functionName = "concat"
                                    , argument =
                                        ElmCompiler.ListExpression
                                            [ ElmCompiler.ListExpression
                                                [ ElmCompiler.ReferenceExpression "value"
                                                ]
                                            , ElmCompiler.ReferenceExpression "result"
                                            ]
                                    }
                                , ElmCompiler.ListExpression
                                    [ ElmCompiler.KernelApplicationExpression
                                        { functionName = "add_int"
                                        , argument =
                                            ElmCompiler.ListExpression
                                                [ ElmCompiler.ReferenceExpression "remainingCount"
                                                , ElmCompiler.LiteralExpression (Pine.valueFromBigInt (BigInt.fromInt -1))
                                                ]
                                        }
                                    , ElmCompiler.ReferenceExpression "value"
                                    ]
                                ]
                        }
                , functionParams =
                    [ [ ( "result", [] )
                      ]
                    , [ ( "remainingCount"
                        , [ ElmCompiler.ListItemDeconstruction 0 ]
                        )
                      , ( "value"
                        , [ ElmCompiler.ListItemDeconstruction 1 ]
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
            ElmCompiler.FunctionApplicationExpression
                (ElmCompiler.ReferenceExpression "repeat_help")
                (List.map ElmCompiler.LiteralExpression
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
                    ElmCompiler.ConditionalExpression
                        { condition =
                            ElmCompiler.KernelApplicationExpression
                                { functionName = "is_sorted_ascending_int"
                                , argument =
                                    ElmCompiler.ListExpression
                                        [ ElmCompiler.ReferenceExpression "remainingCount"
                                        , ElmCompiler.LiteralExpression (Pine.valueFromBigInt (BigInt.fromInt 0))
                                        ]
                                }
                        , ifTrue =
                            ElmCompiler.ReferenceExpression "result"
                        , ifFalse =
                            ElmCompiler.FunctionApplicationExpression
                                (ElmCompiler.ReferenceExpression "repeat_help")
                                [ ElmCompiler.KernelApplicationExpression
                                    { functionName = "concat"
                                    , argument =
                                        ElmCompiler.ListExpression
                                            [ ElmCompiler.ListExpression
                                                [ ElmCompiler.ReferenceExpression "value"
                                                ]
                                            , ElmCompiler.ReferenceExpression "result"
                                            ]
                                    }
                                , ElmCompiler.KernelApplicationExpression
                                    { functionName = "add_int"
                                    , argument =
                                        ElmCompiler.ListExpression
                                            [ ElmCompiler.ReferenceExpression "remainingCount"
                                            , ElmCompiler.LiteralExpression (Pine.valueFromBigInt (BigInt.fromInt -1))
                                            ]
                                    }
                                , ElmCompiler.ReferenceExpression "value"
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
            ElmCompiler.LetBlockExpression
                { declarations =
                    [ ( "decl_from_let"
                      , ElmCompiler.LiteralExpression (Pine.valueFromString "constant_in_let")
                      )
                    ]
                , expression = ElmCompiler.ReferenceExpression "decl_from_let"
                }
        , functionParams = []
        , arguments = []
        , environmentFunctions = []
        , expectedValue = Pine.valueFromString "constant_in_let"
        }
      )
    , ( "let block returning from other decl in same block"
      , { functionInnerExpr =
            ElmCompiler.LetBlockExpression
                { declarations =
                    [ ( "decl_from_let"
                      , ElmCompiler.ReferenceExpression "other_decl_from_let"
                      )
                    , ( "other_decl_from_let"
                      , ElmCompiler.LiteralExpression (Pine.valueFromString "constant_in_let")
                      )
                    ]
                , expression = ElmCompiler.ReferenceExpression "decl_from_let"
                }
        , functionParams = []
        , arguments = []
        , environmentFunctions = []
        , expectedValue = Pine.valueFromString "constant_in_let"
        }
      )
    , ( "let block returning only parent function arg"
      , { functionInnerExpr =
            ElmCompiler.LetBlockExpression
                { declarations =
                    [ ( "decl_from_let"
                      , ElmCompiler.ReferenceExpression "param_0"
                      )
                    ]
                , expression = ElmCompiler.ReferenceExpression "decl_from_let"
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
            ElmCompiler.LetBlockExpression
                { declarations =
                    [ ( "decl_from_let"
                      , ElmCompiler.ReferenceExpression "param_0"
                      )
                    ]
                , expression =
                    ElmCompiler.LetBlockExpression
                        { declarations =
                            [ ( "decl_from_let_inner"
                              , ElmCompiler.ReferenceExpression "decl_from_let"
                              )
                            ]
                        , expression = ElmCompiler.ReferenceExpression "decl_from_let_inner"
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
            ElmCompiler.LetBlockExpression
                { declarations =
                    [ ( "decl_from_let"
                      , ElmCompiler.ReferenceExpression "param_1"
                      )
                    ]
                , expression = ElmCompiler.ReferenceExpression "decl_from_let"
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
            ElmCompiler.LetBlockExpression
                { declarations =
                    [ ( "decl_from_let"
                      , ElmCompiler.FunctionExpression
                            [ [ ( "final_func_param_0"
                                , []
                                )
                              ]
                            ]
                            (ElmCompiler.FunctionApplicationExpression
                                (ElmCompiler.ReferenceExpression "final_func_param_0")
                                [ ElmCompiler.LiteralExpression (Pine.valueFromString "literal_0")
                                ]
                            )
                      )
                    , ( "closure_func"
                      , ElmCompiler.FunctionExpression
                            [ [ ( "closure_func_param_0"
                                , []
                                )
                              ]
                            ]
                            (ElmCompiler.ListExpression
                                [ ElmCompiler.ReferenceExpression "closure_func_param_0"
                                , ElmCompiler.ReferenceExpression "param_0"
                                ]
                            )
                      )
                    ]
                , expression =
                    ElmCompiler.FunctionApplicationExpression
                        (ElmCompiler.ReferenceExpression "decl_from_let")
                        [ ElmCompiler.ReferenceExpression "closure_func"
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
            ElmCompiler.LetBlockExpression
                { declarations =
                    [ ( "decl_from_let"
                      , ElmCompiler.ReferenceExpression "env_func"
                      )
                    ]
                , expression = ElmCompiler.ReferenceExpression "decl_from_let"
                }
        , functionParams =
            [ [ ( "param_0", [] ) ]
            ]
        , arguments = [ Pine.valueFromString "argument_0" ]
        , environmentFunctions =
            [ ( "env_func"
              , { functionInnerExpr = ElmCompiler.LiteralExpression (Pine.valueFromString "const_from_env_func")
                , functionParams = []
                }
              )
            ]
        , expectedValue = Pine.valueFromString "const_from_env_func"
        }
      )
    , ( "Partial application - two - return literal"
      , { functionInnerExpr =
            ElmCompiler.FunctionApplicationExpression
                (ElmCompiler.ReferenceExpression "second_function_partially_applied")
                [ ElmCompiler.LiteralExpression (Pine.valueFromString "second_arg")
                ]
        , functionParams = []
        , arguments = []
        , environmentFunctions =
            [ ( "second_function"
              , { functionInnerExpr = ElmCompiler.LiteralExpression (Pine.valueFromString "constant-literal")
                , functionParams =
                    [ [ ( "second_function_param_alfa", [] ) ]
                    , [ ( "second_function_param_beta", [] ) ]
                    ]
                }
              )
            , ( "second_function_partially_applied"
              , { functionInnerExpr =
                    ElmCompiler.FunctionApplicationExpression
                        (ElmCompiler.ReferenceExpression "second_function")
                        [ ElmCompiler.LiteralExpression (Pine.valueFromString "first_arg")
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
                                ElmCompiler.FunctionExpression params innerExpr

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
                                { moduleImports = ElmCompiler.moduleImportsFromCompilationStack [] compilationConfig
                                , declarationsDependencies = Dict.empty
                                , environmentFunctions = []
                                , environmentDeconstructions = Dict.empty
                                }

                            rootAsExpression =
                                declarationBlockOuterExprFromFunctionParamsAndInnerExpr
                                    testCase.functionParams
                                    testCase.functionInnerExpr

                            emitClosureResult =
                                ElmCompiler.emitExpressionInDeclarationBlock
                                    emptyEmitStack
                                    (Dict.fromList environmentFunctions)
                                    rootAsExpression
                        in
                        emitClosureResult
                            |> Result.andThen
                                ((\partialApplicable ->
                                    ElmCompiler.partialApplicationExpressionFromListOfArguments
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
