module ElmCompiler2024Tests exposing (..)

import BigInt
import Expect
import FirCompiler2024
import Pine2024
import Test


type alias ReduceParseAndEvalTestCase =
    { original : ParseAndEvalExpressionStruct
    , expected : Pine2024.Expression
    , additionalTestEnvironments : List Pine2024.Value
    }


type alias ParseAndEvalExpressionStruct =
    ( Pine2024.Expression, Pine2024.Expression )


standardTestEnvironments : List Pine2024.Value
standardTestEnvironments =
    [ Pine2024.ListValue []
    , List.range 0 4
        |> List.map (BigInt.fromInt >> Pine2024.blobValueFromBigInt >> Pine2024.BlobValue)
        |> Pine2024.ListValue
    , List.range 3 7
        |> List.map
            (\offsetA ->
                List.range (offsetA * 13) (offsetA * 13 + 5)
                    |> List.map
                        (\offsetB ->
                            List.range (offsetB * 91) (offsetB * 91 + 5)
                                |> List.map (BigInt.fromInt >> Pine2024.blobValueFromBigInt >> Pine2024.BlobValue)
                                |> Pine2024.ListValue
                        )
                    |> Pine2024.ListValue
            )
        |> Pine2024.ListValue
    ]


compiler_reduces_decode_and_eval_test_cases : List ( String, ReduceParseAndEvalTestCase )
compiler_reduces_decode_and_eval_test_cases =
    [ ( "simple reducible - literal"
      , { original =
            ( Pine2024.LiteralExpression (Pine2024.valueFromString "test")
                |> Pine2024.encodeExpressionAsValue
                |> Pine2024.LiteralExpression
            , Pine2024.ListExpression []
            )
        , expected = Pine2024.LiteralExpression (Pine2024.valueFromString "test")
        , additionalTestEnvironments = []
        }
      )
    , ( "simple reducible - head"
      , { original =
            ( Pine2024.EnvironmentExpression
                |> Pine2024.encodeExpressionAsValue
                |> Pine2024.LiteralExpression
            , FirCompiler2024.pineKernel_Head_Pine Pine2024.EnvironmentExpression
            )
        , expected =
            FirCompiler2024.pineKernel_Head_Pine Pine2024.EnvironmentExpression
        , additionalTestEnvironments = []
        }
      )
    , ( "reducible - skip 2"
      , { original =
            ( Pine2024.EnvironmentExpression
                |> FirCompiler2024.listSkipExpression_Pine 2
                |> FirCompiler2024.pineKernel_Head_Pine
                |> Pine2024.encodeExpressionAsValue
                |> Pine2024.LiteralExpression
            , Pine2024.ListExpression
                [ Pine2024.EnvironmentExpression
                    |> FirCompiler2024.listSkipExpression_Pine 4
                , Pine2024.EnvironmentExpression
                    |> FirCompiler2024.listSkipExpression_Pine 3
                , Pine2024.EnvironmentExpression
                    |> FirCompiler2024.listSkipExpression_Pine 3
                , Pine2024.EnvironmentExpression
                    |> FirCompiler2024.listSkipExpression_Pine 2
                ]
            )
        , expected =
            Pine2024.EnvironmentExpression
                |> FirCompiler2024.listSkipExpression_Pine 3
        , additionalTestEnvironments = []
        }
      )
    , ( "reducible - skip 1 (skip 2)"
      , { original =
            ( Pine2024.EnvironmentExpression
                |> FirCompiler2024.listSkipExpression_Pine 2
                |> FirCompiler2024.pineKernel_Head_Pine
                |> FirCompiler2024.listSkipExpression_Pine 1
                |> FirCompiler2024.pineKernel_Head_Pine
                |> Pine2024.encodeExpressionAsValue
                |> Pine2024.LiteralExpression
            , Pine2024.ListExpression
                [ Pine2024.ListExpression
                    [ Pine2024.ListExpression []
                    , Pine2024.EnvironmentExpression
                        |> FirCompiler2024.listSkipExpression_Pine 4
                    ]
                , Pine2024.ListExpression
                    [ Pine2024.ListExpression []
                    , Pine2024.EnvironmentExpression
                        |> FirCompiler2024.listSkipExpression_Pine 3
                    ]
                , Pine2024.ListExpression
                    [ Pine2024.ListExpression []
                    , Pine2024.EnvironmentExpression
                        |> FirCompiler2024.listSkipExpression_Pine 3
                    ]
                , Pine2024.ListExpression
                    [ Pine2024.ListExpression []
                    , Pine2024.EnvironmentExpression
                        |> FirCompiler2024.listSkipExpression_Pine 2
                    ]
                ]
            )
        , expected =
            Pine2024.EnvironmentExpression
                |> FirCompiler2024.listSkipExpression_Pine 3
        , additionalTestEnvironments = []
        }
      )
    , ( "simple irreducible"
      , { original =
            ( Pine2024.EnvironmentExpression
            , Pine2024.EnvironmentExpression
            )
        , expected =
            Pine2024.ParseAndEvalExpression
                Pine2024.EnvironmentExpression
                Pine2024.EnvironmentExpression
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
                        (\envIndex environment ->
                            Test.test ("Environment " ++ String.fromInt envIndex) <|
                                \_ ->
                                    let
                                        ( origEncoded, origEnv ) =
                                            testCase.original
                                    in
                                    Pine2024.ParseAndEvalExpression origEncoded origEnv
                                        |> Pine2024.evaluateExpression (Pine2024.EvalEnvironment environment)
                                        |> Expect.equal
                                            (Pine2024.evaluateExpression
                                                (Pine2024.EvalEnvironment environment)
                                                testCase.expected
                                            )
                        )
                    |> Test.describe ("Expression " ++ String.fromInt testCaseIndex ++ " - " ++ testCaseName)
            )
        |> Test.describe "Test cases - Compiler reduces decode and eval expression"


compilerReducesParseAndEvalExpression : Test.Test
compilerReducesParseAndEvalExpression =
    compiler_reduces_decode_and_eval_test_cases
        |> List.indexedMap
            (\testCaseIndex ( testCaseName, testCase ) ->
                Test.test ("Expression " ++ String.fromInt testCaseIndex ++ " - " ++ testCaseName) <|
                    \_ ->
                        testCase.original
                            |> FirCompiler2024.attemptReduceParseAndEvalExpressionRecursive 4
                            |> Expect.equal testCase.expected
            )
        |> Test.describe "Compiler reduces decode and eval expression"


emitClosureExpressionTests : Test.Test
emitClosureExpressionTests =
    [ ( "Zero parameters"
      , { functionInnerExpr =
            FirCompiler2024.LiteralExpression (Pine2024.valueFromString "test")
        , functionParams = []
        , arguments = []
        , environmentFunctions = []
        , expectedValue = Pine2024.valueFromString "test"
        }
      )
    , ( "Zero parameters - return from function with one param"
      , { functionInnerExpr =
            FirCompiler2024.FunctionApplicationExpression
                (FirCompiler2024.ReferenceExpression [] "repeat_three_times")
                [ FirCompiler2024.LiteralExpression (Pine2024.valueFromString "argument_alfa") ]
        , functionParams = []
        , arguments = []
        , environmentFunctions =
            [ ( "repeat_three_times"
              , { functionInnerExpr =
                    FirCompiler2024.ListExpression
                        [ FirCompiler2024.ReferenceExpression [] "param_name"
                        , FirCompiler2024.ReferenceExpression [] "param_name"
                        , FirCompiler2024.ReferenceExpression [] "param_name"
                        ]
                , functionParams =
                    [ [ ( "param_name", [] ) ] ]
                }
              )
            ]
        , expectedValue =
            Pine2024.ListValue
                [ Pine2024.valueFromString "argument_alfa"
                , Pine2024.valueFromString "argument_alfa"
                , Pine2024.valueFromString "argument_alfa"
                ]
        }
      )
    , ( "Zero parameters - return literal from function with zero param - once"
      , { functionInnerExpr =
            FirCompiler2024.FunctionApplicationExpression
                (FirCompiler2024.ReferenceExpression [] "return_constant_literal")
                []
        , functionParams = []
        , arguments = []
        , environmentFunctions =
            [ ( "return_constant_literal"
              , { functionInnerExpr =
                    FirCompiler2024.LiteralExpression (Pine2024.valueFromString "constant")
                , functionParams = []
                }
              )
            ]
        , expectedValue = Pine2024.valueFromString "constant"
        }
      )
    , ( "Zero parameters - return literal from function with zero param - twice"
      , { functionInnerExpr =
            FirCompiler2024.FunctionApplicationExpression
                (FirCompiler2024.ReferenceExpression [] "return_constant_literal_first")
                []
        , functionParams = []
        , arguments = []
        , environmentFunctions =
            [ ( "return_constant_literal_first"
              , { functionInnerExpr =
                    FirCompiler2024.FunctionApplicationExpression
                        (FirCompiler2024.ReferenceExpression [] "return_constant_literal_second")
                        []
                , functionParams = []
                }
              )
            , ( "return_constant_literal_second"
              , { functionInnerExpr =
                    FirCompiler2024.LiteralExpression (Pine2024.valueFromString "constant")
                , functionParams = []
                }
              )
            ]
        , expectedValue = Pine2024.valueFromString "constant"
        }
      )
    , ( "One parameter - literal"
      , { functionInnerExpr =
            FirCompiler2024.LiteralExpression (Pine2024.valueFromString "test-literal")
        , functionParams = [ [ ( "param-name", [] ) ] ]
        , arguments = [ Pine2024.valueFromString "test-123" ]
        , environmentFunctions = []
        , expectedValue = Pine2024.valueFromString "test-literal"
        }
      )
    , ( "One parameter - reference"
      , { functionInnerExpr =
            FirCompiler2024.ReferenceExpression [] "param-name"
        , functionParams = [ [ ( "param-name", [] ) ] ]
        , arguments = [ Pine2024.valueFromString "test-345" ]
        , environmentFunctions = []
        , expectedValue = Pine2024.valueFromString "test-345"
        }
      )
    , ( "One parameter - reference decons tuple second"
      , { functionInnerExpr =
            FirCompiler2024.ReferenceExpression [] "param-name"
        , functionParams =
            [ [ ( "param-name"
                , [ FirCompiler2024.ListItemDeconstruction 1 ]
                )
              ]
            ]
        , arguments =
            [ Pine2024.ListValue
                [ Pine2024.ListValue []
                , Pine2024.valueFromString "test-456"
                ]
            ]
        , environmentFunctions = []
        , expectedValue = Pine2024.valueFromString "test-456"
        }
      )
    , ( "One parameter - repeat"
      , { functionInnerExpr =
            FirCompiler2024.FunctionApplicationExpression
                (FirCompiler2024.ReferenceExpression [] "repeat_help")
                [ FirCompiler2024.ListExpression
                    [ FirCompiler2024.LiteralExpression (Pine2024.ListValue [])
                    , FirCompiler2024.ReferenceExpression [] "count"
                    , FirCompiler2024.ReferenceExpression [] "value"
                    ]
                ]
        , functionParams =
            [ [ ( "count"
                , [ FirCompiler2024.ListItemDeconstruction 0 ]
                )
              , ( "value"
                , [ FirCompiler2024.ListItemDeconstruction 1 ]
                )
              ]
            ]
        , arguments =
            [ Pine2024.ListValue
                [ Pine2024.valueFromInt 3
                , Pine2024.valueFromString "test_elem"
                ]
            ]
        , environmentFunctions =
            [ ( "repeat_help"
              , { functionInnerExpr =
                    FirCompiler2024.ConditionalExpression
                        (FirCompiler2024.KernelApplicationExpression
                            "int_is_sorted_asc"
                            (FirCompiler2024.ListExpression
                                [ FirCompiler2024.ReferenceExpression [] "remainingCount"
                                , FirCompiler2024.LiteralExpression (Pine2024.valueFromInt 0)
                                ]
                            )
                        )
                        (FirCompiler2024.FunctionApplicationExpression
                            (FirCompiler2024.ReferenceExpression [] "repeat_help")
                            [ FirCompiler2024.ListExpression
                                [ FirCompiler2024.KernelApplicationExpression
                                    "concat"
                                    (FirCompiler2024.ListExpression
                                        [ FirCompiler2024.ListExpression
                                            [ FirCompiler2024.ReferenceExpression [] "value"
                                            ]
                                        , FirCompiler2024.ReferenceExpression [] "result"
                                        ]
                                    )
                                , FirCompiler2024.KernelApplicationExpression
                                    "int_add"
                                    (FirCompiler2024.ListExpression
                                        [ FirCompiler2024.ReferenceExpression [] "remainingCount"
                                        , FirCompiler2024.LiteralExpression (Pine2024.valueFromInt -1)
                                        ]
                                    )
                                , FirCompiler2024.ReferenceExpression [] "value"
                                ]
                            ]
                        )
                        (FirCompiler2024.ReferenceExpression [] "result")
                , functionParams =
                    [ [ ( "result"
                        , [ FirCompiler2024.ListItemDeconstruction 0 ]
                        )
                      , ( "remainingCount"
                        , [ FirCompiler2024.ListItemDeconstruction 1 ]
                        )
                      , ( "value"
                        , [ FirCompiler2024.ListItemDeconstruction 2 ]
                        )
                      ]
                    ]
                }
              )
            ]
        , expectedValue =
            Pine2024.valueFromString "test_elem"
                |> List.repeat 3
                |> Pine2024.ListValue
        }
      )
    , ( "One parameter - repeat - separate <= 0"
      , { functionInnerExpr =
            FirCompiler2024.FunctionApplicationExpression
                (FirCompiler2024.ReferenceExpression [] "repeat_help")
                [ FirCompiler2024.ListExpression
                    [ FirCompiler2024.LiteralExpression (Pine2024.ListValue [])
                    , FirCompiler2024.ReferenceExpression [] "count"
                    , FirCompiler2024.ReferenceExpression [] "value"
                    ]
                ]
        , functionParams =
            [ [ ( "count"
                , [ FirCompiler2024.ListItemDeconstruction 0 ]
                )
              , ( "value"
                , [ FirCompiler2024.ListItemDeconstruction 1 ]
                )
              ]
            ]
        , arguments =
            [ Pine2024.ListValue
                [ Pine2024.valueFromInt 3
                , Pine2024.valueFromString "test_elem"
                ]
            ]
        , environmentFunctions =
            [ ( "is_less_than_or_equal_to_zero"
              , { functionInnerExpr =
                    FirCompiler2024.KernelApplicationExpression
                        "int_is_sorted_asc"
                        (FirCompiler2024.ListExpression
                            [ FirCompiler2024.ReferenceExpression [] "num"
                            , FirCompiler2024.LiteralExpression (Pine2024.valueFromInt 0)
                            ]
                        )
                , functionParams =
                    [ [ ( "num", [] )
                      ]
                    ]
                }
              )
            , ( "repeat_help"
              , { functionInnerExpr =
                    FirCompiler2024.ConditionalExpression
                        (FirCompiler2024.FunctionApplicationExpression
                            (FirCompiler2024.ReferenceExpression [] "is_less_than_or_equal_to_zero")
                            [ FirCompiler2024.ReferenceExpression [] "remainingCount"
                            ]
                        )
                        (FirCompiler2024.FunctionApplicationExpression
                            (FirCompiler2024.ReferenceExpression [] "repeat_help")
                            [ FirCompiler2024.ListExpression
                                [ FirCompiler2024.KernelApplicationExpression
                                    "concat"
                                    (FirCompiler2024.ListExpression
                                        [ FirCompiler2024.ListExpression
                                            [ FirCompiler2024.ReferenceExpression [] "value"
                                            ]
                                        , FirCompiler2024.ReferenceExpression [] "result"
                                        ]
                                    )
                                , FirCompiler2024.KernelApplicationExpression
                                    "int_add"
                                    (FirCompiler2024.ListExpression
                                        [ FirCompiler2024.ReferenceExpression [] "remainingCount"
                                        , FirCompiler2024.LiteralExpression (Pine2024.valueFromInt -1)
                                        ]
                                    )
                                , FirCompiler2024.ReferenceExpression [] "value"
                                ]
                            ]
                        )
                        (FirCompiler2024.ReferenceExpression [] "result")
                , functionParams =
                    [ [ ( "result"
                        , [ FirCompiler2024.ListItemDeconstruction 0 ]
                        )
                      , ( "remainingCount"
                        , [ FirCompiler2024.ListItemDeconstruction 1 ]
                        )
                      , ( "value"
                        , [ FirCompiler2024.ListItemDeconstruction 2 ]
                        )
                      ]
                    ]
                }
              )
            ]
        , expectedValue =
            Pine2024.valueFromString "test_elem"
                |> List.repeat 3
                |> Pine2024.ListValue
        }
      )
    , ( "Two parameters - return literal"
      , { functionInnerExpr = FirCompiler2024.LiteralExpression (Pine2024.valueFromString "constant-literal")
        , functionParams =
            [ [ ( "param_alfa", [] )
              ]
            , [ ( "param_beta", [] )
              ]
            ]
        , arguments =
            [ Pine2024.valueFromString "argument_alfa"
            , Pine2024.valueFromString "argument_beta"
            ]
        , environmentFunctions = []
        , expectedValue = Pine2024.valueFromString "constant-literal"
        }
      )
    , ( "Two parameters - return second"
      , { functionInnerExpr = FirCompiler2024.ReferenceExpression [] "param_beta"
        , functionParams =
            [ [ ( "param_alfa", [] )
              ]
            , [ ( "param_beta", [] )
              ]
            ]
        , arguments =
            [ Pine2024.valueFromString "argument_alfa"
            , Pine2024.valueFromString "argument_beta"
            ]
        , environmentFunctions = []
        , expectedValue = Pine2024.valueFromString "argument_beta"
        }
      )
    , ( "Two parameters - return first"
      , { functionInnerExpr = FirCompiler2024.ReferenceExpression [] "param_alfa"
        , functionParams =
            [ [ ( "param_alfa", [] )
              ]
            , [ ( "param_beta", [] )
              ]
            ]
        , arguments =
            [ Pine2024.valueFromString "argument_alfa"
            , Pine2024.valueFromString "argument_beta"
            ]
        , environmentFunctions = []
        , expectedValue = Pine2024.valueFromString "argument_alfa"
        }
      )
    , ( "Three parameters - return literal"
      , { functionInnerExpr = FirCompiler2024.LiteralExpression (Pine2024.valueFromString "constant-literal")
        , functionParams =
            [ [ ( "param_alfa", [] )
              ]
            , [ ( "param_beta", [] )
              ]
            , [ ( "param_gamma", [] )
              ]
            ]
        , arguments =
            [ Pine2024.valueFromString "argument_alfa"
            , Pine2024.valueFromString "argument_beta"
            , Pine2024.valueFromString "argument_gamma"
            ]
        , environmentFunctions = []
        , expectedValue = Pine2024.valueFromString "constant-literal"
        }
      )
    , ( "Three parameters - return third"
      , { functionInnerExpr = FirCompiler2024.ReferenceExpression [] "param_gamma"
        , functionParams =
            [ [ ( "param_alfa", [] )
              ]
            , [ ( "param_beta", [] )
              ]
            , [ ( "param_gamma", [] )
              ]
            ]
        , arguments =
            [ Pine2024.valueFromString "argument_alfa"
            , Pine2024.valueFromString "argument_beta"
            , Pine2024.valueFromString "argument_gamma"
            ]
        , environmentFunctions = []
        , expectedValue = Pine2024.valueFromString "argument_gamma"
        }
      )
    , ( "Three parameters - return second"
      , { functionInnerExpr = FirCompiler2024.ReferenceExpression [] "param_beta"
        , functionParams =
            [ [ ( "param_alfa", [] ) ]
            , [ ( "param_beta", [] ) ]
            , [ ( "param_gamma", [] ) ]
            ]
        , arguments =
            [ Pine2024.valueFromString "argument_alfa"
            , Pine2024.valueFromString "argument_beta"
            , Pine2024.valueFromString "argument_gamma"
            ]
        , environmentFunctions = []
        , expectedValue = Pine2024.valueFromString "argument_beta"
        }
      )
    , ( "Three parameters - return first"
      , { functionInnerExpr = FirCompiler2024.ReferenceExpression [] "param_alfa"
        , functionParams =
            [ [ ( "param_alfa", [] ) ]
            , [ ( "param_beta", [] ) ]
            , [ ( "param_gamma", [] ) ]
            ]
        , arguments =
            [ Pine2024.valueFromString "argument_alfa"
            , Pine2024.valueFromString "argument_beta"
            , Pine2024.valueFromString "argument_gamma"
            ]
        , environmentFunctions = []
        , expectedValue = Pine2024.valueFromString "argument_alfa"
        }
      )
    , ( "Three parameters - return from function with one param"
      , { functionInnerExpr =
            FirCompiler2024.FunctionApplicationExpression
                (FirCompiler2024.ReferenceExpression [] "repeat_three_times")
                [ FirCompiler2024.ReferenceExpression [] "param_alfa" ]
        , functionParams =
            [ [ ( "param_alfa", [] ) ]
            , [ ( "param_beta", [] ) ]
            , [ ( "param_gamma", [] ) ]
            ]
        , arguments =
            [ Pine2024.valueFromString "argument_alfa"
            , Pine2024.valueFromString "argument_beta"
            , Pine2024.valueFromString "argument_gamma"
            ]
        , environmentFunctions =
            [ ( "repeat_three_times"
              , { functionInnerExpr =
                    FirCompiler2024.ListExpression
                        [ FirCompiler2024.ReferenceExpression [] "param_name"
                        , FirCompiler2024.ReferenceExpression [] "param_name"
                        , FirCompiler2024.ReferenceExpression [] "param_name"
                        ]
                , functionParams =
                    [ [ ( "param_name", [] ) ] ]
                }
              )
            ]
        , expectedValue =
            Pine2024.ListValue
                [ Pine2024.valueFromString "argument_alfa"
                , Pine2024.valueFromString "argument_alfa"
                , Pine2024.valueFromString "argument_alfa"
                ]
        }
      )
    , ( "Three parameters - return from function with two param - first"
      , { functionInnerExpr =
            FirCompiler2024.FunctionApplicationExpression
                (FirCompiler2024.ReferenceExpression [] "repeat_three_times")
                [ FirCompiler2024.ReferenceExpression [] "param_alfa"
                , FirCompiler2024.ReferenceExpression [] "param_beta"
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
            [ Pine2024.valueFromString "argument_alfa"
            , Pine2024.valueFromString "argument_beta"
            , Pine2024.valueFromString "argument_gamma"
            ]
        , environmentFunctions =
            [ ( "repeat_three_times"
              , { functionInnerExpr =
                    FirCompiler2024.ListExpression
                        [ FirCompiler2024.ReferenceExpression [] "param_name_a"
                        , FirCompiler2024.ReferenceExpression [] "param_name_a"
                        , FirCompiler2024.ReferenceExpression [] "param_name_a"
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
            Pine2024.ListValue
                [ Pine2024.valueFromString "argument_alfa"
                , Pine2024.valueFromString "argument_alfa"
                , Pine2024.valueFromString "argument_alfa"
                ]
        }
      )
    , ( "Two parameters - repeat"
      , { functionInnerExpr =
            FirCompiler2024.FunctionApplicationExpression
                (FirCompiler2024.ReferenceExpression [] "repeat_help")
                [ FirCompiler2024.LiteralExpression (Pine2024.ListValue [])
                , FirCompiler2024.ListExpression
                    [ FirCompiler2024.ReferenceExpression [] "count"
                    , FirCompiler2024.ReferenceExpression [] "value"
                    ]
                ]
        , functionParams =
            [ [ ( "count", [] )
              ]
            , [ ( "value", [] )
              ]
            ]
        , arguments =
            [ Pine2024.valueFromInt 3
            , Pine2024.valueFromString "test_elem_two"
            ]
        , environmentFunctions =
            [ ( "repeat_help"
              , { functionInnerExpr =
                    FirCompiler2024.ConditionalExpression
                        (FirCompiler2024.KernelApplicationExpression
                            "int_is_sorted_asc"
                            (FirCompiler2024.ListExpression
                                [ FirCompiler2024.ReferenceExpression [] "remainingCount"
                                , FirCompiler2024.LiteralExpression (Pine2024.valueFromInt 0)
                                ]
                            )
                        )
                        (FirCompiler2024.FunctionApplicationExpression
                            (FirCompiler2024.ReferenceExpression [] "repeat_help")
                            [ FirCompiler2024.KernelApplicationExpression
                                "concat"
                                (FirCompiler2024.ListExpression
                                    [ FirCompiler2024.ListExpression
                                        [ FirCompiler2024.ReferenceExpression [] "value"
                                        ]
                                    , FirCompiler2024.ReferenceExpression [] "result"
                                    ]
                                )
                            , FirCompiler2024.ListExpression
                                [ FirCompiler2024.KernelApplicationExpression
                                    "int_add"
                                    (FirCompiler2024.ListExpression
                                        [ FirCompiler2024.ReferenceExpression [] "remainingCount"
                                        , FirCompiler2024.LiteralExpression (Pine2024.valueFromInt -1)
                                        ]
                                    )
                                , FirCompiler2024.ReferenceExpression [] "value"
                                ]
                            ]
                        )
                        (FirCompiler2024.ReferenceExpression [] "result")
                , functionParams =
                    [ [ ( "result", [] )
                      ]
                    , [ ( "remainingCount"
                        , [ FirCompiler2024.ListItemDeconstruction 0 ]
                        )
                      , ( "value"
                        , [ FirCompiler2024.ListItemDeconstruction 1 ]
                        )
                      ]
                    ]
                }
              )
            ]
        , expectedValue =
            Pine2024.valueFromString "test_elem_two"
                |> List.repeat 3
                |> Pine2024.ListValue
        }
      )
    , ( "Three parameters - repeat"
      , { functionInnerExpr =
            FirCompiler2024.FunctionApplicationExpression
                (FirCompiler2024.ReferenceExpression [] "repeat_help")
                (List.map FirCompiler2024.LiteralExpression
                    [ Pine2024.ListValue []
                    , Pine2024.valueFromInt 3
                    , Pine2024.valueFromString "test_elem"
                    ]
                )
        , functionParams = []
        , arguments = []
        , environmentFunctions =
            [ ( "repeat_help"
              , { functionInnerExpr =
                    FirCompiler2024.ConditionalExpression
                        (FirCompiler2024.KernelApplicationExpression
                            "int_is_sorted_asc"
                            (FirCompiler2024.ListExpression
                                [ FirCompiler2024.ReferenceExpression [] "remainingCount"
                                , FirCompiler2024.LiteralExpression (Pine2024.valueFromInt 0)
                                ]
                            )
                        )
                        (FirCompiler2024.FunctionApplicationExpression
                            (FirCompiler2024.ReferenceExpression [] "repeat_help")
                            [ FirCompiler2024.KernelApplicationExpression
                                "concat"
                                (FirCompiler2024.ListExpression
                                    [ FirCompiler2024.ListExpression
                                        [ FirCompiler2024.ReferenceExpression [] "value"
                                        ]
                                    , FirCompiler2024.ReferenceExpression [] "result"
                                    ]
                                )
                            , FirCompiler2024.KernelApplicationExpression
                                "int_add"
                                (FirCompiler2024.ListExpression
                                    [ FirCompiler2024.ReferenceExpression [] "remainingCount"
                                    , FirCompiler2024.LiteralExpression (Pine2024.valueFromInt -1)
                                    ]
                                )
                            , FirCompiler2024.ReferenceExpression [] "value"
                            ]
                        )
                        (FirCompiler2024.ReferenceExpression [] "result")
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
            Pine2024.valueFromString "test_elem"
                |> List.repeat 3
                |> Pine2024.ListValue
        }
      )
    , ( "let block returning literal"
      , { functionInnerExpr =
            FirCompiler2024.DeclarationBlockExpression
                [ ( "decl_from_let"
                  , FirCompiler2024.LiteralExpression (Pine2024.valueFromString "constant_in_let")
                  )
                ]
                (FirCompiler2024.ReferenceExpression [] "decl_from_let")
        , functionParams = []
        , arguments = []
        , environmentFunctions = []
        , expectedValue = Pine2024.valueFromString "constant_in_let"
        }
      )
    , ( "let block returning from other decl in same block"
      , { functionInnerExpr =
            FirCompiler2024.DeclarationBlockExpression
                [ ( "decl_from_let"
                  , FirCompiler2024.ReferenceExpression [] "other_decl_from_let"
                  )
                , ( "other_decl_from_let"
                  , FirCompiler2024.LiteralExpression (Pine2024.valueFromString "constant_in_let")
                  )
                ]
                (FirCompiler2024.ReferenceExpression [] "decl_from_let")
        , functionParams = []
        , arguments = []
        , environmentFunctions = []
        , expectedValue = Pine2024.valueFromString "constant_in_let"
        }
      )
    , ( "let block returning only parent function arg"
      , { functionInnerExpr =
            FirCompiler2024.DeclarationBlockExpression
                [ ( "decl_from_let"
                  , FirCompiler2024.ReferenceExpression [] "param_0"
                  )
                ]
                (FirCompiler2024.ReferenceExpression [] "decl_from_let")
        , functionParams =
            [ [ ( "param_0", [] ) ]
            ]
        , arguments = [ Pine2024.valueFromString "argument_0" ]
        , environmentFunctions = []
        , expectedValue = Pine2024.valueFromString "argument_0"
        }
      )
    , ( "let block in let block returning only parent function arg"
      , { functionInnerExpr =
            FirCompiler2024.DeclarationBlockExpression
                [ ( "decl_from_let"
                  , FirCompiler2024.ReferenceExpression [] "param_0"
                  )
                ]
                (FirCompiler2024.DeclarationBlockExpression
                    [ ( "decl_from_let_inner"
                      , FirCompiler2024.ReferenceExpression [] "decl_from_let"
                      )
                    ]
                    (FirCompiler2024.ReferenceExpression [] "decl_from_let_inner")
                )
        , functionParams =
            [ [ ( "param_0", [] ) ]
            ]
        , arguments = [ Pine2024.valueFromString "argument_0" ]
        , environmentFunctions = []
        , expectedValue = Pine2024.valueFromString "argument_0"
        }
      )
    , ( "let block returning second parent function arg"
      , { functionInnerExpr =
            FirCompiler2024.DeclarationBlockExpression
                [ ( "decl_from_let"
                  , FirCompiler2024.ReferenceExpression [] "param_1"
                  )
                ]
                (FirCompiler2024.ReferenceExpression [] "decl_from_let")
        , functionParams =
            [ [ ( "param_0", [] ) ]
            , [ ( "param_1", [] ) ]
            ]
        , arguments =
            [ Pine2024.valueFromString "argument_0"
            , Pine2024.valueFromString "argument_1"
            ]
        , environmentFunctions = []
        , expectedValue = Pine2024.valueFromString "argument_1"
        }
      )
    , ( "partial application only for closure - one original param"
      , { functionInnerExpr =
            FirCompiler2024.DeclarationBlockExpression
                [ ( "decl_from_let"
                  , FirCompiler2024.FunctionExpression
                        [ [ ( "final_func_param_0"
                            , []
                            )
                          ]
                        ]
                        (FirCompiler2024.FunctionApplicationExpression
                            (FirCompiler2024.ReferenceExpression [] "final_func_param_0")
                            [ FirCompiler2024.LiteralExpression (Pine2024.valueFromString "literal_0")
                            ]
                        )
                  )
                , ( "closure_func"
                  , FirCompiler2024.FunctionExpression
                        [ [ ( "closure_func_param_0"
                            , []
                            )
                          ]
                        ]
                        (FirCompiler2024.ListExpression
                            [ FirCompiler2024.ReferenceExpression [] "closure_func_param_0"
                            , FirCompiler2024.ReferenceExpression [] "param_0"
                            ]
                        )
                  )
                ]
                (FirCompiler2024.FunctionApplicationExpression
                    (FirCompiler2024.ReferenceExpression [] "decl_from_let")
                    [ FirCompiler2024.ReferenceExpression [] "closure_func"
                    ]
                )
        , functionParams =
            [ [ ( "param_0", [] )
              ]
            ]
        , arguments =
            [ Pine2024.valueFromString "argument_0"
            ]
        , environmentFunctions = []
        , expectedValue =
            Pine2024.ListValue
                [ Pine2024.valueFromString "literal_0"
                , Pine2024.valueFromString "argument_0"
                ]
        }
      )
    , ( "let block returning from other outside decl"
      , { functionInnerExpr =
            FirCompiler2024.DeclarationBlockExpression
                [ ( "decl_from_let"
                  , FirCompiler2024.ReferenceExpression [] "env_func"
                  )
                ]
                (FirCompiler2024.ReferenceExpression [] "decl_from_let")
        , functionParams =
            [ [ ( "param_0", [] ) ]
            ]
        , arguments = [ Pine2024.valueFromString "argument_0" ]
        , environmentFunctions =
            [ ( "env_func"
              , { functionInnerExpr = FirCompiler2024.LiteralExpression (Pine2024.valueFromString "const_from_env_func")
                , functionParams = []
                }
              )
            ]
        , expectedValue = Pine2024.valueFromString "const_from_env_func"
        }
      )
    , ( "Partial application - two - return literal"
      , { functionInnerExpr =
            FirCompiler2024.FunctionApplicationExpression
                (FirCompiler2024.ReferenceExpression [] "second_function_partially_applied")
                [ FirCompiler2024.LiteralExpression (Pine2024.valueFromString "second_arg")
                ]
        , functionParams = []
        , arguments = []
        , environmentFunctions =
            [ ( "second_function"
              , { functionInnerExpr = FirCompiler2024.LiteralExpression (Pine2024.valueFromString "constant-literal")
                , functionParams =
                    [ [ ( "second_function_param_alfa", [] ) ]
                    , [ ( "second_function_param_beta", [] ) ]
                    ]
                }
              )
            , ( "second_function_partially_applied"
              , { functionInnerExpr =
                    FirCompiler2024.FunctionApplicationExpression
                        (FirCompiler2024.ReferenceExpression [] "second_function")
                        [ FirCompiler2024.LiteralExpression (Pine2024.valueFromString "first_arg")
                        ]
                , functionParams = []
                }
              )
            ]
        , expectedValue = Pine2024.valueFromString "constant-literal"
        }
      )
    ]
        |> List.indexedMap
            (\testCaseIndex ( testCaseName, testCase ) ->
                Test.test ("Case " ++ String.fromInt testCaseIndex ++ " - " ++ testCaseName) <|
                    \_ ->
                        let
                            declarationBlockOuterExprFromFunctionParamsAndInnerExpr params innerExpr =
                                FirCompiler2024.FunctionExpression params innerExpr

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

                            emptyEmitStack =
                                { importedFunctions = []
                                , importedFunctionsToInline = []
                                , environmentFunctions = []
                                , environmentDeconstructions = []
                                , skipApplyFunction = False
                                }

                            rootFunctionExpression =
                                declarationBlockOuterExprFromFunctionParamsAndInnerExpr
                                    testCase.functionParams
                                    testCase.functionInnerExpr

                            rootFunctionApplication =
                                FirCompiler2024.FunctionApplicationExpression
                                    rootFunctionExpression
                                    (testCase.arguments |> List.map FirCompiler2024.LiteralExpression)

                            emitClosureResult =
                                FirCompiler2024.emitExpressionInDeclarationBlock
                                    emptyEmitStack
                                    environmentFunctions
                                    rootFunctionApplication
                        in
                        case emitClosureResult of
                            Ok applicationExpr ->
                                case Pine2024.evaluateExpression Pine2024.emptyEvalEnvironment applicationExpr of
                                    Ok value ->
                                        Expect.equal value testCase.expectedValue

                                    Err error ->
                                        Expect.fail (Pine2024.displayStringFromPineError error)

                            Err error ->
                                Expect.fail error
            )
        |> Test.describe "emit closure expression"
