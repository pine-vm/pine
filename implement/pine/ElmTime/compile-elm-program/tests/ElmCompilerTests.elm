module ElmCompilerTests exposing (..)

import BigInt
import Dict
import Expect
import FirCompiler
import Pine
import Result.Extra
import Test


type alias ReduceParseAndEvalTestCase =
    { original : ParseAndEvalExpressionStruct
    , expected : Pine.Expression
    , additionalTestEnvironments : List Pine.Value
    }


type alias ParseAndEvalExpressionStruct =
    ( Pine.Expression, Pine.Expression )


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


compiler_reduces_decode_and_eval_test_cases : List ( String, ReduceParseAndEvalTestCase )
compiler_reduces_decode_and_eval_test_cases =
    [ ( "simple reducible - literal"
      , { original =
            ( Pine.ListExpression []
            , Pine.LiteralExpression (Pine.valueFromString "test")
                |> Pine.encodeExpressionAsValue
                |> Pine.LiteralExpression
            )
        , expected = Pine.LiteralExpression (Pine.valueFromString "test")
        , additionalTestEnvironments = []
        }
      )
    , ( "simple reducible - list head"
      , { original =
            ( FirCompiler.pineKernel_ListHead_Pine Pine.EnvironmentExpression
            , Pine.EnvironmentExpression
                |> Pine.encodeExpressionAsValue
                |> Pine.LiteralExpression
            )
        , expected =
            FirCompiler.pineKernel_ListHead_Pine Pine.EnvironmentExpression
        , additionalTestEnvironments = []
        }
      )
    , ( "reducible - skip 2"
      , { original =
            ( Pine.ListExpression
                [ Pine.EnvironmentExpression
                    |> FirCompiler.listSkipExpression_Pine 4
                , Pine.EnvironmentExpression
                    |> FirCompiler.listSkipExpression_Pine 3
                , Pine.EnvironmentExpression
                    |> FirCompiler.listSkipExpression_Pine 3
                , Pine.EnvironmentExpression
                    |> FirCompiler.listSkipExpression_Pine 2
                ]
            , Pine.EnvironmentExpression
                |> FirCompiler.listSkipExpression_Pine 2
                |> FirCompiler.pineKernel_ListHead_Pine
                |> Pine.encodeExpressionAsValue
                |> Pine.LiteralExpression
            )
        , expected =
            Pine.EnvironmentExpression
                |> FirCompiler.listSkipExpression_Pine 3
        , additionalTestEnvironments = []
        }
      )
    , ( "reducible - skip 1 (skip 2)"
      , { original =
            ( Pine.ListExpression
                [ Pine.ListExpression
                    [ Pine.ListExpression []
                    , Pine.EnvironmentExpression
                        |> FirCompiler.listSkipExpression_Pine 4
                    ]
                , Pine.ListExpression
                    [ Pine.ListExpression []
                    , Pine.EnvironmentExpression
                        |> FirCompiler.listSkipExpression_Pine 3
                    ]
                , Pine.ListExpression
                    [ Pine.ListExpression []
                    , Pine.EnvironmentExpression
                        |> FirCompiler.listSkipExpression_Pine 3
                    ]
                , Pine.ListExpression
                    [ Pine.ListExpression []
                    , Pine.EnvironmentExpression
                        |> FirCompiler.listSkipExpression_Pine 2
                    ]
                ]
            , Pine.EnvironmentExpression
                |> FirCompiler.listSkipExpression_Pine 2
                |> FirCompiler.pineKernel_ListHead_Pine
                |> FirCompiler.listSkipExpression_Pine 1
                |> FirCompiler.pineKernel_ListHead_Pine
                |> Pine.encodeExpressionAsValue
                |> Pine.LiteralExpression
            )
        , expected =
            Pine.EnvironmentExpression
                |> FirCompiler.listSkipExpression_Pine 3
        , additionalTestEnvironments = []
        }
      )
    , ( "simple irreducible"
      , { original =
            ( Pine.EnvironmentExpression
            , Pine.EnvironmentExpression
            )
        , expected =
            Pine.ParseAndEvalExpression
                Pine.EnvironmentExpression
                Pine.EnvironmentExpression
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
                                        ( origEnv, origExpr ) =
                                            testCase.original
                                    in
                                    Pine.ParseAndEvalExpression origEnv origExpr
                                        |> Pine.evaluateExpression (Pine.EvalEnvironment environment)
                                        |> Expect.equal
                                            (Pine.evaluateExpression
                                                (Pine.EvalEnvironment environment)
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
                            |> FirCompiler.attemptReduceParseAndEvalExpressionRecursive { maxDepth = 4 }
                            |> Expect.equal testCase.expected
            )
        |> Test.describe "Compiler reduces decode and eval expression"


emitClosureExpressionTests : Test.Test
emitClosureExpressionTests =
    [ ( "Zero parameters"
      , { functionInnerExpr =
            FirCompiler.LiteralExpression (Pine.valueFromString "test")
        , functionParams = []
        , arguments = []
        , environmentFunctions = []
        , expectedValue = Pine.valueFromString "test"
        }
      )
    , ( "Zero parameters - return from function with one param"
      , { functionInnerExpr =
            FirCompiler.FunctionApplicationExpression
                (FirCompiler.ReferenceExpression "repeat_three_times")
                [ FirCompiler.LiteralExpression (Pine.valueFromString "argument_alfa") ]
        , functionParams = []
        , arguments = []
        , environmentFunctions =
            [ ( "repeat_three_times"
              , { functionInnerExpr =
                    FirCompiler.ListExpression
                        [ FirCompiler.ReferenceExpression "param_name"
                        , FirCompiler.ReferenceExpression "param_name"
                        , FirCompiler.ReferenceExpression "param_name"
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
            FirCompiler.FunctionApplicationExpression
                (FirCompiler.ReferenceExpression "return_constant_literal")
                []
        , functionParams = []
        , arguments = []
        , environmentFunctions =
            [ ( "return_constant_literal"
              , { functionInnerExpr =
                    FirCompiler.LiteralExpression (Pine.valueFromString "constant")
                , functionParams = []
                }
              )
            ]
        , expectedValue = Pine.valueFromString "constant"
        }
      )
    , ( "Zero parameters - return literal from function with zero param - twice"
      , { functionInnerExpr =
            FirCompiler.FunctionApplicationExpression
                (FirCompiler.ReferenceExpression "return_constant_literal_first")
                []
        , functionParams = []
        , arguments = []
        , environmentFunctions =
            [ ( "return_constant_literal_first"
              , { functionInnerExpr =
                    FirCompiler.FunctionApplicationExpression
                        (FirCompiler.ReferenceExpression "return_constant_literal_second")
                        []
                , functionParams = []
                }
              )
            , ( "return_constant_literal_second"
              , { functionInnerExpr =
                    FirCompiler.LiteralExpression (Pine.valueFromString "constant")
                , functionParams = []
                }
              )
            ]
        , expectedValue = Pine.valueFromString "constant"
        }
      )
    , ( "One parameter - literal"
      , { functionInnerExpr =
            FirCompiler.LiteralExpression (Pine.valueFromString "test-literal")
        , functionParams = [ [ ( "param-name", [] ) ] ]
        , arguments = [ Pine.valueFromString "test-123" ]
        , environmentFunctions = []
        , expectedValue = Pine.valueFromString "test-literal"
        }
      )
    , ( "One parameter - reference"
      , { functionInnerExpr =
            FirCompiler.ReferenceExpression "param-name"
        , functionParams = [ [ ( "param-name", [] ) ] ]
        , arguments = [ Pine.valueFromString "test-345" ]
        , environmentFunctions = []
        , expectedValue = Pine.valueFromString "test-345"
        }
      )
    , ( "One parameter - reference decons tuple second"
      , { functionInnerExpr =
            FirCompiler.ReferenceExpression "param-name"
        , functionParams =
            [ [ ( "param-name"
                , [ FirCompiler.ListItemDeconstruction 1 ]
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
            FirCompiler.FunctionApplicationExpression
                (FirCompiler.ReferenceExpression "repeat_help")
                [ FirCompiler.ListExpression
                    [ FirCompiler.LiteralExpression (Pine.ListValue [])
                    , FirCompiler.ReferenceExpression "count"
                    , FirCompiler.ReferenceExpression "value"
                    ]
                ]
        , functionParams =
            [ [ ( "count"
                , [ FirCompiler.ListItemDeconstruction 0 ]
                )
              , ( "value"
                , [ FirCompiler.ListItemDeconstruction 1 ]
                )
              ]
            ]
        , arguments =
            [ Pine.ListValue
                [ Pine.valueFromInt 3
                , Pine.valueFromString "test_elem"
                ]
            ]
        , environmentFunctions =
            [ ( "repeat_help"
              , { functionInnerExpr =
                    FirCompiler.ConditionalExpression
                        (FirCompiler.KernelApplicationExpression
                            (FirCompiler.ListExpression
                                [ FirCompiler.ReferenceExpression "remainingCount"
                                , FirCompiler.LiteralExpression (Pine.valueFromInt 0)
                                ]
                            )
                            "is_sorted_ascending_int"
                        )
                        (FirCompiler.FunctionApplicationExpression
                            (FirCompiler.ReferenceExpression "repeat_help")
                            [ FirCompiler.ListExpression
                                [ FirCompiler.KernelApplicationExpression
                                    (FirCompiler.ListExpression
                                        [ FirCompiler.ListExpression
                                            [ FirCompiler.ReferenceExpression "value"
                                            ]
                                        , FirCompiler.ReferenceExpression "result"
                                        ]
                                    )
                                    "concat"
                                , FirCompiler.KernelApplicationExpression
                                    (FirCompiler.ListExpression
                                        [ FirCompiler.ReferenceExpression "remainingCount"
                                        , FirCompiler.LiteralExpression (Pine.valueFromInt -1)
                                        ]
                                    )
                                    "add_int"
                                , FirCompiler.ReferenceExpression "value"
                                ]
                            ]
                        )
                        (FirCompiler.ReferenceExpression "result")
                , functionParams =
                    [ [ ( "result"
                        , [ FirCompiler.ListItemDeconstruction 0 ]
                        )
                      , ( "remainingCount"
                        , [ FirCompiler.ListItemDeconstruction 1 ]
                        )
                      , ( "value"
                        , [ FirCompiler.ListItemDeconstruction 2 ]
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
            FirCompiler.FunctionApplicationExpression
                (FirCompiler.ReferenceExpression "repeat_help")
                [ FirCompiler.ListExpression
                    [ FirCompiler.LiteralExpression (Pine.ListValue [])
                    , FirCompiler.ReferenceExpression "count"
                    , FirCompiler.ReferenceExpression "value"
                    ]
                ]
        , functionParams =
            [ [ ( "count"
                , [ FirCompiler.ListItemDeconstruction 0 ]
                )
              , ( "value"
                , [ FirCompiler.ListItemDeconstruction 1 ]
                )
              ]
            ]
        , arguments =
            [ Pine.ListValue
                [ Pine.valueFromInt 3
                , Pine.valueFromString "test_elem"
                ]
            ]
        , environmentFunctions =
            [ ( "is_less_than_or_equal_to_zero"
              , { functionInnerExpr =
                    FirCompiler.KernelApplicationExpression
                        (FirCompiler.ListExpression
                            [ FirCompiler.ReferenceExpression "num"
                            , FirCompiler.LiteralExpression (Pine.valueFromInt 0)
                            ]
                        )
                        "is_sorted_ascending_int"
                , functionParams =
                    [ [ ( "num", [] )
                      ]
                    ]
                }
              )
            , ( "repeat_help"
              , { functionInnerExpr =
                    FirCompiler.ConditionalExpression
                        (FirCompiler.FunctionApplicationExpression
                            (FirCompiler.ReferenceExpression "is_less_than_or_equal_to_zero")
                            [ FirCompiler.ReferenceExpression "remainingCount"
                            ]
                        )
                        (FirCompiler.FunctionApplicationExpression
                            (FirCompiler.ReferenceExpression "repeat_help")
                            [ FirCompiler.ListExpression
                                [ FirCompiler.KernelApplicationExpression
                                    (FirCompiler.ListExpression
                                        [ FirCompiler.ListExpression
                                            [ FirCompiler.ReferenceExpression "value"
                                            ]
                                        , FirCompiler.ReferenceExpression "result"
                                        ]
                                    )
                                    "concat"
                                , FirCompiler.KernelApplicationExpression
                                    (FirCompiler.ListExpression
                                        [ FirCompiler.ReferenceExpression "remainingCount"
                                        , FirCompiler.LiteralExpression (Pine.valueFromInt -1)
                                        ]
                                    )
                                    "add_int"
                                , FirCompiler.ReferenceExpression "value"
                                ]
                            ]
                        )
                        (FirCompiler.ReferenceExpression "result")
                , functionParams =
                    [ [ ( "result"
                        , [ FirCompiler.ListItemDeconstruction 0 ]
                        )
                      , ( "remainingCount"
                        , [ FirCompiler.ListItemDeconstruction 1 ]
                        )
                      , ( "value"
                        , [ FirCompiler.ListItemDeconstruction 2 ]
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
      , { functionInnerExpr = FirCompiler.LiteralExpression (Pine.valueFromString "constant-literal")
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
      , { functionInnerExpr = FirCompiler.ReferenceExpression "param_beta"
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
      , { functionInnerExpr = FirCompiler.ReferenceExpression "param_alfa"
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
      , { functionInnerExpr = FirCompiler.LiteralExpression (Pine.valueFromString "constant-literal")
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
      , { functionInnerExpr = FirCompiler.ReferenceExpression "param_gamma"
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
      , { functionInnerExpr = FirCompiler.ReferenceExpression "param_beta"
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
      , { functionInnerExpr = FirCompiler.ReferenceExpression "param_alfa"
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
            FirCompiler.FunctionApplicationExpression
                (FirCompiler.ReferenceExpression "repeat_three_times")
                [ FirCompiler.ReferenceExpression "param_alfa" ]
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
                    FirCompiler.ListExpression
                        [ FirCompiler.ReferenceExpression "param_name"
                        , FirCompiler.ReferenceExpression "param_name"
                        , FirCompiler.ReferenceExpression "param_name"
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
            FirCompiler.FunctionApplicationExpression
                (FirCompiler.ReferenceExpression "repeat_three_times")
                [ FirCompiler.ReferenceExpression "param_alfa"
                , FirCompiler.ReferenceExpression "param_beta"
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
                    FirCompiler.ListExpression
                        [ FirCompiler.ReferenceExpression "param_name_a"
                        , FirCompiler.ReferenceExpression "param_name_a"
                        , FirCompiler.ReferenceExpression "param_name_a"
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
            FirCompiler.FunctionApplicationExpression
                (FirCompiler.ReferenceExpression "repeat_help")
                [ FirCompiler.LiteralExpression (Pine.ListValue [])
                , FirCompiler.ListExpression
                    [ FirCompiler.ReferenceExpression "count"
                    , FirCompiler.ReferenceExpression "value"
                    ]
                ]
        , functionParams =
            [ [ ( "count", [] )
              ]
            , [ ( "value", [] )
              ]
            ]
        , arguments =
            [ Pine.valueFromInt 3
            , Pine.valueFromString "test_elem_two"
            ]
        , environmentFunctions =
            [ ( "repeat_help"
              , { functionInnerExpr =
                    FirCompiler.ConditionalExpression
                        (FirCompiler.KernelApplicationExpression
                            (FirCompiler.ListExpression
                                [ FirCompiler.ReferenceExpression "remainingCount"
                                , FirCompiler.LiteralExpression (Pine.valueFromInt 0)
                                ]
                            )
                            "is_sorted_ascending_int"
                        )
                        (FirCompiler.FunctionApplicationExpression
                            (FirCompiler.ReferenceExpression "repeat_help")
                            [ FirCompiler.KernelApplicationExpression
                                (FirCompiler.ListExpression
                                    [ FirCompiler.ListExpression
                                        [ FirCompiler.ReferenceExpression "value"
                                        ]
                                    , FirCompiler.ReferenceExpression "result"
                                    ]
                                )
                                "concat"
                            , FirCompiler.ListExpression
                                [ FirCompiler.KernelApplicationExpression
                                    (FirCompiler.ListExpression
                                        [ FirCompiler.ReferenceExpression "remainingCount"
                                        , FirCompiler.LiteralExpression (Pine.valueFromInt -1)
                                        ]
                                    )
                                    "add_int"
                                , FirCompiler.ReferenceExpression "value"
                                ]
                            ]
                        )
                        (FirCompiler.ReferenceExpression "result")
                , functionParams =
                    [ [ ( "result", [] )
                      ]
                    , [ ( "remainingCount"
                        , [ FirCompiler.ListItemDeconstruction 0 ]
                        )
                      , ( "value"
                        , [ FirCompiler.ListItemDeconstruction 1 ]
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
            FirCompiler.FunctionApplicationExpression
                (FirCompiler.ReferenceExpression "repeat_help")
                (List.map FirCompiler.LiteralExpression
                    [ Pine.ListValue []
                    , Pine.valueFromInt 3
                    , Pine.valueFromString "test_elem"
                    ]
                )
        , functionParams = []
        , arguments = []
        , environmentFunctions =
            [ ( "repeat_help"
              , { functionInnerExpr =
                    FirCompiler.ConditionalExpression
                        (FirCompiler.KernelApplicationExpression
                            (FirCompiler.ListExpression
                                [ FirCompiler.ReferenceExpression "remainingCount"
                                , FirCompiler.LiteralExpression (Pine.valueFromInt 0)
                                ]
                            )
                            "is_sorted_ascending_int"
                        )
                        (FirCompiler.FunctionApplicationExpression
                            (FirCompiler.ReferenceExpression "repeat_help")
                            [ FirCompiler.KernelApplicationExpression
                                (FirCompiler.ListExpression
                                    [ FirCompiler.ListExpression
                                        [ FirCompiler.ReferenceExpression "value"
                                        ]
                                    , FirCompiler.ReferenceExpression "result"
                                    ]
                                )
                                "concat"
                            , FirCompiler.KernelApplicationExpression
                                (FirCompiler.ListExpression
                                    [ FirCompiler.ReferenceExpression "remainingCount"
                                    , FirCompiler.LiteralExpression (Pine.valueFromInt -1)
                                    ]
                                )
                                "add_int"
                            , FirCompiler.ReferenceExpression "value"
                            ]
                        )
                        (FirCompiler.ReferenceExpression "result")
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
            FirCompiler.DeclarationBlockExpression
                [ ( "decl_from_let"
                  , FirCompiler.LiteralExpression (Pine.valueFromString "constant_in_let")
                  )
                ]
                (FirCompiler.ReferenceExpression "decl_from_let")
        , functionParams = []
        , arguments = []
        , environmentFunctions = []
        , expectedValue = Pine.valueFromString "constant_in_let"
        }
      )
    , ( "let block returning from other decl in same block"
      , { functionInnerExpr =
            FirCompiler.DeclarationBlockExpression
                [ ( "decl_from_let"
                  , FirCompiler.ReferenceExpression "other_decl_from_let"
                  )
                , ( "other_decl_from_let"
                  , FirCompiler.LiteralExpression (Pine.valueFromString "constant_in_let")
                  )
                ]
                (FirCompiler.ReferenceExpression "decl_from_let")
        , functionParams = []
        , arguments = []
        , environmentFunctions = []
        , expectedValue = Pine.valueFromString "constant_in_let"
        }
      )
    , ( "let block returning only parent function arg"
      , { functionInnerExpr =
            FirCompiler.DeclarationBlockExpression
                [ ( "decl_from_let"
                  , FirCompiler.ReferenceExpression "param_0"
                  )
                ]
                (FirCompiler.ReferenceExpression "decl_from_let")
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
            FirCompiler.DeclarationBlockExpression
                [ ( "decl_from_let"
                  , FirCompiler.ReferenceExpression "param_0"
                  )
                ]
                (FirCompiler.DeclarationBlockExpression
                    [ ( "decl_from_let_inner"
                      , FirCompiler.ReferenceExpression "decl_from_let"
                      )
                    ]
                    (FirCompiler.ReferenceExpression "decl_from_let_inner")
                )
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
            FirCompiler.DeclarationBlockExpression
                [ ( "decl_from_let"
                  , FirCompiler.ReferenceExpression "param_1"
                  )
                ]
                (FirCompiler.ReferenceExpression "decl_from_let")
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
            FirCompiler.DeclarationBlockExpression
                [ ( "decl_from_let"
                  , FirCompiler.FunctionExpression
                        [ [ ( "final_func_param_0"
                            , []
                            )
                          ]
                        ]
                        (FirCompiler.FunctionApplicationExpression
                            (FirCompiler.ReferenceExpression "final_func_param_0")
                            [ FirCompiler.LiteralExpression (Pine.valueFromString "literal_0")
                            ]
                        )
                  )
                , ( "closure_func"
                  , FirCompiler.FunctionExpression
                        [ [ ( "closure_func_param_0"
                            , []
                            )
                          ]
                        ]
                        (FirCompiler.ListExpression
                            [ FirCompiler.ReferenceExpression "closure_func_param_0"
                            , FirCompiler.ReferenceExpression "param_0"
                            ]
                        )
                  )
                ]
                (FirCompiler.FunctionApplicationExpression
                    (FirCompiler.ReferenceExpression "decl_from_let")
                    [ FirCompiler.ReferenceExpression "closure_func"
                    ]
                )
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
            FirCompiler.DeclarationBlockExpression
                [ ( "decl_from_let"
                  , FirCompiler.ReferenceExpression "env_func"
                  )
                ]
                (FirCompiler.ReferenceExpression "decl_from_let")
        , functionParams =
            [ [ ( "param_0", [] ) ]
            ]
        , arguments = [ Pine.valueFromString "argument_0" ]
        , environmentFunctions =
            [ ( "env_func"
              , { functionInnerExpr = FirCompiler.LiteralExpression (Pine.valueFromString "const_from_env_func")
                , functionParams = []
                }
              )
            ]
        , expectedValue = Pine.valueFromString "const_from_env_func"
        }
      )
    , ( "Partial application - two - return literal"
      , { functionInnerExpr =
            FirCompiler.FunctionApplicationExpression
                (FirCompiler.ReferenceExpression "second_function_partially_applied")
                [ FirCompiler.LiteralExpression (Pine.valueFromString "second_arg")
                ]
        , functionParams = []
        , arguments = []
        , environmentFunctions =
            [ ( "second_function"
              , { functionInnerExpr = FirCompiler.LiteralExpression (Pine.valueFromString "constant-literal")
                , functionParams =
                    [ [ ( "second_function_param_alfa", [] ) ]
                    , [ ( "second_function_param_beta", [] ) ]
                    ]
                }
              )
            , ( "second_function_partially_applied"
              , { functionInnerExpr =
                    FirCompiler.FunctionApplicationExpression
                        (FirCompiler.ReferenceExpression "second_function")
                        [ FirCompiler.LiteralExpression (Pine.valueFromString "first_arg")
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
                                FirCompiler.FunctionExpression params innerExpr

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
                                , declarationsDependencies = Dict.empty
                                , environmentFunctions = []
                                , environmentDeconstructions = []
                                }

                            rootAsExpression =
                                declarationBlockOuterExprFromFunctionParamsAndInnerExpr
                                    testCase.functionParams
                                    testCase.functionInnerExpr

                            emitClosureResult =
                                FirCompiler.emitExpressionInDeclarationBlock
                                    emptyEmitStack
                                    environmentFunctions
                                    rootAsExpression
                        in
                        emitClosureResult
                            |> Result.andThen
                                ((\partialApplicable ->
                                    FirCompiler.partialApplicationExpressionFromListOfArguments
                                        (testCase.arguments |> List.map Pine.LiteralExpression)
                                        emptyEmitStack
                                        partialApplicable
                                        |> Pine.evaluateExpression Pine.emptyEvalEnvironment
                                        |> Result.mapError Pine.displayStringFromPineError
                                 )
                                    >> Result.map (Expect.equal testCase.expectedValue)
                                )
                            |> Result.Extra.unpack Expect.fail identity
            )
        |> Test.describe "emit closure expression"
