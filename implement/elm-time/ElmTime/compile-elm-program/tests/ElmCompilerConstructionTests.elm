module ElmCompilerConstructionTests exposing (..)

import BigInt
import ElmCompilerConstruction
import Expect
import Pine
import Result.Extra
import Test


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
                                ElmCompilerConstruction.generateTemplateEvaluatingToExpression testedExpression
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
