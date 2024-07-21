module ElmCompilerConstructionTests exposing (..)

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
      , Pine.LiteralExpression (Pine.valueFromInt 3)
      )
    , ( "ParseAndEvalExpression"
      , Pine.ParseAndEvalExpression
            (Pine.LiteralExpression (Pine.valueFromString "environment-placeholder"))
            (Pine.LiteralExpression (Pine.valueFromString "expression-placeholder"))
      )
    , ( "KernelApplicationExpression"
      , Pine.KernelApplicationExpression
            (Pine.LiteralExpression (Pine.valueFromString "argument-placeholder"))
            "equal"
      )
    , ( "ConditionalExpression"
      , Pine.ConditionalExpression
            (Pine.LiteralExpression (Pine.valueFromString "condition-placeholder"))
            (Pine.LiteralExpression (Pine.valueFromString "falseBranch-placeholder"))
            (Pine.LiteralExpression (Pine.valueFromString "trueBranch-placeholder"))
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
                            Pine.emptyEvalEnvironment
                            template
                            |> Result.mapError (Pine.displayStringFromPineError >> (++) "Failed evaluate template: ")
                            |> Result.andThen
                                (\value ->
                                    value
                                        |> Pine.parseExpressionFromValue
                                        |> Result.mapError ((++) "Failed decode expression from value: ")
                                )
                            |> Result.Extra.unpack
                                Expect.fail
                                (Expect.equal testedExpression)
            )
        |> Test.describe "generate template evaluating to expression"
