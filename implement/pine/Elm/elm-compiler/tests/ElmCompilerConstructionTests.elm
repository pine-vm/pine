module ElmCompilerConstructionTests exposing (..)

import ElmCompilerConstruction
import Expect
import Pine
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
            (Pine.LiteralExpression (Pine.valueFromString "encoded-placeholder"))
            (Pine.LiteralExpression (Pine.valueFromString "environment-placeholder"))
      )
    , ( "KernelApplicationExpression"
      , Pine.KernelApplicationExpression
            "equal"
            (Pine.LiteralExpression (Pine.valueFromString "argument-placeholder"))
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
                        case Pine.evaluateExpression Pine.emptyEvalEnvironment template of
                            Ok value ->
                                case Pine.parseExpressionFromValue value of
                                    Ok parsedExpression ->
                                        Expect.equal testedExpression parsedExpression

                                    Err decodeError ->
                                        Expect.fail ("Failed decode expression from value: " ++ decodeError)

                            Err evaluationError ->
                                Expect.fail
                                    ("Failed evaluate template: "
                                        ++ Pine.displayStringFromPineError evaluationError
                                    )
            )
        |> Test.describe "generate template evaluating to expression"
