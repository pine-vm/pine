module ElmCompilerConstruction2024Tests exposing (..)

import ElmCompilerConstruction2024
import Expect
import Pine2024
import Test


generateTemplateEvaluatingToExpressionTests : Test.Test
generateTemplateEvaluatingToExpressionTests =
    [ ( "Empty list expression"
      , Pine2024.ListExpression []
      )
    , ( "List of two empty lists"
      , Pine2024.ListExpression
            [ Pine2024.ListExpression []
            , Pine2024.ListExpression []
            ]
      )
    , ( "String literal"
      , Pine2024.LiteralExpression (Pine2024.valueFromString "test")
      )
    , ( "Int literal"
      , Pine2024.LiteralExpression (Pine2024.valueFromInt 3)
      )
    , ( "ParseAndEvalExpression"
      , Pine2024.ParseAndEvalExpression
            (Pine2024.LiteralExpression (Pine2024.valueFromString "encoded-placeholder"))
            (Pine2024.LiteralExpression (Pine2024.valueFromString "environment-placeholder"))
      )
    , ( "KernelApplicationExpression"
      , Pine2024.KernelApplicationExpression
            "equal"
            (Pine2024.LiteralExpression (Pine2024.valueFromString "argument-placeholder"))
      )
    , ( "ConditionalExpression"
      , Pine2024.ConditionalExpression
            (Pine2024.LiteralExpression (Pine2024.valueFromString "condition-placeholder"))
            (Pine2024.LiteralExpression (Pine2024.valueFromString "falseBranch-placeholder"))
            (Pine2024.LiteralExpression (Pine2024.valueFromString "trueBranch-placeholder"))
      )
    , ( "EnvironmentExpression"
      , Pine2024.EnvironmentExpression
      )
    , ( "StringTagExpression"
      , Pine2024.StringTagExpression
            "tag-placeholder"
            (Pine2024.LiteralExpression (Pine2024.valueFromString "tagged-expr-placeholder"))
      )
    ]
        |> List.indexedMap
            (\testCaseIndex ( testCaseName, testedExpression ) ->
                Test.test ("Case " ++ String.fromInt testCaseIndex ++ " - " ++ testCaseName) <|
                    \_ ->
                        let
                            template =
                                ElmCompilerConstruction2024.generateTemplateEvaluatingToExpression testedExpression
                        in
                        case Pine2024.evaluateExpression Pine2024.emptyEvalEnvironment template of
                            Ok value ->
                                case Pine2024.parseExpressionFromValue value of
                                    Ok parsedExpression ->
                                        Expect.equal testedExpression parsedExpression

                                    Err decodeError ->
                                        Expect.fail ("Failed decode expression from value: " ++ decodeError)

                            Err evaluationError ->
                                Expect.fail
                                    ("Failed evaluate template: "
                                        ++ Pine2024.displayStringFromPineError evaluationError
                                    )
            )
        |> Test.describe "generate template evaluating to expression"
