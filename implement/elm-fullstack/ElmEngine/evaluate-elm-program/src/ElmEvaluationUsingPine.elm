module ElmEvaluationUsingPine exposing (evaluateExpressionText)

import Elm.Syntax.Expression
import Elm.Syntax.Node
import ElmEvaluation
import Json.Encode
import Pine exposing (PineExpression(..), PineValue(..))
import Result.Extra


evaluateExpressionText : String -> Result String Json.Encode.Value
evaluateExpressionText elmExpressionText =
    case parseElmExpressionString elmExpressionText of
        Err error ->
            Err ("Failed to map from Elm to Pine expression: " ++ error)

        Ok pineExpression ->
            case Pine.evaluatePineExpression pineExpression of
                Err error ->
                    Err ("Failed to evaluate Pine expression: " ++ error)

                Ok pineValue ->
                    case pineValue of
                        PineStringOrInteger string ->
                            -- TODO: Use type inference to distinguish between string and integer
                            Ok (string |> Json.Encode.string)


parseElmExpressionString : String -> Result String PineExpression
parseElmExpressionString elmExpressionText =
    case ElmEvaluation.parseExpressionFromString elmExpressionText of
        Err error ->
            Err ("Failed to parse Elm syntax: " ++ error)

        Ok elmSyntax ->
            case pineExpressionFromElm elmSyntax of
                Err error ->
                    Err ("Failed to map from Elm to Pine: " ++ error)

                Ok ok ->
                    Ok ok


pineExpressionFromElm : Elm.Syntax.Expression.Expression -> Result String PineExpression
pineExpressionFromElm elmExpression =
    case elmExpression of
        Elm.Syntax.Expression.Literal literal ->
            Ok (PineLiteral (PineStringOrInteger literal))

        Elm.Syntax.Expression.Integer integer ->
            Ok (PineLiteral (PineStringOrInteger (String.fromInt integer)))

        Elm.Syntax.Expression.FunctionOrValue moduleName localName ->
            Ok (PineFunctionOrValue (String.join "." (moduleName ++ [ localName ])))

        Elm.Syntax.Expression.Application application ->
            case application |> List.map (Elm.Syntax.Node.value >> pineExpressionFromElm) |> Result.Extra.combine of
                Err error ->
                    Err ("Failed to map application elements: " ++ error)

                Ok applicationElements ->
                    case applicationElements of
                        appliedFunctionSyntax :: arguments ->
                            Ok (PineApplication { function = appliedFunctionSyntax, arguments = arguments })

                        [] ->
                            Err "Invalid shape of application: Zero elements in the application list"

        Elm.Syntax.Expression.OperatorApplication operator _ leftExpr rightExpr ->
            case
                ( pineExpressionFromElm (Elm.Syntax.Node.value leftExpr)
                , pineExpressionFromElm (Elm.Syntax.Node.value rightExpr)
                )
            of
                ( Ok left, Ok right ) ->
                    Ok
                        (PineApplication
                            { function = PineFunctionOrValue ("(" ++ operator ++ ")")
                            , arguments = [ left, right ]
                            }
                        )

                _ ->
                    Err "Failed to map OperatorApplication left or right expression. TODO: Expand error details."

        Elm.Syntax.Expression.ParenthesizedExpression parenthesizedExpression ->
            pineExpressionFromElm (Elm.Syntax.Node.value parenthesizedExpression)

        _ ->
            Err
                ("Unsupported type of expression: "
                    ++ (elmExpression |> Elm.Syntax.Expression.encode |> Json.Encode.encode 0)
                )
