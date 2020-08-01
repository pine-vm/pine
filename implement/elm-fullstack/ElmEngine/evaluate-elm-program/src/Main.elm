module Main exposing (..)

import Elm.Parser
import Elm.Processing
import Elm.Syntax.Declaration
import Elm.Syntax.Expression
import Elm.Syntax.File
import Elm.Syntax.Node
import Json.Encode
import Parser
import Platform
import Result.Extra


type JsonValue
    = StringValue String


type EvaluationContext
    = EmptyContext
    | ContextWithValueBindings (List { name : String, value : JsonValue }) EvaluationContext


contextWithValueBindings : List { name : String, value : JsonValue } -> EvaluationContext -> EvaluationContext
contextWithValueBindings =
    ContextWithValueBindings


lookUpValueInContext : EvaluationContext -> String -> Maybe JsonValue
lookUpValueInContext context name =
    case context of
        EmptyContext ->
            Nothing

        ContextWithValueBindings bindings parentContext ->
            case bindings |> List.filter (.name >> (==) name) |> List.head of
                Just binding ->
                    Just binding.value

                Nothing ->
                    lookUpValueInContext parentContext name


getValueFromExpressionSyntaxAsJsonString : String -> Result String String
getValueFromExpressionSyntaxAsJsonString =
    evaluateExpressionSyntax
        >> Result.map jsonStringFromJsonValue


jsonStringFromJsonValue : JsonValue -> String
jsonStringFromJsonValue value =
    case value of
        StringValue string ->
            "\"" ++ string ++ "\""


evaluateExpressionSyntax : String -> Result String JsonValue
evaluateExpressionSyntax expressionCode =
    case parseExpressionFromString expressionCode of
        Err parseError ->
            Err ("Failed to parse expression: " ++ parseError)

        Ok expression ->
            evaluateExpression EmptyContext expression
                |> Result.mapError (\error -> "Failed to evaluate expression '" ++ expressionCode ++ "': " ++ error)


evaluateExpression : EvaluationContext -> Elm.Syntax.Expression.Expression -> Result String JsonValue
evaluateExpression context expression =
    case expression of
        Elm.Syntax.Expression.Literal literal ->
            Ok (StringValue literal)

        Elm.Syntax.Expression.OperatorApplication operator direction leftExpr rightExpr ->
            case operator of
                "++" ->
                    case leftExpr |> Elm.Syntax.Node.value |> evaluateExpression context of
                        Err error ->
                            Err ("Failed to evaluate left expression: " ++ error)

                        Ok leftValue ->
                            case rightExpr |> Elm.Syntax.Node.value |> evaluateExpression context of
                                Err error ->
                                    Err ("Failed to evaluate right expression: " ++ error)

                                Ok rightValue ->
                                    case ( leftValue, rightValue ) of
                                        ( StringValue leftString, StringValue rightString ) ->
                                            Ok (StringValue (leftString ++ rightString))

                _ ->
                    Err ("Unsupported type of operator: " ++ operator)

        Elm.Syntax.Expression.LetExpression letBlock ->
            case
                letBlock.declarations
                    |> List.map (Elm.Syntax.Node.value >> getValueBindingFromLetDeclaration context)
                    |> Result.Extra.combine
            of
                Err error ->
                    Err ("Failed to get value bindings from declaration in let block: " ++ error)

                Ok bindings ->
                    letBlock.expression |> Elm.Syntax.Node.value |> evaluateExpression (context |> contextWithValueBindings bindings)

        Elm.Syntax.Expression.FunctionOrValue moduleName localName ->
            if moduleName == [] then
                lookUpValueInContext context localName
                    |> Maybe.map Ok
                    |> Maybe.withDefault (Err ("Failed to look up value for '" ++ localName ++ "'"))

            else
                Err "Module name is not implemented yet."

        _ ->
            Err "Unsupported type of expression"


getValueBindingFromLetDeclaration : EvaluationContext -> Elm.Syntax.Expression.LetDeclaration -> Result String { name : String, value : JsonValue }
getValueBindingFromLetDeclaration context letDeclaration =
    case letDeclaration of
        Elm.Syntax.Expression.LetDestructuring _ _ ->
            Err "LetDestructuring is not implemented yet."

        Elm.Syntax.Expression.LetFunction letFunction ->
            let
                functionDeclaration =
                    Elm.Syntax.Node.value letFunction.declaration
            in
            if functionDeclaration.arguments == [] then
                case functionDeclaration.expression |> Elm.Syntax.Node.value |> evaluateExpression context of
                    Err error ->
                        Err ("Failed to evaluate expression of function in let: " ++ error)

                    Ok value ->
                        Ok { name = Elm.Syntax.Node.value functionDeclaration.name, value = value }

            else
                Err "Function with argument is not implemented yet."


parseExpressionFromString : String -> Result String Elm.Syntax.Expression.Expression
parseExpressionFromString expressionCode =
    let
        indentedExpressionCode =
            expressionCode
                |> String.lines
                |> List.map ((++) "    ")
                |> String.join "\n"

        moduleText =
            """
module Main exposing (..)


wrapping_expression_in_function =
"""
                ++ indentedExpressionCode
                ++ """

"""
    in
    parseElmModuleText moduleText
        |> Result.mapError (always "Failed to parse module")
        |> Result.andThen
            (\file ->
                file.declarations
                    |> List.filterMap
                        (\declaration ->
                            case Elm.Syntax.Node.value declaration of
                                Elm.Syntax.Declaration.FunctionDeclaration functionDeclaration ->
                                    functionDeclaration
                                        |> .declaration
                                        |> Elm.Syntax.Node.value
                                        |> .expression
                                        |> Elm.Syntax.Node.value
                                        |> Just

                                _ ->
                                    Nothing
                        )
                    |> List.head
                    |> Result.fromMaybe "Failed to extract the wrapping function."
            )


parseElmModuleTextToJson : String -> String
parseElmModuleTextToJson elmModule =
    let
        jsonValue =
            case parseElmModuleText elmModule of
                Err _ ->
                    [ ( "Err", "Failed to parse this as module text" |> Json.Encode.string ) ] |> Json.Encode.object

                Ok file ->
                    [ ( "Ok", file |> Elm.Syntax.File.encode ) ] |> Json.Encode.object
    in
    jsonValue |> Json.Encode.encode 0


parseElmModuleText : String -> Result (List Parser.DeadEnd) Elm.Syntax.File.File
parseElmModuleText =
    Elm.Parser.parse >> Result.map (Elm.Processing.process Elm.Processing.init)


{-| Support function-level dead code elimination (<https://elm-lang.org/blog/small-assets-without-the-headache>)
Elm code needed to inform the Elm compiler about our entry points.
-}
main : Program Int () String
main =
    Platform.worker
        { init = \_ -> ( (), Cmd.none )
        , update =
            \event stateBefore ->
                ( parseElmModuleTextToJson "" |> always stateBefore, Cmd.none )
        , subscriptions = \_ -> Sub.none
        }
