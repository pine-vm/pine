module Main exposing (..)

import Elm.Parser
import Elm.Processing
import Elm.Syntax.Declaration
import Elm.Syntax.Expression
import Elm.Syntax.File
import Elm.Syntax.Module
import Elm.Syntax.Node
import Elm.Syntax.Pattern
import Json.Encode
import Parser
import Platform
import Result.Extra


type FunctionOrValue
    = StringValue String
    | FunctionValue String FunctionOrValue
    | ExpressionValue Elm.Syntax.Expression.Expression


type EvaluationContext
    = EmptyContext
    | WithBindingsAdded (List { name : List String, bound : FunctionOrValue }) EvaluationContext


withBindingsAdded : List { name : List String, bound : FunctionOrValue } -> EvaluationContext -> EvaluationContext
withBindingsAdded =
    WithBindingsAdded


contextFromModules : List Elm.Syntax.File.File -> EvaluationContext
contextFromModules modules =
    let
        bindings =
            modules
                |> List.concatMap
                    (\file ->
                        let
                            moduleNameSyntax =
                                case Elm.Syntax.Node.value file.moduleDefinition of
                                    Elm.Syntax.Module.NormalModule normalModule ->
                                        normalModule.moduleName

                                    Elm.Syntax.Module.PortModule portModule ->
                                        portModule.moduleName

                                    Elm.Syntax.Module.EffectModule effectModule ->
                                        effectModule.moduleName

                            moduleName =
                                Elm.Syntax.Node.value moduleNameSyntax
                        in
                        file.declarations
                            |> List.map Elm.Syntax.Node.value
                            |> List.filterMap
                                (\declaration ->
                                    case declaration of
                                        Elm.Syntax.Declaration.FunctionDeclaration functionDeclaration ->
                                            let
                                                functionImplementation =
                                                    Elm.Syntax.Node.value functionDeclaration.declaration
                                            in
                                            case functionValueFromFunctionImplementation functionImplementation of
                                                Err error ->
                                                    Nothing

                                                Ok bound ->
                                                    Just
                                                        { name = moduleName ++ [ Elm.Syntax.Node.value functionImplementation.name ]
                                                        , bound = bound
                                                        }

                                        _ ->
                                            Maybe.Nothing
                                )
                    )
    in
    withBindingsAdded bindings EmptyContext


functionValueFromFunctionImplementation : Elm.Syntax.Expression.FunctionImplementation -> Result String FunctionOrValue
functionValueFromFunctionImplementation functionImplementation =
    let
        withArgumentAdded argument functionBefore =
            case argument of
                Elm.Syntax.Pattern.VarPattern varName ->
                    Ok (FunctionValue varName functionBefore)

                _ ->
                    Err "Type of pattern is not implemented yet."
    in
    functionImplementation.arguments
        |> List.map Elm.Syntax.Node.value
        |> List.foldr
            (\argument -> Result.andThen (withArgumentAdded argument))
            (Ok (ExpressionValue (Elm.Syntax.Node.value functionImplementation.expression)))


lookUpValueInContext : EvaluationContext -> List String -> Maybe FunctionOrValue
lookUpValueInContext context name =
    case context of
        EmptyContext ->
            Nothing

        WithBindingsAdded bindings parentContext ->
            case bindings |> List.filter (.name >> (==) name) |> List.head of
                Just binding ->
                    Just binding.bound

                Nothing ->
                    lookUpValueInContext parentContext name


getValueFromExpressionSyntaxAsJsonString : List String -> String -> Result String String
getValueFromExpressionSyntaxAsJsonString modulesTexts =
    evaluateExpressionSyntax modulesTexts
        >> Result.map jsonStringFromJsonValue


getValueFromJustExpressionSyntaxAsJsonString : String -> Result String String
getValueFromJustExpressionSyntaxAsJsonString =
    getValueFromExpressionSyntaxAsJsonString []


jsonStringFromJsonValue : FunctionOrValue -> String
jsonStringFromJsonValue value =
    case value of
        StringValue string ->
            "\"" ++ string ++ "\""

        FunctionValue _ _ ->
            "Error: Got FunctionValue"

        ExpressionValue _ ->
            "Error: Got ExpressionValue"


evaluateExpressionSyntax : List String -> String -> Result String FunctionOrValue
evaluateExpressionSyntax modulesTexts expressionCode =
    case parseExpressionFromString expressionCode of
        Err parseError ->
            Err ("Failed to parse expression: " ++ parseError)

        Ok expression ->
            case modulesTexts |> List.map parseElmModuleText |> Result.Extra.combine of
                Err _ ->
                    Err "Failed to parse module text"

                Ok modules ->
                    evaluateExpression (contextFromModules modules) expression
                        |> Result.mapError (\error -> "Failed to evaluate expression '" ++ expressionCode ++ "': " ++ error)


evaluateExpression : EvaluationContext -> Elm.Syntax.Expression.Expression -> Result String FunctionOrValue
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
                                            Err "Found unsupported type of value in operands"

                _ ->
                    Err ("Unsupported type of operator: " ++ operator)

        Elm.Syntax.Expression.LetExpression letBlock ->
            case
                letBlock.declarations
                    |> List.map (Elm.Syntax.Node.value >> getExpressionBindingFromLetDeclaration)
                    |> Result.Extra.combine
            of
                Err error ->
                    Err ("Failed to get value bindings from declaration in let block: " ++ error)

                Ok bindings ->
                    let
                        letContext =
                            context
                                |> withBindingsAdded
                                    (bindings
                                        |> List.map
                                            (\binding ->
                                                { name = [ binding.name ], bound = ExpressionValue binding.expression }
                                            )
                                    )
                    in
                    letBlock.expression
                        |> Elm.Syntax.Node.value
                        |> evaluateExpression letContext

        Elm.Syntax.Expression.FunctionOrValue moduleName localName ->
            lookUpValueInContext context (moduleName ++ [ localName ])
                |> Maybe.map
                    (\boundValue ->
                        case boundValue of
                            ExpressionValue expressionValue ->
                                evaluateExpression context expressionValue

                            _ ->
                                Ok boundValue
                    )
                |> Maybe.withDefault (Err ("Failed to look up expression for '" ++ localName ++ "'"))

        Elm.Syntax.Expression.Application application ->
            case application of
                [ appliedFunctionSyntax, argument ] ->
                    case evaluateExpression context (Elm.Syntax.Node.value appliedFunctionSyntax) of
                        Err error ->
                            Err ("Failed to look up function: " ++ error)

                        Ok (FunctionValue paramName nextFunction) ->
                            case evaluateExpression context (Elm.Syntax.Node.value argument) of
                                Err error ->
                                    Err ("Failed to evaluate application argument: " ++ error)

                                Ok argumentValue ->
                                    let
                                        contextInFunction =
                                            context |> withBindingsAdded [ { name = [ paramName ], bound = argumentValue } ]
                                    in
                                    case nextFunction of
                                        ExpressionValue expressionValue ->
                                            evaluateExpression contextInFunction expressionValue

                                        FunctionValue _ _ ->
                                            Err "Unsupported shape of application: nextFunction is a FunctionValue."

                                        StringValue _ ->
                                            Err "Unsupported shape of application: nextFunction is a StringValue."

                        Ok (StringValue _) ->
                            Err "Found unexpected value for first element in application: StringValue"

                        Ok (ExpressionValue _) ->
                            Err "Found unexpected value for first element in application: ExpressionValue"

                _ ->
                    Err "Unsupported shape of application: Number of arguments is not 1"

        _ ->
            Err "Unsupported type of expression"


getExpressionBindingFromLetDeclaration :
    Elm.Syntax.Expression.LetDeclaration
    -> Result String { name : String, expression : Elm.Syntax.Expression.Expression }
getExpressionBindingFromLetDeclaration letDeclaration =
    case letDeclaration of
        Elm.Syntax.Expression.LetDestructuring _ _ ->
            Err "LetDestructuring is not implemented yet."

        Elm.Syntax.Expression.LetFunction letFunction ->
            let
                functionDeclaration =
                    Elm.Syntax.Node.value letFunction.declaration
            in
            if functionDeclaration.arguments == [] then
                Ok
                    { name = Elm.Syntax.Node.value functionDeclaration.name
                    , expression = functionDeclaration.expression |> Elm.Syntax.Node.value
                    }

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
