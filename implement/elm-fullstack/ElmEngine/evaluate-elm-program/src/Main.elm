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


getValueFromExpressionAsJsonString : String -> Result String String
getValueFromExpressionAsJsonString expressionCode =
    case parseExpressionFromString expressionCode of
        Err parseError ->
            Err ("Failed to parse expression: " ++ parseError)

        Ok expression ->
            case expression of
                Elm.Syntax.Expression.Literal literal ->
                    Ok ("\"" ++ literal ++ "\"")

                _ ->
                    Err "Unsupported type of expression."


parseExpressionFromString : String -> Result String Elm.Syntax.Expression.Expression
parseExpressionFromString expressionCode =
    let
        moduleText =
            """
module Main exposing (..)


wrapping_expression_in_function =
    """
                ++ expressionCode
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
