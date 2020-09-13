module Main exposing (..)

import Elm.Syntax.File
import ElmEvaluation
import Json.Decode
import Json.Encode
import Parser
import Platform


type alias EvaluateExpressionArguments =
    { modulesTexts : List String
    , expression : String
    }


parseElmModuleTextToJson : String -> String
parseElmModuleTextToJson =
    ElmEvaluation.parseElmModuleTextToJson


parseElmModuleText : String -> Result (List Parser.DeadEnd) Elm.Syntax.File.File
parseElmModuleText =
    ElmEvaluation.parseElmModuleText


evaluateExpressionInProject : String -> String
evaluateExpressionInProject argumentsJson =
    Json.Encode.encode 0
        (case Json.Decode.decodeString jsonDecodeEvaluateExpressionArguments argumentsJson of
            Err decodeError ->
                Json.Encode.object
                    [ ( "FailedToDecodeArguments"
                      , Json.Encode.string (Json.Decode.errorToString decodeError)
                      )
                    ]

            Ok args ->
                Json.Encode.object
                    [ ( "DecodedArguments"
                      , case ElmEvaluation.evaluateExpressionString args.modulesTexts args.expression of
                            Err evaluateError ->
                                Json.Encode.object [ ( "FailedToEvaluate", Json.Encode.string evaluateError ) ]

                            Ok evalOk ->
                                Json.Encode.object
                                    [ ( "Evaluated"
                                      , Json.Encode.object
                                            [ ( "valueAsJsonString", Json.Encode.string evalOk.valueAsJsonString )
                                            , ( "typeText", Json.Encode.string evalOk.typeText )
                                            ]
                                      )
                                    ]
                      )
                    ]
        )


jsonDecodeEvaluateExpressionArguments : Json.Decode.Decoder EvaluateExpressionArguments
jsonDecodeEvaluateExpressionArguments =
    Json.Decode.map2 EvaluateExpressionArguments
        (Json.Decode.field "modulesTexts" (Json.Decode.list Json.Decode.string))
        (Json.Decode.field "expression" Json.Decode.string)


{-| Support function-level dead code elimination (<https://elm-lang.org/blog/small-assets-without-the-headache>)
Elm code needed to inform the Elm compiler about our entry points.
-}
main : Program Int () String
main =
    Platform.worker
        { init = \_ -> ( (), Cmd.none )
        , update =
            \event stateBefore ->
                ( parseElmModuleTextToJson (evaluateExpressionInProject "") |> always stateBefore, Cmd.none )
        , subscriptions = \_ -> Sub.none
        }
