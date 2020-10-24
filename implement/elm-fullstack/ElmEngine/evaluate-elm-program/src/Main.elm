module Main exposing (..)

import Elm.Syntax.File
import ElmEvaluation
import Json.Decode
import Json.Encode
import Parser
import Platform


type alias EvaluateSubmissionArguments =
    { modulesTexts : List String
    , submission : String
    , previousLocalSubmissions : List String
    }


parseElmModuleTextToJson : String -> String
parseElmModuleTextToJson =
    ElmEvaluation.parseElmModuleTextToJson


parseElmModuleText : String -> Result (List Parser.DeadEnd) Elm.Syntax.File.File
parseElmModuleText =
    ElmEvaluation.parseElmModuleText


evaluateSubmissionInInteractive : String -> String
evaluateSubmissionInInteractive argumentsJson =
    Json.Encode.encode 0
        (case Json.Decode.decodeString jsonDecodeEvaluateSubmissionArguments argumentsJson of
            Err decodeError ->
                Json.Encode.object
                    [ ( "FailedToDecodeArguments"
                      , Json.Encode.string (Json.Decode.errorToString decodeError)
                      )
                    ]

            Ok args ->
                Json.Encode.object
                    [ ( "DecodedArguments"
                      , case
                            ElmEvaluation.evaluateSubmissionStringInInteractive
                                args.modulesTexts
                                args.previousLocalSubmissions
                                args.submission
                        of
                            Err evaluateError ->
                                Json.Encode.object [ ( "FailedToEvaluate", Json.Encode.string evaluateError ) ]

                            Ok evalOk ->
                                Json.Encode.object
                                    [ ( "Evaluated", jsonEncodeSubmissionResponse evalOk ) ]
                      )
                    ]
        )


jsonEncodeSubmissionResponse : ElmEvaluation.SubmissionResponse -> Json.Encode.Value
jsonEncodeSubmissionResponse submissionResponse =
    case submissionResponse of
        ElmEvaluation.SubmissionResponseNoValue ->
            Json.Encode.object
                [ ( "SubmissionResponseNoValue", Json.Encode.list (always Json.Encode.null) [] )
                ]

        ElmEvaluation.SubmissionResponseValue evalOk ->
            Json.Encode.object
                [ ( "SubmissionResponseValue"
                  , Json.Encode.object
                        [ ( "valueAsJsonString", Json.Encode.string evalOk.valueAsJsonString )
                        , ( "typeText", Json.Encode.string evalOk.typeText )
                        ]
                  )
                ]


jsonDecodeEvaluateSubmissionArguments : Json.Decode.Decoder EvaluateSubmissionArguments
jsonDecodeEvaluateSubmissionArguments =
    Json.Decode.map3 EvaluateSubmissionArguments
        (Json.Decode.field "modulesTexts" (Json.Decode.list Json.Decode.string))
        (Json.Decode.field "submission" Json.Decode.string)
        (Json.Decode.field "previousLocalSubmissions" (Json.Decode.list Json.Decode.string))


{-| Support function-level dead code elimination (<https://elm-lang.org/blog/small-assets-without-the-headache>)
Elm code needed to inform the Elm compiler about our entry points.
-}
main : Program Int () String
main =
    Platform.worker
        { init = \_ -> ( (), Cmd.none )
        , update =
            \_ stateBefore ->
                ( parseElmModuleTextToJson (evaluateSubmissionInInteractive "") |> always stateBefore, Cmd.none )
        , subscriptions = \_ -> Sub.none
        }
