module ElmInteractiveMain exposing (..)

import Base64
import Bytes
import Dict
import Elm.Syntax.File
import ElmCompiler
import ElmInteractive
import ElmInteractiveCoreModules
import ElmInteractiveKernelModules
import ElmInteractiveParser
import EncodeElmSyntaxAsPineValue
import Json.Decode
import Json.Encode
import Parser
import Pine
import Platform


type alias EvaluateSubmissionArguments =
    { modulesTexts : List String
    , submission : String
    , previousLocalSubmissions : List String
    }


type alias CompileElmInteractiveEnvironmentRequest =
    { environmentBefore : Pine.Value
    , modulesTexts : List String
    }


parseElmModuleTextToJson : String -> String
parseElmModuleTextToJson =
    ElmInteractiveParser.parseElmModuleTextToJson


parseElmModuleTextToPineValue : String -> String
parseElmModuleTextToPineValue moduleTextJson =
    {- 2024-01-21:
       When using the plain Elm module text as parameter, some characters like emojis arrived in escaped form.
       Therefore we use the Json.Decode.string decoder to decode the module text.
    -}
    let
        result =
            moduleTextJson
                |> Json.Decode.decodeString Json.Decode.string
                |> Result.mapError Json.Decode.errorToString
                |> Result.andThen ElmInteractiveParser.parseElmModuleTextToPineValue
    in
    result
        |> json_encode_Result Json.Encode.string (ElmInteractive.json_encode_pineValue Dict.empty)
        |> Json.Encode.encode 0


parseElmModuleText : String -> Result (List Parser.DeadEnd) Elm.Syntax.File.File
parseElmModuleText =
    ElmInteractiveParser.parseElmModuleText


parseInteractiveSubmissionToPineValue : String -> String
parseInteractiveSubmissionToPineValue submission =
    ElmInteractiveParser.parseInteractiveSubmissionFromString submission
        |> Result.andThen EncodeElmSyntaxAsPineValue.encodeInteractiveSubmissionAsPineValue
        |> Result.map (json_encode_pineValue Dict.empty)
        |> json_encode_Result Json.Encode.string identity
        |> Json.Encode.encode 0


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
                            ElmInteractiveParser.submissionInInteractive
                                (ElmInteractive.CustomModulesContext
                                    { includeCoreModules = Just ElmInteractive.CoreAndOtherKernelModules
                                    , modulesTexts = args.modulesTexts
                                    }
                                )
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


getDefaultElmCoreModulesTexts : String -> String
getDefaultElmCoreModulesTexts _ =
    [ ElmInteractiveCoreModules.elmCoreModulesTexts
    , ElmInteractiveKernelModules.elmKernelModulesTexts
    ]
        |> List.concat
        |> Json.Encode.list Json.Encode.string
        |> Json.Encode.encode 0


compileInteractiveEnvironment : String -> String
compileInteractiveEnvironment =
    Json.Decode.decodeString json_Decode_compileInteractiveEnvironment
        >> Result.mapError (Json.Decode.errorToString >> (++) "Failed to decode arguments: ")
        >> Result.andThen
            (\( environmentDict, { modulesTexts, environmentBefore } ) ->
                ElmInteractiveParser.expandElmInteractiveEnvironmentWithModuleTexts
                    modulesTexts
                    |> Result.andThen
                        (\( _, addModules ) -> addModules environmentBefore)
                    |> Result.mapError ((++) "Failed to prepare the initial context: ")
                    |> Result.map (.environment >> json_encode_pineValue environmentDict)
            )
        >> json_encode_Result Json.Encode.string identity
        >> Json.Encode.encode 0


compileInteractiveSubmission : String -> String
compileInteractiveSubmission requestJson =
    requestJson
        |> Json.Decode.decodeString json_Decode_compileInteractiveSubmission
        |> Result.mapError (Json.Decode.errorToString >> (++) "Failed to decode request: ")
        |> Result.andThen
            (\( ( environment, environmentDict ), submission ) ->
                ElmInteractiveParser.compileInteractiveSubmission environment submission
                    |> Result.map Pine.encodeExpressionAsValue
                    |> Result.map (json_encode_pineValue environmentDict)
            )
        |> json_encode_Result Json.Encode.string identity
        |> Json.Encode.encode 0


submissionResponseFromResponsePineValue : String -> String
submissionResponseFromResponsePineValue response =
    response
        |> Json.Decode.decodeString json_decode_pineValue
        |> Result.mapError Json.Decode.errorToString
        |> Result.andThen (Tuple.first >> ElmInteractive.submissionResponseFromResponsePineValue)
        |> json_encode_Result Json.Encode.string jsonEncodeSubmissionResponse
        |> Json.Encode.encode 0


jsonEncodeSubmissionResponse : ElmInteractive.SubmissionResponse -> Json.Encode.Value
jsonEncodeSubmissionResponse submissionResponse =
    Json.Encode.object
        [ ( "displayText"
          , Json.Encode.string submissionResponse.displayText
          )
        ]


jsonDecodeEvaluateSubmissionArguments : Json.Decode.Decoder EvaluateSubmissionArguments
jsonDecodeEvaluateSubmissionArguments =
    Json.Decode.map3 EvaluateSubmissionArguments
        (Json.Decode.field "modulesTexts" (Json.Decode.list Json.Decode.string))
        (Json.Decode.field "submission" Json.Decode.string)
        (Json.Decode.field "previousLocalSubmissions" (Json.Decode.list Json.Decode.string))


json_Decode_compileInteractiveEnvironment :
    Json.Decode.Decoder
        ( Dict.Dict String Pine.Value
        , CompileElmInteractiveEnvironmentRequest
        )
json_Decode_compileInteractiveEnvironment =
    Json.Decode.map2
        (\( environmentBefore, environmentDictionary ) modulesTexts ->
            ( environmentDictionary, { environmentBefore = environmentBefore, modulesTexts = modulesTexts } )
        )
        (Json.Decode.field "environmentBefore" json_decode_pineValue)
        (Json.Decode.field "modulesTexts" (Json.Decode.list Json.Decode.string))


json_Decode_compileInteractiveSubmission : Json.Decode.Decoder ( ( Pine.Value, Dict.Dict String Pine.Value ), String )
json_Decode_compileInteractiveSubmission =
    Json.Decode.map2
        (\environment submission -> ( environment, submission ))
        (Json.Decode.field "environment" json_decode_pineValue)
        (Json.Decode.field "submission" Json.Decode.string)


json_encode_pineValue : Dict.Dict String Pine.Value -> Pine.Value -> Json.Encode.Value
json_encode_pineValue =
    ElmInteractive.json_encode_pineValue


json_decode_pineValue : Json.Decode.Decoder ( Pine.Value, Dict.Dict String Pine.Value )
json_decode_pineValue =
    ElmInteractive.json_decode_pineValue


{-| Support function-level dead code elimination (<https://elm-lang.org/blog/small-assets-without-the-headache>)
Elm code needed to inform the Elm compiler about our entry points.
-}
main : Program Int () String
main =
    Platform.worker
        { init = always ( (), Cmd.none )
        , update =
            { a = parseElmModuleTextToJson
            , b = evaluateSubmissionInInteractive
            , c = compileInteractiveSubmission
            , d = compileInteractiveEnvironment
            , e = submissionResponseFromResponsePineValue
            , f = getDefaultElmCoreModulesTexts
            , g = parseElmModuleTextToPineValue
            , h = parseInteractiveSubmissionToPineValue
            , i = ElmCompiler.compileAndEvalParsedInteractiveSubmission
            }
                |> always ( (), Cmd.none )
                |> always
                |> always
        , subscriptions = always Sub.none
        }


json_encode_Result : (err -> Json.Encode.Value) -> (ok -> Json.Encode.Value) -> Result err ok -> Json.Encode.Value
json_encode_Result encodeErr encodeOk valueToEncode =
    case valueToEncode of
        Err valueToEncodeError ->
            [ ( "Err", [ valueToEncodeError ] |> Json.Encode.list encodeErr ) ] |> Json.Encode.object

        Ok valueToEncodeOk ->
            [ ( "Ok", [ valueToEncodeOk ] |> Json.Encode.list encodeOk ) ] |> Json.Encode.object


json_decode_Result : Json.Decode.Decoder err -> Json.Decode.Decoder ok -> Json.Decode.Decoder (Result err ok)
json_decode_Result decodeErr decodeOk =
    Json.Decode.oneOf
        [ Json.Decode.field "Err" (Json.Decode.index 0 decodeErr) |> Json.Decode.map Err
        , Json.Decode.field "Ok" (Json.Decode.index 0 decodeOk) |> Json.Decode.map Ok
        ]


json_encode_Bytes : Bytes.Bytes -> Json.Encode.Value
json_encode_Bytes bytes =
    [ ( "AsBase64", bytes |> Base64.fromBytes |> Maybe.withDefault "Error encoding to base64" |> Json.Encode.string ) ]
        |> Json.Encode.object


json_decode_Bytes : Json.Decode.Decoder Bytes.Bytes
json_decode_Bytes =
    Json.Decode.field "AsBase64"
        (Json.Decode.string
            |> Json.Decode.andThen
                (Base64.toBytes >> Maybe.map Json.Decode.succeed >> Maybe.withDefault (Json.Decode.fail "Failed to decode base64."))
        )
