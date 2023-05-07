module Backend.Main exposing
    ( State
    , webServiceMain
    )

import Backend.State
import Base64
import Bytes
import Bytes.Decode
import Bytes.Encode
import Calculator
import CompilationInterface.GenerateJsonConverters
import Json.Decode
import Json.Encode
import Platform.WebService


type alias State =
    Backend.State.State


webServiceMain : Platform.WebService.WebServiceConfig State
webServiceMain =
    { init =
        ( { httpRequestCount = 0
          , operationsViaHttpRequestCount = 0
          , resultingNumber = 0
          }
        , []
        )
    , subscriptions = subscriptions
    }


subscriptions : State -> Platform.WebService.Subscriptions State
subscriptions _ =
    { httpRequest = updateForHttpRequestEvent
    , posixTimeIsPast = Nothing
    }


updateForHttpRequestEvent : Platform.WebService.HttpRequestEventStruct -> State -> ( State, Platform.WebService.Commands State )
updateForHttpRequestEvent httpRequestEvent stateBeforeCountHttpRequest =
    let
        stateBefore =
            { stateBeforeCountHttpRequest
                | httpRequestCount = stateBeforeCountHttpRequest.httpRequestCount + 1
            }

        ( state, result ) =
            case
                httpRequestEvent.request.bodyAsBase64
                    |> Maybe.map (Base64.toBytes >> Maybe.map (decodeBytesToString >> Maybe.withDefault "Failed to decode bytes to string") >> Maybe.withDefault "Failed to decode from base64")
                    |> Maybe.withDefault "Missing HTTP body"
                    |> Json.Decode.decodeString CompilationInterface.GenerateJsonConverters.jsonDecodeCalculatorOperation
            of
                Err error ->
                    ( stateBefore
                    , Err
                        ("Failed to deserialize counter event from HTTP Request content: "
                            ++ Json.Decode.errorToString error
                            ++ "\nLast result number is "
                            ++ String.fromInt stateBefore.resultingNumber
                        )
                    )

                Ok counterEvent ->
                    let
                        resultingNumber =
                            stateBefore.resultingNumber
                                |> Calculator.applyCalculatorOperation counterEvent

                        stateAfterCalculatorOperation =
                            { stateBefore
                                | resultingNumber = resultingNumber
                                , operationsViaHttpRequestCount = stateBefore.operationsViaHttpRequestCount + 1
                            }
                    in
                    ( stateAfterCalculatorOperation
                    , stateAfterCalculatorOperation
                        |> CompilationInterface.GenerateJsonConverters.jsonEncodeBackendState
                        |> Json.Encode.encode 0
                        |> Ok
                    )

        ( httpResponseCode, httpResponseBodyString ) =
            case result of
                Err error ->
                    ( 400, error )

                Ok message ->
                    ( 200, message )

        httpResponse =
            { httpRequestId = httpRequestEvent.httpRequestId
            , response =
                { statusCode = httpResponseCode
                , bodyAsBase64 = httpResponseBodyString |> Bytes.Encode.string |> Bytes.Encode.encode |> Base64.fromBytes
                , headersToAdd = []
                }
            }
    in
    ( state
    , [ Platform.WebService.RespondToHttpRequest httpResponse ]
    )


decodeBytesToString : Bytes.Bytes -> Maybe String
decodeBytesToString bytes =
    bytes |> Bytes.Decode.decode (Bytes.Decode.string (bytes |> Bytes.width))
