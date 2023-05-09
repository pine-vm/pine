module Backend.Main exposing
    ( State
    , webServiceMain
    )

import Backend.StateType
import Base64
import Bytes
import Bytes.Decode
import Bytes.Encode
import CompilationInterface.GenerateJsonConverters
import Json.Decode
import Json.Encode
import Platform.WebService


type alias State =
    Backend.StateType.State


webServiceMain : Platform.WebService.WebServiceConfig State
webServiceMain =
    { init = ( initState, [] )
    , subscriptions = subscriptions
    }


initState : State
initState =
    { maybeString = Nothing
    , otherState = ""
    }


subscriptions : State -> Platform.WebService.Subscriptions State
subscriptions _ =
    { httpRequest = updateForHttpRequestEvent
    , posixTimeIsPast = Nothing
    }


updateForHttpRequestEvent : Platform.WebService.HttpRequestEventStruct -> State -> ( State, Platform.WebService.Commands State )
updateForHttpRequestEvent httpRequestEvent stateBefore =
    let
        ( state, httpResponseCode, httpResponseBodyString ) =
            case httpRequestEvent.request.method |> String.toLower of
                "get" ->
                    ( stateBefore
                    , 200
                    , stateBefore
                        |> CompilationInterface.GenerateJsonConverters.encodeBackendState
                        |> Json.Encode.encode 0
                    )

                "post" ->
                    case
                        httpRequestEvent.request.bodyAsBase64
                            |> Maybe.map (Base64.toBytes >> Maybe.map (decodeBytesToString >> Maybe.withDefault "Failed to decode bytes to string") >> Maybe.withDefault "Failed to decode from base64")
                            |> Maybe.withDefault "Missing HTTP body"
                            |> Json.Decode.decodeString CompilationInterface.GenerateJsonConverters.decodeBackendState
                    of
                        Err decodeErr ->
                            ( stateBefore
                            , 400
                            , "Failed to decode state:\n" ++ (decodeErr |> Json.Decode.errorToString)
                            )

                        Ok decodedState ->
                            ( decodedState
                            , 200
                            , "Successfully set state"
                            )

                _ ->
                    ( stateBefore, 405, "Method not supported" )

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
