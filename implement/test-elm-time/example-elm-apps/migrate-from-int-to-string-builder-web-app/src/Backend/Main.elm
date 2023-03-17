module Backend.Main exposing
    ( State
    , webServerMain
    )

import Base64
import Bytes
import Bytes.Decode
import Bytes.Encode
import Platform.WebServer


type alias State =
    String


webServerMain : Platform.WebServer.WebServerConfig State
webServerMain =
    { init = ( "", [] )
    , subscriptions = subscriptions
    }


subscriptions : State -> Platform.WebServer.Subscriptions State
subscriptions _ =
    { httpRequest = updateForHttpRequestEvent
    , posixTimeIsPast = Nothing
    }


updateForHttpRequestEvent : Platform.WebServer.HttpRequestEventStruct -> State -> ( State, Platform.WebServer.Commands State )
updateForHttpRequestEvent event stateBefore =
    let
        state =
            case event.request.method |> String.toLower of
                "get" ->
                    stateBefore

                "post" ->
                    let
                        addition =
                            event.request.bodyAsBase64
                                |> Maybe.map (Base64.toBytes >> Maybe.map (decodeBytesToString >> Maybe.withDefault "Failed to decode bytes to string") >> Maybe.withDefault "Failed to decode from base64")
                                |> Maybe.withDefault ""
                    in
                    stateBefore ++ addition

                _ ->
                    stateBefore

        httpResponse =
            { httpRequestId = event.httpRequestId
            , response =
                { statusCode = 200
                , bodyAsBase64 = state |> Bytes.Encode.string |> Bytes.Encode.encode |> Base64.fromBytes
                , headersToAdd = []
                }
            }
    in
    ( state
    , [ Platform.WebServer.RespondToHttpRequest httpResponse ]
    )


decodeBytesToString : Bytes.Bytes -> Maybe String
decodeBytesToString bytes =
    bytes |> Bytes.Decode.decode (Bytes.Decode.string (bytes |> Bytes.width))
