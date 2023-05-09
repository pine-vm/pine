module Backend.Main exposing
    ( State
    , webServiceMain
    )

import Base64
import Bytes
import Bytes.Decode
import Bytes.Encode
import Platform.WebService


type alias State =
    String


webServiceMain : Platform.WebService.WebServiceConfig State
webServiceMain =
    { init = ( "", [] )
    , subscriptions = subscriptions
    }


subscriptions : State -> Platform.WebService.Subscriptions State
subscriptions _ =
    { httpRequest = updateForHttpRequestEvent
    , posixTimeIsPast = Nothing
    }


updateForHttpRequestEvent : Platform.WebService.HttpRequestEventStruct -> State -> ( State, Platform.WebService.Commands State )
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
    , [ Platform.WebService.RespondToHttpRequest httpResponse ]
    )


decodeBytesToString : Bytes.Bytes -> Maybe String
decodeBytesToString bytes =
    bytes |> Bytes.Decode.decode (Bytes.Decode.string (bytes |> Bytes.width))
