module Backend.Main exposing
    ( State
    , backendMain
    )

import Base64
import Bytes
import Bytes.Decode
import Bytes.Encode
import ElmFullstack


type alias State =
    String


backendMain : ElmFullstack.BackendConfig State
backendMain =
    { init = ( "", [] )
    , subscriptions = subscriptions
    }


subscriptions : State -> ElmFullstack.BackendSubs State
subscriptions _ =
    { httpRequest = updateForHttpRequestEvent
    , posixTimeIsPast = Nothing
    }


updateForHttpRequestEvent : ElmFullstack.HttpRequestEventStruct -> State -> ( State, ElmFullstack.BackendCmds State )
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
    , [ ElmFullstack.RespondToHttpRequest httpResponse ]
    )


decodeBytesToString : Bytes.Bytes -> Maybe String
decodeBytesToString bytes =
    bytes |> Bytes.Decode.decode (Bytes.Decode.string (bytes |> Bytes.width))
