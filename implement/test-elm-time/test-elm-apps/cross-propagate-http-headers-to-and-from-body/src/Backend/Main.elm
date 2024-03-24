module Backend.Main exposing
    ( State
    , webServiceMain
    )

import Base64
import Bytes
import Bytes.Decode
import Bytes.Encode
import Json.Encode
import Platform.WebService


type alias State =
    ()


webServiceMain : Platform.WebService.WebServiceConfig State
webServiceMain =
    { init = ( (), [] )
    , subscriptions = subscriptions
    }


subscriptions : State -> Platform.WebService.Subscriptions State
subscriptions _ =
    { httpRequest = updateForHttpRequestEvent
    , posixTimeIsPast = Nothing
    }


updateForHttpRequestEvent : Platform.WebService.HttpRequestEventStruct -> State -> ( State, Platform.WebService.Commands State )
updateForHttpRequestEvent httpRequestEvent stateBefore =
    let
        headerToPropagateBody =
            { name = "response-header-name"
            , values =
                [ httpRequestEvent.request.bodyAsBase64
                    |> Maybe.map (Base64.toBytes >> Maybe.map (decodeBytesToString >> Maybe.withDefault "Failed to decode bytes to string") >> Maybe.withDefault "Failed to decode from base64")
                    |> Maybe.withDefault ""
                ]
            }

        httpResponseBodyString =
            httpRequestEvent.request.headers
                |> Json.Encode.list
                    (\requestHeader ->
                        [ ( "name", requestHeader.name |> Json.Encode.string )
                        , ( "values", requestHeader.values |> Json.Encode.list Json.Encode.string )
                        ]
                            |> Json.Encode.object
                    )
                |> Json.Encode.encode 0

        httpResponse =
            { httpRequestId = httpRequestEvent.httpRequestId
            , response =
                { statusCode = 200
                , bodyAsBase64 = httpResponseBodyString |> Bytes.Encode.string |> Bytes.Encode.encode |> Base64.fromBytes
                , headersToAdd = [ headerToPropagateBody, { name = "content-type", values = [ "application/json" ] } ]
                }
            }
    in
    ( stateBefore
    , [ Platform.WebService.RespondToHttpRequest httpResponse ]
    )


decodeBytesToString : Bytes.Bytes -> Maybe String
decodeBytesToString bytes =
    bytes |> Bytes.Decode.decode (Bytes.Decode.string (bytes |> Bytes.width))
