module Backend.Main exposing
    ( State
    , backendMain
    )

import Base64
import Bytes.Encode
import ElmWebServer


type alias State =
    ()


backendMain : ElmWebServer.WebServerConfig ()
backendMain =
    { init = ( (), [] )
    , subscriptions = subscriptions
    }


subscriptions : State -> ElmWebServer.Subscriptions State
subscriptions _ =
    { httpRequest = updateForHttpRequestEvent
    , posixTimeIsPast = Nothing
    }


updateForHttpRequestEvent : ElmWebServer.HttpRequestEventStruct -> State -> ( State, ElmWebServer.Commands State )
updateForHttpRequestEvent httpRequestEvent state =
    let
        httpResponse =
            { statusCode = 200
            , bodyAsBase64 =
                "Hello, World!"
                    |> Bytes.Encode.string
                    |> Bytes.Encode.encode
                    |> Base64.fromBytes
            , headersToAdd = []
            }
    in
    ( state
    , [ ElmWebServer.RespondToHttpRequest
            { httpRequestId = httpRequestEvent.httpRequestId
            , response = httpResponse
            }
      ]
    )
