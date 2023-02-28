module Backend.Main exposing
    ( State
    , backendMain
    )

import Base64
import Bytes.Encode
import Platform.WebServer


type alias State =
    ()


backendMain : Platform.WebServer.WebServerConfig ()
backendMain =
    { init = ( (), [] )
    , subscriptions = subscriptions
    }


subscriptions : State -> Platform.WebServer.Subscriptions State
subscriptions _ =
    { httpRequest = updateForHttpRequestEvent
    , posixTimeIsPast = Nothing
    }


updateForHttpRequestEvent : Platform.WebServer.HttpRequestEventStruct -> State -> ( State, Platform.WebServer.Commands State )
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
    , [ Platform.WebServer.RespondToHttpRequest
            { httpRequestId = httpRequestEvent.httpRequestId
            , response = httpResponse
            }
      ]
    )
