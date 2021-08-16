module Backend.Main exposing
    ( State
    , backendMain
    )

import Base64
import Bytes.Encode
import ElmFullstack


type alias State =
    ()


backendMain : ElmFullstack.BackendConfig ()
backendMain =
    { init = ( (), [] )
    , subscriptions = subscriptions
    }


subscriptions : State -> ElmFullstack.BackendSubs State
subscriptions _ =
    { httpRequest = updateForHttpRequestEvent
    , posixTimeIsPast = Nothing
    }


updateForHttpRequestEvent : ElmFullstack.HttpRequestEventStruct -> State -> ( State, ElmFullstack.BackendCmds State )
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
    , [ ElmFullstack.RespondToHttpRequest
            { httpRequestId = httpRequestEvent.httpRequestId
            , response = httpResponse
            }
      ]
    )
