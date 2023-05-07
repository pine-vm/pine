module Backend.Main exposing
    ( State
    , webServiceMain
    )

import Base64
import Bytes.Encode
import Platform.WebService


type alias State =
    ()


webServiceMain : Platform.WebService.WebServiceConfig ()
webServiceMain =
    { init = ( (), [] )
    , subscriptions = subscriptions
    }


subscriptions : State -> Platform.WebService.Subscriptions State
subscriptions _ =
    { httpRequest = updateForHttpRequestEvent
    , posixTimeIsPast = Nothing
    }


updateForHttpRequestEvent :
    Platform.WebService.HttpRequestEventStruct
    -> State
    -> ( State, Platform.WebService.Commands State )
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
    , [ Platform.WebService.RespondToHttpRequest
            { httpRequestId = httpRequestEvent.httpRequestId
            , response = httpResponse
            }
      ]
    )
