module Backend.Main exposing
    ( State
    , webServiceMain
    )

import Base64
import Bytes
import Bytes.Encode
import Platform.WebService


type alias State =
    ()


webServiceMain : Platform.WebService.WebServiceConfig State
webServiceMain =
    { init = init
    , subscriptions = subscriptions
    }


init : ( State, Platform.WebService.Commands State )
init =
    ( (), [] )


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
        httpResponseBody : Bytes.Bytes
        httpResponseBody =
            "Hello, World!"
                |> Bytes.Encode.string
                |> Bytes.Encode.encode

        httpResponse : Platform.WebService.HttpResponse
        httpResponse =
            { statusCode = 200
            , body = Just httpResponseBody
            , headersToAdd = []
            }

        httpResponseCommand : Platform.WebService.RespondToHttpRequestStruct
        httpResponseCommand =
            { httpRequestId = httpRequestEvent.httpRequestId
            , response = httpResponse
            }
    in
    ( state
    , [ Platform.WebService.RespondToHttpRequest httpResponseCommand
      ]
    )
