module Backend.Main exposing
    ( State
    , backendMain
    )

import Base64
import Bytes.Encode
import ElmFullstack


type alias State =
    ()


backendMain : ElmFullstack.BackendConfiguration ()
backendMain =
    { init = ()
    , update = processEvent
    }


processEvent : ElmFullstack.BackendEvent -> State -> ( State, ElmFullstack.BackendEventResponse )
processEvent hostEvent stateBefore =
    case hostEvent of
        ElmFullstack.HttpRequestEvent httpRequestEvent ->
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
            ( stateBefore
            , ElmFullstack.passiveBackendEventResponse
                |> ElmFullstack.withCompleteHttpResponsesAdded
                    [ { httpRequestId = httpRequestEvent.httpRequestId
                      , response = httpResponse
                      }
                    ]
            )

        ElmFullstack.TaskCompleteEvent _ ->
            ( stateBefore, ElmFullstack.passiveBackendEventResponse )

        ElmFullstack.PosixTimeHasArrivedEvent _ ->
            ( stateBefore, ElmFullstack.passiveBackendEventResponse )
