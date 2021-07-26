module Backend.Main exposing
    ( State
    , interfaceToHost_initState
    , interfaceToHost_processEvent
    , processEvent
    )

import Backend.InterfaceToHost as InterfaceToHost
import Base64
import Bytes.Encode


type alias State =
    ()


interfaceToHost_processEvent : String -> State -> ( State, String )
interfaceToHost_processEvent =
    InterfaceToHost.wrapForSerialInterface_processEvent processEvent


processEvent : InterfaceToHost.AppEvent -> State -> ( State, InterfaceToHost.AppEventResponse )
processEvent hostEvent stateBefore =
    case hostEvent of
        InterfaceToHost.HttpRequestEvent httpRequestEvent ->
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
            , InterfaceToHost.passiveAppEventResponse
                |> InterfaceToHost.withCompleteHttpResponsesAdded
                    [ { httpRequestId = httpRequestEvent.httpRequestId
                      , response = httpResponse
                      }
                    ]
            )

        InterfaceToHost.TaskCompleteEvent _ ->
            ( stateBefore, InterfaceToHost.passiveAppEventResponse )

        InterfaceToHost.ArrivedAtTimeEvent _ ->
            ( stateBefore, InterfaceToHost.passiveAppEventResponse )


interfaceToHost_initState : State
interfaceToHost_initState =
    ()
