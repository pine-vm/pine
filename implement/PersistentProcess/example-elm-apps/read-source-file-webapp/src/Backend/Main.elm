module Backend.Main exposing
    ( State
    , interfaceToHost_initState
    , interfaceToHost_processEvent
    )

import Backend.InterfaceToHost as InterfaceToHost
import Base64
import ElmFullstackCompilerInterface.SourceFiles


type alias State =
    {}


processEvent : InterfaceToHost.AppEvent -> State -> ( State, InterfaceToHost.AppEventResponse )
processEvent hostEvent stateBefore =
    case hostEvent of
        InterfaceToHost.HttpRequestEvent httpRequestEvent ->
            let
                response =
                    if (httpRequestEvent.request.method |> String.toLower) /= "get" then
                        { statusCode = 405
                        , bodyAsBase64 = Nothing
                        , headersToAdd = []
                        }

                    else
                        { statusCode = 200
                        , bodyAsBase64 = ElmFullstackCompilerInterface.SourceFiles.file____static_content_demo_file_mp3 |> Base64.fromBytes
                        , headersToAdd =
                            [ { name = "Cache-Control", values = [ "public, max-age=3600" ] }
                            ]
                        }

                httpResponse =
                    { httpRequestId = httpRequestEvent.httpRequestId
                    , response = response
                    }
            in
            ( stateBefore
            , InterfaceToHost.passiveAppEventResponse
                |> InterfaceToHost.withCompleteHttpResponsesAdded [ httpResponse ]
            )

        InterfaceToHost.TaskCompleteEvent _ ->
            ( stateBefore, InterfaceToHost.passiveAppEventResponse )

        InterfaceToHost.ArrivedAtTimeEvent _ ->
            ( stateBefore, InterfaceToHost.passiveAppEventResponse )


interfaceToHost_initState : State
interfaceToHost_initState =
    State


interfaceToHost_processEvent : String -> State -> ( State, String )
interfaceToHost_processEvent =
    InterfaceToHost.wrapForSerialInterface_processEvent processEvent
