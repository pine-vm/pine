module Backend.Main exposing
    ( State
    , interfaceToHost_initState
    , interfaceToHost_processEvent
    )

import Backend.InterfaceToHost as InterfaceToHost
import ElmFullstackCompilerInterface.SourceFiles


type alias State =
    {}


processEvent : InterfaceToHost.ProcessEvent -> State -> ( State, List InterfaceToHost.ProcessRequest )
processEvent hostEvent stateBefore =
    case hostEvent of
        InterfaceToHost.HttpRequest httpRequestEvent ->
            let
                response =
                    if (httpRequestEvent.request.method |> String.toLower) /= "get" then
                        { statusCode = 405
                        , body = Nothing
                        , headersToAdd = []
                        }

                    else
                        { statusCode = 200
                        , body = Just ElmFullstackCompilerInterface.SourceFiles.file____static_content_demo_file_mp3
                        , headersToAdd =
                            [ { name = "Cache-Control", values = [ "public, max-age=3600" ] }
                            ]
                        }

                httpResponse =
                    { httpRequestId = httpRequestEvent.httpRequestId
                    , response = response
                    }
                        |> InterfaceToHost.CompleteHttpResponse
            in
            ( stateBefore, [ httpResponse ] )

        InterfaceToHost.TaskComplete _ ->
            ( stateBefore, [] )


interfaceToHost_initState : State
interfaceToHost_initState =
    State


interfaceToHost_processEvent : String -> State -> ( State, String )
interfaceToHost_processEvent =
    InterfaceToHost.wrapForSerialInterface_processEvent processEvent
