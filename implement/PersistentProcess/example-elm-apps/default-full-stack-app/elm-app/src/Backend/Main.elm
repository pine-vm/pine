module Backend.Main exposing
    ( State
    , interfaceToHost_initState
    , interfaceToHost_processEvent
    , processEvent
    )

import Backend.InterfaceToHost as InterfaceToHost


type alias State =
    { httpRequestsCount : Int }


interfaceToHost_processEvent : String -> State -> ( State, String )
interfaceToHost_processEvent =
    InterfaceToHost.wrapForSerialInterface_processEvent processEvent


processEvent : InterfaceToHost.ProcessEvent -> State -> ( State, List InterfaceToHost.ProcessRequest )
processEvent hostEvent stateBefore =
    case hostEvent of
        InterfaceToHost.HttpRequest httpRequestEvent ->
            let
                state =
                    { stateBefore | httpRequestsCount = stateBefore.httpRequestsCount + 1 }

                httpResponse =
                    { httpRequestId = httpRequestEvent.httpRequestId
                    , response =
                        { statusCode = 200
                        , bodyAsString =
                            Just
                                ("You are seeing the default configuration for the web app."
                                    ++ "\nFor information on how to configure the web app, see https://github.com/elm-fullstack/elm-fullstack"
                                    ++ "\nThis web app received "
                                    ++ (state.httpRequestsCount |> String.fromInt)
                                    ++ " HTTP requests."
                                )
                        , headersToAdd = []
                        }
                    }
                        |> InterfaceToHost.CompleteHttpResponse
            in
            ( state, [ httpResponse ] )

        InterfaceToHost.TaskComplete _ ->
            ( stateBefore, [] )


interfaceToHost_initState : State
interfaceToHost_initState =
    { httpRequestsCount = 0 }
