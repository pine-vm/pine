module Backend.Main exposing
    ( State
    , interfaceToHost_initState
    , interfaceToHost_processEvent
    , processEvent
    )

import Backend.InterfaceToHost as InterfaceToHost
import Bytes.Encode
import Common


type alias State =
    { httpRequestsCount : Int
    , lastHttpRequests : List InterfaceToHost.HttpRequestEvent
    }


interfaceToHost_processEvent : String -> State -> ( State, String )
interfaceToHost_processEvent =
    InterfaceToHost.wrapForSerialInterface_processEvent processEvent


processEvent : InterfaceToHost.ProcessEvent -> State -> ( State, List InterfaceToHost.ProcessRequest )
processEvent hostEvent stateBefore =
    case hostEvent of
        InterfaceToHost.HttpRequest httpRequestEvent ->
            let
                state =
                    { stateBefore
                        | httpRequestsCount = stateBefore.httpRequestsCount + 1
                        , lastHttpRequests = httpRequestEvent :: stateBefore.lastHttpRequests |> List.take 4
                    }

                httpResponse =
                    { httpRequestId = httpRequestEvent.httpRequestId
                    , response =
                        { statusCode = 200
                        , body =
                            [ Common.guideMarkdown
                            , ""
                            , "This backend process received " ++ (state.httpRequestsCount |> String.fromInt) ++ " HTTP requests."
                            ]
                                |> String.join "\n"
                                |> Bytes.Encode.string
                                |> Bytes.Encode.encode
                                |> Just
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
    { httpRequestsCount = 0
    , lastHttpRequests = []
    }
