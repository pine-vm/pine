module Backend.Main exposing
    ( State
    , interfaceToHost_initState
    , interfaceToHost_processEvent
    , processEvent
    )

import Backend.InterfaceToHost as InterfaceToHost
import Bytes
import Bytes.Decode
import Bytes.Encode


type alias State =
    String


processEvent : InterfaceToHost.ProcessEvent -> State -> ( State, List InterfaceToHost.ProcessRequest )
processEvent hostEvent stateBefore =
    case hostEvent of
        InterfaceToHost.HttpRequest httpRequestEvent ->
            let
                state =
                    case httpRequestEvent.request.method |> String.toLower of
                        "get" ->
                            stateBefore

                        "post" ->
                            stateBefore
                                ++ (httpRequestEvent.request.body
                                        |> Maybe.andThen (\bytes -> bytes |> Bytes.Decode.decode (Bytes.Decode.string (bytes |> Bytes.width)))
                                        |> Maybe.withDefault ""
                                   )

                        _ ->
                            stateBefore

                httpResponse =
                    { httpRequestId = httpRequestEvent.httpRequestId
                    , response =
                        { statusCode = 200
                        , body =
                            [ "The request uri was: " ++ httpRequestEvent.request.uri
                            , "The current state is:" ++ state
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
    ""


interfaceToHost_processEvent : String -> State -> ( State, String )
interfaceToHost_processEvent =
    InterfaceToHost.wrapForSerialInterface_processEvent processEvent
