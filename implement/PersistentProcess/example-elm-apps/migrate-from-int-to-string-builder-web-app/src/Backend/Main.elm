module Backend.Main exposing
    ( State
    , interfaceToHost_initState
    , interfaceToHost_processEvent
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
                            stateBefore ++ (httpRequestEvent.request.body |> Maybe.andThen decodeBytesToString |> Maybe.withDefault "")

                        _ ->
                            stateBefore

                httpResponse =
                    { httpRequestId = httpRequestEvent.httpRequestId
                    , response =
                        { statusCode = 200
                        , body = state |> Bytes.Encode.string |> Bytes.Encode.encode |> Just
                        , headersToAdd = []
                        }
                    }
                        |> InterfaceToHost.CompleteHttpResponse
            in
            ( state, [ httpResponse ] )

        InterfaceToHost.TaskComplete _ ->
            ( stateBefore, [] )


decodeBytesToString : Bytes.Bytes -> Maybe String
decodeBytesToString bytes =
    bytes |> Bytes.Decode.decode (Bytes.Decode.string (bytes |> Bytes.width))


interfaceToHost_initState : State
interfaceToHost_initState =
    ""


interfaceToHost_processEvent : String -> State -> ( State, String )
interfaceToHost_processEvent =
    InterfaceToHost.wrapForSerialInterface_processEvent processEvent
