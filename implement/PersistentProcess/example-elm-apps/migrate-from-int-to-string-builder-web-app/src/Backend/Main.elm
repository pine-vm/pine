module Backend.Main exposing
    ( State
    , interfaceToHost_initState
    , interfaceToHost_processEvent
    )

import Backend.InterfaceToHost as InterfaceToHost
import Base64
import Bytes
import Bytes.Decode
import Bytes.Encode


type alias State =
    String


processEvent : InterfaceToHost.AppEvent -> State -> ( State, InterfaceToHost.AppEventResponse )
processEvent hostEvent stateBefore =
    case hostEvent of
        InterfaceToHost.HttpRequestEvent httpRequestEvent ->
            let
                state =
                    case httpRequestEvent.request.method |> String.toLower of
                        "get" ->
                            stateBefore

                        "post" ->
                            let
                                addition =
                                    httpRequestEvent.request.bodyAsBase64
                                        |> Maybe.map (Base64.toBytes >> Maybe.map (decodeBytesToString >> Maybe.withDefault "Failed to decode bytes to string") >> Maybe.withDefault "Failed to decode from base64")
                                        |> Maybe.withDefault ""
                            in
                            stateBefore ++ addition

                        _ ->
                            stateBefore

                httpResponse =
                    { httpRequestId = httpRequestEvent.httpRequestId
                    , response =
                        { statusCode = 200
                        , bodyAsBase64 = state |> Bytes.Encode.string |> Bytes.Encode.encode |> Base64.fromBytes
                        , headersToAdd = []
                        }
                    }
            in
            ( state
            , InterfaceToHost.passiveAppEventResponse
                |> InterfaceToHost.withCompleteHttpResponsesAdded [ httpResponse ]
            )

        InterfaceToHost.TaskCompleteEvent _ ->
            ( stateBefore, InterfaceToHost.passiveAppEventResponse )

        InterfaceToHost.ArrivedAtTimeEvent _ ->
            ( stateBefore, InterfaceToHost.passiveAppEventResponse )


decodeBytesToString : Bytes.Bytes -> Maybe String
decodeBytesToString bytes =
    bytes |> Bytes.Decode.decode (Bytes.Decode.string (bytes |> Bytes.width))


interfaceToHost_initState : State
interfaceToHost_initState =
    ""


interfaceToHost_processEvent : String -> State -> ( State, String )
interfaceToHost_processEvent =
    InterfaceToHost.wrapForSerialInterface_processEvent processEvent
