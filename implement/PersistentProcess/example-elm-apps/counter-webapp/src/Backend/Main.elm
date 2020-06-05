module Backend.Main exposing
    ( State
    , interfaceToHost_initState
    , interfaceToHost_processEvent
    )

import Backend.InterfaceToHost as InterfaceToHost
import Bytes
import Bytes.Decode
import Bytes.Encode
import Json.Decode


type alias State =
    Int


type alias CounterEvent =
    { addition : Int }


processEvent : InterfaceToHost.ProcessEvent -> State -> ( State, List InterfaceToHost.ProcessRequest )
processEvent hostEvent stateBefore =
    case hostEvent of
        InterfaceToHost.HttpRequest httpRequestEvent ->
            let
                ( state, result ) =
                    case
                        httpRequestEvent.request.body
                            |> Maybe.map (decodeBytesToString >> Maybe.withDefault "Failed to decode bytes to string")
                            |> Maybe.withDefault "Missing HTTP body"
                            |> deserializeCounterEvent
                    of
                        Err error ->
                            ( stateBefore, Err ("Failed to deserialize counter event from HTTP Request content: " ++ error) )

                        Ok counterEvent ->
                            stateBefore |> processCounterEvent counterEvent |> Tuple.mapSecond Ok

                ( httpResponseCode, httpResponseBodyString ) =
                    case result of
                        Err error ->
                            ( 400, error )

                        Ok message ->
                            ( 200, message )

                httpResponse =
                    { httpRequestId = httpRequestEvent.httpRequestId
                    , response =
                        { statusCode = httpResponseCode
                        , body = httpResponseBodyString |> Bytes.Encode.string |> Bytes.Encode.encode |> Just
                        , headersToAdd = []
                        }
                    }
                        |> InterfaceToHost.CompleteHttpResponse
            in
            ( state, [ httpResponse ] )

        InterfaceToHost.TaskComplete _ ->
            ( stateBefore, [] )


processCounterEvent : CounterEvent -> State -> ( State, String )
processCounterEvent counterEvent stateBefore =
    let
        state =
            stateBefore + counterEvent.addition
    in
    ( state, state |> String.fromInt )


deserializeCounterEvent : String -> Result String CounterEvent
deserializeCounterEvent serializedEvent =
    serializedEvent
        |> Json.Decode.decodeString (Json.Decode.field "addition" Json.Decode.int |> Json.Decode.map CounterEvent)
        |> Result.mapError Json.Decode.errorToString


decodeBytesToString : Bytes.Bytes -> Maybe String
decodeBytesToString bytes =
    bytes |> Bytes.Decode.decode (Bytes.Decode.string (bytes |> Bytes.width))


interfaceToHost_initState : State
interfaceToHost_initState =
    0


interfaceToHost_processEvent : String -> State -> ( State, String )
interfaceToHost_processEvent =
    InterfaceToHost.wrapForSerialInterface_processEvent processEvent
