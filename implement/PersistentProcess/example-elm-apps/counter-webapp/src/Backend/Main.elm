module Backend.Main exposing
    ( State
    , interfaceToHost_deserializeState
    , interfaceToHost_initState
    , interfaceToHost_processEvent
    , interfaceToHost_serializeState
    )

import Backend.InterfaceToHost as InterfaceToHost
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
                    case (httpRequestEvent.request.bodyAsString |> Maybe.withDefault "") |> deserializeCounterEvent of
                        Err error ->
                            ( stateBefore, Err ("Failed to deserialize counter event from HTTP Request content: " ++ error) )

                        Ok counterEvent ->
                            stateBefore |> processCounterEvent counterEvent |> Tuple.mapSecond Ok

                ( httpResponseCode, httpResponseBody ) =
                    case result of
                        Err error ->
                            ( 400, error )

                        Ok message ->
                            ( 200, message )

                httpResponse =
                    { httpRequestId = httpRequestEvent.httpRequestId
                    , response =
                        { statusCode = httpResponseCode
                        , bodyAsString = Just httpResponseBody
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


interfaceToHost_initState : State
interfaceToHost_initState =
    0


interfaceToHost_processEvent : String -> State -> ( State, String )
interfaceToHost_processEvent =
    InterfaceToHost.wrapForSerialInterface_processEvent processEvent


interfaceToHost_serializeState : State -> String
interfaceToHost_serializeState =
    String.fromInt


interfaceToHost_deserializeState : String -> State
interfaceToHost_deserializeState =
    String.toInt >> Maybe.withDefault interfaceToHost_initState
