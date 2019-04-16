module CounterWebApp exposing
    ( CounterEvent
    , State
    , deserializeCounterEvent
    , deserializeState
    , initState
    , main
    , processCounterEvent
    , processEvent
    , processSerializedEvent
    , serializeState
    )

import ElmAppInKalmitProcess
import Json.Decode
import Json.Encode
import Platform


type alias State =
    Int


type alias CounterEvent =
    { addition : Int }


processSerializedEvent : String -> State -> ( State, String )
processSerializedEvent =
    ElmAppInKalmitProcess.wrapUpdateForSerialInterface processEvent


processEvent : ElmAppInKalmitProcess.KalmitProcessEvent -> State -> ( State, List ElmAppInKalmitProcess.KalmitProcessResponse )
processEvent hostEvent stateBefore =
    case hostEvent of
        ElmAppInKalmitProcess.HttpRequest httpRequestEvent ->
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
                        |> ElmAppInKalmitProcess.CompleteHttpResponse
            in
            ( state, [ httpResponse ] )


processCounterEvent : CounterEvent -> State -> ( State, String )
processCounterEvent counterEvent stateBefore =
    let
        state =
            stateBefore + counterEvent.addition
    in
    ( state, state |> String.fromInt )


serializeState : State -> String
serializeState =
    String.fromInt


deserializeState : String -> State
deserializeState =
    String.toInt >> Maybe.withDefault initState


deserializeCounterEvent : String -> Result String CounterEvent
deserializeCounterEvent serializedEvent =
    serializedEvent
        |> Json.Decode.decodeString (Json.Decode.field "addition" Json.Decode.int |> Json.Decode.map CounterEvent)
        |> Result.mapError Json.Decode.errorToString


initState : State
initState =
    0



-- Support function-level dead code elimination (https://elm-lang.org/blog/small-assets-without-the-headache) Elm code needed to inform the Elm compiler about our entry points.


main : Program Int State String
main =
    Platform.worker
        { init = \_ -> ( initState, Cmd.none )
        , update =
            \event stateBefore ->
                processSerializedEvent event (stateBefore |> serializeState |> deserializeState) |> Tuple.mapSecond (always Cmd.none)
        , subscriptions = \_ -> Sub.none
        }
