module Backend.Main exposing
    ( Event
    , State
    , interfaceToHost_deserializeState
    , interfaceToHost_initState
    , interfaceToHost_processEvent
    , interfaceToHost_serializeState
    )

import Json.Decode


type alias State =
    Int


type alias Event =
    { addition : Int }


interfaceToHost_processEvent : String -> State -> ( State, String )
interfaceToHost_processEvent serializedEvent stateBefore =
    case serializedEvent |> deserializeEvent of
        Err error ->
            ( stateBefore, "Failed to deserialize event: " ++ error )

        Ok event ->
            let
                state =
                    stateBefore + event.addition
            in
            ( state, state |> String.fromInt )


deserializeEvent : String -> Result String Event
deserializeEvent serializedEvent =
    serializedEvent
        |> Json.Decode.decodeString (Json.Decode.field "addition" Json.Decode.int |> Json.Decode.map Event)
        |> Result.mapError Json.Decode.errorToString


interfaceToHost_serializeState : State -> String
interfaceToHost_serializeState =
    String.fromInt


interfaceToHost_deserializeState : String -> State
interfaceToHost_deserializeState =
    String.toInt >> Maybe.withDefault interfaceToHost_initState


interfaceToHost_initState : State
interfaceToHost_initState =
    0
