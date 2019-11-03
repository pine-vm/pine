module Backend.Main exposing
    ( Event
    , State
    , interfaceToHost_initState
    , interfaceToHost_processEvent
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


interfaceToHost_initState : State
interfaceToHost_initState =
    0
