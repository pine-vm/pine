module Counter exposing (..)

import Platform
import Json.Decode

type alias State = Int

type alias Event = { addition : Int }

processSerializedEvent : String -> State -> (State, String)
processSerializedEvent serializedEvent stateBefore =
    case serializedEvent |> deserializeEvent of
    Err error -> (stateBefore, "Failed to deserialize event: " ++ error)
    Ok event ->
        let
            state = stateBefore + event.addition
        in
            (state, state |> String.fromInt)

serializeState : State -> String
serializeState = String.fromInt

deserializeState : String -> State
deserializeState = String.toInt >> Maybe.withDefault initState

deserializeEvent : String -> Result String Event
deserializeEvent serializedEvent =
    serializedEvent |> Json.Decode.decodeString ((Json.Decode.field "addition" Json.Decode.int) |> Json.Decode.map Event)
    |> Result.mapError Json.Decode.errorToString

initState : State
initState = 0

-- Support function-level dead code elimination (https://elm-lang.org/blog/small-assets-without-the-headache) Elm code needed to inform the Elm compiler about our entry points.
main : Program Int State String
main =
    Platform.worker
        {   init = (\_ -> (initState, Cmd.none))
        ,   update = (\event stateBefore ->
                (processSerializedEvent event (stateBefore |> serializeState |> deserializeState) |> Tuple.mapSecond (always Cmd.none)))
        ,   subscriptions = (\_ -> Sub.none)
        }
