module Echo exposing (..)

import Platform

type alias State = ()

type alias Msg = String

serializedRequest : String -> State -> (State, String)
serializedRequest request stateBefore = (stateBefore, "Echo from Elm:" ++ request)

serializeState : State -> String
serializeState _ = ""

deserializeState : String -> State
deserializeState _ = ()

initState : State
initState = ()

-- Support function-level dead code elimination (https://elm-lang.org/blog/small-assets-without-the-headache) Elm code needed to inform the Elm compiler about our entry points.
main : Program Int State String
main =
    Platform.worker
        {   init = (\_ -> (initState, Cmd.none))
        ,   update = (\request stateBefore ->
                (serializedRequest request (stateBefore |> serializeState |> deserializeState) |> Tuple.mapSecond (always Cmd.none)))
        ,   subscriptions = (\_ -> Sub.none)
        }
