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

-- Ensure Elm compiler understands that functions above are needed in the artifact.
main : Program Int State String
main =
    Platform.worker
        {   init = (\_ -> (initState, Cmd.none))
        ,   update = (\request stateBefore ->
                (serializedRequest request (stateBefore |> serializeState |> deserializeState) |> Tuple.mapSecond (always Cmd.none)))
        ,   subscriptions = (\_ -> Sub.none)
        }
