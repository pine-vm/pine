module Backend.Main exposing
    ( Msg
    , State
    , interfaceToHost_deserializeState
    , interfaceToHost_initState
    , interfaceToHost_processEvent
    , interfaceToHost_serializeState
    , main
    )

import Platform


type alias State =
    ()


type alias Msg =
    String


interfaceToHost_initState : State
interfaceToHost_initState =
    ()


interfaceToHost_processEvent : String -> State -> ( State, String )
interfaceToHost_processEvent event stateBefore =
    ( stateBefore, "Echo from Elm:" ++ event )


interfaceToHost_serializeState : State -> String
interfaceToHost_serializeState _ =
    ""


interfaceToHost_deserializeState : String -> State
interfaceToHost_deserializeState _ =
    ()



-- Support function-level dead code elimination (https://elm-lang.org/blog/small-assets-without-the-headache) Elm code needed to inform the Elm compiler about our entry points.


main : Program Int State String
main =
    Platform.worker
        { init = \_ -> ( interfaceToHost_initState, Cmd.none )
        , update =
            \event stateBefore ->
                interfaceToHost_processEvent event (stateBefore |> interfaceToHost_serializeState |> interfaceToHost_deserializeState) |> Tuple.mapSecond (always Cmd.none)
        , subscriptions = \_ -> Sub.none
        }
