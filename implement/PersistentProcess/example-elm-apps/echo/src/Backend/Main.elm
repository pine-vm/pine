module Backend.Main exposing
    ( Msg
    , State
    , interfaceToHost_deserializeState
    , interfaceToHost_initState
    , interfaceToHost_processEvent
    , interfaceToHost_serializeState
    )


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
