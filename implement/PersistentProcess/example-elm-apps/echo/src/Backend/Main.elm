module Backend.Main exposing
    ( Msg
    , State
    , interfaceToHost_initState
    , interfaceToHost_processEvent
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
