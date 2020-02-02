module Backend.Main exposing
    ( State
    , interfaceToHost_initState
    , interfaceToHost_processEvent
    )


type alias State =
    { eventCount : Int }


interfaceToHost_initState : State
interfaceToHost_initState =
    { eventCount = 0 }


interfaceToHost_processEvent : String -> State -> ( State, String )
interfaceToHost_processEvent event stateBefore =
    let
        eventCount =
            stateBefore.eventCount + 1
    in
    ( { stateBefore | eventCount = eventCount }, eventCount |> String.fromInt )
