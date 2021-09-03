module Backend.StateType exposing (State)


type alias State =
    { maybeString : Maybe String
    , otherState : String
    }
