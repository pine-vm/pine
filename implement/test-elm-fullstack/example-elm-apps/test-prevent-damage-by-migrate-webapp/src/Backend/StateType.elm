module Backend.StateType exposing (State)


type alias State =
    { attemptSetMaybeStringOnMigration : Bool
    , maybeString : Maybe String
    , otherState : String
    }
