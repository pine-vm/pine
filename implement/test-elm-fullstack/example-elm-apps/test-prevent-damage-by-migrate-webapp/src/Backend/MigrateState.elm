module Backend.MigrateState exposing (BackendState, migrate)

{-| Use a `BackendState` record that does not exactly match the BackendState type of the web app:
The `maybeString` field has a different type.
As long as that field has the value `Nothing`, the different type is not apparent in the serialized representation.
-}

import ElmFullstack


type alias BackendState =
    { attemptSetMaybeStringOnMigration : Bool
    , maybeString : Maybe Int
    , otherState : String
    }


migrate : BackendState -> ( BackendState, ElmFullstack.BackendCmds BackendState )
migrate originalState =
    let
        state =
            { originalState
                | otherState = originalState.otherState ++ (originalState.otherState |> String.length |> String.fromInt)
            }
    in
    if state.attemptSetMaybeStringOnMigration then
        ( { state | maybeString = Just 123 }, [] )

    else
        ( state, [] )
