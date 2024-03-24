module Backend.MigrateState exposing (BackendState, migrate)

{-| Use a `BackendState` record that does not exactly match the BackendState type of the web app:
The `maybeString` field has a different type.
As long as that field has the value `Nothing`, the different type is not apparent in the serialized representation.
-}

import Backend.Main
import Platform.WebService


type alias BackendState =
    { maybeString : Maybe Int
    , otherState : String
    }


migrate : BackendState -> ( Backend.Main.State, Platform.WebService.Commands Backend.Main.State )
migrate originalState =
    let
        state =
            { otherState = originalState.otherState ++ (originalState.otherState |> String.length |> String.fromInt)
            , maybeString = Nothing
            }
    in
    ( state, [] )
