module Backend.MigrateState exposing (migrate)

import Backend.Main


type alias PreviousBackendState =
    Int


migrate : PreviousBackendState -> Backend.Main.State
migrate originalState =
    originalState |> String.fromInt
