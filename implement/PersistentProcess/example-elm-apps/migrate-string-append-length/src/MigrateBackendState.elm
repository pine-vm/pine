module MigrateBackendState exposing (BackendState, migrate)


type alias BackendState =
    String


migrate : BackendState -> BackendState
migrate originalState =
    originalState ++ (originalState |> String.length |> String.fromInt)
