module MigrateBackendState exposing (migrate)

import Backend.Main


migrate : Backend.Main.State -> Backend.Main.State
migrate =
    identity
