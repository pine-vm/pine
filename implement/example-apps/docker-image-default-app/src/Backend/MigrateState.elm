module Backend.MigrateState exposing (migrate)

import Backend.Main


migrate : Backend.Main.State -> Backend.Main.State
migrate =
    identity
