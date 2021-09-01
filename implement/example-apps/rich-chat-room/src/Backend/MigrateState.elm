module Backend.MigrateState exposing (migrate)

import Backend.Main
import ElmFullstack


migrate : Backend.Main.State -> ( Backend.Main.State, ElmFullstack.BackendCmds Backend.Main.State )
migrate state =
    ( state, [] )
