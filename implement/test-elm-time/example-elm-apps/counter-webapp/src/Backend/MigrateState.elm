module Backend.MigrateState exposing (migrate)

import Backend.Main
import ElmFullstack


type alias PreviousBackendState =
    Int


migrate : PreviousBackendState -> ( Backend.Main.State, ElmFullstack.BackendCmds Backend.Main.State )
migrate state =
    ( state, [] )
