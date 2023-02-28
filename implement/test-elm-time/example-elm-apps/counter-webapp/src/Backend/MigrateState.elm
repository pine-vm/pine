module Backend.MigrateState exposing (migrate)

import Backend.Main
import Platform.WebServer


type alias PreviousBackendState =
    Int


migrate : PreviousBackendState -> ( Backend.Main.State, Platform.WebServer.Commands Backend.Main.State )
migrate state =
    ( state, [] )
