module Backend.MigrateState exposing (migrate)

import Backend.Main
import Platform.WebServer


migrate : Backend.Main.State -> ( Backend.Main.State, Platform.WebServer.Commands Backend.Main.State )
migrate state =
    ( state, [] )
