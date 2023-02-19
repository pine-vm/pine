module Backend.MigrateState exposing (migrate)

import Backend.Main
import ElmWebServer


migrate : Backend.Main.State -> ( Backend.Main.State, ElmWebServer.Commands Backend.Main.State )
migrate state =
    ( state, [] )
