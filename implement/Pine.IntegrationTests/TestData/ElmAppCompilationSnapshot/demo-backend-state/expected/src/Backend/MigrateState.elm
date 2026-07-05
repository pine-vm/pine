module Backend.MigrateState exposing (migrate)

import Backend.Main
import Platform.WebService


migrate : Backend.Main.State -> ( Backend.Main.State, Platform.WebService.Commands Backend.Main.State )
migrate state =
    ( state, [] )
