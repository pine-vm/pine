module Backend.MigrateState exposing (migrate)

import Backend.Main
import Platform.WebService


type alias PreviousBackendState =
    Int


migrate : PreviousBackendState -> ( Backend.Main.State, Platform.WebService.Commands Backend.Main.State )
migrate state =
    ( state, [] )
