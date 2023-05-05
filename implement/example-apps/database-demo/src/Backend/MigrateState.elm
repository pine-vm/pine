module Backend.MigrateState exposing (migrate)

import Backend.State
import Platform.WebService


migrate : Backend.State.State -> ( Backend.State.State, Platform.WebService.Commands Backend.State.State )
migrate state =
    ( state, [] )
