module Backend.MigrateState exposing (migrate)

import Backend.Main
import ElmWebServer


type alias PreviousBackendState =
    Int


migrate : PreviousBackendState -> ( Backend.Main.State, ElmWebServer.Commands Backend.Main.State )
migrate originalState =
    ( originalState |> String.fromInt, [] )
