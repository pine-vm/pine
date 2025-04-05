module Backend.State exposing (..)

import Bytes
import Dict


type alias State =
    { store : Dict.Dict Int { entryBytes : Bytes.Bytes }
    }
