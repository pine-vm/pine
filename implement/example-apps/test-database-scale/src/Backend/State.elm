module Backend.State exposing (..)

import Dict


type alias State =
    { store : Dict.Dict Int { entryBase64 : String }
    }
