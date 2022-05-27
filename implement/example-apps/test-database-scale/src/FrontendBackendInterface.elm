module FrontendBackendInterface exposing (..)

import Dict


type alias GetDirectoryResponse =
    Dict.Dict Int { length : Int }
