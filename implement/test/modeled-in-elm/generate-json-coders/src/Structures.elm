module Structures exposing (..)

import OpaqueCustomType exposing (OpaqueCustomType)


type alias MixedRecord =
    { int : Int
    , opaqueCustomType : OpaqueCustomType
    }
