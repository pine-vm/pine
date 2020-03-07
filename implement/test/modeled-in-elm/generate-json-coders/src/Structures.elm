module Structures exposing (..)

import OpaqueCustomType exposing (OpaqueCustomType)


type alias MixedRecord =
    { int : Int
    , opaqueCustomType : OpaqueCustomType
    , list_custom_type : List CustomType
    }


type CustomType
    = CustomTagWithoutParameter
    | CustomTagWithOneParameter Int
    | CustomTagWithTwoParameters String Int
    | CustomTagWithMaybeInstance (Maybe Int)
    | CustomTagWithResultInstance (Result String Int)
