module Structures exposing (..)

import OpaqueChoiceType exposing (OpaqueChoiceType)


type alias MixedRecord =
    { int : Int
    , opaqueChoiceType : OpaqueChoiceType
    , list_custom_type : List ChoiceType
    }


type ChoiceType
    = CustomTagWithoutParameter
    | CustomTagWithOneParameter Int
    | CustomTagWithTwoParameters String Int
    | CustomTagWithMaybeInstance (Maybe Int)
    | CustomTagWithResultInstance (Result String Int)
