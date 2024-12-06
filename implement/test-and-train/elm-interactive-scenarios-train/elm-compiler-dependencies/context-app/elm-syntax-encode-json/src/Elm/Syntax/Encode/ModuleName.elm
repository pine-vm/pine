module Elm.Syntax.Encode.ModuleName exposing (..)

import Elm.Syntax.ModuleName exposing (ModuleName)
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)


{-| Encode a `ModuleName` syntax element to JSON.
-}
encode : ModuleName -> Value
encode =
    JE.list JE.string


{-| JSON decoder for a `ModuleName` syntax element.
-}
decoder : Decoder ModuleName
decoder =
    JD.list JD.string
