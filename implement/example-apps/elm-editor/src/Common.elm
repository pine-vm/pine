module Common exposing (..)

import Base64
import Bytes
import Bytes.Decode


decodeBytesToString : Bytes.Bytes -> Maybe String
decodeBytesToString bytes =
    bytes |> Bytes.Decode.decode (Bytes.Decode.string (bytes |> Bytes.width))


decodeBase64ToString : String -> Maybe String
decodeBase64ToString =
    Base64.toBytes >> Maybe.andThen decodeBytesToString


faviconPath : String
faviconPath =
    "favicon.svg"
