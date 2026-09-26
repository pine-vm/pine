module CompilationInterface.GenerateJsonConverters exposing (..)

import Interface
import Json.Decode
import Json.Encode


jsonEncodeRequest : Interface.Request -> Json.Encode.Value
jsonEncodeRequest =
    always (Json.Encode.string "The compiler replaces this function.")


jsonDecodeRequest : Json.Decode.Decoder Interface.Request
jsonDecodeRequest =
    Json.Decode.fail "The compiler replaces this function."
