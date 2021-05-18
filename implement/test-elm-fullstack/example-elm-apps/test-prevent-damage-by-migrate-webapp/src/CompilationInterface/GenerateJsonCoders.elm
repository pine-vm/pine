module CompilationInterface.GenerateJsonCoders exposing
    ( decodeBackendState
    , encodeBackendState
    )

import Backend.StateType
import Json.Decode
import Json.Encode


encodeBackendState : Backend.StateType.State -> Json.Encode.Value
encodeBackendState =
    always (Json.Encode.string "The compiler replaces this function.")


decodeBackendState : Json.Decode.Decoder Backend.StateType.State
decodeBackendState =
    Json.Decode.fail "The compiler replaces this function."
