module ElmFullstackLoweringInterface.GenerateJsonCoders exposing
    ( decodeBackendState
    , encodeBackendState
    )

import Backend.StateType
import Json.Decode
import Json.Encode


encodeBackendState : Backend.StateType.State -> Json.Encode.Value
encodeBackendState =
    always (Json.Encode.string "The Elm-fullstack compiler replaces this function.")


decodeBackendState : Json.Decode.Decoder Backend.StateType.State
decodeBackendState =
    Json.Decode.fail "The Elm-fullstack compiler replaces this function."
