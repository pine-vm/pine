module CompilationInterface.GenerateJsonCoders exposing (..)

import Backend.State
import Json.Encode


jsonEncodeBackendState : Backend.State.State -> Json.Encode.Value
jsonEncodeBackendState =
    always (Json.Encode.string "The compiler replaces this function.")
