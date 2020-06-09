module ElmFullstackCompilerInterface.GenerateJsonCoders exposing (..)

import Backend.State
import Json.Encode


jsonEncodeBackendState : Backend.State.State -> Json.Encode.Value
jsonEncodeBackendState =
    always (Json.Encode.string "The Elm-fullstack compiler replaces this function.")
