module ElmFullstackCompilerInterface.GenerateJsonCoders exposing (..)

import FrontendBackendInterface
import Json.Decode
import Json.Encode


jsonEncodeRequestStructure : FrontendBackendInterface.RequestStructure -> Json.Encode.Value
jsonEncodeRequestStructure =
    always (Json.Encode.string "The Elm-fullstack compiler replaces this function.")


jsonDecodeRequestStructure : Json.Decode.Decoder FrontendBackendInterface.RequestStructure
jsonDecodeRequestStructure =
    Json.Decode.fail "The Elm-fullstack compiler replaces this function."


jsonDecodeResponseStructure : Json.Decode.Decoder FrontendBackendInterface.ResponseStructure
jsonDecodeResponseStructure =
    Json.Decode.fail "The Elm-fullstack compiler replaces this function."
