module ElmFullstackCompilerInterface.GenerateJsonCoders exposing (..)

import FrontendBackendInterface
import Json.Decode
import Json.Encode


jsonEncodeElmMakeRequestStructure : FrontendBackendInterface.ElmMakeRequestStructure -> Json.Encode.Value
jsonEncodeElmMakeRequestStructure =
    always (Json.Encode.string "The Elm-fullstack compiler replaces this function.")


jsonDecodeElmMakeRequestStructure : Json.Decode.Decoder FrontendBackendInterface.ElmMakeRequestStructure
jsonDecodeElmMakeRequestStructure =
    Json.Decode.fail "The Elm-fullstack compiler replaces this function."


jsonDecodeElmMakeResponseStructure : Json.Decode.Decoder FrontendBackendInterface.ElmMakeResponseStructure
jsonDecodeElmMakeResponseStructure =
    Json.Decode.fail "The Elm-fullstack compiler replaces this function."
