module ElmFullstackCompilerInterface.GenerateJsonCoders exposing (..)

import FrontendBackendInterface
import Json.Decode
import Json.Encode


jsonEncodeRequestFromUser : FrontendBackendInterface.RequestFromUser -> Json.Encode.Value
jsonEncodeRequestFromUser =
    always (Json.Encode.string "The Elm-fullstack compiler replaces this function.")


jsonDecodeRequestFromUser : Json.Decode.Decoder FrontendBackendInterface.RequestFromUser
jsonDecodeRequestFromUser =
    Json.Decode.fail "The Elm-fullstack compiler replaces this function."


jsonEncodeMessageToClient : FrontendBackendInterface.MessageToClient -> Json.Encode.Value
jsonEncodeMessageToClient =
    always (Json.Encode.string "The Elm-fullstack compiler replaces this function.")


jsonDecodeMessageToClient : Json.Decode.Decoder FrontendBackendInterface.MessageToClient
jsonDecodeMessageToClient =
    Json.Decode.fail "The Elm-fullstack compiler replaces this function."
