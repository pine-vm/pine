module ElmFullstackCompilerInterface.GenerateJsonCoders exposing (..)

import FrontendBackendInterface
import Json.Decode
import Json.Encode


jsonEncodeRequestFromUser : FrontendBackendInterface.RequestFromUser -> Json.Encode.Value
jsonEncodeRequestFromUser =
    always (Json.Encode.string "The function expression will be replaced by auto-generated code.")


jsonDecodeRequestFromUser : Json.Decode.Decoder FrontendBackendInterface.RequestFromUser
jsonDecodeRequestFromUser =
    Json.Decode.fail "The function expression will be replaced by auto-generated code."


jsonEncodeMessageToClient : FrontendBackendInterface.MessageToClient -> Json.Encode.Value
jsonEncodeMessageToClient =
    always (Json.Encode.string "The function expression will be replaced by auto-generated code.")


jsonDecodeMessageToClient : Json.Decode.Decoder FrontendBackendInterface.MessageToClient
jsonDecodeMessageToClient =
    Json.Decode.fail "The function expression will be replaced by auto-generated code."
