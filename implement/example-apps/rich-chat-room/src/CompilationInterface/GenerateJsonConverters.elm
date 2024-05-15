module CompilationInterface.GenerateJsonConverters exposing (..)

{-| For documentation of the compilation interface, see <https://github.com/pine-vm/pine/blob/main/guide/how-to-use-elm-compilation-interfaces.md#compilationinterfacegeneratejsonconverters-elm-module>
-}

import FrontendBackendInterface
import Json.Decode
import Json.Encode


jsonEncodeRequestFromUser : FrontendBackendInterface.RequestFromUser -> Json.Encode.Value
jsonEncodeRequestFromUser =
    always (Json.Encode.string "The compiler replaces this declaration.")


jsonDecodeRequestFromUser : Json.Decode.Decoder FrontendBackendInterface.RequestFromUser
jsonDecodeRequestFromUser =
    Json.Decode.fail "The compiler replaces this declaration."


jsonEncodeMessageToClient : FrontendBackendInterface.MessageToClient -> Json.Encode.Value
jsonEncodeMessageToClient =
    always (Json.Encode.string "The compiler replaces this declaration.")


jsonDecodeMessageToClient : Json.Decode.Decoder FrontendBackendInterface.MessageToClient
jsonDecodeMessageToClient =
    Json.Decode.fail "The compiler replaces this declaration."
