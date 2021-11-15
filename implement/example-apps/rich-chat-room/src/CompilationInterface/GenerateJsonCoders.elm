module CompilationInterface.GenerateJsonCoders exposing (..)

{-| For documentation of the compilation interface, see <https://github.com/elm-fullstack/elm-fullstack/blob/main/guide/how-to-configure-and-deploy-an-elm-fullstack-app.md#compilationinterfacegeneratejsoncoders-elm-module>
-}

import FrontendBackendInterface
import Json.Decode
import Json.Encode


jsonEncodeRequestFromUser : FrontendBackendInterface.RequestFromUser -> Json.Encode.Value
jsonEncodeRequestFromUser =
    always (Json.Encode.string "The compiler replaces this function.")


jsonDecodeRequestFromUser : Json.Decode.Decoder FrontendBackendInterface.RequestFromUser
jsonDecodeRequestFromUser =
    Json.Decode.fail "The compiler replaces this function."


jsonEncodeMessageToClient : FrontendBackendInterface.MessageToClient -> Json.Encode.Value
jsonEncodeMessageToClient =
    always (Json.Encode.string "The compiler replaces this function.")


jsonDecodeMessageToClient : Json.Decode.Decoder FrontendBackendInterface.MessageToClient
jsonDecodeMessageToClient =
    Json.Decode.fail "The compiler replaces this function."
