module CompilationInterface.GenerateJsonConverters exposing (..)

{-| For documentation of the compilation interface, see <https://github.com/elm-time/elm-time/blob/main/guide/how-to-configure-and-deploy-an-elm-backend-app.md#compilationinterfacegeneratejsonconverters-elm-module>
-}

import FrontendBackendInterface
import Json.Decode
import Json.Encode


jsonEncodeGetDirectoryResponse : FrontendBackendInterface.GetDirectoryResponse -> Json.Encode.Value
jsonEncodeGetDirectoryResponse =
    always (Json.Encode.string "The compiler replaces this declaration.")


jsonDecodeGetDirectoryResponse : Json.Decode.Decoder FrontendBackendInterface.GetDirectoryResponse
jsonDecodeGetDirectoryResponse =
    Json.Decode.fail "The compiler replaces this declaration."
