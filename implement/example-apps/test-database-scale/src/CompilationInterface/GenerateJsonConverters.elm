module CompilationInterface.GenerateJsonConverters exposing (..)

{-| For documentation of the compilation interface, see <https://github.com/pine-vm/pine/blob/main/guide/customizing-elm-app-builds-with-compilation-interfaces.md#compilationinterfacegeneratejsonconverters-elm-module>
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
