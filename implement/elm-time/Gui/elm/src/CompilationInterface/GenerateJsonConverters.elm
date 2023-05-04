module CompilationInterface.GenerateJsonConverters exposing (..)

{-| For documentation of the compilation interface, see <https://github.com/elm-time/elm-time/blob/main/guide/how-to-configure-and-deploy-an-elm-backend-app.md#compilationinterfacegeneratejsonconverters-elm-module>
-}

import HostInterface
import Json.Decode
import Json.Encode


jsonEncodeMessageToHost : HostInterface.MessageToHost -> Json.Encode.Value
jsonEncodeMessageToHost =
    always (Json.Encode.string "The compiler replaces this declaration.")


jsonDecodeEventFromHost : Json.Decode.Decoder HostInterface.EventFromHost
jsonDecodeEventFromHost =
    Json.Decode.fail "The compiler replaces this declaration."
