module CompilationInterface.GenerateJsonConverters exposing (..)

{-| For documentation of the compilation interface, see <https://github.com/elm-time/elm-time/blob/main/guide/how-to-configure-and-deploy-an-elm-backend-app.md#compilationinterfacegeneratejsonconverters-elm-module>
-}

import Backend.State
import Json.Encode


jsonEncodeBackendState : Backend.State.State -> Json.Encode.Value
jsonEncodeBackendState =
    always (Json.Encode.string "The compiler replaces this declaration.")
