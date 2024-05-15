module CompilationInterface.GenerateJsonConverters exposing (..)

{-| For documentation of the compilation interface, see <https://github.com/pine-vm/pine/blob/main/guide/how-to-use-elm-compilation-interfaces.md#compilationinterfacegeneratejsonconverters-elm-module>
-}

import Backend.State
import Json.Encode


jsonEncodeBackendState : Backend.State.State -> Json.Encode.Value
jsonEncodeBackendState =
    always (Json.Encode.string "The compiler replaces this declaration.")
