module CompilationInterface.GenerateJsonConverters exposing
    ( decodeBackendState
    , encodeBackendState
    )

{-| For documentation of the compilation interface, see <https://github.com/pine-vm/pine/blob/main/guide/customizing-elm-app-builds-with-compilation-interfaces.md#compilationinterfacegeneratejsonconverters-elm-module>
-}

import Backend.StateType
import Json.Decode
import Json.Encode


encodeBackendState : Backend.StateType.State -> Json.Encode.Value
encodeBackendState =
    always (Json.Encode.string "The compiler replaces this declaration.")


decodeBackendState : Json.Decode.Decoder Backend.StateType.State
decodeBackendState =
    Json.Decode.fail "The compiler replaces this declaration."
