module CompilationInterface.GenerateJsonConverters exposing (..)

{-| For documentation of the compilation interface, see <https://github.com/pine-vm/pine/blob/main/guide/customizing-elm-app-builds-with-compilation-interfaces.md#compilationinterfacegeneratejsonconverters-elm-module>
-}

import Backend.State
import Json.Encode
import CompilationInterface.GenerateJsonConverters.Generated_JsonConverters
import Dict
import Set
import Array
import Json.Decode
import Json.Encode
import Bytes
import Bytes.Decode
import Bytes.Encode
import Backend.State
import ListDict


jsonEncodeBackendState : Backend.State.State -> Json.Encode.Value
jsonEncodeBackendState =
    CompilationInterface.GenerateJsonConverters.Generated_JsonConverters.jsonEncode_51061477
