module CompilationInterface.GenerateJsonConverters exposing (..)

import Interface
import Json.Decode
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
import Common.EffectOnWindow
import Interface


jsonEncodeRequest : Interface.Request -> Json.Encode.Value
jsonEncodeRequest =
    CompilationInterface.GenerateJsonConverters.Generated_JsonConverters.jsonEncode_2190912599


jsonDecodeRequest : Json.Decode.Decoder Interface.Request
jsonDecodeRequest =
    CompilationInterface.GenerateJsonConverters.Generated_JsonConverters.jsonDecode_2190912599
