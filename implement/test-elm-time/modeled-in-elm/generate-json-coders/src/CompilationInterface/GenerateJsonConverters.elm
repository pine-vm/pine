module CompilationInterface.GenerateJsonConverters exposing
    ( decodeMixedRecord
    , encodeMixedRecord
    , testsValueToInterface
    )

{-| For documentation of the compilation interface, see <https://github.com/pine-vm/pine/blob/main/guide/customizing-elm-app-builds-with-compilation-interfaces.md#compilationinterfacegeneratejsonconverters-elm-module>
-}

import Json.Decode
import Json.Encode
import Structures


encodeMixedRecord : Structures.MixedRecord -> Json.Encode.Value
encodeMixedRecord =
    always (Json.Encode.string "The compiler replaces this declaration.")


decodeMixedRecord : Json.Decode.Decoder Structures.MixedRecord
decodeMixedRecord =
    Json.Decode.fail "The compiler replaces this declaration."


testsValueToInterface : List { testName : String, expected : String, derived : String } -> Json.Encode.Value
testsValueToInterface =
    always (Json.Encode.string "The compiler replaces this declaration.")
