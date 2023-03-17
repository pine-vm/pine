module CompilationInterface.GenerateJsonConverters exposing
    ( decodeMixedRecord
    , encodeMixedRecord
    , testsValueToInterface
    )

{-| For documentation of the compilation interface, see <https://github.com/elm-time/elm-time/blob/main/guide/how-to-configure-and-deploy-an-elm-backend-app.md#compilationinterfacegeneratejsoncoders-elm-module>
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
