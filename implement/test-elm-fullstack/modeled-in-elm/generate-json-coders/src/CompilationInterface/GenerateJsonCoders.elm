module CompilationInterface.GenerateJsonCoders exposing
    ( decodeMixedRecord
    , encodeMixedRecord
    , testsValueToInterface
    )

{-| For documentation of the compilation interface, see <https://github.com/elm-fullstack/elm-fullstack/blob/main/guide/how-to-configure-and-deploy-an-elm-fullstack-app.md#compilationinterfacegeneratejsoncoders-elm-module>
-}

import Json.Decode
import Json.Encode
import Structures


encodeMixedRecord : Structures.MixedRecord -> Json.Encode.Value
encodeMixedRecord =
    always (Json.Encode.string "The compiler replaces this function.")


decodeMixedRecord : Json.Decode.Decoder Structures.MixedRecord
decodeMixedRecord =
    Json.Decode.fail "The compiler replaces this function."


testsValueToInterface : List { testName : String, expected : String, derived : String } -> Json.Encode.Value
testsValueToInterface =
    always (Json.Encode.string "The compiler replaces this function.")
