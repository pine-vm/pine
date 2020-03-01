module ElmFullstackLoweringInterface.GenerateJsonCoders exposing
    ( decodeMixedRecord
    , encodeMixedRecord
    , testsValueToInterface
    )

import Json.Decode
import Json.Encode
import Structures


encodeMixedRecord : Structures.MixedRecord -> Json.Encode.Value
encodeMixedRecord =
    always (Json.Encode.string "The Elm-fullstack compiler replaces this function.")


decodeMixedRecord : Json.Decode.Decoder Structures.MixedRecord
decodeMixedRecord =
    Json.Decode.fail "The Elm-fullstack compiler replaces this function."


testsValueToInterface : List { testName : String, expected : String, derived : String } -> Json.Encode.Value
testsValueToInterface =
    always (Json.Encode.string "The Elm-fullstack compiler replaces this function.")
