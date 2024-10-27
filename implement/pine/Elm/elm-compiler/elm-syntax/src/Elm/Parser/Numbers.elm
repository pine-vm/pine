module Elm.Parser.Numbers exposing (forgivingNumber, number)

import Combine exposing (Parser)
import Elm.Parser.State exposing (State)
import Parser as Core


raw : Maybe (Float -> a) -> (Int -> a) -> (Int -> a) -> Core.Parser a
raw floatf intf hexf =
    Core.number
        { int = Just intf
        , hex = Just hexf
        , octal = Nothing
        , binary = Nothing
        , float = floatf
        }


{-| Strange case that a number is consumes and does not function in a `oneOf`
-}
forgivingNumber : (Float -> a) -> (Int -> a) -> (Int -> a) -> Parser State a
forgivingNumber floatf intf hexf =
    Core.backtrackable (raw (Just floatf) intf hexf)
        |> Combine.fromCore


number : (Int -> a) -> (Int -> a) -> Parser State a
number intf hexf =
    raw Nothing intf hexf
        |> Combine.fromCore
