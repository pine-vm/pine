module Tests exposing (..)

import Expect
import Test exposing (Test)


type NumericSyntax
    = Floatable Float


suite : Test
suite =
    Test.test "normalizes numeric values inside choice tags" <|
        \_ ->
            Expect.equal
                (Floatable 350)
                (Floatable 350.0)
