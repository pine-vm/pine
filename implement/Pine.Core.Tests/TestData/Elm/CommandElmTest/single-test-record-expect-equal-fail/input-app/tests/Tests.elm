module Tests exposing (..)

import Expect
import Test exposing (Test)


suite : Test
suite =
    Test.test "A Test Title" <|
        \_ ->
            Expect.equal
                { alfa = 31, beta = 41, gamma = 47 }
                { alfa = 31, beta = 43, gamma = 47 }
