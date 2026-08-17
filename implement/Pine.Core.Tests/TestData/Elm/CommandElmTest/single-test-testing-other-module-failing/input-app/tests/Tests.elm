module Tests exposing (..)

import Expect
import Library
import Test exposing (Test)


suite : Test
suite =
    Test.test "A Test Title" <|
        \_ ->
            Library.calculate 31 37 39
                |> Expect.equal 71
