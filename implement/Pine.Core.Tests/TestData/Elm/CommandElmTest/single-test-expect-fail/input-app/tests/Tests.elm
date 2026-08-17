module Tests exposing (..)

import Expect
import Test exposing (Test)


suite : Test
suite =
    Test.test "A Test Title" <|
        \_ ->
            Expect.fail "A failing branch"
