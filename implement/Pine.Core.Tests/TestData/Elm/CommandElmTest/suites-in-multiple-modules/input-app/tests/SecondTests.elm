module SecondTests exposing (suite)

import Expect
import Test exposing (Test)


suite : Test
suite =
    Test.test "second test" <|
        \_ ->
            Expect.pass
