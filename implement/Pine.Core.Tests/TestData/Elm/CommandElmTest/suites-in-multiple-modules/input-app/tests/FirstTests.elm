module FirstTests exposing (suite)

import Expect
import Test exposing (Test)


suite : Test
suite =
    Test.test "first test" <|
        \_ ->
            Expect.pass
