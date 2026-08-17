module Tests exposing (..)

import Expect
import Test exposing (Test)


suite : Test
suite =
    Test.test "A Test Title" <|
        \_ ->
            Expect.all
                [ Expect.greaterThan -2
                , Expect.lessThan 5
                ]
                (List.length [])
