module Tests exposing (..)

import Expect
import Test exposing (Test)


suite : Test
suite =
    Test.describe
        "Group Title"
        [ Test.test "Test Title" <|
            \_ ->
                71 |> Expect.equal 71
        , Test.test "Another Test Title" <|
            \_ ->
                41 |> Expect.equal 41
        , Test.test "Yet Another Test Title" <|
            \_ ->
                21 |> Expect.equal 21
        ]
