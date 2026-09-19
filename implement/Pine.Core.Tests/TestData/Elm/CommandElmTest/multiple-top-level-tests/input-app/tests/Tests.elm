module Tests exposing (firstTest, helperValue, secondGroup)

import Expect
import Test exposing (Test)


firstTest : Test
firstTest =
    Test.test "first test" <|
        \_ ->
            Expect.pass


helperValue : Int
helperValue =
    42


secondGroup : Test
secondGroup =
    Test.describe "second group"
        [ Test.test "second test" <|
            \_ ->
                Expect.pass
        ]


privateTest : Test
privateTest =
    Test.test "private test" <|
        \_ ->
            Expect.fail "This unexposed test must not run."
