module Tests exposing (ordinary, suite)

import Expect
import Fuzz
import Test exposing (Test)


ordinary : Test
ordinary =
    Test.test "other root" <| \_ -> Expect.fail "Excluded by only in suite"


suite : Test
suite =
    Test.describe "Focus"
        [ Test.todo "not focused"
        , Test.describe "ordinary group"
            [ Test.test "excluded" <| \_ -> Expect.fail "Must not run" ]
        , Test.only
            (Test.describe "focused group"
                [ Test.test "selected" <| \_ -> Expect.pass
                , Test.fuzzWith { runs = 3, distribution = Test.noDistribution }
                    Fuzz.int
                    "selected fuzz"
                    (\_ -> Expect.pass)
                ]
            )
        , Test.skip (Test.only (Test.todo "skip wins"))
        ]
