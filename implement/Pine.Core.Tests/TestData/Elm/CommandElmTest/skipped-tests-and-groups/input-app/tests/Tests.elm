module Tests exposing (suite)

import Expect
import Fuzz
import Test exposing (Test)


suite : Test
suite =
    Test.describe "All tests"
        [ Test.test "runs" <| \_ -> Expect.pass
        , Test.skip (Test.test "skipped unit" <| \_ -> Expect.fail "Must not run")
        , Test.skip
            (Test.describe "skipped group"
                [ Test.test "child unit" <| \_ -> Expect.fail "Must not run"
                , Test.fuzzWith { runs = 7, distribution = Test.noDistribution }
                    Fuzz.int
                    "skipped fuzz"
                    (\_ -> Expect.fail "Must not run")
                , Test.todo "skipped todo"
                ]
            )
        , Test.describe "nested"
            [ Test.skip (Test.todo "skipped placeholder") ]
        ]
