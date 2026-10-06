module PineTestBridge exposing (prepare)

import Random
import Test exposing (Test)
import Test.Runner exposing (SeededRunners)


prepare : Int -> Int -> Test -> SeededRunners
prepare runs seed test =
    Test.Runner.fromTest runs (Random.initialSeed seed) test
