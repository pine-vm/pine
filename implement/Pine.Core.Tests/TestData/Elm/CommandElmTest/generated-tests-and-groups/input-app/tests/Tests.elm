module Tests exposing (suite)

import Factory
import Test exposing (Test)


suite : Test
suite =
    Test.describe "generated suite" (List.map Factory.group [ 1, 2 ])
