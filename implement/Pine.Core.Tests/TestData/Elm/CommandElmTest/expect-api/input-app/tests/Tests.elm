module Tests exposing (suite)

import Dict
import Expect exposing (FloatingPointTolerance(..))
import Set
import Test exposing (Test)


suite : Test
suite =
    Test.describe "Expect API"
        [ Test.test "notEqual" <| \_ -> Expect.notEqual 1 2
        , Test.test "atMost" <| \_ -> Expect.atMost 2 2
        , Test.test "atLeast" <| \_ -> Expect.atLeast 2 2
        , Test.test "within" <| \_ -> Expect.within (Absolute 0.01) 1 1.005
        , Test.test "notWithin" <| \_ -> Expect.notWithin (Relative 0.01) 1 2
        , Test.test "ok" <| \_ -> Expect.ok (Ok 1)
        , Test.test "err" <| \_ -> Expect.err (Err "error")
        , Test.test "equalLists" <| \_ -> Expect.equalLists [ 1, 2 ] [ 1, 2 ]
        , Test.test "equalDicts" <|
            \_ -> Expect.equalDicts (Dict.singleton 1 "one") (Dict.singleton 1 "one")
        , Test.test "equalSets" <|
            \_ -> Expect.equalSets (Set.fromList [ 1, 2 ]) (Set.fromList [ 1, 2 ])
        , Test.test "onFail" <| \_ -> Expect.onFail "replacement" Expect.pass
        , Test.test "all" <| \_ -> Expect.all [ Expect.atLeast 1, Expect.atMost 3 ] 2
        ]
