module Factory exposing (group)

import Expect
import Test exposing (Test)


group : Int -> Test
group index =
    Test.describe ("group " ++ String.fromInt index)
        [ Test.test "generated" <| \_ -> Expect.equal index index ]
