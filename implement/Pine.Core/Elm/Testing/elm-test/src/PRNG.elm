module PRNG exposing (PRNG(..), getRun, getSeed, hardcoded, random)

{-| A way to draw values. There are two ways:

1.  Random: draw genuinely new random values using a Random.Seed. We remember
    drawn values on the side.

2.  Hardcoded: draw predefined values out of a recorded RandomRun. Handy
    when reproducing a failure. This can run out of values to draw, but that
    shouldn't happen during the normal execution.

-}

import Random
import RandomRun exposing (RandomRun)


type PRNG
    = Random
        { run : RandomRun
        , seed : Random.Seed
        }
    | Hardcoded
        { wholeRun : RandomRun
        , unusedPart : RandomRun
        }


random : Random.Seed -> PRNG
random seed =
    Random
        { run = RandomRun.empty
        , seed = seed
        }


hardcoded : RandomRun -> PRNG
hardcoded run =
    Hardcoded
        { wholeRun = run
        , unusedPart = run
        }


getRun : PRNG -> RandomRun
getRun prng =
    case prng of
        Random { run } ->
            run

        Hardcoded { wholeRun } ->
            wholeRun


getSeed : PRNG -> Maybe Random.Seed
getSeed prng =
    case prng of
        Random { seed } ->
            Just seed

        Hardcoded _ ->
            Nothing
