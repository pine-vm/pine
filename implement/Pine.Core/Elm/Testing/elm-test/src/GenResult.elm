module GenResult exposing (GenResult(..), getPrng)

{-| A result of a Fuzzer trying to generate a value from a given PRNG.
-}

import PRNG exposing (PRNG)


type GenResult a
    = Generated
        { value : a
        , prng : PRNG
        }
    | Rejected
        { reason : String
        , prng : PRNG
        }


getPrng : GenResult a -> PRNG
getPrng genResult =
    case genResult of
        Generated { prng } ->
            prng

        Rejected { prng } ->
            prng
