module Fuzz.Internal exposing (Fuzzer(..), generate)

{-| This module is here just to hide the `generate` function from the end users
of the library.
-}

import GenResult exposing (GenResult)
import PRNG exposing (PRNG)


type Fuzzer a
    = Fuzzer (PRNG -> GenResult a)


generate : PRNG -> Fuzzer a -> GenResult a
generate prng (Fuzzer fuzzer) =
    fuzzer prng
