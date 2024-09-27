module CompositeParser exposing (..)

import Parser exposing ((|.), (|=))


signedInt : Parser.Parser Int
signedInt =
    Parser.oneOf
        [ Parser.succeed negate
            |. Parser.symbol "-"
            |= Parser.int
        , Parser.int
        ]
