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


type Boolean
    = MyTrue
    | MyFalse
    | MyOr Boolean Boolean


boolean : Parser.Parser Boolean
boolean =
    Parser.oneOf
        [ Parser.succeed MyTrue
            |. Parser.keyword "true"
        , Parser.succeed MyFalse
            |. Parser.keyword "false"
        , Parser.succeed MyOr
            |. Parser.symbol "("
            |. Parser.spaces
            |= Parser.lazy (\_ -> boolean)
            |. Parser.spaces
            |. Parser.symbol "||"
            |. Parser.spaces
            |= Parser.lazy (\_ -> boolean)
            |. Parser.spaces
            |. Parser.symbol ")"
        ]


elmLineComment : Parser.Parser ()
elmLineComment =
    Parser.lineComment "--"
