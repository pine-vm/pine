module CompositeParser exposing (..)

import Parser.Advanced exposing ((|.), (|=))


negateToken : Parser.Advanced.Token ()
negateToken =
    Parser.Advanced.Token "-" ()


oneOf_only_int : Parser.Advanced.Parser () () Int
oneOf_only_int =
    Parser.Advanced.oneOf
        [ Parser.Advanced.int () ()
        ]


signedInt : Parser.Advanced.Parser () () Int
signedInt =
    Parser.Advanced.oneOf
        [ Parser.Advanced.succeed negate
            |. Parser.Advanced.symbol negateToken
            |= Parser.Advanced.int () ()
        , Parser.Advanced.int () ()
        ]
