module Elm.Parser.Whitespace exposing (many1Spaces, manySpaces, realNewLine, untilNewlineToken)

import Combine exposing (Parser)
import Parser as Core exposing ((|.))


manySpaces : Parser s ()
manySpaces =
    Combine.fromCore (Core.chompWhile (\c -> c == ' '))


many1Spaces : Parser s ()
many1Spaces =
    Core.token " "
        |. Core.chompWhile (\c -> c == ' ')
        |> Combine.fromCore


realNewLine : Parser s String
realNewLine =
    Core.oneOf
        [ Core.chompIf ((==) '\u{000D}')
        , Core.succeed ()
        ]
        |. Core.symbol "\n"
        |> Core.getChompedString
        |> Combine.fromCore


untilNewlineToken : Parser s String
untilNewlineToken =
    Core.getChompedString (Core.chompWhile (\c -> c /= '\u{000D}' && c /= '\n'))
        |> Combine.fromCore
