module ElmSyntax.Concrete.Parser.StringParsing exposing (..)

{-| Low-level string scanning shared by the parsers in `ElmSyntax.Concrete.Parser`.

The direct-source parser (`ElmSyntax.Concrete.Parser.FromString`) and the tokenizer
(`ElmSyntax.Concrete.Parser.TokensFromString`) classify characters and scan runs of source
identically, therefore those primitives live here instead of being duplicated in each parser.

Everything in this module is first-order and operates on a `String` together with an offset into
it: no parser state and no higher-order functions, so each declaration stays a single specialized
recursive function that the runtime can accelerate with a precompiled leaf for both parsers at
once.

-}



-- CHARACTER CLASSES


isIdentifierStart : String -> Bool
isIdentifierStart character =
    case character of
        "_" ->
            True

        "a" ->
            True

        "b" ->
            True

        "c" ->
            True

        "d" ->
            True

        "e" ->
            True

        "f" ->
            True

        "g" ->
            True

        "h" ->
            True

        "i" ->
            True

        "j" ->
            True

        "k" ->
            True

        "l" ->
            True

        "m" ->
            True

        "n" ->
            True

        "o" ->
            True

        "p" ->
            True

        "q" ->
            True

        "r" ->
            True

        "s" ->
            True

        "t" ->
            True

        "u" ->
            True

        "v" ->
            True

        "w" ->
            True

        "x" ->
            True

        "y" ->
            True

        "z" ->
            True

        "A" ->
            True

        "B" ->
            True

        "C" ->
            True

        "D" ->
            True

        "E" ->
            True

        "F" ->
            True

        "G" ->
            True

        "H" ->
            True

        "I" ->
            True

        "J" ->
            True

        "K" ->
            True

        "L" ->
            True

        "M" ->
            True

        "N" ->
            True

        "O" ->
            True

        "P" ->
            True

        "Q" ->
            True

        "R" ->
            True

        "S" ->
            True

        "T" ->
            True

        "U" ->
            True

        "V" ->
            True

        "W" ->
            True

        "X" ->
            True

        "Y" ->
            True

        "Z" ->
            True

        _ ->
            False


isIdentifierChar : String -> Bool
isIdentifierChar character =
    {-
    TODO: Expand code analysis to optimize form using `List.member` to get the same level of efficiency:
    List.member char [ "_", "0", "1", ..., "9", "a", "b", ..., "z", "A", "B", ..., "Z" ]
    -}
    case character of
        "_" ->
            True

        "0" ->
            True

        "1" ->
            True

        "2" ->
            True

        "3" ->
            True

        "4" ->
            True

        "5" ->
            True

        "6" ->
            True

        "7" ->
            True

        "8" ->
            True

        "9" ->
            True

        "a" ->
            True

        "b" ->
            True

        "c" ->
            True

        "d" ->
            True

        "e" ->
            True

        "f" ->
            True

        "g" ->
            True

        "h" ->
            True

        "i" ->
            True

        "j" ->
            True

        "k" ->
            True

        "l" ->
            True

        "m" ->
            True

        "n" ->
            True

        "o" ->
            True

        "p" ->
            True

        "q" ->
            True

        "r" ->
            True

        "s" ->
            True

        "t" ->
            True

        "u" ->
            True

        "v" ->
            True

        "w" ->
            True

        "x" ->
            True

        "y" ->
            True

        "z" ->
            True

        "A" ->
            True

        "B" ->
            True

        "C" ->
            True

        "D" ->
            True

        "E" ->
            True

        "F" ->
            True

        "G" ->
            True

        "H" ->
            True

        "I" ->
            True

        "J" ->
            True

        "K" ->
            True

        "L" ->
            True

        "M" ->
            True

        "N" ->
            True

        "O" ->
            True

        "P" ->
            True

        "Q" ->
            True

        "R" ->
            True

        "S" ->
            True

        "T" ->
            True

        "U" ->
            True

        "V" ->
            True

        "W" ->
            True

        "X" ->
            True

        "Y" ->
            True

        "Z" ->
            True

        _ ->
            False


isDigit : String -> Bool
isDigit character =
    case character of
        "0" ->
            True

        "1" ->
            True

        "2" ->
            True

        "3" ->
            True

        "4" ->
            True

        "5" ->
            True

        "6" ->
            True

        "7" ->
            True

        "8" ->
            True

        "9" ->
            True

        _ ->
            False


isAsciiHexDigit : String -> Bool
isAsciiHexDigit character =
    case character of
        "0" ->
            True

        "1" ->
            True

        "2" ->
            True

        "3" ->
            True

        "4" ->
            True

        "5" ->
            True

        "6" ->
            True

        "7" ->
            True

        "8" ->
            True

        "9" ->
            True

        "a" ->
            True

        "A" ->
            True

        "b" ->
            True

        "B" ->
            True

        "c" ->
            True

        "C" ->
            True

        "d" ->
            True

        "D" ->
            True

        "e" ->
            True

        "E" ->
            True

        "f" ->
            True

        "F" ->
            True

        _ ->
            False


isWhitespace : String -> Bool
isWhitespace character =
    case character of
        " " ->
            True

        "\u{000D}" ->
            True

        "\n" ->
            True

        "\t" ->
            True

        _ ->
            False


isOperatorChar : String -> Bool
isOperatorChar character =
    {-
    TODO: Expand code analysis to optimize form using `List.member` to get the same level of efficiency:
    List.member char [ "+", "-", "/", "*", "=", ".", "$", "<", ">", ":", "&", "|", "^", "?", "%", "#", "!" ]
    -}
    case character of
        "+" ->
            True

        "-" ->
            True

        "/" ->
            True

        "*" ->
            True

        "=" ->
            True

        "." ->
            True

        "$" ->
            True

        "<" ->
            True

        ">" ->
            True

        ":" ->
            True

        "&" ->
            True

        "|" ->
            True

        "^" ->
            True

        "?" ->
            True

        "%" ->
            True

        "#" ->
            True

        "!" ->
            True

        _ ->
            False


isUpperCharacter : String -> Bool
isUpperCharacter character =
    case character of
        "A" ->
            True

        "B" ->
            True

        "C" ->
            True

        "D" ->
            True

        "E" ->
            True

        "F" ->
            True

        "G" ->
            True

        "H" ->
            True

        "I" ->
            True

        "J" ->
            True

        "K" ->
            True

        "L" ->
            True

        "M" ->
            True

        "N" ->
            True

        "O" ->
            True

        "P" ->
            True

        "Q" ->
            True

        "R" ->
            True

        "S" ->
            True

        "T" ->
            True

        "U" ->
            True

        "V" ->
            True

        "W" ->
            True

        "X" ->
            True

        "Y" ->
            True

        "Z" ->
            True

        _ ->
            False


startsWithUpper : String -> Bool
startsWithUpper name =
    isUpperCharacter (String.slice 0 1 name)



-- SOURCE SCANNING


skipInlineWhitespace : String -> Int -> Int
skipInlineWhitespace source offset =
    case String.slice offset (offset + 1) source of
        " " ->
            skipInlineWhitespace source (offset + 1)

        "\t" ->
            skipInlineWhitespace source (offset + 1)

        _ ->
            offset


skipToIdentifierEnd : String -> Int -> Int
skipToIdentifierEnd source offset =
    if isIdentifierChar (String.slice offset (offset + 1) source) then
        skipToIdentifierEnd source (offset + 1)

    else
        offset


skipToAsciiDecimalDigitEnd : String -> Int -> Int
skipToAsciiDecimalDigitEnd source offset =
    if isDigit (String.slice offset (offset + 1) source) then
        skipToAsciiDecimalDigitEnd source (offset + 1)

    else
        offset


skipToAsciiHexDigitEnd : String -> Int -> Int
skipToAsciiHexDigitEnd source offset =
    if isAsciiHexDigit (String.slice offset (offset + 1) source) then
        skipToAsciiHexDigitEnd source (offset + 1)

    else
        offset


skipOperatorChars : String -> Int -> Int -> Int
skipOperatorChars source offset offsetMax =
    if offset >= offsetMax then
        offset

    else if isOperatorChar (String.slice offset (offset + 1) source) then
        skipOperatorChars source (offset + 1) offsetMax

    else
        offset


numberEnd : String -> String -> Int -> Int
numberEnd source first startOffset =
    if first == "0" && String.slice (startOffset + 1) (startOffset + 2) source == "x" then
        skipToAsciiHexDigitEnd source (startOffset + 2)

    else
        numberEndDecimal source (startOffset + 1)


{-| Scans the decimal, fractional, and exponent portions of an Elm number.

Keeping this recursive scanner separate from `numberEnd` lets the runtime replace the complete
common decimal path with one precompiled leaf, instead of re-entering the VM for every phase.
-}
numberEndDecimal : String -> Int -> Int
numberEndDecimal source offset =
    case String.slice offset (offset + 1) source of
        first ->
            if isDigit first then
                numberEndDecimal source (offset + 1)

            else if first == "." && isDigit (String.slice (offset + 1) (offset + 2) source) then
                numberEndFraction source (offset + 2)

            else
                numberEndExponent source offset


numberEndFraction : String -> Int -> Int
numberEndFraction source offset =
    case String.slice offset (offset + 1) source of
        first ->
            if isDigit first then
                numberEndFraction source (offset + 1)

            else
                numberEndExponent source offset


numberEndExponent : String -> Int -> Int
numberEndExponent source offset =
    case String.slice offset (offset + 1) source of
        "e" ->
            numberEndExponentDigits source (offset + 1)

        "E" ->
            numberEndExponentDigits source (offset + 1)

        _ ->
            offset


numberEndExponentDigits : String -> Int -> Int
numberEndExponentDigits source offset =
    case String.slice offset (offset + 1) source of
        "+" ->
            skipToAsciiDecimalDigitEnd source (offset + 1)

        "-" ->
            skipToAsciiDecimalDigitEnd source (offset + 1)

        _ ->
            skipToAsciiDecimalDigitEnd source offset


{-| True when a non-hexadecimal numeric literal contains a decimal or exponent marker. -}
isFloatLiteral : String -> Bool
isFloatLiteral source =
    isFloatLiteralAt source 0


isFloatLiteralAt : String -> Int -> Bool
isFloatLiteralAt source offset =
    case String.slice offset (offset + 1) source of
        "." ->
            True

        "e" ->
            True

        "E" ->
            True

        "" ->
            False

        _ ->
            isFloatLiteralAt source (offset + 1)


{-| Finds the offset where a line comment's content ends: right before the first LF, CR, or
CRLF line break, or at the end of input. The line break itself is left unconsumed.
-}
lineCommentEnd : String -> Int -> Int
lineCommentEnd source offset =
    case String.slice offset (offset + 1) source of
        "" ->
            offset

        "\n" ->
            offset

        "\u{000D}" ->
            offset

        _ ->
            lineCommentEnd source (offset + 1)


type MultilineCommentRunEnd
    = MultilineCommentRunEnd_EndOfInput
    | MultilineCommentRunEnd_NewlineLF
    | MultilineCommentRunEnd_NewlineCRLF
    | MultilineCommentRunEnd_NewlineCR
    | MultilineCommentRunEnd_StartComment
    | MultilineCommentRunEnd_EndComment


{-| Finds the offset where a run of plain multi-line-comment content ends: at the next `{`, `-`,
line break, or the end of input. As with `findLiteralRunEnd`, this lets the caller take a single
`String.slice` for the whole run.
-}
multilineCommentRunEnd : String -> Int -> ( Int, MultilineCommentRunEnd )
multilineCommentRunEnd source offset =
    case String.slice offset (offset + 2) source of
        "{-" ->
            ( offset, MultilineCommentRunEnd_StartComment )

        "-}" ->
            ( offset, MultilineCommentRunEnd_EndComment )

        "\u{000D}\n" ->
            ( offset, MultilineCommentRunEnd_NewlineCRLF )

        _ ->
            case String.slice offset (offset + 1) source of
                "" ->
                    ( offset, MultilineCommentRunEnd_EndOfInput )

                "\n" ->
                    ( offset, MultilineCommentRunEnd_NewlineLF )

                "\u{000D}" ->
                    ( offset, MultilineCommentRunEnd_NewlineCR )

                _ ->
                    multilineCommentRunEnd source (offset + 1)



-- LITERAL SCANNING


type LiteralTermination
    = SingleQuoteTermination
    | DoubleQuoteTermination
    | TripleQuoteTermination


literalTerminationLength : LiteralTermination -> Int
literalTerminationLength termination =
    case termination of
        SingleQuoteTermination ->
            1

        DoubleQuoteTermination ->
            1

        TripleQuoteTermination ->
            3


{-| Where a run of plain literal content ends, and why it ended.
-}
type LiteralRunBoundary
    = LiteralRunTermination
    | LiteralRunBackslash
    | LiteralRunNewlineLF
    | LiteralRunNewlineCRLF
    | LiteralRunNewlineCR
    | LiteralRunUnterminated


{-| Scans forward from `offset` while the source neither matches the termination sequence nor
contains a backslash escape or a line break, returning the offset where that run of plain
content ends together with the reason it stopped. Callers use this offset to take a single
`String.slice` for the whole run instead of accumulating characters one at a time.
-}
findLiteralRunEnd : LiteralTermination -> String -> Int -> ( Int, LiteralRunBoundary )
findLiteralRunEnd termination source offset =
    case termination of
        SingleQuoteTermination ->
            findSingleQuotedLiteralRunEnd source offset

        DoubleQuoteTermination ->
            findDoubleQuotedLiteralRunEnd source offset

        TripleQuoteTermination ->
            findTripleQuotedLiteralRunEnd source offset


findSingleQuotedLiteralRunEnd : String -> Int -> ( Int, LiteralRunBoundary )
findSingleQuotedLiteralRunEnd source offset =
    case String.slice offset (offset + 1) source of
        "'" ->
            ( offset, LiteralRunTermination )

        "" ->
            ( offset, LiteralRunUnterminated )

        "\\" ->
            ( offset, LiteralRunBackslash )

        "\n" ->
            ( offset, LiteralRunNewlineLF )

        "\u{000D}" ->
            if String.slice (offset + 1) (offset + 2) source == "\n" then
                ( offset, LiteralRunNewlineCRLF )

            else
                ( offset, LiteralRunNewlineCR )

        _ ->
            findSingleQuotedLiteralRunEnd source (offset + 1)


findDoubleQuotedLiteralRunEnd : String -> Int -> ( Int, LiteralRunBoundary )
findDoubleQuotedLiteralRunEnd source offset =
    case String.slice offset (offset + 1) source of
        "\"" ->
            ( offset, LiteralRunTermination )

        "" ->
            ( offset, LiteralRunUnterminated )

        "\\" ->
            ( offset, LiteralRunBackslash )

        "\n" ->
            ( offset, LiteralRunNewlineLF )

        "\u{000D}" ->
            if String.slice (offset + 1) (offset + 2) source == "\n" then
                ( offset, LiteralRunNewlineCRLF )

            else
                ( offset, LiteralRunNewlineCR )

        _ ->
            findDoubleQuotedLiteralRunEnd source (offset + 1)


findTripleQuotedLiteralRunEnd : String -> Int -> ( Int, LiteralRunBoundary )
findTripleQuotedLiteralRunEnd source offset =
    if String.slice offset (offset + 3) source == "\"\"\"" then
        ( offset, LiteralRunTermination )

    else
        case String.slice offset (offset + 1) source of
            "" ->
                ( offset, LiteralRunUnterminated )

            "\\" ->
                ( offset, LiteralRunBackslash )

            "\n" ->
                ( offset, LiteralRunNewlineLF )

            "\u{000D}" ->
                if String.slice (offset + 1) (offset + 2) source == "\n" then
                    ( offset, LiteralRunNewlineCRLF )

                else
                    ( offset, LiteralRunNewlineCR )

            _ ->
                findTripleQuotedLiteralRunEnd source (offset + 1)


scanUnicodeEscapeDigits : String -> Int -> Maybe ( Int, Int )
scanUnicodeEscapeDigits source offset =
    case String.slice offset (offset + 1) source of
        "0" ->
            Just (scanUnicodeEscapeDigitsHelp source (offset + 1) 0)

        "1" ->
            Just (scanUnicodeEscapeDigitsHelp source (offset + 1) 1)

        "2" ->
            Just (scanUnicodeEscapeDigitsHelp source (offset + 1) 2)

        "3" ->
            Just (scanUnicodeEscapeDigitsHelp source (offset + 1) 3)

        "4" ->
            Just (scanUnicodeEscapeDigitsHelp source (offset + 1) 4)

        "5" ->
            Just (scanUnicodeEscapeDigitsHelp source (offset + 1) 5)

        "6" ->
            Just (scanUnicodeEscapeDigitsHelp source (offset + 1) 6)

        "7" ->
            Just (scanUnicodeEscapeDigitsHelp source (offset + 1) 7)

        "8" ->
            Just (scanUnicodeEscapeDigitsHelp source (offset + 1) 8)

        "9" ->
            Just (scanUnicodeEscapeDigitsHelp source (offset + 1) 9)

        "a" ->
            Just (scanUnicodeEscapeDigitsHelp source (offset + 1) 10)

        "A" ->
            Just (scanUnicodeEscapeDigitsHelp source (offset + 1) 10)

        "b" ->
            Just (scanUnicodeEscapeDigitsHelp source (offset + 1) 11)

        "B" ->
            Just (scanUnicodeEscapeDigitsHelp source (offset + 1) 11)

        "c" ->
            Just (scanUnicodeEscapeDigitsHelp source (offset + 1) 12)

        "C" ->
            Just (scanUnicodeEscapeDigitsHelp source (offset + 1) 12)

        "d" ->
            Just (scanUnicodeEscapeDigitsHelp source (offset + 1) 13)

        "D" ->
            Just (scanUnicodeEscapeDigitsHelp source (offset + 1) 13)

        "e" ->
            Just (scanUnicodeEscapeDigitsHelp source (offset + 1) 14)

        "E" ->
            Just (scanUnicodeEscapeDigitsHelp source (offset + 1) 14)

        "f" ->
            Just (scanUnicodeEscapeDigitsHelp source (offset + 1) 15)

        "F" ->
            Just (scanUnicodeEscapeDigitsHelp source (offset + 1) 15)

        _ ->
            Nothing


scanUnicodeEscapeDigitsHelp : String -> Int -> Int -> ( Int, Int )
scanUnicodeEscapeDigitsHelp source offset value =
    case String.slice offset (offset + 1) source of
        "0" ->
            scanUnicodeEscapeDigitsHelp source (offset + 1) (value * 16)

        "1" ->
            scanUnicodeEscapeDigitsHelp source (offset + 1) (value * 16 + 1)

        "2" ->
            scanUnicodeEscapeDigitsHelp source (offset + 1) (value * 16 + 2)

        "3" ->
            scanUnicodeEscapeDigitsHelp source (offset + 1) (value * 16 + 3)

        "4" ->
            scanUnicodeEscapeDigitsHelp source (offset + 1) (value * 16 + 4)

        "5" ->
            scanUnicodeEscapeDigitsHelp source (offset + 1) (value * 16 + 5)

        "6" ->
            scanUnicodeEscapeDigitsHelp source (offset + 1) (value * 16 + 6)

        "7" ->
            scanUnicodeEscapeDigitsHelp source (offset + 1) (value * 16 + 7)

        "8" ->
            scanUnicodeEscapeDigitsHelp source (offset + 1) (value * 16 + 8)

        "9" ->
            scanUnicodeEscapeDigitsHelp source (offset + 1) (value * 16 + 9)

        "a" ->
            scanUnicodeEscapeDigitsHelp source (offset + 1) (value * 16 + 10)

        "A" ->
            scanUnicodeEscapeDigitsHelp source (offset + 1) (value * 16 + 10)

        "b" ->
            scanUnicodeEscapeDigitsHelp source (offset + 1) (value * 16 + 11)

        "B" ->
            scanUnicodeEscapeDigitsHelp source (offset + 1) (value * 16 + 11)

        "c" ->
            scanUnicodeEscapeDigitsHelp source (offset + 1) (value * 16 + 12)

        "C" ->
            scanUnicodeEscapeDigitsHelp source (offset + 1) (value * 16 + 12)

        "d" ->
            scanUnicodeEscapeDigitsHelp source (offset + 1) (value * 16 + 13)

        "D" ->
            scanUnicodeEscapeDigitsHelp source (offset + 1) (value * 16 + 13)

        "e" ->
            scanUnicodeEscapeDigitsHelp source (offset + 1) (value * 16 + 14)

        "E" ->
            scanUnicodeEscapeDigitsHelp source (offset + 1) (value * 16 + 14)

        "f" ->
            scanUnicodeEscapeDigitsHelp source (offset + 1) (value * 16 + 15)

        "F" ->
            scanUnicodeEscapeDigitsHelp source (offset + 1) (value * 16 + 15)

        _ ->
            ( offset, value )


prependNonEmptyChunk : String -> List String -> List String
prependNonEmptyChunk chunk chunksRev =
    if String.isEmpty chunk then
        chunksRev

    else
        chunk :: chunksRev


concatenateChunksRev : List String -> String
concatenateChunksRev chunksRev =
    case chunksRev of
        [] ->
            ""

        _ ->
            String.concat (List.reverse chunksRev)



-- ERROR MESSAGE RENDERING


{-| Renders a source position as `row:column` for error messages. The argument is structurally
the same record as `ElmSyntax.Concrete.Range.Location`, spelled out here so that this module
stays independent of the syntax types.
-}
locationString : { row : Int, column : Int } -> String
locationString location =
    String.fromInt location.row ++ ":" ++ String.fromInt location.column



-- HEXADECIMAL CONVERSION


{-| Parses a (non-empty) string of ASCII hex digits into its integer value with specialized
first-order recursion. Used for hexadecimal integer literals and `\u{...}` escapes.
-}
hexStringToInt : String -> Maybe Int
hexStringToInt digits =
    case String.slice 0 1 digits of
        "0" ->
            Just 0

        "1" ->
            convert0OrMoreHexadecimalValue 1 1 digits

        "2" ->
            convert0OrMoreHexadecimalValue 2 1 digits

        "3" ->
            convert0OrMoreHexadecimalValue 3 1 digits

        "4" ->
            convert0OrMoreHexadecimalValue 4 1 digits

        "5" ->
            convert0OrMoreHexadecimalValue 5 1 digits

        "6" ->
            convert0OrMoreHexadecimalValue 6 1 digits

        "7" ->
            convert0OrMoreHexadecimalValue 7 1 digits

        "8" ->
            convert0OrMoreHexadecimalValue 8 1 digits

        "9" ->
            convert0OrMoreHexadecimalValue 9 1 digits

        "a" ->
            convert0OrMoreHexadecimalValue 10 1 digits

        "A" ->
            convert0OrMoreHexadecimalValue 10 1 digits

        "b" ->
            convert0OrMoreHexadecimalValue 11 1 digits

        "B" ->
            convert0OrMoreHexadecimalValue 11 1 digits

        "c" ->
            convert0OrMoreHexadecimalValue 12 1 digits

        "C" ->
            convert0OrMoreHexadecimalValue 12 1 digits

        "d" ->
            convert0OrMoreHexadecimalValue 13 1 digits

        "D" ->
            convert0OrMoreHexadecimalValue 13 1 digits

        "e" ->
            convert0OrMoreHexadecimalValue 14 1 digits

        "E" ->
            convert0OrMoreHexadecimalValue 14 1 digits

        "f" ->
            convert0OrMoreHexadecimalValue 15 1 digits

        "F" ->
            convert0OrMoreHexadecimalValue 15 1 digits

        _ ->
            Nothing


convert0OrMoreHexadecimalValue : Int -> Int -> String -> Maybe Int
convert0OrMoreHexadecimalValue value offset source =
    case String.slice offset (offset + 1) source of
        "" ->
            Just value

        "0" ->
            convert0OrMoreHexadecimalValue (value * 16) (offset + 1) source

        "1" ->
            convert0OrMoreHexadecimalValue (value * 16 + 1) (offset + 1) source

        "2" ->
            convert0OrMoreHexadecimalValue (value * 16 + 2) (offset + 1) source

        "3" ->
            convert0OrMoreHexadecimalValue (value * 16 + 3) (offset + 1) source

        "4" ->
            convert0OrMoreHexadecimalValue (value * 16 + 4) (offset + 1) source

        "5" ->
            convert0OrMoreHexadecimalValue (value * 16 + 5) (offset + 1) source

        "6" ->
            convert0OrMoreHexadecimalValue (value * 16 + 6) (offset + 1) source

        "7" ->
            convert0OrMoreHexadecimalValue (value * 16 + 7) (offset + 1) source

        "8" ->
            convert0OrMoreHexadecimalValue (value * 16 + 8) (offset + 1) source

        "9" ->
            convert0OrMoreHexadecimalValue (value * 16 + 9) (offset + 1) source

        "a" ->
            convert0OrMoreHexadecimalValue (value * 16 + 10) (offset + 1) source

        "A" ->
            convert0OrMoreHexadecimalValue (value * 16 + 10) (offset + 1) source

        "b" ->
            convert0OrMoreHexadecimalValue (value * 16 + 11) (offset + 1) source

        "B" ->
            convert0OrMoreHexadecimalValue (value * 16 + 11) (offset + 1) source

        "c" ->
            convert0OrMoreHexadecimalValue (value * 16 + 12) (offset + 1) source

        "C" ->
            convert0OrMoreHexadecimalValue (value * 16 + 12) (offset + 1) source

        "d" ->
            convert0OrMoreHexadecimalValue (value * 16 + 13) (offset + 1) source

        "D" ->
            convert0OrMoreHexadecimalValue (value * 16 + 13) (offset + 1) source

        "e" ->
            convert0OrMoreHexadecimalValue (value * 16 + 14) (offset + 1) source

        "E" ->
            convert0OrMoreHexadecimalValue (value * 16 + 14) (offset + 1) source

        "f" ->
            convert0OrMoreHexadecimalValue (value * 16 + 15) (offset + 1) source

        "F" ->
            convert0OrMoreHexadecimalValue (value * 16 + 15) (offset + 1) source

        _ ->
            Nothing
