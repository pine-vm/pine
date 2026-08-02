module ElmSyntax.Concrete.Parser.TokensFromString exposing (..)

import Char
import ElmSyntax.Concrete.Parser.Token as Token
import ElmSyntax.Concrete.Range as Range


parseFile : String -> Result String (List Token.Token)
parseFile input =
    tokenize input


parseExpression : String -> Result String (List Token.Token)
parseExpression input =
    tokenize input


parseDeclaration : String -> Result String (List Token.Token)
parseDeclaration input =
    tokenize input


parseExpressionOrDeclaration : String -> Result String (List Token.Token)
parseExpressionOrDeclaration input =
    tokenize input


{-| Tracks the current position while scanning the source text: `offset` is the index (in
`String.slice` terms) of the next character to inspect, while `row` and `column` are the
1-based source location corresponding to that offset. Keeping all three together lets every
scan branch advance offset and row/column in one step, instead of computing the source
location in a separate pass afterward.
-}
type alias Position =
    { offset : Int
    , row : Int
    , column : Int
    }


positionLocation : Position -> Range.Location
positionLocation position =
    { row = position.row, column = position.column }


tokenize : String -> Result String (List Token.Token)
tokenize input =
    tokenizeHelp input { offset = 0, row = 1, column = 1 } []


tokenizeHelp : String -> Position -> List Token.Token -> Result String (List Token.Token)
tokenizeHelp source position tokensRev =
    if
        position.offset >= 0
        {-
        Add an explicit branch to make it trivial to prove that `position.offset` is >= 0 for all usages below.
        Based on that proof, compiler have an easier way to prove that `String.slice` is always called non-negative offsets,
        which in turn allows the compile-time removal of the branches in those instances of `String.slice`
        -}
    then
        case classifyAt source position.offset of
            AtEnd ->
                Ok (List.reverse tokensRev)

            AtNewlineLF ->
                emitNewline source position 1 tokensRev

            AtNewlineCRLF ->
                emitNewline source position 2 tokensRev

            AtNewlineCR ->
                emitNewline source position 1 tokensRev

            AtChar first ->
                if isWhitespace first then
                    let
                        endOffset =
                            skipAllWhitespace source position.offset

                        lexeme =
                            String.slice position.offset endOffset source

                        nextPosition =
                            { offset = endOffset
                            , row = position.row
                            , column = position.column + (endOffset - position.offset)
                            }
                    in
                    tokenizeHelp
                        source
                        nextPosition
                        (makeToken
                            Token.Whitespace
                            lexeme
                            (positionLocation position)
                            (positionLocation nextPosition)
                            Nothing
                            :: tokensRev
                        )

                else if isDigit first then
                    let
                        endOffset =
                            numberEnd source position.offset

                        lexeme =
                            String.slice position.offset endOffset source

                        nextPosition =
                            { offset = endOffset
                            , row = position.row
                            , column = position.column + (endOffset - position.offset)
                            }
                    in
                    tokenizeHelp
                        source
                        nextPosition
                        (makeToken
                            Token.NumberLiteral
                            lexeme
                            (positionLocation position)
                            (positionLocation nextPosition)
                            Nothing
                            :: tokensRev
                        )

                else if isIdentifierStart first then
                    let
                        endOffset =
                            skipToIdentifierEnd source position.offset

                        lexeme =
                            String.slice position.offset endOffset source

                        nextPosition =
                            { offset = endOffset
                            , row = position.row
                            , column = position.column + (endOffset - position.offset)
                            }
                    in
                    tokenizeHelp
                        source
                        nextPosition
                        (makeToken
                            Token.Identifier
                            lexeme
                            (positionLocation position)
                            (positionLocation nextPosition)
                            Nothing
                            :: tokensRev
                        )

                else
                    tokenizeSymbol source position tokensRev

    else
        Err
            ("Internal error: negative offset "
                ++ String.fromInt position.offset ++ " at " ++ locationString (positionLocation position) ++ "."
            )


{-| Emits a single `Newline` token whose lexeme is always `"\n"`, regardless of whether the
source used LF, CRLF, or a lone CR at this position. `consumedLength` is how many source
characters the line break itself occupies (1 for LF or CR, 2 for CRLF); the row always
advances by exactly one and the column resets to 1.
-}
emitNewline : String -> Position -> Int -> List Token.Token -> Result String (List Token.Token)
emitNewline source position consumedLength tokensRev =
    let
        nextPosition =
            { offset = position.offset + consumedLength, row = position.row + 1, column = 1 }
    in
    tokenizeHelp
        source
        nextPosition
        (makeToken
            Token.Newline
            "\n"
            (positionLocation position)
            (positionLocation nextPosition)
            Nothing
            :: tokensRev
        )


{-| Classifies the source at a given offset as either the end of input, one of the three
supported line-break forms (LF, CRLF, or a lone CR), or an ordinary character. Every place in
this module that needs to recognize line breaks (the main tokenizer loop, multi-line comments,
and string/char literal content) shares this classification so that LF, CRLF, and CR are all
handled consistently and in a single pass over the source.
-}
type Classified
    = AtEnd
    | AtNewlineLF
    | AtNewlineCRLF
    | AtNewlineCR
    | AtChar String


classifyAt : String -> Int -> Classified
classifyAt source offset =
    case String.slice offset (offset + 2) source of
        "\u{000D}\n" ->
            AtNewlineCRLF

        _ ->
            case String.slice offset (offset + 1) source of
                "" ->
                    AtEnd

                "\n" ->
                    AtNewlineLF

                "\u{000D}" ->
                    AtNewlineCR

                other ->
                    AtChar other


tokenizeSymbol : String -> Position -> List Token.Token -> Result String (List Token.Token)
tokenizeSymbol source position tokensRev =
    let
        addToken : Token.TokenType -> String -> Int -> Result String (List Token.Token)
        addToken tokenType lexeme consumedLength =
            let
                nextPosition =
                    { offset = position.offset + consumedLength
                    , row = position.row
                    , column = position.column + consumedLength
                    }
            in
            tokenizeHelp
                source
                nextPosition
                (makeToken
                    tokenType
                    lexeme
                    (positionLocation position)
                    (positionLocation nextPosition)
                    Nothing
                    :: tokensRev
                )

        addSingle : Token.TokenType -> Result String (List Token.Token)
        addSingle tokenType =
            addToken tokenType (String.slice position.offset (position.offset + 1) source) 1
    in
    case String.slice position.offset (position.offset + 1) source of
        "" ->
            Ok (List.reverse tokensRev)

        "-" ->
            case String.slice (position.offset + 1) (position.offset + 2) source of
                "-" ->
                    let
                        contentEnd =
                            lineCommentEnd source (position.offset + 2)

                        lexeme =
                            String.slice position.offset contentEnd source

                        nextPosition =
                            { offset = contentEnd
                            , row = position.row
                            , column = position.column + (contentEnd - position.offset)
                            }
                    in
                    tokenizeHelp source
                        nextPosition
                        (makeToken Token.Comment
                            lexeme
                            (positionLocation position)
                            (positionLocation nextPosition)
                            Nothing
                            :: tokensRev
                        )

                ">" ->
                    addToken Token.Arrow "->" 2

                _ ->
                    if minusIsOperator source position tokensRev then
                        addSingle Token.Operator

                    else
                        addSingle Token.Negation

        "{" ->
            case String.slice (position.offset + 1) (position.offset + 2) source of
                "-" ->
                    let
                        contentStart =
                            { offset = position.offset + 2
                            , row = position.row
                            , column = position.column + 2
                            }
                    in
                    tokenizeMultilineComment source contentStart position tokensRev 1 "{-"

                _ ->
                    addSingle Token.OpenBrace

        "\"" ->
            if String.slice position.offset (position.offset + 3) source == "\"\"\"" then
                tokenizeLiteral Token.TripleQuotedStringLiteral "\"\"\"" source position tokensRev

            else
                tokenizeLiteral Token.StringLiteral "\"" source position tokensRev

        "'" ->
            tokenizeLiteral Token.CharLiteral "'" source position tokensRev

        "\\" ->
            addSingle Token.Lambda

        "(" ->
            addSingle Token.OpenParen

        ")" ->
            addSingle Token.CloseParen

        "}" ->
            addSingle Token.CloseBrace

        "[" ->
            addSingle Token.OpenBracket

        "]" ->
            addSingle Token.CloseBracket

        "," ->
            addSingle Token.Comma

        "." ->
            case String.slice (position.offset + 1) (position.offset + 2) source of
                "." ->
                    addToken Token.DotDot ".." 2

                next ->
                    if isOperatorChar next then
                        addToken Token.Operator (String.slice position.offset (position.offset + 2) source) 2

                    else
                        addSingle Token.Dot

        "=" ->
            if isOperatorChar (String.slice (position.offset + 1) (position.offset + 2) source) then
                addToken Token.Operator (String.slice position.offset (position.offset + 2) source) 2

            else
                addSingle Token.Equal

        "|" ->
            if isOperatorChar (String.slice (position.offset + 1) (position.offset + 2) source) then
                addToken Token.Operator (String.slice position.offset (position.offset + 2) source) 2

            else
                addSingle Token.Pipe

        ":" ->
            if isOperatorChar (String.slice (position.offset + 1) (position.offset + 2) source) then
                addToken Token.Operator (String.slice position.offset (position.offset + 2) source) 2

            else
                addSingle Token.Colon

        first ->
            if isOperatorChar first then
                let
                    endOffset =
                        takeAtMostEnd 2 isOperatorChar source (position.offset + 1)

                    lexeme =
                        String.slice position.offset endOffset source
                in
                addToken Token.Operator lexeme (endOffset - position.offset)

            else
                addSingle Token.Unknown


{-| Finds the offset where a line comment's content ends: right before the first LF, CR, or
CRLF line break, or at the end of input. The line break itself is left unconsumed so that the
next call into `tokenizeHelp` emits the corresponding `Newline` token.
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


tokenizeLiteral :
    Token.TokenType
    -> String
    -> String
    -> Position
    -> List Token.Token
    -> Result String (List Token.Token)
tokenizeLiteral tokenType termination source start tokensRev =
    let
        terminationLength =
            String.length termination

        afterOpening =
            { offset = start.offset + terminationLength
            , row = start.row
            , column = start.column + terminationLength
            }
    in
    case consumeLiteral termination source start afterOpening "" "" of
        Ok consumed ->
            tokenizeHelp source
                consumed.end
                (makeToken tokenType
                    consumed.decoded
                    (positionLocation start)
                    (positionLocation consumed.end)
                    (Just consumed.raw) :: tokensRev
                )

        Err error ->
            Err error


type alias ConsumedLiteral =
    { decoded : String
    , raw : String
    , end : Position
    }


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
findLiteralRunEnd : String -> String -> Int -> ( Int, LiteralRunBoundary )
findLiteralRunEnd termination source offset =
    let
        terminationLength =
            String.length termination
    in
    if String.slice offset (offset + terminationLength) source == termination then
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
                findLiteralRunEnd termination source (offset + 1)


consumeLiteral :
    String
    -> String
    -> Position
    -> Position
    -> String
    -> String
    -> Result String ConsumedLiteral
consumeLiteral termination source start position decodedAcc rawAcc =
    let
        ( runEndOffset, boundary ) =
            findLiteralRunEnd termination source position.offset

        run =
            String.slice position.offset runEndOffset source

        positionAfterRun =
            { offset = runEndOffset
            , row = position.row
            , column = position.column + (runEndOffset - position.offset)
            }

        decodedAfterRun =
            decodedAcc ++ run

        rawAfterRun =
            rawAcc ++ run
    in
    case boundary of
        LiteralRunTermination ->
            let
                terminationLength =
                    String.length termination

                endPosition =
                    { offset = positionAfterRun.offset + terminationLength
                    , row = positionAfterRun.row
                    , column = positionAfterRun.column + terminationLength
                    }
            in
            Ok { decoded = decodedAfterRun, raw = rawAfterRun, end = endPosition }

        LiteralRunUnterminated ->
            Err ("Unterminated literal at " ++ locationString (positionLocation start) ++ ".")

        LiteralRunNewlineLF ->
            let
                nextPosition =
                    { offset = positionAfterRun.offset + 1, row = positionAfterRun.row + 1, column = 1 }
            in
            consumeLiteral termination source start nextPosition (decodedAfterRun ++ "\n") (rawAfterRun ++ "\n")

        LiteralRunNewlineCRLF ->
            let
                nextPosition =
                    { offset = positionAfterRun.offset + 2, row = positionAfterRun.row + 1, column = 1 }
            in
            consumeLiteral termination source start nextPosition (decodedAfterRun ++ "\n") (rawAfterRun ++ "\n")

        LiteralRunNewlineCR ->
            let
                nextPosition =
                    { offset = positionAfterRun.offset + 1, row = positionAfterRun.row + 1, column = 1 }
            in
            consumeLiteral termination source start nextPosition (decodedAfterRun ++ "\n") (rawAfterRun ++ "\n")

        LiteralRunBackslash ->
            case String.slice (positionAfterRun.offset + 1) (positionAfterRun.offset + 2) source of
                "u" ->
                    consumeUnicodeEscape termination source start positionAfterRun decodedAfterRun rawAfterRun

                "" ->
                    Err ("Unterminated literal at " ++ locationString (positionLocation start) ++ ".")

                escaped ->
                    let
                        decodedCharacter =
                            case escaped of
                                "n" ->
                                    "\n"

                                "r" ->
                                    "\u{000D}"

                                "t" ->
                                    "\t"

                                _ ->
                                    escaped

                        nextPosition =
                            { offset = positionAfterRun.offset + 2
                            , row = positionAfterRun.row
                            , column = positionAfterRun.column + 2
                            }
                    in
                    consumeLiteral termination
                        source
                        start
                        nextPosition
                        (decodedAfterRun ++ decodedCharacter)
                        (rawAfterRun ++ "\\" ++ escaped)


{-| Handles a `\u...` escape found immediately at `escapeStart.offset` (that is, `escapeStart`
points at the backslash). Only the `\u{XXXX}` form is a valid unicode escape; any other
character (or no `{`) following `\u` falls back to treating it the same way an unrecognized
single-character escape like `\z` would be treated elsewhere, i.e. decoding to a literal `u`.
-}
consumeUnicodeEscape :
    String
    -> String
    -> Position
    -> Position
    -> String
    -> String
    -> Result String ConsumedLiteral
consumeUnicodeEscape termination source start escapeStart decodedAcc rawAcc =
    let
        afterPrefixOffset =
            escapeStart.offset + 2
    in
    if String.slice afterPrefixOffset (afterPrefixOffset + 1) source == "{" then
        let
            digitsEndOffset =
                skipToAsciiHexDigitEnd source (afterPrefixOffset + 1)
        in
        if String.slice digitsEndOffset (digitsEndOffset + 1) source == "}" then
            let
                digitsText =
                    String.slice (afterPrefixOffset + 1) digitsEndOffset source
            in
            case hexStringToInt digitsText of
                Just codePoint ->
                    if codePoint <= 0x10FFFF && not (codePoint >= 0xD800 && codePoint <= 0xDFFF) then
                        let
                            consumedLength =
                                (digitsEndOffset + 1) - escapeStart.offset

                            nextPosition =
                                { offset = digitsEndOffset + 1
                                , row = escapeStart.row
                                , column = escapeStart.column + consumedLength
                                }

                            rawEscape =
                                String.slice escapeStart.offset (digitsEndOffset + 1) source
                        in
                        consumeLiteral termination
                            source
                            start
                            nextPosition
                            (decodedAcc ++ String.fromChar (Char.fromCode codePoint))
                            (rawAcc ++ rawEscape)

                    else
                        Err ("Invalid unicode escape at " ++ locationString (positionLocation escapeStart) ++ ".")

                Nothing ->
                    Err ("Invalid unicode escape at " ++ locationString (positionLocation escapeStart) ++ ".")

        else
            Err ("Invalid unicode escape at " ++ locationString (positionLocation escapeStart) ++ ".")

    else
        let
            nextPosition =
                { offset = afterPrefixOffset
                , row = escapeStart.row
                , column = escapeStart.column + 2
                }
        in
        consumeLiteral termination
            source
            start
            nextPosition
            (decodedAcc ++ "u")
            (rawAcc ++ "\\u")


tokenizeMultilineComment :
    String
    -> Position
    -> Position
    -> List Token.Token
    -> Int
    -> String
    -> Result String (List Token.Token)
tokenizeMultilineComment source position start tokensRev depth accumulated =
    if
        position.offset >= 0
        {-
        Add an explicit branch to make it trivial to prove that `position.offset` is >= 0 for all usages below.
        Based on that proof, compiler have an easier way to prove that `String.slice` is always called non-negative offsets,
        which in turn allows the compile-time removal of the branches in those instances of `String.slice`
        -}
    then
        let
            ( runEndOffset, runEndType ) =
                multilineCommentRunEnd source position.offset

            run =
                String.slice position.offset runEndOffset source

            positionAfterRun =
                { offset = runEndOffset
                , row = position.row
                , column = position.column + (runEndOffset - position.offset)
                }

            accumulatedAfterRun =
                accumulated ++ run
        in
        case classifyAt source positionAfterRun.offset of
            AtEnd ->
                Err ("Unterminated comment at " ++ locationString (positionLocation start) ++ ".")

            AtNewlineLF ->
                let
                    nextPosition =
                        { offset = positionAfterRun.offset + 1
                        , row = positionAfterRun.row + 1
                        , column = 1
                        }
                in
                tokenizeMultilineComment
                    source
                    nextPosition
                    start
                    tokensRev
                    depth
                    (accumulatedAfterRun ++ "\n")

            AtNewlineCRLF ->
                let
                    nextPosition =
                        { offset = positionAfterRun.offset + 2
                        , row = positionAfterRun.row + 1
                        , column = 1
                        }
                in
                tokenizeMultilineComment
                    source
                    nextPosition
                    start
                    tokensRev
                    depth
                    (accumulatedAfterRun ++ "\n")

            AtNewlineCR ->
                let
                    nextPosition =
                        { offset = positionAfterRun.offset + 1
                        , row = positionAfterRun.row + 1
                        , column = 1
                        }
                in
                tokenizeMultilineComment
                    source
                    nextPosition
                    start
                    tokensRev
                    depth
                    (accumulatedAfterRun ++ "\n")

            AtChar other ->
                case runEndType of
                    MultilineCommentRunEnd_StartComment ->
                        let
                            nextPosition =
                                { offset = positionAfterRun.offset + 2
                                , row = positionAfterRun.row
                                , column = positionAfterRun.column + 2
                                }
                        in
                        tokenizeMultilineComment
                            source
                            nextPosition
                            start
                            tokensRev
                            (depth + 1)
                            (accumulatedAfterRun ++ "{-")

                    MultilineCommentRunEnd_EndComment ->
                        let
                            finalLexeme =
                                accumulatedAfterRun ++ "-}"

                            endPosition =
                                { offset = positionAfterRun.offset + 2
                                , row = positionAfterRun.row
                                , column = positionAfterRun.column + 2
                                }
                        in
                        if depth == 1 then
                            tokenizeHelp
                                source
                                endPosition
                                (makeToken
                                    Token.Comment
                                    finalLexeme
                                    (positionLocation start)
                                    (positionLocation endPosition)
                                    Nothing
                                    :: tokensRev
                                )

                        else
                            tokenizeMultilineComment source endPosition start tokensRev (depth - 1) finalLexeme

                    MultilineCommentRunEnd_Other ->
                        -- Unreachable: `multilineCommentRunEnd` only stops at '{', '-', a line break, or
                        -- the end of input, all of which are handled above. Kept for totality.
                        let
                            nextPosition =
                                { offset = positionAfterRun.offset + 1
                                , row = positionAfterRun.row
                                , column = positionAfterRun.column + 1
                                }
                        in
                        tokenizeMultilineComment
                            source
                            nextPosition
                            start
                            tokensRev
                            depth
                            (accumulatedAfterRun ++ other)

    else
        Err
            ("Internal error: negative offset "
                ++ String.fromInt position.offset ++ " at " ++ locationString (positionLocation position) ++ "."
            )


type MultilineCommentRunEnd
    = MultilineCommentRunEnd_StartComment
    | MultilineCommentRunEnd_EndComment
    | MultilineCommentRunEnd_Other


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

        _ ->
            case String.slice offset (offset + 1) source of
                "" ->
                    ( offset, MultilineCommentRunEnd_Other )

                "\n" ->
                    ( offset, MultilineCommentRunEnd_Other )

                "\u{000D}" ->
                    ( offset, MultilineCommentRunEnd_Other )

                _ ->
                    multilineCommentRunEnd source (offset + 1)


minusIsOperator : String -> Position -> List Token.Token -> Bool
minusIsOperator source position tokensRev =
    case String.slice (position.offset + 1) (position.offset + 2) source of
        "" ->
            True

        next ->
            isWhitespace next
                || (case next of
                        ")" ->
                            True

                        "]" ->
                            True

                        "}" ->
                            True

                        _ ->
                            False
                   )
                || previousAdjacentTokenCanEndExpression tokensRev


previousAdjacentTokenCanEndExpression : List Token.Token -> Bool
previousAdjacentTokenCanEndExpression tokensRev =
    case tokensRev of
        token :: _ ->
            case token.tokenType of
                Token.Identifier ->
                    True

                Token.NumberLiteral ->
                    True

                Token.StringLiteral ->
                    True

                Token.TripleQuotedStringLiteral ->
                    True

                Token.CharLiteral ->
                    True

                Token.CloseParen ->
                    True

                Token.CloseBracket ->
                    True

                Token.CloseBrace ->
                    True

                _ ->
                    False

        [] ->
            False


makeToken :
    Token.TokenType
    -> String
    -> Range.Location
    -> Range.Location
    -> Maybe String
    -> Token.Token
makeToken tokenType lexeme start end rawText =
    { tokenType = tokenType
    , lexeme = lexeme
    , start = start
    , end = end
    , rawText = rawText
    }


numberEnd : String -> Int -> Int
numberEnd source startOffset =
    if String.slice startOffset (startOffset + 2) source == "0x" then
        skipToAsciiHexDigitEnd source (startOffset + 2)

    else
        let
            afterInteger =
                skipToAsciiDecimalDigitEnd source startOffset

            afterFraction =
                case String.slice afterInteger (afterInteger + 1) source of
                    "." ->
                        if isDigit (String.slice (afterInteger + 1) (afterInteger + 2) source) then
                            skipToAsciiDecimalDigitEnd source (afterInteger + 1)

                        else
                            afterInteger

                    _ ->
                        afterInteger
        in
        case String.slice afterFraction (afterFraction + 1) source of
            "e" ->
                exponentEnd source (afterFraction + 1)

            "E" ->
                exponentEnd source (afterFraction + 1)

            _ ->
                afterFraction


exponentEnd : String -> Int -> Int
exponentEnd source offset =
    let
        afterSign =
            case String.slice offset (offset + 1) source of
                "+" ->
                    offset + 1

                "-" ->
                    offset + 1

                _ ->
                    offset
    in
    skipToAsciiDecimalDigitEnd source afterSign


skipAllWhitespace : String -> Int -> Int
skipAllWhitespace source offset =
    if isWhitespace (String.slice offset (offset + 1) source) then
        skipAllWhitespace source (offset + 1)

    else
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


{-| Like `takeWhileEnd`, but stops after at most `remainingCount` further characters even if the
predicate keeps holding.
-}
takeAtMostEnd : Int -> (String -> Bool) -> String -> Int -> Int
takeAtMostEnd remainingCount predicate source offset =
    if remainingCount <= 0 then
        offset

    else if predicate (String.slice offset (offset + 1) source) then
        takeAtMostEnd (remainingCount - 1) predicate source (offset + 1)

    else
        offset


{-| Parses a (non-empty) string of ASCII hex digits into its integer value with specialized
first-order recursion. Shared with `FromString.hexStringToInt` so hexadecimal integer literals
and `\u{...}` escapes use the same implementation.
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


locationString : Range.Location -> String
locationString location =
    String.fromInt location.row ++ ":" ++ String.fromInt location.column
