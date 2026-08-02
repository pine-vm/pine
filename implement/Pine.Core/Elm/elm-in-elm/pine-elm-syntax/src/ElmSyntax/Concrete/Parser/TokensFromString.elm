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


makeLocation : Int -> Int -> Range.Location
makeLocation row column =
    { row = row, column = column }


tokenize : String -> Result String (List Token.Token)
tokenize input =
    tokenizeHelp input 0 1 1 []


tokenizeHelp : String -> Int -> Int -> Int -> List Token.Token -> Result String (List Token.Token)
tokenizeHelp source offset row column tokensRev =
    if
        offset >= 0
        {-
        Add an explicit branch to make it trivial to prove that `offset` is >= 0 for all usages below.
        Based on that proof, compiler have an easier way to prove that `String.slice` is always called non-negative offsets,
        which in turn allows the compile-time removal of the branches in those instances of `String.slice`
        -}
    then
        case String.slice offset (offset + 1) source of
            "" ->
                Ok (List.reverse tokensRev)

            "\n" ->
                skipNewline source offset row 1 tokensRev

            "\u{000D}" ->
                if String.slice (offset + 1) (offset + 2) source == "\n" then
                    skipNewline source offset row 2 tokensRev

                else
                    skipNewline source offset row 1 tokensRev

            first ->
                if isWhitespace first then
                    let
                        endOffset =
                            skipInlineWhitespace source (offset + 1)

                        endColumn =
                            column + (endOffset - offset)
                    in
                    tokenizeHelp source endOffset row endColumn tokensRev

                else if isDigit first then
                    let
                        endOffset =
                            numberEnd source first offset

                        lexeme =
                            String.slice offset endOffset source

                        endColumn =
                            column + (endOffset - offset)
                    in
                    tokenizeHelp
                        source
                        endOffset
                        row
                        endColumn
                        (makeToken
                            Token.NumberLiteral
                            lexeme
                            (makeLocation row column)
                            (makeLocation row endColumn)
                            Nothing
                            :: tokensRev
                        )

                else if isIdentifierStart first then
                    let
                        endOffset =
                            skipToIdentifierEnd source (offset + 1)

                        lexeme =
                            String.slice offset endOffset source

                        endColumn =
                            column + (endOffset - offset)
                    in
                    tokenizeHelp
                        source
                        endOffset
                        row
                        endColumn
                        (makeToken
                            Token.Identifier
                            lexeme
                            (makeLocation row column)
                            (makeLocation row endColumn)
                            Nothing
                            :: tokensRev
                        )

                else
                    tokenizeSymbol first source offset row column tokensRev

    else
        Err
            ("Internal error: negative offset "
                ++ String.fromInt offset ++ " at " ++ locationString (makeLocation row column) ++ "."
            )


{-| Advances past an LF, CRLF, or lone CR without emitting a token. `consumedLength` is how
many source characters the line break occupies (1 for LF or CR, 2 for CRLF); the row always
advances by exactly one and the column resets to 1.
-}
skipNewline : String -> Int -> Int -> Int -> List Token.Token -> Result String (List Token.Token)
skipNewline source offset row consumedLength tokensRev =
    tokenizeHelp source (offset + consumedLength) (row + 1) 1 tokensRev


tokenizeSymbol : String -> String -> Int -> Int -> Int -> List Token.Token -> Result String (List Token.Token)
tokenizeSymbol first source offset row column tokensRev =
    let
        addToken : Token.TokenType -> String -> Int -> Result String (List Token.Token)
        addToken tokenType lexeme consumedLength =
            let
                nextOffset =
                    offset + consumedLength

                nextColumn =
                    column + consumedLength
            in
            tokenizeHelp
                source
                nextOffset
                row
                nextColumn
                (makeToken
                    tokenType
                    lexeme
                    (makeLocation row column)
                    (makeLocation row nextColumn)
                    Nothing
                    :: tokensRev
                )

        addSingle : Token.TokenType -> Result String (List Token.Token)
        addSingle tokenType =
            addToken tokenType first 1
    in
    case first of
        "-" ->
            case String.slice (offset + 1) (offset + 2) source of
                "-" ->
                    let
                        contentEnd =
                            lineCommentEnd source (offset + 2)

                        lexeme =
                            String.slice offset contentEnd source

                        endColumn =
                            column + (contentEnd - offset)
                    in
                    tokenizeHelp
                        source
                        contentEnd
                        row
                        endColumn
                        (makeToken Token.Comment
                            lexeme
                            (makeLocation row column)
                            (makeLocation row endColumn)
                            Nothing
                            :: tokensRev
                        )

                ">" ->
                    addToken Token.Arrow "->" 2

                _ ->
                    if minusIsOperator source offset row column tokensRev then
                        addSingle Token.Operator

                    else
                        addSingle Token.Negation

        "{" ->
            case String.slice (offset + 1) (offset + 2) source of
                "-" ->
                    tokenizeMultilineComment source (offset + 2) row (column + 2) row column tokensRev 1 [ "{-" ]

                _ ->
                    addSingle Token.OpenBrace

        "\"" ->
            if String.slice offset (offset + 3) source == "\"\"\"" then
                tokenizeLiteral
                    Token.TripleQuotedStringLiteral
                    TripleQuoteTermination
                    source
                    offset
                    row
                    column
                    tokensRev

            else
                tokenizeLiteral
                    Token.StringLiteral
                    DoubleQuoteTermination
                    source
                    offset
                    row
                    column
                    tokensRev

        "'" ->
            tokenizeLiteral
                Token.CharLiteral
                SingleQuoteTermination
                source
                offset
                row
                column
                tokensRev

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
            case String.slice (offset + 1) (offset + 2) source of
                "." ->
                    addToken Token.DotDot ".." 2

                next ->
                    if isOperatorChar next then
                        addToken Token.Operator (String.slice offset (offset + 2) source) 2

                    else
                        addSingle Token.Dot

        "=" ->
            if isOperatorChar (String.slice (offset + 1) (offset + 2) source) then
                addToken Token.Operator (String.slice offset (offset + 2) source) 2

            else
                addSingle Token.Equal

        "|" ->
            if isOperatorChar (String.slice (offset + 1) (offset + 2) source) then
                addToken Token.Operator (String.slice offset (offset + 2) source) 2

            else
                addSingle Token.Pipe

        ":" ->
            if isOperatorChar (String.slice (offset + 1) (offset + 2) source) then
                addToken Token.Operator (String.slice offset (offset + 2) source) 2

            else
                addSingle Token.Colon

        symbol ->
            if isOperatorChar symbol then
                let
                    endOffset =
                        skipOperatorChars source (offset + 1) (offset + 3)

                    lexeme =
                        String.slice offset endOffset source
                in
                addToken Token.Operator lexeme (endOffset - offset)

            else
                addSingle Token.Unknown


{-| Finds the offset where a line comment's content ends: right before the first LF, CR, or
CRLF line break, or at the end of input. The line break itself is left unconsumed so that the
next call into `tokenizeHelp` advances the source position.
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


tokenizeLiteral :
    Token.TokenType
    -> LiteralTermination
    -> String
    -> Int
    -> Int
    -> Int
    -> List Token.Token
    -> Result String (List Token.Token)
tokenizeLiteral tokenType termination source startOffset startRow startColumn tokensRev =
    let
        terminationLength =
            literalTerminationLength termination

        afterOpeningOffset =
            startOffset + terminationLength
    in
    case
        consumeLiteral
            termination
            source
            startRow
            startColumn
            afterOpeningOffset
            startRow
            (startColumn + terminationLength)
            []
            []
    of
        Ok consumed ->
            tokenizeHelp
                source
                consumed.endOffset
                consumed.endRow
                consumed.endColumn
                (makeToken tokenType
                    consumed.decoded
                    (makeLocation startRow startColumn)
                    (makeLocation consumed.endRow consumed.endColumn)
                    (Just consumed.raw) :: tokensRev
                )

        Err error ->
            Err error


type alias ConsumedLiteral =
    { decoded : String
    , raw : String
    , endOffset : Int
    , endRow : Int
    , endColumn : Int
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


consumeLiteral :
    LiteralTermination
    -> String
    -> Int
    -> Int
    -> Int
    -> Int
    -> Int
    -> List String
    -> List String
    -> Result String ConsumedLiteral
consumeLiteral termination source startRow startColumn offset row column decodedChunksRev rawChunksRev =
    let
        ( runEndOffset, boundary ) =
            findLiteralRunEnd termination source offset

        run =
            String.slice offset runEndOffset source

        columnAfterRun =
            column + (runEndOffset - offset)

        decodedChunksAfterRun =
            prependNonEmptyChunk run decodedChunksRev

        rawChunksAfterRun =
            prependNonEmptyChunk run rawChunksRev
    in
    case boundary of
        LiteralRunTermination ->
            let
                terminationLength =
                    literalTerminationLength termination

                endOffset =
                    runEndOffset + terminationLength
            in
            Ok
                { decoded = concatenateChunksRev decodedChunksAfterRun
                , raw = concatenateChunksRev rawChunksAfterRun
                , endOffset = endOffset
                , endRow = row
                , endColumn = columnAfterRun + terminationLength
                }

        LiteralRunUnterminated ->
            Err ("Unterminated literal at " ++ locationString (makeLocation startRow startColumn) ++ ".")

        LiteralRunNewlineLF ->
            consumeLiteral
                termination
                source
                startRow
                startColumn
                (runEndOffset + 1)
                (row + 1)
                1
                ("\n" :: decodedChunksAfterRun)
                ("\n" :: rawChunksAfterRun)

        LiteralRunNewlineCRLF ->
            consumeLiteral
                termination
                source
                startRow
                startColumn
                (runEndOffset + 2)
                (row + 1)
                1
                ("\n" :: decodedChunksAfterRun)
                ("\n" :: rawChunksAfterRun)

        LiteralRunNewlineCR ->
            consumeLiteral
                termination
                source
                startRow
                startColumn
                (runEndOffset + 1)
                (row + 1)
                1
                ("\n" :: decodedChunksAfterRun)
                ("\n" :: rawChunksAfterRun)

        LiteralRunBackslash ->
            case String.slice (runEndOffset + 1) (runEndOffset + 2) source of
                "u" ->
                    consumeUnicodeEscape
                        termination
                        source
                        startRow
                        startColumn
                        runEndOffset
                        row
                        columnAfterRun
                        decodedChunksAfterRun
                        rawChunksAfterRun

                "" ->
                    Err ("Unterminated literal at " ++ locationString (makeLocation startRow startColumn) ++ ".")

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
                    in
                    consumeLiteral
                        termination
                        source
                        startRow
                        startColumn
                        (runEndOffset + 2)
                        row
                        (columnAfterRun + 2)
                        (decodedCharacter :: decodedChunksAfterRun)
                        (("\\" ++ escaped) :: rawChunksAfterRun)


{-| Handles a `\u...` escape beginning at `escapeOffset` (the backslash). Only the
`\u{XXXX}` form is a valid unicode escape; any other
character (or no `{`) following `\u` falls back to treating it the same way an unrecognized
single-character escape like `\z` would be treated elsewhere, i.e. decoding to a literal `u`.
-}
consumeUnicodeEscape :
    LiteralTermination
    -> String
    -> Int
    -> Int
    -> Int
    -> Int
    -> Int
    -> List String
    -> List String
    -> Result String ConsumedLiteral
consumeUnicodeEscape termination source startRow startColumn escapeOffset escapeRow escapeColumn decodedChunksRev rawChunksRev =
    let
        afterPrefixOffset =
            escapeOffset + 2
    in
    if String.slice afterPrefixOffset (afterPrefixOffset + 1) source == "{" then
        case scanUnicodeEscapeDigits source (afterPrefixOffset + 1) of
            Just ( digitsEndOffset, codePoint ) ->
                if
                    String.slice digitsEndOffset (digitsEndOffset + 1) source == "}"
                        && codePoint <= 0x10FFFF
                        && not (codePoint >= 0xD800 && codePoint <= 0xDFFF)
                then
                    let
                        consumedLength =
                            (digitsEndOffset + 1) - escapeOffset

                        rawEscape =
                            String.slice escapeOffset (digitsEndOffset + 1) source
                    in
                    consumeLiteral termination
                        source
                        startRow
                        startColumn
                        (digitsEndOffset + 1)
                        escapeRow
                        (escapeColumn + consumedLength)
                        (String.fromChar (Char.fromCode codePoint) :: decodedChunksRev)
                        (rawEscape :: rawChunksRev)

                else
                    Err ("Invalid unicode escape at " ++ locationString (makeLocation escapeRow escapeColumn) ++ ".")

            Nothing ->
                Err ("Invalid unicode escape at " ++ locationString (makeLocation escapeRow escapeColumn) ++ ".")

    else
        consumeLiteral
            termination
            source
            startRow
            startColumn
            afterPrefixOffset
            escapeRow
            (escapeColumn + 2)
            ("u" :: decodedChunksRev)
            ("\\u" :: rawChunksRev)


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
    String.concat (List.reverse chunksRev)


tokenizeMultilineComment :
    String
    -> Int
    -> Int
    -> Int
    -> Int
    -> Int
    -> List Token.Token
    -> Int
    -> List String
    -> Result String (List Token.Token)
tokenizeMultilineComment source offset row column startRow startColumn tokensRev depth chunksRev =
    if
        offset >= 0
        {-
        Add an explicit branch to make it trivial to prove that `offset` is >= 0 for all usages below.
        Based on that proof, compiler have an easier way to prove that `String.slice` is always called non-negative offsets,
        which in turn allows the compile-time removal of the branches in those instances of `String.slice`
        -}
    then
        let
            ( runEndOffset, runEndType ) =
                multilineCommentRunEnd source offset

            run =
                String.slice offset runEndOffset source

            columnAfterRun =
                column + (runEndOffset - offset)

            chunksAfterRun =
                prependNonEmptyChunk run chunksRev
        in
        case runEndType of
            MultilineCommentRunEnd_EndOfInput ->
                Err ("Unterminated comment at " ++ locationString (makeLocation startRow startColumn) ++ ".")

            MultilineCommentRunEnd_NewlineLF ->
                tokenizeMultilineComment
                    source
                    (runEndOffset + 1)
                    (row + 1)
                    1
                    startRow
                    startColumn
                    tokensRev
                    depth
                    ("\n" :: chunksAfterRun)

            MultilineCommentRunEnd_NewlineCRLF ->
                tokenizeMultilineComment
                    source
                    (runEndOffset + 2)
                    (row + 1)
                    1
                    startRow
                    startColumn
                    tokensRev
                    depth
                    ("\n" :: chunksAfterRun)

            MultilineCommentRunEnd_NewlineCR ->
                tokenizeMultilineComment
                    source
                    (runEndOffset + 1)
                    (row + 1)
                    1
                    startRow
                    startColumn
                    tokensRev
                    depth
                    ("\n" :: chunksAfterRun)

            MultilineCommentRunEnd_StartComment ->
                tokenizeMultilineComment
                    source
                    (runEndOffset + 2)
                    row
                    (columnAfterRun + 2)
                    startRow
                    startColumn
                    tokensRev
                    (depth + 1)
                    ("{-" :: chunksAfterRun)

            MultilineCommentRunEnd_EndComment ->
                let
                    finalChunksRev =
                        "-}" :: chunksAfterRun

                    endOffset =
                        runEndOffset + 2

                    endColumn =
                        columnAfterRun + 2
                in
                if depth == 1 then
                    tokenizeHelp
                        source
                        endOffset
                        row
                        endColumn
                        (makeToken
                            Token.Comment
                            (concatenateChunksRev finalChunksRev)
                            (makeLocation startRow startColumn)
                            (makeLocation row endColumn)
                            Nothing
                            :: tokensRev
                        )

                else
                    tokenizeMultilineComment source endOffset row endColumn startRow startColumn tokensRev (depth - 1) finalChunksRev

    else
        Err
            ("Internal error: negative offset "
                ++ String.fromInt offset ++ " at " ++ locationString (makeLocation row column) ++ "."
            )


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


minusIsOperator : String -> Int -> Int -> Int -> List Token.Token -> Bool
minusIsOperator source offset row column tokensRev =
    case String.slice (offset + 1) (offset + 2) source of
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
                || previousAdjacentTokenCanEndExpression row column tokensRev


previousAdjacentTokenCanEndExpression : Int -> Int -> List Token.Token -> Bool
previousAdjacentTokenCanEndExpression row column tokensRev =
    case tokensRev of
        token :: _ ->
            if token.end.row == row && token.end.column == column then
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

            else
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


numberEnd : String -> String -> Int -> Int
numberEnd source first startOffset =
    if first == "0" && String.slice (startOffset + 1) (startOffset + 2) source == "x" then
        skipToAsciiHexDigitEnd source (startOffset + 2)

    else
        let
            afterInteger =
                skipToAsciiDecimalDigitEnd source (startOffset + 1)

            afterFraction =
                case String.slice afterInteger (afterInteger + 1) source of
                    "." ->
                        if isDigit (String.slice (afterInteger + 1) (afterInteger + 2) source) then
                            skipToAsciiDecimalDigitEnd source (afterInteger + 2)

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
