module ElmSyntax.Concrete.Parser.TokensFromString exposing (..)

import Char
import ElmSyntax.Concrete.Parser.StringParsing
    exposing
        ( LiteralRunBoundary(..)
        , LiteralTermination(..)
        , MultilineCommentRunEnd(..)
        , concatenateChunksRev
        , findLiteralRunEnd
        , isDigit
        , isIdentifierStart
        , isOperatorChar
        , isWhitespace
        , lineCommentEnd
        , literalTerminationLength
        , locationString
        , multilineCommentRunEnd
        , numberEnd
        , prependNonEmptyChunk
        , scanUnicodeEscapeDigits
        , skipInlineWhitespace
        , skipOperatorChars
        , skipToIdentifierEnd
        )
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
                    addToken source offset row column tokensRev Token.Arrow "->" 2

                _ ->
                    if minusIsOperator source offset row column tokensRev then
                        addToken source offset row column tokensRev Token.Operator first 1

                    else
                        addToken source offset row column tokensRev Token.Negation first 1

        "{" ->
            case String.slice (offset + 1) (offset + 2) source of
                "-" ->
                    tokenizeMultilineComment source (offset + 2) row (column + 2) row column tokensRev 1 [ "{-" ]

                _ ->
                    addToken source offset row column tokensRev Token.OpenBrace first 1

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
            addToken source offset row column tokensRev Token.Lambda first 1

        "(" ->
            addToken source offset row column tokensRev Token.OpenParen first 1

        ")" ->
            addToken source offset row column tokensRev Token.CloseParen first 1

        "}" ->
            addToken source offset row column tokensRev Token.CloseBrace first 1

        "[" ->
            addToken source offset row column tokensRev Token.OpenBracket first 1

        "]" ->
            addToken source offset row column tokensRev Token.CloseBracket first 1

        "," ->
            addToken source offset row column tokensRev Token.Comma first 1

        "." ->
            case String.slice (offset + 1) (offset + 2) source of
                "." ->
                    addToken source offset row column tokensRev Token.DotDot ".." 2

                next ->
                    if isOperatorChar next then
                        addToken source offset row column tokensRev Token.Operator (String.slice offset (offset + 2) source) 2

                    else
                        addToken source offset row column tokensRev Token.Dot first 1

        "=" ->
            if isOperatorChar (String.slice (offset + 1) (offset + 2) source) then
                addToken source offset row column tokensRev Token.Operator (String.slice offset (offset + 2) source) 2

            else
                addToken source offset row column tokensRev Token.Equal first 1

        "|" ->
            if isOperatorChar (String.slice (offset + 1) (offset + 2) source) then
                addToken source offset row column tokensRev Token.Operator (String.slice offset (offset + 2) source) 2

            else
                addToken source offset row column tokensRev Token.Pipe first 1

        ":" ->
            if isOperatorChar (String.slice (offset + 1) (offset + 2) source) then
                addToken source offset row column tokensRev Token.Operator (String.slice offset (offset + 2) source) 2

            else
                addToken source offset row column tokensRev Token.Colon first 1

        symbol ->
            if isOperatorChar symbol then
                let
                    endOffset =
                        skipOperatorChars source (offset + 1) (offset + 3)

                    lexeme =
                        String.slice offset endOffset source
                in
                addToken source offset row column tokensRev Token.Operator lexeme (endOffset - offset)

            else
                addToken source offset row column tokensRev Token.Unknown first 1


addToken :
    String
    -> Int
    -> Int
    -> Int
    -> List Token.Token
    -> Token.TokenType
    -> String
    -> Int
    -> Result String (List Token.Token)
addToken source offset row column tokensRev tokenType lexeme consumedLength =
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
                    (Just consumed.raw)
                    :: tokensRev
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
