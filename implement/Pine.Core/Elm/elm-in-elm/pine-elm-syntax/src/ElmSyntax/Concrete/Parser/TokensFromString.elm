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


tokenize : String -> Result String (List Token.Token)
tokenize input =
    input
        |> String.replace "\u{000D}\n" "\n"
        |> String.replace "\u{000D}" "\n"
        |> String.toList
        |> (\chars -> tokenizeHelp chars { row = 1, column = 1 } [])


tokenizeHelp : List Char -> Range.Location -> List Token.Token -> Result String (List Token.Token)
tokenizeHelp remaining location tokensRev =
    case remaining of
        [] ->
            Ok (List.reverse tokensRev)

        '\n' :: rest ->
            let
                nextLocation =
                    { row = location.row + 1, column = 1 }
            in
            tokenizeHelp
                rest
                nextLocation
                (makeToken Token.Newline "\n" location nextLocation Nothing :: tokensRev)

        first :: _ ->
            if isWhitespace first then
                let
                    ( whitespace, rest ) =
                        takeWhile isWhitespace remaining

                    end =
                        advanceColumns (List.length whitespace) location
                in
                tokenizeHelp
                    rest
                    end
                    (makeToken Token.Whitespace (String.fromList whitespace) location end Nothing :: tokensRev)

            else if Char.isDigit first then
                let
                    ( literal, rest ) =
                        takeNumber remaining

                    end =
                        advanceColumns (List.length literal) location
                in
                tokenizeHelp rest
                    end
                    (makeToken Token.NumberLiteral (String.fromList literal) location end Nothing :: tokensRev)

            else if isIdentifierStart first then
                let
                    ( identifier, rest ) =
                        takeWhile isIdentifierChar remaining

                    end =
                        advanceColumns (List.length identifier) location
                in
                tokenizeHelp rest
                    end
                    (makeToken Token.Identifier (String.fromList identifier) location end Nothing :: tokensRev)

            else
                tokenizeSymbol remaining location tokensRev


tokenizeSymbol : List Char -> Range.Location -> List Token.Token -> Result String (List Token.Token)
tokenizeSymbol remaining location tokensRev =
    let
        addToken tokenType lexeme consumed rest =
            let
                end =
                    advanceColumns consumed location
            in
            tokenizeHelp rest end (makeToken tokenType lexeme location end Nothing :: tokensRev)

        addSingle tokenType rest =
            addToken tokenType (String.left 1 (String.fromList remaining)) 1 rest
    in
    case remaining of
        '-' :: '-' :: rest ->
            let
                ( commentTail, afterComment ) =
                    takeWhile (\char -> char /= '\n') rest

                lexeme =
                    "--" ++ String.fromList commentTail

                end =
                    advanceColumns (String.length lexeme) location
            in
            tokenizeHelp afterComment end (makeToken Token.Comment lexeme location end Nothing :: tokensRev)

        '{' :: '-' :: rest ->
            tokenizeMultilineComment rest location tokensRev 1 [ '-', '{' ]

        '"' :: '"' :: '"' :: rest ->
            tokenizeLiteral Token.TripleQuotedStringLiteral [ '"', '"', '"' ] rest location tokensRev

        '"' :: rest ->
            tokenizeLiteral Token.StringLiteral [ '"' ] rest location tokensRev

        '\'' :: rest ->
            tokenizeLiteral Token.CharLiteral [ '\'' ] rest location tokensRev

        '-' :: '>' :: rest ->
            addToken Token.Arrow "->" 2 rest

        '\\' :: rest ->
            addSingle Token.Lambda rest

        '(' :: rest ->
            addSingle Token.OpenParen rest

        ')' :: rest ->
            addSingle Token.CloseParen rest

        '{' :: rest ->
            addSingle Token.OpenBrace rest

        '}' :: rest ->
            addSingle Token.CloseBrace rest

        '[' :: rest ->
            addSingle Token.OpenBracket rest

        ']' :: rest ->
            addSingle Token.CloseBracket rest

        ',' :: rest ->
            addSingle Token.Comma rest

        '.' :: '.' :: rest ->
            addToken Token.DotDot ".." 2 rest

        '.' :: next :: rest ->
            if isOperatorChar next then
                addToken Token.Operator (String.fromList [ '.', next ]) 2 rest

            else
                addSingle Token.Dot (next :: rest)

        '.' :: rest ->
            addSingle Token.Dot rest

        '=' :: next :: rest ->
            if isOperatorChar next then
                addToken Token.Operator (String.fromList [ '=', next ]) 2 rest

            else
                addSingle Token.Equal (next :: rest)

        '=' :: rest ->
            addSingle Token.Equal rest

        '|' :: next :: rest ->
            if isOperatorChar next then
                addToken Token.Operator (String.fromList [ '|', next ]) 2 rest

            else
                addSingle Token.Pipe (next :: rest)

        '|' :: rest ->
            addSingle Token.Pipe rest

        ':' :: next :: rest ->
            if isOperatorChar next then
                addToken Token.Operator (String.fromList [ ':', next ]) 2 rest

            else
                addSingle Token.Colon (next :: rest)

        ':' :: rest ->
            addSingle Token.Colon rest

        '-' :: rest ->
            if minusIsOperator rest tokensRev then
                addSingle Token.Operator rest

            else
                addSingle Token.Negation rest

        first :: rest ->
            if isOperatorChar first then
                let
                    ( furtherOperatorChars, afterOperator ) =
                        takeAtMost 2 isOperatorChar rest

                    operatorChars =
                        first :: furtherOperatorChars
                in
                addToken
                    Token.Operator
                    (String.fromList operatorChars)
                    (List.length operatorChars)
                    afterOperator

            else
                addSingle Token.Unknown rest

        [] ->
            Ok (List.reverse tokensRev)


tokenizeLiteral :
    Token.TokenType
    -> List Char
    -> List Char
    -> Range.Location
    -> List Token.Token
    -> Result String (List Token.Token)
tokenizeLiteral tokenType termination afterOpening start tokensRev =
    let
        afterOpeningLocation =
            advanceColumns (List.length termination) start
    in
    consumeLiteral termination afterOpening afterOpeningLocation [] []
        |> Result.andThen
            (\consumed ->
                tokenizeHelp consumed.remaining
                    consumed.end
                    (makeToken tokenType consumed.decoded start consumed.end (Just consumed.raw) :: tokensRev)
            )


type alias ConsumedLiteral =
    { decoded : String
    , raw : String
    , remaining : List Char
    , end : Range.Location
    }


consumeLiteral :
    List Char
    -> List Char
    -> Range.Location
    -> List Char
    -> List Char
    -> Result String ConsumedLiteral
consumeLiteral termination remaining location decodedRev rawRev =
    if List.take (List.length termination) remaining == termination then
        let
            end =
                advanceColumns (List.length termination) location
        in
        Ok
            { decoded = String.fromList (List.reverse decodedRev)
            , raw = String.fromList (List.reverse rawRev)
            , remaining = List.drop (List.length termination) remaining
            , end = end
            }

    else
        case remaining of
            [] ->
                Err ("Unterminated literal at " ++ locationString location ++ ".")

            '\\' :: escaped :: rest ->
                if escaped == 'u' then
                    consumeUnicodeEscape termination rest location decodedRev rawRev

                else
                    let
                        decoded =
                            case escaped of
                                'n' ->
                                    '\n'

                                'r' ->
                                    '\u{000D}'

                                't' ->
                                    '\t'

                                _ ->
                                    escaped

                        nextLocation =
                            advanceColumns 2 location
                    in
                    consumeLiteral termination
                        rest
                        nextLocation
                        (decoded :: decodedRev)
                        (escaped :: '\\' :: rawRev)

            char :: rest ->
                consumeLiteral termination
                    rest
                    (advanceLocation char location)
                    (char :: decodedRev)
                    (char :: rawRev)


consumeUnicodeEscape :
    List Char
    -> List Char
    -> Range.Location
    -> List Char
    -> List Char
    -> Result String ConsumedLiteral
consumeUnicodeEscape termination remaining location decodedRev rawRev =
    case remaining of
        '{' :: afterOpen ->
            let
                ( digits, afterDigits ) =
                    takeWhile isAsciiHexDigit afterOpen
            in
            case afterDigits of
                '}' :: rest ->
                    case hexCharsToInt digits of
                        Just codePoint ->
                            if codePoint <= 0x10FFFF && not (codePoint >= 0xD800 && codePoint <= 0xDFFF) then
                                let
                                    rawEscape =
                                        '}' :: List.reverse digits ++ [ '{', 'u', '\\' ]

                                    nextLocation =
                                        advanceColumns (4 + List.length digits) location
                                in
                                consumeLiteral termination
                                    rest
                                    nextLocation
                                    (Char.fromCode codePoint :: decodedRev)
                                    (rawEscape ++ rawRev)

                            else
                                Err ("Invalid unicode escape at " ++ locationString location ++ ".")

                        Nothing ->
                            Err ("Invalid unicode escape at " ++ locationString location ++ ".")

                _ ->
                    Err ("Invalid unicode escape at " ++ locationString location ++ ".")

        _ ->
            consumeLiteral termination
                remaining
                (advanceColumns 2 location)
                ('u' :: decodedRev)
                ('u' :: '\\' :: rawRev)


tokenizeMultilineComment :
    List Char
    -> Range.Location
    -> List Token.Token
    -> Int
    -> List Char
    -> Result String (List Token.Token)
tokenizeMultilineComment remaining start tokensRev depth commentRev =
    case remaining of
        [] ->
            Err ("Unterminated comment at " ++ locationString start ++ ".")

        '{' :: '-' :: rest ->
            tokenizeMultilineComment rest start tokensRev (depth + 1) ('-' :: '{' :: commentRev)

        '-' :: '}' :: rest ->
            let
                nextCommentRev =
                    '}' :: '-' :: commentRev
            in
            if depth == 1 then
                let
                    lexeme =
                        String.fromList (List.reverse nextCommentRev)

                    end =
                        advanceString lexeme start
                in
                tokenizeHelp rest end (makeToken Token.Comment lexeme start end Nothing :: tokensRev)

            else
                tokenizeMultilineComment rest start tokensRev (depth - 1) nextCommentRev

        char :: rest ->
            tokenizeMultilineComment rest start tokensRev depth (char :: commentRev)


minusIsOperator : List Char -> List Token.Token -> Bool
minusIsOperator afterMinus tokensRev =
    case afterMinus of
        next :: _ ->
            isWhitespace next
                || next
                == '\n'
                || List.member next [ ')', ']', '}' ]
                || previousAdjacentTokenCanEndExpression tokensRev

        [] ->
            True


previousAdjacentTokenCanEndExpression : List Token.Token -> Bool
previousAdjacentTokenCanEndExpression tokensRev =
    case tokensRev of
        token :: _ ->
            List.member token.tokenType
                [ Token.Identifier
                , Token.NumberLiteral
                , Token.StringLiteral
                , Token.TripleQuotedStringLiteral
                , Token.CharLiteral
                , Token.CloseParen
                , Token.CloseBracket
                , Token.CloseBrace
                ]

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


advanceColumns : Int -> Range.Location -> Range.Location
advanceColumns count location =
    { column = location.column + count
    , row = location.row
    }


advanceLocation : Char -> Range.Location -> Range.Location
advanceLocation char location =
    if char == '\n' then
        { row = location.row + 1, column = 1 }

    else
        advanceColumns 1 location


advanceString : String -> Range.Location -> Range.Location
advanceString string location =
    List.foldl advanceLocation location (String.toList string)


takeNumber : List Char -> ( List Char, List Char )
takeNumber chars =
    case chars of
        '0' :: 'x' :: afterPrefix ->
            let
                ( digits, rest ) =
                    takeWhile isAsciiHexDigit afterPrefix
            in
            ( '0' :: 'x' :: digits, rest )

        _ ->
            let
                ( integerDigits, afterInteger ) =
                    takeWhile Char.isDigit chars

                ( fraction, afterFraction ) =
                    case afterInteger of
                        '.' :: ((nextDigit :: _) as afterDot) ->
                            if Char.isDigit nextDigit then
                                let
                                    ( fractionDigits, afterFractionDigits ) =
                                        takeWhile Char.isDigit afterDot
                                in
                                ( '.' :: fractionDigits, afterFractionDigits )

                            else
                                ( [], afterInteger )

                        _ ->
                            ( [], afterInteger )

                ( exponent, afterExponent ) =
                    case afterFraction of
                        ('e' as marker) :: afterMarker ->
                            takeExponent marker afterMarker

                        ('E' as marker) :: afterMarker ->
                            takeExponent marker afterMarker

                        _ ->
                            ( [], afterFraction )
            in
            ( integerDigits ++ fraction ++ exponent, afterExponent )


takeExponent : Char -> List Char -> ( List Char, List Char )
takeExponent marker chars =
    let
        ( sign, afterSign ) =
            case chars of
                ('+' as signChar) :: afterSignChar ->
                    ( [ signChar ], afterSignChar )

                ('-' as signChar) :: afterSignChar ->
                    ( [ signChar ], afterSignChar )

                _ ->
                    ( [], chars )

        ( digits, afterDigits ) =
            takeWhile Char.isDigit afterSign
    in
    ( marker :: sign ++ digits, afterDigits )


takeWhile : (Char -> Bool) -> List Char -> ( List Char, List Char )
takeWhile predicate chars =
    takeWhileHelp predicate chars []


takeWhileHelp : (Char -> Bool) -> List Char -> List Char -> ( List Char, List Char )
takeWhileHelp predicate remaining takenRev =
    case remaining of
        first :: rest ->
            if predicate first then
                takeWhileHelp predicate rest (first :: takenRev)

            else
                ( List.reverse takenRev, remaining )

        [] ->
            ( List.reverse takenRev, [] )


takeAtMost : Int -> (Char -> Bool) -> List Char -> ( List Char, List Char )
takeAtMost count predicate chars =
    if count <= 0 then
        ( [], chars )

    else
        case chars of
            first :: rest ->
                if predicate first then
                    let
                        ( taken, remaining ) =
                            takeAtMost (count - 1) predicate rest
                    in
                    ( first :: taken, remaining )

                else
                    ( [], chars )

            [] ->
                ( [], [] )


hexCharsToInt : List Char -> Maybe Int
hexCharsToInt chars =
    case chars of
        [] ->
            Nothing

        _ ->
            hexCharsToIntHelp 0 chars


hexCharsToIntHelp : Int -> List Char -> Maybe Int
hexCharsToIntHelp accumulated chars =
    case chars of
        [] ->
            Just accumulated

        char :: remaining ->
            case char of
                '0' ->
                    hexCharsToIntHelp (accumulated * 16) remaining

                '1' ->
                    hexCharsToIntHelp (accumulated * 16 + 1) remaining

                '2' ->
                    hexCharsToIntHelp (accumulated * 16 + 2) remaining

                '3' ->
                    hexCharsToIntHelp (accumulated * 16 + 3) remaining

                '4' ->
                    hexCharsToIntHelp (accumulated * 16 + 4) remaining

                '5' ->
                    hexCharsToIntHelp (accumulated * 16 + 5) remaining

                '6' ->
                    hexCharsToIntHelp (accumulated * 16 + 6) remaining

                '7' ->
                    hexCharsToIntHelp (accumulated * 16 + 7) remaining

                '8' ->
                    hexCharsToIntHelp (accumulated * 16 + 8) remaining

                '9' ->
                    hexCharsToIntHelp (accumulated * 16 + 9) remaining

                'a' ->
                    hexCharsToIntHelp (accumulated * 16 + 10) remaining

                'A' ->
                    hexCharsToIntHelp (accumulated * 16 + 10) remaining

                'b' ->
                    hexCharsToIntHelp (accumulated * 16 + 11) remaining

                'B' ->
                    hexCharsToIntHelp (accumulated * 16 + 11) remaining

                'c' ->
                    hexCharsToIntHelp (accumulated * 16 + 12) remaining

                'C' ->
                    hexCharsToIntHelp (accumulated * 16 + 12) remaining

                'd' ->
                    hexCharsToIntHelp (accumulated * 16 + 13) remaining

                'D' ->
                    hexCharsToIntHelp (accumulated * 16 + 13) remaining

                'e' ->
                    hexCharsToIntHelp (accumulated * 16 + 14) remaining

                'E' ->
                    hexCharsToIntHelp (accumulated * 16 + 14) remaining

                'f' ->
                    hexCharsToIntHelp (accumulated * 16 + 15) remaining

                'F' ->
                    hexCharsToIntHelp (accumulated * 16 + 15) remaining

                _ ->
                    Nothing


isAsciiHexDigit : Char -> Bool
isAsciiHexDigit char =
    case char of
        '0' ->
            True

        '1' ->
            True

        '2' ->
            True

        '3' ->
            True

        '4' ->
            True

        '5' ->
            True

        '6' ->
            True

        '7' ->
            True

        '8' ->
            True

        '9' ->
            True

        'a' ->
            True

        'A' ->
            True

        'b' ->
            True

        'B' ->
            True

        'c' ->
            True

        'C' ->
            True

        'd' ->
            True

        'D' ->
            True

        'e' ->
            True

        'E' ->
            True

        'f' ->
            True

        'F' ->
            True

        _ ->
            False


isWhitespace : Char -> Bool
isWhitespace char =
    case char of
        ' ' ->
            True

        '\t' ->
            True

        _ ->
            False


isIdentifierStart : Char -> Bool
isIdentifierStart char =
    Char.isAlpha char || char == '_'


isIdentifierChar : Char -> Bool
isIdentifierChar char =
    Char.isAlphaNum char || char == '_' || char == '\''


isOperatorChar : Char -> Bool
isOperatorChar char =
    {-
    TODO: Expand code analysis to optimize form using `List.member` to get the same level of efficiency:
    List.member char [ '+', '-', '/', '*', '=', '.', '$', '<', '>', ':', '&', '|', '^', '?', '%', '#', '!' ]
    -}
    case char of
        '+' ->
            True

        '-' ->
            True

        '/' ->
            True

        '*' ->
            True

        '=' ->
            True

        '.' ->
            True

        '$' ->
            True

        '<' ->
            True

        '>' ->
            True

        ':' ->
            True

        '&' ->
            True

        '|' ->
            True

        '^' ->
            True

        '?' ->
            True

        '%' ->
            True

        '#' ->
            True

        '!' ->
            True

        _ ->
            False


locationString : Range.Location -> String
locationString location =
    String.fromInt location.row ++ ":" ++ String.fromInt location.column
