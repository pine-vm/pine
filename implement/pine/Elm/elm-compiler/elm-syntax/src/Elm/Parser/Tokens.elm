module Elm.Parser.Tokens exposing
    ( inToken
    , equal, parensEnd
    , isAllowedOperatorToken, isOperatorSymbolChar
    , characterLiteralMapWithRange, singleOrTripleQuotedStringLiteralMapWithRange
    , functionName, functionNameNode, functionNameMapWithRange, functionNameNotInfixNode, typeName, typeNameNode, typeNameMapWithRange
    )

{-|

@docs inToken

@docs equal, parensEnd
@docs isAllowedOperatorToken, isOperatorSymbolChar

@docs characterLiteralMapWithRange, singleOrTripleQuotedStringLiteralMapWithRange
@docs functionName, functionNameNode, functionNameMapWithRange, functionNameNotInfixNode, typeName, typeNameNode, typeNameMapWithRange

-}

import Char
import Elm.Syntax.Node exposing (Node(..))
import Elm.Syntax.Range exposing (Range)
import ParserFast


isNotReserved : String -> Bool
isNotReserved name =
    case name of
        "module" ->
            False

        "exposing" ->
            False

        "import" ->
            False

        "as" ->
            False

        "if" ->
            False

        "then" ->
            False

        "else" ->
            False

        "let" ->
            False

        "in" ->
            False

        "case" ->
            False

        "of" ->
            False

        "port" ->
            False

        --"infixr"
        --"infixl"
        "type" ->
            False

        -- "infix" Apparently this is not a reserved keyword
        -- "alias" Apparently this is not a reserved keyword
        "where" ->
            False

        _ ->
            True


inToken : ParserFast.Parser ()
inToken =
    ParserFast.keyword "in" ()


escapedCharValueMap : (Char -> res) -> ParserFast.Parser res
escapedCharValueMap charToRes =
    ParserFast.oneOf7
        (ParserFast.symbol "'" (charToRes '\''))
        (ParserFast.symbol "\"" (charToRes '"'))
        (ParserFast.symbol "n" (charToRes '\n'))
        (ParserFast.symbol "t" (charToRes '\t'))
        -- Eventhough Elm-format will change \r to a unicode version. When you dont use elm-format, this will not happen.
        (ParserFast.symbol "r" (charToRes '\u{000D}'))
        (ParserFast.symbol "\\" (charToRes '\\'))
        (ParserFast.symbolFollowedBy "u{"
            (ParserFast.ifFollowedByWhileMapWithoutLinebreak
                (\hex ->
                    charToRes (Char.fromCode (hexStringToInt hex))
                )
                Char.isHexDigit
                Char.isHexDigit
                |> ParserFast.followedBySymbol "}"
            )
        )


hexStringToInt : String -> Int
hexStringToInt string =
    String.foldr
        (\c soFar ->
            { exponent = soFar.exponent + 1
            , result = soFar.result + 16 ^ soFar.exponent * charToHex c
            }
        )
        { exponent = 0, result = 0 }
        string
        |> .result


charToHex : Char -> Int
charToHex c =
    case c of
        '0' ->
            0

        '1' ->
            1

        '2' ->
            2

        '3' ->
            3

        '4' ->
            4

        '5' ->
            5

        '6' ->
            6

        '7' ->
            7

        '8' ->
            8

        '9' ->
            9

        'a' ->
            10

        'b' ->
            11

        'c' ->
            12

        'd' ->
            13

        'e' ->
            14

        'f' ->
            15

        'A' ->
            10

        'B' ->
            11

        'C' ->
            12

        'D' ->
            13

        'E' ->
            14

        -- 'F'
        _ ->
            15


characterLiteralMapWithRange : (Range -> Char -> res) -> ParserFast.Parser res
characterLiteralMapWithRange rangeAndCharToRes =
    ParserFast.symbolFollowedBy "'"
        (ParserFast.oneOf2MapWithStartRowColumnAndEndRowColumn
            (\startRow startColumn char endRow endColumn ->
                rangeAndCharToRes
                    { start = { row = startRow, column = startColumn - 1 }
                    , end = { row = endRow, column = endColumn + 1 }
                    }
                    char
            )
            (ParserFast.symbolFollowedBy "\\" (escapedCharValueMap identity))
            (\startRow startColumn char endRow endColumn ->
                rangeAndCharToRes
                    { start = { row = startRow, column = startColumn - 1 }
                    , end = { row = endRow, column = endColumn + 1 }
                    }
                    char
            )
            ParserFast.anyChar
            |> ParserFast.followedBySymbol "'"
        )


singleOrTripleQuotedStringLiteralMapWithRange : (Range -> String -> res) -> ParserFast.Parser res
singleOrTripleQuotedStringLiteralMapWithRange rangeAndStringToRes =
    ParserFast.symbolFollowedBy "\""
        (ParserFast.oneOf2MapWithStartRowColumnAndEndRowColumn
            (\startRow startColumn string endRow endColumn ->
                rangeAndStringToRes
                    { start = { row = startRow, column = startColumn - 1 }
                    , end = { row = endRow, column = endColumn }
                    }
                    string
            )
            (ParserFast.symbolFollowedBy "\"\""
                tripleQuotedStringLiteralOfterTripleDoubleQuote
            )
            (\startRow startColumn string endRow endColumn ->
                rangeAndStringToRes
                    { start = { row = startRow, column = startColumn - 1 }
                    , end = { row = endRow, column = endColumn }
                    }
                    string
            )
            singleQuotedStringLiteralAfterDoubleQuote
        )


singleQuotedStringLiteralAfterDoubleQuote : ParserFast.Parser String
singleQuotedStringLiteralAfterDoubleQuote =
    ParserFast.loopUntil (ParserFast.symbol "\"" ())
        (ParserFast.oneOf2
            (ParserFast.symbolFollowedBy "\\" (escapedCharValueMap String.fromChar))
            (ParserFast.whileWithoutLinebreak (\c -> c /= '"' && c /= '\\'))
        )
        ""
        (\extension soFar ->
            soFar ++ extension ++ ""
        )
        identity


tripleQuotedStringLiteralOfterTripleDoubleQuote : ParserFast.Parser String
tripleQuotedStringLiteralOfterTripleDoubleQuote =
    ParserFast.loopUntil (ParserFast.symbol "\"\"\"" ())
        (ParserFast.oneOf3
            (ParserFast.symbol "\"" "\"")
            (ParserFast.symbolFollowedBy "\\" (escapedCharValueMap String.fromChar))
            (ParserFast.while (\c -> c /= '"' && c /= '\\'))
        )
        ""
        (\extension soFar ->
            soFar ++ extension ++ ""
        )
        identity


functionName : ParserFast.Parser String
functionName =
    ParserFast.ifFollowedByWhileValidateWithoutLinebreak
        Char.isLower
        isAlphaNumOrUnderscore
        isNotReserved


functionNameNode : ParserFast.Parser (Node String)
functionNameNode =
    ParserFast.ifFollowedByWhileValidateMapWithRangeWithoutLinebreak Node
        Char.isLower
        isAlphaNumOrUnderscore
        isNotReserved


functionNameMapWithRange : (Range -> String -> res) -> ParserFast.Parser res
functionNameMapWithRange rangeAndNameToResult =
    ParserFast.ifFollowedByWhileValidateMapWithRangeWithoutLinebreak
        rangeAndNameToResult
        Char.isLower
        isAlphaNumOrUnderscore
        isNotReserved


functionNameNotInfixNode : ParserFast.Parser (Node String)
functionNameNotInfixNode =
    ParserFast.ifFollowedByWhileValidateMapWithRangeWithoutLinebreak Node
        Char.isLower
        isAlphaNumOrUnderscore
        (\name -> name /= "infix" && isNotReserved name)


typeName : ParserFast.Parser String
typeName =
    ParserFast.ifFollowedByWhileWithoutLinebreak
        Char.isUpper
        isAlphaNumOrUnderscore


typeNameMapWithRange : (Range -> String -> res) -> ParserFast.Parser res
typeNameMapWithRange rangeAndNameToRes =
    ParserFast.ifFollowedByWhileMapWithRangeWithoutLinebreak rangeAndNameToRes
        Char.isUpper
        isAlphaNumOrUnderscore


typeNameNode : ParserFast.Parser (Node String)
typeNameNode =
    ParserFast.ifFollowedByWhileMapWithRangeWithoutLinebreak Node
        Char.isUpper
        isAlphaNumOrUnderscore


isAlphaNumOrUnderscore : Char -> Bool
isAlphaNumOrUnderscore c =
    Char.isAlphaNum c || c == '_'


isAllowedOperatorToken : String -> Bool
isAllowedOperatorToken operatorCandidateToValidate =
    case operatorCandidateToValidate of
        "==" ->
            True

        "/=" ->
            True

        "::" ->
            True

        "++" ->
            True

        "+" ->
            True

        "*" ->
            True

        "<|" ->
            True

        "|>" ->
            True

        "||" ->
            True

        "<=" ->
            True

        ">=" ->
            True

        "|=" ->
            True

        "|." ->
            True

        "//" ->
            True

        "</>" ->
            True

        "<?>" ->
            True

        "^" ->
            True

        "<<" ->
            True

        ">>" ->
            True

        "<" ->
            True

        ">" ->
            True

        "/" ->
            True

        "&&" ->
            True

        "-" ->
            True

        _ ->
            False


isOperatorSymbolChar : Char -> Bool
isOperatorSymbolChar c =
    case c of
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

        _ ->
            False


equal : ParserFast.Parser ()
equal =
    ParserFast.symbol "=" ()


parensEnd : ParserFast.Parser ()
parensEnd =
    ParserFast.symbol ")" ()
