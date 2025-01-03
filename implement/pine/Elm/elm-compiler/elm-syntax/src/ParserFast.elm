module ParserFast exposing
    ( Parser, run
    , symbol, symbolWithEndLocation, symbolWithRange, symbolFollowedBy, symbolBacktrackableFollowedBy, followedBySymbol
    , keyword, keywordFollowedBy
    , anyChar, while, whileWithoutLinebreak, whileMapWithRange, ifFollowedByWhileWithoutLinebreak, ifFollowedByWhileMapWithoutLinebreak, ifFollowedByWhileMapWithRangeWithoutLinebreak, ifFollowedByWhileValidateWithoutLinebreak, ifFollowedByWhileValidateMapWithRangeWithoutLinebreak, whileWithoutLinebreakAnd2PartUtf16ToResultAndThen, whileWithoutLinebreakAnd2PartUtf16ValidateMapWithRangeBacktrackableFollowedBySymbol
    , integerDecimalMapWithRange, integerDecimalOrHexadecimalMapWithRange, floatOrIntegerDecimalOrHexadecimalMapWithRange
    , skipWhileWhitespaceFollowedBy, followedBySkipWhileWhitespace, nestableMultiCommentMapWithRange
    , map, validate, lazy
    , map2, map2WithStartLocation, map2WithRange, map3, map3WithStartLocation, map3WithRange, map4, map4WithRange, map5, map5WithStartLocation, map5WithRange, map6, map6WithStartLocation, map6WithRange, map7WithRange, map8WithStartLocation, map9WithRange
    , loopWhileSucceeds, loopWhileSucceedsOntoResultFromParser, loopUntil
    , orSucceed, map2OrSucceed, map2WithRangeOrSucceed, map3OrSucceed, map4OrSucceed, oneOf2, oneOf2Map, oneOf2MapWithStartRowColumnAndEndRowColumn, oneOf2OrSucceed, oneOf3, oneOf4, oneOf5, oneOf7, oneOf9
    , withIndentSetToColumn, withIndentSetToColumnMinus, columnIndentAndThen, validateEndColumnIndentation
    , mapWithRange, columnAndThen, offsetSourceAndThen, offsetSourceAndThenOrSucceed
    , problem
    )

{-|

@docs Parser, run


### a note about backtracking and committing

By default, even the smallest parsers itself commit, even if no characters end up being consumed.
This will behave exactly how you'd expect pretty much always,
e.g. when differentiating between `let`-expression, records and references, since a simple symbol is enough to tell them apart.

You might however run into unintuitive behavior with e.g. whitespace (which commits even after seeing 0 blank characters):

    ParserFast.symbolFollowedBy "("
        (ParserFast.oneOf2
            (ParserFast.skipWhileWhitespaceFollowedBy
                parenthesizedAfterOpeningParens
            )
            (ParserFast.symbol ")" Unit)
        )

With `elm/parser`, the above will work perfectly fine since
the whitespace parser can succeed but still
track back to the unit case in case `parenthesizedAfterOpeningParens`
fails (without committing).

With `ParserFast`, you need to either

  - check for `)` first. This should be preferred in >90% of cases

  - _add_ a `skipWhileWhitespaceBacktracksIfEmptyFollowedBy` in this module with something like

        if s1.offset > s0.offset then
            parseNext s1 |> pStepCommit

        else
            parseNext s1

  - if applicable/feasible in that situation,
    build a whitespace parser that always ignores at least one character

(this note does not just apply to whitespace!)


# Exact match primitives

@docs symbol, symbolWithEndLocation, symbolWithRange, symbolFollowedBy, symbolBacktrackableFollowedBy, followedBySymbol
@docs keyword, keywordFollowedBy


# Fuzzy match primitives

@docs anyChar, while, whileWithoutLinebreak, whileMapWithRange, ifFollowedByWhileWithoutLinebreak, ifFollowedByWhileMapWithoutLinebreak, ifFollowedByWhileMapWithRangeWithoutLinebreak, ifFollowedByWhileValidateWithoutLinebreak, ifFollowedByWhileValidateMapWithRangeWithoutLinebreak, whileWithoutLinebreakAnd2PartUtf16ToResultAndThen, whileWithoutLinebreakAnd2PartUtf16ValidateMapWithRangeBacktrackableFollowedBySymbol
@docs integerDecimalMapWithRange, integerDecimalOrHexadecimalMapWithRange, floatOrIntegerDecimalOrHexadecimalMapWithRange


# Whitespace primitives

@docs skipWhileWhitespaceFollowedBy, followedBySkipWhileWhitespace, nestableMultiCommentMapWithRange


# Flow

@docs map, validate, lazy


## sequence

@docs map2, map2WithStartLocation, map2WithRange, map3, map3WithStartLocation, map3WithRange, map4, map4WithRange, map5, map5WithStartLocation, map5WithRange, map6, map6WithStartLocation, map6WithRange, map7WithRange, map8WithStartLocation, map9WithRange

@docs loopWhileSucceeds, loopWhileSucceedsOntoResultFromParser, loopUntil


## choice

Parsing JSON for example, the values can be strings, floats, booleans,
arrays, objects, or null. You need a way to pick one of them! Here is a
sample of what that code might look like:

    type Json
        = Number Float
        | Boolean Bool
        | Null

    json : Parser Json
    json =
        oneOf4
            (ParserFast.map Number float)
            (ParserFast.keyword "true" (Boolean True))
            (ParserFast.keyword "False" (Boolean False))
            (ParserFast.keyword "null" Null)

This parser will keep trying down the list of parsers until one of them starts committing.
Once a path is chosen, it does not come back and try the others.

@docs orSucceed, map2OrSucceed, map2WithRangeOrSucceed, map3OrSucceed, map4OrSucceed, oneOf2, oneOf2Map, oneOf2MapWithStartRowColumnAndEndRowColumn, oneOf2OrSucceed, oneOf3, oneOf4, oneOf5, oneOf7, oneOf9


# Indentation, Locations and source

@docs withIndentSetToColumn, withIndentSetToColumnMinus, columnIndentAndThen, validateEndColumnIndentation
@docs mapWithRange, columnAndThen, offsetSourceAndThen, offsetSourceAndThenOrSucceed
@docs problem


# Test-only

-}

import Elm.Syntax.Range exposing (Location, Range)
import Parser


type Problem
    = ExpectingNumber Int Int
    | ExpectingSymbol Int Int String
    | ExpectingAnyChar Int Int
    | ExpectingKeyword Int Int String
    | ExpectingCharSatisfyingPredicate Int Int
    | ExpectingStringSatisfyingPredicate Int Int
    | ExpectingCustom Int Int String
    | ExpectingOneOf Problem Problem (List Problem)


{-| A `Parser` helps turn a `String` into nicely structured data. For example,
we can [`run`](#run) an int parser to turn `String` to `Int`:

    run int "123456" == Ok 123456
    run int "3.1415" == Err ...

    int : Parser Int
    int =
        ParserFast.integerDecimalMapWithRange (\_ n -> n)

The cool thing is that you can combine `Parser` values to handle much more
complex scenarios.

-}
type Parser a
    = Parser (State -> PStep a)


type PStep value
    = Good value State
    | Bad Bool Problem


type State
    = PState (List Char) Int Int Int Int


{-| Try a parser. Here are some examples using the [`keyword`](#keyword)
parser:

    run (keyword "true" ()) "true" --> Ok ()

    run (keyword "true" ()) "True" --> Err ...

    run (keyword "true" ()) "false" --> Err ...

    run (keyword "true" ()) "true!" --> Err ...

Notice the last case!
It's guaranteed you have reached the end of the string you are parsing.
Parsers can't succeed without parsing the whole string.

Currently reuses the `elm/parser` `DeadEnd` type to report problems
to avoid breaking changes

-}
run : Parser a -> String -> Result (List Parser.DeadEnd) a
run (Parser parse) src =
    let
        srcChars : List Char
        srcChars =
            String.toList src
    in
    case parse (PState srcChars 0 1 1 1) of
        Good value (PState finalSrc finalOffset _ finalRow finalCol) ->
            if finalOffset - List.length finalSrc == 0 then
                Ok value

            else
                Err [ { row = finalRow, col = finalCol, problem = Parser.ExpectingEnd } ]

        Bad _ deadEnds ->
            Err (ropeFilledToList deadEnds [])


ropeFilledToList : Problem -> List Parser.DeadEnd -> List Parser.DeadEnd
ropeFilledToList problemToConvert soFar =
    case problemToConvert of
        ExpectingOneOf firstTry secondTry thirdTryUp ->
            List.foldr ropeFilledToList soFar thirdTryUp
                |> ropeFilledToList secondTry
                |> ropeFilledToList firstTry

        ExpectingNumber row col ->
            { row = row, col = col, problem = Parser.ExpectingNumber } :: soFar

        ExpectingSymbol row col symbolString ->
            { row = row, col = col, problem = Parser.ExpectingSymbol symbolString } :: soFar

        ExpectingAnyChar row col ->
            { row = row, col = col, problem = Parser.Problem "expecting any char" } :: soFar

        ExpectingKeyword row col keywordString ->
            { row = row, col = col, problem = Parser.ExpectingKeyword keywordString } :: soFar

        ExpectingCharSatisfyingPredicate row col ->
            { row = row, col = col, problem = Parser.UnexpectedChar } :: soFar

        ExpectingStringSatisfyingPredicate row col ->
            { row = row, col = col, problem = Parser.Problem "expected string to pass validation" } :: soFar

        ExpectingCustom row col customMessage ->
            { row = row, col = col, problem = Parser.Problem customMessage } :: soFar


{-| Helper to delay computation,
mostly to refer to a recursive parser.

Say we want a parser for simple boolean expressions:

    true

    false

    true || false

    true || (true || false)

Notice that a boolean expression might contain _other_ boolean expressions.
That means we will want to define our parser in terms of itself:

    type Boolean
        = MyTrue
        | MyFalse
        | MyOr Boolean Boolean

    boolean : Parser Boolean
    boolean =
        ParserFast.oneOf3
            (ParserFast.keyword "true" MyTrue)
            (ParserFast.keyword "false" MyFalse)
            (ParserFast.symbolFollowedBy "("
                (ParserFast.map2 MyOr
                    (ParserFast.skipWhileWhitespaceFollowedBy
                        (ParserFast.lazy (\_ -> boolean))
                        |> ParserFast.followedBySkipWhileWhitespace
                        |> ParserFast.followedBySymbol "||"
                        |> ParserFast.followedBySkipWhileWhitespace
                    )
                    (ParserFast.lazy (\_ -> boolean)
                        |> ParserFast.followedBySkipWhileWhitespace
                    )
                )
                |> ParserFast.followedBySymbol ")"
            )

**Notice that `boolean` uses `boolean` in its definition!** In Elm, you can
only define a value in terms of itself it is behind a function call. So
`lazy` helps us define these self-referential parsers.

If the recursion is linear, first consider the loop- helpers to avoid stack overflows

-}
lazy : (() -> Parser a) -> Parser a
lazy thunk =
    Parser
        (\s ->
            let
                (Parser parse) =
                    thunk ()
            in
            parse s
        )


validate : (a -> Bool) -> String -> Parser a -> Parser a
validate isOkay problemOnNotOkay (Parser parseA) =
    Parser
        (\s0 ->
            case parseA s0 of
                Bad committed x ->
                    Bad committed x

                (Good a (PState _ _ _ s1Row s1Col)) as good ->
                    if isOkay a then
                        good

                    else
                        Bad True (ExpectingCustom s1Row s1Col problemOnNotOkay)
        )


columnAndThen : (Int -> Parser a) -> Parser a
columnAndThen callback =
    Parser
        (\s ->
            let
                (PState _ _ _ _ col) =
                    s

                (Parser parse) =
                    callback col
            in
            parse s
        )


{-| Can be used to verify the current indentation like this:

    checkIndent : Parser ()
    checkIndent =
        columnIndentAndThen (\indent column -> column > indent)
            "expecting more spaces"

So the `checkIndent` parser only succeeds when you are "deeper" than the
current indent level. You could use this to parse elm-style `let` expressions.

-}
columnIndentAndThen : (Int -> Int -> Parser b) -> Parser b
columnIndentAndThen callback =
    Parser
        (\s ->
            let
                (PState _ _ indent _ col) =
                    s

                (Parser parse) =
                    callback col indent
            in
            parse s
        )


validateEndColumnIndentation : (Int -> Int -> Bool) -> String -> Parser a -> Parser a
validateEndColumnIndentation isOkay problemOnIsNotOkay (Parser parse) =
    Parser
        (\s0 ->
            case parse s0 of
                (Good _ s1) as good ->
                    let
                        (PState _ _ indent row col) =
                            s1
                    in
                    if isOkay col indent then
                        good

                    else
                        Bad True (ExpectingCustom row col problemOnIsNotOkay)

                bad ->
                    bad
        )


{-| Editors think of code as a grid, but behind the scenes it is just a flat
array of UTF-16 characters. `getOffset` tells you your index in that flat
array. So if you consume `"\n\n\n\n"` you are on row 5, column 1, and offset 4.

**Note:** JavaScript uses a somewhat odd version of UTF-16 strings, so a single
character may take two slots. So in JavaScript, `'abc'.length === 3` but
`'🙈🙉🙊'.length === 6`. Try it out! And since Elm runs in JavaScript, the offset
moves by those rules.

-}
offsetSourceAndThen : (Int -> List Char -> Parser a) -> Parser a
offsetSourceAndThen callback =
    Parser
        (\s ->
            let
                (PState src offset _ _ _) =
                    s

                (Parser parse) =
                    callback offset src
            in
            parse s
        )


offsetSourceAndThenOrSucceed : (Int -> List Char -> Maybe (Parser a)) -> a -> Parser a
offsetSourceAndThenOrSucceed callback fallback =
    Parser
        (\s ->
            let
                (PState src offset _ _ _) =
                    s
            in
            case callback offset src of
                Nothing ->
                    Good fallback s

                Just (Parser parse) ->
                    parse s
        )


{-| Parse 2 parser in sequence and combine their results
-}
map2 : (a -> b -> value) -> Parser a -> Parser b -> Parser value
map2 func (Parser parseA) (Parser parseB) =
    Parser
        (\s0 ->
            case parseA s0 of
                Bad committed x ->
                    Bad committed x

                Good a s1 ->
                    case parseB s1 of
                        Bad _ x ->
                            Bad True x

                        Good b s2 ->
                            Good (func a b) s2
        )


map2WithStartLocation : (Location -> a -> b -> value) -> Parser a -> Parser b -> Parser value
map2WithStartLocation func (Parser parseA) (Parser parseB) =
    Parser
        (\s0 ->
            let
                (PState _ _ _ s0Row s0Col) =
                    s0
            in
            case parseA s0 of
                Bad committed x ->
                    Bad committed x

                Good a s1 ->
                    case parseB s1 of
                        Bad _ x ->
                            Bad True x

                        Good b s2 ->
                            Good (func { row = s0Row, column = s0Col } a b) s2
        )


map2WithRange : (Range -> a -> b -> value) -> Parser a -> Parser b -> Parser value
map2WithRange func (Parser parseA) (Parser parseB) =
    Parser
        (\s0 ->
            case parseA s0 of
                Bad committed x ->
                    Bad committed x

                Good a s1 ->
                    case parseB s1 of
                        Bad _ x ->
                            Bad True x

                        Good b s2 ->
                            let
                                (PState _ _ _ s2Row s2Col) =
                                    s2

                                (PState _ _ _ s0Row s0Col) =
                                    s0
                            in
                            Good
                                (func
                                    { start = { row = s0Row, column = s0Col }
                                    , end = { row = s2Row, column = s2Col }
                                    }
                                    a
                                    b
                                )
                                s2
        )


map3 : (a -> b -> c -> value) -> Parser a -> Parser b -> Parser c -> Parser value
map3 func (Parser parseA) (Parser parseB) (Parser parseC) =
    Parser
        (\s0 ->
            case parseA s0 of
                Bad committed x ->
                    Bad committed x

                Good a s1 ->
                    case parseB s1 of
                        Bad _ x ->
                            Bad True x

                        Good b s2 ->
                            case parseC s2 of
                                Bad _ x ->
                                    Bad True x

                                Good c s3 ->
                                    Good (func a b c) s3
        )


map3WithRange : (Range -> a -> b -> c -> value) -> Parser a -> Parser b -> Parser c -> Parser value
map3WithRange func (Parser parseA) (Parser parseB) (Parser parseC) =
    Parser
        (\s0 ->
            case parseA s0 of
                Bad committed x ->
                    Bad committed x

                Good a s1 ->
                    case parseB s1 of
                        Bad _ x ->
                            Bad True x

                        Good b s2 ->
                            case parseC s2 of
                                Bad _ x ->
                                    Bad True x

                                Good c s3 ->
                                    let
                                        (PState _ _ _ s3Row s3Col) =
                                            s3

                                        (PState _ _ _ s0Row s0Col) =
                                            s0
                                    in
                                    Good
                                        (func
                                            { start = { row = s0Row, column = s0Col }
                                            , end = { row = s3Row, column = s3Col }
                                            }
                                            a
                                            b
                                            c
                                        )
                                        s3
        )


map4 : (a -> b -> c -> d -> value) -> Parser a -> Parser b -> Parser c -> Parser d -> Parser value
map4 func (Parser parseA) (Parser parseB) (Parser parseC) (Parser parseD) =
    Parser
        (\s0 ->
            case parseA s0 of
                Bad committed x ->
                    Bad committed x

                Good a s1 ->
                    case parseB s1 of
                        Bad _ x ->
                            Bad True x

                        Good b s2 ->
                            case parseC s2 of
                                Bad _ x ->
                                    Bad True x

                                Good c s3 ->
                                    case parseD s3 of
                                        Bad _ x ->
                                            Bad True x

                                        Good d s4 ->
                                            Good (func a b c d) s4
        )


map4WithRange : (Range -> a -> b -> c -> d -> value) -> Parser a -> Parser b -> Parser c -> Parser d -> Parser value
map4WithRange func (Parser parseA) (Parser parseB) (Parser parseC) (Parser parseD) =
    Parser
        (\s0 ->
            case parseA s0 of
                Bad committed x ->
                    Bad committed x

                Good a s1 ->
                    case parseB s1 of
                        Bad _ x ->
                            Bad True x

                        Good b s2 ->
                            case parseC s2 of
                                Bad _ x ->
                                    Bad True x

                                Good c s3 ->
                                    case parseD s3 of
                                        Bad _ x ->
                                            Bad True x

                                        Good d s4 ->
                                            let
                                                (PState _ _ _ s4Row s4Col) =
                                                    s4

                                                (PState _ _ _ s0Row s0Col) =
                                                    s0
                                            in
                                            Good
                                                (func
                                                    { start = { row = s0Row, column = s0Col }
                                                    , end = { row = s4Row, column = s4Col }
                                                    }
                                                    a
                                                    b
                                                    c
                                                    d
                                                )
                                                s4
        )


map3WithStartLocation : (Location -> a -> b -> c -> value) -> Parser a -> Parser b -> Parser c -> Parser value
map3WithStartLocation func (Parser parseA) (Parser parseB) (Parser parseC) =
    Parser
        (\s0 ->
            case parseA s0 of
                Bad committed x ->
                    Bad committed x

                Good a s1 ->
                    case parseB s1 of
                        Bad _ x ->
                            Bad True x

                        Good b s2 ->
                            case parseC s2 of
                                Bad _ x ->
                                    Bad True x

                                Good c s3 ->
                                    let
                                        (PState _ _ _ s0Row s0Col) =
                                            s0
                                    in
                                    Good (func { row = s0Row, column = s0Col } a b c) s3
        )


map5 : (a -> b -> c -> d -> e -> value) -> Parser a -> Parser b -> Parser c -> Parser d -> Parser e -> Parser value
map5 func (Parser parseA) (Parser parseB) (Parser parseC) (Parser parseD) (Parser parseE) =
    Parser
        (\s0 ->
            case parseA s0 of
                Bad committed x ->
                    Bad committed x

                Good a s1 ->
                    case parseB s1 of
                        Bad _ x ->
                            Bad True x

                        Good b s2 ->
                            case parseC s2 of
                                Bad _ x ->
                                    Bad True x

                                Good c s3 ->
                                    case parseD s3 of
                                        Bad _ x ->
                                            Bad True x

                                        Good d s4 ->
                                            case parseE s4 of
                                                Bad _ x ->
                                                    Bad True x

                                                Good e s5 ->
                                                    Good (func a b c d e) s5
        )


map5WithStartLocation : (Location -> a -> b -> c -> d -> e -> value) -> Parser a -> Parser b -> Parser c -> Parser d -> Parser e -> Parser value
map5WithStartLocation func (Parser parseA) (Parser parseB) (Parser parseC) (Parser parseD) (Parser parseE) =
    Parser
        (\s0 ->
            case parseA s0 of
                Bad committed x ->
                    Bad committed x

                Good a s1 ->
                    case parseB s1 of
                        Bad _ x ->
                            Bad True x

                        Good b s2 ->
                            case parseC s2 of
                                Bad _ x ->
                                    Bad True x

                                Good c s3 ->
                                    case parseD s3 of
                                        Bad _ x ->
                                            Bad True x

                                        Good d s4 ->
                                            case parseE s4 of
                                                Bad _ x ->
                                                    Bad True x

                                                Good e s5 ->
                                                    let
                                                        (PState _ _ _ s0Row s0Col) =
                                                            s0
                                                    in
                                                    Good (func { row = s0Row, column = s0Col } a b c d e) s5
        )


map5WithRange : (Range -> a -> b -> c -> d -> e -> value) -> Parser a -> Parser b -> Parser c -> Parser d -> Parser e -> Parser value
map5WithRange func (Parser parseA) (Parser parseB) (Parser parseC) (Parser parseD) (Parser parseE) =
    Parser
        (\s0 ->
            case parseA s0 of
                Bad committed x ->
                    Bad committed x

                Good a s1 ->
                    case parseB s1 of
                        Bad _ x ->
                            Bad True x

                        Good b s2 ->
                            case parseC s2 of
                                Bad _ x ->
                                    Bad True x

                                Good c s3 ->
                                    case parseD s3 of
                                        Bad _ x ->
                                            Bad True x

                                        Good d s4 ->
                                            case parseE s4 of
                                                Bad _ x ->
                                                    Bad True x

                                                Good e s5 ->
                                                    let
                                                        (PState _ _ _ s5Row s5Col) =
                                                            s5

                                                        (PState _ _ _ s0Row s0Col) =
                                                            s0
                                                    in
                                                    Good
                                                        (func
                                                            { start = { row = s0Row, column = s0Col }
                                                            , end = { row = s5Row, column = s5Col }
                                                            }
                                                            a
                                                            b
                                                            c
                                                            d
                                                            e
                                                        )
                                                        s5
        )


map6 : (a -> b -> c -> d -> e -> f -> value) -> Parser a -> Parser b -> Parser c -> Parser d -> Parser e -> Parser f -> Parser value
map6 func (Parser parseA) (Parser parseB) (Parser parseC) (Parser parseD) (Parser parseE) (Parser parseF) =
    Parser
        (\s0 ->
            case parseA s0 of
                Bad committed x ->
                    Bad committed x

                Good a s1 ->
                    case parseB s1 of
                        Bad _ x ->
                            Bad True x

                        Good b s2 ->
                            case parseC s2 of
                                Bad _ x ->
                                    Bad True x

                                Good c s3 ->
                                    case parseD s3 of
                                        Bad _ x ->
                                            Bad True x

                                        Good d s4 ->
                                            case parseE s4 of
                                                Bad _ x ->
                                                    Bad True x

                                                Good e s5 ->
                                                    case parseF s5 of
                                                        Bad _ x ->
                                                            Bad True x

                                                        Good f s6 ->
                                                            Good (func a b c d e f) s6
        )


map6WithStartLocation : (Location -> a -> b -> c -> d -> e -> f -> value) -> Parser a -> Parser b -> Parser c -> Parser d -> Parser e -> Parser f -> Parser value
map6WithStartLocation func (Parser parseA) (Parser parseB) (Parser parseC) (Parser parseD) (Parser parseE) (Parser parseF) =
    Parser
        (\s0 ->
            case parseA s0 of
                Bad committed x ->
                    Bad committed x

                Good a s1 ->
                    case parseB s1 of
                        Bad _ x ->
                            Bad True x

                        Good b s2 ->
                            case parseC s2 of
                                Bad _ x ->
                                    Bad True x

                                Good c s3 ->
                                    case parseD s3 of
                                        Bad _ x ->
                                            Bad True x

                                        Good d s4 ->
                                            case parseE s4 of
                                                Bad _ x ->
                                                    Bad True x

                                                Good e s5 ->
                                                    case parseF s5 of
                                                        Bad _ x ->
                                                            Bad True x

                                                        Good f s6 ->
                                                            let
                                                                (PState _ _ _ s0Row s0Col) =
                                                                    s0
                                                            in
                                                            Good (func { row = s0Row, column = s0Col } a b c d e f) s6
        )


map6WithRange : (Range -> a -> b -> c -> d -> e -> f -> value) -> Parser a -> Parser b -> Parser c -> Parser d -> Parser e -> Parser f -> Parser value
map6WithRange func (Parser parseA) (Parser parseB) (Parser parseC) (Parser parseD) (Parser parseE) (Parser parseF) =
    Parser
        (\s0 ->
            case parseA s0 of
                Bad committed x ->
                    Bad committed x

                Good a s1 ->
                    case parseB s1 of
                        Bad _ x ->
                            Bad True x

                        Good b s2 ->
                            case parseC s2 of
                                Bad _ x ->
                                    Bad True x

                                Good c s3 ->
                                    case parseD s3 of
                                        Bad _ x ->
                                            Bad True x

                                        Good d s4 ->
                                            case parseE s4 of
                                                Bad _ x ->
                                                    Bad True x

                                                Good e s5 ->
                                                    case parseF s5 of
                                                        Bad _ x ->
                                                            Bad True x

                                                        Good f s6 ->
                                                            let
                                                                (PState _ _ _ s6Row s6Col) =
                                                                    s6

                                                                (PState _ _ _ s0Row s0Col) =
                                                                    s0
                                                            in
                                                            Good
                                                                (func
                                                                    { start = { row = s0Row, column = s0Col }
                                                                    , end = { row = s6Row, column = s6Col }
                                                                    }
                                                                    a
                                                                    b
                                                                    c
                                                                    d
                                                                    e
                                                                    f
                                                                )
                                                                s6
        )


map7WithRange : (Range -> a -> b -> c -> d -> e -> f -> g -> value) -> Parser a -> Parser b -> Parser c -> Parser d -> Parser e -> Parser f -> Parser g -> Parser value
map7WithRange func (Parser parseA) (Parser parseB) (Parser parseC) (Parser parseD) (Parser parseE) (Parser parseF) (Parser parseG) =
    Parser
        (\s0 ->
            case parseA s0 of
                Bad committed x ->
                    Bad committed x

                Good a s1 ->
                    case parseB s1 of
                        Bad _ x ->
                            Bad True x

                        Good b s2 ->
                            case parseC s2 of
                                Bad _ x ->
                                    Bad True x

                                Good c s3 ->
                                    case parseD s3 of
                                        Bad _ x ->
                                            Bad True x

                                        Good d s4 ->
                                            case parseE s4 of
                                                Bad _ x ->
                                                    Bad True x

                                                Good e s5 ->
                                                    case parseF s5 of
                                                        Bad _ x ->
                                                            Bad True x

                                                        Good f s6 ->
                                                            case parseG s6 of
                                                                Bad _ x ->
                                                                    Bad True x

                                                                Good g s7 ->
                                                                    let
                                                                        (PState _ _ _ s7Row s7Col) =
                                                                            s7

                                                                        (PState _ _ _ s0Row s0Col) =
                                                                            s0
                                                                    in
                                                                    Good
                                                                        (func
                                                                            { start = { row = s0Row, column = s0Col }
                                                                            , end = { row = s7Row, column = s7Col }
                                                                            }
                                                                            a
                                                                            b
                                                                            c
                                                                            d
                                                                            e
                                                                            f
                                                                            g
                                                                        )
                                                                        s7
        )


map8WithStartLocation : (Location -> a -> b -> c -> d -> e -> f -> g -> h -> value) -> Parser a -> Parser b -> Parser c -> Parser d -> Parser e -> Parser f -> Parser g -> Parser h -> Parser value
map8WithStartLocation func (Parser parseA) (Parser parseB) (Parser parseC) (Parser parseD) (Parser parseE) (Parser parseF) (Parser parseG) (Parser parseH) =
    Parser
        (\s0 ->
            case parseA s0 of
                Bad committed x ->
                    Bad committed x

                Good a s1 ->
                    case parseB s1 of
                        Bad _ x ->
                            Bad True x

                        Good b s2 ->
                            case parseC s2 of
                                Bad _ x ->
                                    Bad True x

                                Good c s3 ->
                                    case parseD s3 of
                                        Bad _ x ->
                                            Bad True x

                                        Good d s4 ->
                                            case parseE s4 of
                                                Bad _ x ->
                                                    Bad True x

                                                Good e s5 ->
                                                    case parseF s5 of
                                                        Bad _ x ->
                                                            Bad True x

                                                        Good f s6 ->
                                                            case parseG s6 of
                                                                Bad _ x ->
                                                                    Bad True x

                                                                Good g s7 ->
                                                                    case parseH s7 of
                                                                        Bad _ x ->
                                                                            Bad True x

                                                                        Good h s8 ->
                                                                            let
                                                                                (PState _ _ _ s0Row s0Col) =
                                                                                    s0
                                                                            in
                                                                            Good (func { row = s0Row, column = s0Col } a b c d e f g h) s8
        )


map9WithRange : (Range -> a -> b -> c -> d -> e -> f -> g -> h -> i -> value) -> Parser a -> Parser b -> Parser c -> Parser d -> Parser e -> Parser f -> Parser g -> Parser h -> Parser i -> Parser value
map9WithRange func (Parser parseA) (Parser parseB) (Parser parseC) (Parser parseD) (Parser parseE) (Parser parseF) (Parser parseG) (Parser parseH) (Parser parseI) =
    Parser
        (\s0 ->
            case parseA s0 of
                Bad committed x ->
                    Bad committed x

                Good a s1 ->
                    case parseB s1 of
                        Bad _ x ->
                            Bad True x

                        Good b s2 ->
                            case parseC s2 of
                                Bad _ x ->
                                    Bad True x

                                Good c s3 ->
                                    case parseD s3 of
                                        Bad _ x ->
                                            Bad True x

                                        Good d s4 ->
                                            case parseE s4 of
                                                Bad _ x ->
                                                    Bad True x

                                                Good e s5 ->
                                                    case parseF s5 of
                                                        Bad _ x ->
                                                            Bad True x

                                                        Good f s6 ->
                                                            case parseG s6 of
                                                                Bad _ x ->
                                                                    Bad True x

                                                                Good g s7 ->
                                                                    case parseH s7 of
                                                                        Bad _ x ->
                                                                            Bad True x

                                                                        Good h s8 ->
                                                                            case parseI s8 of
                                                                                Bad _ x ->
                                                                                    Bad True x

                                                                                Good i s9 ->
                                                                                    let
                                                                                        (PState _ _ _ s9Row s9Col) =
                                                                                            s9

                                                                                        (PState _ _ _ s0Row s0Col) =
                                                                                            s0
                                                                                    in
                                                                                    Good
                                                                                        (func
                                                                                            { start = { row = s0Row, column = s0Col }
                                                                                            , end = { row = s9Row, column = s9Col }
                                                                                            }
                                                                                            a
                                                                                            b
                                                                                            c
                                                                                            d
                                                                                            e
                                                                                            f
                                                                                            g
                                                                                            h
                                                                                            i
                                                                                        )
                                                                                        s9
        )


{-| Indicate that a parser has reached a dead end. "Everything was going fine
until I ran into this problem." Check out the -AndThen helpers for where to use this.
-}
problem : String -> Parser a
problem msg =
    Parser
        (\(PState _ _ _ row col) ->
            Bad False (ExpectingCustom row col msg)
        )


orSucceed : Parser a -> a -> Parser a
orSucceed (Parser attemptFirst) secondRes =
    Parser
        (\s ->
            case attemptFirst s of
                (Good _ _) as firstGood ->
                    firstGood

                (Bad firstCommitted _) as firstBad ->
                    if firstCommitted then
                        firstBad

                    else
                        Good secondRes s
        )


map2OrSucceed : (a -> b -> value) -> Parser a -> Parser b -> value -> Parser value
map2OrSucceed func (Parser parseA) (Parser parseB) fallback =
    Parser
        (\s0 ->
            case parseA s0 of
                Bad c1 x ->
                    if c1 then
                        Bad True x

                    else
                        Good fallback s0

                Good a s1 ->
                    case parseB s1 of
                        Bad _ x ->
                            Bad True x

                        Good b s2 ->
                            Good (func a b) s2
        )


map2WithRangeOrSucceed : (Range -> a -> b -> value) -> Parser a -> Parser b -> value -> Parser value
map2WithRangeOrSucceed func (Parser parseA) (Parser parseB) fallback =
    Parser
        (\s0 ->
            case parseA s0 of
                Bad c1 x ->
                    if c1 then
                        Bad True x

                    else
                        Good fallback s0

                Good a s1 ->
                    case parseB s1 of
                        Bad _ x ->
                            Bad True x

                        Good b s2 ->
                            let
                                (PState _ _ _ s2Row s2Col) =
                                    s2

                                (PState _ _ _ s0Row s0Col) =
                                    s0
                            in
                            Good
                                (func
                                    { start = { row = s0Row, column = s0Col }
                                    , end = { row = s2Row, column = s2Col }
                                    }
                                    a
                                    b
                                )
                                s2
        )


map3OrSucceed : (a -> b -> c -> value) -> Parser a -> Parser b -> Parser c -> value -> Parser value
map3OrSucceed func (Parser parseA) (Parser parseB) (Parser parseC) fallback =
    Parser
        (\s0 ->
            case parseA s0 of
                Bad c1 x ->
                    if c1 then
                        Bad True x

                    else
                        Good fallback s0

                Good a s1 ->
                    case parseB s1 of
                        Bad _ x ->
                            Bad True x

                        Good b s2 ->
                            case parseC s2 of
                                Bad _ x ->
                                    Bad True x

                                Good c s3 ->
                                    Good (func a b c) s3
        )


map4OrSucceed : (a -> b -> c -> d -> value) -> Parser a -> Parser b -> Parser c -> Parser d -> value -> Parser value
map4OrSucceed func (Parser parseA) (Parser parseB) (Parser parseC) (Parser parseD) fallback =
    Parser
        (\s0 ->
            case parseA s0 of
                Bad c1 x ->
                    if c1 then
                        Bad True x

                    else
                        Good fallback s0

                Good a s1 ->
                    case parseB s1 of
                        Bad _ x ->
                            Bad True x

                        Good b s2 ->
                            case parseC s2 of
                                Bad _ x ->
                                    Bad True x

                                Good c s3 ->
                                    case parseD s3 of
                                        Bad _ x ->
                                            Bad True x

                                        Good d s4 ->
                                            Good (func a b c d) s4
        )


oneOf2Map :
    (first -> choice)
    -> Parser first
    -> (second -> choice)
    -> Parser second
    -> Parser choice
oneOf2Map firstToChoice (Parser attemptFirst) secondToChoice (Parser attemptSecond) =
    Parser
        (\s ->
            case attemptFirst s of
                Good first s1 ->
                    Good (firstToChoice first) s1

                Bad firstCommitted firstX ->
                    if firstCommitted then
                        Bad firstCommitted firstX

                    else
                        case attemptSecond s of
                            Good second s1 ->
                                Good (secondToChoice second) s1

                            Bad secondCommitted secondX ->
                                if secondCommitted then
                                    Bad secondCommitted secondX

                                else
                                    Bad False (ExpectingOneOf firstX secondX [])
        )


oneOf2MapWithStartRowColumnAndEndRowColumn :
    (Int -> Int -> first -> Int -> Int -> choice)
    -> Parser first
    -> (Int -> Int -> second -> Int -> Int -> choice)
    -> Parser second
    -> Parser choice
oneOf2MapWithStartRowColumnAndEndRowColumn firstToChoice (Parser attemptFirst) secondToChoice (Parser attemptSecond) =
    Parser
        (\s ->
            let
                (PState _ _ _ sRow sCol) =
                    s
            in
            case attemptFirst s of
                Good first s1 ->
                    let
                        (PState _ _ _ s1Row s1Col) =
                            s1
                    in
                    Good
                        (firstToChoice sRow sCol first s1Row s1Col)
                        s1

                Bad firstCommitted firstX ->
                    if firstCommitted then
                        Bad firstCommitted firstX

                    else
                        case attemptSecond s of
                            Good second s1 ->
                                let
                                    (PState _ _ _ s1Row s1Col) =
                                        s1
                                in
                                Good
                                    (secondToChoice sRow sCol second s1Row s1Col)
                                    s1

                            Bad secondCommitted secondX ->
                                if secondCommitted then
                                    Bad secondCommitted secondX

                                else
                                    Bad False (ExpectingOneOf firstX secondX [])
        )


oneOf2 : Parser a -> Parser a -> Parser a
oneOf2 (Parser attemptFirst) (Parser attemptSecond) =
    Parser
        (\s ->
            case attemptFirst s of
                (Good _ _) as firstGood ->
                    firstGood

                (Bad firstCommitted firstX) as firstBad ->
                    if firstCommitted then
                        firstBad

                    else
                        case attemptSecond s of
                            (Good _ _) as secondGood ->
                                secondGood

                            (Bad secondCommitted secondX) as secondBad ->
                                if secondCommitted then
                                    secondBad

                                else
                                    Bad False (ExpectingOneOf firstX secondX [])
        )


oneOf2OrSucceed : Parser a -> Parser a -> a -> Parser a
oneOf2OrSucceed (Parser attemptFirst) (Parser attemptSecond) thirdRes =
    Parser
        (\s ->
            case attemptFirst s of
                (Good _ _) as firstGood ->
                    firstGood

                (Bad firstCommitted _) as firstBad ->
                    if firstCommitted then
                        firstBad

                    else
                        case attemptSecond s of
                            (Good _ _) as secondGood ->
                                secondGood

                            (Bad secondCommitted _) as secondBad ->
                                if secondCommitted then
                                    secondBad

                                else
                                    Good thirdRes s
        )


oneOf3 : Parser a -> Parser a -> Parser a -> Parser a
oneOf3 (Parser attemptFirst) (Parser attemptSecond) (Parser attemptThird) =
    Parser
        (\s ->
            case attemptFirst s of
                (Good _ _) as firstGood ->
                    firstGood

                (Bad firstCommitted firstX) as firstBad ->
                    if firstCommitted then
                        firstBad

                    else
                        case attemptSecond s of
                            (Good _ _) as secondGood ->
                                secondGood

                            (Bad secondCommitted secondX) as secondBad ->
                                if secondCommitted then
                                    secondBad

                                else
                                    case attemptThird s of
                                        (Good _ _) as thirdGood ->
                                            thirdGood

                                        (Bad thirdCommitted thirdX) as thirdBad ->
                                            if thirdCommitted then
                                                thirdBad

                                            else
                                                Bad False (ExpectingOneOf firstX secondX [ thirdX ])
        )


oneOf4 : Parser a -> Parser a -> Parser a -> Parser a -> Parser a
oneOf4 (Parser attemptFirst) (Parser attemptSecond) (Parser attemptThird) (Parser attemptFourth) =
    Parser
        (\s ->
            case attemptFirst s of
                (Good _ _) as firstGood ->
                    firstGood

                (Bad firstCommitted firstX) as firstBad ->
                    if firstCommitted then
                        firstBad

                    else
                        case attemptSecond s of
                            (Good _ _) as secondGood ->
                                secondGood

                            (Bad secondCommitted secondX) as secondBad ->
                                if secondCommitted then
                                    secondBad

                                else
                                    case attemptThird s of
                                        (Good _ _) as thirdGood ->
                                            thirdGood

                                        (Bad thirdCommitted thirdX) as thirdBad ->
                                            if thirdCommitted then
                                                thirdBad

                                            else
                                                case attemptFourth s of
                                                    (Good _ _) as fourthGood ->
                                                        fourthGood

                                                    (Bad fourthCommitted fourthX) as fourthBad ->
                                                        if fourthCommitted then
                                                            fourthBad

                                                        else
                                                            Bad False (ExpectingOneOf firstX secondX [ thirdX, fourthX ])
        )


oneOf5 : Parser a -> Parser a -> Parser a -> Parser a -> Parser a -> Parser a
oneOf5 (Parser attemptFirst) (Parser attemptSecond) (Parser attemptThird) (Parser attemptFourth) (Parser attemptFifth) =
    Parser
        (\s ->
            case attemptFirst s of
                (Good _ _) as firstGood ->
                    firstGood

                (Bad firstCommitted firstX) as firstBad ->
                    if firstCommitted then
                        firstBad

                    else
                        case attemptSecond s of
                            (Good _ _) as secondGood ->
                                secondGood

                            (Bad secondCommitted secondX) as secondBad ->
                                if secondCommitted then
                                    secondBad

                                else
                                    case attemptThird s of
                                        (Good _ _) as thirdGood ->
                                            thirdGood

                                        (Bad thirdCommitted thirdX) as thirdBad ->
                                            if thirdCommitted then
                                                thirdBad

                                            else
                                                case attemptFourth s of
                                                    (Good _ _) as fourthGood ->
                                                        fourthGood

                                                    (Bad fourthCommitted fourthX) as fourthBad ->
                                                        if fourthCommitted then
                                                            fourthBad

                                                        else
                                                            case attemptFifth s of
                                                                (Good _ _) as fifthGood ->
                                                                    fifthGood

                                                                (Bad fifthCommitted fifthX) as fifthBad ->
                                                                    if fifthCommitted then
                                                                        fifthBad

                                                                    else
                                                                        Bad False (ExpectingOneOf firstX secondX [ thirdX, fourthX, fifthX ])
        )


oneOf7 : Parser a -> Parser a -> Parser a -> Parser a -> Parser a -> Parser a -> Parser a -> Parser a
oneOf7 (Parser attempt0) (Parser attempt1) (Parser attempt2) (Parser attempt3) (Parser attempt4) (Parser attempt5) (Parser attempt6) =
    Parser
        (\s ->
            case attempt0 s of
                (Good _ _) as good ->
                    good

                (Bad committed0 x0) as bad0 ->
                    if committed0 then
                        bad0

                    else
                        case attempt1 s of
                            (Good _ _) as good ->
                                good

                            (Bad committed1 x1) as bad1 ->
                                if committed1 then
                                    bad1

                                else
                                    case attempt2 s of
                                        (Good _ _) as good ->
                                            good

                                        (Bad committed2 x2) as bad2 ->
                                            if committed2 then
                                                bad2

                                            else
                                                case attempt3 s of
                                                    (Good _ _) as good ->
                                                        good

                                                    (Bad committed3 x3) as bad3 ->
                                                        if committed3 then
                                                            bad3

                                                        else
                                                            case attempt4 s of
                                                                (Good _ _) as good ->
                                                                    good

                                                                (Bad committed4 x4) as bad4 ->
                                                                    if committed4 then
                                                                        bad4

                                                                    else
                                                                        case attempt5 s of
                                                                            (Good _ _) as good ->
                                                                                good

                                                                            (Bad committed5 x5) as bad5 ->
                                                                                if committed5 then
                                                                                    bad5

                                                                                else
                                                                                    case attempt6 s of
                                                                                        (Good _ _) as good ->
                                                                                            good

                                                                                        (Bad committed6 x6) as bad6 ->
                                                                                            if committed6 then
                                                                                                bad6

                                                                                            else
                                                                                                Bad False (ExpectingOneOf x0 x1 [ x2, x3, x4, x5, x6 ])
        )


oneOf9 : Parser a -> Parser a -> Parser a -> Parser a -> Parser a -> Parser a -> Parser a -> Parser a -> Parser a -> Parser a
oneOf9 (Parser attempt0) (Parser attempt1) (Parser attempt2) (Parser attempt3) (Parser attempt4) (Parser attempt5) (Parser attempt6) (Parser attempt7) (Parser attempt8) =
    Parser
        (\s ->
            case attempt0 s of
                (Good _ _) as good ->
                    good

                (Bad committed0 x0) as bad0 ->
                    if committed0 then
                        bad0

                    else
                        case attempt1 s of
                            (Good _ _) as good ->
                                good

                            (Bad committed1 x1) as bad1 ->
                                if committed1 then
                                    bad1

                                else
                                    case attempt2 s of
                                        (Good _ _) as good ->
                                            good

                                        (Bad committed2 x2) as bad2 ->
                                            if committed2 then
                                                bad2

                                            else
                                                case attempt3 s of
                                                    (Good _ _) as good ->
                                                        good

                                                    (Bad committed3 x3) as bad3 ->
                                                        if committed3 then
                                                            bad3

                                                        else
                                                            case attempt4 s of
                                                                (Good _ _) as good ->
                                                                    good

                                                                (Bad committed4 x4) as bad4 ->
                                                                    if committed4 then
                                                                        bad4

                                                                    else
                                                                        case attempt5 s of
                                                                            (Good _ _) as good ->
                                                                                good

                                                                            (Bad committed5 x5) as bad5 ->
                                                                                if committed5 then
                                                                                    bad5

                                                                                else
                                                                                    case attempt6 s of
                                                                                        (Good _ _) as good ->
                                                                                            good

                                                                                        (Bad committed6 x6) as bad6 ->
                                                                                            if committed6 then
                                                                                                bad6

                                                                                            else
                                                                                                case attempt7 s of
                                                                                                    (Good _ _) as good ->
                                                                                                        good

                                                                                                    (Bad committed7 x7) as bad7 ->
                                                                                                        if committed7 then
                                                                                                            bad7

                                                                                                        else
                                                                                                            case attempt8 s of
                                                                                                                (Good _ _) as good ->
                                                                                                                    good

                                                                                                                (Bad committed8 x8) as bad8 ->
                                                                                                                    if committed8 then
                                                                                                                        bad8

                                                                                                                    else
                                                                                                                        Bad False (ExpectingOneOf x0 x1 [ x2, x3, x4, x5, x6, x7, x8 ])
        )


{-| Transform the successful result of a parser.

Maybe you have a value that is an integer or `null`:


    nullOrInt : Parser (Maybe Int)
    nullOrInt =
        ParserFast.oneOf2
            (ParserFast.map Just int)
            (ParserFast.keyword "null" Nothing)

    -- run nullOrInt "0"    == Ok (Just 0)
    -- run nullOrInt "13"   == Ok (Just 13)
    -- run nullOrInt "null" == Ok Nothing
    -- run nullOrInt "zero" == Err ...

-}
map : (a -> b) -> Parser a -> Parser b
map func (Parser parse) =
    Parser
        (\s0 ->
            case parse s0 of
                Good a s1 ->
                    Good (func a) s1

                Bad committed x ->
                    Bad committed x
        )


loopWhileSucceeds : Parser element -> folded -> (element -> folded -> folded) -> (folded -> res) -> Parser res
loopWhileSucceeds element initialFolded reduce foldedToRes =
    Parser
        (\s -> loopWhileSucceedsHelp element initialFolded reduce foldedToRes s)


loopWhileSucceedsHelp : Parser element -> folded -> (element -> folded -> folded) -> (folded -> res) -> State -> PStep res
loopWhileSucceedsHelp ((Parser parseElement) as element) soFar reduce foldedToRes s0 =
    case parseElement s0 of
        Good elementResult s1 ->
            loopWhileSucceedsHelp
                element
                (soFar |> reduce elementResult)
                reduce
                foldedToRes
                s1

        Bad elementCommitted x ->
            if elementCommitted then
                Bad True x

            else
                Good (foldedToRes soFar) s0


loopWhileSucceedsOntoResultFromParser :
    Parser element
    -> Parser folded
    -> (element -> folded -> folded)
    -> (folded -> res)
    -> Parser res
loopWhileSucceedsOntoResultFromParser element (Parser parseInitialFolded) reduce foldedToRes =
    Parser
        (\s0 ->
            case parseInitialFolded s0 of
                Good initialFolded s1 ->
                    loopWhileSucceedsHelp element initialFolded reduce foldedToRes s1

                Bad committed x ->
                    Bad committed x
        )


loopUntil : Parser () -> Parser element -> folded -> (element -> folded -> folded) -> (folded -> res) -> Parser res
loopUntil endParser element initialFolded reduce foldedToRes =
    Parser
        (\s -> loopUntilHelp False endParser element initialFolded reduce foldedToRes s)


loopUntilHelp : Bool -> Parser () -> Parser element -> folded -> (element -> folded -> folded) -> (folded -> res) -> State -> PStep res
loopUntilHelp committedSoFar ((Parser parseEnd) as endParser) ((Parser parseElement) as element) soFar reduce foldedToRes s0 =
    case parseEnd s0 of
        Good () s1 ->
            Good (foldedToRes soFar) s1

        Bad endCommitted endX ->
            if endCommitted then
                Bad True endX

            else
                case parseElement s0 of
                    Good elementResult s1 ->
                        loopUntilHelp True
                            endParser
                            element
                            (soFar |> reduce elementResult)
                            reduce
                            foldedToRes
                            s1

                    Bad elementCommitted x ->
                        Bad (committedSoFar || elementCommitted) x


{-| Parse an integer base 10.

    run int "1"    == Ok 1
    run int "1234" == Ok 1234

    run int "-789" == Err ...
    run int "0123" == Err ...
    run int "1.34" == Err ...
    run int "1e31" == Err ...
    run int "123a" == Err ...
    run int "0x1A" == Err ...



    int : Parser Int
    int =
        ParserFast.integerDecimalMapWithRange (\_ n -> n)

-}
integerDecimalMapWithRange : (Range -> Int -> res) -> Parser res
integerDecimalMapWithRange rangeAndIntToRes =
    Parser
        (\s0 ->
            let
                (PState s0Src s0Offset s0Indent s0Row s0Col) =
                    s0

                s1 : { int : Int, offset : Int }
                s1 =
                    convertIntegerDecimal s0Offset s0Src
            in
            if s1.offset == -1 then
                Bad False (ExpectingNumber s0Row s0Col)

            else
                let
                    newColumn : Int
                    newColumn =
                        s0Col + (s1.offset - s0Offset)
                in
                Good
                    (rangeAndIntToRes
                        { start = { row = s0Row, column = s0Col }
                        , end = { row = s0Row, column = newColumn }
                        }
                        s1.int
                    )
                    (PState
                        s0Src
                        s1.offset
                        s0Indent
                        s0Row
                        newColumn
                    )
        )


{-| Parse an integer base 10 or base 16.

    run int "1"    == Ok 1
    run int "1234" == Ok 1234
    run int "0x1A" == Ok 26

    run int "-789" == Err ...
    run int "0123" == Err ...
    run int "1.34" == Err ...
    run int "1e31" == Err ...
    run int "123a" == Err ...



    int : Parser Int
    int =
        ParserFast.integerDecimalOrHexadecimalMapWithRange (\_ n -> n) (\_ n -> n)

-}
integerDecimalOrHexadecimalMapWithRange : (Range -> Int -> res) -> (Range -> Int -> res) -> Parser res
integerDecimalOrHexadecimalMapWithRange rangeAndIntDecimalToRes rangeAndIntHexadecimalToRes =
    Parser
        (\s0 ->
            let
                (PState s0Src s0Offset s0Indent s0Row s0Col) =
                    s0

                s1 : { base : Base, offsetAndInt : { int : Int, offset : Int } }
                s1 =
                    convertIntegerDecimalOrHexadecimal s0Offset s0Src
            in
            if s1.offsetAndInt.offset == -1 then
                Bad False (ExpectingNumber s0Row s0Col)

            else
                let
                    newColumn : Int
                    newColumn =
                        s0Col + (s1.offsetAndInt.offset - s0Offset)

                    range : Range
                    range =
                        { start = { row = s0Row, column = s0Col }
                        , end = { row = s0Row, column = newColumn }
                        }
                in
                Good
                    (case s1.base of
                        Decimal ->
                            rangeAndIntDecimalToRes range s1.offsetAndInt.int

                        Hexadecimal ->
                            rangeAndIntHexadecimalToRes range s1.offsetAndInt.int
                    )
                    (PState
                        s0Src
                        s1.offsetAndInt.offset
                        s0Indent
                        s0Row
                        newColumn
                    )
        )


{-| Parse an integer base 10 or base 16 or a float.

    run number "1"    == Ok 1
    run number "1234" == Ok 1234
    run number "0x1A" == Ok 26
    run number "1.34" == Ok 1.34
    run number "2e3"  == Ok 2000

    run number "-789" == Err ...
    run number "0123" == Err ...
    run number "123a" == Err ...



    number : Parser Int
    number =
        ParserFast.integerDecimalOrHexadecimalMapWithRange (\_ n -> n) (\_ n -> n) (\_ n -> n)

-}
floatOrIntegerDecimalOrHexadecimalMapWithRange : (Range -> Float -> res) -> (Range -> Int -> res) -> (Range -> Int -> res) -> Parser res
floatOrIntegerDecimalOrHexadecimalMapWithRange rangeAndFloatToRes rangeAndIntDecimalToRes rangeAndIntHexadecimalToRes =
    Parser
        (\(PState s0Src s0Offset s0Indent s0Row s0Col) ->
            let
                s0OffsetInt : Int
                s0OffsetInt =
                    s0Offset

                s1 : { base : Base, offsetAndInt : { int : Int, offset : Int } }
                s1 =
                    convertIntegerDecimalOrHexadecimal s0Offset s0Src
            in
            if s1.offsetAndInt.offset == -1 then
                Bad False (ExpectingNumber s0Row s0Col)

            else
                let
                    offsetAfterFloat : Int
                    offsetAfterFloat =
                        skipFloatAfterIntegerDecimal s1.offsetAndInt.offset s0Src
                in
                if offsetAfterFloat == -1 then
                    let
                        newColumn : Int
                        newColumn =
                            s0Col + (s1.offsetAndInt.offset - s0OffsetInt)

                        range : Range
                        range =
                            { start = { row = s0Row, column = s0Col }
                            , end = { row = s0Row, column = newColumn }
                            }
                    in
                    Good
                        (case s1.base of
                            Decimal ->
                                rangeAndIntDecimalToRes range s1.offsetAndInt.int

                            Hexadecimal ->
                                rangeAndIntHexadecimalToRes range s1.offsetAndInt.int
                        )
                        (PState
                            s0Src
                            s1.offsetAndInt.offset
                            s0Indent
                            s0Row
                            newColumn
                        )

                else
                    let
                        slice : List Char
                        slice =
                            List.take
                                (offsetAfterFloat - s0OffsetInt)
                                (List.drop s0OffsetInt s0Src)
                    in
                    case String.toFloat (String.fromList slice) of
                        Just float ->
                            let
                                newColumn : Int
                                newColumn =
                                    s0Col + (offsetAfterFloat - s0OffsetInt)
                            in
                            Good
                                (rangeAndFloatToRes
                                    { start = { row = s0Row, column = s0Col }
                                    , end = { row = s0Row, column = newColumn }
                                    }
                                    float
                                )
                                (PState
                                    s0Src
                                    offsetAfterFloat
                                    s0Indent
                                    s0Row
                                    newColumn
                                )

                        Nothing ->
                            Bad False (ExpectingNumber s0Row s0Col)
        )


skipFloatAfterIntegerDecimal : Int -> List Char -> Int
skipFloatAfterIntegerDecimal offset src =
    case List.take 1 (List.drop offset src) of
        [ '.' ] ->
            let
                offsetAfterDigits : Int
                offsetAfterDigits =
                    skip1OrMoreDigits0To9 (offset + 1) src
            in
            if offsetAfterDigits == -1 then
                -1

            else
                case List.take 1 (List.drop offsetAfterDigits src) of
                    [ 'e' ] ->
                        skipAfterFloatExponentMark (offsetAfterDigits + 1) src

                    [ 'E' ] ->
                        skipAfterFloatExponentMark (offsetAfterDigits + 1) src

                    _ ->
                        offsetAfterDigits

        [ 'e' ] ->
            skipAfterFloatExponentMark (offset + 1) src

        [ 'E' ] ->
            skipAfterFloatExponentMark (offset + 1) src

        _ ->
            -1


skipAfterFloatExponentMark : Int -> List Char -> Int
skipAfterFloatExponentMark offset src =
    case List.take 1 (List.drop offset src) of
        [ '+' ] ->
            skip1OrMoreDigits0To9 (offset + 1) src

        [ '-' ] ->
            skip1OrMoreDigits0To9 (offset + 1) src

        _ ->
            skip1OrMoreDigits0To9 offset src


skip1OrMoreDigits0To9 : Int -> List Char -> Int
skip1OrMoreDigits0To9 offset src =
    case List.take 1 (List.drop offset src) of
        [ '0' ] ->
            skip0OrMoreDigits0To9 (offset + 1) src

        [ '1' ] ->
            skip0OrMoreDigits0To9 (offset + 1) src

        [ '2' ] ->
            skip0OrMoreDigits0To9 (offset + 1) src

        [ '3' ] ->
            skip0OrMoreDigits0To9 (offset + 1) src

        [ '4' ] ->
            skip0OrMoreDigits0To9 (offset + 1) src

        [ '5' ] ->
            skip0OrMoreDigits0To9 (offset + 1) src

        [ '6' ] ->
            skip0OrMoreDigits0To9 (offset + 1) src

        [ '7' ] ->
            skip0OrMoreDigits0To9 (offset + 1) src

        [ '8' ] ->
            skip0OrMoreDigits0To9 (offset + 1) src

        [ '9' ] ->
            skip0OrMoreDigits0To9 (offset + 1) src

        _ ->
            -1


skip0OrMoreDigits0To9 : Int -> List Char -> Int
skip0OrMoreDigits0To9 offset src =
    case List.take 1 (List.drop offset src) of
        [ '0' ] ->
            skip0OrMoreDigits0To9 (offset + 1) src

        [ '1' ] ->
            skip0OrMoreDigits0To9 (offset + 1) src

        [ '2' ] ->
            skip0OrMoreDigits0To9 (offset + 1) src

        [ '3' ] ->
            skip0OrMoreDigits0To9 (offset + 1) src

        [ '4' ] ->
            skip0OrMoreDigits0To9 (offset + 1) src

        [ '5' ] ->
            skip0OrMoreDigits0To9 (offset + 1) src

        [ '6' ] ->
            skip0OrMoreDigits0To9 (offset + 1) src

        [ '7' ] ->
            skip0OrMoreDigits0To9 (offset + 1) src

        [ '8' ] ->
            skip0OrMoreDigits0To9 (offset + 1) src

        [ '9' ] ->
            skip0OrMoreDigits0To9 (offset + 1) src

        _ ->
            offset


convert1OrMoreHexadecimal : Int -> List Char -> { int : Int, offset : Int }
convert1OrMoreHexadecimal offset src =
    case List.take 1 (List.drop offset src) of
        [ '0' ] ->
            convert0OrMoreHexadecimal 0 (offset + 1) src

        [ '1' ] ->
            convert0OrMoreHexadecimal 1 (offset + 1) src

        [ '2' ] ->
            convert0OrMoreHexadecimal 2 (offset + 1) src

        [ '3' ] ->
            convert0OrMoreHexadecimal 3 (offset + 1) src

        [ '4' ] ->
            convert0OrMoreHexadecimal 4 (offset + 1) src

        [ '5' ] ->
            convert0OrMoreHexadecimal 5 (offset + 1) src

        [ '6' ] ->
            convert0OrMoreHexadecimal 6 (offset + 1) src

        [ '7' ] ->
            convert0OrMoreHexadecimal 7 (offset + 1) src

        [ '8' ] ->
            convert0OrMoreHexadecimal 8 (offset + 1) src

        [ '9' ] ->
            convert0OrMoreHexadecimal 9 (offset + 1) src

        [ 'a' ] ->
            convert0OrMoreHexadecimal 10 (offset + 1) src

        [ 'A' ] ->
            convert0OrMoreHexadecimal 10 (offset + 1) src

        [ 'b' ] ->
            convert0OrMoreHexadecimal 11 (offset + 1) src

        [ 'B' ] ->
            convert0OrMoreHexadecimal 11 (offset + 1) src

        [ 'c' ] ->
            convert0OrMoreHexadecimal 12 (offset + 1) src

        [ 'C' ] ->
            convert0OrMoreHexadecimal 12 (offset + 1) src

        [ 'd' ] ->
            convert0OrMoreHexadecimal 13 (offset + 1) src

        [ 'D' ] ->
            convert0OrMoreHexadecimal 13 (offset + 1) src

        [ 'e' ] ->
            convert0OrMoreHexadecimal 14 (offset + 1) src

        [ 'E' ] ->
            convert0OrMoreHexadecimal 14 (offset + 1) src

        [ 'f' ] ->
            convert0OrMoreHexadecimal 15 (offset + 1) src

        [ 'F' ] ->
            convert0OrMoreHexadecimal 15 (offset + 1) src

        _ ->
            { int = 0, offset = -1 }


convert0OrMoreHexadecimal : Int -> Int -> List Char -> { int : Int, offset : Int }
convert0OrMoreHexadecimal soFar offset src =
    case List.take 1 (List.drop offset src) of
        [ '0' ] ->
            convert0OrMoreHexadecimal (soFar * 16) (offset + 1) src

        [ '1' ] ->
            convert0OrMoreHexadecimal (soFar * 16 + 1) (offset + 1) src

        [ '2' ] ->
            convert0OrMoreHexadecimal (soFar * 16 + 2) (offset + 1) src

        [ '3' ] ->
            convert0OrMoreHexadecimal (soFar * 16 + 3) (offset + 1) src

        [ '4' ] ->
            convert0OrMoreHexadecimal (soFar * 16 + 4) (offset + 1) src

        [ '5' ] ->
            convert0OrMoreHexadecimal (soFar * 16 + 5) (offset + 1) src

        [ '6' ] ->
            convert0OrMoreHexadecimal (soFar * 16 + 6) (offset + 1) src

        [ '7' ] ->
            convert0OrMoreHexadecimal (soFar * 16 + 7) (offset + 1) src

        [ '8' ] ->
            convert0OrMoreHexadecimal (soFar * 16 + 8) (offset + 1) src

        [ '9' ] ->
            convert0OrMoreHexadecimal (soFar * 16 + 9) (offset + 1) src

        [ 'a' ] ->
            convert0OrMoreHexadecimal (soFar * 16 + 10) (offset + 1) src

        [ 'A' ] ->
            convert0OrMoreHexadecimal (soFar * 16 + 10) (offset + 1) src

        [ 'b' ] ->
            convert0OrMoreHexadecimal (soFar * 16 + 11) (offset + 1) src

        [ 'B' ] ->
            convert0OrMoreHexadecimal (soFar * 16 + 11) (offset + 1) src

        [ 'c' ] ->
            convert0OrMoreHexadecimal (soFar * 16 + 12) (offset + 1) src

        [ 'C' ] ->
            convert0OrMoreHexadecimal (soFar * 16 + 12) (offset + 1) src

        [ 'd' ] ->
            convert0OrMoreHexadecimal (soFar * 16 + 13) (offset + 1) src

        [ 'D' ] ->
            convert0OrMoreHexadecimal (soFar * 16 + 13) (offset + 1) src

        [ 'e' ] ->
            convert0OrMoreHexadecimal (soFar * 16 + 14) (offset + 1) src

        [ 'E' ] ->
            convert0OrMoreHexadecimal (soFar * 16 + 14) (offset + 1) src

        [ 'f' ] ->
            convert0OrMoreHexadecimal (soFar * 16 + 15) (offset + 1) src

        [ 'F' ] ->
            convert0OrMoreHexadecimal (soFar * 16 + 15) (offset + 1) src

        _ ->
            { int = soFar, offset = offset }


type Base
    = Decimal
    | Hexadecimal


convertIntegerDecimalOrHexadecimal : Int -> List Char -> { base : Base, offsetAndInt : { int : Int, offset : Int } }
convertIntegerDecimalOrHexadecimal offset src =
    case List.take 1 (List.drop offset src) of
        [ '0' ] ->
            case List.take 1 (List.drop (offset + 1) src) of
                [ 'x' ] ->
                    let
                        --_ = Debug.todo "not huh"
                        hex : { int : Int, offset : Int }
                        hex =
                            convert1OrMoreHexadecimal (offset + 2) src
                    in
                    { base = Hexadecimal, offsetAndInt = { int = hex.int, offset = hex.offset } }

                _ ->
                    { base = Decimal, offsetAndInt = { int = 0, offset = offset + 1 } }

        [ '1' ] ->
            { base = Decimal, offsetAndInt = convert0OrMore0To9s 1 (offset + 1) src }

        [ '2' ] ->
            { base = Decimal, offsetAndInt = convert0OrMore0To9s 2 (offset + 1) src }

        [ '3' ] ->
            { base = Decimal, offsetAndInt = convert0OrMore0To9s 3 (offset + 1) src }

        [ '4' ] ->
            { base = Decimal, offsetAndInt = convert0OrMore0To9s 4 (offset + 1) src }

        [ '5' ] ->
            { base = Decimal, offsetAndInt = convert0OrMore0To9s 5 (offset + 1) src }

        [ '6' ] ->
            { base = Decimal, offsetAndInt = convert0OrMore0To9s 6 (offset + 1) src }

        [ '7' ] ->
            { base = Decimal, offsetAndInt = convert0OrMore0To9s 7 (offset + 1) src }

        [ '8' ] ->
            { base = Decimal, offsetAndInt = convert0OrMore0To9s 8 (offset + 1) src }

        [ '9' ] ->
            { base = Decimal, offsetAndInt = convert0OrMore0To9s 9 (offset + 1) src }

        _ ->
            errorAsBaseOffsetAndInt


errorAsBaseOffsetAndInt : { base : Base, offsetAndInt : { int : number, offset : number } }
errorAsBaseOffsetAndInt =
    { base = Decimal, offsetAndInt = { int = 0, offset = -1 } }


convertIntegerDecimal : Int -> List Char -> { int : Int, offset : Int }
convertIntegerDecimal offset src =
    case List.take 1 (List.drop offset src) of
        [ '0' ] ->
            { int = 0, offset = offset + 1 }

        [ '1' ] ->
            convert0OrMore0To9s 1 (offset + 1) src

        [ '2' ] ->
            convert0OrMore0To9s 2 (offset + 1) src

        [ '3' ] ->
            convert0OrMore0To9s 3 (offset + 1) src

        [ '4' ] ->
            convert0OrMore0To9s 4 (offset + 1) src

        [ '5' ] ->
            convert0OrMore0To9s 5 (offset + 1) src

        [ '6' ] ->
            convert0OrMore0To9s 6 (offset + 1) src

        [ '7' ] ->
            convert0OrMore0To9s 7 (offset + 1) src

        [ '8' ] ->
            convert0OrMore0To9s 8 (offset + 1) src

        [ '9' ] ->
            convert0OrMore0To9s 9 (offset + 1) src

        _ ->
            errorAsOffsetAndInt


errorAsOffsetAndInt : { int : Int, offset : Int }
errorAsOffsetAndInt =
    { int = 0, offset = -1 }


convert0OrMore0To9s : Int -> Int -> List Char -> { int : Int, offset : Int }
convert0OrMore0To9s soFar offset src =
    case List.take 1 (List.drop offset src) of
        [ '0' ] ->
            convert0OrMore0To9s (soFar * 10) (offset + 1) src

        [ '1' ] ->
            convert0OrMore0To9s (soFar * 10 + 1) (offset + 1) src

        [ '2' ] ->
            convert0OrMore0To9s (soFar * 10 + 2) (offset + 1) src

        [ '3' ] ->
            convert0OrMore0To9s (soFar * 10 + 3) (offset + 1) src

        [ '4' ] ->
            convert0OrMore0To9s (soFar * 10 + 4) (offset + 1) src

        [ '5' ] ->
            convert0OrMore0To9s (soFar * 10 + 5) (offset + 1) src

        [ '6' ] ->
            convert0OrMore0To9s (soFar * 10 + 6) (offset + 1) src

        [ '7' ] ->
            convert0OrMore0To9s (soFar * 10 + 7) (offset + 1) src

        [ '8' ] ->
            convert0OrMore0To9s (soFar * 10 + 8) (offset + 1) src

        [ '9' ] ->
            convert0OrMore0To9s (soFar * 10 + 9) (offset + 1) src

        _ ->
            { int = soFar, offset = offset }


{-| Parse exact text like `(` and `,`.
Make sure the given String isn't empty and does not contain \\n
or 2-part UTF-16 characters.

    run (symbol "[" ()) "[" == Ok ()
    run (symbol "[" ()) "4" == Err ... (ExpectingSymbol "[") ...

**Note:** This is good for stuff like brackets and semicolons,
but it might need extra validations for binary operators like `-` because you can find
yourself in weird situations. For example, is `3--4` a typo? Or is it `3 - -4`?

-}
symbol : String -> res -> Parser res
symbol str res =
    let
        strChars : List Char
        strChars =
            String.toList str

        strLength : Int
        strLength =
            List.length strChars
    in
    Parser
        (\(PState sSrc sOffset sIndent sRow sCol) ->
            let
                sOffsetInt : Int
                sOffsetInt =
                    sOffset

                sColInt : Int
                sColInt =
                    sCol
            in
            if List.take strLength (List.drop sOffset sSrc) == strChars then
                Good res
                    (PState sSrc
                        (sOffsetInt + strLength)
                        sIndent
                        sRow
                        (sColInt + strLength)
                    )

            else
                Bad False (ExpectingSymbol sRow sCol str)
        )


followedBySymbol : String -> Parser a -> Parser a
followedBySymbol str (Parser parsePrevious) =
    let
        strChars : List Char
        strChars =
            String.toList str

        strLength : Int
        strLength =
            List.length strChars
    in
    Parser
        (\s0 ->
            case parsePrevious s0 of
                Good res (PState s1Src s1Offset s1Indent s1Row s1Col) ->
                    let
                        s1OffsetInt : Int
                        s1OffsetInt =
                            s1Offset

                        s1ColInt : Int
                        s1ColInt =
                            s1Col
                    in
                    if List.take strLength (List.drop s1OffsetInt s1Src) == strChars then
                        Good res
                            (PState
                                s1Src
                                (s1OffsetInt + strLength)
                                s1Indent
                                s1Row
                                (s1ColInt + strLength)
                            )

                    else
                        Bad True (ExpectingSymbol s1Row s1Col str)

                bad ->
                    bad
        )


symbolWithEndLocation : String -> (Location -> res) -> Parser res
symbolWithEndLocation str endLocationToRes =
    let
        strChars : List Char
        strChars =
            String.toList str

        strLength : Int
        strLength =
            List.length strChars
    in
    Parser
        (\(PState sSrc sOffset sIndent sRow sCol) ->
            let
                sOffsetInt : Int
                sOffsetInt =
                    sOffset

                sColInt : Int
                sColInt =
                    sCol

                srcSlice : List Char
                srcSlice =
                    List.take
                        strLength
                        (List.drop sOffsetInt sSrc)
            in
            if srcSlice == strChars then
                let
                    newCol : Int
                    newCol =
                        sColInt + strLength
                in
                Good
                    (endLocationToRes { row = sRow, column = newCol })
                    (PState sSrc
                        (sOffsetInt + strLength)
                        sIndent
                        sRow
                        newCol
                    )

            else
                Bad False (ExpectingSymbol sRow sCol str)
        )


symbolWithRange : String -> (Range -> res) -> Parser res
symbolWithRange str startAndEndLocationToRes =
    let
        strChars : List Char
        strChars =
            String.toList str

        strLength : Int
        strLength =
            List.length strChars
    in
    Parser
        (\(PState sSrc sOffset sIndent sRow sCol) ->
            let
                sOffsetInt : Int
                sOffsetInt =
                    sOffset

                sColInt : Int
                sColInt =
                    sCol

                srcSlice : List Char
                srcSlice =
                    List.take
                        strLength
                        (List.drop sOffsetInt sSrc)
            in
            if srcSlice == strChars then
                let
                    newCol : Int
                    newCol =
                        sColInt + strLength
                in
                Good
                    (startAndEndLocationToRes
                        { start = { row = sRow, column = sCol }
                        , end = { row = sRow, column = newCol }
                        }
                    )
                    (PState sSrc
                        (sOffsetInt + strLength)
                        sIndent
                        sRow
                        newCol
                    )

            else
                Bad False (ExpectingSymbol sRow sCol str)
        )


{-| Make sure the given String isn't empty and does not contain \\n
or 2-part UTF-16 characters.
-}
symbolFollowedBy : String -> Parser next -> Parser next
symbolFollowedBy str parseNext =
    Parser
        (symbolFollowedByParser
            (String.toList str)
            parseNext
        )


symbolFollowedByParser : List Char -> Parser next -> State -> PStep next
symbolFollowedByParser strChars (Parser parseNext) (PState sSrc sOffset sIndent sRow sCol) =
    let
        strLength : Int
        strLength =
            List.length strChars

        sOffsetInt : Int
        sOffsetInt =
            sOffset

        sColInt : Int
        sColInt =
            sCol

        srcSlice : List Char
        srcSlice =
            List.take
                strLength
                (List.drop sOffsetInt sSrc)
    in
    if srcSlice == strChars then
        parseNext
            (PState sSrc
                (sOffsetInt + strLength)
                sIndent
                sRow
                (sColInt + strLength)
            )
            |> pStepCommit

    else
        Bad False (ExpectingSymbol sRow sCol (String.fromList strChars))


{-| Make sure the given String isn't empty and does not contain \\n
or 2-part UTF-16 characters.
-}
symbolBacktrackableFollowedBy : String -> Parser next -> Parser next
symbolBacktrackableFollowedBy str (Parser parseNext) =
    let
        strChars : List Char
        strChars =
            String.toList str

        strLength : Int
        strLength =
            List.length strChars
    in
    Parser
        (\(PState sSrc sOffset sIndent sRow sCol) ->
            let
                sOffsetInt : Int
                sOffsetInt =
                    sOffset

                sColInt : Int
                sColInt =
                    sCol

                srcSlice : List Char
                srcSlice =
                    List.take
                        strLength
                        (List.drop sOffsetInt sSrc)
            in
            if srcSlice == strChars then
                parseNext
                    (PState sSrc
                        (sOffsetInt + strLength)
                        sIndent
                        sRow
                        (sColInt + strLength)
                    )

            else
                Bad False (ExpectingSymbol sRow sCol str)
        )


pStepCommit : PStep a -> PStep a
pStepCommit pStep =
    case pStep of
        (Good _ _) as good ->
            good

        Bad _ x ->
            Bad True x


{-| Parse words without other word characters after like `let`, `case`, `type` or `import`.
Make sure the given String isn't empty and does not contain \\n
or 2-part UTF-16 characters.

    run (keyword "let" ()) "let"     == Ok ()
    run (keyword "let" ()) "var"     == Err ... (ExpectingKeyword "let") ...
    run (keyword "let" ()) "letters" == Err ... (ExpectingKeyword "let") ...

**Note:** Notice the third case there! `keyword` actually looks ahead one
character to make sure it is not a letter, digit, or underscore.
This will help with the weird cases like
`case(x, y)` being totally fine but `casex` not being fine.

-}
keyword : String -> res -> Parser res
keyword kwd res =
    let
        kwdChars : List Char
        kwdChars =
            String.toList kwd

        kwdLength : Int
        kwdLength =
            List.length kwdChars
    in
    Parser
        (\(PState sSrc sOffset sIndent sRow sCol) ->
            let
                sOffsetInt : Int
                sOffsetInt =
                    sOffset

                newOffset : Int
                newOffset =
                    sOffsetInt + kwdLength

                sColInt : Int
                sColInt =
                    sCol
            in
            if List.take kwdLength (List.drop sOffset sSrc) == kwdChars then
                if isSubCharAlphaNumOrUnderscore newOffset sSrc then
                    Bad False (ExpectingKeyword sRow sCol kwd)

                else
                    Good res
                        (PState sSrc
                            newOffset
                            sIndent
                            sRow
                            (sColInt + kwdLength)
                        )

            else
                Bad False (ExpectingKeyword sRow sCol kwd)
        )


isSubCharAlphaNumOrUnderscore : Int -> List Char -> Bool
isSubCharAlphaNumOrUnderscore offset string =
    case List.take 1 (List.drop offset string) of
        [ '_' ] ->
            True

        [ c ] ->
            Char.isAlphaNum c

        _ ->
            False


{-| Make sure the given String isn't empty and does not contain \\n
or 2-part UTF-16 characters.
-}
keywordFollowedBy : String -> Parser next -> Parser next
keywordFollowedBy kwd parseNext =
    Parser
        (keywordFollowedByParser
            (String.toList kwd)
            parseNext
        )


keywordFollowedByParser : List Char -> Parser next -> State -> PStep next
keywordFollowedByParser kwdChars (Parser parseNext) (PState sSrc sOffset sIndent sRow sCol) =
    let
        kwdLength : Int
        kwdLength =
            List.length kwdChars

        sOffsetInt : Int
        sOffsetInt =
            sOffset

        sColInt : Int
        sColInt =
            sCol

        srcSlice : List Char
        srcSlice =
            List.take
                kwdLength
                (List.drop sOffsetInt sSrc)

        newOffset : Int
        newOffset =
            sOffsetInt + kwdLength
    in
    if srcSlice == kwdChars then
        if isSubCharAlphaNumOrUnderscore newOffset sSrc then
            Bad False (ExpectingKeyword sRow sCol (String.fromList kwdChars))

        else
            parseNext
                (PState
                    sSrc
                    newOffset
                    sIndent
                    sRow
                    (sColInt + kwdLength)
                )
                |> pStepCommit

    else
        Bad False (ExpectingKeyword sRow sCol (String.fromList kwdChars))


anyChar : Parser Char
anyChar =
    Parser
        (\(PState sSrc sOffset sIndent sRow sCol) ->
            let
                sOffsetInt : Int
                sOffsetInt =
                    sOffset

                newOffset : Int
                newOffset =
                    charOrEnd sOffsetInt sSrc

                sColInt : Int
                sColInt =
                    sCol

                sRowInt : Int
                sRowInt =
                    sRow
            in
            if newOffset == -1 then
                -- end of source
                Bad False (ExpectingAnyChar sRow sCol)

            else if newOffset == -2 then
                -- newline
                Good '\n'
                    (PState
                        sSrc
                        (sOffsetInt + 1)
                        sIndent
                        (sRowInt + 1)
                        1
                    )

            else
                -- found
                case List.take 1 (List.drop sOffsetInt sSrc) of
                    [] ->
                        Bad False (ExpectingAnyChar sRowInt sCol)

                    c :: _ ->
                        Good c
                            (PState sSrc
                                newOffset
                                sIndent
                                sRowInt
                                (sColInt + 1)
                            )
        )


charOrEnd : Int -> List Char -> Int
charOrEnd offset string =
    case List.take 1 (List.drop offset string) of
        [ '\n' ] ->
            -2

        [] ->
            -1

        _ ->
            offset + 1


whileMapWithRange : (Char -> Bool) -> (Range -> String -> res) -> Parser res
whileMapWithRange isGood rangeAndConsumedStringToRes =
    Parser
        (\(PState s0Src s0Offset s0Indent s0Row s0Col) ->
            let
                s0OffsetInt : Int
                s0OffsetInt =
                    s0Offset

                s1 : State
                s1 =
                    skipWhileHelp isGood s0Offset s0Row s0Col s0Src s0Indent

                (PState _ s1Offset _ s1Row s1Col) =
                    s1

                s1OffsetInt : Int
                s1OffsetInt =
                    s1Offset

                length : Int
                length =
                    s1OffsetInt - s0OffsetInt

                slice : List Char
                slice =
                    List.take
                        length
                        (List.drop s0Offset s0Src)
            in
            Good
                (rangeAndConsumedStringToRes
                    { start = { row = s0Row, column = s0Col }
                    , end = { row = s1Row, column = s1Col }
                    }
                    (String.fromList slice)
                )
                s1
        )


skipWhileHelp : (Char -> Bool) -> Int -> Int -> Int -> List Char -> Int -> State
skipWhileHelp isGood offset row col src indent =
    case List.take 1 (List.drop offset src) of
        actualChar :: _ ->
            if isGood actualChar then
                case actualChar of
                    '\n' ->
                        skipWhileHelp isGood (offset + 1) (row + 1) 1 src indent

                    _ ->
                        skipWhileHelp isGood (offset + 1) row (col + 1) src indent

            else
                -- no match
                PState src offset indent row col

        [] ->
            -- no match
            PState src offset indent row col


skipWhileWithoutLinebreakHelp : (Char -> Bool) -> Int -> Int -> Int -> List Char -> Int -> State
skipWhileWithoutLinebreakHelp isGood offset row col src indent =
    case List.take 1 (List.drop offset src) of
        actualChar :: _ ->
            if isGood actualChar then
                skipWhileWithoutLinebreakHelp isGood (offset + 1) row (col + 1) src indent

            else
                -- no match
                PState src offset indent row col

        [] ->
            -- no match
            PState src offset indent row col


skipWhileWithoutLinebreakAnd2PartUtf16Help : (Char -> Bool) -> Int -> List Char -> Int
skipWhileWithoutLinebreakAnd2PartUtf16Help isGood offset src =
    case List.take 1 (List.drop offset src) of
        actualChar :: _ ->
            if isGood actualChar then
                skipWhileWithoutLinebreakAnd2PartUtf16Help isGood (offset + 1) src

            else
                offset

        [] ->
            offset


followedBySkipWhileWhitespace : Parser before -> Parser before
followedBySkipWhileWhitespace (Parser parseBefore) =
    Parser
        (\s0 ->
            case parseBefore s0 of
                Good res s1 ->
                    let
                        (PState s1Src s1Offset s1Indent s1Row s1Col) =
                            s1

                        s2 : State
                        s2 =
                            skipWhileWhitespaceHelp s1Offset s1Row s1Col s1Src s1Indent
                    in
                    Good res s2

                bad ->
                    bad
        )


{-| Match zero or more \\n, \\r and space characters, then proceed with the given parser
-}
skipWhileWhitespaceFollowedBy : Parser next -> Parser next
skipWhileWhitespaceFollowedBy (Parser parseNext) =
    Parser
        (\(PState s0Src s0Offset s0Indent s0Row s0Col) ->
            let
                s1 : State
                s1 =
                    skipWhileWhitespaceHelp s0Offset s0Row s0Col s0Src s0Indent
            in
            parseNext s1 |> pStepCommit
        )


skipWhileWhitespaceHelp : Int -> Int -> Int -> List Char -> Int -> State
skipWhileWhitespaceHelp offset row col src indent =
    case List.take 1 (List.drop offset src) of
        [ ' ' ] ->
            skipWhileWhitespaceHelp (offset + 1) row (col + 1) src indent

        [ '\n' ] ->
            skipWhileWhitespaceHelp (offset + 1) (row + 1) 1 src indent

        [ '\u{000D}' ] ->
            skipWhileWhitespaceHelp (offset + 1) row (col + 1) src indent

        -- empty or non-whitespace
        _ ->
            PState src offset indent row col


changeIndent : Int -> State -> State
changeIndent newIndent (PState src offset _ row col) =
    PState src offset newIndent row col


{-| For a given parser, take the current start column as indentation for the whole block
parsed by the given parser
-}
withIndentSetToColumn : Parser a -> Parser a
withIndentSetToColumn (Parser parse) =
    Parser
        (\s0 ->
            let
                (PState _ _ s0Indent _ s0Col) =
                    s0
            in
            case parse (changeIndent s0Col s0) of
                Good a s1 ->
                    Good a (changeIndent s0Indent s1)

                bad ->
                    bad
        )


withIndentSetToColumnMinus : Int -> Parser a -> Parser a
withIndentSetToColumnMinus columnToMoveIndentationBaseBackBy (Parser parse) =
    Parser
        (\s0 ->
            let
                (PState _ _ s0Indent _ s0Col) =
                    s0
            in
            case parse (changeIndent (s0Col - columnToMoveIndentationBaseBackBy) s0) of
                Good a s1 ->
                    Good a (changeIndent s0Indent s1)

                bad ->
                    bad
        )


mapWithRange :
    (Range -> a -> b)
    -> Parser a
    -> Parser b
mapWithRange combineStartAndResult (Parser parse) =
    Parser
        (\s0 ->
            case parse s0 of
                Good a s1 ->
                    let
                        (PState _ _ _ s0Row s0Col) =
                            s0

                        (PState _ _ _ s1Row s1Col) =
                            s1
                    in
                    Good
                        (combineStartAndResult
                            { start = { row = s0Row, column = s0Col }
                            , end = { row = s1Row, column = s1Col }
                            }
                            a
                        )
                        s1

                Bad committed x ->
                    Bad committed x
        )


{-| Create a parser for variables. If we wanted to parse type variables in Elm,
we could try something like this:

    import Char
    import ParserFast exposing (..)
    import Set

    typeVar : Parser String
    typeVar =
        ParserFast.ifFollowedByWhileValidateWithoutLinebreak
            Char.isLower
            (\c -> Char.isAlphaNum c || c == '_')
            (\final -> final == "let" || final == "in" || final == "case" || final == "of")

This is saying it _must_ start with a lower-case character. After that,
characters can be letters, digits, or underscores. It is also saying that if
you run into any of these reserved names after parsing as much as possible,
it is definitely not a variable.

-}
ifFollowedByWhileValidateWithoutLinebreak :
    (Char -> Bool)
    -> (Char -> Bool)
    -> (String -> Bool)
    -> Parser String
ifFollowedByWhileValidateWithoutLinebreak firstIsOkay afterFirstIsOkay resultIsOkay =
    Parser
        (ifFollowedByWhileValidateWithoutLinebreakParser
            firstIsOkay
            afterFirstIsOkay
            resultIsOkay
        )


ifFollowedByWhileValidateWithoutLinebreakParser :
    (Char -> Bool)
    -> (Char -> Bool)
    -> (String -> Bool)
    -> State
    -> PStep String
ifFollowedByWhileValidateWithoutLinebreakParser firstIsOkay afterFirstIsOkay resultIsOkay (PState sSrc sOffset sIndent sRow sCol) =
    let
        sOffsetInt : Int
        sOffsetInt =
            sOffset

        sColInt : Int
        sColInt =
            sCol
    in
    case List.take 1 (List.drop sOffset sSrc) of
        [] ->
            Bad False (ExpectingCharSatisfyingPredicate sRow sCol)

        firstChar :: _ ->
            if firstIsOkay firstChar then
                let
                    s1 : State
                    s1 =
                        skipWhileWithoutLinebreakHelp
                            afterFirstIsOkay
                            (sOffsetInt + 1)
                            sRow
                            (sColInt + 1)
                            sSrc
                            sIndent

                    (PState _ s1Offset _ _ _) =
                        s1

                    s1OffsetInt : Int
                    s1OffsetInt =
                        s1Offset

                    nameChars : List Char
                    nameChars =
                        List.take
                            (s1OffsetInt - sOffsetInt)
                            (List.drop sOffsetInt sSrc)

                    name : String
                    name =
                        String.fromList nameChars
                in
                if resultIsOkay name then
                    Good name s1

                else
                    Bad
                        False
                        (ExpectingStringSatisfyingPredicate sRow (sColInt + 1))

            else
                Bad False (ExpectingCharSatisfyingPredicate sRow sCol)


whileWithoutLinebreakAnd2PartUtf16ToResultAndThen :
    (Char -> Bool)
    -> (String -> Result String intermediate)
    -> (intermediate -> Parser res)
    -> Parser res
whileWithoutLinebreakAnd2PartUtf16ToResultAndThen whileCharIsOkay consumedStringToIntermediateOrErr intermediateToFollowupParser =
    Parser
        (\(PState s0Src s0Offset s0Indent s0Row s0Col) ->
            let
                s0OffsetInt : Int
                s0OffsetInt =
                    s0Offset

                s1Offset : Int
                s1Offset =
                    skipWhileWithoutLinebreakAnd2PartUtf16Help
                        whileCharIsOkay
                        s0Offset
                        s0Src

                whileContentLength : Int
                whileContentLength =
                    s1Offset - s0OffsetInt

                whileContentSlice : List Char
                whileContentSlice =
                    List.take
                        whileContentLength
                        (List.drop s0Offset s0Src)

                whileContent : String
                whileContent =
                    String.fromList whileContentSlice
            in
            case consumedStringToIntermediateOrErr whileContent of
                Err problemMessage ->
                    Bad False (ExpectingCustom s0Row s0Col problemMessage)

                Ok intermediate ->
                    let
                        s1Column : Int
                        s1Column =
                            s0Col + (s1Offset - s0Offset)

                        (Parser parseFollowup) =
                            intermediateToFollowupParser intermediate
                    in
                    parseFollowup
                        (PState s0Src
                            s1Offset
                            s0Indent
                            s0Row
                            s1Column
                        )
                        |> pStepCommit
        )


whileWithoutLinebreakAnd2PartUtf16ValidateMapWithRangeBacktrackableFollowedBySymbol :
    (Range -> String -> res)
    -> (Char -> Bool)
    -> (String -> Bool)
    -> String
    -> Parser res
whileWithoutLinebreakAnd2PartUtf16ValidateMapWithRangeBacktrackableFollowedBySymbol whileRangeAndContentToRes whileCharIsOkay whileResultIsOkay mandatoryFinalSymbol =
    let
        mandatoryFinalSymbolLength : Int
        mandatoryFinalSymbolLength =
            String.length mandatoryFinalSymbol
    in
    Parser
        (\(PState s0Src s0Offset s0Indent s0Row s0Col) ->
            let
                s0OffsetInt : Int
                s0OffsetInt =
                    s0Offset

                s0ColInt : Int
                s0ColInt =
                    s0Col

                s1Offset : Int
                s1Offset =
                    skipWhileWithoutLinebreakAnd2PartUtf16Help
                        whileCharIsOkay
                        s0Offset
                        s0Src

                whileContentLength : Int
                whileContentLength =
                    s1Offset - s0OffsetInt

                whileContentSlice : List Char
                whileContentSlice =
                    List.take
                        whileContentLength
                        (List.drop s0Offset s0Src)

                whileContent : String
                whileContent =
                    String.fromList whileContentSlice

                finalSymbolSlice : List Char
                finalSymbolSlice =
                    List.take
                        mandatoryFinalSymbolLength
                        (List.drop s1Offset s0Src)

                finalSymbolString : String
                finalSymbolString =
                    String.fromList finalSymbolSlice
            in
            if
                (finalSymbolString == (mandatoryFinalSymbol ++ ""))
                    && whileResultIsOkay whileContent
            then
                let
                    s1Column : Int
                    s1Column =
                        s0ColInt + (s1Offset - s0OffsetInt)
                in
                Good
                    (whileRangeAndContentToRes
                        { start = { row = s0Row, column = s0Col }
                        , end = { row = s0Row, column = s1Column }
                        }
                        whileContent
                    )
                    (PState s0Src
                        (s1Offset + mandatoryFinalSymbolLength)
                        s0Indent
                        s0Row
                        (s1Column + mandatoryFinalSymbolLength)
                    )

            else
                Bad False (ExpectingStringSatisfyingPredicate s0Row (s0ColInt + 1))
        )


ifFollowedByWhileValidateMapWithRangeWithoutLinebreak :
    (Range -> String -> res)
    -> (Char -> Bool)
    -> (Char -> Bool)
    -> (String -> Bool)
    -> Parser res
ifFollowedByWhileValidateMapWithRangeWithoutLinebreak toResult firstIsOkay afterFirstIsOkay resultIsOkay =
    Parser
        (\(PState s0Src s0Offset s0Indent s0Row s0Col) ->
            let
                s0OffsetInt : Int
                s0OffsetInt =
                    s0Offset

                s0ColInt : Int
                s0ColInt =
                    s0Col

                firstOffset : Int
                firstOffset =
                    isSubCharWithoutLinebreak firstIsOkay s0Offset s0Src
            in
            if firstOffset == -1 then
                Bad False (ExpectingCharSatisfyingPredicate s0Row s0Col)

            else
                let
                    s1 : State
                    s1 =
                        skipWhileWithoutLinebreakHelp
                            afterFirstIsOkay
                            firstOffset
                            s0Row
                            (s0ColInt + 1)
                            s0Src
                            s0Indent

                    (PState _ s1Offset _ s1Row s1Col) =
                        s1

                    nameSlice : List Char
                    nameSlice =
                        List.take
                            (s1Offset - s0OffsetInt)
                            (List.drop s0OffsetInt s0Src)

                    name : String
                    name =
                        String.fromList nameSlice
                in
                if resultIsOkay name then
                    Good
                        (toResult
                            { start = { row = s0Row, column = s0Col }
                            , end = { row = s1Row, column = s1Col }
                            }
                            name
                        )
                        s1

                else
                    Bad False (ExpectingStringSatisfyingPredicate s0Row (s0ColInt + 1))
        )


ifFollowedByWhileWithoutLinebreak :
    (Char -> Bool)
    -> (Char -> Bool)
    -> Parser String
ifFollowedByWhileWithoutLinebreak firstIsOkay afterFirstIsOkay =
    Parser
        (\(PState sSrc sOffset sIndent sRow sCol) ->
            let
                sOffsetInt : Int
                sOffsetInt =
                    sOffset

                sColInt : Int
                sColInt =
                    sCol

                firstOffset : Int
                firstOffset =
                    isSubCharWithoutLinebreak firstIsOkay sOffset sSrc
            in
            if firstOffset == -1 then
                Bad False (ExpectingCharSatisfyingPredicate sRow sCol)

            else
                let
                    s1 : State
                    s1 =
                        skipWhileWithoutLinebreakHelp
                            afterFirstIsOkay
                            firstOffset
                            sRow
                            (sColInt + 1)
                            sSrc
                            sIndent

                    (PState _ s1Offset _ _ _) =
                        s1

                    nameSlice : List Char
                    nameSlice =
                        List.take
                            (s1Offset - sOffsetInt)
                            (List.drop sOffsetInt sSrc)
                in
                Good (String.fromList nameSlice) s1
        )


ifFollowedByWhileMapWithRangeWithoutLinebreak :
    (Range -> String -> res)
    -> (Char -> Bool)
    -> (Char -> Bool)
    -> Parser res
ifFollowedByWhileMapWithRangeWithoutLinebreak rangeAndConsumedStringToRes firstIsOkay afterFirstIsOkay =
    Parser
        (\(PState s0Src s0Offset s0Indent s0Row s0Col) ->
            let
                s0OffsetInt : Int
                s0OffsetInt =
                    s0Offset

                s0ColInt : Int
                s0ColInt =
                    s0Col

                firstOffset : Int
                firstOffset =
                    isSubCharWithoutLinebreak firstIsOkay s0Offset s0Src
            in
            if firstOffset == -1 then
                Bad False (ExpectingCharSatisfyingPredicate s0Row s0Col)

            else
                let
                    s1 : State
                    s1 =
                        skipWhileWithoutLinebreakHelp
                            afterFirstIsOkay
                            firstOffset
                            s0Row
                            (s0ColInt + 1)
                            s0Src
                            s0Indent

                    (PState _ s1Offset _ s1Row s1Col) =
                        s1

                    s1OffsetInt : Int
                    s1OffsetInt =
                        s1Offset

                    consumedChars : List Char
                    consumedChars =
                        List.take
                            (s1OffsetInt - s0OffsetInt)
                            (List.drop s0OffsetInt s0Src)
                in
                Good
                    (rangeAndConsumedStringToRes
                        { start = { row = s0Row, column = s0Col }
                        , end = { row = s1Row, column = s1Col }
                        }
                        (String.fromList consumedChars)
                    )
                    s1
        )


ifFollowedByWhileMapWithoutLinebreak :
    (String -> res)
    -> (Char -> Bool)
    -> (Char -> Bool)
    -> Parser res
ifFollowedByWhileMapWithoutLinebreak consumedStringToRes firstIsOkay afterFirstIsOkay =
    Parser
        (\(PState s0Src s0Offset s0Indent s0Row s0Col) ->
            let
                s0OffsetInt : Int
                s0OffsetInt =
                    s0Offset

                s0ColInt : Int
                s0ColInt =
                    s0Col

                firstOffset : Int
                firstOffset =
                    isSubCharWithoutLinebreak firstIsOkay s0Offset s0Src
            in
            if firstOffset == -1 then
                Bad False (ExpectingCharSatisfyingPredicate s0Row s0Col)

            else
                let
                    s1 : State
                    s1 =
                        skipWhileWithoutLinebreakHelp
                            afterFirstIsOkay
                            firstOffset
                            s0Row
                            (s0ColInt + 1)
                            s0Src
                            s0Indent

                    (PState _ s1Offset _ _ _) =
                        s1

                    s1OffsetInt : Int
                    s1OffsetInt =
                        s1Offset

                    consumedChars : List Char
                    consumedChars =
                        List.take
                            (s1OffsetInt - s0OffsetInt)
                            (List.drop s0OffsetInt s0Src)

                    consumedString : String
                    consumedString =
                        String.fromList consumedChars
                in
                Good
                    (consumedStringToRes consumedString)
                    s1
        )


{-| Parse multi-line comments that can itself contain other arbitrary multi-comments inside.
-}
nestableMultiCommentMapWithRange : (Range -> String -> res) -> ( Char, String ) -> ( Char, String ) -> Parser res
nestableMultiCommentMapWithRange rangeContentToRes ( openChar, openTail ) ( closeChar, closeTail ) =
    let
        open : String
        open =
            String.cons openChar openTail

        close : String
        close =
            String.cons closeChar closeTail

        isNotRelevant : Char -> Bool
        isNotRelevant =
            charDoesNotEqualAnyOfTwo closeChar openChar
    in
    map2WithRange
        (\range afterOpen contentAfterAfterOpen ->
            rangeContentToRes
                range
                (open ++ afterOpen ++ contentAfterAfterOpen ++ close)
        )
        (symbolFollowedBy open
            (while isNotRelevant)
        )
        (oneOf2
            (symbol close "")
            (loop
                ( "", 1 )
                (oneOf3
                    (symbol close ( close, -1 ))
                    (symbol open ( open, 1 ))
                    (anyCharFollowedByWhileMap (\consumed -> ( consumed, 0 ))
                        isNotRelevant
                    )
                )
                (\( toAppend, nestingChange ) ( soFarContent, soFarNesting ) ->
                    let
                        newNesting : Int
                        newNesting =
                            soFarNesting + nestingChange
                    in
                    if newNesting == 0 then
                        Done soFarContent

                    else
                        Loop ( soFarContent ++ toAppend ++ "", newNesting )
                )
            )
        )


charDoesNotEqualAnyOfTwo : Char -> Char -> Char -> Bool
charDoesNotEqualAnyOfTwo a b char =
    if char == a then
        False

    else
        char /= b


while : (Char -> Bool) -> Parser String
while isGood =
    Parser
        (\(PState s0Src s0Offset s0Indent s0Row s0Col) ->
            let
                s0OffsetInt : Int
                s0OffsetInt =
                    s0Offset

                s1 : State
                s1 =
                    skipWhileHelp isGood s0Offset s0Row s0Col s0Src s0Indent

                (PState _ s1Offset _ _ _) =
                    s1

                s1OffsetInt : Int
                s1OffsetInt =
                    s1Offset

                slice : List Char
                slice =
                    List.take
                        (s1OffsetInt - s0OffsetInt)
                        (List.drop s0Offset s0Src)
            in
            Good
                (String.fromList slice)
                s1
        )


whileWithoutLinebreak : (Char -> Bool) -> Parser String
whileWithoutLinebreak isGood =
    Parser
        (\(PState s0Src s0Offset s0Indent s0Row s0Col) ->
            let
                s0OffsetInt : Int
                s0OffsetInt =
                    s0Offset

                s1 : State
                s1 =
                    skipWhileWithoutLinebreakHelp isGood s0Offset s0Row s0Col s0Src s0Indent

                (PState _ s1Offset _ _ _) =
                    s1

                s1OffsetInt : Int
                s1OffsetInt =
                    s1Offset

                slice : List Char
                slice =
                    List.take
                        (s1OffsetInt - s0OffsetInt)
                        (List.drop s0Offset s0Src)
            in
            Good
                (String.fromList slice)
                s1
        )


anyCharFollowedByWhileMap :
    (String -> res)
    -> (Char -> Bool)
    -> Parser res
anyCharFollowedByWhileMap consumedStringToRes afterFirstIsOkay =
    Parser
        (\(PState sSrc sOffset sIndent sRow sCol) ->
            let
                sOffsetInt : Int
                sOffsetInt =
                    sOffset

                firstOffset : Int
                firstOffset =
                    charOrEnd sOffset sSrc
            in
            if firstOffset == -1 then
                -- end of source
                Bad False (ExpectingAnyChar sRow sCol)

            else
                let
                    s1 : State
                    s1 =
                        if firstOffset == -2 then
                            skipWhileHelp afterFirstIsOkay (sOffsetInt + 1) (sRow + 1) 1 sSrc sIndent

                        else
                            skipWhileHelp afterFirstIsOkay firstOffset sRow (sCol + 1) sSrc sIndent

                    (PState _ s1Offset _ _ _) =
                        s1

                    s1OffsetInt : Int
                    s1OffsetInt =
                        s1Offset

                    slice : List Char
                    slice =
                        List.take
                            (s1OffsetInt - sOffsetInt)
                            (List.drop sOffsetInt sSrc)
                in
                Good (consumedStringToRes (String.fromList slice)) s1
        )


{-| Decide what steps to take next in your `loop`.

If you are `Done`, you give the result of the whole `loop`. If you decide to
`Loop` around again, you give a new state to work from. Maybe you need to add
an item to a list? Or maybe you need to track some information about what you
just saw?

**Note:** It may be helpful to learn about [finite-state machines][fsm] to get
a broader intuition about using `state`. I.e. You may want to create a `type`
that describes four possible states, and then use `Loop` to transition between
them as you consume characters.

[fsm]: https://en.wikipedia.org/wiki/Finite-state_machine

-}
type Step state a
    = Loop state
    | Done a


{-| A parser that can loop indefinitely. This can be helpful when parsing
repeated structures, like a bunch of statements:


    statements : Parser (List Statement)
    statements =
        loop maybeStatementSemicolonWhitespace
            []
            (\step soFar ->
                case step of
                    Just lastStatement ->
                        Loop (lastStatement :: soFar)

                    Nothing ->
                        Done (List.reverse soFar)
            )

    maybeStatementSemicolonWhitespace : Maybe Statement
    maybeStatementSemicolonWhitespace =
        orSucceed
            (ParserFast.map Just
                (statement
                    |> ParserFast.followedBySkipWhileWhitespace
                    |> ParserFast.followedBySymbol ";"
                    |> ParserFast.followedBySkipWhileWhitespace
                )
            )
            Nothing

    -- statement : Parser Statement

Notice that the statements are tracked in reverse as we `Loop`, and we reorder
them only once we are `Done`. This is a very common pattern with `loop`!

**IMPORTANT NOTE:** Parsers like `while Char.isAlpha` can
succeed without consuming any characters. So in some cases you may want to e.g.
use an [`ifFollowedByWhileWithoutLinebreak`](#ifFollowedByWhileWithoutLinebreak) to ensure that each step actually consumed characters.
Otherwise you could end up in an infinite loop!

You very likely don't need to keep track of specific state before deciding on how
to continue, so I recommend using one of the loop- helpers instead.

-}
loop : state -> Parser extension -> (extension -> state -> Step state a) -> Parser a
loop state element reduce =
    Parser
        (\s -> loopHelp False state element reduce s)


loopHelp : Bool -> state -> Parser extension -> (extension -> state -> Step state a) -> State -> PStep a
loopHelp committedSoFar state ((Parser parseElement) as element) reduce s0 =
    case parseElement s0 of
        Good step s1 ->
            case reduce step state of
                Loop newState ->
                    loopHelp True newState element reduce s1

                Done result ->
                    Good result s1

        Bad elementCommitted x ->
            Bad (committedSoFar || elementCommitted) x


{-| When parsing, you want to allocate as little as possible.
So this function lets you say:

    isSubCharWithoutLinebreak isSpace offset "this is the source string"
        --==> newOffset

The `(Char -> Bool)` argument is called a predicate.
The `newOffset` value can be a few different things:

  - `-1` means that the predicate failed
  - otherwise you will get `offset + 1` or `offset + 2`
    depending on whether the UTF16 character is one or two
    words wide.

-}
isSubCharWithoutLinebreak : (Char -> Bool) -> Int -> List Char -> Int
isSubCharWithoutLinebreak predicate offset string =
    case List.take 1 (List.drop offset string) of
        actualChar :: _ ->
            if predicate actualChar then
                offset + 1

            else
                -1

        [] ->
            -1


stringSlice : Int -> Int -> List Char -> List Char
stringSlice start end list =
    List.take
        (end - start)
        (List.drop start list)
