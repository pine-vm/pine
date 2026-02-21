module Parser.Advanced exposing
    ( Parser, run, DeadEnd, inContext, Token(..)
    , int, float, number, symbol, keyword, variable, end
    , succeed, (|=), (|.), lazy, andThen, problem
    , oneOf, map, backtrackable, commit, token
    , sequence, Trailing(..), loop, Step(..)
    , spaces, lineComment, multiComment, Nestable(..)
    , getChompedString, chompIf, chompWhile, chompUntil, chompUntilEndOr, mapChompedString
    , withIndent, getIndent
    , getPosition, getRow, getCol, getOffset, getSource
    , changeIndent, ignorer, keeper
    )

{-|


# Parsers

@docs Parser, run, DeadEnd, inContext, Token


# Building Blocks

@docs int, float, number, symbol, keyword, variable, end


# Pipelines

@docs succeed, (|=), (|.), lazy, andThen, problem


# Branches

@docs oneOf, map, backtrackable, commit, token


# Loops

@docs sequence, Trailing, loop, Step


# Whitespace

@docs spaces, lineComment, multiComment, Nestable


# Chompers

@docs getChompedString, chompIf, chompWhile, chompUntil, chompUntilEndOr, mapChompedString


# Indentation

@docs withIndent, getIndent


# Positions

@docs getPosition, getRow, getCol, getOffset, getSource

-}

import Elm.Kernel.Parser
import Set



-- INFIX OPERATORS


infix left  5 (|=) = keeper
infix left  6 (|.) = ignorer



{- NOTE: the (|.) oporator binds tighter to slightly reduce the amount
   of recursion in pipelines. For example:

       func
         |. a
         |. b
         |= c
         |. d
         |. e

   With the same precedence:

       (ignorer (ignorer (keeper (ignorer (ignorer func a) b) c) d) e)

   With higher precedence:

       keeper (ignorer (ignorer func a) b) (ignorer (ignorer c d) e)

   So the maximum call depth goes from 5 to 3.
-}
-- PARSERS


{-| An advanced `Parser` gives two ways to improve your error messages:

  - `problem` &mdash; Instead of all errors being a `String`, you can create a
    custom type like `type Problem = BadIndent | BadKeyword String` and track
    problems much more precisely.
  - `context` &mdash; Error messages can be further improved when precise
    problems are paired with information about where you ran into trouble. By
    tracking the context, instead of saying “I found a bad keyword” you can say
    “I found a bad keyword when parsing a list” and give folks a better idea of
    what the parser thinks it is doing.

I recommend starting with the simpler [`Parser`][parser] module though, and
when you feel comfortable and want better error messages, you can create a type
alias like this:

    import Parser.Advanced

    type alias MyParser a =
        Parser.Advanced.Parser Context Problem a

    type Context
        = Definition String
        | List
        | Record

    type Problem
        = BadIndent
        | BadKeyword String

All of the functions from `Parser` should exist in `Parser.Advanced` in some
form, allowing you to switch over pretty easily.

[parser]: /packages/elm/parser/latest/Parser

-}
type Parser context problem value
    = Parser (State context -> PStep context problem value)


type String
    = String Int
      -- We need another tag to prevent the compiler from assuming that the condition for tag 'String' is always true.
    | AnyOtherKind_String


type PStep context problem value
    = Good Bool value (State context)
    | Bad Bool (Bag context problem)


type State context
    = PState Int Int Int (List (Located context)) Int Int


type alias Located context =
    { row : Int
    , col : Int
    , context : context
    }



-- RUN


{-| This works just like [`Parser.run`](/packages/elm/parser/latest/Parser#run).
The only difference is that when it fails, it has much more precise information
for each dead end.
-}
run : Parser c x a -> String -> Result (List (DeadEnd c x)) a
run (Parser parse) (String srcBytes) =
    case
        parse
            (PState
                srcBytes
                0
                1
                []
                1
                1
            )
    of
        Good _ value _ ->
            Ok value

        Bad _ bag ->
            Err (bagToList bag [])



-- PROBLEMS


{-| Say you are parsing a function named `viewHealthData` that contains a list.
You might get a `DeadEnd` like this:

    { row = 18
    , col = 22
    , problem = UnexpectedComma
    , contextStack =
        [ { row = 14
          , col = 1
          , context = Definition "viewHealthData"
          }
        , { row = 15
          , col = 4
          , context = List
          }
        ]
    }

We have a ton of information here! So in the error message, we can say that “I
ran into an issue when parsing a list in the definition of `viewHealthData`. It
looks like there is an extra comma.” Or maybe something even better!

Furthermore, many parsers just put a mark where the problem manifested. By
tracking the `row` and `col` of the context, we can show a much larger region
as a way of indicating “I thought I was parsing this thing that starts over
here.” Otherwise you can get very confusing error messages on a missing `]` or
`}` or `)` because “I need more indentation” on something unrelated.

**Note:** Rows and columns are counted like a text editor. The beginning is `row=1`
and `col=1`. The `col` increments as characters are chomped. When a `\n` is chomped,
`row` is incremented and `col` starts over again at `1`.

-}
type alias DeadEnd context problem =
    { row : Int
    , col : Int
    , problem : problem
    , contextStack : List { row : Int, col : Int, context : context }
    }


type Bag c x
    = Empty
    | AddRight (Bag c x) (DeadEnd c x)
    | Append (Bag c x) (Bag c x)


fromState : State c -> x -> Bag c x
fromState (PState srcChars offset indent context row col) x =
    AddRight Empty (DeadEnd row col x context)


fromInfo : Int -> Int -> x -> List (Located c) -> Bag c x
fromInfo row col x context =
    AddRight Empty (DeadEnd row col x context)


bagToList : Bag c x -> List (DeadEnd c x) -> List (DeadEnd c x)
bagToList bag list =
    case bag of
        Empty ->
            list

        AddRight bag1 x ->
            bagToList bag1 (x :: list)

        Append bag1 bag2 ->
            bagToList bag1 (bagToList bag2 list)



-- PRIMITIVES


{-| Just like [`Parser.succeed`](Parser#succeed)
-}
succeed : a -> Parser c x a
succeed a =
    Parser
        (\s ->
            Good False a s
        )


{-| Just like [`Parser.problem`](Parser#problem) except you provide a custom
type for your problem.
-}
problem : x -> Parser c x a
problem x =
    Parser
        (\s ->
            Bad False (fromState s x)
        )



-- MAPPING


{-| Just like [`Parser.map`](Parser#map)
-}
map : (a -> b) -> Parser c x a -> Parser c x b
map func (Parser parse) =
    Parser
        (\s0 ->
            case parse s0 of
                Good p a s1 ->
                    Good p (func a) s1

                Bad p x ->
                    Bad p x
        )


map2 : (a -> b -> value) -> Parser c x a -> Parser c x b -> Parser c x value
map2 func (Parser parseA) (Parser parseB) =
    Parser
        (\s0 ->
            case parseA s0 of
                Bad p x ->
                    Bad p x

                Good p1 a s1 ->
                    case parseB s1 of
                        Bad p2 x ->
                            Bad (p1 || p2) x

                        Good p2 b s2 ->
                            Good (p1 || p2) (func a b) s2
        )


{-| Just like the [`(|=)`](Parser#|=) from the `Parser` module.
-}
keeper : Parser c x (a -> b) -> Parser c x a -> Parser c x b
keeper parseFunc parseArg =
    map2 (<|) parseFunc parseArg


{-| Just like the [`(|.)`](Parser#|.) from the `Parser` module.
-}
ignorer : Parser c x keep -> Parser c x ignore -> Parser c x keep
ignorer keepParser ignoreParser =
    map2 always keepParser ignoreParser



-- AND THEN


{-| Just like [`Parser.andThen`](Parser#andThen)
-}
andThen : (a -> Parser c x b) -> Parser c x a -> Parser c x b
andThen callback (Parser parseA) =
    Parser
        (\s0 ->
            case parseA s0 of
                Bad p x ->
                    Bad p x

                Good p1 a s1 ->
                    let
                        (Parser parseB) =
                            callback a
                    in
                    case parseB s1 of
                        Bad p2 x ->
                            Bad (p1 || p2) x

                        Good p2 b s2 ->
                            Good (p1 || p2) b s2
        )



-- LAZY


{-| Just like [`Parser.lazy`](Parser#lazy)
-}
lazy : (() -> Parser c x a) -> Parser c x a
lazy thunk =
    Parser
        (\s ->
            let
                (Parser parse) =
                    thunk ()
            in
            parse s
        )



-- ONE OF


{-| Just like [`Parser.oneOf`](Parser#oneOf)
-}
oneOf : List (Parser c x a) -> Parser c x a
oneOf parsers =
    Parser
        (\s -> oneOfHelp s Empty parsers)


oneOfHelp : State c -> Bag c x -> List (Parser c x a) -> PStep c x a
oneOfHelp s0 bag parsers =
    case parsers of
        [] ->
            Bad False bag

        (Parser parse) :: remainingParsers ->
            case parse s0 of
                (Good _ _ _) as step ->
                    step

                (Bad p x) as step ->
                    if p then
                        step

                    else
                        oneOfHelp s0 (Append bag x) remainingParsers



-- LOOP


{-| Just like [`Parser.Step`](Parser#Step)
-}
type Step state a
    = Loop state
    | Done a


{-| Just like [`Parser.loop`](Parser#loop)
-}
loop : state -> (state -> Parser c x (Step state a)) -> Parser c x a
loop state callback =
    Parser
        (\s ->
            loopHelp False state callback s
        )


loopHelp : Bool -> state -> (state -> Parser c x (Step state a)) -> State c -> PStep c x a
loopHelp p state callback s0 =
    let
        (Parser parse) =
            callback state
    in
    case parse s0 of
        Good p1 step s1 ->
            case step of
                Loop newState ->
                    loopHelp (p || p1) newState callback s1

                Done result ->
                    Good (p || p1) result s1

        Bad p1 x ->
            Bad (p || p1) x



-- BACKTRACKABLE


{-| Just like [`Parser.backtrackable`](Parser#backtrackable)
-}
backtrackable : Parser c x a -> Parser c x a
backtrackable (Parser parse) =
    Parser
        (\s0 ->
            case parse s0 of
                Bad _ x ->
                    Bad False x

                Good _ a s1 ->
                    Good False a s1
        )


{-| Just like [`Parser.commit`](Parser#commit)
-}
commit : a -> Parser c x a
commit a =
    Parser (\s -> Good True a s)



-- SYMBOL


{-| Just like [`Parser.symbol`](Parser#symbol) except you provide a `Token` to
clearly indicate your custom type of problems:

    comma : Parser Context Problem ()
    comma =
        symbol (Token "," ExpectingComma)

-}
symbol : Token x -> Parser c x ()
symbol t =
    token t



-- KEYWORD


{-| Just like [`Parser.keyword`](Parser#keyword) except you provide a `Token`
to clearly indicate your custom type of problems:

    let_ : Parser Context Problem ()
    let_ =
        symbol (Token "let" ExpectingLet)

Note that this would fail to chomp `letter` because of the subsequent
characters. Use `token` if you do not want that last letter check.

-}
keyword : Token x -> Parser c x ()
keyword (Token kwd expecting) =
    let
        progress =
            kwd /= ""
    in
    Parser
        (\s ->
            let
                (PState srcBytes sOffset sIndent sContext sRow sCol) =
                    s

                ( newOffset, newRow, newCol ) =
                    Elm.Kernel.Parser.isSubString kwd sOffset sRow sCol srcBytes
            in
            if Pine_kernel.equal [ newOffset, -1 ] then
                Bad False (fromState s expecting)

            else if
                Pine_kernel.int_is_sorted_asc
                    [ 0
                    , isSubChar
                        (\c ->
                            Char.isAlphaNum c || Pine_kernel.equal [ c, '_' ]
                        )
                        newOffset
                        srcBytes
                    ]
            then
                Bad False (fromState s expecting)

            else
                Good progress
                    ()
                    (PState
                        srcBytes
                        newOffset
                        sIndent
                        sContext
                        newRow
                        newCol
                    )
        )



-- TOKEN


{-| With the simpler `Parser` module, you could just say `symbol ","` and
parse all the commas you wanted. But now that we have a custom type for our
problems, we actually have to specify that as well. So anywhere you just used
a `String` in the simpler module, you now use a `Token Problem` in the advanced
module:

    type Problem
        = ExpectingComma
        | ExpectingListEnd

    comma : Token Problem
    comma =
        Token "," ExpectingComma

    listEnd : Token Problem
    listEnd =
        Token "]" ExpectingListEnd

You can be creative with your custom type. Maybe you want a lot of detail.
Maybe you want looser categories. It is a custom type. Do what makes sense for
you!

-}
type Token x
    = Token String x


{-| Just like [`Parser.token`](Parser#token) except you provide a `Token`
specifying your custom type of problems.
-}
token : Token x -> Parser c x ()
token (Token str expecting) =
    let
        progress =
            str /= ""
    in
    Parser
        (\s ->
            let
                (PState srcBytes sOffset sIndent sContext sRow sCol) =
                    s

                ( newOffset, newRow, newCol ) =
                    Elm.Kernel.Parser.isSubString str sOffset sRow sCol srcBytes
            in
            if Pine_kernel.equal [ newOffset, -1 ] then
                Bad False (fromState s expecting)

            else
                Good progress
                    ()
                    (PState
                        srcBytes
                        newOffset
                        sIndent
                        sContext
                        newRow
                        newCol
                    )
        )



-- INT


{-| Just like [`Parser.int`](Parser#int) where you have to handle negation
yourself. The only difference is that you provide a two potential problems:

    int : x -> x -> Parser c x Int
    int expecting invalid =
        number
            { int = Ok identity
            , hex = Err invalid
            , octal = Err invalid
            , binary = Err invalid
            , float = Err invalid
            , invalid = invalid
            , expecting = expecting
            }

You can use problems like `ExpectingInt` and `InvalidNumber`.

-}
int : x -> x -> Parser c x Int
int expecting invalid =
    number
        { int = Ok identity
        , hex = Err invalid
        , octal = Err invalid
        , binary = Err invalid
        , float = Err invalid
        , invalid = invalid
        , expecting = expecting
        }



-- FLOAT


{-| Just like [`Parser.float`](Parser#float) where you have to handle negation
yourself. The only difference is that you provide a two potential problems:

    float : x -> x -> Parser c x Float
    float expecting invalid =
        number
            { int = Ok toFloat
            , hex = Err invalid
            , octal = Err invalid
            , binary = Err invalid
            , float = Ok identity
            , invalid = invalid
            , expecting = expecting
            }

You can use problems like `ExpectingFloat` and `InvalidNumber`.

-}
float : x -> x -> Parser c x Float
float expecting invalid =
    number
        { int = Ok toFloat
        , hex = Err invalid
        , octal = Err invalid
        , binary = Err invalid
        , float = Ok identity
        , invalid = invalid
        , expecting = expecting
        }



-- NUMBER


{-| Just like [`Parser.number`](Parser#number) where you have to handle
negation yourself. The only difference is that you provide all the potential
problems.
-}
number :
    { int : Result x (Int -> a)
    , hex : Result x (Int -> a)
    , octal : Result x (Int -> a)
    , binary : Result x (Int -> a)
    , float : Result x (Float -> a)
    , invalid : x
    , expecting : x
    }
    -> Parser c x a
number c =
    Parser
        (\s ->
            let
                (PState srcBytes sOffset sIndent sContext sRow sCol) =
                    s

                firstChar =
                    Pine_kernel.take [ 4, Pine_kernel.skip [ sOffset, srcBytes ] ]
            in
            if Pine_kernel.equal [ firstChar, '0' ] then
                let
                    zeroOffset =
                        Pine_kernel.int_add [ sOffset, 4 ]

                    secondChar =
                        Pine_kernel.take [ 4, Pine_kernel.skip [ zeroOffset, srcBytes ] ]

                    baseOffset =
                        Pine_kernel.int_add [ zeroOffset, 4 ]
                in
                if Pine_kernel.equal [ secondChar, 'x' ] then
                    finalizeInt c.invalid c.hex baseOffset (consumeBase16 baseOffset srcBytes) s

                else if Pine_kernel.equal [ secondChar, 'o' ] then
                    finalizeInt c.invalid c.octal baseOffset (consumeBase 8 baseOffset srcBytes) s

                else if Pine_kernel.equal [ secondChar, 'b' ] then
                    finalizeInt c.invalid c.binary baseOffset (consumeBase 2 baseOffset srcBytes) s

                else
                    finalizeFloat c.invalid c.expecting c.int c.float ( zeroOffset, 0 ) s

            else
                finalizeFloat c.invalid c.expecting c.int c.float (consumeBase 10 sOffset srcBytes) s
        )


consumeBase : Int -> Int -> Int -> ( Int, Int )
consumeBase base offset stringBytes =
    Elm.Kernel.Parser.consumeBase base offset stringBytes


consumeBase16 : Int -> Int -> ( Int, Int )
consumeBase16 offset stringBytes =
    Elm.Kernel.Parser.consumeBase16 offset stringBytes


finalizeInt : x -> Result x (Int -> a) -> Int -> ( Int, Int ) -> State c -> PStep c x a
finalizeInt invalid handler startOffset ( endOffset, n ) s =
    case handler of
        Err x ->
            Bad True (fromState s x)

        Ok toValue ->
            let
                (PState srcBytes sOffset sIndent sContext sRow sCol) =
                    s
            in
            if Pine_kernel.equal [ startOffset, endOffset ] then
                Bad
                    (Pine_kernel.negate
                        (Pine_kernel.int_is_sorted_asc [ startOffset, sOffset ])
                    )
                    (fromState s invalid)

            else
                Good True (toValue n) (bumpOffset endOffset s)


bumpOffset : Int -> State c -> State c
bumpOffset newOffset (PState srcBytes offset indent context row col) =
    let
        bytesDelta : Int
        bytesDelta =
            Pine_kernel.int_add [ newOffset, Pine_kernel.negate offset ]

        charsDelta : Int
        charsDelta =
            Pine_kernel.concat
                [ Pine_kernel.take [ 1, 0 ]
                , Pine_kernel.bit_shift_right
                    [ 2
                    , Pine_kernel.skip [ 1, bytesDelta ]
                    ]
                ]
    in
    PState
        srcBytes
        newOffset
        indent
        context
        row
        (Pine_kernel.int_add [ col, charsDelta ])


finalizeFloat : x -> x -> Result x (Int -> a) -> Result x (Float -> a) -> ( Int, Int ) -> State c -> PStep c x a
finalizeFloat invalid expecting intSettings floatSettings intPair s =
    let
        (PState srcBytes sOffset sIndent sContext sRow sCol) =
            s

        ( intOffset, _ ) =
            intPair

        floatOffset =
            consumeDotAndExp intOffset srcBytes
    in
    if Pine_kernel.int_is_sorted_asc [ 0, floatOffset ] then
        if Pine_kernel.equal [ sOffset, floatOffset ] then
            Bad False (fromState s expecting)

        else if Pine_kernel.equal [ intOffset, floatOffset ] then
            finalizeInt invalid intSettings sOffset intPair s

        else
            case floatSettings of
                Err x ->
                    Bad True (fromState s invalid)

                Ok toValue ->
                    let
                        sliceLength : Int
                        sliceLength =
                            Pine_kernel.int_add [ floatOffset, Pine_kernel.negate sOffset ]

                        sliceBytes : Int
                        sliceBytes =
                            Pine_kernel.take
                                [ sliceLength
                                , Pine_kernel.skip
                                    [ sOffset
                                    , srcBytes
                                    ]
                                ]
                    in
                    case String.toFloat (String.String sliceBytes) of
                        Nothing ->
                            Bad True (fromState s invalid)

                        Just n ->
                            Good True (toValue n) (bumpOffset floatOffset s)

    else
        Bad True
            (fromInfo
                sRow
                (let
                    tempBytes =
                        Pine_kernel.int_add [ floatOffset, sOffset ]

                    tempChars =
                        Pine_kernel.concat
                            [ Pine_kernel.take [ 1, 0 ]
                            , Pine_kernel.bit_shift_right [ 2, Pine_kernel.skip [ 1, tempBytes ] ]
                            ]
                 in
                 Pine_kernel.int_add [ sCol, Pine_kernel.negate tempChars ]
                )
                invalid
                sContext
            )



--
-- On a failure, returns negative index of problem.
--


consumeDotAndExp : Int -> Int -> Int
consumeDotAndExp offset charsBytes =
    if Pine_kernel.equal [ Pine_kernel.take [ 4, Pine_kernel.skip [ offset, charsBytes ] ], '.' ] then
        consumeExp
            (Elm.Kernel.Parser.chompBase10
                (Pine_kernel.int_add [ offset, 4 ])
                charsBytes
            )
            charsBytes

    else
        consumeExp offset charsBytes



--
-- On a failure, returns negative index of problem.
--


consumeExp : Int -> Int -> Int
consumeExp offset charsBytes =
    let
        nextChar =
            Pine_kernel.take
                [ 4
                , Pine_kernel.skip [ offset, charsBytes ]
                ]
    in
    if Pine_kernel.equal [ nextChar, 'e' ] || Pine_kernel.equal [ nextChar, 'E' ] then
        let
            eOffset : Int
            eOffset =
                Pine_kernel.int_add [ offset, 4 ]

            charAfterE =
                Pine_kernel.take
                    [ 4
                    , Pine_kernel.skip [ eOffset, charsBytes ]
                    ]

            expOffset : Int
            expOffset =
                if Pine_kernel.equal [ charAfterE, '+' ] || Pine_kernel.equal [ charAfterE, '-' ] then
                    Pine_kernel.int_add [ eOffset, 4 ]

                else
                    eOffset

            newOffset : Int
            newOffset =
                Elm.Kernel.Parser.chompBase10 expOffset charsBytes
        in
        if Pine_kernel.equal [ expOffset, newOffset ] then
            Pine_kernel.negate newOffset

        else
            newOffset

    else
        offset



-- END


{-| Just like [`Parser.end`](Parser#end) except you provide the problem that
arises when the parser is not at the end of the input.
-}
end : x -> Parser c x ()
end x =
    Parser
        (\s ->
            let
                (PState srcBytes sOffset sIndent sContext sRow sCol) =
                    s
            in
            if Pine_kernel.equal [ Pine_kernel.length srcBytes, sOffset ] then
                Good False () s

            else
                Bad False (fromState s x)
        )



-- CHOMPED STRINGS


{-| Just like [`Parser.getChompedString`](Parser#getChompedString)
-}
getChompedString : Parser c x a -> Parser c x String
getChompedString parser =
    mapChompedString always parser


{-| Just like [`Parser.mapChompedString`](Parser#mapChompedString)
-}
mapChompedString : (String -> a -> b) -> Parser c x a -> Parser c x b
mapChompedString func (Parser parse) =
    Parser
        (\s0 ->
            case parse s0 of
                Bad p x ->
                    Bad p x

                Good p a s1 ->
                    let
                        (PState srcBytes sOffset sIndent sContext sRow sCol) =
                            s0

                        (PState _ s1Offset _ _ _ _) =
                            s1

                        sliceLength : Int
                        sliceLength =
                            Pine_kernel.int_add
                                [ s1Offset
                                , Pine_kernel.negate sOffset
                                ]

                        sliceBytes : Int
                        sliceBytes =
                            Pine_kernel.take
                                [ sliceLength
                                , Pine_kernel.skip
                                    [ sOffset
                                    , srcBytes
                                    ]
                                ]
                    in
                    Good p (func (String sliceBytes) a) s1
        )



-- CHOMP IF


{-| Just like [`Parser.chompIf`](Parser#chompIf) except you provide a problem
in case a character cannot be chomped.
-}
chompIf : (Char -> Bool) -> x -> Parser c x ()
chompIf isGood expecting =
    Parser
        (\s ->
            let
                (PState srcBytes sOffset sIndent sContext sRow sCol) =
                    s

                newOffset =
                    isSubChar isGood sOffset srcBytes
            in
            -- not found
            if Pine_kernel.equal [ newOffset, -1 ] then
                Bad False (fromState s expecting)
                -- newline

            else if Pine_kernel.equal [ newOffset, -2 ] then
                Good True
                    ()
                    (PState
                        srcBytes
                        (Pine_kernel.int_add [ sOffset, 4 ])
                        sIndent
                        sContext
                        (Pine_kernel.int_add [ sRow, 1 ])
                        1
                    )
                -- found

            else
                Good True
                    ()
                    (PState
                        srcBytes
                        newOffset
                        sIndent
                        sContext
                        sRow
                        (Pine_kernel.int_add [ sCol, 1 ])
                    )
        )



-- CHOMP WHILE


{-| Just like [`Parser.chompWhile`](Parser#chompWhile)
-}
chompWhile : (Char -> Bool) -> Parser c x ()
chompWhile isGood =
    Parser
        (\s ->
            let
                (PState srcBytes sOffset sIndent sContext sRow sCol) =
                    s

                ( newOffset, newRow, newCol ) =
                    Elm.Kernel.Parser.chompWhileHelp
                        isGood
                        ( sOffset, sRow, sCol )
                        srcBytes
            in
            Good
                (Pine_kernel.negate
                    (Pine_kernel.int_is_sorted_asc [ newOffset, sOffset ])
                )
                ()
                (PState
                    srcBytes
                    newOffset
                    sIndent
                    sContext
                    newRow
                    newCol
                )
        )



-- CHOMP UNTIL


{-| Just like [`Parser.chompUntil`](Parser#chompUntil) except you provide a
`Token` in case you chomp all the way to the end of the input without finding
what you need.
-}
chompUntil : Token x -> Parser c x ()
chompUntil (Token str expecting) =
    Parser
        (\s ->
            let
                (PState srcBytes sOffset sIndent sContext sRow sCol) =
                    s

                ( newOffset, newRow, newCol ) =
                    Elm.Kernel.Parser.findSubString str sOffset sRow sCol srcBytes
            in
            if Pine_kernel.equal [ newOffset, -1 ] then
                Bad False (fromInfo newRow newCol expecting sContext)

            else
                Good
                    (Pine_kernel.negate
                        (Pine_kernel.int_is_sorted_asc [ newOffset, sOffset ])
                    )
                    ()
                    (PState
                        srcBytes
                        newOffset
                        sIndent
                        sContext
                        newRow
                        newCol
                    )
        )


{-| Just like [`Parser.chompUntilEndOr`](Parser#chompUntilEndOr)
-}
chompUntilEndOr : String -> Parser c x ()
chompUntilEndOr str =
    Parser
        (\s ->
            let
                (PState srcBytes sOffset sIndent sContext sRow sCol) =
                    s

                ( newOffset, newRow, newCol ) =
                    Elm.Kernel.Parser.findSubString str sOffset sRow sCol srcBytes

                adjustedOffset : Int
                adjustedOffset =
                    if Pine_kernel.int_is_sorted_asc [ 0, newOffset ] then
                        newOffset

                    else
                        Pine_kernel.length srcBytes
            in
            Good (Pine_kernel.negate (Pine_kernel.int_is_sorted_asc [ adjustedOffset, sOffset ]))
                ()
                (PState
                    srcBytes
                    adjustedOffset
                    sIndent
                    sContext
                    newRow
                    newCol
                )
        )



-- CONTEXT


{-| This is how you mark that you are in a certain context. For example, here
is a rough outline of some code that uses `inContext` to mark when you are
parsing a specific definition:

    import Char
    import Parser.Advanced exposing (..)
    import Set

    type Context
        = Definition String
        | List

    definition : Parser Context Problem Expr
    definition =
        functionName
            |> andThen definitionBody

    definitionBody : String -> Parser Context Problem Expr
    definitionBody name =
        inContext (Definition name) <|
            succeed (Function name)
                |= arguments
                |. symbol (Token "=" ExpectingEquals)
                |= expression

    functionName : Parser c Problem String
    functionName =
        variable
            { start = Char.isLower
            , inner = Char.isAlphaNum
            , reserved = Set.fromList [ "let", "in" ]
            , expecting = ExpectingFunctionName
            }

First we parse the function name, and then we parse the rest of the definition.
Importantly, we call `inContext` so that any dead end that occurs in
`definitionBody` will get this extra context information. That way you can say
things like, “I was expecting an equals sign in the `view` definition.” Context!

-}
inContext : context -> Parser context x a -> Parser context x a
inContext context (Parser parse) =
    Parser
        (\s0 ->
            let
                (PState srcBytes offset indent sContext row col) =
                    s0
            in
            case parse (changeContext (Located row col context :: sContext) s0) of
                Good p a s1 ->
                    Good p a (changeContext sContext s1)

                (Bad _ _) as step ->
                    step
        )


changeContext : List (Located c) -> State c -> State c
changeContext newContext (PState srcBytes offset indent context row col) =
    PState srcBytes offset indent newContext row col



-- INDENTATION


{-| Just like [`Parser.getIndent`](Parser#getIndent)
-}
getIndent : Parser c x Int
getIndent =
    Parser (\s -> Good False s.indent s)


{-| Just like [`Parser.withIndent`](Parser#withIndent)
-}
withIndent : Int -> Parser c x a -> Parser c x a
withIndent newIndent (Parser parse) =
    Parser
        (\s0 ->
            let
                (PState srcBytes offset s0Indent context row col) =
                    s0
            in
            case parse (changeIndent newIndent s0) of
                Good p a s1 ->
                    Good p a (changeIndent s0Indent s1)

                Bad p x ->
                    Bad p x
        )


changeIndent : Int -> State c -> State c
changeIndent newIndent (PState srcBytes offset indent context row col) =
    PState srcBytes offset newIndent context row col



-- POSITION


{-| Just like [`Parser.getPosition`](Parser#getPosition)
-}
getPosition : Parser c x ( Int, Int )
getPosition =
    Parser
        (\s ->
            let
                (PState srcBytes offset indent context row col) =
                    s
            in
            Good False ( row, col ) s
        )


{-| Just like [`Parser.getRow`](Parser#getRow)
-}
getRow : Parser c x Int
getRow =
    Parser
        (\s ->
            let
                (PState srcBytes offset indent context row col) =
                    s
            in
            Good False row s
        )


{-| Just like [`Parser.getCol`](Parser#getCol)
-}
getCol : Parser c x Int
getCol =
    Parser
        (\s ->
            let
                (PState srcBytes offset indent context row col) =
                    s
            in
            Good False col s
        )


{-| Just like [`Parser.getOffset`](Parser#getOffset)
-}
getOffset : Parser c x Int
getOffset =
    Parser
        (\s ->
            let
                (PState srcBytes offset indent context row col) =
                    s
            in
            Good False offset s
        )


{-| Just like [`Parser.getSource`](Parser#getSource)
-}
getSource : Parser c x String
getSource =
    Parser
        (\s ->
            let
                (PState srcBytes offset indent context row col) =
                    s
            in
            Good False (String srcBytes) s
        )


{-| Again, when parsing, you want to allocate as little as possible.
So this function lets you say:

    isSubChar isSpace offset "this is the source string"
        --==> newOffset

The `(Char -> Bool)` argument is called a predicate.
The `newOffset` value can be a few different things:

  - `-1` means that the predicate failed
  - `-2` means the predicate succeeded with a `\n`
  - otherwise you will get `offset + 1` or `offset + 2`
    depending on whether the UTF16 character is one or two
    words wide.

-}
isSubChar : (Char -> Bool) -> Int -> Int -> Int
isSubChar =
    Elm.Kernel.Parser.isSubChar



-- VARIABLES


{-| Just like [`Parser.variable`](Parser#variable) except you specify the
problem yourself.
-}
variable :
    { start : Char -> Bool
    , inner : Char -> Bool
    , reserved : Set.Set String
    , expecting : x
    }
    -> Parser c x String
variable i =
    Parser
        (\s ->
            let
                (PState srcBytes sOffset indent context row col) =
                    s

                firstChar =
                    Pine_kernel.take
                        [ 4
                        , Pine_kernel.skip [ sOffset, srcBytes ]
                        ]
            in
            {-
               First check if we have reached the end of the source string, to account for the possibility of
               a predicate for i.start crashing when given an empty list.
            -}
            if Pine_kernel.equal [ Pine_kernel.length firstChar, 0 ] then
                Bad False (fromState s i.expecting)

            else if i.start firstChar then
                let
                    s1 =
                        if Pine_kernel.equal [ firstChar, '\n' ] then
                            varHelp
                                i.inner
                                (Pine_kernel.int_add [ sOffset, 4 ])
                                (Pine_kernel.int_add [ row, 1 ])
                                1
                                srcBytes
                                indent
                                context

                        else
                            varHelp
                                i.inner
                                (Pine_kernel.int_add [ sOffset, 4 ])
                                row
                                (Pine_kernel.int_add [ col, 1 ])
                                srcBytes
                                indent
                                context

                    (PState _ s1Offset _ _ _ _) =
                        s1

                    sliceLength : Int
                    sliceLength =
                        Pine_kernel.int_add
                            [ s1Offset
                            , Pine_kernel.negate sOffset
                            ]

                    nameBytes : Int
                    nameBytes =
                        Pine_kernel.take
                            [ sliceLength
                            , Pine_kernel.skip [ sOffset, srcBytes ]
                            ]

                    name : String
                    name =
                        String nameBytes
                in
                if Set.member name i.reserved then
                    Bad False (fromState s i.expecting)

                else
                    Good True name s1

            else
                Bad False (fromState s i.expecting)
        )


varHelp : (Char -> Bool) -> Int -> Int -> Int -> Int -> Int -> List (Located c) -> State c
varHelp isGood offset row col srcBytes indent context =
    let
        ( newOffset, newRow, newCol ) =
            Elm.Kernel.Parser.chompWhileHelp isGood ( offset, row, col ) srcBytes
    in
    PState
        srcBytes
        newOffset
        indent
        context
        newRow
        newCol



-- SEQUENCES


{-| Just like [`Parser.sequence`](Parser#sequence) except with a `Token` for
the start, separator, and end. That way you can specify your custom type of
problem for when something is not found.
-}
sequence :
    { start : Token x
    , separator : Token x
    , end : Token x
    , spaces : Parser c x ()
    , item : Parser c x a
    , trailing : Trailing
    }
    -> Parser c x (List a)
sequence i =
    skip (token i.start)
        (skip i.spaces
            (sequenceEnd (token i.end) i.spaces i.item (token i.separator) i.trailing)
        )


{-| What’s the deal with trailing commas? Are they `Forbidden`?
Are they `Optional`? Are they `Mandatory`? Welcome to [shapes
club](https://poorlydrawnlines.com/comic/shapes-club/)!
-}
type Trailing
    = Forbidden
    | Optional
    | Mandatory


skip : Parser c x ignore -> Parser c x keep -> Parser c x keep
skip iParser kParser =
    map2 revAlways iParser kParser


revAlways : a -> b -> b
revAlways _ b =
    b


sequenceEnd :
    Parser c x ()
    -> Parser c x ()
    -> Parser c x a
    -> Parser c x ()
    -> Trailing
    -> Parser c x (List a)
sequenceEnd ender ws parseItem sep trailing =
    let
        chompRest item =
            case trailing of
                Forbidden ->
                    loop [ item ] (sequenceEndForbidden ender ws parseItem sep)

                Optional ->
                    loop [ item ] (sequenceEndOptional ender ws parseItem sep)

                Mandatory ->
                    ignorer
                        (skip ws
                            (skip sep
                                (skip ws
                                    (loop [ item ] (sequenceEndMandatory ws parseItem sep))
                                )
                            )
                        )
                        ender
    in
    oneOf
        [ parseItem |> andThen chompRest
        , ender |> map (\_ -> [])
        ]


sequenceEndForbidden :
    Parser c x ()
    -> Parser c x ()
    -> Parser c x a
    -> Parser c x ()
    -> List a
    -> Parser c x (Step (List a) (List a))
sequenceEndForbidden ender ws parseItem sep revItems =
    let
        chompRest item =
            sequenceEndForbidden ender ws parseItem sep (item :: revItems)
    in
    skip ws <|
        oneOf
            [ skip sep <| skip ws <| map (\item -> Loop (item :: revItems)) parseItem
            , ender |> map (\_ -> Done (List.reverse revItems))
            ]


sequenceEndOptional :
    Parser c x ()
    -> Parser c x ()
    -> Parser c x a
    -> Parser c x ()
    -> List a
    -> Parser c x (Step (List a) (List a))
sequenceEndOptional ender ws parseItem sep revItems =
    let
        parseEnd =
            map (\_ -> Done (List.reverse revItems)) ender
    in
    skip ws
        (oneOf
            [ skip sep
                (skip ws
                    (oneOf
                        [ parseItem |> map (\item -> Loop (item :: revItems))
                        , parseEnd
                        ]
                    )
                )
            , parseEnd
            ]
        )


sequenceEndMandatory :
    Parser c x ()
    -> Parser c x a
    -> Parser c x ()
    -> List a
    -> Parser c x (Step (List a) (List a))
sequenceEndMandatory ws parseItem sep revItems =
    oneOf
        [ map (\item -> Loop (item :: revItems)) <|
            ignorer parseItem (ignorer ws (ignorer sep ws))
        , map (\_ -> Done (List.reverse revItems)) (succeed ())
        ]



-- WHITESPACE


{-| Just like [`Parser.spaces`](Parser#spaces)
-}
spaces : Parser c x ()
spaces =
    chompWhile
        (\c ->
            if Pine_kernel.equal [ c, ' ' ] then
                True

            else if Pine_kernel.equal [ c, '\n' ] then
                True

            else if Pine_kernel.equal [ c, '\u{000D}' ] then
                True

            else
                False
        )


{-| Just like [`Parser.lineComment`](Parser#lineComment) except you provide a
`Token` describing the starting symbol.
-}
lineComment : Token x -> Parser c x ()
lineComment start =
    ignorer (token start) (chompUntilEndOr "\n")


{-| Just like [`Parser.multiComment`](Parser#multiComment) except with a
`Token` for the open and close symbols.
-}
multiComment : Token x -> Token x -> Nestable -> Parser c x ()
multiComment open close nestable =
    case nestable of
        NotNestable ->
            ignorer (token open) (chompUntil close)

        Nestable ->
            nestableComment open close


{-| Works just like [`Parser.Nestable`](Parser#nestable) to help distinguish
between unnestable `/*` `*/` comments like in JS and nestable `{-` `-}`
comments like in Elm.
-}
type Nestable
    = NotNestable
    | Nestable


nestableComment : Token x -> Token x -> Parser c x ()
nestableComment ((Token (String openChars) oX) as open) ((Token (String closeChars) cX) as close) =
    let
        openChar =
            Pine_kernel.take [ 4, openChars ]

        closeChar =
            Pine_kernel.take [ 4, closeChars ]
    in
    if Pine_kernel.equal [ Pine_kernel.length openChars, 0 ] then
        problem oX

    else if Pine_kernel.equal [ Pine_kernel.length closeChars, 0 ] then
        problem cX

    else
        let
            chompOpen =
                token open
        in
        ignorer
            chompOpen
            (nestableHelp
                (nestableCommentPredicateNotRelevant openChar closeChar)
                chompOpen
                (token close)
                cX
                1
            )


nestableCommentPredicateNotRelevant : Char -> Char -> Char -> Bool
nestableCommentPredicateNotRelevant openChar closeChar char =
    if Pine_kernel.equal [ char, openChar ] then
        False

    else if Pine_kernel.equal [ char, closeChar ] then
        False

    else
        True


nestableHelp : (Char -> Bool) -> Parser c x () -> Parser c x () -> x -> Int -> Parser c x ()
nestableHelp isNotRelevant open close expectingClose nestLevel =
    skip
        (chompWhile isNotRelevant)
        (oneOf
            [ if Pine_kernel.equal [ nestLevel, 1 ] then
                close

              else
                close
                    |> andThen
                        (\_ ->
                            nestableHelp
                                isNotRelevant
                                open
                                close
                                expectingClose
                                (Pine_kernel.int_add [ nestLevel, -1 ])
                        )
            , open
                |> andThen
                    (\_ ->
                        nestableHelp
                            isNotRelevant
                            open
                            close
                            expectingClose
                            (Pine_kernel.int_add [ nestLevel, 1 ])
                    )
            , chompIf isChar expectingClose
                |> andThen
                    (\_ ->
                        nestableHelp
                            isNotRelevant
                            open
                            close
                            expectingClose
                            nestLevel
                    )
            ]
        )


isChar : Char -> Bool
isChar char =
    True
