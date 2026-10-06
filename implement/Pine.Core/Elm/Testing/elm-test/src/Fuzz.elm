module Fuzz exposing
    ( Fuzzer, examples, labelExamples
    , int, intRange, uniformInt, intAtLeast, intAtMost
    , float, niceFloat, percentage, floatRange, floatAtLeast, floatAtMost
    , char, asciiChar
    , string, stringOfLength, stringOfLengthBetween, asciiString, asciiStringOfLength, asciiStringOfLengthBetween
    , pair, triple
    , list, listOfLength, listOfLengthBetween, shuffledList
    , array, maybe, result
    , bool, unit, order, weightedBool
    , oneOf, oneOfValues, frequency, frequencyValues
    , constant, invalid, filter, filterMap
    , map, map2, map3, map4, map5, map6, map7, map8, andMap
    , andThen, lazy, sequence, traverse
    , fromGenerator
    )

{-| This is a library of _fuzzers_ you can use to supply values to your fuzz
tests. You can typically pick out which ones you need according to their types.

A `Fuzzer a` knows how to create values of type `a`. It can create them randomly,
so that your test's expectations are run against many values. Fuzzers will often
generate edge cases likely to find bugs. If the fuzzer can make your test fail,
the test runner also knows how to "simplify" that failing input into more minimal
examples, some of which might also cause the tests to fail. In this way, fuzzers
can usually find the simplest input that reproduces a bug.


## Fuzzers

@docs Fuzzer, examples, labelExamples


## Number fuzzers

@docs int, intRange, uniformInt, intAtLeast, intAtMost
@docs float, niceFloat, percentage, floatRange, floatAtLeast, floatAtMost


## String-related fuzzers

@docs char, asciiChar
@docs string, stringOfLength, stringOfLengthBetween, asciiString, asciiStringOfLength, asciiStringOfLengthBetween


## Collection fuzzers

@docs pair, triple
@docs list, listOfLength, listOfLengthBetween, shuffledList
@docs array, maybe, result


## Other fuzzers

@docs bool, unit, order, weightedBool


## Choosing fuzzers

@docs oneOf, oneOfValues, frequency, frequencyValues


## Working with Fuzzers

@docs constant, invalid, filter, filterMap
@docs map, map2, map3, map4, map5, map6, map7, map8, andMap
@docs andThen, lazy, sequence, traverse


## Misc helpers

@docs fromGenerator

-}

import Array exposing (Array)
import Bitwise
import Char
import Dict exposing (Dict)
import Fuzz.Float
import Fuzz.Internal exposing (Fuzzer(..))
import GenResult exposing (GenResult(..))
import MicroDictExtra as Dict
import MicroListExtra as List
import PRNG exposing (PRNG(..))
import Random
import RandomRun


{-| The representation of fuzzers is opaque. Conceptually, a `Fuzzer a` consists
of a way to randomly generate values of type `a` in a way allowing the test runner
to simplify those values.
-}
type alias Fuzzer a =
    Fuzz.Internal.Fuzzer a


{-| A fuzzer for the unit value. Unit is a type with only one value, commonly
used as a placeholder.
-}
unit : Fuzzer ()
unit =
    constant ()


{-| A fuzzer for boolean values. It's useful when building up fuzzers of complex
types that contain a boolean somewhere.

We recommend against writing tests fuzzing over booleans. Write a unit test for
the true and false cases explicitly.

Simplifies in order `False < True`.

-}
bool : Fuzzer Bool
bool =
    oneOfValues [ False, True ]


{-| A fuzzer for order values.

Simplifies in order `LT < EQ < GT`.

-}
order : Fuzzer Order
order =
    oneOfValues [ LT, EQ, GT ]


intBucketingThreshold : Int
intBucketingThreshold =
    255


intPreferences : List { weight : Int, bits : Int }
intPreferences =
    [ { weight = 4, bits = 4 } -- 0..15
    , { weight = 8, bits = 8 } -- 0..255
    , { weight = 2, bits = 16 } -- 0..65535
    , { weight = 1, bits = 32 } -- 0..4294967295
    ]


{-| A fuzzer for int values. It will never produce `NaN`, `Infinity`, or
`-Infinity`.

This fuzzer will generate values in the range `Random.minInt .. Random.maxInt`.

  - Simplifies towards 0
  - Prefers positive values over negative ones
  - Prefers smaller values over larger ones

-}
int : Fuzzer Int
int =
    intPreferences
        |> List.map (\{ weight, bits } -> ( weight, intBits bits ))
        |> intFrequency
        |> map
            (\n ->
                let
                    isNegative =
                        Bitwise.and 1 n == 1

                    withoutFirstBit =
                        Bitwise.shiftRightBy 1 n
                in
                if isNegative then
                    negate withoutFirstBit

                else
                    withoutFirstBit
            )


{-| An unsigned integer taken uniformly from the range 0..(2^n)-1.
-}
intBits : Int -> Fuzzer Int
intBits bitsCount =
    uniformInt ((2 ^ bitsCount) - 1)


{-| A fuzzer that will generate values in range n..2^32-1.
-}
intAtLeast : Int -> Fuzzer Int
intAtLeast n =
    intRange n (2 ^ 32 - 1)


{-| A fuzzer that will generate values in range -(2^32-1)..n.
-}
intAtMost : Int -> Fuzzer Int
intAtMost n =
    intRange -(2 ^ 32 - 1) n


{-| A fuzzer for int values between a given minimum and maximum value,
inclusive. Shrunk values will also be within the range.
-}
intRange : Int -> Int -> Fuzzer Int
intRange lo hi =
    if hi < lo then
        intRange hi lo

    else if lo == hi then
        constant lo

    else
        let
            int_ : Int -> Fuzzer Int
            int_ upperLimit =
                {- Two variants:

                   1. If the range of numbers is low enough, we skip the
                      bucketing and just do uniform choice in the whole range.

                      Meaning `intRange INT_BELOW_THRESHOLD` will only draw
                      one integer.

                   2. If the range is above some threshold, we turn on the
                      bucketing and prefer smaller values.

                      Meaning `intRange INT_ABOVE_THRESHOLD` will draw
                      two integers: one for the bucket and one for the actual
                      integer inside.
                -}
                if upperLimit <= intBucketingThreshold then
                    {- TODO PERF: is
                           intBits bitsCount |> map (modBy (upperLimit + 1))
                       faster than
                           uniformInt upperLimit
                       ?
                    -}
                    uniformInt upperLimit

                else
                    let
                        range : Int
                        range =
                            upperLimit + 1

                        maxBits : Int
                        maxBits =
                            range
                                -- find how many bits the number takes
                                |> toFloat
                                |> logBase 2
                                |> ceiling
                                -- then find the next power of 2 (which is what our intPreferences are)
                                |> toFloat
                                |> logBase 2
                                |> ceiling
                                |> (\n -> 2 ^ n)
                    in
                    intPreferences
                        |> List.filter (\{ bits } -> bits <= maxBits)
                        |> (\list_ ->
                                {- failsafe for values taking less than 4 bits
                                   (lowest bitcount in intPreferences)
                                -}
                                if List.isEmpty list_ then
                                    List.take 1 intPreferences

                                else
                                    list_
                           )
                        |> List.map (\{ weight, bits } -> ( weight, intBits bits ))
                        |> intFrequency
                        |> map (modBy range)
        in
        if lo >= 0 then
            -- both non-negative
            int_ (hi - lo)
                {- intRange 2 5: uniformInt 3: 0,1,2,3
                   => (+) 2 => 2,3,4,5
                   simplifying towards zero, not Inf
                -}
                |> map (\n -> n + lo)

        else if hi <= 0 then
            -- both negative
            int_ (hi - lo)
                {- intRange -5 -2: uniformInt 3: 0,1,2,3
                   => negate => -0,-1,-2,-3
                   => (+) -2 => -2,-3,-4,-5
                   simplifying towards zero, not -Inf
                -}
                |> map (\n -> negate n + hi)

        else
            {- somewhere in the middle, divide it into negative and positive ranges,
               both of which will simplify towards zero. We prefer positive values.
            -}
            oneOf
                [ intRange 0 hi -- the conditions above guarantee hi >= 1
                , intRange lo -1 -- the conditions above guarantee lo <= -1
                ]


{-| A fuzzer for float values.

Will prefer integer values, nice fractions and positive numbers over the rest.

Will occasionally try infinities and NaN. If you don't want to generate these,
use [`Fuzz.niceFloat`](#niceFloat).

-}
float : Fuzzer Float
float =
    intFrequency
        [ {- Just to shrink nicely. The wellShrinkingFloat below needs 3 items
             in the RandomRun so sometimes it's not an option anymore.
          -}
          ( 1, constant 0 )
        , ( 5, wellShrinkingFloat )
        , ( 1, constant (1 / 0) )
        , ( 1, constant (-1 / 0) )
        , ( 1, constant (0 / 0) )
        ]


{-| A fuzzer for float values.

Will prefer integer values, nice fractions and positive numbers over the rest.

Will never try infinities or NaN.

-}
niceFloat : Fuzzer Float
niceFloat =
    wellShrinkingFloat


{-| This float fuzzer will prefer non-fractional floats and (if it must) nice
fractions.
-}
wellShrinkingFloat : Fuzzer Float
wellShrinkingFloat =
    map3
        (\hi lo shouldNegate ->
            let
                f : Float
                f =
                    Fuzz.Float.wellShrinkingFloat ( hi, lo )
            in
            if shouldNegate then
                negate f

            else
                f
        )
        int32
        int32
        bool
        |> filter (\n -> not (isInfinite n || isNaN n))


{-| Fuzzer generating floats in range `n..Infinity`.

The positive part of the range will shrink nicely, the negative part will shrink uniformly.

The fuzzer will occasionally try the minimum, 0 (if in range) and Infinity.

-}
floatAtLeast : Float -> Fuzzer Float
floatAtLeast n =
    if n <= 0 then
        intFrequency
            [ ( 4, floatRange n 0 )
            , ( 4, wellShrinkingFloat |> map abs )
            , ( 2, constant n )
            , ( 2, constant (1 / 0) )
            , ( 1, constant 0 )
            ]

    else
        intFrequency
            [ ( 8, wellShrinkingFloat |> map (\x -> n + abs x) )
            , ( 2, constant n )
            , ( 2, constant (1 / 0) )
            ]


{-| Fuzzer generating floats in range `-Infinity..n`.

The negative part of the range will shrink nicely, the positive part will shrink uniformly.

The fuzzer will occasionally try the maximum, 0 (if in range) and -Infinity.

-}
floatAtMost : Float -> Fuzzer Float
floatAtMost n =
    if n >= 0 then
        intFrequency
            [ ( 4, floatRange 0 n )
            , ( 4, wellShrinkingFloat |> map (negate << abs) )
            , ( 2, constant n )
            , ( 2, constant (-1 / 0) )
            , ( 1, constant 0 )
            ]

    else
        intFrequency
            [ ( 8, wellShrinkingFloat |> map (\x -> n - abs x) )
            , ( 2, constant n )
            , ( 2, constant (-1 / 0) )
            ]


{-| A fuzzer for float values within between a given minimum and maximum (inclusive).

Shrunken values will also be within the range.

-}
floatRange : Float -> Float -> Fuzzer Float
floatRange lo hi =
    if hi < lo then
        floatRange hi lo

    else if lo == hi then
        constant lo

    else if lo >= 0 then
        -- both non-negative
        intFrequency
            [ ( 1, constant lo )
            , ( 1, constant hi )
            , ( 4, scaledFloat lo hi )
            ]

    else if hi <= 0 then
        -- both negative
        intFrequency
            [ ( 1, constant lo )
            , ( 1, constant hi )
            , ( 4
              , scaledFloat (negate hi) (negate lo)
                    -- simplify towards zero
                    |> map negate
              )
            ]

    else
        {- somewhere in the middle, divide it into negative and positive ranges,
           both of which will simplify towards zero. We prefer positive values.
        -}
        intFrequency
            [ ( 1, constant 0 )
            , ( 2, constant lo )
            , ( 2, constant hi )
            , ( 4, scaledFloat 0 (negate lo) |> map negate )
            , ( 4, scaledFloat 0 hi )
            ]


{-| This float fuzzer won't shrink nicely (to integers or nice fractions). For
that, use `wellShrinkingFloat`.
-}
scaledFloat : Float -> Float -> Fuzzer Float
scaledFloat lo hi =
    if lo == hi then
        constant lo

    else if lo > hi then
        scaledFloat hi lo

    else
        percentage
            |> map (\f -> f * (hi - lo) + lo)


{-| A fuzzer for percentage values. Generates random floats between `0.0`
inclusive and `1.0` exclusive, in an uniform fashion.

Will occasionally try the boundaries.

Doesn't shrink to nice values like [`Fuzz.float`](#float) does; shrinks towards
zero.

-}
percentage : Fuzzer Float
percentage =
    {- We can't use Random.Generators here as all fuzzed values must be
       representable as one or more ints. We generally use a pair of 32bit ints
       to represent a 64bit float.

       Here we know the top 12 bits of the high int wouldn't be used for the
       mantissa calculations so we don't bother generating those.
    -}
    intFrequency
        [ ( 1, constant 0 )
        , ( 1, constant Fuzz.Float.maxFractionalFloat ) -- just barely below 1
        , ( 4
          , pair (uniformInt 0x000FFFFF) int32
                |> map Fuzz.Float.fractionalFloat
          )
        ]


int32 : Fuzzer Int
int32 =
    uniformInt 0xFFFFFFFF


{-| A fuzzer for simple ASCII char values (range 32..126).
Skips control characters and the extended character set.

For more serious char fuzzing look at [`Fuzz.char`](#char) which generates the
whole Unicode range.

-}
asciiChar : Fuzzer Char
asciiChar =
    -- TODO: what about preferring nasty chars like \, /, $, @ (interpolation, SQL injections, ...)?
    intRange 32 126
        |> map Char.fromCode


{-| A fuzzer for arbitrary Unicode char values.

Avoids surrogate pairs or their components (`0xD800..0xDFFF`).

Will prefer ASCII characters, whitespace, and some examples known to cause
trouble, like combining diacritics marks and emojis.

-}
char : Fuzzer Char
char =
    let
        whitespaceChar : Fuzzer Char
        whitespaceChar =
            oneOfValues
                [ ' '
                , '\t'
                , '\n'
                ]

        combiningDiacriticalMarkChar : Fuzzer Char
        combiningDiacriticalMarkChar =
            -- going a roundabout way about this to keep Vim from being confused about unclosed strings
            oneOfValues
                [ Char.fromCode 0x0302 -- combining circumflex accent
                , Char.fromCode 0x0303 -- combining tilde
                , Char.fromCode 0x0308 -- combining diaeresis
                ]

        emojiChar : Fuzzer Char
        emojiChar =
            oneOfValues
                [ '🌈'
                , '❤'
                , '🔥'
                ]

        {- Note: This can produce garbage values as Unicode doesn't use all valid values.

           0xD800..0xDFFF are surrogate code units and would break tests like
           `(str |> reverse |> reverse) == str` because of being converted to
           0xFFFD REPLACEMENT CHARACTER.
        -}
        arbitraryUnicodeChar : Fuzzer Char
        arbitraryUnicodeChar =
            intRange 0 0x0010FFFF
                |> filter (\n -> not (n >= 0xD800 && n <= 0xDFFF))
                |> map Char.fromCode
    in
    intFrequency
        [ ( 5, asciiChar )
        , ( 2, whitespaceChar )
        , ( 1, combiningDiacriticalMarkChar )
        , ( 1, emojiChar )
        , ( 1, arbitraryUnicodeChar )
        ]


{-| Generates random unicode strings of up to 10 characters.
-}
string : Fuzzer String
string =
    stringOfLengthBetween 0 10


{-| Generates random unicode strings of a given length.

Note that some unicode characters have `String.length` of 2. This fuzzer will
make sure the `String.length` of the returned string is equal to the wanted
length, even if it will mean there are less characters. If you instead want it
to give N characters even if their `String.length` will be above N, you can use

    Fuzz.listOfLength n Fuzz.char
        |> Fuzz.map String.fromList

-}
stringOfLength : Int -> Fuzzer String
stringOfLength n =
    stringOfLengthBetween n n


{-| Generates random unicode strings of length between the given limits.

Note that some unicode characters have `String.length` of 2. This fuzzer will
make sure the `String.length` of the returned string is equal to the wanted
length, even if it will mean there are less characters. If you instead want it
to give between MIN and MAX characters even if their `String.length` will be
above MAX, you can use

    Fuzz.listOfLengthBetween min max Fuzz.char
        |> Fuzz.map String.fromList

-}
stringOfLengthBetween : Int -> Int -> Fuzzer String
stringOfLengthBetween min max =
    if min > max then
        stringOfLengthBetween max min

    else if max <= 0 then
        constant ""

    else
        listOfLengthBetween min max char
            |> map String.fromList
            |> filter
                (\str ->
                    let
                        length =
                            String.length str
                    in
                    length >= min && length <= max
                )


{-| Generates random ASCII strings of up to 10 characters.
-}
asciiString : Fuzzer String
asciiString =
    asciiStringOfLengthBetween 0 10


{-| Generates random ASCII strings of a given length.
-}
asciiStringOfLength : Int -> Fuzzer String
asciiStringOfLength n =
    asciiStringOfLengthBetween n n


{-| Generates random ASCII strings of length between the given limits.
-}
asciiStringOfLengthBetween : Int -> Int -> Fuzzer String
asciiStringOfLengthBetween min max =
    listOfLengthBetween min max asciiChar
        |> map String.fromList


{-| Given a fuzzer of a type, create a fuzzer of a maybe for that type.
-}
maybe : Fuzzer a -> Fuzzer (Maybe a)
maybe fuzzer =
    intFrequency
        [ ( 1, constant Nothing )
        , ( 3, map Just fuzzer )
        ]


{-| Given fuzzers for an error type and a success type, create a fuzzer for
a result.
-}
result : Fuzzer error -> Fuzzer value -> Fuzzer (Result error value)
result fuzzerError fuzzerValue =
    intFrequency
        [ ( 1, map Err fuzzerError )
        , ( 3, map Ok fuzzerValue )
        ]


{-| Given a fuzzer of a type, create a fuzzer of a list of that type.
Generates random lists of varying length, up to 32 elements.
-}
list : Fuzzer a -> Fuzzer (List a)
list fuzzer =
    listOfLengthBetween 0 32 fuzzer


{-| Given a fuzzer of a type, create a fuzzer of a list of that type.
Generates random lists of exactly the specified length.
-}
listOfLength : Int -> Fuzzer a -> Fuzzer (List a)
listOfLength n fuzzer =
    listOfLengthBetween n n fuzzer


{-| Given a fuzzer of a type, create a fuzzer of a list of that type.
Generates random lists of length between the two given integers.
-}
listOfLengthBetween : Int -> Int -> Fuzzer a -> Fuzzer (List a)
listOfLengthBetween lo hi itemFuzzer =
    if lo > hi then
        -- the name allows for it, even if it's a little weird
        listOfLengthBetween hi lo itemFuzzer

    else if hi <= 0 then
        constant []

    else
        let
            average : Float
            average =
                toFloat lo + toFloat hi / 2

            continueProbability : Float
            continueProbability =
                {- Taken from Python Hypothesis library (ListStrategy).
                   It should supposedly be a geometric distribution, although I
                   don't see the connection from the below formula. ~janiczek
                -}
                1 - 1 / (1 + average)

            end : List a -> Fuzzer (List a)
            end acc =
                constant (List.reverse acc)

            go : Int -> List a -> Fuzzer (List a)
            go length acc =
                if length < lo then
                    forcedChoice 1
                        |> andThen
                            (\_ ->
                                itemFuzzer |> andThen (\item -> go (length + 1) (item :: acc))
                            )

                else if length == hi then
                    forcedChoice 0
                        |> andThen (\_ -> end acc)

                else
                    weightedBool continueProbability
                        |> andThen
                            (\oneMorePlease ->
                                if oneMorePlease then
                                    itemFuzzer |> andThen (\item -> go (length + 1) (item :: acc))

                                else
                                    end acc
                            )
        in
        go 0 []


{-| Given a fuzzer of a type, create a fuzzer of an array of that type.
Generates random arrays of varying length, favoring shorter arrays.
-}
array : Fuzzer a -> Fuzzer (Array a)
array fuzzer =
    map Array.fromList (list fuzzer)


{-| Create a fuzzer of pairs from two fuzzers.
-}
pair : Fuzzer a -> Fuzzer b -> Fuzzer ( a, b )
pair fuzzerA fuzzerB =
    map2 (\a b -> ( a, b )) fuzzerA fuzzerB


{-| Create a fuzzer of triples from three fuzzers.
-}
triple : Fuzzer a -> Fuzzer b -> Fuzzer c -> Fuzzer ( a, b, c )
triple fuzzerA fuzzerB fuzzerC =
    map3 (\a b c -> ( a, b, c )) fuzzerA fuzzerB fuzzerC


{-| Create a fuzzer that only and always returns the value provided, and performs
no simplifying. This is hardly random, and so this function is best used as a
helper when creating more complicated fuzzers.
-}
constant : a -> Fuzzer a
constant x =
    Fuzzer <|
        \prng ->
            Generated
                { value = x
                , prng = prng
                }


{-| Map a function over a fuzzer.
-}
map : (a -> b) -> Fuzzer a -> Fuzzer b
map fn (Fuzzer fuzzer) =
    Fuzzer <|
        \prng ->
            case fuzzer prng of
                Generated g ->
                    Generated
                        { value = fn g.value
                        , prng = g.prng
                        }

                Rejected r ->
                    Rejected r


{-| Map over two fuzzers.
-}
map2 : (a -> b -> c) -> Fuzzer a -> Fuzzer b -> Fuzzer c
map2 fn (Fuzzer fuzzerA) (Fuzzer fuzzerB) =
    Fuzzer <|
        \prng ->
            case fuzzerA prng of
                Generated a ->
                    case fuzzerB a.prng of
                        Generated b ->
                            Generated
                                { value = fn a.value b.value
                                , prng = b.prng
                                }

                        Rejected r ->
                            Rejected r

                Rejected r ->
                    Rejected r


{-| Map over three fuzzers.
-}
map3 : (a -> b -> c -> d) -> Fuzzer a -> Fuzzer b -> Fuzzer c -> Fuzzer d
map3 fn (Fuzzer fuzzerA) (Fuzzer fuzzerB) (Fuzzer fuzzerC) =
    Fuzzer <|
        \prng ->
            case fuzzerA prng of
                Generated a ->
                    case fuzzerB a.prng of
                        Generated b ->
                            case fuzzerC b.prng of
                                Generated c ->
                                    Generated
                                        { value = fn a.value b.value c.value
                                        , prng = c.prng
                                        }

                                Rejected r ->
                                    Rejected r

                        Rejected r ->
                            Rejected r

                Rejected r ->
                    Rejected r


{-| Map over four fuzzers.
-}
map4 : (a -> b -> c -> d -> e) -> Fuzzer a -> Fuzzer b -> Fuzzer c -> Fuzzer d -> Fuzzer e
map4 fn (Fuzzer fuzzerA) (Fuzzer fuzzerB) (Fuzzer fuzzerC) (Fuzzer fuzzerD) =
    Fuzzer <|
        \prng ->
            case fuzzerA prng of
                Generated a ->
                    case fuzzerB a.prng of
                        Generated b ->
                            case fuzzerC b.prng of
                                Generated c ->
                                    case fuzzerD c.prng of
                                        Generated d ->
                                            Generated
                                                { value = fn a.value b.value c.value d.value
                                                , prng = d.prng
                                                }

                                        Rejected r ->
                                            Rejected r

                                Rejected r ->
                                    Rejected r

                        Rejected r ->
                            Rejected r

                Rejected r ->
                    Rejected r


{-| Map over five fuzzers.
-}
map5 : (a -> b -> c -> d -> e -> f) -> Fuzzer a -> Fuzzer b -> Fuzzer c -> Fuzzer d -> Fuzzer e -> Fuzzer f
map5 fn (Fuzzer fuzzerA) (Fuzzer fuzzerB) (Fuzzer fuzzerC) (Fuzzer fuzzerD) (Fuzzer fuzzerE) =
    Fuzzer <|
        \prng ->
            case fuzzerA prng of
                Generated a ->
                    case fuzzerB a.prng of
                        Generated b ->
                            case fuzzerC b.prng of
                                Generated c ->
                                    case fuzzerD c.prng of
                                        Generated d ->
                                            case fuzzerE d.prng of
                                                Generated e ->
                                                    Generated
                                                        { value = fn a.value b.value c.value d.value e.value
                                                        , prng = e.prng
                                                        }

                                                Rejected r ->
                                                    Rejected r

                                        Rejected r ->
                                            Rejected r

                                Rejected r ->
                                    Rejected r

                        Rejected r ->
                            Rejected r

                Rejected r ->
                    Rejected r


{-| Map over six fuzzers.
-}
map6 : (a -> b -> c -> d -> e -> f -> g) -> Fuzzer a -> Fuzzer b -> Fuzzer c -> Fuzzer d -> Fuzzer e -> Fuzzer f -> Fuzzer g
map6 fn (Fuzzer fuzzerA) (Fuzzer fuzzerB) (Fuzzer fuzzerC) (Fuzzer fuzzerD) (Fuzzer fuzzerE) (Fuzzer fuzzerF) =
    Fuzzer <|
        \prng ->
            case fuzzerA prng of
                Generated a ->
                    case fuzzerB a.prng of
                        Generated b ->
                            case fuzzerC b.prng of
                                Generated c ->
                                    case fuzzerD c.prng of
                                        Generated d ->
                                            case fuzzerE d.prng of
                                                Generated e ->
                                                    case fuzzerF e.prng of
                                                        Generated f ->
                                                            Generated
                                                                { value = fn a.value b.value c.value d.value e.value f.value
                                                                , prng = f.prng
                                                                }

                                                        Rejected r ->
                                                            Rejected r

                                                Rejected r ->
                                                    Rejected r

                                        Rejected r ->
                                            Rejected r

                                Rejected r ->
                                    Rejected r

                        Rejected r ->
                            Rejected r

                Rejected r ->
                    Rejected r


{-| Map over seven fuzzers.
-}
map7 : (a -> b -> c -> d -> e -> f -> g -> h) -> Fuzzer a -> Fuzzer b -> Fuzzer c -> Fuzzer d -> Fuzzer e -> Fuzzer f -> Fuzzer g -> Fuzzer h
map7 fn (Fuzzer fuzzerA) (Fuzzer fuzzerB) (Fuzzer fuzzerC) (Fuzzer fuzzerD) (Fuzzer fuzzerE) (Fuzzer fuzzerF) (Fuzzer fuzzerG) =
    Fuzzer <|
        \prng ->
            case fuzzerA prng of
                Generated a ->
                    case fuzzerB a.prng of
                        Generated b ->
                            case fuzzerC b.prng of
                                Generated c ->
                                    case fuzzerD c.prng of
                                        Generated d ->
                                            case fuzzerE d.prng of
                                                Generated e ->
                                                    case fuzzerF e.prng of
                                                        Generated f ->
                                                            case fuzzerG f.prng of
                                                                Generated g ->
                                                                    Generated
                                                                        { value = fn a.value b.value c.value d.value e.value f.value g.value
                                                                        , prng = g.prng
                                                                        }

                                                                Rejected r ->
                                                                    Rejected r

                                                        Rejected r ->
                                                            Rejected r

                                                Rejected r ->
                                                    Rejected r

                                        Rejected r ->
                                            Rejected r

                                Rejected r ->
                                    Rejected r

                        Rejected r ->
                            Rejected r

                Rejected r ->
                    Rejected r


{-| Map over eight fuzzers.
-}
map8 : (a -> b -> c -> d -> e -> f -> g -> h -> i) -> Fuzzer a -> Fuzzer b -> Fuzzer c -> Fuzzer d -> Fuzzer e -> Fuzzer f -> Fuzzer g -> Fuzzer h -> Fuzzer i
map8 fn (Fuzzer fuzzerA) (Fuzzer fuzzerB) (Fuzzer fuzzerC) (Fuzzer fuzzerD) (Fuzzer fuzzerE) (Fuzzer fuzzerF) (Fuzzer fuzzerG) (Fuzzer fuzzerH) =
    Fuzzer <|
        \prng ->
            case fuzzerA prng of
                Generated a ->
                    case fuzzerB a.prng of
                        Generated b ->
                            case fuzzerC b.prng of
                                Generated c ->
                                    case fuzzerD c.prng of
                                        Generated d ->
                                            case fuzzerE d.prng of
                                                Generated e ->
                                                    case fuzzerF e.prng of
                                                        Generated f ->
                                                            case fuzzerG f.prng of
                                                                Generated g ->
                                                                    case fuzzerH g.prng of
                                                                        Generated h ->
                                                                            Generated
                                                                                { value = fn a.value b.value c.value d.value e.value f.value g.value h.value
                                                                                , prng = h.prng
                                                                                }

                                                                        Rejected r ->
                                                                            Rejected r

                                                                Rejected r ->
                                                                    Rejected r

                                                        Rejected r ->
                                                            Rejected r

                                                Rejected r ->
                                                    Rejected r

                                        Rejected r ->
                                            Rejected r

                                Rejected r ->
                                    Rejected r

                        Rejected r ->
                            Rejected r

                Rejected r ->
                    Rejected r


{-| Map over many fuzzers. This can act as `mapN` for `N > 8`.
The argument order is meant to accommodate chaining:

    Fuzz.constant fn
        |> Fuzz.andMap fuzzerA
        |> Fuzz.andMap fuzzerB
        |> Fuzz.andMap fuzzerC

-}
andMap : Fuzzer a -> Fuzzer (a -> b) -> Fuzzer b
andMap =
    map2 (|>)


{-| Create a new `Fuzzer` by providing a list of probabilistic weights to use
with other fuzzers.
For example, to create a `Fuzzer` that has a 1/4 chance of generating an int
between -1 and -100, and a 3/4 chance of generating one between 1 and 100,
you could do this:

    Fuzz.frequency
        [ ( 1, Fuzz.intRange -100 -1 )
        , ( 3, Fuzz.intRange 1 100 )
        ]

This fuzzer will simplify towards the fuzzers earlier in the list (each of which
will also apply its own way to simplify the values).

There are a few circumstances in which this function will return an invalid
fuzzer, which causes it to fail any test that uses it:

  - If you provide an empty list of frequencies
  - If any of the weights are less than 0
  - If the weights sum to 0

Be careful recursively using this fuzzer in its arguments. Often using
[`Fuzz.map`](#map) is a better way to do what you want. If you are fuzzing a
tree-like data structure, you should include a depth limit so to avoid infinite
recursion, like so:

    type Tree
        = Leaf
        | Branch Tree Tree

    tree : Int -> Fuzzer Tree
    tree i =
        if i <= 0 then
            Fuzz.constant Leaf

        else
            Fuzz.frequency
                [ ( 1, Fuzz.constant Leaf )
                , ( 2, Fuzz.map2 Branch (tree (i - 1)) (tree (i - 1)) )
                ]

-}
frequency : List ( Float, Fuzzer a ) -> Fuzzer a
frequency fuzzers =
    frequencyHelp "Fuzz.frequency" fuzzers


frequencyHelp : String -> List ( Float, Fuzzer a ) -> Fuzzer a
frequencyHelp functionName fuzzers =
    if List.any (\( w, _ ) -> w < 0) fuzzers then
        invalid <| functionName ++ ": No frequency weights can be less than 0."

    else
        let
            nonzeroFuzzers =
                List.filter (\( w, _ ) -> w > 0) fuzzers
        in
        if List.isEmpty nonzeroFuzzers then
            invalid <| functionName ++ ": You must provide at least one frequency pair with weight greater than 0."

        else
            let
                allWeightsAreInts =
                    List.all (\( w, _ ) -> w == toFloat (round w)) nonzeroFuzzers
            in
            if allWeightsAreInts then
                intFrequency (List.map (Tuple.mapFirst round) nonzeroFuzzers)

            else
                let
                    weightSum : Float
                    weightSum =
                        List.foldl (\( w, _ ) acc -> w + acc) 0 nonzeroFuzzers
                in
                percentage
                    |> andThen
                        (\p ->
                            let
                                f : Float
                                f =
                                    p * weightSum

                                go : Float -> List ( Float, Fuzzer a ) -> Fuzzer a
                                go countdown acc =
                                    case acc of
                                        [] ->
                                            invalid <| "elm-test bug: " ++ functionName ++ " encountered empty list after checking for it."

                                        [ ( _, last ) ] ->
                                            last

                                        ( w, current ) :: rest ->
                                            if countdown <= w then
                                                current

                                            else
                                                go (countdown - w) rest
                            in
                            go f nonzeroFuzzers
                        )


{-| A version of `frequency` restricted to Ints that has a much nicer RandomRun
footprint.

`frequency` draws a `percentage` float = two 32bit integers.
This function hides the complexity behind Random.weighted and so only draws one
small integer (index into the user-given list).

`frequency` will automatically switch to this function if used with integers.

-}
intFrequency : List ( Int, Fuzzer a ) -> Fuzzer a
intFrequency fuzzers =
    if List.any (\( w, _ ) -> w <= 0) fuzzers then
        invalid "intFrequency: Weights cannot be non-positive"

    else
        case fuzzers of
            ( n, _ ) :: rest ->
                let
                    weightSum : Int
                    weightSum =
                        List.foldl (\( w, _ ) acc -> w + acc) n rest
                in
                rollDice (weightSum - 1) (intFrequencyGenerator n (List.map Tuple.first rest))
                    |> andThen
                        (\i ->
                            fuzzers
                                |> List.drop i
                                |> List.head
                                |> Maybe.map Tuple.second
                                |> Maybe.withDefault (invalid "elm-test bug: intFrequency index out of range")
                        )

            [] ->
                invalid "intFrequency: You must provide at least one item."


{-| Create a `Fuzzer` by providing a list of probabilistic weights to use with
values.
For example, to create a `Fuzzer` that has a 1/4 chance of generating a string
"foo", and a 3/4 chance of generating a string "bar", you could do this:

    Fuzz.frequencyValues
        [ ( 1, "foo" )
        , ( 3, "bar" )
        ]

This fuzzer will simplify towards the values earlier in the list.

There are a few circumstances in which this function will return an invalid
fuzzer, which causes it to fail any test that uses it:

  - If you provide an empty list of frequencies
  - If any of the weights are less than 0
  - If the weights sum to 0

-}
frequencyValues : List ( Float, a ) -> Fuzzer a
frequencyValues values =
    frequencyHelp "Fuzz.frequencyValues"
        (List.map (Tuple.mapSecond constant) values)


{-| Choose one of the given fuzzers at random. Each fuzzer has an equal chance
of being chosen; to customize the probabilities, use [`Fuzz.frequency`](#frequency).

This fuzzer will simplify towards the fuzzers earlier in the list (each of which
will also apply its own way to simplify the values).

    Fuzz.oneOf
        [ Fuzz.intRange 0 3
        , Fuzz.intRange 7 9
        ]

-}
oneOf : List (Fuzzer a) -> Fuzzer a
oneOf fuzzers =
    oneOfHelp "Fuzz.oneOf" "fuzzer" fuzzers


oneOfHelp : String -> String -> List (Fuzzer a) -> Fuzzer a
oneOfHelp functionName itemName fuzzers =
    case List.length fuzzers of
        0 ->
            invalid <| functionName ++ ": You must provide at least one item."

        length ->
            uniformInt (length - 1)
                |> andThen
                    (\i ->
                        case List.getAt i fuzzers of
                            Nothing ->
                                -- shouldn't happen
                                invalid <| "elm-test bug: " ++ functionName ++ " didn't find a " ++ itemName ++ " at position " ++ String.fromInt i ++ " in the list of length " ++ String.fromInt length ++ "."

                            Just fuzzer ->
                                fuzzer
                    )


{-| Choose one of the given values at random. Each value has an equal chance
of being chosen; to customize the probabilities, use
[`Fuzz.frequencyValues`](#frequencyValues).

This fuzzer will simplify towards the values earlier in the list.

    Fuzz.oneOfValues
        [ 999
        , -42
        ]

-}
oneOfValues : List a -> Fuzzer a
oneOfValues values =
    oneOfHelp "Fuzz.oneOfValues" "value" (List.map constant values)


{-| A fuzzer that is invalid for the provided reason. Any fuzzers built with it
are also invalid. Any tests using an invalid fuzzer fail.
-}
invalid : String -> Fuzzer a
invalid reason =
    Fuzzer <|
        \prng ->
            Rejected
                { reason = reason
                , prng = prng
                }


{-| A fuzzer that only lets through values satisfying the given predicate
function.

Warning: By using `Fuzz.filter` you can get exceptionally unlucky and get 15
rejections in a row, in which case the test will fluke out and fail!

It's always preferable to get to your wanted values using [`Fuzz.map`](#map),
as you don't run the risk of rejecting too may values and slowing down your
tests, for example using `Fuzz.intRange 0 5 |> Fuzz.map (\x -> x * 2)` instead
of `Fuzz.intRange 0 9 |> Fuzz.filter (\x -> modBy 2 x == 0)`.

If you want to generate indefinitely until you find a satisfactory value (with
a risk of infinite loop depending on the predicate), you can use this pattern:

    goodItemFuzzer =
        itemFuzzer
            |> Fuzz.andThen
                (\item ->
                    if isGood item then
                        Fuzz.constant item

                    else
                        goodItemFuzzer
                )

-}
filter : (a -> Bool) -> Fuzzer a -> Fuzzer a
filter predicate =
    filterMap
        (\a ->
            if predicate a then
                Just a

            else
                Nothing
        )


{-| A fuzzer that applies a function returning a Maybe on a given fuzzer and
output values, as List.filterMap does.

Example usage:

    type UnicodeNonLetter
        = UnicodeNonLetter Char

    fromChar : Char -> Maybe UnicodeNonLetter
    fromChar c =
        if (c |> Unicode.isLower |> not) && (c |> Unicode.isUpper |> not) then
            Just (UnicodeNonLetter c)

        else
            Nothing

    fuzz : Fuzzer UnicodeNonLetter
    fuzz =
        Fuzz.char |> Fuzz.filterMap fromChar

Warning: By using `Fuzz.filterMap` you can get exceptionally unlucky and get 15
rejections in a row, in which case the test will fluke out and fail!

It's always preferable to get to your wanted values using [`Fuzz.map`](#map),
as you don't run the risk of rejecting too may values and slowing down your
tests, for example using `Fuzz.intRange 0 5 |> Fuzz.map (\x -> x * 2)` instead
of `Fuzz.intRange 0 9 |> Fuzz.filterMap (\x -> if modBy 2 x == 0 then Just x else Nothing)`.

If you want to generate indefinitely until you find a satisfactory value (with
a risk of infinite loop depending on the predicate), you can use this pattern:

    goodItemFuzzer =
        itemFuzzer
            |> Fuzz.andThen
                (\item ->
                    case f item of
                        Just b ->
                            Fuzz.constant b

                        Nothing ->
                            goodItemFuzzer
                )

-}
filterMap : (a -> Maybe b) -> Fuzzer a -> Fuzzer b
filterMap f fuzzer =
    let
        go : Int -> Fuzzer b
        go rejectionCount =
            if rejectionCount > 15 then
                invalid "Too many values were filtered out"

            else
                fuzzer
                    |> andThen
                        (\value ->
                            case f value of
                                Just b ->
                                    constant b

                                Nothing ->
                                    go (rejectionCount + 1)
                        )
    in
    go 0


{-| Use a generated value to decide what fuzzer to use next.

For example, let's say you want to generate a list of given length.
One (not ideal) possible way to do that is first choosing how many elements
will there be (generating a number), `andThen` generating a list with that many
items:

    Fuzz.intRange 1 10
        |> Fuzz.andThen
            (\length ->
                let
                    go : Int -> List a -> Fuzzer (List a)
                    go left acc =
                        if left <= 0 then
                            Fuzz.constant (List.reverse acc)

                        else
                            itemFuzzer
                                |> Fuzz.andThen (\item -> go (length - 1) (item :: acc))
                in
                go length []
            )

This will work! Different fuzzers will have different PRNG usage patterns
though and will shrink with varying success. The currently best known way to
fuzz a list of items is based on a "flip a coin, `andThen` generate a value and
repeat or end" approach, and is implemented in the [`Fuzz.list`](#list) helpers
in this module. Use them instead of rolling your own list generator!

Think of `andThen` as a generalization of [`Fuzz.map`](#map). Inside
[`Fuzz.map`](#map) you don't have the option to fuzz another value based on
what you already have; inside `andThen` you do.

-}
andThen : (a -> Fuzzer b) -> Fuzzer a -> Fuzzer b
andThen fn (Fuzzer fuzzer) =
    Fuzzer <|
        \prng ->
            case fuzzer prng of
                Generated g ->
                    let
                        (Fuzzer newFuzzer) =
                            fn g.value
                    in
                    newFuzzer g.prng

                Rejected r ->
                    Rejected r


{-| A fuzzer that delays its execution. Handy for recursive types and preventing
infinite recursion.
-}
lazy : (() -> Fuzzer a) -> Fuzzer a
lazy thunk =
    Fuzzer <|
        \prng ->
            let
                (Fuzzer fuzzer) =
                    thunk ()
            in
            fuzzer prng


{-| A fuzzer that shuffles the given list.
-}
shuffledList : List a -> Fuzzer (List a)
shuffledList items =
    items
        |> traverse (\item -> int |> map (\index -> ( index, item )))
        |> map
            (\listWithIndexes ->
                listWithIndexes
                    |> List.sortBy Tuple.first
                    |> List.map Tuple.second
            )


{-| Executes every fuzzer in the list and collects their values into the returned
list.

Rejections (eg. from [`Fuzz.filter`](#filter) or [`Fuzz.invalid`](#invalid))
bubble up instead of being discarded.

-}
sequence : List (Fuzzer a) -> Fuzzer (List a)
sequence fuzzers =
    List.foldr (map2 (::)) (constant []) fuzzers


{-| Runs the Fuzzer-returning function on every item in the list, executes them=
and collects their values into the returned list.

Rejections (eg. from [`Fuzz.filter`](#filter) or [`Fuzz.invalid`](#invalid))
bubble up instead of being discarded.

-}
traverse : (a -> Fuzzer b) -> List a -> Fuzzer (List b)
traverse toFuzzer items =
    sequence (List.map toFuzzer items)


{-| Draw an integer between 0 and n inclusive.

Will simplify towards 0, but draws uniformly over the whole range.

Max supported value is 2^32 - 1.

-}
uniformInt : Int -> Fuzzer Int
uniformInt n =
    rollDice n (Random.int 0 n)


{-| A fuzzer for boolean values, generating True with the given probability
(0.0 = always False, 1.0 = always True).

Probabilities outside the `0..1` range will be clamped to `0..1`.

Simplifies towards False (if not prevented to do that by using probability >= 1).

-}
weightedBool : Float -> Fuzzer Bool
weightedBool p =
    (if p <= 0 then
        forcedChoice 0

     else if p >= 1 then
        forcedChoice 1

     else
        rollDice 1 (weightedBoolGenerator p)
    )
        |> map intToBool


{-| This is the only place that accepts Random.Generators.
And only Int ones at that!

This is because our underlying implementation is a sequence of Ints (RandomRun).
All other generated values (Floats, Bools, ...) have to be somehow mapped to
one or more Ints.

Based on the PRNG value, this function:

  - either draws and remembers a random number (PRNG.Random)
  - or picks a number from the hardcoded list. (PRNG.Hardcoded)

-}
rollDice : Int -> Random.Generator Int -> Fuzzer Int
rollDice maxValue diceGenerator =
    Fuzzer <|
        \prng ->
            case prng of
                Random r ->
                    let
                        ( diceRoll, newSeed ) =
                            Random.step diceGenerator r.seed
                    in
                    if diceRoll < 0 then
                        Rejected
                            { reason = "elm-test bug: generated a choice < 0"
                            , prng = prng
                            }

                    else if diceRoll > maxValue then
                        Rejected
                            { reason = "elm-test bug: generated a choice > maxChoice"
                            , prng = prng
                            }

                    else
                        Generated
                            { value = diceRoll
                            , prng =
                                Random
                                    { seed = newSeed
                                    , run = RandomRun.append diceRoll r.run
                                    }
                            }

                Hardcoded h ->
                    case RandomRun.nextChoice h.unusedPart of
                        Nothing ->
                            -- This happens if we simplified too much / in an incompatible way
                            Rejected
                                { reason = "elm-test internals: hardcoded PRNG run out of numbers"
                                , prng = prng
                                }

                        Just ( hardcodedChoice, restOfChoices ) ->
                            if hardcodedChoice < 0 then
                                -- This happens eg. when decrementing after delete shrink
                                Rejected
                                    { reason = "elm-test internals: generated a choice < 0"
                                    , prng = prng
                                    }

                            else if hardcodedChoice > maxValue then
                                -- This happens eg. when redistributing choices
                                Rejected
                                    { reason = "elm-test internals: generated a choice > maxChoice"
                                    , prng = prng
                                    }

                            else
                                Generated
                                    { value = hardcodedChoice
                                    , prng = Hardcoded { h | unusedPart = restOfChoices }
                                    }


forcedChoice : Int -> Fuzzer Int
forcedChoice n =
    Fuzzer <|
        \prng ->
            if n < 0 then
                Rejected
                    { reason = "elm-test bug: forcedChoice: n < 0"
                    , prng = prng
                    }

            else
                case prng of
                    Random r ->
                        Generated
                            { value = n
                            , prng = Random { r | run = RandomRun.append n r.run }
                            }

                    Hardcoded h ->
                        case RandomRun.nextChoice h.unusedPart of
                            Nothing ->
                                -- This happens if we simplified too much / in an incompatible way
                                Rejected
                                    { reason = "elm-test internals: hardcoded PRNG run out of numbers"
                                    , prng = prng
                                    }

                            Just ( hardcodedChoice, restOfChoices ) ->
                                if hardcodedChoice /= n then
                                    Rejected
                                        { reason = "elm-test internals: hardcoded value was not the same as the forced one"
                                        , prng = prng
                                        }

                                else
                                    Generated
                                        { value = n
                                        , prng = Hardcoded { h | unusedPart = restOfChoices }
                                        }


{-| We could golf this to ((/=) 0) but this is perhaps more readable.
-}
intToBool : Int -> Bool
intToBool n =
    if n == 0 then
        False

    else
        True


weightedBoolGenerator : Float -> Random.Generator Int
weightedBoolGenerator p =
    Random.float 0 1
        |> Random.map
            (\f ->
                if f <= p then
                    1

                else
                    0
            )


intFrequencyGenerator : Int -> List Int -> Random.Generator Int
intFrequencyGenerator w1 ws =
    Random.weighted
        ( toFloat w1, 0 )
        (List.indexedMap (\i w -> ( toFloat w, i + 1 )) ws)


{-| Generate a few example values from the fuzzer.

Useful in REPL:

    > import Fuzz
    > Fuzz.examples 20 (Fuzz.intRange 20 50)
    [42,45,32,26,33,29,41,45,23,45,34,23,22,42,29,27,41,43,30,50]
        : List Int

Uses the first argument as the seed as well as the count of examples to generate.

Will return an empty list in case of rejection.

-}
examples : Int -> Fuzzer a -> List a
examples n fuzzer =
    case
        Fuzz.Internal.generate
            (PRNG.random (Random.initialSeed n))
            (listOfLength n fuzzer)
    of
        Generated { value } ->
            value

        Rejected _ ->
            []


{-| Show examples of values satisfying given classification predicates (see
also [`Test.reportDistribution`](Test#reportDistribution) and
[`Test.expectDistribution`](Test#expectDistribution)).

Generates a given number of values and classifies them based on the predicates.

Uses the first argument as the seed as well as the count of examples to
generate.

This function will always return all the given "base" labels, even if no
examples of them could be found:

    Fuzz.labelExamples 100
        [ ( "Lower boundary (1)", \n -> n == 1 )
        , ( "Upper boundary (20)", \n -> n == 20 )
        , ( "In the middle (2..19)", \n -> n > 1 && n < 20 )
        , ( "Outside boundaries??", \n -> n < 1 || n > 20 )
        ]
        (Fuzz.intRange 1 20)

    -->
    [ ( [ "Lower boundary (1)" ], Just 1 )
    , ( [ "Upper boundary (20)" ], Just 20 )
    , ( [ "In the middle (2..19)" ], Just 5 )
    , ( [ "Outside boundaries??" ], Nothing )
    ]

In case of predicate overlap (eg. something is both green and big) this
function will also return all the found combinations:

    Fuzz.labelExamples 100
        [ ( "fizz", \n -> (n |> modBy 3) == 0 )
        , ( "buzz", \n -> (n |> modBy 5) == 0 )
        ]
        (Fuzz.intRange 1 20)

    -->
    [ ( [ "fizz" ], Just 3 )
    , ( [ "buzz" ], Just 10 )
    , ( [ "fizz, buzz" ], Just 15 )
    ]

-}
labelExamples : Int -> List ( String, a -> Bool ) -> Fuzzer a -> List ( List String, Maybe a )
labelExamples n labels fuzzer =
    case
        Fuzz.Internal.generate
            (PRNG.random (Random.initialSeed n))
            (listOfLength n fuzzer)
    of
        Generated { value } ->
            let
                foundExamples : Dict (List String) a
                foundExamples =
                    value
                        |> List.foldl
                            (\item acc ->
                                let
                                    categories : List String
                                    categories =
                                        labels
                                            |> List.filterMap
                                                (\( label, predicate ) ->
                                                    if predicate item then
                                                        Just label

                                                    else
                                                        Nothing
                                                )
                                in
                                if List.isEmpty categories then
                                    acc

                                else
                                    acc
                                        |> Dict.update categories
                                            (\maybeExample ->
                                                case maybeExample of
                                                    Nothing ->
                                                        Just item

                                                    Just original ->
                                                        Just original
                                            )
                            )
                            Dict.empty

                combinations : List ( List String, a )
                combinations =
                    foundExamples
                        |> Dict.filter (\k _ -> List.length k > 1)
                        |> Dict.toList
            in
            List.filterMap
                (\( label, _ ) ->
                    case Dict.get [ label ] foundExamples of
                        Nothing ->
                            if Dict.any (\k _ -> List.member label k) foundExamples then
                                -- don't show this example: all its occurences were included in combination with some other label
                                Nothing

                            else
                                -- show that we didn't find it (in any combination nor alone)
                                Just ( [ label ], Nothing )

                        Just example ->
                            Just ( [ label ], Just example )
                )
                labels
                ++ List.map (\( label, example ) -> ( label, Just example )) combinations

        Rejected _ ->
            []


{-| (Avoid this function if you can! It is only provided as an escape hatch.)

Convert a Random.Generator into a Fuzzer.

Works internally by generating a random seed and running `Random.step`.

Note this will not shrink well (in fact it will shrink randomly, to smaller
_seeds_), as Generators are black boxes from the perspective of Fuzzers. If you
want meaningful shrinking, define fuzzers using the other functions in this
module!

-}
fromGenerator : Random.Generator a -> Fuzzer a
fromGenerator generator =
    int32
        |> map
            (\seed ->
                Random.step generator (Random.initialSeed seed)
                    |> Tuple.first
            )
