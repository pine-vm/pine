module String exposing
    ( String
    , all
    , any
    , append
    , concat
    , cons
    , contains
    , dropLeft
    , dropRight
    , endsWith
    , filter
    , foldl
    , foldr
    , fromChar
    , fromFloat
    , fromInt
    , fromList
    , indexes
    , indices
    , isEmpty
    , join
    , left
    , length
    , lines
    , map
    , pad
    , padLeft
    , padRight
    , repeat
    , replace
    , reverse
    , right
    , slice
    , split
    , startsWith
    , toFloat
    , toInt
    , toList
    , toLower
    , toUpper
    , trim
    , trimLeft
    , trimRight
    , uncons
    , words
    )

import Basics
import Char
import List exposing ((::))
import Maybe exposing (Maybe)
import Tuple


type String
    = String (List Char.Char)


type Elm_Float
    = Elm_Float Int Int
      -- We need another tag to prevent the compiler from assuming that the condition for tag 'String' is always true.
    | AnyOtherKind_Float


toList : String -> List Char
toList (String chars) =
    chars


fromList : List Char -> String
fromList chars =
    String chars


fromChar : Char -> String
fromChar char =
    String [ char ]


cons : Char -> String -> String
cons char (String string) =
    String (char :: string)


uncons : String -> Maybe ( Char, String )
uncons (String chars) =
    case chars of
        [] ->
            Nothing

        first :: rest ->
            Just ( first, String rest )


isEmpty : String -> Bool
isEmpty (String chars) =
    Pine_kernel.equal [ chars, [] ]


length : String -> Int
length (String chars) =
    Pine_kernel.length chars


reverse : String -> String
reverse (String chars) =
    String (Pine_kernel.reverse chars)


foldl : (Char -> b -> b) -> b -> String -> b
foldl func acc (String chars) =
    foldlChars func acc chars


foldlChars : (Char -> b -> b) -> b -> List Char -> b
foldlChars func acc chars =
    let
        nextChar =
            Pine_kernel.head chars
    in
    if Pine_kernel.equal [ nextChar, [] ] then
        acc

    else
        foldlChars func (func nextChar acc) (Pine_kernel.skip [ 1, chars ])


foldr : (Char -> b -> b) -> b -> String -> b
foldr func acc (String chars) =
    foldlChars func acc (Pine_kernel.reverse chars)


map : (Char -> Char) -> String -> String
map func (String chars) =
    String (List.map func chars)


repeat : Int -> String -> String
repeat n (String chars) =
    String (Pine_kernel.concat (List.repeat n chars))


replace : String -> String -> String -> String
replace before after string =
    join after (split before string)


append : String -> String -> String
append (String a) (String b) =
    String (Pine_kernel.concat [ a, b ])


concat : List String -> String
concat strings =
    let
        charsLists =
            List.map toList strings
    in
    String (Pine_kernel.concat charsLists)


split : String -> String -> List String
split (String sep) (String string) =
    if Pine_kernel.equal [ sep, [] ] then
        List.map fromChar string

    else
        splitHelperOnList 0 [] 0 sep string


splitHelperOnList : Int -> List String -> Int -> List Char -> List Char -> List String
splitHelperOnList offset collected lastStart sep string =
    let
        slice : List Char
        slice =
            Pine_kernel.take
                [ Pine_kernel.length sep
                , Pine_kernel.skip [ offset, string ]
                ]
    in
    if Pine_kernel.equal [ slice, sep ] then
        let
            separatedSliceLength : Int
            separatedSliceLength =
                Pine_kernel.int_add
                    [ offset
                    , Pine_kernel.int_mul [ -1, lastStart ]
                    ]

            separatedSlice : List Char
            separatedSlice =
                Pine_kernel.take
                    [ separatedSliceLength
                    , Pine_kernel.skip [ lastStart, string ]
                    ]
        in
        splitHelperOnList
            (Pine_kernel.int_add [ offset, Pine_kernel.length sep ])
            (Pine_kernel.concat [ collected, [ String separatedSlice ] ])
            (Pine_kernel.int_add [ offset, Pine_kernel.length sep ])
            sep
            string

    else if Pine_kernel.equal [ slice, [] ] then
        let
            separatedSlice : List Char
            separatedSlice =
                Pine_kernel.skip [ lastStart, string ]
        in
        Pine_kernel.concat [ collected, [ String separatedSlice ] ]

    else
        splitHelperOnList
            (Pine_kernel.int_add [ offset, 1 ])
            collected
            lastStart
            sep
            string


join : String -> List String -> String
join (String sepList) chunks =
    let
        charsLists =
            List.intersperse
                sepList
                (List.map toList chunks)
    in
    String (Pine_kernel.concat charsLists)


slice : Int -> Int -> String -> String
slice start end (String chars) =
    let
        absoluteIndex relativeIndex =
            {-
               Instead of using integer comparison together with the literal 0,
               check the first byte if the sign is negative.
            -}
            if
                Pine_kernel.equal
                    [ Pine_kernel.take [ 1, relativeIndex ]
                    , Pine_kernel.take [ 1, -1 ]
                    ]
            then
                Pine_kernel.int_add [ relativeIndex, Pine_kernel.length chars ]

            else
                relativeIndex

        absoluteStart : Int
        absoluteStart =
            absoluteIndex start

        sliceLength : Int
        sliceLength =
            Pine_kernel.int_add
                [ absoluteIndex end
                , Pine_kernel.int_mul [ -1, absoluteStart ]
                ]
    in
    String
        (Pine_kernel.take
            [ sliceLength
            , Pine_kernel.skip [ absoluteStart, chars ]
            ]
        )


left : Int -> String -> String
left n (String chars) =
    String (List.take n chars)


right : Int -> String -> String
right n string =
    if Pine_kernel.int_is_sorted_asc [ n, 0 ] then
        ""

    else
        slice -n (length string) string


dropLeft : Int -> String -> String
dropLeft n (String chars) =
    String (List.drop n chars)


dropRight : Int -> String -> String
dropRight n string =
    if Pine_kernel.int_is_sorted_asc [ n, 0 ] then
        string

    else
        slice 0 -n string


contains : String -> String -> Bool
contains (String patternList) (String stringList) =
    if Pine_kernel.equal [ patternList, [] ] then
        True

    else
        containsOnList patternList stringList


containsOnList : List Char -> List Char -> Bool
containsOnList pattern string =
    if
        Pine_kernel.equal
            [ Pine_kernel.take [ Pine_kernel.length pattern, string ]
            , pattern
            ]
    then
        True

    else if Pine_kernel.int_is_sorted_asc [ Pine_kernel.length string, Pine_kernel.length pattern ] then
        False

    else
        containsOnList pattern (Pine_kernel.skip [ 1, string ])


startsWith : String -> String -> Bool
startsWith (String patternList) (String stringList) =
    Pine_kernel.equal
        [ Pine_kernel.take [ Pine_kernel.length patternList, stringList ]
        , patternList
        ]


endsWith : String -> String -> Bool
endsWith pattern string =
    Pine_kernel.equal
        [ right (length pattern) string
        , pattern
        ]


toInt : String -> Maybe Int
toInt (String chars) =
    toIntFromList chars


fromInt : Int -> String
fromInt int =
    String (fromIntAsList int)


toIntFromList : List Char -> Maybe Int
toIntFromList stringAsList =
    let
        firstChar =
            Pine_kernel.head stringAsList
    in
    if Pine_kernel.equal [ firstChar, [] ] then
        Nothing

    else
        let
            ( valueString, signMultiplier ) =
                case firstChar of
                    '-' ->
                        ( Pine_kernel.skip [ 1, stringAsList ], -1 )

                    '+' ->
                        ( Pine_kernel.skip [ 1, stringAsList ], 1 )

                    _ ->
                        ( stringAsList, 1 )
        in
        if Pine_kernel.equal [ valueString, [] ] then
            Nothing

        else
            case toUnsignedIntFromList 0 valueString of
                Just unsigned ->
                    Just (Pine_kernel.int_mul [ signMultiplier, unsigned ])

                Nothing ->
                    Nothing


toUnsignedIntFromList : Int -> List Char -> Maybe Int
toUnsignedIntFromList upper chars =
    let
        char =
            Pine_kernel.head chars
    in
    if Pine_kernel.equal [ char, [] ] then
        Just upper

    else
        case digitValueFromChar char of
            Nothing ->
                Nothing

            Just digitValue ->
                toUnsignedIntFromList
                    (Pine_kernel.int_add [ digitValue, Pine_kernel.int_mul [ upper, 10 ] ])
                    (Pine_kernel.skip [ 1, chars ])


digitValueFromChar : Char -> Maybe Int
digitValueFromChar char =
    case char of
        '0' ->
            Just 0

        '1' ->
            Just 1

        '2' ->
            Just 2

        '3' ->
            Just 3

        '4' ->
            Just 4

        '5' ->
            Just 5

        '6' ->
            Just 6

        '7' ->
            Just 7

        '8' ->
            Just 8

        '9' ->
            Just 9

        _ ->
            Nothing


fromIntAsList : Int -> List Char
fromIntAsList int =
    if Pine_kernel.int_is_sorted_asc [ 0, int ] then
        fromUnsignedIntAsList int

    else
        Pine_kernel.concat [ [ '-' ], fromUnsignedIntAsList -int ]


fromUnsignedIntAsList : Int -> List Char
fromUnsignedIntAsList int =
    fromUnsignedIntAsListHelper int []


fromUnsignedIntAsListHelper : Int -> List Char -> List Char
fromUnsignedIntAsListHelper int lowerDigits =
    if Pine_kernel.int_is_sorted_asc [ int, 0 ] then
        if lowerDigits == [] then
            [ '0' ]

        else
            lowerDigits

    else
        let
            upperDigitsValue : Int
            upperDigitsValue =
                int // 10

            digitChar =
                unsafeDigitCharacterFromValue
                    (Pine_kernel.int_add
                        [ int
                        , Pine_kernel.int_mul [ upperDigitsValue, -10 ]
                        ]
                    )
        in
        fromUnsignedIntAsListHelper upperDigitsValue (digitChar :: lowerDigits)


unsafeDigitCharacterFromValue : Int -> Char
unsafeDigitCharacterFromValue digitValue =
    case digitValue of
        0 ->
            '0'

        1 ->
            '1'

        2 ->
            '2'

        3 ->
            '3'

        4 ->
            '4'

        5 ->
            '5'

        6 ->
            '6'

        7 ->
            '7'

        8 ->
            '8'

        9 ->
            '9'

        _ ->
            unsafeDigitCharacterFromValue digitValue


trim : String -> String
trim (String chars) =
    String
        (dropWhileList isCharRemovedOnTrim
            (List.reverse (dropWhileList isCharRemovedOnTrim (List.reverse chars)))
        )


trimLeft : String -> String
trimLeft (String chars) =
    String (dropWhileList isCharRemovedOnTrim chars)


trimRight : String -> String
trimRight (String chars) =
    String (List.reverse (dropWhileList isCharRemovedOnTrim (List.reverse chars)))


isCharRemovedOnTrim : Char -> Bool
isCharRemovedOnTrim char =
    if Pine_kernel.equal [ char, ' ' ] then
        True

    else if Pine_kernel.equal [ char, '\t' ] then
        True

    else if Pine_kernel.equal [ char, '\n' ] then
        True

    else if Pine_kernel.equal [ char, '\u{000D}' ] then
        True

    else
        False


dropWhileList : (Char -> Bool) -> List Char -> List Char
dropWhileList predicate stringList =
    case stringList of
        [] ->
            []

        char :: rest ->
            if predicate char then
                dropWhileList predicate rest

            else
                stringList


padLeft : Int -> Char -> String -> String
padLeft n char (String list) =
    String
        (Pine_kernel.concat [ List.repeat (n - Pine_kernel.length list) char, list ])


lines : String -> List String
lines (String chars) =
    linesHelper 0 [] 0 chars


linesHelper : Int -> List String -> Int -> List Char -> List String
linesHelper currentLineStart currentLines offset chars =
    let
        nextChar =
            Pine_kernel.head (Pine_kernel.skip [ offset, chars ])

        nextTwoChars =
            Pine_kernel.take [ 2, Pine_kernel.skip [ offset, chars ] ]
    in
    if Pine_kernel.equal [ nextChar, [] ] then
        let
            currentLineLength =
                Pine_kernel.int_add [ offset, -currentLineStart ]

            currentLineChars : List Char
            currentLineChars =
                Pine_kernel.take
                    [ currentLineLength
                    , Pine_kernel.skip [ currentLineStart, chars ]
                    ]
        in
        Pine_kernel.concat
            [ currentLines
            , [ String (Pine_kernel.skip [ currentLineStart, chars ]) ]
            ]

    else if Pine_kernel.equal [ nextTwoChars, [ '\u{000D}', '\n' ] ] then
        let
            currentLineLength =
                Pine_kernel.int_add [ offset, -currentLineStart ]

            currentLineChars : List Char
            currentLineChars =
                Pine_kernel.take
                    [ currentLineLength
                    , Pine_kernel.skip [ currentLineStart, chars ]
                    ]
        in
        linesHelper
            (Pine_kernel.int_add [ offset, 2 ])
            (Pine_kernel.concat [ currentLines, [ String currentLineChars ] ])
            (Pine_kernel.int_add [ offset, 2 ])
            chars

    else if Pine_kernel.equal [ nextTwoChars, [ '\n', '\u{000D}' ] ] then
        let
            currentLineLength =
                Pine_kernel.int_add [ offset, -currentLineStart ]

            currentLineChars : List Char
            currentLineChars =
                Pine_kernel.take
                    [ currentLineLength
                    , Pine_kernel.skip [ currentLineStart, chars ]
                    ]
        in
        linesHelper
            (Pine_kernel.int_add [ offset, 2 ])
            (Pine_kernel.concat [ currentLines, [ String currentLineChars ] ])
            (Pine_kernel.int_add [ offset, 2 ])
            chars

    else if Pine_kernel.equal [ nextChar, '\n' ] then
        let
            currentLineLength =
                Pine_kernel.int_add [ offset, -currentLineStart ]

            currentLineChars : List Char
            currentLineChars =
                Pine_kernel.take
                    [ currentLineLength
                    , Pine_kernel.skip [ currentLineStart, chars ]
                    ]
        in
        linesHelper
            (Pine_kernel.int_add [ offset, 1 ])
            (Pine_kernel.concat [ currentLines, [ String currentLineChars ] ])
            (Pine_kernel.int_add [ offset, 1 ])
            chars

    else if Pine_kernel.equal [ nextChar, '\u{000D}' ] then
        let
            currentLineLength =
                Pine_kernel.int_add [ offset, -currentLineStart ]

            currentLineChars : List Char
            currentLineChars =
                Pine_kernel.take
                    [ currentLineLength
                    , Pine_kernel.skip [ currentLineStart, chars ]
                    ]
        in
        linesHelper
            (Pine_kernel.int_add [ offset, 1 ])
            (Pine_kernel.concat [ currentLines, [ String currentLineChars ] ])
            (Pine_kernel.int_add [ offset, 1 ])
            chars

    else
        linesHelper
            currentLineStart
            currentLines
            (Pine_kernel.int_add [ offset, 1 ])
            chars


words : String -> List String
words (String chars) =
    wordsHelper 0 [] 0 chars


wordsHelper : Int -> List String -> Int -> List Char -> List String
wordsHelper currentWordStart currentWords offset chars =
    let
        nextChar =
            Pine_kernel.head (Pine_kernel.skip [ offset, chars ])
    in
    if Pine_kernel.equal [ nextChar, [] ] then
        let
            currentWordLength : Int
            currentWordLength =
                Pine_kernel.int_add
                    [ offset
                    , Pine_kernel.int_mul [ currentWordStart, -1 ]
                    ]

            currentWordChars : List Char
            currentWordChars =
                Pine_kernel.take
                    [ currentWordLength
                    , Pine_kernel.skip [ currentWordStart, chars ]
                    ]
        in
        if Pine_kernel.equal [ currentWordChars, [] ] then
            currentWords

        else
            Pine_kernel.concat
                [ currentWords
                , [ String currentWordChars ]
                ]

    else
        let
            currentIsBreak : Bool
            currentIsBreak =
                isCharRemovedOnTrim nextChar
        in
        if currentIsBreak then
            let
                currentWordLength : Int
                currentWordLength =
                    Pine_kernel.int_add
                        [ offset
                        , Pine_kernel.int_mul [ currentWordStart, -1 ]
                        ]

                currentWordChars : List Char
                currentWordChars =
                    Pine_kernel.take
                        [ currentWordLength
                        , Pine_kernel.skip [ currentWordStart, chars ]
                        ]
            in
            if Pine_kernel.equal [ currentWordChars, [] ] then
                wordsHelper
                    (Pine_kernel.int_add [ offset, 1 ])
                    currentWords
                    (Pine_kernel.int_add [ offset, 1 ])
                    chars

            else
                wordsHelper
                    (Pine_kernel.int_add [ offset, 1 ])
                    (Pine_kernel.concat [ currentWords, [ String currentWordChars ] ])
                    (Pine_kernel.int_add [ offset, 1 ])
                    chars

        else
            wordsHelper
                currentWordStart
                currentWords
                (Pine_kernel.int_add [ offset, 1 ])
                chars


toFloat : String -> Maybe Float
toFloat (String chars) =
    let
        firstChar =
            Pine_kernel.head chars
    in
    if Pine_kernel.equal [ firstChar, [] ] then
        Nothing

    else if Pine_kernel.equal [ firstChar, '-' ] then
        case toRationalComponentsLessSign (Pine_kernel.skip [ 1, chars ]) of
            Nothing ->
                Nothing

            Just (Elm_Float numAbs denom) ->
                let
                    numSigned =
                        if Pine_kernel.equal [ numAbs, 0 ] then
                            0

                        else
                            Pine_kernel.int_mul [ -1, numAbs ]
                in
                Just (Elm_Float numSigned denom)

    else
        case toRationalComponentsLessSign chars of
            Nothing ->
                Nothing

            Just (Elm_Float numAbs denom) ->
                Just (Elm_Float numAbs denom)


toRationalComponentsLessSign : List Char -> Maybe ( Int, Int )
toRationalComponentsLessSign chars =
    case splitHelperOnList 0 [] 0 [ '.' ] chars of
        [] ->
            Nothing

        [ String whole ] ->
            case toUnsignedIntFromList 0 whole of
                Nothing ->
                    Nothing

                Just numerator ->
                    Just (Elm_Float numerator 1)

        [ String beforeSep, String afterSep ] ->
            if Pine_kernel.equal [ afterSep, [] ] then
                if Pine_kernel.equal [ beforeSep, [] ] then
                    Nothing

                else
                    case toUnsignedIntFromList 0 beforeSep of
                        Nothing ->
                            Nothing

                        Just beforeSepInt ->
                            Just (Elm_Float beforeSepInt 1)

            else
                case toUnsignedIntFromList 0 beforeSep of
                    Nothing ->
                        Nothing

                    Just beforeSepInt ->
                        case toUnsignedIntFromList 0 afterSep of
                            Nothing ->
                                Nothing

                            Just afterSepInt ->
                                let
                                    denom =
                                        Basics.pow 10 (Pine_kernel.length afterSep)

                                    numerator =
                                        Pine_kernel.int_add
                                            [ Pine_kernel.int_mul [ beforeSepInt, denom ], afterSepInt ]
                                in
                                Just (Elm_Float numerator denom)

        _ ->
            Nothing


any : (Char -> Bool) -> String -> Bool
any predicate (String chars) =
    charsAny predicate chars


charsAny : (Char -> Bool) -> List Char -> Bool
charsAny predicate chars =
    case chars of
        [] ->
            False

        char :: rest ->
            if predicate char then
                True

            else
                charsAny predicate rest


toUpper : String -> String
toUpper (String chars) =
    String (List.map Char.toUpper chars)


toLower : String -> String
toLower (String chars) =
    String (List.map Char.toLower chars)
