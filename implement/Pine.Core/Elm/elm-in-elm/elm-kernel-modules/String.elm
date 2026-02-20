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
    = String Int


type Elm_Float
    = Elm_Float Int Int
      -- We need another tag to prevent the compiler from assuming that the condition for tag 'String' is always true.
    | AnyOtherKind_Float


toList : String -> List Char
toList (String charsBlob) =
    toListRecursive
        0
        []
        charsBlob


toListRecursive : Int -> List Char -> Int -> List Char
toListRecursive offset list blob =
    let
        nextChar =
            Pine_kernel.take
                [ 4
                , Pine_kernel.skip [ offset, blob ]
                ]
    in
    if Pine_kernel.equal [ Pine_kernel.length nextChar, 0 ] then
        list

    else
        toListRecursive
            (Pine_kernel.int_add [ offset, 4 ])
            (Pine_kernel.concat [ list, [ nextChar ] ])
            blob


fromList : List Char -> String
fromList chars =
    String (Pine_kernel.concat chars)


fromChar : Char -> String
fromChar char =
    String char


cons : Char -> String -> String
cons char (String string) =
    String (Pine_kernel.concat [ char, string ])


uncons : String -> Maybe ( Char, String )
uncons (String chars) =
    if Pine_kernel.equal [ Pine_kernel.length chars, 0 ] then
        Nothing

    else
        Just ( Pine_kernel.take [ 4, chars ], String (Pine_kernel.skip [ 4, chars ]) )


isEmpty : String -> Bool
isEmpty (String chars) =
    Pine_kernel.equal
        [ Pine_kernel.length chars, 0 ]


length : String -> Int
length (String chars) =
    Pine_kernel.concat
        [ Pine_kernel.take [ 1, 0 ]
        , Pine_kernel.bit_shift_right
            [ 2
            , Pine_kernel.skip [ 1, Pine_kernel.length chars ]
            ]
        ]


reverse : String -> String
reverse string =
    fromList (List.reverse (toList string))


foldl : (Char -> b -> b) -> b -> String -> b
foldl func acc string =
    foldlChars
        func
        acc
        (toList string)


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
foldr func acc string =
    foldlChars
        func
        acc
        (List.reverse (toList string))


map : (Char -> Char) -> String -> String
map func (String charsBytes) =
    String
        (charsMap
            0
            (Pine_kernel.take [ 0, charsBytes ])
            func
            charsBytes
        )


charsMap : Int -> Int -> (Char -> Char) -> Int -> Int
charsMap offset mappedBytes func charsBytes =
    let
        char =
            Pine_kernel.take
                [ 4
                , Pine_kernel.skip [ offset, charsBytes ]
                ]
    in
    if Pine_kernel.equal [ Pine_kernel.length char, 0 ] then
        mappedBytes

    else
        let
            mappedChar =
                func char
        in
        charsMap
            (Pine_kernel.int_add [ offset, 4 ])
            (Pine_kernel.concat [ mappedBytes, mappedChar ])
            func
            charsBytes


filter : (Char -> Bool) -> String -> String
filter predicate (String chars) =
    charsFilter
        0
        (Pine_kernel.take [ 0, chars ])
        predicate
        chars


charsFilter : Int -> Int -> (Char -> Bool) -> Int -> String
charsFilter offset charsBytesFiltered predicate charsBytes =
    let
        char =
            Pine_kernel.take [ 4, Pine_kernel.skip [ offset, charsBytes ] ]
    in
    if Pine_kernel.equal [ Pine_kernel.length char, 0 ] then
        String charsBytesFiltered

    else if predicate char then
        charsFilter
            (Pine_kernel.int_add [ offset, 4 ])
            (Pine_kernel.concat [ charsBytesFiltered, char ])
            predicate
            charsBytes

    else
        charsFilter
            (Pine_kernel.int_add [ offset, 4 ])
            charsBytesFiltered
            predicate
            charsBytes


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
        charsBlobs =
            List.map
                (\(String chars) -> chars)
                strings
    in
    String (Pine_kernel.concat charsBlobs)


split : String -> String -> List String
split (String sep) ((String stringBytes) as string) =
    if Pine_kernel.equal [ Pine_kernel.length sep, 0 ] then
        List.map fromChar (toList string)

    else
        splitHelperOnBlob 0 [] 0 sep stringBytes


splitHelperOnBlob : Int -> List String -> Int -> Int -> Int -> List String
splitHelperOnBlob offset collected lastStart sepBytes stringBytes =
    let
        sliceBytes : Int
        sliceBytes =
            Pine_kernel.take
                [ Pine_kernel.length sepBytes
                , Pine_kernel.skip [ offset, stringBytes ]
                ]
    in
    if Pine_kernel.equal [ sliceBytes, sepBytes ] then
        let
            separatedSliceLength : Int
            separatedSliceLength =
                Pine_kernel.int_add
                    [ offset
                    , Pine_kernel.int_mul [ -1, lastStart ]
                    ]

            separatedSlice : Int
            separatedSlice =
                Pine_kernel.take
                    [ separatedSliceLength
                    , Pine_kernel.skip [ lastStart, stringBytes ]
                    ]
        in
        splitHelperOnBlob
            (Pine_kernel.int_add [ offset, Pine_kernel.length sepBytes ])
            (Pine_kernel.concat [ collected, [ String separatedSlice ] ])
            (Pine_kernel.int_add [ offset, Pine_kernel.length sepBytes ])
            sepBytes
            stringBytes

    else if Pine_kernel.equal [ Pine_kernel.length sliceBytes, 0 ] then
        let
            separatedSlice : Int
            separatedSlice =
                Pine_kernel.skip [ lastStart, stringBytes ]
        in
        Pine_kernel.concat [ collected, [ String separatedSlice ] ]

    else
        splitHelperOnBlob
            (Pine_kernel.int_add [ offset, 4 ])
            collected
            lastStart
            sepBytes
            stringBytes


join : String -> List String -> String
join (String sepCharsBytes) chunks =
    let
        charsBytesList =
            Pine_kernel.skip
                [ 1
                , List.concatMap
                    (\(String chars) -> [ sepCharsBytes, chars ])
                    chunks
                ]
    in
    String
        (Pine_kernel.concat charsBytesList)


slice : Int -> Int -> String -> String
slice start end (String charsBlob) =
    if Pine_kernel.int_is_sorted_asc [ 0, start, end ] then
        let
            sliceLength : Int
            sliceLength =
                Pine_kernel.int_add [ end, Pine_kernel.int_mul [ -1, start ] ]
        in
        String
            (Pine_kernel.take
                [ Pine_kernel.int_mul [ sliceLength, 4 ]
                , Pine_kernel.skip
                    [ Pine_kernel.int_mul [ start, 4 ]
                    , charsBlob
                    ]
                ]
            )

    else
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
                    Pine_kernel.int_add [ relativeIndex, Pine_kernel.length charsBlob ]

                else
                    relativeIndex

            absoluteStart : Int
            absoluteStart =
                absoluteIndex
                    (Pine_kernel.int_mul [ start, 4 ])

            sliceLength : Int
            sliceLength =
                Pine_kernel.int_add
                    [ absoluteIndex (Pine_kernel.int_mul [ end, 4 ])
                    , Pine_kernel.int_mul [ -1, absoluteStart ]
                    ]
        in
        String
            (Pine_kernel.take
                [ sliceLength
                , Pine_kernel.skip [ absoluteStart, charsBlob ]
                ]
            )


left : Int -> String -> String
left n (String chars) =
    String
        (Pine_kernel.take
            [ Pine_kernel.int_mul [ n, 4 ]
            , chars
            ]
        )


right : Int -> String -> String
right n string =
    if Pine_kernel.int_is_sorted_asc [ n, 0 ] then
        ""

    else
        slice -n (length string) string


dropLeft : Int -> String -> String
dropLeft n (String chars) =
    String
        (Pine_kernel.skip
            [ Pine_kernel.int_mul [ n, 4 ]
            , chars
            ]
        )


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
        containsOnBlob 0 patternList stringList


containsOnBlob : Int -> Int -> Int -> Bool
containsOnBlob offset patternBytes stringBytes =
    let
        stringSlice =
            Pine_kernel.take
                [ Pine_kernel.length patternBytes
                , Pine_kernel.skip [ offset, stringBytes ]
                ]
    in
    if Pine_kernel.equal [ stringSlice, patternBytes ] then
        True

    else if Pine_kernel.equal [ Pine_kernel.length stringSlice, 0 ] then
        False

    else
        containsOnBlob
            (Pine_kernel.int_add [ offset, 4 ])
            patternBytes
            stringBytes


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
    parseInt chars


fromInt : Int -> String
fromInt int =
    String (Pine_kernel.concat (fromIntAsList int))


parseInt : Int -> Maybe Int
parseInt src =
    let
        nextChar =
            Pine_kernel.take
                [ 4
                , src
                ]
    in
    case nextChar of
        '-' ->
            case parseUnsignedInt src 4 of
                Just unsignedVal ->
                    Just (Pine_kernel.int_mul [ -1, unsignedVal ])

                Nothing ->
                    Nothing

        '+' ->
            parseUnsignedInt src 4

        _ ->
            -- If no minus sign, parse the rest as an unsigned integer
            parseUnsignedInt src 0


parseUnsignedInt : Int -> Int -> Maybe Int
parseUnsignedInt src offset0 =
    case Pine_kernel.take [ 4, Pine_kernel.skip [ offset0, src ] ] of
        '0' ->
            parseUnsignedIntRec 0 src (Pine_kernel.int_add [ offset0, 4 ])

        '1' ->
            parseUnsignedIntRec 1 src (Pine_kernel.int_add [ offset0, 4 ])

        '2' ->
            parseUnsignedIntRec 2 src (Pine_kernel.int_add [ offset0, 4 ])

        '3' ->
            parseUnsignedIntRec 3 src (Pine_kernel.int_add [ offset0, 4 ])

        '4' ->
            parseUnsignedIntRec 4 src (Pine_kernel.int_add [ offset0, 4 ])

        '5' ->
            parseUnsignedIntRec 5 src (Pine_kernel.int_add [ offset0, 4 ])

        '6' ->
            parseUnsignedIntRec 6 src (Pine_kernel.int_add [ offset0, 4 ])

        '7' ->
            parseUnsignedIntRec 7 src (Pine_kernel.int_add [ offset0, 4 ])

        '8' ->
            parseUnsignedIntRec 8 src (Pine_kernel.int_add [ offset0, 4 ])

        '9' ->
            parseUnsignedIntRec 9 src (Pine_kernel.int_add [ offset0, 4 ])

        _ ->
            Nothing


parseUnsignedIntRec : Int -> Int -> Int -> Maybe Int
parseUnsignedIntRec upper src offset0 =
    let
        nextChar =
            Pine_kernel.take [ 4, Pine_kernel.skip [ offset0, src ] ]
    in
    if Pine_kernel.equal [ Pine_kernel.length nextChar, 0 ] then
        Just upper

    else if Pine_kernel.equal [ nextChar, '0' ] then
        parseUnsignedIntRec (Pine_kernel.int_mul [ upper, 10 ]) src (Pine_kernel.int_add [ offset0, 4 ])

    else if Pine_kernel.equal [ nextChar, '1' ] then
        parseUnsignedIntRec (Pine_kernel.int_add [ Pine_kernel.int_mul [ upper, 10 ], 1 ]) src (Pine_kernel.int_add [ offset0, 4 ])

    else if Pine_kernel.equal [ nextChar, '2' ] then
        parseUnsignedIntRec (Pine_kernel.int_add [ Pine_kernel.int_mul [ upper, 10 ], 2 ]) src (Pine_kernel.int_add [ offset0, 4 ])

    else if Pine_kernel.equal [ nextChar, '3' ] then
        parseUnsignedIntRec (Pine_kernel.int_add [ Pine_kernel.int_mul [ upper, 10 ], 3 ]) src (Pine_kernel.int_add [ offset0, 4 ])

    else if Pine_kernel.equal [ nextChar, '4' ] then
        parseUnsignedIntRec (Pine_kernel.int_add [ Pine_kernel.int_mul [ upper, 10 ], 4 ]) src (Pine_kernel.int_add [ offset0, 4 ])

    else if Pine_kernel.equal [ nextChar, '5' ] then
        parseUnsignedIntRec (Pine_kernel.int_add [ Pine_kernel.int_mul [ upper, 10 ], 5 ]) src (Pine_kernel.int_add [ offset0, 4 ])

    else if Pine_kernel.equal [ nextChar, '6' ] then
        parseUnsignedIntRec (Pine_kernel.int_add [ Pine_kernel.int_mul [ upper, 10 ], 6 ]) src (Pine_kernel.int_add [ offset0, 4 ])

    else if Pine_kernel.equal [ nextChar, '7' ] then
        parseUnsignedIntRec (Pine_kernel.int_add [ Pine_kernel.int_mul [ upper, 10 ], 7 ]) src (Pine_kernel.int_add [ offset0, 4 ])

    else if Pine_kernel.equal [ nextChar, '8' ] then
        parseUnsignedIntRec (Pine_kernel.int_add [ Pine_kernel.int_mul [ upper, 10 ], 8 ]) src (Pine_kernel.int_add [ offset0, 4 ])

    else if Pine_kernel.equal [ nextChar, '9' ] then
        parseUnsignedIntRec (Pine_kernel.int_add [ Pine_kernel.int_mul [ upper, 10 ], 9 ]) src (Pine_kernel.int_add [ offset0, 4 ])

    else
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
    let
        leftTrimmedCount : Int
        leftTrimmedCount =
            trimLeftCountBytesTrimmed 0 chars

        rightRemainingLength : Int
        rightRemainingLength =
            trimRightCountBytesRemaining
                (Pine_kernel.length chars)
                chars
    in
    String
        (Pine_kernel.skip
            [ leftTrimmedCount
            , Pine_kernel.take
                [ rightRemainingLength
                , chars
                ]
            ]
        )


trimLeft : String -> String
trimLeft (String chars) =
    let
        trimmedCount : Int
        trimmedCount =
            trimLeftCountBytesTrimmed 0 chars
    in
    String
        (Pine_kernel.skip
            [ trimmedCount
            , chars
            ]
        )


trimRight : String -> String
trimRight (String chars) =
    let
        remainingLength : Int
        remainingLength =
            trimRightCountBytesRemaining
                (Pine_kernel.length chars)
                chars
    in
    String
        (Pine_kernel.take
            [ remainingLength
            , chars
            ]
        )


trimLeftCountBytesTrimmed : Int -> Int -> Int
trimLeftCountBytesTrimmed offset charsBytes =
    let
        nextCharBytes =
            Pine_kernel.take
                [ 4
                , Pine_kernel.skip [ offset, charsBytes ]
                ]
    in
    if Pine_kernel.equal [ Pine_kernel.length nextCharBytes, 0 ] then
        offset

    else if isCharRemovedOnTrim nextCharBytes then
        trimLeftCountBytesTrimmed
            (Pine_kernel.int_add [ offset, 4 ])
            charsBytes

    else
        offset


trimRightCountBytesRemaining : Int -> Int -> Int
trimRightCountBytesRemaining remainingLength charsBytes =
    if Pine_kernel.equal [ remainingLength, 0 ] then
        0

    else
        let
            char =
                Pine_kernel.take
                    [ 4
                    , Pine_kernel.skip
                        [ Pine_kernel.int_add [ remainingLength, -4 ]
                        , charsBytes
                        ]
                    ]
        in
        if isCharRemovedOnTrim char then
            trimRightCountBytesRemaining
                (Pine_kernel.int_add [ remainingLength, -4 ])
                charsBytes

        else
            remainingLength


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

    else if Pine_kernel.equal [ char, '\u{00A0}' ] then
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
padLeft n char ((String charsBytes) as string) =
    let
        stringBytesLength : Int
        stringBytesLength =
            Pine_kernel.length charsBytes

        stringLength : Int
        stringLength =
            Pine_kernel.concat
                [ Pine_kernel.take [ 1, 0 ]
                , Pine_kernel.bit_shift_right [ 2, Pine_kernel.skip [ 1, stringBytesLength ] ]
                ]

        paddingLength : Int
        paddingLength =
            Pine_kernel.int_add
                [ n
                , Pine_kernel.int_mul [ stringLength, -1 ]
                ]
    in
    if Pine_kernel.int_is_sorted_asc [ paddingLength, 0 ] then
        string

    else
        String
            (Pine_kernel.concat
                [ Pine_kernel.concat (List.repeat paddingLength char)
                , charsBytes
                ]
            )


padRight : Int -> Char -> String -> String
padRight n char ((String charsBytes) as string) =
    let
        stringBytesLength : Int
        stringBytesLength =
            Pine_kernel.length charsBytes

        stringLength : Int
        stringLength =
            Pine_kernel.concat
                [ Pine_kernel.take [ 1, 0 ]
                , Pine_kernel.bit_shift_right [ 2, Pine_kernel.skip [ 1, stringBytesLength ] ]
                ]

        paddingLength : Int
        paddingLength =
            Pine_kernel.int_add
                [ n
                , Pine_kernel.int_mul [ stringLength, -1 ]
                ]
    in
    if Pine_kernel.int_is_sorted_asc [ paddingLength, 0 ] then
        string

    else
        String
            (Pine_kernel.concat
                [ charsBytes
                , Pine_kernel.concat (List.repeat paddingLength char)
                ]
            )


pad : Int -> Char -> String -> String
pad n char ((String charsBytes) as string) =
    let
        stringBytesLength : Int
        stringBytesLength =
            Pine_kernel.length charsBytes

        stringLength : Int
        stringLength =
            Pine_kernel.concat
                [ Pine_kernel.take [ 1, 0 ]
                , Pine_kernel.bit_shift_right [ 2, Pine_kernel.skip [ 1, stringBytesLength ] ]
                ]

        totalPadding : Int
        totalPadding =
            Pine_kernel.int_add
                [ n
                , Pine_kernel.int_mul [ stringLength, -1 ]
                ]

        rightPadding : Int
        rightPadding =
            totalPadding // 2

        leftPadding : Int
        leftPadding =
            Pine_kernel.int_add
                [ totalPadding
                , Pine_kernel.int_mul [ rightPadding, -1 ]
                ]
    in
    if Pine_kernel.int_is_sorted_asc [ totalPadding, 0 ] then
        string

    else
        String
            (Pine_kernel.concat
                [ Pine_kernel.concat (List.repeat leftPadding char)
                , charsBytes
                , Pine_kernel.concat (List.repeat rightPadding char)
                ]
            )


lines : String -> List String
lines (String chars) =
    linesHelper 0 [] 0 chars


linesHelper : Int -> List String -> Int -> Int -> List String
linesHelper currentLineStart currentLines offset charsBytes =
    let
        nextChar =
            Pine_kernel.take
                [ 4
                , Pine_kernel.skip [ offset, charsBytes ]
                ]

        nextTwoChars =
            Pine_kernel.take
                [ 8
                , Pine_kernel.skip [ offset, charsBytes ]
                ]
    in
    if Pine_kernel.equal [ Pine_kernel.length nextChar, 0 ] then
        let
            currentLineLength =
                Pine_kernel.int_add [ offset, -currentLineStart ]
        in
        Pine_kernel.concat
            [ currentLines
            , [ String (Pine_kernel.skip [ currentLineStart, charsBytes ]) ]
            ]

    else if Pine_kernel.equal [ nextTwoChars, Pine_kernel.concat [ '\u{000D}', '\n' ] ] then
        let
            currentLineLength =
                Pine_kernel.int_add [ offset, -currentLineStart ]

            currentLineChars : Int
            currentLineChars =
                Pine_kernel.take
                    [ currentLineLength
                    , Pine_kernel.skip [ currentLineStart, charsBytes ]
                    ]
        in
        linesHelper
            (Pine_kernel.int_add [ offset, 8 ])
            (Pine_kernel.concat [ currentLines, [ String currentLineChars ] ])
            (Pine_kernel.int_add [ offset, 8 ])
            charsBytes

    else if Pine_kernel.equal [ nextChar, '\n' ] then
        let
            currentLineLength =
                Pine_kernel.int_add [ offset, -currentLineStart ]

            currentLineChars : Int
            currentLineChars =
                Pine_kernel.take
                    [ currentLineLength
                    , Pine_kernel.skip [ currentLineStart, charsBytes ]
                    ]
        in
        linesHelper
            (Pine_kernel.int_add [ offset, 4 ])
            (Pine_kernel.concat [ currentLines, [ String currentLineChars ] ])
            (Pine_kernel.int_add [ offset, 4 ])
            charsBytes

    else if Pine_kernel.equal [ nextChar, '\u{000D}' ] then
        let
            currentLineLength =
                Pine_kernel.int_add [ offset, -currentLineStart ]

            currentLineChars : Int
            currentLineChars =
                Pine_kernel.take
                    [ currentLineLength
                    , Pine_kernel.skip [ currentLineStart, charsBytes ]
                    ]
        in
        linesHelper
            (Pine_kernel.int_add [ offset, 4 ])
            (Pine_kernel.concat [ currentLines, [ String currentLineChars ] ])
            (Pine_kernel.int_add [ offset, 4 ])
            charsBytes

    else
        linesHelper
            currentLineStart
            currentLines
            (Pine_kernel.int_add [ offset, 4 ])
            charsBytes


words : String -> List String
words string =
    wordsHelper 0 [] 0 (toList string)


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
toFloat (String charsBlob) =
    let
        firstChar =
            Pine_kernel.take [ 4, charsBlob ]
    in
    if Pine_kernel.equal [ Pine_kernel.length firstChar, 0 ] then
        Nothing

    else if Pine_kernel.equal [ firstChar, '-' ] then
        case toRationalComponentsLessSign (Pine_kernel.skip [ 4, charsBlob ]) of
            Nothing ->
                Nothing

            Just (Elm_Float numAbs denom) ->
                let
                    numSigned : Int
                    numSigned =
                        Pine_kernel.int_mul [ -1, numAbs ]
                in
                Just (Elm_Float numSigned denom)

    else
        case toRationalComponentsLessSign charsBlob of
            Nothing ->
                Nothing

            Just (Elm_Float numAbs denom) ->
                Just (Elm_Float numAbs denom)


fromFloat : Float -> String
fromFloat float =
    case float of
        Elm_Float numerator denom ->
            fromFloatDecimal 16 ( numerator, denom )

        int ->
            fromInt int


fromFloatDecimal : Int -> ( Int, Int ) -> String
fromFloatDecimal decimalPlacesMax ( numerator, denom ) =
    case denom of
        1 ->
            fromInt numerator

        0 ->
            if
                Pine_kernel.equal
                    [ Pine_kernel.take [ 1, numerator ]
                    , Pine_kernel.take [ 1, -1 ]
                    ]
            then
                "-Infinity"

            else
                "Infinity"

        _ ->
            let
                isNegative : Bool
                isNegative =
                    Pine_kernel.equal
                        [ Pine_kernel.take [ 1, numerator ]
                        , Pine_kernel.take [ 1, -1 ]
                        ]

                ( signStr, absNum ) =
                    if isNegative then
                        ( [ '-' ]
                        , Pine_kernel.int_mul [ -1, numerator ]
                        )

                    else
                        ( []
                        , numerator
                        )

                intPart : Int
                intPart =
                    absNum // denom

                remainder : Int
                remainder =
                    modBy denom absNum
            in
            if Pine_kernel.equal [ remainder, 0 ] || Pine_kernel.equal [ decimalPlacesMax, 0 ] then
                -- No remainder OR no decimal places requested
                String
                    (Pine_kernel.concat
                        [ signStr
                        , fromIntAsList intPart
                        ]
                    )

            else
                -- 3) Scale and round remainder to get fractional part
                let
                    scale : Int
                    scale =
                        intPow 1 10 decimalPlacesMax

                    scaledVal : Int
                    scaledVal =
                        Pine_kernel.int_mul [ remainder, scale ]

                    scaledInt : Int
                    scaledInt =
                        scaledVal // denom

                    leftover : Int
                    leftover =
                        modBy denom scaledVal

                    -- 4) ROUND HALF-UP:
                    scaledIntRounded : Int
                    scaledIntRounded =
                        if Pine_kernel.int_is_sorted_asc [ denom, Pine_kernel.int_mul [ leftover, 2 ] ] then
                            Pine_kernel.int_add [ 1, scaledInt ]

                        else
                            scaledInt

                    scaledStr : List Char
                    scaledStr =
                        fromIntAsList scaledIntRounded

                    -- If scaledIntRounded >= scale (e.g. 100 when scale=100),
                    -- that means we "overflowed" into the next integer. For instance:
                    -- fromFloatDecimal 2 (Elm_Float 999 100)
                    -- might round from "9.99" to "10.00".
                    overflowed : Bool
                    overflowed =
                        Pine_kernel.int_is_sorted_asc [ scale, scaledIntRounded ]

                    ( newIntPart, fractionDigits ) =
                        if overflowed then
                            -- increment integer part, fraction becomes e.g. "000"
                            let
                                incremented : Int
                                incremented =
                                    Pine_kernel.int_add [ intPart, 1 ]

                                -- e.g. scaledStr = "100" => dropLeft 1 => "00"
                                -- If scaledStr was "1000" => dropLeft 1 => "000"
                                fractionNoSign : List Char
                                fractionNoSign =
                                    Pine_kernel.skip [ 1, scaledStr ]
                            in
                            ( incremented, fractionNoSign )

                        else
                            -- If not overflowed, we may need zero‐padding to the left.
                            let
                                neededZeros : Int
                                neededZeros =
                                    Pine_kernel.int_add
                                        [ decimalPlacesMax
                                        , Pine_kernel.int_mul [ -1, Pine_kernel.length scaledStr ]
                                        ]

                                fractionNoSign : List Char
                                fractionNoSign =
                                    Pine_kernel.concat
                                        [ List.repeat neededZeros '0'
                                        , scaledStr
                                        ]
                            in
                            ( intPart, fractionNoSign )

                    -- Now remove trailing zeros from fractionDigits.
                    trimmedFraction : List Char
                    trimmedFraction =
                        removeTrailingZeros fractionDigits
                in
                if trimmedFraction == [] then
                    -- Entire fractional part was zeros, so just show an integer.
                    String
                        (Pine_kernel.concat
                            [ signStr
                            , fromIntAsList newIntPart
                            ]
                        )

                else
                    String
                        (Pine_kernel.concat
                            [ signStr
                            , fromIntAsList newIntPart
                            , [ '.' ]
                            , trimmedFraction
                            ]
                        )


removeTrailingZeros : List Char -> List Char
removeTrailingZeros chars =
    removeTrailingZerosHelper (Pine_kernel.length chars) chars


removeTrailingZerosHelper : Int -> List Char -> List Char
removeTrailingZerosHelper offset chars =
    if Pine_kernel.equal [ offset, 0 ] then
        chars

    else
        let
            nextOffset : Int
            nextOffset =
                Pine_kernel.int_add [ offset, -1 ]
        in
        case
            Pine_kernel.take
                [ 1
                , Pine_kernel.skip [ nextOffset, chars ]
                ]
        of
            [ '0' ] ->
                removeTrailingZerosHelper nextOffset chars

            _ ->
                Pine_kernel.take [ offset, chars ]


intPow : Int -> Int -> Int -> Int
intPow acc base exponent =
    if Pine_kernel.int_is_sorted_asc [ exponent, 0 ] then
        acc

    else
        intPow
            (Pine_kernel.int_mul [ acc, base ])
            base
            (Pine_kernel.int_add [ exponent, -1 ])


toRationalComponentsLessSign : Int -> Maybe ( Int, Int )
toRationalComponentsLessSign charsBlob =
    let
        parseWithExponent exponentChar =
            case splitHelperOnBlob 0 [] 0 exponentChar charsBlob of
                [ String mantissa, String exponent ] ->
                    if Pine_kernel.equal [ Pine_kernel.length mantissa, 0 ] then
                        Nothing

                    else if Pine_kernel.equal [ Pine_kernel.length exponent, 0 ] then
                        Nothing

                    else
                        case
                            ( toRationalComponentsWithoutExponent mantissa
                            , parseInt exponent
                            )
                        of
                            ( Just (Elm_Float numerator denom), Just exponentInt ) ->
                                let
                                    exponentIsNonPositive =
                                        Pine_kernel.int_is_sorted_asc [ exponentInt, 0 ]

                                    exponentMagnitude =
                                        if exponentIsNonPositive then
                                            Pine_kernel.int_mul [ -1, exponentInt ]

                                        else
                                            exponentInt

                                    powTen =
                                        intPow 1 10 exponentMagnitude
                                in
                                if exponentIsNonPositive then
                                    Just
                                        (Elm_Float
                                            numerator
                                            (Pine_kernel.int_mul [ denom, powTen ])
                                        )

                                else
                                    Just
                                        (Elm_Float
                                            (Pine_kernel.int_mul [ numerator, powTen ])
                                            denom
                                        )

                            _ ->
                                Nothing

                [ String _ ] ->
                    Nothing

                _ ->
                    Nothing
    in
    case parseWithExponent 'e' of
        Just result ->
            Just result

        Nothing ->
            case parseWithExponent 'E' of
                Just upperCaseResult ->
                    Just upperCaseResult

                Nothing ->
                    toRationalComponentsWithoutExponent charsBlob


toRationalComponentsWithoutExponent : Int -> Maybe ( Int, Int )
toRationalComponentsWithoutExponent charsBlob =
    case splitHelperOnBlob 0 [] 0 '.' charsBlob of
        [] ->
            Nothing

        [ String whole ] ->
            case parseUnsignedInt whole 0 of
                Nothing ->
                    Nothing

                Just numerator ->
                    Just (Elm_Float numerator 1)

        [ String beforeSep, String afterSep ] ->
            if Pine_kernel.equal [ Pine_kernel.length afterSep, 0 ] then
                if Pine_kernel.equal [ Pine_kernel.length beforeSep, 0 ] then
                    Nothing

                else
                    case parseUnsignedIntRec 0 beforeSep 0 of
                        Nothing ->
                            Nothing

                        Just beforeSepInt ->
                            Just (Elm_Float beforeSepInt 1)

            else
                case parseUnsignedIntRec 0 beforeSep 0 of
                    Nothing ->
                        Nothing

                    Just beforeSepInt ->
                        case parseUnsignedIntRec 0 afterSep 0 of
                            Nothing ->
                                Nothing

                            Just afterSepInt ->
                                let
                                    denom : Int
                                    denom =
                                        case Pine_kernel.length afterSep of
                                            4 ->
                                                10

                                            8 ->
                                                100

                                            12 ->
                                                1000

                                            16 ->
                                                10000

                                            20 ->
                                                100000

                                            24 ->
                                                1000000

                                            28 ->
                                                10000000

                                            32 ->
                                                100000000

                                            36 ->
                                                1000000000

                                            40 ->
                                                10000000000

                                            _ ->
                                                1

                                    numerator =
                                        Pine_kernel.int_add
                                            [ Pine_kernel.int_mul [ beforeSepInt, denom ], afterSepInt ]
                                in
                                Just (Elm_Float numerator denom)

        _ ->
            Nothing


any : (Char -> Bool) -> String -> Bool
any predicate (String chars) =
    charsAny 0 predicate chars


charsAny : Int -> (Char -> Bool) -> Int -> Bool
charsAny offset predicate charsBytes =
    let
        char =
            Pine_kernel.take [ 4, Pine_kernel.skip [ offset, charsBytes ] ]
    in
    if Pine_kernel.equal [ Pine_kernel.length char, 0 ] then
        False

    else if predicate char then
        True

    else
        charsAny
            (Pine_kernel.int_add [ offset, 4 ])
            predicate
            charsBytes


all : (Char -> Bool) -> String -> Bool
all predicate (String chars) =
    charsAll 0 predicate chars


charsAll : Int -> (Char -> Bool) -> Int -> Bool
charsAll offset predicate charsBytes =
    let
        char =
            Pine_kernel.take [ 4, Pine_kernel.skip [ offset, charsBytes ] ]
    in
    if Pine_kernel.equal [ Pine_kernel.length char, 0 ] then
        True

    else if predicate char then
        charsAll
            (Pine_kernel.int_add [ offset, 4 ])
            predicate
            charsBytes

    else
        False


indexes : String -> String -> List Int
indexes (String pattern) (String string) =
    indexesHelper 0 [] pattern string


indexesHelper : Int -> List Int -> Int -> Int -> List Int
indexesHelper offset currentIndexes pattern string =
    let
        stringSlice =
            Pine_kernel.take
                [ Pine_kernel.length pattern
                , Pine_kernel.skip
                    [ Pine_kernel.int_mul [ offset, 4 ]
                    , string
                    ]
                ]
    in
    if Pine_kernel.equal [ Pine_kernel.length stringSlice, 0 ] then
        currentIndexes

    else if
        Pine_kernel.equal
            [ stringSlice
            , pattern
            ]
    then
        indexesHelper
            (Pine_kernel.int_add [ offset, 1 ])
            (Pine_kernel.concat [ currentIndexes, [ offset ] ])
            pattern
            string

    else
        indexesHelper
            (Pine_kernel.int_add [ offset, 1 ])
            currentIndexes
            pattern
            string


indices : String -> String -> List Int
indices pattern string =
    indexes pattern string


toUpper : String -> String
toUpper string =
    map Char.toUpper string


toLower : String -> String
toLower string =
    map Char.toLower string
