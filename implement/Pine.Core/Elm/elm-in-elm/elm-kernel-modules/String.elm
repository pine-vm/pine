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
            Pine_builtin.take
                [ 4
                , Pine_builtin.skip [ offset, blob ]
                ]
    in
    if Pine_builtin.equal [ Pine_builtin.length nextChar, 0 ] then
        list

    else
        toListRecursive
            (Pine_builtin.int_add [ offset, 4 ])
            (Pine_builtin.concat [ list, [ nextChar ] ])
            blob


fromList : List Char -> String
fromList chars =
    String (Pine_builtin.concat chars)


fromChar : Char -> String
fromChar char =
    String char


cons : Char -> String -> String
cons char (String string) =
    String (Pine_builtin.concat [ char, string ])


uncons : String -> Maybe ( Char, String )
uncons (String chars) =
    if Pine_builtin.equal [ Pine_builtin.length chars, 0 ] then
        Nothing

    else
        Just ( Pine_builtin.take [ 4, chars ], String (Pine_builtin.skip [ 4, chars ]) )


isEmpty : String -> Bool
isEmpty (String chars) =
    Pine_builtin.equal
        [ Pine_builtin.length chars, 0 ]


length : String -> Int
length (String chars) =
    Pine_builtin.concat
        [ Pine_builtin.take [ 1, 0 ]
        , Pine_builtin.bit_shift_right
            [ 2
            , Pine_builtin.skip [ 1, Pine_builtin.length chars ]
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
            Pine_builtin.head chars
    in
    if Pine_builtin.equal [ nextChar, [] ] then
        acc

    else
        foldlChars func (func nextChar acc) (Pine_builtin.skip [ 1, chars ])


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
            (Pine_builtin.take [ 0, charsBytes ])
            func
            charsBytes
        )


charsMap : Int -> Int -> (Char -> Char) -> Int -> Int
charsMap offset mappedBytes func charsBytes =
    let
        char =
            Pine_builtin.take
                [ 4
                , Pine_builtin.skip [ offset, charsBytes ]
                ]
    in
    if Pine_builtin.equal [ Pine_builtin.length char, 0 ] then
        mappedBytes

    else
        let
            mappedChar =
                func char
        in
        charsMap
            (Pine_builtin.int_add [ offset, 4 ])
            (Pine_builtin.concat [ mappedBytes, mappedChar ])
            func
            charsBytes


filter : (Char -> Bool) -> String -> String
filter predicate (String chars) =
    charsFilter
        0
        (Pine_builtin.take [ 0, chars ])
        predicate
        chars


charsFilter : Int -> Int -> (Char -> Bool) -> Int -> String
charsFilter offset charsBytesFiltered predicate charsBytes =
    let
        char =
            Pine_builtin.take [ 4, Pine_builtin.skip [ offset, charsBytes ] ]
    in
    if Pine_builtin.equal [ Pine_builtin.length char, 0 ] then
        String charsBytesFiltered

    else if predicate char then
        charsFilter
            (Pine_builtin.int_add [ offset, 4 ])
            (Pine_builtin.concat [ charsBytesFiltered, char ])
            predicate
            charsBytes

    else
        charsFilter
            (Pine_builtin.int_add [ offset, 4 ])
            charsBytesFiltered
            predicate
            charsBytes


repeat : Int -> String -> String
repeat n (String chars) =
    String (Pine_builtin.concat (List.repeat n chars))


replace : String -> String -> String -> String
replace before after string =
    join after (split before string)


append : String -> String -> String
append (String a) (String b) =
    String (Pine_builtin.concat [ a, b ])


concat : List String -> String
concat strings =
    let
        charsBlobs =
            List.map
                (\(String chars) -> chars)
                strings
    in
    String (Pine_builtin.concat charsBlobs)


split : String -> String -> List String
split (String sep) ((String stringBytes) as string) =
    if Pine_builtin.equal [ Pine_builtin.length sep, 0 ] then
        List.map fromChar (toList string)

    else
        splitHelperOnBlob 0 [] 0 sep stringBytes


splitHelperOnBlob : Int -> List String -> Int -> Int -> Int -> List String
splitHelperOnBlob offset collected lastStart sepBytes stringBytes =
    let
        sliceBytes : Int
        sliceBytes =
            Pine_builtin.take
                [ Pine_builtin.length sepBytes
                , Pine_builtin.skip [ offset, stringBytes ]
                ]
    in
    if Pine_builtin.equal [ sliceBytes, sepBytes ] then
        let
            separatedSliceLength : Int
            separatedSliceLength =
                Pine_builtin.int_add
                    [ offset
                    , Pine_builtin.int_mul [ -1, lastStart ]
                    ]

            separatedSlice : Int
            separatedSlice =
                Pine_builtin.take
                    [ separatedSliceLength
                    , Pine_builtin.skip [ lastStart, stringBytes ]
                    ]
        in
        splitHelperOnBlob
            (Pine_builtin.int_add [ offset, Pine_builtin.length sepBytes ])
            (Pine_builtin.concat [ collected, [ String separatedSlice ] ])
            (Pine_builtin.int_add [ offset, Pine_builtin.length sepBytes ])
            sepBytes
            stringBytes

    else if Pine_builtin.equal [ Pine_builtin.length sliceBytes, 0 ] then
        let
            separatedSlice : Int
            separatedSlice =
                Pine_builtin.skip [ lastStart, stringBytes ]
        in
        Pine_builtin.concat [ collected, [ String separatedSlice ] ]

    else
        splitHelperOnBlob
            (Pine_builtin.int_add [ offset, 4 ])
            collected
            lastStart
            sepBytes
            stringBytes


join : String -> List String -> String
join (String sepCharsBytes) chunks =
    let
        charsBytesList =
            Pine_builtin.skip
                [ 1
                , List.concatMap
                    (\(String chars) -> [ sepCharsBytes, chars ])
                    chunks
                ]
    in
    String
        (Pine_builtin.concat charsBytesList)


slice : Int -> Int -> String -> String
slice start end (String charsBlob) =
    if Pine_builtin.int_is_sorted_asc [ 0, start, end ] then
        let
            sliceLength : Int
            sliceLength =
                Pine_builtin.int_add [ end, Pine_builtin.int_mul [ -1, start ] ]
        in
        String
            (Pine_builtin.take
                [ Pine_builtin.int_mul [ sliceLength, 4 ]
                , Pine_builtin.skip
                    [ Pine_builtin.int_mul [ start, 4 ]
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
                    Pine_builtin.equal
                        [ Pine_builtin.take [ 1, relativeIndex ]
                        , Pine_builtin.take [ 1, -1 ]
                        ]
                then
                    Pine_builtin.int_add [ relativeIndex, Pine_builtin.length charsBlob ]

                else
                    relativeIndex

            absoluteStart : Int
            absoluteStart =
                absoluteIndex
                    (Pine_builtin.int_mul [ start, 4 ])

            sliceLength : Int
            sliceLength =
                Pine_builtin.int_add
                    [ absoluteIndex (Pine_builtin.int_mul [ end, 4 ])
                    , Pine_builtin.int_mul [ -1, absoluteStart ]
                    ]
        in
        String
            (Pine_builtin.take
                [ sliceLength
                , Pine_builtin.skip [ absoluteStart, charsBlob ]
                ]
            )


left : Int -> String -> String
left n (String chars) =
    String
        (Pine_builtin.take
            [ Pine_builtin.int_mul [ n, 4 ]
            , chars
            ]
        )


right : Int -> String -> String
right n string =
    if Pine_builtin.int_is_sorted_asc [ n, 0 ] then
        ""

    else
        slice -n (length string) string


dropLeft : Int -> String -> String
dropLeft n (String chars) =
    String
        (Pine_builtin.skip
            [ Pine_builtin.int_mul [ n, 4 ]
            , chars
            ]
        )


dropRight : Int -> String -> String
dropRight n string =
    if Pine_builtin.int_is_sorted_asc [ n, 0 ] then
        string

    else
        slice 0 -n string


contains : String -> String -> Bool
contains (String patternList) (String stringList) =
    if Pine_builtin.equal [ patternList, [] ] then
        True

    else
        containsOnBlob 0 patternList stringList


containsOnBlob : Int -> Int -> Int -> Bool
containsOnBlob offset patternBytes stringBytes =
    let
        stringSlice =
            Pine_builtin.take
                [ Pine_builtin.length patternBytes
                , Pine_builtin.skip [ offset, stringBytes ]
                ]
    in
    if Pine_builtin.equal [ stringSlice, patternBytes ] then
        True

    else if Pine_builtin.equal [ Pine_builtin.length stringSlice, 0 ] then
        False

    else
        containsOnBlob
            (Pine_builtin.int_add [ offset, 4 ])
            patternBytes
            stringBytes


startsWith : String -> String -> Bool
startsWith (String patternList) (String stringList) =
    Pine_builtin.equal
        [ Pine_builtin.take [ Pine_builtin.length patternList, stringList ]
        , patternList
        ]


endsWith : String -> String -> Bool
endsWith pattern string =
    Pine_builtin.equal
        [ right (length pattern) string
        , pattern
        ]


toInt : String -> Maybe Int
toInt (String chars) =
    parseInt chars


fromInt : Int -> String
fromInt int =
    String (Pine_builtin.concat (fromIntAsList int))


parseInt : Int -> Maybe Int
parseInt src =
    let
        nextChar =
            Pine_builtin.take
                [ 4
                , src
                ]
    in
    case nextChar of
        '-' ->
            case parseUnsignedInt src 4 of
                Just unsignedVal ->
                    Just (Pine_builtin.int_mul [ -1, unsignedVal ])

                Nothing ->
                    Nothing

        '+' ->
            parseUnsignedInt src 4

        _ ->
            -- If no minus sign, parse the rest as an unsigned integer
            parseUnsignedInt src 0


parseUnsignedInt : Int -> Int -> Maybe Int
parseUnsignedInt src offset0 =
    case Pine_builtin.take [ 4, Pine_builtin.skip [ offset0, src ] ] of
        '0' ->
            parseUnsignedIntRec 0 src (Pine_builtin.int_add [ offset0, 4 ])

        '1' ->
            parseUnsignedIntRec 1 src (Pine_builtin.int_add [ offset0, 4 ])

        '2' ->
            parseUnsignedIntRec 2 src (Pine_builtin.int_add [ offset0, 4 ])

        '3' ->
            parseUnsignedIntRec 3 src (Pine_builtin.int_add [ offset0, 4 ])

        '4' ->
            parseUnsignedIntRec 4 src (Pine_builtin.int_add [ offset0, 4 ])

        '5' ->
            parseUnsignedIntRec 5 src (Pine_builtin.int_add [ offset0, 4 ])

        '6' ->
            parseUnsignedIntRec 6 src (Pine_builtin.int_add [ offset0, 4 ])

        '7' ->
            parseUnsignedIntRec 7 src (Pine_builtin.int_add [ offset0, 4 ])

        '8' ->
            parseUnsignedIntRec 8 src (Pine_builtin.int_add [ offset0, 4 ])

        '9' ->
            parseUnsignedIntRec 9 src (Pine_builtin.int_add [ offset0, 4 ])

        _ ->
            Nothing


parseUnsignedIntRec : Int -> Int -> Int -> Maybe Int
parseUnsignedIntRec upper src offset0 =
    let
        nextChar =
            Pine_builtin.take [ 4, Pine_builtin.skip [ offset0, src ] ]
    in
    if Pine_builtin.equal [ Pine_builtin.length nextChar, 0 ] then
        Just upper

    else if Pine_builtin.equal [ nextChar, '0' ] then
        parseUnsignedIntRec (Pine_builtin.int_mul [ upper, 10 ]) src (Pine_builtin.int_add [ offset0, 4 ])

    else if Pine_builtin.equal [ nextChar, '1' ] then
        parseUnsignedIntRec (Pine_builtin.int_add [ Pine_builtin.int_mul [ upper, 10 ], 1 ]) src (Pine_builtin.int_add [ offset0, 4 ])

    else if Pine_builtin.equal [ nextChar, '2' ] then
        parseUnsignedIntRec (Pine_builtin.int_add [ Pine_builtin.int_mul [ upper, 10 ], 2 ]) src (Pine_builtin.int_add [ offset0, 4 ])

    else if Pine_builtin.equal [ nextChar, '3' ] then
        parseUnsignedIntRec (Pine_builtin.int_add [ Pine_builtin.int_mul [ upper, 10 ], 3 ]) src (Pine_builtin.int_add [ offset0, 4 ])

    else if Pine_builtin.equal [ nextChar, '4' ] then
        parseUnsignedIntRec (Pine_builtin.int_add [ Pine_builtin.int_mul [ upper, 10 ], 4 ]) src (Pine_builtin.int_add [ offset0, 4 ])

    else if Pine_builtin.equal [ nextChar, '5' ] then
        parseUnsignedIntRec (Pine_builtin.int_add [ Pine_builtin.int_mul [ upper, 10 ], 5 ]) src (Pine_builtin.int_add [ offset0, 4 ])

    else if Pine_builtin.equal [ nextChar, '6' ] then
        parseUnsignedIntRec (Pine_builtin.int_add [ Pine_builtin.int_mul [ upper, 10 ], 6 ]) src (Pine_builtin.int_add [ offset0, 4 ])

    else if Pine_builtin.equal [ nextChar, '7' ] then
        parseUnsignedIntRec (Pine_builtin.int_add [ Pine_builtin.int_mul [ upper, 10 ], 7 ]) src (Pine_builtin.int_add [ offset0, 4 ])

    else if Pine_builtin.equal [ nextChar, '8' ] then
        parseUnsignedIntRec (Pine_builtin.int_add [ Pine_builtin.int_mul [ upper, 10 ], 8 ]) src (Pine_builtin.int_add [ offset0, 4 ])

    else if Pine_builtin.equal [ nextChar, '9' ] then
        parseUnsignedIntRec (Pine_builtin.int_add [ Pine_builtin.int_mul [ upper, 10 ], 9 ]) src (Pine_builtin.int_add [ offset0, 4 ])

    else
        Nothing


fromIntAsList : Int -> List Char
fromIntAsList int =
    if Pine_builtin.int_is_sorted_asc [ 0, int ] then
        fromUnsignedIntAsList int

    else
        Pine_builtin.concat [ [ '-' ], fromUnsignedIntAsList -int ]


fromUnsignedIntAsList : Int -> List Char
fromUnsignedIntAsList int =
    fromUnsignedIntAsListHelper int []


fromUnsignedIntAsListHelper : Int -> List Char -> List Char
fromUnsignedIntAsListHelper int lowerDigits =
    if Pine_builtin.int_is_sorted_asc [ int, 0 ] then
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
                    (Pine_builtin.int_add
                        [ int
                        , Pine_builtin.int_mul [ upperDigitsValue, -10 ]
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
                (Pine_builtin.length chars)
                chars
    in
    String
        (Pine_builtin.skip
            [ leftTrimmedCount
            , Pine_builtin.take
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
        (Pine_builtin.skip
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
                (Pine_builtin.length chars)
                chars
    in
    String
        (Pine_builtin.take
            [ remainingLength
            , chars
            ]
        )


trimLeftCountBytesTrimmed : Int -> Int -> Int
trimLeftCountBytesTrimmed offset charsBytes =
    let
        nextCharBytes =
            Pine_builtin.take
                [ 4
                , Pine_builtin.skip [ offset, charsBytes ]
                ]
    in
    if Pine_builtin.equal [ Pine_builtin.length nextCharBytes, 0 ] then
        offset

    else if isCharRemovedOnTrim nextCharBytes then
        trimLeftCountBytesTrimmed
            (Pine_builtin.int_add [ offset, 4 ])
            charsBytes

    else
        offset


trimRightCountBytesRemaining : Int -> Int -> Int
trimRightCountBytesRemaining remainingLength charsBytes =
    if Pine_builtin.equal [ remainingLength, 0 ] then
        0

    else
        let
            char =
                Pine_builtin.take
                    [ 4
                    , Pine_builtin.skip
                        [ Pine_builtin.int_add [ remainingLength, -4 ]
                        , charsBytes
                        ]
                    ]
        in
        if isCharRemovedOnTrim char then
            trimRightCountBytesRemaining
                (Pine_builtin.int_add [ remainingLength, -4 ])
                charsBytes

        else
            remainingLength


isCharRemovedOnTrim : Char -> Bool
isCharRemovedOnTrim char =
    if Pine_builtin.equal [ char, ' ' ] then
        True

    else if Pine_builtin.equal [ char, '\t' ] then
        True

    else if Pine_builtin.equal [ char, '\n' ] then
        True

    else if Pine_builtin.equal [ char, '\u{000D}' ] then
        True

    else if Pine_builtin.equal [ char, '\u{00A0}' ] then
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
            Pine_builtin.length charsBytes

        stringLength : Int
        stringLength =
            Pine_builtin.concat
                [ Pine_builtin.take [ 1, 0 ]
                , Pine_builtin.bit_shift_right [ 2, Pine_builtin.skip [ 1, stringBytesLength ] ]
                ]

        paddingLength : Int
        paddingLength =
            Pine_builtin.int_add
                [ n
                , Pine_builtin.int_mul [ stringLength, -1 ]
                ]
    in
    if Pine_builtin.int_is_sorted_asc [ paddingLength, 0 ] then
        string

    else
        String
            (Pine_builtin.concat
                [ Pine_builtin.concat (List.repeat paddingLength char)
                , charsBytes
                ]
            )


padRight : Int -> Char -> String -> String
padRight n char ((String charsBytes) as string) =
    let
        stringBytesLength : Int
        stringBytesLength =
            Pine_builtin.length charsBytes

        stringLength : Int
        stringLength =
            Pine_builtin.concat
                [ Pine_builtin.take [ 1, 0 ]
                , Pine_builtin.bit_shift_right [ 2, Pine_builtin.skip [ 1, stringBytesLength ] ]
                ]

        paddingLength : Int
        paddingLength =
            Pine_builtin.int_add
                [ n
                , Pine_builtin.int_mul [ stringLength, -1 ]
                ]
    in
    if Pine_builtin.int_is_sorted_asc [ paddingLength, 0 ] then
        string

    else
        String
            (Pine_builtin.concat
                [ charsBytes
                , Pine_builtin.concat (List.repeat paddingLength char)
                ]
            )


pad : Int -> Char -> String -> String
pad n char ((String charsBytes) as string) =
    let
        stringBytesLength : Int
        stringBytesLength =
            Pine_builtin.length charsBytes

        stringLength : Int
        stringLength =
            Pine_builtin.concat
                [ Pine_builtin.take [ 1, 0 ]
                , Pine_builtin.bit_shift_right [ 2, Pine_builtin.skip [ 1, stringBytesLength ] ]
                ]

        totalPadding : Int
        totalPadding =
            Pine_builtin.int_add
                [ n
                , Pine_builtin.int_mul [ stringLength, -1 ]
                ]

        rightPadding : Int
        rightPadding =
            totalPadding // 2

        leftPadding : Int
        leftPadding =
            Pine_builtin.int_add
                [ totalPadding
                , Pine_builtin.int_mul [ rightPadding, -1 ]
                ]
    in
    if Pine_builtin.int_is_sorted_asc [ totalPadding, 0 ] then
        string

    else
        String
            (Pine_builtin.concat
                [ Pine_builtin.concat (List.repeat leftPadding char)
                , charsBytes
                , Pine_builtin.concat (List.repeat rightPadding char)
                ]
            )


lines : String -> List String
lines (String chars) =
    linesHelper 0 [] 0 chars


linesHelper : Int -> List String -> Int -> Int -> List String
linesHelper currentLineStart currentLines offset charsBytes =
    let
        nextChar =
            Pine_builtin.take
                [ 4
                , Pine_builtin.skip [ offset, charsBytes ]
                ]

        nextTwoChars =
            Pine_builtin.take
                [ 8
                , Pine_builtin.skip [ offset, charsBytes ]
                ]
    in
    if Pine_builtin.equal [ Pine_builtin.length nextChar, 0 ] then
        let
            currentLineLength =
                Pine_builtin.int_add [ offset, -currentLineStart ]
        in
        Pine_builtin.concat
            [ currentLines
            , [ String (Pine_builtin.skip [ currentLineStart, charsBytes ]) ]
            ]

    else if Pine_builtin.equal [ nextTwoChars, Pine_builtin.concat [ '\u{000D}', '\n' ] ] then
        let
            currentLineLength =
                Pine_builtin.int_add [ offset, -currentLineStart ]

            currentLineChars : Int
            currentLineChars =
                Pine_builtin.take
                    [ currentLineLength
                    , Pine_builtin.skip [ currentLineStart, charsBytes ]
                    ]
        in
        linesHelper
            (Pine_builtin.int_add [ offset, 8 ])
            (Pine_builtin.concat [ currentLines, [ String currentLineChars ] ])
            (Pine_builtin.int_add [ offset, 8 ])
            charsBytes

    else if Pine_builtin.equal [ nextChar, '\n' ] then
        let
            currentLineLength =
                Pine_builtin.int_add [ offset, -currentLineStart ]

            currentLineChars : Int
            currentLineChars =
                Pine_builtin.take
                    [ currentLineLength
                    , Pine_builtin.skip [ currentLineStart, charsBytes ]
                    ]
        in
        linesHelper
            (Pine_builtin.int_add [ offset, 4 ])
            (Pine_builtin.concat [ currentLines, [ String currentLineChars ] ])
            (Pine_builtin.int_add [ offset, 4 ])
            charsBytes

    else if Pine_builtin.equal [ nextChar, '\u{000D}' ] then
        let
            currentLineLength =
                Pine_builtin.int_add [ offset, -currentLineStart ]

            currentLineChars : Int
            currentLineChars =
                Pine_builtin.take
                    [ currentLineLength
                    , Pine_builtin.skip [ currentLineStart, charsBytes ]
                    ]
        in
        linesHelper
            (Pine_builtin.int_add [ offset, 4 ])
            (Pine_builtin.concat [ currentLines, [ String currentLineChars ] ])
            (Pine_builtin.int_add [ offset, 4 ])
            charsBytes

    else
        linesHelper
            currentLineStart
            currentLines
            (Pine_builtin.int_add [ offset, 4 ])
            charsBytes


words : String -> List String
words string =
    wordsHelper 0 [] 0 (toList string)


wordsHelper : Int -> List String -> Int -> List Char -> List String
wordsHelper currentWordStart currentWords offset chars =
    let
        nextChar =
            Pine_builtin.head (Pine_builtin.skip [ offset, chars ])
    in
    if Pine_builtin.equal [ nextChar, [] ] then
        let
            currentWordLength : Int
            currentWordLength =
                Pine_builtin.int_add
                    [ offset
                    , Pine_builtin.int_mul [ currentWordStart, -1 ]
                    ]

            currentWordChars : List Char
            currentWordChars =
                Pine_builtin.take
                    [ currentWordLength
                    , Pine_builtin.skip [ currentWordStart, chars ]
                    ]
        in
        if Pine_builtin.equal [ currentWordChars, [] ] then
            currentWords

        else
            Pine_builtin.concat
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
                    Pine_builtin.int_add
                        [ offset
                        , Pine_builtin.int_mul [ currentWordStart, -1 ]
                        ]

                currentWordChars : List Char
                currentWordChars =
                    Pine_builtin.take
                        [ currentWordLength
                        , Pine_builtin.skip [ currentWordStart, chars ]
                        ]
            in
            if Pine_builtin.equal [ currentWordChars, [] ] then
                wordsHelper
                    (Pine_builtin.int_add [ offset, 1 ])
                    currentWords
                    (Pine_builtin.int_add [ offset, 1 ])
                    chars

            else
                wordsHelper
                    (Pine_builtin.int_add [ offset, 1 ])
                    (Pine_builtin.concat [ currentWords, [ String currentWordChars ] ])
                    (Pine_builtin.int_add [ offset, 1 ])
                    chars

        else
            wordsHelper
                currentWordStart
                currentWords
                (Pine_builtin.int_add [ offset, 1 ])
                chars


toFloat : String -> Maybe Float
toFloat (String charsBlob) =
    let
        firstChar =
            Pine_builtin.take [ 4, charsBlob ]
    in
    if Pine_builtin.equal [ Pine_builtin.length firstChar, 0 ] then
        Nothing

    else if Pine_builtin.equal [ firstChar, '-' ] then
        case toRationalComponentsLessSign (Pine_builtin.skip [ 4, charsBlob ]) of
            Nothing ->
                Nothing

            Just (Elm_Float numAbs denom) ->
                let
                    numSigned : Int
                    numSigned =
                        Pine_builtin.int_mul [ -1, numAbs ]
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
                Pine_builtin.equal
                    [ Pine_builtin.take [ 1, numerator ]
                    , Pine_builtin.take [ 1, -1 ]
                    ]
            then
                "-Infinity"

            else
                "Infinity"

        _ ->
            let
                isNegative : Bool
                isNegative =
                    Pine_builtin.equal
                        [ Pine_builtin.take [ 1, numerator ]
                        , Pine_builtin.take [ 1, -1 ]
                        ]

                ( signStr, absNum ) =
                    if isNegative then
                        ( [ '-' ]
                        , Pine_builtin.int_mul [ -1, numerator ]
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
            if Pine_builtin.equal [ remainder, 0 ] || Pine_builtin.equal [ decimalPlacesMax, 0 ] then
                -- No remainder OR no decimal places requested
                String
                    (Pine_builtin.concat
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
                        Pine_builtin.int_mul [ remainder, scale ]

                    scaledInt : Int
                    scaledInt =
                        scaledVal // denom

                    leftover : Int
                    leftover =
                        modBy denom scaledVal

                    -- 4) ROUND HALF-UP:
                    scaledIntRounded : Int
                    scaledIntRounded =
                        if Pine_builtin.int_is_sorted_asc [ denom, Pine_builtin.int_mul [ leftover, 2 ] ] then
                            Pine_builtin.int_add [ 1, scaledInt ]

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
                        Pine_builtin.int_is_sorted_asc [ scale, scaledIntRounded ]

                    ( newIntPart, fractionDigits ) =
                        if overflowed then
                            -- increment integer part, fraction becomes e.g. "000"
                            let
                                incremented : Int
                                incremented =
                                    Pine_builtin.int_add [ intPart, 1 ]

                                -- e.g. scaledStr = "100" => dropLeft 1 => "00"
                                -- If scaledStr was "1000" => dropLeft 1 => "000"
                                fractionNoSign : List Char
                                fractionNoSign =
                                    Pine_builtin.skip [ 1, scaledStr ]
                            in
                            ( incremented, fractionNoSign )

                        else
                            -- If not overflowed, we may need zero‐padding to the left.
                            let
                                neededZeros : Int
                                neededZeros =
                                    Pine_builtin.int_add
                                        [ decimalPlacesMax
                                        , Pine_builtin.int_mul [ -1, Pine_builtin.length scaledStr ]
                                        ]

                                fractionNoSign : List Char
                                fractionNoSign =
                                    Pine_builtin.concat
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
                        (Pine_builtin.concat
                            [ signStr
                            , fromIntAsList newIntPart
                            ]
                        )

                else
                    String
                        (Pine_builtin.concat
                            [ signStr
                            , fromIntAsList newIntPart
                            , [ '.' ]
                            , trimmedFraction
                            ]
                        )


removeTrailingZeros : List Char -> List Char
removeTrailingZeros chars =
    removeTrailingZerosHelper (Pine_builtin.length chars) chars


removeTrailingZerosHelper : Int -> List Char -> List Char
removeTrailingZerosHelper offset chars =
    if Pine_builtin.equal [ offset, 0 ] then
        chars

    else
        let
            nextOffset : Int
            nextOffset =
                Pine_builtin.int_add [ offset, -1 ]
        in
        case
            Pine_builtin.take
                [ 1
                , Pine_builtin.skip [ nextOffset, chars ]
                ]
        of
            [ '0' ] ->
                removeTrailingZerosHelper nextOffset chars

            _ ->
                Pine_builtin.take [ offset, chars ]


intPow : Int -> Int -> Int -> Int
intPow acc base exponent =
    if Pine_builtin.int_is_sorted_asc [ exponent, 0 ] then
        acc

    else
        intPow
            (Pine_builtin.int_mul [ acc, base ])
            base
            (Pine_builtin.int_add [ exponent, -1 ])


toRationalComponentsLessSign : Int -> Maybe ( Int, Int )
toRationalComponentsLessSign charsBlob =
    let
        parseWithExponent exponentChar =
            case splitHelperOnBlob 0 [] 0 exponentChar charsBlob of
                [ String mantissa, String exponent ] ->
                    if Pine_builtin.equal [ Pine_builtin.length mantissa, 0 ] then
                        Nothing

                    else if Pine_builtin.equal [ Pine_builtin.length exponent, 0 ] then
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
                                        Pine_builtin.int_is_sorted_asc [ exponentInt, 0 ]

                                    exponentMagnitude =
                                        if exponentIsNonPositive then
                                            Pine_builtin.int_mul [ -1, exponentInt ]

                                        else
                                            exponentInt

                                    powTen =
                                        intPow 1 10 exponentMagnitude
                                in
                                if exponentIsNonPositive then
                                    Just
                                        (Elm_Float
                                            numerator
                                            (Pine_builtin.int_mul [ denom, powTen ])
                                        )

                                else
                                    Just
                                        (Elm_Float
                                            (Pine_builtin.int_mul [ numerator, powTen ])
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
            if Pine_builtin.equal [ Pine_builtin.length afterSep, 0 ] then
                if Pine_builtin.equal [ Pine_builtin.length beforeSep, 0 ] then
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
                                        case Pine_builtin.length afterSep of
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
                                        Pine_builtin.int_add
                                            [ Pine_builtin.int_mul [ beforeSepInt, denom ], afterSepInt ]
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
            Pine_builtin.take [ 4, Pine_builtin.skip [ offset, charsBytes ] ]
    in
    if Pine_builtin.equal [ Pine_builtin.length char, 0 ] then
        False

    else if predicate char then
        True

    else
        charsAny
            (Pine_builtin.int_add [ offset, 4 ])
            predicate
            charsBytes


all : (Char -> Bool) -> String -> Bool
all predicate (String chars) =
    charsAll 0 predicate chars


charsAll : Int -> (Char -> Bool) -> Int -> Bool
charsAll offset predicate charsBytes =
    let
        char =
            Pine_builtin.take [ 4, Pine_builtin.skip [ offset, charsBytes ] ]
    in
    if Pine_builtin.equal [ Pine_builtin.length char, 0 ] then
        True

    else if predicate char then
        charsAll
            (Pine_builtin.int_add [ offset, 4 ])
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
            Pine_builtin.take
                [ Pine_builtin.length pattern
                , Pine_builtin.skip
                    [ Pine_builtin.int_mul [ offset, 4 ]
                    , string
                    ]
                ]
    in
    if Pine_builtin.equal [ Pine_builtin.length stringSlice, 0 ] then
        currentIndexes

    else if
        Pine_builtin.equal
            [ stringSlice
            , pattern
            ]
    then
        indexesHelper
            (Pine_builtin.int_add [ offset, 1 ])
            (Pine_builtin.concat [ currentIndexes, [ offset ] ])
            pattern
            string

    else
        indexesHelper
            (Pine_builtin.int_add [ offset, 1 ])
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
