module Elm.Kernel.Parser exposing (..)

import Char


consumeBase : Int -> Int -> String -> ( Int, Int )
consumeBase base offset (String chars) =
    consumeBaseHelper base offset chars 0


consumeBaseHelper : Int -> Int -> List Char -> Int -> ( Int, Int )
consumeBaseHelper base offset chars total =
    case Pine_kernel.skip [ offset, chars ] of
        [] ->
            ( offset, total )

        nextChar :: _ ->
            let
                digit =
                    Pine_kernel.int_add [ Char.toCode nextChar, -48 ]
            in
            if Pine_kernel.int_is_sorted_asc [ 0, digit, base ] then
                consumeBaseHelper
                    base
                    (Pine_kernel.int_add [ offset, 1 ])
                    chars
                    (Pine_kernel.int_add [ Pine_kernel.int_mul [ base, total ], digit ])

            else
                ( offset, total )


consumeBase16 : Int -> String -> ( Int, Int )
consumeBase16 offset (String chars) =
    consumeBase16Helper offset chars 0


consumeBase16Helper : Int -> List Char -> Int -> ( Int, Int )
consumeBase16Helper offset chars total =
    let
        char =
            Pine_kernel.head (Pine_kernel.skip [ offset, chars ])
    in
    if Pine_kernel.equal [ char, [] ] then
        ( offset, total )

    else
        let
            code =
                Char.toCode char

            digit =
                if Pine_kernel.int_is_sorted_asc [ 48, code, 57 ] then
                    -- '0' to '9'
                    Just (Pine_kernel.int_add [ code, -48 ])

                else if Pine_kernel.int_is_sorted_asc [ 65, code, 70 ] then
                    -- 'A' to 'F'
                    Just (Pine_kernel.int_add [ code, -55 ])

                else if Pine_kernel.int_is_sorted_asc [ 97, code, 102 ] then
                    -- 'a' to 'f'
                    Just (Pine_kernel.int_add [ code, -87 ])

                else
                    Nothing
        in
        case digit of
            Just d ->
                consumeBase16Helper
                    (Pine_kernel.int_add [ offset, 1 ])
                    chars
                    (Pine_kernel.int_add [ Pine_kernel.int_mul [ 16, total ], d ])

            Nothing ->
                ( offset, total )


chompBase10 : Int -> String -> Int
chompBase10 offset (String chars) =
    chompBase10Helper offset chars


chompBase10Helper : Int -> List Char -> Int
chompBase10Helper offset chars =
    let
        char =
            Pine_kernel.head (Pine_kernel.skip [ offset, chars ])
    in
    if Pine_kernel.equal [ char, [] ] then
        offset

    else
        let
            code =
                Char.toCode char
        in
        if Pine_kernel.int_is_sorted_asc [ 48, code, 57 ] then
            chompBase10Helper
                (Pine_kernel.int_add [ offset, 1 ])
                chars

        else
            offset


isSubString : String -> Int -> Int -> Int -> String -> ( Int, Int, Int )
isSubString (String smallChars) offset row col (String bigChars) =
    let
        expectedLength : Int
        expectedLength =
            Pine_kernel.length smallChars

        sliceFromSourceChars : List Char
        sliceFromSourceChars =
            Pine_kernel.take
                [ expectedLength
                , Pine_kernel.skip [ offset, bigChars ]
                ]
    in
    if Pine_kernel.equal [ sliceFromSourceChars, smallChars ] then
        let
            ( newlineCount, colShift ) =
                countOffsetsInString ( 0, 0 ) smallChars

            newOffset : Int
            newOffset =
                Pine_kernel.int_add [ offset, expectedLength ]

            newRow : Int
            newRow =
                Pine_kernel.int_add [ row, newlineCount ]

            newCol : Int
            newCol =
                if Pine_kernel.equal [ newlineCount, 0 ] then
                    Pine_kernel.int_add [ col, colShift ]

                else
                    Pine_kernel.int_add [ 1, colShift ]
        in
        ( newOffset, newRow, newCol )

    else
        ( -1, row, col )


countOffsetsInString : ( Int, Int ) -> List Char -> ( Int, Int )
countOffsetsInString ( newlines, col ) chars =
    let
        nextChar =
            Pine_kernel.head chars
    in
    if Pine_kernel.equal [ nextChar, [] ] then
        ( newlines, col )

    else if Pine_kernel.equal [ nextChar, '\n' ] then
        countOffsetsInString
            ( Pine_kernel.int_add [ newlines, 1 ], 0 )
            (Pine_kernel.skip [ 1, chars ])

    else
        countOffsetsInString
            ( newlines, Pine_kernel.int_add [ col, 1 ] )
            (Pine_kernel.skip [ 1, chars ])


isSubChar : (Char -> Bool) -> Int -> String -> Int
isSubChar predicate offset (String chars) =
    let
        nextChar =
            Pine_kernel.head (Pine_kernel.skip [ offset, chars ])
    in
    if Pine_kernel.equal [ nextChar, [] ] then
        -1

    else if predicate nextChar then
        if Pine_kernel.equal [ nextChar, newlineChar ] then
            -- Special code for newline
            -2

        else
            Pine_kernel.int_add [ offset, 1 ]

    else
        -1


findSubString : String -> Int -> Int -> Int -> String -> ( Int, Int, Int )
findSubString (String smallChars) offset row col (String bigChars) =
    let
        newOffset =
            indexOf smallChars bigChars offset

        targetOffset =
            if newOffset == -1 then
                List.length bigChars

            else
                newOffset + List.length smallChars

        ( newRow, newCol ) =
            updateRowColOverRange offset targetOffset bigChars row col
    in
    ( newOffset, newRow, newCol )


indexOf : List Char -> List Char -> Int -> Int
indexOf smallChars bigChars offset =
    if offset > List.length bigChars - List.length smallChars then
        -1

    else if startsWith smallChars (List.drop offset bigChars) then
        offset

    else
        indexOf smallChars bigChars (offset + 1)


startsWith : List Char -> List Char -> Bool
startsWith patternList stringList =
    Pine_kernel.equal
        [ Pine_kernel.take [ Pine_kernel.length patternList, stringList ]
        , patternList
        ]


updateRowColOverRange : Int -> Int -> List Char -> Int -> Int -> ( Int, Int )
updateRowColOverRange currentOffset targetOffset chars row col =
    if currentOffset >= targetOffset then
        ( row, col )

    else
        case List.head (List.drop currentOffset chars) of
            Just char ->
                if Pine_kernel.equal [ char, newlineChar ] then
                    updateRowColOverRange (currentOffset + 1) targetOffset chars (row + 1) 1

                else
                    updateRowColOverRange (currentOffset + 1) targetOffset chars row (col + 1)

            Nothing ->
                ( row, col )


newlineChar : Char
newlineChar =
    -- ASCII code for '\\n'
    Pine_kernel.skip [ 1, 10 ]


isAsciiCode : Int -> Int -> String -> Bool
isAsciiCode code offset (String chars) =
    let
        nextChar =
            Pine_kernel.head
                (Pine_kernel.skip [ offset, chars ])
    in
    Pine_kernel.equal [ nextChar, Char.fromCode code ]
