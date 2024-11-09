module Elm.Kernel.Parser exposing (..)

import Char


consumeBase : Int -> Int -> List Char -> ( Int, Int )
consumeBase base offset chars =
    consumeBaseHelper base offset chars 0


consumeBaseHelper : Int -> Int -> List Char -> Int -> ( Int, Int )
consumeBaseHelper base offset chars total =
    case Pine_kernel.skip [ offset, chars ] of
        [] ->
            ( offset, total )

        nextChar :: _ ->
            let
                digit : Int
                digit =
                    Pine_kernel.int_add [ Char.toCode nextChar, -48 ]

                lastDigit : Int
                lastDigit =
                    Pine_kernel.int_add [ base, -1 ]
            in
            if Pine_kernel.int_is_sorted_asc [ 0, digit, lastDigit ] then
                consumeBaseHelper
                    base
                    (Pine_kernel.int_add [ offset, 1 ])
                    chars
                    (Pine_kernel.int_add [ Pine_kernel.int_mul [ base, total ], digit ])

            else
                ( offset, total )


consumeBase16 : Int -> List Char -> ( Int, Int )
consumeBase16 offset chars =
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


chompBase10 : Int -> List Char -> Int
chompBase10 offset chars =
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


isSubString : String -> Int -> Int -> Int -> List Char -> ( Int, Int, Int )
isSubString (String smallChars) offset row col bigChars =
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
                countOffsetsInString ( 0, 0, 0 ) ( smallChars, expectedLength )

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


isSubChar : (Char -> Bool) -> Int -> List Char -> Int
isSubChar predicate offset chars =
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


findSubString : String -> Int -> Int -> Int -> List Char -> ( Int, Int, Int )
findSubString (String smallChars) offset row col bigChars =
    let
        newOffset : Int
        newOffset =
            indexOf smallChars bigChars offset

        consumedLength : Int
        consumedLength =
            Pine_kernel.int_add
                [ newOffset
                , Pine_kernel.negate offset
                ]

        targetOffset : Int
        targetOffset =
            if Pine_kernel.equal [ newOffset, -1 ] then
                List.length bigChars

            else
                Pine_kernel.int_add [ newOffset, Pine_kernel.length smallChars ]

        ( newlineCount, colShift ) =
            countOffsetsInString ( offset, 0, 0 ) ( bigChars, newOffset )

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


indexOf : List Char -> List Char -> Int -> Int
indexOf smallChars bigChars offset =
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
    if Pine_kernel.equal [ Pine_kernel.length sliceFromSourceChars, expectedLength ] then
        if Pine_kernel.equal [ sliceFromSourceChars, smallChars ] then
            offset

        else
            indexOf smallChars bigChars (Pine_kernel.int_add [ offset, 1 ])

    else
        -1


chompWhileHelp : (Char -> Bool) -> ( Int, Int, Int ) -> List Char -> ( Int, Int, Int )
chompWhileHelp isGood ( offset, row, col ) srcChars =
    let
        nextChar =
            Pine_kernel.head (Pine_kernel.skip [ offset, srcChars ])
    in
    if isGood nextChar then
        if Pine_kernel.equal [ nextChar, '\n' ] then
            -- matched a newline
            chompWhileHelp
                isGood
                ( Pine_kernel.int_add [ offset, 1 ]
                , Pine_kernel.int_add [ row, 1 ]
                , 1
                )
                srcChars

        else
            -- normal match
            chompWhileHelp
                isGood
                ( Pine_kernel.int_add [ offset, 1 ]
                , row
                , Pine_kernel.int_add [ col, 1 ]
                )
                srcChars

    else
        -- no match
        ( offset
        , row
        , col
        )


countOffsetsInString : ( Int, Int, Int ) -> ( List Char, Int ) -> ( Int, Int )
countOffsetsInString ( offset, newlines, col ) ( chars, end ) =
    let
        currentChar =
            Pine_kernel.head
                (Pine_kernel.skip [ offset, chars ])

        nextOffset =
            Pine_kernel.int_add [ offset, 1 ]
    in
    if Pine_kernel.equal [ currentChar, [] ] then
        ( newlines, col )

    else if Pine_kernel.int_is_sorted_asc [ end, offset ] then
        ( newlines, col )

    else if Pine_kernel.equal [ currentChar, '\n' ] then
        countOffsetsInString
            ( nextOffset, Pine_kernel.int_add [ newlines, 1 ], 0 )
            ( chars, end )

    else
        countOffsetsInString
            ( nextOffset, newlines, Pine_kernel.int_add [ col, 1 ] )
            ( chars, end )


newlineChar : Char
newlineChar =
    -- ASCII code for '\\n'
    Pine_kernel.skip [ 1, 10 ]


isAsciiCode : Int -> Int -> List Char -> Bool
isAsciiCode code offset chars =
    let
        nextChar =
            Pine_kernel.head
                (Pine_kernel.skip [ offset, chars ])
    in
    Pine_kernel.equal [ nextChar, Char.fromCode code ]
