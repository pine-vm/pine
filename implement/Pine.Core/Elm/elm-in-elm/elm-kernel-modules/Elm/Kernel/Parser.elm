module Elm.Kernel.Parser exposing (..)


consumeBase : Int -> Int -> Int -> ( Int, Int )
consumeBase base offset charsBytes =
    consumeBaseHelper base offset charsBytes 0


consumeBaseHelper : Int -> Int -> Int -> Int -> ( Int, Int )
consumeBaseHelper base offset charsBytes total =
    let
        nextChar =
            Pine_builtin.take [ 4, Pine_builtin.skip [ offset, charsBytes ] ]
    in
    if Pine_builtin.equal [ Pine_builtin.length nextChar, 0 ] then
        ( offset, total )

    else
        let
            code =
                Char.toCode nextChar

            digit : Int
            digit =
                Pine_builtin.int_add [ code, -48 ]

            lastDigit : Int
            lastDigit =
                Pine_builtin.int_add [ base, -1 ]
        in
        if Pine_builtin.int_is_sorted_asc [ 0, digit, lastDigit ] then
            consumeBaseHelper
                base
                (Pine_builtin.int_add [ offset, 4 ])
                charsBytes
                (Pine_builtin.int_add [ Pine_builtin.int_mul [ base, total ], digit ])

        else
            ( offset, total )


consumeBase16 : Int -> Int -> ( Int, Int )
consumeBase16 offset charsBytes =
    consumeBase16Helper offset charsBytes 0


consumeBase16Helper : Int -> Int -> Int -> ( Int, Int )
consumeBase16Helper offset charsBytes total =
    let
        char =
            Pine_builtin.take [ 4, Pine_builtin.skip [ offset, charsBytes ] ]
    in
    if Pine_builtin.equal [ Pine_builtin.length char, 0 ] then
        ( offset, total )

    else
        let
            code =
                Char.toCode char

            digit =
                if Pine_builtin.int_is_sorted_asc [ 48, code, 57 ] then
                    Just (Pine_builtin.int_add [ code, -48 ])

                else if Pine_builtin.int_is_sorted_asc [ 65, code, 70 ] then
                    Just (Pine_builtin.int_add [ code, -55 ])

                else if Pine_builtin.int_is_sorted_asc [ 97, code, 102 ] then
                    Just (Pine_builtin.int_add [ code, -87 ])

                else
                    Nothing
        in
        case digit of
            Just d ->
                consumeBase16Helper
                    (Pine_builtin.int_add [ offset, 4 ])
                    charsBytes
                    (Pine_builtin.int_add [ Pine_builtin.int_mul [ 16, total ], d ])

            Nothing ->
                ( offset, total )


chompBase10 : Int -> Int -> Int
chompBase10 offset charsBytes =
    chompBase10Helper offset charsBytes


chompBase10Helper : Int -> Int -> Int
chompBase10Helper offset charsBytes =
    let
        char =
            Pine_builtin.take [ 4, Pine_builtin.skip [ offset, charsBytes ] ]
    in
    if Pine_builtin.equal [ Pine_builtin.length char, 0 ] then
        offset

    else
        let
            code =
                Char.toCode char
        in
        if Pine_builtin.int_is_sorted_asc [ 48, code, 57 ] then
            chompBase10Helper
                (Pine_builtin.int_add [ offset, 4 ])
                charsBytes

        else
            offset


isSubString : String -> Int -> Int -> Int -> Int -> ( Int, Int, Int )
isSubString (String smallBytes) offset row col bigBytes =
    let
        sliceFromSource : Int
        sliceFromSource =
            Pine_builtin.take
                [ Pine_builtin.length smallBytes
                , Pine_builtin.skip [ offset, bigBytes ]
                ]
    in
    if Pine_builtin.equal [ sliceFromSource, smallBytes ] then
        let
            ( newlineCount, colShift ) =
                countOffsetsInString ( 0, 0, 0 ) ( smallBytes, Pine_builtin.length smallBytes )

            newOffset : Int
            newOffset =
                Pine_builtin.int_add [ offset, Pine_builtin.length smallBytes ]

            newRow : Int
            newRow =
                Pine_builtin.int_add [ row, newlineCount ]

            newCol : Int
            newCol =
                if Pine_builtin.equal [ newlineCount, 0 ] then
                    Pine_builtin.int_add [ col, colShift ]

                else
                    Pine_builtin.int_add [ 1, colShift ]
        in
        ( newOffset, newRow, newCol )

    else
        ( -1, row, col )


isSubChar : (Char -> Bool) -> Int -> Int -> Int
isSubChar predicate offset charsBytes =
    let
        nextChar =
            Pine_builtin.take [ 4, Pine_builtin.skip [ offset, charsBytes ] ]
    in
    if Pine_builtin.equal [ Pine_builtin.length nextChar, 0 ] then
        -1

    else if predicate nextChar then
        if Pine_builtin.equal [ nextChar, newlineChar ] then
            -- Special code for newline
            -2

        else
            Pine_builtin.int_add [ offset, 4 ]

    else
        -1


findSubString : String -> Int -> Int -> Int -> Int -> ( Int, Int, Int )
findSubString (String smallBytes) offset row col bigBytes =
    let
        newOffset : Int
        newOffset =
            indexOf smallBytes bigBytes offset

        ( newlineCount, colShift ) =
            countOffsetsInString ( offset, 0, 0 ) ( bigBytes, newOffset )

        newRow : Int
        newRow =
            Pine_builtin.int_add [ row, newlineCount ]

        newCol : Int
        newCol =
            if Pine_builtin.equal [ newlineCount, 0 ] then
                Pine_builtin.int_add [ col, colShift ]

            else
                Pine_builtin.int_add [ 1, colShift ]
    in
    ( newOffset, newRow, newCol )


indexOf : Int -> Int -> Int -> Int
indexOf smallBytes bigBytes offset =
    let
        expectedLength : Int
        expectedLength =
            Pine_builtin.length smallBytes

        sliceFromSource : Int
        sliceFromSource =
            Pine_builtin.take
                [ expectedLength
                , Pine_builtin.skip [ offset, bigBytes ]
                ]
    in
    if Pine_builtin.equal [ Pine_builtin.length sliceFromSource, expectedLength ] then
        if Pine_builtin.equal [ sliceFromSource, smallBytes ] then
            offset

        else
            indexOf smallBytes bigBytes (Pine_builtin.int_add [ offset, 4 ])

    else
        -1


chompWhileHelp : (Char -> Bool) -> ( Int, Int, Int ) -> Int -> ( Int, Int, Int )
chompWhileHelp isGood ( offset, row, col ) srcBytes =
    let
        nextChar =
            Pine_builtin.take [ 4, Pine_builtin.skip [ offset, srcBytes ] ]
    in
    if isGood nextChar then
        if Pine_builtin.equal [ nextChar, '\n' ] then
            -- matched a newline
            chompWhileHelp
                isGood
                ( Pine_builtin.int_add [ offset, 4 ]
                , Pine_builtin.int_add [ row, 1 ]
                , 1
                )
                srcBytes

        else
            -- normal match
            chompWhileHelp
                isGood
                ( Pine_builtin.int_add [ offset, 4 ]
                , row
                , Pine_builtin.int_add [ col, 1 ]
                )
                srcBytes

    else
        -- no match
        ( offset
        , row
        , col
        )


countOffsetsInString : ( Int, Int, Int ) -> ( Int, Int ) -> ( Int, Int )
countOffsetsInString ( offset, newlines, col ) ( charsBytes, end ) =
    let
        currentChar =
            Pine_builtin.take
                [ 4
                , Pine_builtin.skip [ offset, charsBytes ]
                ]

        nextOffset =
            Pine_builtin.int_add [ offset, 4 ]
    in
    if Pine_builtin.equal [ Pine_builtin.length currentChar, 0 ] then
        ( newlines, col )

    else if Pine_builtin.int_is_sorted_asc [ end, offset ] then
        ( newlines, col )

    else if Pine_builtin.equal [ currentChar, '\n' ] then
        countOffsetsInString
            ( nextOffset, Pine_builtin.int_add [ newlines, 1 ], 0 )
            ( charsBytes, end )

    else
        countOffsetsInString
            ( nextOffset, newlines, Pine_builtin.int_add [ col, 1 ] )
            ( charsBytes, end )


newlineChar : Char
newlineChar =
    -- ASCII code for '\\n'
    '\n'


isAsciiCode : Int -> Int -> Int -> Bool
isAsciiCode code offset charsBytes =
    let
        nextChar =
            Pine_builtin.take
                [ 4
                , Pine_builtin.skip [ offset, charsBytes ]
                ]
    in
    Pine_builtin.equal [ nextChar, Char.fromCode code ]
