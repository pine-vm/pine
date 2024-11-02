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
    if offset >= List.length chars then
        ( offset, total )

    else
        case Pine_kernel.skip [ offset, chars ] of
            char :: _ ->
                let
                    code =
                        Char.toCode char

                    digit =
                        if Pine_kernel.int_is_sorted_asc [ 48, code, 57 ] then
                            -- '0' to '9'
                            Just (code - 48)

                        else if Pine_kernel.int_is_sorted_asc [ 65, code, 70 ] then
                            -- 'A' to 'F'
                            Just (code - 55)

                        else if Pine_kernel.int_is_sorted_asc [ 97, code, 102 ] then
                            -- 'a' to 'f'
                            Just (code - 87)

                        else
                            Nothing
                in
                case digit of
                    Just d ->
                        consumeBase16Helper
                            (offset + 1)
                            chars
                            (16 * total + d)

                    Nothing ->
                        ( offset, total )

            [] ->
                ( offset, total )


chompBase10 : Int -> String -> Int
chompBase10 offset (String chars) =
    chompBase10Helper offset chars


chompBase10Helper : Int -> List Char -> Int
chompBase10Helper offset chars =
    if Pine_kernel.int_is_sorted_asc [ offset, Pine_kernel.length chars ] then
        offset

    else
        case Pine_kernel.skip [ offset, chars ] of
            [] ->
                offset

            char :: _ ->
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
        bigCharsFromOffset =
            List.drop offset bigChars

        ( newOffset, newRow, newCol, matched ) =
            isSubStringHelper smallChars bigCharsFromOffset offset row col
    in
    if matched then
        ( newOffset, newRow, newCol )

    else
        ( -1, row, col )


isSubStringHelper : List Char -> List Char -> Int -> Int -> Int -> ( Int, Int, Int, Bool )
isSubStringHelper smallChars bigChars offset row col =
    case ( smallChars, bigChars ) of
        ( [], _ ) ->
            ( offset, row, col, True )

        ( _, [] ) ->
            ( offset, row, col, False )

        ( sChar :: sRest, bChar :: bRest ) ->
            if Pine_kernel.equal [ sChar, bChar ] then
                let
                    ( newRow, newCol ) =
                        if Pine_kernel.equal [ sChar, newlineChar ] then
                            -- ASCII code for '\\n'
                            ( Pine_kernel.int_add [ row, 1 ], 1 )

                        else
                            ( row, Pine_kernel.int_add [ col, 1 ] )
                in
                isSubStringHelper
                    sRest
                    bRest
                    (Pine_kernel.int_add [ offset, 1 ])
                    newRow
                    newCol

            else
                ( offset, row, col, False )


isSubChar : (Char -> Bool) -> Int -> String -> Int
isSubChar predicate offset (String chars) =
    case Pine_kernel.skip [ offset, chars ] of
        [] ->
            -1

        nextChar :: _ ->
            if predicate nextChar then
                if Pine_kernel.equal [ nextChar, newlineChar ] then
                    -2
                    -- Special code for newline

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
                if char == 10 then
                    -- ASCII code for '\\n'
                    updateRowColOverRange (currentOffset + 1) targetOffset chars (row + 1) 1

                else
                    updateRowColOverRange (currentOffset + 1) targetOffset chars row (col + 1)

            Nothing ->
                ( row, col )


newlineChar : Char
newlineChar =
    Pine_kernel.skip [ 1, 10 ]


isAsciiCode : Int -> Int -> String -> Bool
isAsciiCode code offset (String chars) =
    case Pine_kernel.skip [ offset, chars ] of
        [] ->
            False

        nextChar :: _ ->
            Pine_kernel.equal [ nextChar, Char.fromCode code ]
