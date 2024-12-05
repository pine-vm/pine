module Bytes.Decode exposing (..)

import Bytes



-- PARSER


{-| Describes how to turn a sequence of bytes into a nice Elm value.
-}
type Decoder a
    = Decoder (Bytes -> Int -> ( Int, a ))


{-| Turn a sequence of bytes into a nice Elm value.

    -- decode (unsignedInt16 BE) <0007> == Just 7
    -- decode (unsignedInt16 LE) <0700> == Just 7
    -- decode (unsignedInt16 BE) <0700> == Just 1792
    -- decode (unsignedInt32 BE) <0700> == Nothing



The `Decoder` specifies exactly how this should happen. This process may fail
if the sequence of bytes is corrupted or unexpected somehow. The examples above
show a case where there are not enough bytes.

-}
decode : Decoder a -> Bytes -> Maybe a
decode (Decoder decoder) bytes =
    let
        (Bytes.Elm_Bytes blob) =
            bytes

        ( offset, result ) =
            decoder bytes 0

        blobLength =
            Pine_kernel.length blob
    in
    if Pine_kernel.int_is_sorted_asc [ 0, offset, blobLength ] then
        Just result

    else
        Nothing


bytes : Int -> Decoder Bytes
bytes length =
    Decoder
        (\blob offset ->
            ( Pine_kernel.int_add [ offset, length ]
            , Bytes.Elm_Bytes (Pine_kernel.take [ length, Pine_kernel.skip [ offset, blob ] ])
            )
        )


{-| Decode one byte into an integer from `0` to `255`.
-}
unsignedInt8 : Decoder Int
unsignedInt8 =
    Decoder
        (\(Bytes.Elm_Bytes blob) offset ->
            let
                byte =
                    Pine_kernel.take [ 1, Pine_kernel.skip [ offset, blob ] ]
            in
            ( Pine_kernel.int_add [ offset, 1 ]
            , Pine_kernel.concat [ Pine_kernel.take [ 1, 0 ], byte ]
            )
        )


signedInt8 : Decoder Int
signedInt8 =
    Decoder
        (\(Bytes.Elm_Bytes blob) offset ->
            let
                byte =
                    Pine_kernel.take [ 1, Pine_kernel.skip [ offset, blob ] ]

                asInt =
                    if Pine_kernel.equal [ Pine_kernel.bit_and [ byte, 0x80 ], 0 ] then
                        Pine_kernel.concat [ Pine_kernel.take [ 1, 0 ], byte ]

                    else
                        Pine_kernel.int_add
                            [ -1
                            , Pine_kernel.concat
                                [ Pine_kernel.take [ 1, -1 ]
                                , Pine_kernel.bit_not byte
                                ]
                            ]
            in
            ( Pine_kernel.int_add [ offset, 1 ]
            , asInt
            )
        )


unsignedInt16 : Bytes.Endianness -> Decoder Int
unsignedInt16 endianness =
    Decoder
        (\(Bytes.Elm_Bytes blob) offset ->
            let
                bytes =
                    Pine_kernel.take [ 2, Pine_kernel.skip [ offset, blob ] ]

                asInt =
                    if Pine_kernel.equal [ endianness, Bytes.LE ] then
                        Pine_kernel.concat
                            [ Pine_kernel.take [ 1, 0 ]
                            , Pine_kernel.reverse bytes
                            ]

                    else
                        Pine_kernel.concat
                            [ Pine_kernel.take [ 1, 0 ]
                            , bytes
                            ]
            in
            ( Pine_kernel.int_add [ offset, 2 ]
            , asInt
            )
        )


signedInt16 : Bytes.Endianness -> Decoder Int
signedInt16 endianness =
    Decoder
        (\(Bytes.Elm_Bytes blob) offset ->
            let
                bytes =
                    Pine_kernel.take [ 2, Pine_kernel.skip [ offset, blob ] ]

                bytesOrdered =
                    if Pine_kernel.equal [ endianness, Bytes.LE ] then
                        Pine_kernel.reverse bytes

                    else
                        bytes

                asInt =
                    if
                        Pine_kernel.equal
                            [ Pine_kernel.bit_and [ bytesOrdered, 0x8000 ]
                            , Pine_kernel.skip [ 2, 0x00010000 ]
                            ]
                    then
                        Pine_kernel.concat [ Pine_kernel.take [ 1, 0 ], bytesOrdered ]

                    else
                        Pine_kernel.int_add
                            [ -1
                            , Pine_kernel.concat
                                [ Pine_kernel.take [ 1, -1 ]
                                , Pine_kernel.bit_not bytesOrdered
                                ]
                            ]
            in
            ( Pine_kernel.int_add [ offset, 2 ]
            , asInt
            )
        )


unsignedInt32 : Bytes.Endianness -> Decoder Int
unsignedInt32 endianness =
    Decoder
        (\(Bytes.Elm_Bytes blob) offset ->
            let
                bytes =
                    Pine_kernel.take [ 4, Pine_kernel.skip [ offset, blob ] ]

                asInt =
                    if Pine_kernel.equal [ endianness, Bytes.LE ] then
                        Pine_kernel.concat
                            [ Pine_kernel.take [ 1, 0 ]
                            , Pine_kernel.reverse bytes
                            ]

                    else
                        Pine_kernel.concat [ Pine_kernel.take [ 1, 0 ], bytes ]
            in
            ( Pine_kernel.int_add [ offset, 4 ]
            , asInt
            )
        )


signedInt32 : Bytes.Endianness -> Decoder Int
signedInt32 endianness =
    Decoder
        (\(Bytes.Elm_Bytes blob) offset ->
            let
                bytes =
                    Pine_kernel.take [ 4, Pine_kernel.skip [ offset, blob ] ]

                bytesOrdered =
                    if Pine_kernel.equal [ endianness, Bytes.LE ] then
                        Pine_kernel.reverse bytes

                    else
                        bytes

                asInt =
                    if
                        Pine_kernel.equal
                            [ Pine_kernel.bit_and [ bytesOrdered, 0x80000000 ]
                            , Pine_kernel.skip [ 2, 0x0000000100000000 ]
                            ]
                    then
                        Pine_kernel.concat [ Pine_kernel.take [ 1, 0 ], bytesOrdered ]

                    else
                        Pine_kernel.int_add
                            [ -1
                            , Pine_kernel.concat
                                [ Pine_kernel.take [ 1, -1 ]
                                , Pine_kernel.bit_not bytesOrdered
                                ]
                            ]
            in
            ( Pine_kernel.int_add [ offset, 4 ]
            , asInt
            )
        )


string : Int -> Decoder String
string length =
    Decoder
        (\(Bytes.Elm_Bytes blob) offset ->
            let
                bytes =
                    Pine_kernel.take [ length, Pine_kernel.skip [ offset, blob ] ]
            in
            ( Pine_kernel.int_add [ offset, length ]
            , decodeBlobAsChars bytes
            )
        )


decodeBlobAsChars : Int -> String
decodeBlobAsChars blob =
    decodeBlobAsCharsRec 0 blob []


decodeBlobAsCharsRec : Int -> Int -> List Char -> String
decodeBlobAsCharsRec offset blob chars =
    if Pine_kernel.int_is_sorted_asc [ Pine_kernel.length blob, offset ] then
        String.fromList (List.reverse chars)

    else
        let
            ( char, bytesConsumed ) =
                decodeUtf8Char blob offset
        in
        decodeBlobAsCharsRec
            (offset + bytesConsumed)
            blob
            (char :: chars)


decodeUtf8Char : Int -> Int -> ( Int, Int )
decodeUtf8Char blob offset =
    let
        firstByte =
            Pine_kernel.take [ 1, Pine_kernel.skip [ offset, blob ] ]

        firstByteInt =
            Pine_kernel.concat [ Pine_kernel.take [ 1, 0 ], firstByte ]
    in
    if Pine_kernel.int_is_sorted_asc [ firstByteInt, 0x7F ] then
        -- 1-byte character (ASCII)
        ( firstByte, 1 )

    else if Pine_kernel.equal [ Pine_kernel.bit_and [ firstByteInt, 0xE0 ], 0xC0 ] then
        -- 2-byte character
        let
            byte2 =
                Pine_kernel.take [ 1, Pine_kernel.skip [ Pine_kernel.int_add [ offset, 1 ], blob ] ]

            byte2Int =
                Pine_kernel.concat [ Pine_kernel.take [ 1, 0 ], byte2 ]

            firstFiveBits =
                Pine_kernel.bit_and [ firstByteInt, 0x1F ]

            secondSixBits =
                Pine_kernel.bit_and [ byte2Int, 0x3F ]

            charCode =
                Pine_kernel.int_add
                    [ Pine_kernel.int_mul [ firstFiveBits, 64 ] -- Multiply by 2^6
                    , secondSixBits
                    ]
        in
        ( Pine_kernel.skip [ 1, charCode ], 2 )

    else if Pine_kernel.equal [ Pine_kernel.bit_and [ firstByteInt, 0xF0 ], 0xE0 ] then
        -- 3-byte character
        let
            byte2 =
                Pine_kernel.take [ 1, Pine_kernel.skip [ Pine_kernel.int_add [ offset, 1 ], blob ] ]

            byte2Int =
                Pine_kernel.concat [ Pine_kernel.take [ 1, 0 ], byte2 ]

            byte3 =
                Pine_kernel.take [ 1, Pine_kernel.skip [ Pine_kernel.int_add [ offset, 2 ], blob ] ]

            byte3Int =
                Pine_kernel.concat [ Pine_kernel.take [ 1, 0 ], byte3 ]

            firstFourBits =
                Pine_kernel.bit_and [ firstByteInt, 0x0F ]

            secondSixBits =
                Pine_kernel.bit_and [ byte2Int, 0x3F ]

            thirdSixBits =
                Pine_kernel.bit_and [ byte3Int, 0x3F ]

            charCode =
                Pine_kernel.int_add
                    [ Pine_kernel.int_mul [ firstFourBits, 4096 ] -- Multiply by 2^12
                    , Pine_kernel.int_mul [ secondSixBits, 64 ] -- Multiply by 2^6
                    , thirdSixBits
                    ]
        in
        ( Pine_kernel.skip [ 1, charCode ], 3 )

    else if Pine_kernel.equal [ Pine_kernel.bit_and [ firstByteInt, 0xF8 ], 0xF0 ] then
        -- 4-byte character
        let
            byte2 =
                Pine_kernel.take [ 1, Pine_kernel.skip [ Pine_kernel.int_add [ offset, 1 ], blob ] ]

            byte2Int =
                Pine_kernel.concat [ Pine_kernel.take [ 1, 0 ], byte2 ]

            byte3 =
                Pine_kernel.take [ 1, Pine_kernel.skip [ Pine_kernel.int_add [ offset, 2 ], blob ] ]

            byte3Int =
                Pine_kernel.concat [ Pine_kernel.take [ 1, 0 ], byte3 ]

            byte4 =
                Pine_kernel.take [ 1, Pine_kernel.skip [ Pine_kernel.int_add [ offset, 3 ], blob ] ]

            byte4Int =
                Pine_kernel.concat [ Pine_kernel.take [ 1, 0 ], byte4 ]

            firstThreeBits =
                Pine_kernel.bit_and [ firstByteInt, 0x07 ]

            secondSixBits =
                Pine_kernel.bit_and [ byte2Int, 0x3F ]

            thirdSixBits =
                Pine_kernel.bit_and [ byte3Int, 0x3F ]

            fourthSixBits =
                Pine_kernel.bit_and [ byte4Int, 0x3F ]

            charCode =
                Pine_kernel.int_add
                    [ Pine_kernel.int_mul [ firstThreeBits, 262144 ] -- Multiply by 2^18
                    , Pine_kernel.int_mul [ secondSixBits, 4096 ] -- Multiply by 2^12
                    , Pine_kernel.int_mul [ thirdSixBits, 64 ] -- Multiply by 2^6
                    , fourthSixBits
                    ]
        in
        ( Pine_kernel.skip [ 1, charCode ], 4 )

    else
        -- Invalid UTF-8 sequence; use replacement character
        ( Pine_kernel.skip [ 1, 0xFFFD ], 1 )


succeed : a -> Decoder a
succeed a =
    Decoder (\_ offset -> ( offset, a ))


map : (a -> b) -> Decoder a -> Decoder b
map func (Decoder decodeA) =
    Decoder
        (\bites offset ->
            let
                ( aOffset, a ) =
                    decodeA bites offset
            in
            ( aOffset, func a )
        )


type Step state a
    = Loop state
    | Done a


loop : state -> (state -> Decoder (Step state a)) -> Decoder a
loop state callback =
    Decoder (loopHelp state callback)


loopHelp : state -> (state -> Decoder (Step state a)) -> Bytes -> Int -> ( Int, a )
loopHelp state callback bites offset =
    let
        (Decoder decoder) =
            callback state

        ( newOffset, step ) =
            decoder bites offset
    in
    case step of
        Loop newState ->
            loopHelp newState callback bites newOffset

        Done result ->
            ( newOffset, result )


map2 : (a -> b -> value) -> Decoder a -> Decoder b -> Decoder value
map2 func (Decoder decodeA) (Decoder decodeB) =
    Decoder
        (\bites offset ->
            let
                ( offsetA, a ) =
                    decodeA bites offset

                ( offsetB, b ) =
                    decodeB bites offsetA
            in
            ( offsetB, func a b )
        )


map3 : (a -> b -> c -> value) -> Decoder a -> Decoder b -> Decoder c -> Decoder value
map3 func (Decoder decodeA) (Decoder decodeB) (Decoder decodeC) =
    Decoder
        (\bites offset ->
            let
                ( offsetA, a ) =
                    decodeA bites offset

                ( offsetB, b ) =
                    decodeB bites offsetA

                ( offsetC, c ) =
                    decodeC bites offsetB
            in
            ( offsetC, func a b c )
        )


map4 :
    (a -> b -> c -> d -> value)
    -> Decoder a
    -> Decoder b
    -> Decoder c
    -> Decoder d
    -> Decoder value
map4 func (Decoder decodeA) (Decoder decodeB) (Decoder decodeC) (Decoder decodeD) =
    Decoder
        (\bites offset ->
            let
                ( offsetA, a ) =
                    decodeA bites offset

                ( offsetB, b ) =
                    decodeB bites offsetA

                ( offsetC, c ) =
                    decodeC bites offsetB

                ( offsetD, d ) =
                    decodeD bites offsetC
            in
            ( offsetD, func a b c d )
        )


map5 :
    (a -> b -> c -> d -> e -> value)
    -> Decoder a
    -> Decoder b
    -> Decoder c
    -> Decoder d
    -> Decoder e
    -> Decoder value
map5 func (Decoder decodeA) (Decoder decodeB) (Decoder decodeC) (Decoder decodeD) (Decoder decodeE) =
    Decoder
        (\bites offset ->
            let
                ( offsetA, a ) =
                    decodeA bites offset

                ( offsetB, b ) =
                    decodeB bites offsetA

                ( offsetC, c ) =
                    decodeC bites offsetB

                ( offsetD, d ) =
                    decodeD bites offsetC

                ( offsetE, e ) =
                    decodeE bites offsetD
            in
            ( offsetE, func a b c d e )
        )


andThen : (a -> Decoder b) -> Decoder a -> Decoder b
andThen callback (Decoder decodeA) =
    Decoder
        (\bites offset ->
            let
                ( offsetA, a ) =
                    decodeA bites offset

                (Decoder decodeB) =
                    callback a

                ( offsetB, b ) =
                    decodeB bites offsetA
            in
            ( offsetB, b )
        )
