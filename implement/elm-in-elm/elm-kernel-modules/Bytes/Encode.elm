module Bytes.Encode exposing (..)

import Bytes exposing (Bytes, Endianness)


type Encoder
    = I8 Int
    | I16 Endianness Int
    | I32 Endianness Int
    | U8 Int
    | U16 Endianness Int
    | U32 Endianness Int
    | SequenceEncoder (List Encoder)
    | BytesEncoder Bytes.Bytes


encode : Encoder -> Bytes.Bytes
encode builder =
    Bytes.Elm_Bytes (encodeBlob builder)


encodeBlob : Encoder -> Int
encodeBlob builder =
    case builder of
        I8 n ->
            if Pine_builtin.int_is_sorted_asc [ 0, n ] then
                Pine_builtin.take [ 1, Pine_builtin.reverse n ]

            else
                Pine_builtin.take
                    [ 1
                    , Pine_builtin.reverse
                        (Pine_builtin.bit_not (Pine_builtin.int_add [ n, 1 ]))
                    ]

        I16 e n ->
            let
                littleEndian =
                    if Pine_builtin.int_is_sorted_asc [ 0, n ] then
                        Pine_builtin.take
                            [ 2
                            , Pine_builtin.concat
                                [ Pine_builtin.reverse
                                    (Pine_builtin.skip [ 1, n ])
                                , Pine_builtin.skip [ 1, 0 ]
                                ]
                            ]

                    else
                        Pine_builtin.take
                            [ 2
                            , Pine_builtin.concat
                                [ Pine_builtin.reverse
                                    (Pine_builtin.skip
                                        [ 1
                                        , Pine_builtin.bit_not (Pine_builtin.int_add [ n, 1 ])
                                        ]
                                    )
                                , Pine_builtin.skip [ 1, 0xFF ]
                                ]
                            ]
            in
            if Pine_builtin.equal [ e, Bytes.LE ] then
                littleEndian

            else
                Pine_builtin.reverse littleEndian

        I32 e n ->
            let
                littleEndian =
                    if Pine_builtin.int_is_sorted_asc [ 0, n ] then
                        Pine_builtin.take
                            [ 4
                            , Pine_builtin.concat
                                [ Pine_builtin.reverse
                                    (Pine_builtin.skip [ 1, n ])
                                , Pine_builtin.skip [ 2, 0x01000000 ]
                                ]
                            ]

                    else
                        Pine_builtin.take
                            [ 4
                            , Pine_builtin.concat
                                [ Pine_builtin.reverse
                                    (Pine_builtin.skip
                                        [ 1
                                        , Pine_builtin.bit_not (Pine_builtin.int_add [ n, 1 ])
                                        ]
                                    )
                                , Pine_builtin.skip [ 1, 0x00FFFFFF ]
                                ]
                            ]
            in
            if Pine_builtin.equal [ e, Bytes.LE ] then
                littleEndian

            else
                Pine_builtin.reverse littleEndian

        U8 n ->
            Pine_builtin.take [ 1, Pine_builtin.reverse n ]

        U16 e n ->
            let
                littleEndian =
                    Pine_builtin.take
                        [ 2
                        , Pine_builtin.concat
                            [ Pine_builtin.reverse
                                (Pine_builtin.skip [ 1, n ])
                            , Pine_builtin.skip [ 1, 0 ]
                            ]
                        ]
            in
            if Pine_builtin.equal [ e, Bytes.LE ] then
                littleEndian

            else
                Pine_builtin.reverse littleEndian

        U32 e n ->
            let
                littleEndian =
                    Pine_builtin.take
                        [ 4
                        , Pine_builtin.concat
                            [ Pine_builtin.reverse
                                (Pine_builtin.skip [ 1, n ])
                            , Pine_builtin.skip [ 2, 0x01000000 ]
                            ]
                        ]
            in
            if Pine_builtin.equal [ e, Bytes.LE ] then
                littleEndian

            else
                Pine_builtin.reverse littleEndian

        SequenceEncoder bs ->
            if Pine_builtin.equal [ bs, [] ] then
                Pine_builtin.take [ 0, 0 ]

            else
                Pine_builtin.concat (List.map encodeBlob bs)

        BytesEncoder (Bytes.Elm_Bytes blob) ->
            blob



-- INTEGERS


{-| Encode integers from `0` to `255` in one byte.
-}
unsignedInt8 : Int -> Encoder
unsignedInt8 int =
    U8 int


signedInt8 : Int -> Encoder
signedInt8 int =
    I8 int


{-| Encode integers from `0` to `65535` in two bytes.
-}
unsignedInt16 : Endianness -> Int -> Encoder
unsignedInt16 int =
    U16 int


signedInt16 : Endianness -> Int -> Encoder
signedInt16 int =
    I16 int


{-| Encode integers from `0` to `4294967295` in four bytes.
-}
unsignedInt32 : Endianness -> Int -> Encoder
unsignedInt32 int =
    U32 int


signedInt32 : Endianness -> Int -> Encoder
signedInt32 int =
    I32 int


bytes : Bytes.Bytes -> Encoder
bytes b =
    BytesEncoder b


sequence : List Encoder -> Encoder
sequence builders =
    SequenceEncoder builders


string : String -> Encoder
string (String charsBytes) =
    BytesEncoder
        (Bytes.Elm_Bytes (encodeCharsAsBlob charsBytes))


encodeCharsAsBlob : Int -> Int
encodeCharsAsBlob charsBytes =
    encodeCharsAsBlobHelp emptyBlob 0 charsBytes


encodeCharsAsBlobHelp : Int -> Int -> Int -> Int
encodeCharsAsBlobHelp acc offset charsBytes =
    let
        nextChar =
            Pine_builtin.take [ 4, Pine_builtin.skip [ offset, charsBytes ] ]
    in
    if Pine_builtin.equal [ Pine_builtin.length nextChar, 0 ] then
        acc

    else
        encodeCharsAsBlobHelp
            (Pine_builtin.concat [ acc, encodeCharAsBlob nextChar ])
            (Pine_builtin.int_add [ offset, 4 ])
            charsBytes


encodeCharAsBlob : Char -> Int
encodeCharAsBlob char =
    let
        code =
            Char.toCode char
    in
    if Pine_builtin.int_is_sorted_asc [ code, 0x7F ] then
        -- 1-byte encoding
        Pine_builtin.skip
            [ 1
            , code
            ]

    else if Pine_builtin.int_is_sorted_asc [ code, 0x07FF ] then
        -- 2-byte encoding
        let
            byte1 =
                Pine_builtin.bit_or
                    [ 0xC0
                    , code // 64
                    ]

            byte2 =
                Pine_builtin.bit_or
                    [ maskSingleByteMSB
                    , Pine_builtin.bit_and [ 63, code ]
                    ]
        in
        Pine_builtin.concat
            [ Pine_builtin.bit_and [ byte1, maskSingleByte ]
            , Pine_builtin.bit_and [ byte2, maskSingleByte ]
            ]

    else if Pine_builtin.int_is_sorted_asc [ code, 0xFFFF ] then
        -- 3-byte encoding
        let
            byte1 =
                Pine_builtin.bit_or
                    [ 0xE0
                    , code // 4096
                    ]

            byte2 =
                Pine_builtin.bit_or
                    [ maskSingleByteMSB
                    , Pine_builtin.bit_and [ 63, code // 64 ]
                    ]

            byte3 =
                Pine_builtin.bit_or
                    [ maskSingleByteMSB
                    , Pine_builtin.bit_and [ 63, code ]
                    ]
        in
        Pine_builtin.concat
            [ Pine_builtin.bit_and [ byte1, maskSingleByte ]
            , Pine_builtin.bit_and [ byte2, maskSingleByte ]
            , Pine_builtin.bit_and [ byte3, maskSingleByte ]
            ]

    else
        -- 4-byte encoding for code points >= 0x10000
        let
            byte1 =
                Pine_builtin.bit_or
                    [ Pine_builtin.bit_and [ 0xF0, maskSingleByte ]
                    , code // 262144
                    ]

            byte2 =
                Pine_builtin.bit_or
                    [ maskSingleByteMSB
                    , Pine_builtin.bit_and [ 63, code // 4096 ]
                    ]

            byte3 =
                Pine_builtin.bit_or
                    [ maskSingleByteMSB
                    , Pine_builtin.bit_and [ 63, code // 64 ]
                    ]

            byte4 =
                Pine_builtin.bit_or
                    [ maskSingleByteMSB
                    , Pine_builtin.bit_and [ 63, code ]
                    ]
        in
        Pine_builtin.concat
            [ Pine_builtin.bit_and [ byte1, maskSingleByte ]
            , Pine_builtin.bit_and [ byte2, maskSingleByte ]
            , Pine_builtin.bit_and [ byte3, maskSingleByte ]
            , Pine_builtin.bit_and [ byte4, maskSingleByte ]
            ]


getStringWidth : String -> Int
getStringWidth (String charsBytes) =
    Pine_builtin.length
        (encodeCharsAsBlob charsBytes)


maskSingleByte : Int
maskSingleByte =
    Pine_builtin.skip [ 1, 0xFF ]


maskSingleByteMSB : Int
maskSingleByteMSB =
    Pine_builtin.skip [ 1, 0x80 ]


emptyBlob : Int
emptyBlob =
    Pine_builtin.take [ 0, 0 ]
