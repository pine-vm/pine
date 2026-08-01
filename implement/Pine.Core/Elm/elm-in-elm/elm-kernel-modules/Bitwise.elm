module Bitwise exposing
    ( and
    , complement
    , or
    , shiftLeftBy
    , shiftRightBy
    , shiftRightZfBy
    , xor
    )

{-
   Functions in the 'Bitwise' module emulate limits of JavaScript bitwise operations for backwards-compatibility.

   To provide an Elm core library that is backward-compatible with libraries and apps implemented for
   legacy platforms, simulate mapping from integer to two's complement and wrapping to 32-bit.
-}


and : Int -> Int -> Int
and a b =
    let
        bytesA =
            if
                Pine_builtin.int_is_sorted_asc
                    [ 0
                    , a
                    ]
            then
                Pine_builtin.bit_and
                    [ Pine_builtin.skip [ 1, a ]
                    , Pine_builtin.skip [ 1, 0xFFFFFFFF ]
                    ]

            else if
                Pine_builtin.int_is_sorted_asc
                    [ -0x80000000
                    , a
                    ]
            then
                Pine_builtin.bit_not
                    (Pine_builtin.bit_or
                        [ Pine_builtin.skip
                            [ 1
                            , Pine_builtin.int_add [ a, 1 ]
                            ]
                        , Pine_builtin.skip [ 2, 0x0000000100000000 ]
                        ]
                    )

            else
                Pine_builtin.bit_not
                    (Pine_builtin.bit_and
                        [ Pine_builtin.int_add [ a, 1 ]
                        , Pine_builtin.skip [ 1, 0xFFFFFFFF ]
                        ]
                    )

        bytesB =
            if
                Pine_builtin.int_is_sorted_asc
                    [ 0
                    , b
                    ]
            then
                Pine_builtin.bit_and
                    [ Pine_builtin.skip [ 1, b ]
                    , Pine_builtin.skip [ 1, 0xFFFFFFFF ]
                    ]

            else if
                Pine_builtin.int_is_sorted_asc
                    [ -0x80000000
                    , b
                    ]
            then
                Pine_builtin.bit_not
                    (Pine_builtin.bit_or
                        [ Pine_builtin.skip
                            [ 1
                            , Pine_builtin.int_add [ b, 1 ]
                            ]
                        , Pine_builtin.skip [ 2, 0x0000000100000000 ]
                        ]
                    )

            else
                Pine_builtin.bit_not
                    (Pine_builtin.bit_and
                        [ Pine_builtin.int_add [ b, 1 ]
                        , Pine_builtin.skip [ 1, 0xFFFFFFFF ]
                        ]
                    )

        combined =
            Pine_builtin.bit_and
                [ bytesA
                , bytesB
                ]
    in
    if
        Pine_builtin.equal
            [ Pine_builtin.bit_and
                [ combined
                , Pine_builtin.skip [ 1, 0x80000000 ]
                ]
            , Pine_builtin.skip [ 1, 0x80000000 ]
            ]
    then
        Pine_builtin.int_add
            [ Pine_builtin.concat
                [ Pine_builtin.take [ 1, 11 ]
                , Pine_builtin.bit_and
                    [ combined
                    , Pine_builtin.skip [ 1, 0x7FFFFFFF ]
                    ]
                ]
            , -0x80000000
            ]

    else
        Pine_builtin.int_add
            [ Pine_builtin.concat
                [ Pine_builtin.take [ 1, 11 ]
                , Pine_builtin.bit_and
                    [ combined
                    , Pine_builtin.skip [ 1, 0x7FFFFFFF ]
                    ]
                ]
            , 0
            ]


or : Int -> Int -> Int
or a b =
    let
        bytesA =
            if
                Pine_builtin.int_is_sorted_asc
                    [ 0
                    , a
                    ]
            then
                Pine_builtin.bit_and
                    [ Pine_builtin.skip [ 1, a ]
                    , Pine_builtin.skip [ 1, 0xFFFFFFFF ]
                    ]

            else if
                Pine_builtin.int_is_sorted_asc
                    [ -0x80000000
                    , a
                    ]
            then
                Pine_builtin.bit_not
                    (Pine_builtin.bit_or
                        [ Pine_builtin.skip
                            [ 1
                            , Pine_builtin.int_add [ a, 1 ]
                            ]
                        , Pine_builtin.skip [ 2, 0x0000000100000000 ]
                        ]
                    )

            else
                Pine_builtin.bit_not
                    (Pine_builtin.bit_and
                        [ Pine_builtin.int_add [ a, 1 ]
                        , Pine_builtin.skip [ 1, 0xFFFFFFFF ]
                        ]
                    )

        bytesB =
            if
                Pine_builtin.int_is_sorted_asc
                    [ 0
                    , b
                    ]
            then
                Pine_builtin.bit_and
                    [ Pine_builtin.skip [ 1, b ]
                    , Pine_builtin.skip [ 1, 0xFFFFFFFF ]
                    ]

            else if
                Pine_builtin.int_is_sorted_asc
                    [ -0x80000000
                    , b
                    ]
            then
                Pine_builtin.bit_not
                    (Pine_builtin.bit_or
                        [ Pine_builtin.skip
                            [ 1
                            , Pine_builtin.int_add [ b, 1 ]
                            ]
                        , Pine_builtin.skip [ 2, 0x0000000100000000 ]
                        ]
                    )

            else
                Pine_builtin.bit_not
                    (Pine_builtin.bit_and
                        [ Pine_builtin.int_add [ b, 1 ]
                        , Pine_builtin.skip [ 1, 0xFFFFFFFF ]
                        ]
                    )

        combined =
            Pine_builtin.bit_or
                [ bytesA
                , bytesB
                ]
    in
    if
        Pine_builtin.equal
            [ Pine_builtin.bit_and
                [ combined
                , Pine_builtin.skip [ 1, 0x80000000 ]
                ]
            , Pine_builtin.skip [ 1, 0x80000000 ]
            ]
    then
        Pine_builtin.int_add
            [ Pine_builtin.concat
                [ Pine_builtin.take [ 1, 11 ]
                , Pine_builtin.bit_and
                    [ combined
                    , Pine_builtin.skip [ 1, 0x7FFFFFFF ]
                    ]
                ]
            , -0x80000000
            ]

    else
        Pine_builtin.int_add
            [ Pine_builtin.concat
                [ Pine_builtin.take [ 1, 11 ]
                , Pine_builtin.bit_and
                    [ combined
                    , Pine_builtin.skip [ 1, 0x7FFFFFFF ]
                    ]
                ]
            , 0
            ]


xor : Int -> Int -> Int
xor a b =
    let
        bytesA =
            if
                Pine_builtin.int_is_sorted_asc
                    [ 0
                    , a
                    ]
            then
                Pine_builtin.bit_and
                    [ Pine_builtin.skip [ 1, a ]
                    , Pine_builtin.skip [ 1, 0xFFFFFFFF ]
                    ]

            else if
                Pine_builtin.int_is_sorted_asc
                    [ -0x80000000
                    , a
                    ]
            then
                Pine_builtin.bit_not
                    (Pine_builtin.bit_or
                        [ Pine_builtin.skip
                            [ 1
                            , Pine_builtin.int_add [ a, 1 ]
                            ]
                        , Pine_builtin.skip [ 2, 0x0000000100000000 ]
                        ]
                    )

            else
                Pine_builtin.bit_not
                    (Pine_builtin.bit_and
                        [ Pine_builtin.int_add [ a, 1 ]
                        , Pine_builtin.skip [ 1, 0xFFFFFFFF ]
                        ]
                    )

        bytesB =
            if
                Pine_builtin.int_is_sorted_asc
                    [ 0
                    , b
                    ]
            then
                Pine_builtin.bit_and
                    [ Pine_builtin.skip [ 1, b ]
                    , Pine_builtin.skip [ 1, 0xFFFFFFFF ]
                    ]

            else if
                Pine_builtin.int_is_sorted_asc
                    [ -0x80000000
                    , b
                    ]
            then
                Pine_builtin.bit_not
                    (Pine_builtin.bit_or
                        [ Pine_builtin.skip
                            [ 1
                            , Pine_builtin.int_add [ b, 1 ]
                            ]
                        , Pine_builtin.skip [ 2, 0x0000000100000000 ]
                        ]
                    )

            else
                Pine_builtin.bit_not
                    (Pine_builtin.bit_and
                        [ Pine_builtin.int_add [ b, 1 ]
                        , Pine_builtin.skip [ 1, 0xFFFFFFFF ]
                        ]
                    )

        combined =
            Pine_builtin.bit_xor
                [ bytesA
                , bytesB
                ]
    in
    if
        Pine_builtin.equal
            [ Pine_builtin.bit_and
                [ combined
                , Pine_builtin.skip [ 1, 0x80000000 ]
                ]
            , Pine_builtin.skip [ 1, 0x80000000 ]
            ]
    then
        Pine_builtin.int_add
            [ Pine_builtin.concat
                [ Pine_builtin.take [ 1, 11 ]
                , Pine_builtin.bit_and
                    [ combined
                    , Pine_builtin.skip [ 1, 0x7FFFFFFF ]
                    ]
                ]
            , -0x80000000
            ]

    else
        Pine_builtin.int_add
            [ Pine_builtin.concat
                [ Pine_builtin.take [ 1, 11 ]
                , Pine_builtin.bit_and
                    [ combined
                    , Pine_builtin.skip [ 1, 0x7FFFFFFF ]
                    ]
                ]
            , 0
            ]


complement : Int -> Int
complement asInt =
    if
        Pine_builtin.int_is_sorted_asc
            [ -0x80000000
            , asInt
            , 0x7FFFFFFF
            ]
    then
        Pine_builtin.int_add
            [ Pine_builtin.int_mul [ -1, asInt ]
            , -1
            ]

    else if
        Pine_builtin.equal
            [ Pine_builtin.bit_and
                [ asInt
                , Pine_builtin.skip [ 1, 0x80000000 ]
                ]
            , Pine_builtin.skip [ 1, 0x80000000 ]
            ]
    then
        Pine_builtin.int_add
            [ Pine_builtin.concat
                [ Pine_builtin.take [ 1, -11 ]
                , Pine_builtin.bit_and
                    [ asInt
                    , Pine_builtin.skip [ 1, 0x7FFFFFFF ]
                    ]
                ]
            , 0x7FFFFFFF
            ]

    else
        Pine_builtin.int_add
            [ Pine_builtin.concat
                [ Pine_builtin.take [ 1, 11 ]
                , Pine_builtin.bit_and
                    [ asInt
                    , Pine_builtin.skip [ 1, 0x7FFFFFFF ]
                    ]
                ]
            , -1
            ]


shiftLeftBy : Int -> Int -> Int
shiftLeftBy offset asInt =
    let
        withPadding =
            if
                Pine_builtin.int_is_sorted_asc
                    [ -0x80000000
                    , asInt
                    , -1
                    ]
            then
                Pine_builtin.bit_or
                    [ Pine_builtin.int_add
                        [ asInt
                        , 0x80000000
                        ]
                    , Pine_builtin.skip [ 1, 0x80000000 ]
                    ]

            else
                Pine_builtin.bit_or
                    [ Pine_builtin.skip [ 1, asInt ]
                    , Pine_builtin.skip [ 2, 0x0000000100000000 ]
                    ]

        beforeTruncate =
            Pine_builtin.bit_shift_left
                [ offset
                , withPadding
                ]
    in
    if
        Pine_builtin.equal
            [ Pine_builtin.bit_and
                [ beforeTruncate
                , Pine_builtin.skip [ 1, 0x80000000 ]
                ]
            , Pine_builtin.skip [ 1, 0x80000000 ]
            ]
    then
        Pine_builtin.int_add
            [ Pine_builtin.concat
                [ Pine_builtin.take [ 1, 11 ]
                , Pine_builtin.bit_and
                    [ beforeTruncate
                    , Pine_builtin.skip [ 1, 0x7FFFFFFF ]
                    ]
                ]
            , -0x80000000
            ]

    else
        Pine_builtin.int_add
            [ Pine_builtin.concat
                [ Pine_builtin.take [ 1, 11 ]
                , Pine_builtin.bit_and
                    [ beforeTruncate
                    , Pine_builtin.skip [ 1, 0x7FFFFFFF ]
                    ]
                ]
            , 0
            ]


shiftRightBy : Int -> Int -> Int
shiftRightBy offset asInt =
    if
        Pine_builtin.int_is_sorted_asc
            [ -0x80000000
            , asInt
            , 0x7FFFFFFF
            ]
    then
        if
            Pine_builtin.int_is_sorted_asc
                [ 0
                , asInt
                ]
        then
            let
                lessSign =
                    Pine_builtin.skip
                        [ 1
                        , Pine_builtin.int_add [ asInt, 0 ]
                        ]

                beforeTruncate =
                    Pine_builtin.bit_shift_right
                        [ offset
                        , lessSign
                        ]
            in
            Pine_builtin.int_add
                [ Pine_builtin.concat
                    [ Pine_builtin.take [ 1, 0 ]
                    , beforeTruncate
                    ]
                , 0
                ]

        else
            let
                lessSign =
                    Pine_builtin.skip
                        [ 1
                        , Pine_builtin.int_add [ asInt, -1 ]
                        ]

                beforeTruncate =
                    Pine_builtin.bit_shift_right
                        [ offset
                        , lessSign
                        ]
            in
            Pine_builtin.int_add
                [ Pine_builtin.concat
                    [ Pine_builtin.take [ 1, -1 ]
                    , beforeTruncate
                    ]
                , 0
                ]

    else
        let
            asInt32 =
                if
                    Pine_builtin.equal
                        [ Pine_builtin.bit_and
                            [ asInt
                            , Pine_builtin.skip [ 1, 0x80000000 ]
                            ]
                        , Pine_builtin.skip [ 1, 0x80000000 ]
                        ]
                then
                    Pine_builtin.int_add
                        [ Pine_builtin.concat
                            [ Pine_builtin.take [ 1, 11 ]
                            , Pine_builtin.bit_and
                                [ asInt
                                , Pine_builtin.skip [ 1, 0x7FFFFFFF ]
                                ]
                            ]
                        , -0x80000000
                        ]

                else
                    Pine_builtin.int_add
                        [ Pine_builtin.concat
                            [ Pine_builtin.take [ 1, -11 ]
                            , Pine_builtin.bit_and
                                [ asInt
                                , Pine_builtin.skip [ 1, 0x7FFFFFFF ]
                                ]
                            ]
                        , -1
                        ]
        in
        if
            Pine_builtin.int_is_sorted_asc
                [ 0
                , asInt32
                ]
        then
            let
                lessSign =
                    Pine_builtin.skip
                        [ 1
                        , Pine_builtin.int_add [ asInt32, 0 ]
                        ]

                beforeTruncate =
                    Pine_builtin.bit_shift_right
                        [ offset
                        , lessSign
                        ]
            in
            Pine_builtin.int_add
                [ Pine_builtin.concat
                    [ Pine_builtin.take [ 1, 0 ]
                    , beforeTruncate
                    ]
                , 0
                ]

        else
            let
                lessSign =
                    Pine_builtin.skip
                        [ 1
                        , Pine_builtin.int_add [ asInt32, -1 ]
                        ]

                beforeTruncate =
                    Pine_builtin.bit_shift_right
                        [ offset
                        , lessSign
                        ]
            in
            Pine_builtin.int_add
                [ Pine_builtin.concat
                    [ Pine_builtin.take [ 1, -1 ]
                    , beforeTruncate
                    ]
                , 0
                ]


shiftRightZfBy : Int -> Int -> Int
shiftRightZfBy offset bytes =
    let
        sign =
            Pine_builtin.take [ 1, bytes ]
    in
    if
        Pine_builtin.equal
            [ sign
            , Pine_builtin.take [ 1, 0 ]
            ]
    then
        let
            beforeTruncate =
                Pine_builtin.bit_shift_right
                    [ offset
                    , Pine_builtin.skip [ 1, bytes ]
                    ]
        in
        Pine_builtin.concat
            [ sign
            , trimLeadingZeros
                (Pine_builtin.reverse
                    (Pine_builtin.take
                        [ 4
                        , Pine_builtin.reverse beforeTruncate
                        ]
                    )
                )
            ]

    else
        let
            fromTwosComplement32 =
                Pine_builtin.bit_not
                    (Pine_builtin.reverse
                        (Pine_builtin.take
                            [ 4
                            , Pine_builtin.concat
                                [ Pine_builtin.reverse
                                    (Pine_builtin.skip
                                        [ 1
                                        , Pine_builtin.int_add
                                            [ bytes
                                            , 1
                                            ]
                                        ]
                                    )
                                , Pine_builtin.skip [ 2, 0x0000000100000000 ]
                                ]
                            ]
                        )
                    )

            beforeTruncate =
                Pine_builtin.bit_shift_right
                    [ offset
                    , fromTwosComplement32
                    ]
        in
        Pine_builtin.concat
            [ Pine_builtin.take [ 1, 0 ]
            , trimLeadingZeros beforeTruncate
            ]


trimLeadingZeros : Int -> Int
trimLeadingZeros bytes =
    Pine_builtin.skip
        [ 1
        , Pine_builtin.int_add
            [ Pine_builtin.concat
                [ Pine_builtin.take [ 1, 0 ]
                , bytes
                ]
            , 0
            ]
        ]
