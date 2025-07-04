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
                Pine_kernel.int_is_sorted_asc
                    [ 0
                    , a
                    ]
            then
                Pine_kernel.bit_and
                    [ Pine_kernel.skip [ 1, a ]
                    , Pine_kernel.skip [ 1, 0xFFFFFFFF ]
                    ]

            else if
                Pine_kernel.int_is_sorted_asc
                    [ -0x80000000
                    , a
                    ]
            then
                Pine_kernel.bit_not
                    (Pine_kernel.bit_or
                        [ Pine_kernel.skip
                            [ 1
                            , Pine_kernel.int_add [ a, 1 ]
                            ]
                        , Pine_kernel.skip [ 2, 0x0000000100000000 ]
                        ]
                    )

            else
                Pine_kernel.bit_not
                    (Pine_kernel.bit_and
                        [ Pine_kernel.int_add [ a, 1 ]
                        , Pine_kernel.skip [ 1, 0xFFFFFFFF ]
                        ]
                    )

        bytesB =
            if
                Pine_kernel.int_is_sorted_asc
                    [ 0
                    , b
                    ]
            then
                Pine_kernel.bit_and
                    [ Pine_kernel.skip [ 1, b ]
                    , Pine_kernel.skip [ 1, 0xFFFFFFFF ]
                    ]

            else if
                Pine_kernel.int_is_sorted_asc
                    [ -0x80000000
                    , b
                    ]
            then
                Pine_kernel.bit_not
                    (Pine_kernel.bit_or
                        [ Pine_kernel.skip
                            [ 1
                            , Pine_kernel.int_add [ b, 1 ]
                            ]
                        , Pine_kernel.skip [ 2, 0x0000000100000000 ]
                        ]
                    )

            else
                Pine_kernel.bit_not
                    (Pine_kernel.bit_and
                        [ Pine_kernel.int_add [ b, 1 ]
                        , Pine_kernel.skip [ 1, 0xFFFFFFFF ]
                        ]
                    )

        combined =
            Pine_kernel.bit_and
                [ bytesA
                , bytesB
                ]
    in
    if
        Pine_kernel.equal
            [ Pine_kernel.bit_and
                [ combined
                , Pine_kernel.skip [ 1, 0x80000000 ]
                ]
            , Pine_kernel.skip [ 1, 0x80000000 ]
            ]
    then
        Pine_kernel.int_add
            [ Pine_kernel.concat
                [ Pine_kernel.take [ 1, 11 ]
                , Pine_kernel.bit_and
                    [ combined
                    , Pine_kernel.skip [ 1, 0x7FFFFFFF ]
                    ]
                ]
            , -0x80000000
            ]

    else
        Pine_kernel.int_add
            [ Pine_kernel.concat
                [ Pine_kernel.take [ 1, 11 ]
                , Pine_kernel.bit_and
                    [ combined
                    , Pine_kernel.skip [ 1, 0x7FFFFFFF ]
                    ]
                ]
            , 0
            ]


or : Int -> Int -> Int
or a b =
    let
        bytesA =
            if
                Pine_kernel.int_is_sorted_asc
                    [ 0
                    , a
                    ]
            then
                Pine_kernel.bit_and
                    [ Pine_kernel.skip [ 1, a ]
                    , Pine_kernel.skip [ 1, 0xFFFFFFFF ]
                    ]

            else if
                Pine_kernel.int_is_sorted_asc
                    [ -0x80000000
                    , a
                    ]
            then
                Pine_kernel.bit_not
                    (Pine_kernel.bit_or
                        [ Pine_kernel.skip
                            [ 1
                            , Pine_kernel.int_add [ a, 1 ]
                            ]
                        , Pine_kernel.skip [ 2, 0x0000000100000000 ]
                        ]
                    )

            else
                Pine_kernel.bit_not
                    (Pine_kernel.bit_and
                        [ Pine_kernel.int_add [ a, 1 ]
                        , Pine_kernel.skip [ 1, 0xFFFFFFFF ]
                        ]
                    )

        bytesB =
            if
                Pine_kernel.int_is_sorted_asc
                    [ 0
                    , b
                    ]
            then
                Pine_kernel.bit_and
                    [ Pine_kernel.skip [ 1, b ]
                    , Pine_kernel.skip [ 1, 0xFFFFFFFF ]
                    ]

            else if
                Pine_kernel.int_is_sorted_asc
                    [ -0x80000000
                    , b
                    ]
            then
                Pine_kernel.bit_not
                    (Pine_kernel.bit_or
                        [ Pine_kernel.skip
                            [ 1
                            , Pine_kernel.int_add [ b, 1 ]
                            ]
                        , Pine_kernel.skip [ 2, 0x0000000100000000 ]
                        ]
                    )

            else
                Pine_kernel.bit_not
                    (Pine_kernel.bit_and
                        [ Pine_kernel.int_add [ b, 1 ]
                        , Pine_kernel.skip [ 1, 0xFFFFFFFF ]
                        ]
                    )

        combined =
            Pine_kernel.bit_or
                [ bytesA
                , bytesB
                ]
    in
    if
        Pine_kernel.equal
            [ Pine_kernel.bit_and
                [ combined
                , Pine_kernel.skip [ 1, 0x80000000 ]
                ]
            , Pine_kernel.skip [ 1, 0x80000000 ]
            ]
    then
        Pine_kernel.int_add
            [ Pine_kernel.concat
                [ Pine_kernel.take [ 1, 11 ]
                , Pine_kernel.bit_and
                    [ combined
                    , Pine_kernel.skip [ 1, 0x7FFFFFFF ]
                    ]
                ]
            , -0x80000000
            ]

    else
        Pine_kernel.int_add
            [ Pine_kernel.concat
                [ Pine_kernel.take [ 1, 11 ]
                , Pine_kernel.bit_and
                    [ combined
                    , Pine_kernel.skip [ 1, 0x7FFFFFFF ]
                    ]
                ]
            , 0
            ]


xor : Int -> Int -> Int
xor a b =
    let
        bytesA =
            if
                Pine_kernel.int_is_sorted_asc
                    [ 0
                    , a
                    ]
            then
                Pine_kernel.bit_and
                    [ Pine_kernel.skip [ 1, a ]
                    , Pine_kernel.skip [ 1, 0xFFFFFFFF ]
                    ]

            else if
                Pine_kernel.int_is_sorted_asc
                    [ -0x80000000
                    , a
                    ]
            then
                Pine_kernel.bit_not
                    (Pine_kernel.bit_or
                        [ Pine_kernel.skip
                            [ 1
                            , Pine_kernel.int_add [ a, 1 ]
                            ]
                        , Pine_kernel.skip [ 2, 0x0000000100000000 ]
                        ]
                    )

            else
                Pine_kernel.bit_not
                    (Pine_kernel.bit_and
                        [ Pine_kernel.int_add [ a, 1 ]
                        , Pine_kernel.skip [ 1, 0xFFFFFFFF ]
                        ]
                    )

        bytesB =
            if
                Pine_kernel.int_is_sorted_asc
                    [ 0
                    , b
                    ]
            then
                Pine_kernel.bit_and
                    [ Pine_kernel.skip [ 1, b ]
                    , Pine_kernel.skip [ 1, 0xFFFFFFFF ]
                    ]

            else if
                Pine_kernel.int_is_sorted_asc
                    [ -0x80000000
                    , b
                    ]
            then
                Pine_kernel.bit_not
                    (Pine_kernel.bit_or
                        [ Pine_kernel.skip
                            [ 1
                            , Pine_kernel.int_add [ b, 1 ]
                            ]
                        , Pine_kernel.skip [ 2, 0x0000000100000000 ]
                        ]
                    )

            else
                Pine_kernel.bit_not
                    (Pine_kernel.bit_and
                        [ Pine_kernel.int_add [ b, 1 ]
                        , Pine_kernel.skip [ 1, 0xFFFFFFFF ]
                        ]
                    )

        combined =
            Pine_kernel.bit_xor
                [ bytesA
                , bytesB
                ]
    in
    if
        Pine_kernel.equal
            [ Pine_kernel.bit_and
                [ combined
                , Pine_kernel.skip [ 1, 0x80000000 ]
                ]
            , Pine_kernel.skip [ 1, 0x80000000 ]
            ]
    then
        Pine_kernel.int_add
            [ Pine_kernel.concat
                [ Pine_kernel.take [ 1, 11 ]
                , Pine_kernel.bit_and
                    [ combined
                    , Pine_kernel.skip [ 1, 0x7FFFFFFF ]
                    ]
                ]
            , -0x80000000
            ]

    else
        Pine_kernel.int_add
            [ Pine_kernel.concat
                [ Pine_kernel.take [ 1, 11 ]
                , Pine_kernel.bit_and
                    [ combined
                    , Pine_kernel.skip [ 1, 0x7FFFFFFF ]
                    ]
                ]
            , 0
            ]


complement : Int -> Int
complement asInt =
    if
        Pine_kernel.int_is_sorted_asc
            [ -0x80000000
            , asInt
            , 0x7FFFFFFF
            ]
    then
        Pine_kernel.int_add
            [ Pine_kernel.int_mul [ -1, asInt ]
            , -1
            ]

    else if
        Pine_kernel.equal
            [ Pine_kernel.bit_and
                [ asInt
                , Pine_kernel.skip [ 1, 0x80000000 ]
                ]
            , Pine_kernel.skip [ 1, 0x80000000 ]
            ]
    then
        Pine_kernel.int_add
            [ Pine_kernel.concat
                [ Pine_kernel.take [ 1, -11 ]
                , Pine_kernel.bit_and
                    [ asInt
                    , Pine_kernel.skip [ 1, 0x7FFFFFFF ]
                    ]
                ]
            , 0x7FFFFFFF
            ]

    else
        Pine_kernel.int_add
            [ Pine_kernel.concat
                [ Pine_kernel.take [ 1, 11 ]
                , Pine_kernel.bit_and
                    [ asInt
                    , Pine_kernel.skip [ 1, 0x7FFFFFFF ]
                    ]
                ]
            , -1
            ]


shiftLeftBy : Int -> Int -> Int
shiftLeftBy offset asInt =
    let
        withPadding =
            if
                Pine_kernel.int_is_sorted_asc
                    [ -0x80000000
                    , asInt
                    , -1
                    ]
            then
                Pine_kernel.bit_or
                    [ Pine_kernel.int_add
                        [ asInt
                        , 0x80000000
                        ]
                    , Pine_kernel.skip [ 1, 0x80000000 ]
                    ]

            else
                Pine_kernel.bit_or
                    [ Pine_kernel.skip [ 1, asInt ]
                    , Pine_kernel.skip [ 2, 0x0000000100000000 ]
                    ]

        beforeTruncate =
            Pine_kernel.bit_shift_left
                [ offset
                , withPadding
                ]
    in
    if
        Pine_kernel.equal
            [ Pine_kernel.bit_and
                [ beforeTruncate
                , Pine_kernel.skip [ 1, 0x80000000 ]
                ]
            , Pine_kernel.skip [ 1, 0x80000000 ]
            ]
    then
        Pine_kernel.int_add
            [ Pine_kernel.concat
                [ Pine_kernel.take [ 1, 11 ]
                , Pine_kernel.bit_and
                    [ beforeTruncate
                    , Pine_kernel.skip [ 1, 0x7FFFFFFF ]
                    ]
                ]
            , -0x80000000
            ]

    else
        Pine_kernel.int_add
            [ Pine_kernel.concat
                [ Pine_kernel.take [ 1, 11 ]
                , Pine_kernel.bit_and
                    [ beforeTruncate
                    , Pine_kernel.skip [ 1, 0x7FFFFFFF ]
                    ]
                ]
            , 0
            ]


shiftRightBy : Int -> Int -> Int
shiftRightBy offset asInt =
    if
        Pine_kernel.int_is_sorted_asc
            [ -0x80000000
            , asInt
            , 0x7FFFFFFF
            ]
    then
        if
            Pine_kernel.int_is_sorted_asc
                [ 0
                , asInt
                ]
        then
            let
                lessSign =
                    Pine_kernel.skip
                        [ 1
                        , Pine_kernel.int_add [ asInt, 0 ]
                        ]

                beforeTruncate =
                    Pine_kernel.bit_shift_right
                        [ offset
                        , lessSign
                        ]
            in
            Pine_kernel.int_add
                [ Pine_kernel.concat
                    [ Pine_kernel.take [ 1, 0 ]
                    , beforeTruncate
                    ]
                , 0
                ]

        else
            let
                lessSign =
                    Pine_kernel.skip
                        [ 1
                        , Pine_kernel.int_add [ asInt, -1 ]
                        ]

                beforeTruncate =
                    Pine_kernel.bit_shift_right
                        [ offset
                        , lessSign
                        ]
            in
            Pine_kernel.int_add
                [ Pine_kernel.concat
                    [ Pine_kernel.take [ 1, -1 ]
                    , beforeTruncate
                    ]
                , 0
                ]

    else
        let
            asInt32 =
                if
                    Pine_kernel.equal
                        [ Pine_kernel.bit_and
                            [ asInt
                            , Pine_kernel.skip [ 1, 0x80000000 ]
                            ]
                        , Pine_kernel.skip [ 1, 0x80000000 ]
                        ]
                then
                    Pine_kernel.int_add
                        [ Pine_kernel.concat
                            [ Pine_kernel.take [ 1, 11 ]
                            , Pine_kernel.bit_and
                                [ asInt
                                , Pine_kernel.skip [ 1, 0x7FFFFFFF ]
                                ]
                            ]
                        , -0x80000000
                        ]

                else
                    Pine_kernel.int_add
                        [ Pine_kernel.concat
                            [ Pine_kernel.take [ 1, -11 ]
                            , Pine_kernel.bit_and
                                [ asInt
                                , Pine_kernel.skip [ 1, 0x7FFFFFFF ]
                                ]
                            ]
                        , -1
                        ]
        in
        if
            Pine_kernel.int_is_sorted_asc
                [ 0
                , asInt32
                ]
        then
            let
                lessSign =
                    Pine_kernel.skip
                        [ 1
                        , Pine_kernel.int_add [ asInt32, 0 ]
                        ]

                beforeTruncate =
                    Pine_kernel.bit_shift_right
                        [ offset
                        , lessSign
                        ]
            in
            Pine_kernel.int_add
                [ Pine_kernel.concat
                    [ Pine_kernel.take [ 1, 0 ]
                    , beforeTruncate
                    ]
                , 0
                ]

        else
            let
                lessSign =
                    Pine_kernel.skip
                        [ 1
                        , Pine_kernel.int_add [ asInt32, -1 ]
                        ]

                beforeTruncate =
                    Pine_kernel.bit_shift_right
                        [ offset
                        , lessSign
                        ]
            in
            Pine_kernel.int_add
                [ Pine_kernel.concat
                    [ Pine_kernel.take [ 1, -1 ]
                    , beforeTruncate
                    ]
                , 0
                ]


shiftRightZfBy : Int -> Int -> Int
shiftRightZfBy offset bytes =
    let
        sign =
            Pine_kernel.take [ 1, bytes ]
    in
    if
        Pine_kernel.equal
            [ sign
            , Pine_kernel.take [ 1, 0 ]
            ]
    then
        let
            beforeTruncate =
                Pine_kernel.bit_shift_right
                    [ offset
                    , Pine_kernel.skip [ 1, bytes ]
                    ]
        in
        Pine_kernel.concat
            [ sign
            , trimLeadingZeros
                (Pine_kernel.reverse
                    (Pine_kernel.take
                        [ 4
                        , Pine_kernel.reverse beforeTruncate
                        ]
                    )
                )
            ]

    else
        let
            fromTwosComplement32 =
                Pine_kernel.bit_not
                    (Pine_kernel.reverse
                        (Pine_kernel.take
                            [ 4
                            , Pine_kernel.concat
                                [ Pine_kernel.reverse
                                    (Pine_kernel.skip
                                        [ 1
                                        , Pine_kernel.int_add
                                            [ bytes
                                            , 1
                                            ]
                                        ]
                                    )
                                , Pine_kernel.skip [ 2, 0x0000000100000000 ]
                                ]
                            ]
                        )
                    )

            beforeTruncate =
                Pine_kernel.bit_shift_right
                    [ offset
                    , fromTwosComplement32
                    ]
        in
        Pine_kernel.concat
            [ Pine_kernel.take [ 1, 0 ]
            , trimLeadingZeros beforeTruncate
            ]


trimLeadingZeros : Int -> Int
trimLeadingZeros bytes =
    Pine_kernel.skip
        [ 1
        , Pine_kernel.int_add
            [ Pine_kernel.concat
                [ Pine_kernel.take [ 1, 0 ]
                , bytes
                ]
            , 0
            ]
        ]
