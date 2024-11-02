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
   Emulate limits of JavaScript bitwise operations for backwards-compat

   To provide an Elm core library that is backward-compatible with libraries and apps implemented for
   classic environments, limit the functions in the `Bitwise` module to 32-bit.
-}


and : Int -> Int -> Int
and a b =
    Pine_kernel.concat
        [ Pine_kernel.take [ 1, 0 ]
        , Pine_kernel.reverse
            (Pine_kernel.take
                [ 4
                , Pine_kernel.reverse
                    (Pine_kernel.bit_and
                        [ Pine_kernel.skip [ 1, a ]
                        , Pine_kernel.skip [ 1, b ]
                        ]
                    )
                ]
            )
        ]


or : Int -> Int -> Int
or a b =
    Pine_kernel.concat
        [ Pine_kernel.take [ 1, 0 ]
        , Pine_kernel.reverse
            (Pine_kernel.take
                [ 4
                , Pine_kernel.reverse
                    (Pine_kernel.bit_or
                        [ Pine_kernel.skip [ 1, a ]
                        , Pine_kernel.skip [ 1, b ]
                        ]
                    )
                ]
            )
        ]


xor : Int -> Int -> Int
xor a b =
    Pine_kernel.concat
        [ Pine_kernel.take [ 1, 0 ]
        , Pine_kernel.reverse
            (Pine_kernel.take
                [ 4
                , Pine_kernel.reverse
                    (Pine_kernel.bit_xor
                        [ Pine_kernel.skip [ 1, a ]
                        , Pine_kernel.skip [ 1, b ]
                        ]
                    )
                ]
            )
        ]


complement : Int -> Int
complement a =
    Pine_kernel.concat
        [ Pine_kernel.take [ 1, 0 ]
        , Pine_kernel.bit_not (Pine_kernel.skip [ 1, a ])
        ]


shiftLeftBy : Int -> Int -> Int
shiftLeftBy offset bytes =
    let
        sign =
            Pine_kernel.take [ 1, bytes ]

        withExtension =
            Pine_kernel.concat
                [ Pine_kernel.skip [ 1, 0 ]
                , Pine_kernel.skip [ 1, 0 ]
                , Pine_kernel.skip [ 1, 0 ]
                , Pine_kernel.skip [ 1, bytes ]
                ]

        beforeTruncate =
            Pine_kernel.bit_shift_left
                [ offset
                , withExtension
                ]
    in
    Pine_kernel.concat
        [ sign
        , truncateLeadingZeros
            (Pine_kernel.reverse
                (Pine_kernel.take
                    [ 4
                    , Pine_kernel.reverse beforeTruncate
                    ]
                )
            )
        ]


shiftRightBy : Int -> Int -> Int
shiftRightBy offset bytes =
    let
        sign =
            Pine_kernel.take [ 1, bytes ]

        beforeTruncate =
            Pine_kernel.bit_shift_right
                [ offset
                , Pine_kernel.skip [ 1, bytes ]
                ]
    in
    Pine_kernel.concat
        [ sign
        , truncateLeadingZeros beforeTruncate
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
            , truncateLeadingZeros beforeTruncate
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
            , truncateLeadingZeros beforeTruncate
            ]


truncateLeadingZeros : Int -> Int
truncateLeadingZeros bytes =
    if
        Pine_kernel.equal
            [ Pine_kernel.length bytes
            , 1
            ]
    then
        bytes

    else if
        Pine_kernel.equal
            [ Pine_kernel.take [ 1, bytes ]
            , Pine_kernel.skip [ 1, 0 ]
            ]
    then
        truncateLeadingZeros (Pine_kernel.skip [ 1, bytes ])

    else
        bytes
