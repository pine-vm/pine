module Bitwise exposing (and, complement, or, shiftLeftBy, shiftRightBy, shiftRightZfBy, xor)

{-| Functions in the 'Bitwise' module emulate limits of JavaScript bitwise operations for backwards-compatibility.

   To provide an Elm core library that is backward-compatible with libraries and apps implemented for
   legacy platforms, simulate mapping from integer to two's complement and wrapping to 32-bit.
-}


and : Int -> Int -> Int
and a b =
    fromSignedBytes (Pine_builtin.bit_and [ toBytes a, toBytes b ])


or : Int -> Int -> Int
or a b =
    fromSignedBytes (Pine_builtin.bit_or [ toBytes a, toBytes b ])


xor : Int -> Int -> Int
xor a b =
    fromSignedBytes (Pine_builtin.bit_xor [ toBytes a, toBytes b ])


complement : Int -> Int
complement value =
    fromSignedBytes (Pine_builtin.bit_not (toBytes value))


shiftLeftBy : Int -> Int -> Int
shiftLeftBy offset value =
    fromSignedBytes (Pine_builtin.bit_shift_left [ shiftCount offset, toBytes value ])


shiftRightBy : Int -> Int -> Int
shiftRightBy offset value =
    let
        bytes =
            toBytes value

        count =
            shiftCount offset

        shifted =
            Pine_builtin.bit_shift_right [ count, bytes ]
    in
    if hasSignBit bytes then
        fromSignedBytes
            (Pine_builtin.bit_or
                [ shifted
                , Pine_builtin.bit_not
                    (Pine_builtin.bit_shift_right [ count, Pine_builtin.skip [ 1, 0xFFFFFFFF ] ])
                ]
            )

    else
        fromUnsignedBytes shifted


shiftRightZfBy : Int -> Int -> Int
shiftRightZfBy offset value =
    fromUnsignedBytes (Pine_builtin.bit_shift_right [ shiftCount offset, toBytes value ])


shiftCount : Int -> Int
shiftCount offset =
    fromUnsignedBytes (Pine_builtin.bit_and [ toBytes offset, Pine_builtin.skip [ 1, 31 ] ])


toBytes : Int -> Int
toBytes value =
    if Pine_builtin.int_is_sorted_asc [ 0, value ] then
        padLow32 (Pine_builtin.skip [ 1, Pine_builtin.int_add [ value, 0 ] ])

    else
        Pine_builtin.bit_not
            (padLow32 (Pine_builtin.skip [ 1, Pine_builtin.int_add [ value, 1 ] ]))


padLow32 : Int -> Int
padLow32 bytes =
    Pine_builtin.reverse
        (Pine_builtin.take
            [ 4
            , Pine_builtin.concat
                [ Pine_builtin.reverse bytes
                , Pine_builtin.skip [ 2, 0x0000000100000000 ]
                ]
            ]
        )


hasSignBit : Int -> Bool
hasSignBit bytes =
    Pine_builtin.equal
        [ Pine_builtin.bit_and [ bytes, Pine_builtin.skip [ 1, 0x80000000 ] ]
        , Pine_builtin.skip [ 1, 0x80000000 ]
        ]


fromUnsignedBytes : Int -> Int
fromUnsignedBytes bytes =
    Pine_builtin.int_add
        [ Pine_builtin.concat [ Pine_builtin.take [ 1, 0 ], bytes ]
        , 0
        ]


fromSignedBytes : Int -> Int
fromSignedBytes bytes =
    Pine_builtin.int_add
        [ fromUnsignedBytes bytes
        , if hasSignBit bytes then
            -0x0000000100000000

          else
            0
        ]
