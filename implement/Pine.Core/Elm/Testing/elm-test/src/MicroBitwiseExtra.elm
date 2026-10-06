module MicroBitwiseExtra exposing
    ( int52FromTuple
    , int52ToTuple
    , isBitSet
    , keepBits
    , reverse52Bits
    , reverseNBits
    , signedToUnsigned
    )

import Bitwise


isBitSet : Int -> Int -> Bool
isBitSet index num =
    if index >= 32 then
        -- let's move this into the realm of 32bit numbers and try again
        isBitSet (index - 32) (num // 0xFFFFFFFF)

    else
        (num
            |> Bitwise.shiftRightZfBy index
            |> Bitwise.and 1
        )
            == 1


int52FromTuple : ( Int, Int ) -> Int
int52FromTuple ( highBits, lowBits ) =
    (+)
        (highBits
            |> keepBits 20
            |> signedToUnsigned
            -- Bitwise.shiftLeftBy 32 would be buggy, so we do:
            |> (*) 0x0000000100000000
        )
        (lowBits
            |> signedToUnsigned
            |> keepBits 32
        )


int52ToTuple : Int -> ( Int, Int )
int52ToTuple n =
    ( -- Bitwise.shiftRightZfBy 32 would be buggy, so we do:
      (n // 0x0000000100000000)
        |> keepBits 20
        |> signedToUnsigned
    , n
        |> keepBits 32
        |> signedToUnsigned
    )


ones : Int -> Int
ones count =
    if count == 32 then
        -- edge case...
        0xFFFFFFFF

    else
        Bitwise.shiftLeftBy count 1 - 1


keepBits : Int -> Int -> Int
keepBits count num =
    Bitwise.and (ones count) num


reverseByte : Int -> Int
reverseByte b_ =
    let
        go : Int -> Int -> Int -> Int
        go result i b =
            if i <= 0 then
                result

            else
                let
                    newResult =
                        result
                            |> Bitwise.shiftLeftBy 1
                            |> Bitwise.or (Bitwise.and 1 b)

                    newB =
                        Bitwise.shiftRightBy 1 b
                in
                go newResult (i - 1) newB
    in
    go 0 8 b_


memoizedReverseByte : Int -> Int
memoizedReverseByte b =
    if b < 0 || b > 255 then
        0

    else
        reverseByte b


reverse32Bits : Int -> Int
reverse32Bits n =
    Bitwise.shiftLeftBy 24 (memoizedReverseByte (Bitwise.and 0xFF (Bitwise.shiftRightBy 0 n)))
        |> Bitwise.or (Bitwise.shiftLeftBy 16 (memoizedReverseByte (Bitwise.and 0xFF (Bitwise.shiftRightBy 8 n))))
        |> Bitwise.or (Bitwise.shiftLeftBy 8 (memoizedReverseByte (Bitwise.and 0xFF (Bitwise.shiftRightBy 16 n))))
        |> Bitwise.or (Bitwise.shiftLeftBy 0 (memoizedReverseByte (Bitwise.and 0xFF (Bitwise.shiftRightBy 24 n))))
        |> signedToUnsigned


signedToUnsigned : Int -> Int
signedToUnsigned =
    Bitwise.shiftRightZfBy 0


{-| Beware,

  - `value` must be at most a 32bit number
  - `count` must be lower than the number of bits in `value`.

-}
reverseNBits : Int -> Int -> Int
reverseNBits count value =
    value
        |> reverse32Bits
        |> Bitwise.shiftRightZfBy (32 - count)


{-| A bit of bit twiddling between two "containers" to reverse a 52bit number.

Simplified view of the bits:

     hi20         lo32
     [ abcDEFGH ] [ IJKLMNOP ]
     ==>
     newHi20      newLo32
     [ abcPONML ] [ KJIHGFED ]

-}
reverse52Bits : ( Int, Int ) -> ( Int, Int )
reverse52Bits ( hi20, lo32 ) =
    let
        reversedLo32 =
            reverse32Bits lo32

        reversedHi20 =
            reverseNBits 20 hi20

        newHi20 =
            reversedLo32
                |> Bitwise.shiftRightBy 12
                |> keepBits 20
                |> signedToUnsigned

        newLo32 =
            reversedLo32
                |> keepBits 12
                |> Bitwise.shiftLeftBy 20
                |> Bitwise.or reversedHi20
                |> signedToUnsigned
    in
    ( newHi20, newLo32 )
