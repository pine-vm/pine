module SHA256 exposing
    ( Digest
    , fromString
    , fromBytes
    , fromByteValues
    , toHex, toBase64
    , toBytes, toByteValues
    , wordToHex
    )

{-| [SHA-256] is a [cryptographic hash function] that gives 128 bits of security.

[SHA-256]: http://www.iwar.org.uk/comsec/resources/cipher/sha256-384-512.pdf
[cryptographic hash function]: https://en.wikipedia.org/wiki/Cryptographic_hash_function

@docs Digest


# Creating digests

@docs fromString
@docs fromBytes
@docs fromByteValues


# Formatting digests

@docs toHex, toBase64


# To binary data

@docs toBytes, toByteValues

-}

import Base64
import Bitwise exposing (and, complement, or, shiftLeftBy, shiftRightZfBy)
import Bytes exposing (Bytes, Endianness(..))
import Bytes.Decode as Decode exposing (Decoder, Step(..))
import Bytes.Encode as Encode
import Hex
import Internal.SHA256 as Internal exposing (Digest(..), Tuple8(..))



-- TYPES


{-| An abstract sha256 digest
-}
type alias Digest =
    Internal.Digest



-- CALCULATING


{-| Create a digest from a `String`.

    "hello world"
        |> SHA256.fromString
        |> SHA256.toHex
    --> "b94d27b9934d3e08a52e52d7da7dabfac484efe37a5380ee9088f7ace2efcde9"

-}
fromString : String -> Digest
fromString =
    Internal.fromString initialState


{-| Create a digest from integer byte values.
Values are considered mod 256, which means that larger than 255 overflow.

    SHA256.fromByteValues
        [72, 105, 33, 32, 240, 159, 152, 132]
    --> SHA256.fromString "Hi! 😄"

    [0x00, 0xFF, 0x34, 0xA5]
        |> SHA256.fromByteValues
        |> SHA256.toBase64
    --> "SQ/kWBD948Wwg8toEA/O0cPo+oodOMYnZc8HmexxyCs="

-}
fromByteValues : List Int -> Digest
fromByteValues =
    Internal.fromByteValues initialState


{-| Create a digest from a [`Bytes`](https://package.elm-lang.org/packages/elm/bytes/latest/)

    import Bytes.Encode as Encode
    import Bytes exposing (Bytes, Endianness(..))

    buffer : Bytes
    buffer = Encode.encode (Encode.unsignedInt32 BE 42)

    SHA256.fromBytes buffer
        |> SHA256.toHex
        --> "ae3c8b8d99a39542f78af83dbbb42c81cd94199ec1b5f60a0801063e95842570"

-}
fromBytes : Bytes -> Digest
fromBytes =
    Internal.hashBytes initialState


initialState : Internal.State
initialState =
    Internal.State (Tuple8 0x6A09E667 0xBB67AE85 0x3C6EF372 0xA54FF53A 0x510E527F 0x9B05688C 0x1F83D9AB 0x5BE0CD19)



-- FORMATTING


{-| Get the individual byte values as integers

    "And the band begins to play"
        |> SHA256.fromString
        |> SHA256.toByteValues
        --> [ 0xb1, 0x13, 0x61, 0x72, 0xce, 0xf9, 0x6d, 0xe6, 0xf0, 0x61, 0x58, 0xd1, 0x43, 0x34, 0x32, 0xaa, 0xaf, 0xe7, 0x68, 0xf, 0xd3, 0xb4, 0x6f, 0x55, 0x92, 0xcd, 0xed, 0xb3, 0x3a, 0xf5, 0x7a, 0x50 ]

-}
toByteValues : Digest -> List Int
toByteValues (Digest (Tuple8 a b c d e f g h)) =
    List.concatMap wordToBytes [ a, b, c, d, e, f, g, h ]


wordToBytes : Int -> List Int
wordToBytes int =
    [ int |> shiftRightZfBy 0x18 |> and 0xFF
    , int |> shiftRightZfBy 0x10 |> and 0xFF
    , int |> shiftRightZfBy 0x08 |> and 0xFF
    , int |> and 0xFF
    ]


toEncoder : Digest -> Encode.Encoder
toEncoder (Digest (Tuple8 a b c d e f g h)) =
    Encode.sequence
        [ Encode.unsignedInt32 BE a
        , Encode.unsignedInt32 BE b
        , Encode.unsignedInt32 BE c
        , Encode.unsignedInt32 BE d
        , Encode.unsignedInt32 BE e
        , Encode.unsignedInt32 BE f
        , Encode.unsignedInt32 BE g
        , Encode.unsignedInt32 BE h
        ]


{-| Turn a digest into `Bytes`. The digest is stored as 8 big-endian 32-bit unsigned integers, so the width is 32 bytes or 256 bits.
-}
toBytes : Digest -> Bytes
toBytes =
    Encode.encode << toEncoder


{-| Represent the digest as a string of hexadecimal digits.

    "And our friends are all aboard"
        |> SHA256.fromString
        |> SHA256.toHex
    --> "a40bc1de58430a446e4b446a722fdfd493375c93bf93b1066793909f717da796"

-}
toHex : Digest -> String
toHex (Digest (Tuple8 a b c d e f g h)) =
    wordToHex a
        ++ wordToHex b
        ++ wordToHex c
        ++ wordToHex d
        ++ wordToHex e
        ++ wordToHex f
        ++ wordToHex g
        ++ wordToHex h


wordToHex : Int -> String
wordToHex byte =
    byte
        -- force integer to be unsigned
        |> Bitwise.shiftRightZfBy 0
        |> Hex.toString
        |> String.padLeft 8 '0'



-- Base64 uses 1 character per 6 bits, which doesn't divide very nicely into our
-- 5 32-bit  integers! The  base64 digest  is 28  characters long,  although the
-- final character  is a '=',  which means it's  padded. Therefore, it  uses 162
-- bits  of entropy  to display  our 160  bit  digest, so  the digest  has 2  0s
-- appended.


{-| Represent the digest as its base-64 encoding.

    "Many more of them live next door"
        |> SHA256.fromString
        |> SHA256.toBase64
    --> "1ov4iAbzsCXuC2R9heu+y57YF/Seb5Vu8cRvVyEY6jM="

-}
toBase64 : Digest -> String
toBase64 digest =
    digest
        |> toEncoder
        |> Encode.encode
        |> Base64.fromBytes
        |> Maybe.withDefault ""
