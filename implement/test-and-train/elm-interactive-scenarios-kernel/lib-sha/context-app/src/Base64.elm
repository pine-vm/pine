module Base64 exposing
    ( fromBytes, fromString, toBytes, toString
    , encoder, decoder
    )

{-| This package can convert
[bytes](https://package.elm-lang.org/packages/elm/bytes/latest/)
to Base64 strings and vice versa.


# Conversion

@docs fromBytes, fromString, toBytes, toString


# Bytes Encoder and Decoder

Slightly lower level functions.

[`fromBytes`](#fromBytes) and [`toBytes`](#toBytes) functions
are pretty much wrappers around these functions.

@docs encoder, decoder

-}

import Bytes exposing (Bytes)
import Bytes.Decode
import Bytes.Encode
import Decode
import Encode


{-| Convert bytes to a Base64 string.
If you want more control over the process, you should use [`decoder`](#decoder).

This function should never return `Nothing`, but it uses
[`Bytes.Decode.decode`](https://package.elm-lang.org/packages/elm/bytes/latest/Bytes-Decode#decode),
which returns a `Maybe String`.

-}
fromBytes : Bytes -> Maybe String
fromBytes =
    Decode.fromBytes


{-| Encode a string into a Base64 string.
This function is a wrapper around [`fromBytes`](#fromBytes).

Similarly, it should never return `Nothing`, but alas, [`Bytes.Decode.decode`](https://package.elm-lang.org/packages/elm/bytes/latest/Bytes-Decode#decode),
which [`fromBytes`](#fromBytes) uses, returns a `Maybe String`.

-}
fromString : String -> Maybe String
fromString string =
    string
        |> Bytes.Encode.string
        |> Bytes.Encode.encode
        |> fromBytes


{-| Convert a Base64 string to bytes.
If you want more control over the process, you should use [`encoder`](#encoder).

This function fails (returns `Nothing`) if you give it an invalid Base64 sequence.

-}
toBytes : String -> Maybe Bytes
toBytes =
    Encode.toBytes


{-| Decode a Base64 string into a string.
This function is a wrapper around [`toBytes`](#toBytes).

It will fail (return `Nothing`) if you give it an invalid Base64 sequence.

-}
toString : String -> Maybe String
toString b64String =
    case toBytes b64String of
        Nothing ->
            Nothing

        Just b64Bytes ->
            Bytes.Decode.decode
                (Bytes.Decode.string (Bytes.width b64Bytes))
                b64Bytes


{-| `decoder width` is a bytes decoder that will convert `width` bytes into a
Base64 string.

It's used in [`fromBytes`](#fromBytes):

    fromBytes : Bytes -> Maybe String
    fromBytes bytes =
        Bytes.Decode.decode (decoder (Bytes.width bytes)) bytes

-}
decoder : Int -> Bytes.Decode.Decoder String
decoder =
    Decode.decoder


{-| `encoder` returns a bytes encoder. It fails if the string that is passed
to it is not a valid Base64 sequence.

It's used in [`toBytes`](#toBytes):

    toBytes : String -> Maybe Bytes
    toBytes string =
        Maybe.map Bytes.Encode.encode (encoder string)

-}
encoder : String -> Maybe Bytes.Encode.Encoder
encoder =
    Encode.encoder
