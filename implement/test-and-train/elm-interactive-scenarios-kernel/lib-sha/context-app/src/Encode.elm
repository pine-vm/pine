module Encode exposing (encoder, toBytes)

import Bitwise
import Bytes exposing (Bytes, Endianness(..))
import Bytes.Encode as Encode exposing (Encoder)


toBytes : String -> Maybe Bytes
toBytes string =
    Maybe.map Encode.encode (encoder string)


encoder : String -> Maybe Encode.Encoder
encoder string =
    encodeChunks string []
        |> Maybe.map (List.reverse >> Encode.sequence)


{-| Big picture:

  - read 4 base64 characters
  - convert them to 3 bytes (24 bits)
  - encode these bytes

-}
encodeChunks : String -> List Encoder -> Maybe (List Encoder)
encodeChunks input accum =
    {- Performance Note

       slice and toList is just as fast as (possibly a little faster than) repeated `String.uncons`,
       but this code is much more readable
    -}
    case String.toList (String.left 4 input) of
        [] ->
            Just accum

        [ a, b, c, d ] ->
            case encodeCharacters a b c d of
                Just enc ->
                    encodeChunks (String.dropLeft 4 input) (enc :: accum)

                Nothing ->
                    Nothing

        [ a, b, c ] ->
            case encodeCharacters a b c '=' of
                Nothing ->
                    Nothing

                Just enc ->
                    Just (enc :: accum)

        [ a, b ] ->
            case encodeCharacters a b '=' '=' of
                Nothing ->
                    Nothing

                Just enc ->
                    Just (enc :: accum)

        _ ->
            Nothing


{-| Convert 4 characters to 24 bits (as an Encoder)
-}
encodeCharacters : Char -> Char -> Char -> Char -> Maybe Encoder
encodeCharacters a b c d =
    {- Performance notes

       We use bitshifts and other bitwise operators here. They are much faster than the alternatives.
       This may not normally matter but this function is called a lot so even small increases
       in efficiency are noticable

       Secondly, we combine two `uint8` into one `uint16 BE`. This has no direct speed benefit
       (elm/bytes uses a DataView, which only natively supports adding/reading uint8)
       but having fewer list items decreases # of encoding steps and allocation,
       and is therefore faster.
    -}
    if isValidChar a && isValidChar b then
        let
            n1 =
                unsafeConvertChar a

            n2 =
                unsafeConvertChar b
        in
        -- `=` is the padding character, and must be special-cased
        -- only the `c` and `d` char are allowed to be padding
        case d of
            '=' ->
                case c of
                    '=' ->
                        let
                            n =
                                Bitwise.or
                                    (Bitwise.shiftLeftBy 18 n1)
                                    (Bitwise.shiftLeftBy 12 n2)

                            b1 =
                                -- masking higher bits is not needed, Encode.unsignedInt8 ignores higher bits
                                Bitwise.shiftRightBy 16 n
                        in
                        Just (Encode.unsignedInt8 b1)

                    _ ->
                        if isValidChar c then
                            let
                                n3 =
                                    unsafeConvertChar c

                                n =
                                    Bitwise.or
                                        (Bitwise.or (Bitwise.shiftLeftBy 18 n1) (Bitwise.shiftLeftBy 12 n2))
                                        (Bitwise.shiftLeftBy 6 n3)

                                combined =
                                    Bitwise.shiftRightBy 8 n
                            in
                            Just (Encode.unsignedInt16 BE combined)

                        else
                            Nothing

            _ ->
                if isValidChar c && isValidChar d then
                    let
                        n3 =
                            unsafeConvertChar c

                        n4 =
                            unsafeConvertChar d

                        n =
                            Bitwise.or
                                (Bitwise.or (Bitwise.shiftLeftBy 18 n1) (Bitwise.shiftLeftBy 12 n2))
                                (Bitwise.or (Bitwise.shiftLeftBy 6 n3) n4)

                        b3 =
                            -- Masking the higher bits is not needed: Encode.unsignedInt8 ignores higher bits
                            n

                        combined =
                            Bitwise.shiftRightBy 8 n
                    in
                    Just
                        (Encode.sequence
                            [ Encode.unsignedInt16 BE combined
                            , Encode.unsignedInt8 b3
                            ]
                        )

                else
                    Nothing

    else
        Nothing


{-| is the character a base64 digit?

The base16 digits are: A-Z, a-z, 0-1, '+' and '/'

-}
isValidChar : Char -> Bool
isValidChar c =
    if Char.isAlphaNum c then
        True

    else
        case c of
            '+' ->
                True

            '/' ->
                True

            _ ->
                False


{-| Convert a base64 character/digit to its index

See also [Wikipedia](https://en.wikipedia.org/wiki/Base64#Base64_table)

-}
unsafeConvertChar : Char -> Int
unsafeConvertChar char =
    {- Performance Note

       Working with the key directly is faster than using e.g. `Char.isAlpha` and `Char.isUpper`
    -}
    let
        key =
            Char.toCode char
    in
    if key >= 65 && key <= 90 then
        -- A-Z
        key - 65

    else if key >= 97 && key <= 122 then
        -- a-z
        (key - 97) + 26

    else if key >= 48 && key <= 57 then
        -- 0-9
        (key - 48) + 26 + 26

    else
        case char of
            '+' ->
                62

            '/' ->
                63

            _ ->
                -1
