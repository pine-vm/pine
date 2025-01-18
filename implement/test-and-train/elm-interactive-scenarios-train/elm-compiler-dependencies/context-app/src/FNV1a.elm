module FNV1a exposing (hash, hashWithSeed, initialSeed)

{-|

@docs hash, hashWithSeed, initialSeed

-}

import Bitwise


{-| The initial seed represents the starting point of a hash.
In other words:

    hash "" == initialSeed

-}
initialSeed : Int
initialSeed =
    0x811C9DC5


{-| Turn a string into an integer value based on its contents.
When passed the empty string, the `initialSeed` is returned.

    hash "Lorem ipsum" == 2898375356

-}
hash : String -> Int
hash str =
    hashWithSeed str initialSeed


{-| Like `hash`, this turns a string into an integer value.
It differs in that you can provide your own seed, or your own initial hash value.
This allows you to hash two, or more, strings in sequence without concatenating them first.

    hashWithSeed "ipsum" (hash "Lorem ") == hash "Lorem ipsum"

-}
hashWithSeed : String -> Int -> Int
hashWithSeed str seed =
    let
        chars : List Char
        chars =
            String.toList str
    in
    Bitwise.shiftRightZfBy 0 (List.foldl utf32ToUtf8 seed chars)


utf32ToUtf8 : Char -> Int -> Int
utf32ToUtf8 char acc =
    {- Implementation copied from: https://github.com/zwilias/elm-utf-tools/tree/2.0.1 -}
    let
        byte : Int
        byte =
            Char.toCode char
    in
    if byte < 0x80 then
        hasher byte acc

    else if byte < 0x0800 then
        acc
            |> hasher (Bitwise.or 0xC0 <| Bitwise.shiftRightZfBy 6 byte)
            |> hasher (Bitwise.or 0x80 <| Bitwise.and 0x3F byte)

    else if byte < 0x00010000 then
        acc
            |> hasher (Bitwise.or 0xE0 <| Bitwise.shiftRightZfBy 12 byte)
            |> hasher (Bitwise.or 0x80 <| Bitwise.and 0x3F <| Bitwise.shiftRightZfBy 6 byte)
            |> hasher (Bitwise.or 0x80 <| Bitwise.and 0x3F byte)

    else
        acc
            |> hasher (Bitwise.or 0xF0 <| Bitwise.shiftRightZfBy 18 byte)
            |> hasher (Bitwise.or 0x80 <| Bitwise.and 0x3F <| Bitwise.shiftRightZfBy 12 byte)
            |> hasher (Bitwise.or 0x80 <| Bitwise.and 0x3F <| Bitwise.shiftRightZfBy 6 byte)
            |> hasher (Bitwise.or 0x80 <| Bitwise.and 0x3F byte)


hasher : Int -> Int -> Int
hasher byte hashValue =
    {- Implementation ported from: https://gist.github.com/vaiorabbit/5657561 -}
    let
        mixed : Int
        mixed =
            Bitwise.xor byte hashValue
    in
    mixed
        + Bitwise.shiftLeftBy 1 mixed
        + Bitwise.shiftLeftBy 4 mixed
        + Bitwise.shiftLeftBy 7 mixed
        + Bitwise.shiftLeftBy 8 mixed
        + Bitwise.shiftLeftBy 24 mixed
