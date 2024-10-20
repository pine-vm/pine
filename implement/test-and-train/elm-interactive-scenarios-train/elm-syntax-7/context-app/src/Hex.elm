module Hex exposing (fromString, toString)

{-| Convert to and from Hex strings.

@docs fromString, toString

-}


{-| Convert a hexdecimal string such as "abc94f" to a decimal integer.

    Hex.fromString "a5" == Ok 165
    Hex.fromString "hat" == Err "invalid hexadecimal string"

-}
fromString : String -> Result String Int
fromString str =
    if String.isEmpty str then
        Err "Empty strings are not valid hexadecimal strings."

    else
        let
            result =
                if String.startsWith "-" str then
                    let
                        list =
                            str
                                |> String.toList
                                |> List.tail
                                |> Maybe.withDefault []
                    in
                    fromStringHelp (List.length list - 1) list 0
                        |> Result.map negate

                else
                    fromStringHelp (String.length str - 1) (String.toList str) 0

            formatError err =
                String.join " "
                    [ "\"" ++ str ++ "\""
                    , "is not a valid hexadecimal string because"
                    , err
                    ]
        in
        Result.mapError formatError result


fromStringHelp : Int -> List Char -> Int -> Result String Int
fromStringHelp position chars accumulated =
    case chars of
        [] ->
            Ok accumulated

        char :: rest ->
            -- NOTE: It's important to have this call `fromStringHelp` directly.
            -- Previously this called a helper function, but that meant this
            -- was not tail-call optimized; it did not compile to a `while` loop
            -- the way it does now. See 240c3d5aa4f97463b924728935d2989621e9fd6b
            case char of
                '0' ->
                    fromStringHelp (position - 1) rest accumulated

                '1' ->
                    fromStringHelp (position - 1) rest (accumulated + (16 ^ position))

                '2' ->
                    fromStringHelp (position - 1) rest (accumulated + (2 * (16 ^ position)))

                '3' ->
                    fromStringHelp (position - 1) rest (accumulated + (3 * (16 ^ position)))

                '4' ->
                    fromStringHelp (position - 1) rest (accumulated + (4 * (16 ^ position)))

                '5' ->
                    fromStringHelp (position - 1) rest (accumulated + (5 * (16 ^ position)))

                '6' ->
                    fromStringHelp (position - 1) rest (accumulated + (6 * (16 ^ position)))

                '7' ->
                    fromStringHelp (position - 1) rest (accumulated + (7 * (16 ^ position)))

                '8' ->
                    fromStringHelp (position - 1) rest (accumulated + (8 * (16 ^ position)))

                '9' ->
                    fromStringHelp (position - 1) rest (accumulated + (9 * (16 ^ position)))

                'a' ->
                    fromStringHelp (position - 1) rest (accumulated + (10 * (16 ^ position)))

                'b' ->
                    fromStringHelp (position - 1) rest (accumulated + (11 * (16 ^ position)))

                'c' ->
                    fromStringHelp (position - 1) rest (accumulated + (12 * (16 ^ position)))

                'd' ->
                    fromStringHelp (position - 1) rest (accumulated + (13 * (16 ^ position)))

                'e' ->
                    fromStringHelp (position - 1) rest (accumulated + (14 * (16 ^ position)))

                'f' ->
                    fromStringHelp (position - 1) rest (accumulated + (15 * (16 ^ position)))

                nonHex ->
                    Err (String.fromChar nonHex ++ " is not a valid hexadecimal character.")


{-| Convert a decimal integer to a hexdecimal string such as `"abc94f"`.

    Hex.toString 165 == "a5"

-}
toString : Int -> String
toString num =
    String.fromList <|
        if num < 0 then
            '-' :: unsafePositiveToDigits [] (negate num)

        else
            unsafePositiveToDigits [] num


{-| ONLY EVER CALL THIS WITH POSITIVE INTEGERS!
-}
unsafePositiveToDigits : List Char -> Int -> List Char
unsafePositiveToDigits digits num =
    if num < 16 then
        unsafeToDigit num :: digits

    else
        unsafePositiveToDigits (unsafeToDigit (modBy 16 num) :: digits) (num // 16)


{-| ONLY EVER CALL THIS WITH INTEGERS BETWEEN 0 and 15!
-}
unsafeToDigit : Int -> Char
unsafeToDigit num =
    case num of
        0 ->
            '0'

        1 ->
            '1'

        2 ->
            '2'

        3 ->
            '3'

        4 ->
            '4'

        5 ->
            '5'

        6 ->
            '6'

        7 ->
            '7'

        8 ->
            '8'

        9 ->
            '9'

        10 ->
            'a'

        11 ->
            'b'

        12 ->
            'c'

        13 ->
            'd'

        14 ->
            'e'

        15 ->
            'f'

        _ ->
            -- if this ever gets called with a number over 15, it will never
            -- terminate! If that happens, debug further by uncommenting this:
            --
            -- Debug.todo ("Tried to convert " ++ toString num ++ " to hexadecimal.")
            unsafeToDigit num
