module ElmMake.JavaScriptLiterals exposing (character, characterPattern, string, unicode)

import Char


string : String -> String
string value =
    "'" ++ (String.toList value |> List.map stringEscape |> String.concat) ++ "'"


character : Int -> String
character code =
    if code == 13 then
        "'\\r'"

    else
        characterPattern code


characterPattern : Int -> String
characterPattern code =
    let
        content =
            case code of
                9 ->
                    "\\t"

                10 ->
                    "\\n"

                34 ->
                    "\\\""

                39 ->
                    "\\'"

                92 ->
                    "\\\\"

                _ ->
                    if code < 32 || (code >= 127 && code <= 160) || List.member code (0x1680 :: 0x2028 :: 0x2029 :: 0x202F :: 0x205F :: 0x3000 :: List.range 0x2000 0x200A) then
                        unicode code

                    else
                        String.fromChar (Char.fromCode code)
    in
    "'" ++ content ++ "'"


stringEscape : Char -> String
stringEscape char =
    case char of
        '\'' ->
            "\\'"

        '\\' ->
            "\\\\"

        '\n' ->
            "\\n"

        '\u{000D}' ->
            "\\r"

        '\t' ->
            "\\t"

        '<' ->
            "\\u003C"

        _ ->
            if Char.toCode char < 32 || List.member (Char.toCode char) [ 0x2028, 0x2029 ] then
                unicode (Char.toCode char)

            else
                String.fromChar char


unicode : Int -> String
unicode code =
    let
        digit shift =
            String.slice (modBy 16 (code // shift)) (modBy 16 (code // shift) + 1) "0123456789ABCDEF"
    in
    "\\u" ++ digit 4096 ++ digit 256 ++ digit 16 ++ digit 1
