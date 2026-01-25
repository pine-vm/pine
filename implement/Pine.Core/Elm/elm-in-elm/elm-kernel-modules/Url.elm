module Url exposing
    ( Protocol(..)
    , Url
    , fromString
    , percentDecode
    , percentEncode
    , toString
    )

{-| In [the URI spec](https://tools.ietf.org/html/rfc3986), Tim Berners-Lee
says a URL looks like this:

      https://example.com:8042/over/there?name=ferret#nose
      \___/   \______________/\_________/ \_________/ \__/
        |            |            |            |        |
      scheme     authority       path        query   fragment

When you are creating a single-page app with [`Browser.fullscreen`][fs], you
use the [`Url.Parser`](Url-Parser) module to turn a `Url` into even nicer data.

If you want to create your own URLs, check out the [`Url.Builder`](Url-Builder)
module as well!

[fs]: /packages/elm/browser/latest/Browser#fullscreen

**Note:** This is a subset of all the full possibilities listed in the URI
spec. Specifically, it does not accept the `userinfo` segment you see in email
addresses like `tom@example.com`.

-}


type alias Url =
    { protocol : Protocol
    , host : String
    , port_ : Maybe Int
    , path : String
    , query : Maybe String
    , fragment : Maybe String
    }


{-| Is the URL served over a secure connection or not?
-}
type Protocol
    = Http
    | Https


fromString : String -> Maybe Url
fromString str =
    if String.startsWith "http://" str then
        chompAfterProtocol Http (String.dropLeft 7 str)

    else if String.startsWith "https://" str then
        chompAfterProtocol Https (String.dropLeft 8 str)

    else
        Nothing


chompAfterProtocol : Protocol -> String -> Maybe Url
chompAfterProtocol protocol str =
    if String.isEmpty str then
        Nothing

    else
        case String.indexes "#" str of
            [] ->
                chompBeforeFragment protocol Nothing str

            i :: _ ->
                chompBeforeFragment protocol (Just (String.dropLeft (i + 1) str)) (String.left i str)


chompBeforeFragment : Protocol -> Maybe String -> String -> Maybe Url
chompBeforeFragment protocol frag str =
    if String.isEmpty str then
        Nothing

    else
        case String.indexes "?" str of
            [] ->
                chompBeforeQuery protocol Nothing frag str

            i :: _ ->
                chompBeforeQuery protocol (Just (String.dropLeft (i + 1) str)) frag (String.left i str)


chompBeforeQuery : Protocol -> Maybe String -> Maybe String -> String -> Maybe Url
chompBeforeQuery protocol params frag str =
    if String.isEmpty str then
        Nothing

    else
        case String.indexes "/" str of
            [] ->
                chompBeforePath protocol "/" params frag str

            i :: _ ->
                chompBeforePath protocol (String.dropLeft i str) params frag (String.left i str)


chompBeforePath : Protocol -> String -> Maybe String -> Maybe String -> String -> Maybe Url
chompBeforePath protocol path params frag str =
    if String.isEmpty str || String.contains "@" str then
        Nothing

    else
        case String.indexes ":" str of
            [] ->
                Just <| Url protocol str Nothing path params frag

            i :: [] ->
                case String.toInt (String.dropLeft (i + 1) str) of
                    Nothing ->
                        Nothing

                    port_ ->
                        Just <| Url protocol (String.left i str) port_ path params frag

            _ ->
                Nothing


{-| Turn a [`Url`](#Url) into a `String`.
-}
toString : Url -> String
toString url =
    let
        http =
            case url.protocol of
                Http ->
                    "http://"

                Https ->
                    "https://"
    in
    addPort url.port_ (http ++ url.host)
        ++ url.path
        |> addPrefixed "?" url.query
        |> addPrefixed "#" url.fragment


addPort : Maybe Int -> String -> String
addPort maybePort starter =
    case maybePort of
        Nothing ->
            starter

        Just port_ ->
            starter ++ ":" ++ String.fromInt port_


addPrefixed : String -> Maybe String -> String -> String
addPrefixed prefix maybeSegment starter =
    case maybeSegment of
        Nothing ->
            starter

        Just segment ->
            starter ++ prefix ++ segment



-- PERCENT ENCODING


{-| **Use [Url.Builder](Url-Builder) instead!** Functions like `absolute`,
`relative`, and `crossOrigin` already do this automatically! `percentEncode`
is only available so that extremely custom cases are possible, if needed.

Percent-encoding is how [the official URI spec][uri] “escapes” special
characters. You can still represent a `?` even though it is reserved for
queries.

This function exists in case you want to do something extra custom. Here are
some examples:

    -- standard ASCII encoding
    percentEncode "hat" == "hat"

    percentEncode "to be" == "to%20be"

    percentEncode "99%" == "99%25"

    -- non-standard, but widely accepted, UTF-8 encoding
    percentEncode "$" == "%24"

    percentEncode "¢" == "%C2%A2"

    percentEncode "€" == "%E2%82%AC"

This is the same behavior as JavaScript's [`encodeURIComponent`][js] function,
and the rules are described in more detail officially [here][s2] and with some
notes about Unicode [here][wiki].

[js]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/encodeURIComponent
[uri]: https://tools.ietf.org/html/rfc3986
[s2]: https://tools.ietf.org/html/rfc3986#section-2.1
[wiki]: https://en.wikipedia.org/wiki/Percent-encoding

-}
percentEncode : String -> String
percentEncode string =
    string
        |> String.toList
        |> List.concatMap encodeChar
        |> String.fromList


encodeChar : Char -> List Char
encodeChar char =
    if isUnreserved char then
        [ char ]

    else
        let
            code =
                Char.toCode char
        in
        if code < 128 then
            encodeByte code

        else
            encodeUtf8 code


encodeByte : Int -> List Char
encodeByte byte =
    let
        upper : Int
        upper =
            byte // 16
    in
    [ '%'
    , hexDigitChar upper
    , hexDigitChar (byte - upper * 16)
    ]


encodeUtf8 : Int -> List Char
encodeUtf8 code =
    let
        upperPart : Int
        upperPart =
            code // 64

        lowerByte : Int
        lowerByte =
            code - upperPart * 64
    in
    List.concat
        [ encodeByte (0xC0 + upperPart)
        , encodeByte (0x80 + lowerByte)
        ]


{-| **Use [Url.Parser](Url-Parser) instead!** It will decode query
parameters appropriately already! `percentDecode` is only available so that
extremely custom cases are possible, if needed.

Check out the `percentEncode` function to learn about percent-encoding.
This function does the opposite! Here are the reverse examples:

    -- ASCII
    percentDecode "99%25" == Just "hat"

    percentDecode "to%20be" == Just "to be"

    percentDecode "hat" == Just "99%"

    -- UTF-8
    percentDecode "%24" == Just "$"

    percentDecode "%C2%A2" == Just "¢"

    percentDecode "%E2%82%AC" == Just "€"

Why is it a `Maybe` though? Well, these strings come from strangers on the
internet as a bunch of bits and may have encoding problems. For example:

    percentDecode "%" == Nothing -- not followed by two hex digits

    percentDecode "%XY" == Nothing -- not followed by two HEX digits

    percentDecode "%C2" == Nothing -- half of the "¢" encoding "%C2%A2"

This is the same behavior as JavaScript's [`decodeURIComponent`][js] function.

[js]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/decodeURIComponent

-}
percentDecode : String -> Maybe String
percentDecode input =
    input
        |> String.toList
        |> decodeAll
        |> Maybe.map String.fromList



{-

   {-| Step through the entire list of characters, decoding one UTF-8 character at a
       time. If anything is malformed, we return Nothing.
   -}
   decodeAll : List Char -> Maybe (List Char)
   decodeAll chars =
       case chars of
           [] ->
               Just []

           _ ->
               case decodeOne chars of
                   Nothing ->
                       Nothing

                   Just (decodedChar, rest) ->
                       case decodeAll rest of
                           Nothing ->
                               Nothing

                           Just decodedChars ->
                               Just (decodedChar :: decodedChars)


   {-| Decode exactly one UTF-8 character from the front of the list. This function
       returns `(character, restOfList)` or `Nothing` if malformed.
   -}
   decodeOne : List Char -> Maybe ( Char, List Char )
   decodeOne chars =
       case chars of
           -- If it is NOT a '%', then it is an unescaped character:
           c :: rest ->
               if c /= '%' ->
               Just ( c, rest )

           -- If it IS '%', parse the next two hex digits as a byte
           '%' :: r1 :: r2 :: rest ->
               case parseHex2 r1 r2 of
                   Nothing ->
                       Nothing

                   Just b ->
                       decodeUtf8Byte b rest

           -- Anything else means incomplete "%xx" or empty:
           _ ->
               Nothing
-}


decodeAll : List Char -> Maybe (List Char)
decodeAll chars =
    decodeRec 0 chars []


decodeRec : Int -> List Char -> List Char -> Maybe (List Char)
decodeRec offset chars acc =
    case List.take 1 (List.drop offset chars) of
        [] ->
            Just (List.reverse acc)

        _ ->
            case decodeOne offset chars of
                Nothing ->
                    Nothing

                Just ( decodedChar, newOffset ) ->
                    decodeRec newOffset chars (decodedChar :: acc)


decodeOne : Int -> List Char -> Maybe ( Char, Int )
decodeOne offset chars =
    case List.take 1 (List.drop offset chars) of
        [ '%' ] ->
            case List.drop (offset + 1) chars of
                r1 :: r2 :: rest ->
                    case parseHex2 r1 r2 of
                        Nothing ->
                            Nothing

                        Just b ->
                            case decodeUtf8Byte b rest of
                                Nothing ->
                                    Nothing

                                Just ( decodedChar, remainingChars ) ->
                                    let
                                        consumedCount : Int
                                        consumedCount =
                                            List.length rest - List.length remainingChars
                                    in
                                    Just
                                        ( decodedChar
                                        , offset + 3 + consumedCount
                                        )

                _ ->
                    Nothing

        [ c ] ->
            Just ( c, offset + 1 )

        _ ->
            Nothing


{-| We have read one byte `b` (from "%xx"). Next step is to see if it is:

  - < 128 => single-byte ASCII
  - 0xC0..0xDF => 2-byte UTF-8
  - 0xE0..0xEF => 3-byte UTF-8
  - 0xF0..0xF7 => 4-byte UTF-8
  - else => invalid

-}
decodeUtf8Byte : Int -> List Char -> Maybe ( Char, List Char )
decodeUtf8Byte b rest =
    if b < 0x80 then
        -- single-byte ASCII
        Just ( Char.fromCode b, rest )

    else if 0xC0 <= b && b <= 0xDF then
        decodeUtf8Multi 1 (b - 0xC0) rest

    else if 0xE0 <= b && b <= 0xEF then
        decodeUtf8Multi 2 (b - 0xE0) rest

    else if 0xF0 <= b && b <= 0xF7 then
        decodeUtf8Multi 3 (b - 0xF0) rest

    else
        -- Not a valid UTF-8 lead byte
        Nothing


{-| Parse `count` continuation bytes (each is `%xx`) from the list,
then combine them with the lead bits from `leadValue`.
-}
decodeUtf8Multi : Int -> Int -> List Char -> Maybe ( Char, List Char )
decodeUtf8Multi count leadValue chars =
    case takeBytes count chars of
        Nothing ->
            Nothing

        Just ( bytes, rest ) ->
            let
                -- If every continuation byte is in 0x80..0xBF, combine them:
                allValid =
                    List.all (\b -> 0x80 <= b && b <= 0xBF) bytes

                codePoint =
                    List.foldl
                        (\b acc -> (acc * 64) + (b - 0x80))
                        leadValue
                        bytes
            in
            if not allValid then
                Nothing

            else
                Just
                    ( Char.fromCode codePoint
                    , rest
                    )


{-| Parse `n` UTF-8 continuation bytes from the list:
each must look like '%xx'. Collect them as Ints.
-}
takeBytes : Int -> List Char -> Maybe ( List Int, List Char )
takeBytes n chars =
    if n <= 0 then
        Just ( [], chars )
        -- no bytes requested

    else
        case chars of
            '%' :: c1 :: c2 :: rest ->
                case parseHex2 c1 c2 of
                    Just b ->
                        case takeBytes (n - 1) rest of
                            Just ( more, leftover ) ->
                                Just ( b :: more, leftover )

                            Nothing ->
                                Nothing

                    Nothing ->
                        Nothing

            _ ->
                -- Not enough chars or not in "%xx" form
                Nothing


{-| Parse two hex digits, e.g. '4' 'F' => 79.
-}
parseHex2 : Char -> Char -> Maybe Int
parseHex2 hi lo =
    case ( fromHex hi, fromHex lo ) of
        ( Just x, Just y ) ->
            Just (x * 16 + y)

        _ ->
            Nothing


{-| Convert a single hex character (0-9,A-F,a-f) to its numeric value 0..15.
-}
fromHex : Char -> Maybe Int
fromHex char =
    case char of
        '0' ->
            Just 0

        '1' ->
            Just 1

        '2' ->
            Just 2

        '3' ->
            Just 3

        '4' ->
            Just 4

        '5' ->
            Just 5

        '6' ->
            Just 6

        '7' ->
            Just 7

        '8' ->
            Just 8

        '9' ->
            Just 9

        'A' ->
            Just 10

        'B' ->
            Just 11

        'C' ->
            Just 12

        'D' ->
            Just 13

        'E' ->
            Just 14

        'F' ->
            Just 15

        'a' ->
            Just 10

        'b' ->
            Just 11

        'c' ->
            Just 12

        'd' ->
            Just 13

        'e' ->
            Just 14

        'f' ->
            Just 15

        _ ->
            Nothing


{-| Class of unreserved characters from <https://datatracker.ietf.org/doc/html/rfc3986#section-2.3>
-}
isUnreserved : Char -> Bool
isUnreserved char =
    case char of
        'A' ->
            True

        'B' ->
            True

        'C' ->
            True

        'D' ->
            True

        'E' ->
            True

        'F' ->
            True

        'G' ->
            True

        'H' ->
            True

        'I' ->
            True

        'J' ->
            True

        'K' ->
            True

        'L' ->
            True

        'M' ->
            True

        'N' ->
            True

        'O' ->
            True

        'P' ->
            True

        'Q' ->
            True

        'R' ->
            True

        'S' ->
            True

        'T' ->
            True

        'U' ->
            True

        'V' ->
            True

        'W' ->
            True

        'X' ->
            True

        'Y' ->
            True

        'Z' ->
            True

        'a' ->
            True

        'b' ->
            True

        'c' ->
            True

        'd' ->
            True

        'e' ->
            True

        'f' ->
            True

        'g' ->
            True

        'h' ->
            True

        'i' ->
            True

        'j' ->
            True

        'k' ->
            True

        'l' ->
            True

        'm' ->
            True

        'n' ->
            True

        'o' ->
            True

        'p' ->
            True

        'q' ->
            True

        'r' ->
            True

        's' ->
            True

        't' ->
            True

        'u' ->
            True

        'v' ->
            True

        'w' ->
            True

        'x' ->
            True

        'y' ->
            True

        'z' ->
            True

        '0' ->
            True

        '1' ->
            True

        '2' ->
            True

        '3' ->
            True

        '4' ->
            True

        '5' ->
            True

        '6' ->
            True

        '7' ->
            True

        '8' ->
            True

        '9' ->
            True

        '-' ->
            True

        '_' ->
            True

        '.' ->
            True

        '~' ->
            True

        _ ->
            False


hexDigitChar : Int -> Char
hexDigitChar n =
    case n of
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
            'A'

        11 ->
            'B'

        12 ->
            'C'

        13 ->
            'D'

        14 ->
            'E'

        15 ->
            'F'

        _ ->
            '0'
