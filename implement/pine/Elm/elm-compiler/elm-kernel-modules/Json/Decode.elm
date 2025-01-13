module Json.Decode exposing
    ( Decoder
    , Error(..)
    , Value
    , andThen
    , array
    , at
    , bool
    , decodeString
    , decodeValue
    , dict
    , errorToString
    , fail
    , field
    , float
    , index
    , int
    , keyValuePairs
    , lazy
    , list
    , map
    , map2
    , map3
    , map4
    , map5
    , map6
    , map7
    , map8
    , maybe
    , null
    , nullable
    , oneOf
    , oneOrMore
    , string
    , succeed
    , value
    )

import Json.Encode exposing (Value(..))


{-| A structured error describing exactly how the decoder failed. You can use
this to create more elaborate visualizations of a decoder problem. For example,
you could show the entire JSON object and show the part causing the failure in
red.
-}
type Error
    = Field String Error
    | Index Int Error
    | OneOf (List Error)
    | Failure String Value


type alias Decoder a =
    Value -> Result Error a


temporaryStubErrorNotImplemented : Value -> Error
temporaryStubErrorNotImplemented =
    Failure "Json.Decode error not implemented yet"


decodeValue : Decoder a -> Value -> Result Error a
decodeValue decoder jsonValue =
    decoder jsonValue


{-| Do not do anything with a JSON value, just bring it into Elm as a `Value`.
This can be useful if you have particularly complex data that you would like to
deal with later. Or if you are going to send it out a port and do not care
about its structure.
-}
value : Decoder Value
value =
    Ok


{-| Decode a `null` value into some Elm value.

    decodeString (null False) "null" == Ok False
    decodeString (null 42) "null"    == Ok 42
    decodeString (null 42) "42"      == Err ..
    decodeString (null 42) "false"   == Err ..

So if you ever see a `null`, this will return whatever value you specified.

-}
null : a -> Decoder a
null ok jsonValue =
    if jsonValue == NullValue then
        Ok ok

    else
        Err (temporaryStubErrorNotImplemented jsonValue)


bool : Decoder Bool
bool jsonValue =
    case jsonValue of
        BoolValue boolVal ->
            Ok boolVal

        _ ->
            Err (temporaryStubErrorNotImplemented jsonValue)


int : Decoder Int
int jsonValue =
    case jsonValue of
        IntValue intVal ->
            Ok intVal

        _ ->
            Err (temporaryStubErrorNotImplemented jsonValue)


string : Decoder String
string jsonValue =
    case jsonValue of
        StringValue stringVal ->
            Ok stringVal

        _ ->
            Err (temporaryStubErrorNotImplemented jsonValue)


list : Decoder a -> Decoder (List a)
list decoder jsonValue =
    case jsonValue of
        ArrayValue values ->
            decodeListRecursively [] decoder values

        _ ->
            Err (temporaryStubErrorNotImplemented jsonValue)


decodeListRecursively : List a -> Decoder a -> List Value -> Result Error (List a)
decodeListRecursively result decoder values =
    case values of
        [] ->
            Ok (List.reverse result)

        value :: rest ->
            case decoder value of
                Ok a ->
                    decodeListRecursively (a :: result) decoder rest

                Err err ->
                    Err err


field : String -> Decoder a -> Decoder a
field key decoder jsonValue =
    case jsonValue of
        ObjectValue fields ->
            case List.filter (\( k, _ ) -> k == key) fields of
                [ ( _, value ) ] ->
                    decoder value

                _ ->
                    Err (Failure ("Expecting an object with a field named `" ++ key ++ "`") jsonValue)

        _ ->
            Err (temporaryStubErrorNotImplemented jsonValue)


map : (a -> b) -> Decoder a -> Decoder b
map func decoder jsonValue =
    case decoder jsonValue of
        Ok a ->
            Ok (func a)

        Err err ->
            Err err


map2 :
    (a -> b -> value)
    -> Decoder a
    -> Decoder b
    -> Decoder value
map2 func decoderA decoderB jsonValue =
    case ( decoderA jsonValue, decoderB jsonValue ) of
        ( Ok a, Ok b ) ->
            Ok (func a b)

        ( Err err, _ ) ->
            Err err

        ( _, Err err ) ->
            Err err


map3 :
    (a -> b -> c -> value)
    -> Decoder a
    -> Decoder b
    -> Decoder c
    -> Decoder value
map3 func decoderA decoderB decoderC jsonValue =
    case ( decoderA jsonValue, decoderB jsonValue, decoderC jsonValue ) of
        ( Ok a, Ok b, Ok c ) ->
            Ok (func a b c)

        ( Err err, _, _ ) ->
            Err err

        ( _, Err err, _ ) ->
            Err err

        ( _, _, Err err ) ->
            Err err


map4 :
    (a -> b -> c -> d -> value)
    -> Decoder a
    -> Decoder b
    -> Decoder c
    -> Decoder d
    -> Decoder value
map4 func decoderA decoderB decoderC decoderD jsonValue =
    case ( ( decoderA jsonValue, decoderB jsonValue ), ( decoderC jsonValue, decoderD jsonValue ) ) of
        ( ( Ok a, Ok b ), ( Ok c, Ok d ) ) ->
            Ok (func a b c d)

        ( ( Err err, _ ), _ ) ->
            Err err

        ( ( _, Err err ), _ ) ->
            Err err

        ( _, ( Err err, _ ) ) ->
            Err err

        ( _, ( _, Err err ) ) ->
            Err err


map5 :
    (a -> b -> c -> d -> e -> value)
    -> Decoder a
    -> Decoder b
    -> Decoder c
    -> Decoder d
    -> Decoder e
    -> Decoder value
map5 func decoderA decoderB decoderC decoderD decoderE jsonValue =
    case decoderA jsonValue of
        Err err ->
            Err err

        Ok a ->
            case decoderB jsonValue of
                Err err ->
                    Err err

                Ok b ->
                    case decoderC jsonValue of
                        Err err ->
                            Err err

                        Ok c ->
                            case decoderD jsonValue of
                                Err err ->
                                    Err err

                                Ok d ->
                                    case decoderE jsonValue of
                                        Err err ->
                                            Err err

                                        Ok e ->
                                            Ok (func a b c d e)


map6 :
    (a -> b -> c -> d -> e -> f -> value)
    -> Decoder a
    -> Decoder b
    -> Decoder c
    -> Decoder d
    -> Decoder e
    -> Decoder f
    -> Decoder value
map6 func decoderA decoderB decoderC decoderD decoderE decoderF jsonValue =
    case decoderA jsonValue of
        Err err ->
            Err err

        Ok a ->
            case decoderB jsonValue of
                Err err ->
                    Err err

                Ok b ->
                    case decoderC jsonValue of
                        Err err ->
                            Err err

                        Ok c ->
                            case decoderD jsonValue of
                                Err err ->
                                    Err err

                                Ok d ->
                                    case decoderE jsonValue of
                                        Err err ->
                                            Err err

                                        Ok e ->
                                            case decoderF jsonValue of
                                                Err err ->
                                                    Err err

                                                Ok f ->
                                                    Ok (func a b c d e f)


map7 :
    (a -> b -> c -> d -> e -> f -> g -> value)
    -> Decoder a
    -> Decoder b
    -> Decoder c
    -> Decoder d
    -> Decoder e
    -> Decoder f
    -> Decoder g
    -> Decoder value
map7 func decoderA decoderB decoderC decoderD decoderE decoderF decoderG jsonValue =
    case decoderA jsonValue of
        Err err ->
            Err err

        Ok a ->
            case decoderB jsonValue of
                Err err ->
                    Err err

                Ok b ->
                    case decoderC jsonValue of
                        Err err ->
                            Err err

                        Ok c ->
                            case decoderD jsonValue of
                                Err err ->
                                    Err err

                                Ok d ->
                                    case decoderE jsonValue of
                                        Err err ->
                                            Err err

                                        Ok e ->
                                            case decoderF jsonValue of
                                                Err err ->
                                                    Err err

                                                Ok f ->
                                                    case decoderG jsonValue of
                                                        Err err ->
                                                            Err err

                                                        Ok g ->
                                                            Ok (func a b c d e f g)


map8 :
    (a -> b -> c -> d -> e -> f -> g -> h -> value)
    -> Decoder a
    -> Decoder b
    -> Decoder c
    -> Decoder d
    -> Decoder e
    -> Decoder f
    -> Decoder g
    -> Decoder h
    -> Decoder value
map8 func decoderA decoderB decoderC decoderD decoderE decoderF decoderG decoderH jsonValue =
    case decoderA jsonValue of
        Err err ->
            Err err

        Ok a ->
            case decoderB jsonValue of
                Err err ->
                    Err err

                Ok b ->
                    case decoderC jsonValue of
                        Err err ->
                            Err err

                        Ok c ->
                            case decoderD jsonValue of
                                Err err ->
                                    Err err

                                Ok d ->
                                    case decoderE jsonValue of
                                        Err err ->
                                            Err err

                                        Ok e ->
                                            case decoderF jsonValue of
                                                Err err ->
                                                    Err err

                                                Ok f ->
                                                    case decoderG jsonValue of
                                                        Err err ->
                                                            Err err

                                                        Ok g ->
                                                            case decoderH jsonValue of
                                                                Err err ->
                                                                    Err err

                                                                Ok h ->
                                                                    Ok (func a b c d e f g h)


andThen : (a -> Decoder b) -> Decoder a -> Decoder b
andThen callback decoder jsonValue =
    case decoder jsonValue of
        Ok a ->
            callback a jsonValue

        Err err ->
            Err err


oneOf : List (Decoder a) -> Decoder a
oneOf decoders jsonValue =
    case decoders of
        [] ->
            Err (Failure "oneOf used with an empty list of decoders" jsonValue)

        _ ->
            oneOfRecursively [] decoders jsonValue


oneOfRecursively : List Error -> List (Decoder a) -> Value -> Result Error a
oneOfRecursively errors decoders jsonValue =
    case decoders of
        [] ->
            Err (OneOf errors)

        decoder :: rest ->
            case decoder jsonValue of
                Ok a ->
                    Ok a

                Err err ->
                    oneOfRecursively (err :: errors) rest jsonValue


nullable : Decoder a -> Decoder (Maybe a)
nullable decoder jsonValue =
    case jsonValue of
        NullValue ->
            Ok Nothing

        _ ->
            case decoder jsonValue of
                Ok a ->
                    Ok (Just a)

                Err err ->
                    Err err


lazy : (() -> Decoder a) -> Decoder a
lazy callback jsonValue =
    callback () jsonValue


{-| Ignore the JSON and produce a certain Elm value.

    decodeString (succeed 42) "true"    == Ok 42
    decodeString (succeed 42) "[1,2,3]" == Ok 42
    decodeString (succeed 42) "hello"   == Err ... -- this is not a valid JSON string

This is handy when used with `oneOf` or `andThen`.

-}
succeed : a -> Decoder a
succeed success _ =
    Ok success


{-| Ignore the JSON and make the decoder fail. This is handy when used with
`oneOf` or `andThen` where you want to give a custom error message in some
case.

See the [`andThen`](#andThen) docs for an example.

-}
fail : String -> Decoder a
fail error jsonValue =
    Err (Failure error jsonValue)


{-| Parse the given string into a JSON value and then run the `Decoder` on it.
This will fail if the string is not well-formed JSON or if the `Decoder`
fails for some reason.

    decodeString int "4"     == Ok 4
    decodeString int "1 + 2" == Err ...

-}
decodeString : Decoder a -> String -> Result Error a
decodeString decoder jsonString =
    case parseJsonStringToValue jsonString of
        Err parseErr ->
            Err (Failure ("Bad JSON: " ++ parseErr) NullValue)

        Ok jsonValue ->
            decodeValue decoder jsonValue


parseJsonStringToValue : String -> Result String Value
parseJsonStringToValue jsonString =
    case parseValue (String.toList (String.trim jsonString)) of
        ( Ok ok, consumed ) ->
            if consumed < String.length jsonString then
                Err ("Unexpected string at the end: " ++ String.dropLeft consumed jsonString)

            else
                Ok ok

        ( Err err, _ ) ->
            Err err


type alias Parser a =
    List Char -> ( Result String a, Int )


parseValue : Parser Value
parseValue str =
    if str == [] then
        ( Err "Unexpected end of input", 0 )

    else if listStartsWith [ 'n', 'u', 'l', 'l' ] str then
        ( Ok NullValue, 4 )

    else if listStartsWith [ 't', 'r', 'u', 'e' ] str then
        ( Ok (BoolValue True), 4 )

    else if listStartsWith [ 'f', 'a', 'l', 's', 'e' ] str then
        ( Ok (BoolValue False), 5 )

    else if listStartsWith [ '"' ] str then
        parseString (List.drop 1 str)
            |> Tuple.mapFirst (Result.map (String.fromList >> StringValue))
            |> Tuple.mapSecond ((+) 1)

    else if listStartsWith [ '[' ] str then
        parseArray (List.drop 1 str)
            |> Tuple.mapFirst (Result.map ArrayValue)
            |> Tuple.mapSecond ((+) 1)

    else if listStartsWith [ '{' ] str then
        parseObject (List.drop 1 str)
            |> Tuple.mapFirst (Result.map ObjectValue)
            |> Tuple.mapSecond ((+) 1)

    else
        parseInt str
            |> Tuple.mapFirst (Result.map IntValue)


parseString : Parser (List Char)
parseString str =
    -- We call parseJsonString with initial index = 0, collecting chars in [].
    case parseJsonString str 0 [] of
        Err msg ->
            -- If parseJsonString fails, match the old return shape: (Err msg, 0)
            ( Err msg, 0 )

        Ok ( parsedString, usedCount ) ->
            -- If it succeeds, we convert the parsedString into a list of chars
            -- and return the number of characters used up.
            ( Ok (String.toList parsedString), usedCount )


parseInt : Parser Int
parseInt str =
    case str of
        '-' :: rest ->
            let
                ( result, offset ) =
                    parseUnsignedInt rest
            in
            case result of
                Ok intAbsolute ->
                    ( Ok -intAbsolute
                    , offset + 1
                    )

                Err err ->
                    ( Err err
                    , offset + 1
                    )

        _ ->
            parseUnsignedInt str


parseUnsignedInt : Parser Int
parseUnsignedInt str =
    parseUnsignedIntHelp ( 0, Nothing ) str


parseUnsignedIntHelp : ( Int, Maybe Int ) -> Parser Int
parseUnsignedIntHelp previousDigits str =
    let
        ( previousDigitsCount, maybePreviusDigitsValue ) =
            previousDigits

        digitValueFromCharacter char =
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

                _ ->
                    Nothing

        complete =
            ( Result.fromMaybe "No digits found" maybePreviusDigitsValue
            , previousDigitsCount
            )
    in
    case str of
        [] ->
            complete

        currentChar :: followingChars ->
            case digitValueFromCharacter currentChar of
                Just digitValue ->
                    parseUnsignedIntHelp
                        ( previousDigitsCount + 1
                        , Just (Maybe.withDefault 0 maybePreviusDigitsValue * 10 + digitValue)
                        )
                        followingChars

                Nothing ->
                    complete


countCharsWhile : (Char -> Bool) -> List Char -> Int
countCharsWhile predicate str =
    case str of
        [] ->
            0

        char :: rest ->
            if predicate char then
                countCharsWhile predicate rest + 1

            else
                0


parseArray : Parser (List Value)
parseArray str =
    parseArrayHelper [] str
        |> Tuple.mapFirst (Result.map List.reverse)


parseArrayHelper : List Value -> Parser (List Value)
parseArrayHelper previousItems str =
    let
        strTrimmed =
            dropWhileList isCharRemovedOnTrim str

        trimmedLength =
            List.length str - List.length strTrimmed
    in
    case strTrimmed of
        [] ->
            ( Err "Unexpected end of input while parsing array"
            , 0
            )

        ']' :: _ ->
            ( Ok previousItems
            , trimmedLength + 1
            )

        _ ->
            parseArrayHelperTrimmedNotEmpty previousItems strTrimmed
                |> Tuple.mapSecond ((+) trimmedLength)


parseArrayHelperTrimmedNotEmpty : List Value -> Parser (List Value)
parseArrayHelperTrimmedNotEmpty previousItems strTrimmed =
    let
        ( valueResult, itemLength ) =
            parseValue strTrimmed
    in
    case valueResult of
        Err err ->
            ( Err err, itemLength )

        Ok itemValue ->
            let
                strAfterItem =
                    List.drop itemLength strTrimmed

                strAfterValueTrimmed =
                    dropWhileList isCharRemovedOnTrim strAfterItem

                trimmedLength =
                    List.length strAfterItem - List.length strAfterValueTrimmed
            in
            case strAfterValueTrimmed of
                ',' :: afterComma ->
                    parseArrayHelper (itemValue :: previousItems) afterComma
                        |> Tuple.mapSecond ((+) (itemLength + trimmedLength + 1))

                _ ->
                    parseArrayHelper (itemValue :: previousItems) strAfterValueTrimmed
                        |> Tuple.mapSecond ((+) (itemLength + trimmedLength))


parseObject : Parser (List ( String, Value ))
parseObject str =
    -- Object parsing logic goes here
    -- Similar to `parseArray`, but also needs to handle the colon (:) separating keys and values.
    parseObjectHelper [] str
        |> Tuple.mapFirst (Result.map List.reverse)


parseObjectHelper : List ( String, Value ) -> Parser (List ( String, Value ))
parseObjectHelper previousItems str =
    let
        strTrimmed =
            dropWhileList isCharRemovedOnTrim str

        trimmedLength =
            List.length str - List.length strTrimmed
    in
    case strTrimmed of
        [] ->
            ( Err "Unexpected end of input while parsing object"
            , 0
            )

        '}' :: rest ->
            ( Ok previousItems
            , trimmedLength + 1
            )

        _ ->
            parseObjectHelperTrimmedNotEmpty previousItems strTrimmed
                |> Tuple.mapSecond ((+) trimmedLength)


parseObjectHelperTrimmedNotEmpty : List ( String, Value ) -> Parser (List ( String, Value ))
parseObjectHelperTrimmedNotEmpty previousItems strTrimmed =
    case strTrimmed of
        [] ->
            ( Err "Unexpected end of input while parsing object", 0 )

        '"' :: rest ->
            -- Parse the key string
            let
                ( keyResult, keyConsumed ) =
                    parseString rest
            in
            case keyResult of
                Err err ->
                    ( Err ("Error parsing object key: " ++ err), keyConsumed + 1 )

                Ok keyChars ->
                    let
                        keyString =
                            String.fromList keyChars

                        totalKeyConsumed =
                            keyConsumed + 1

                        -- plus 1 for the starting '"'
                        afterKey =
                            List.drop totalKeyConsumed strTrimmed

                        afterKeyTrimmed =
                            dropWhileList isCharRemovedOnTrim afterKey

                        trimmedLength =
                            List.length afterKey - List.length afterKeyTrimmed

                        totalConsumedKey =
                            totalKeyConsumed + trimmedLength
                    in
                    case afterKeyTrimmed of
                        ':' :: afterColon ->
                            let
                                afterColonTrimmed =
                                    dropWhileList isCharRemovedOnTrim afterColon

                                trimmedAfterColonLength =
                                    List.length afterColon - List.length afterColonTrimmed

                                totalConsumedSoFar =
                                    totalConsumedKey + 1 + trimmedAfterColonLength

                                ( valueResult, valueConsumed ) =
                                    parseValue afterColonTrimmed
                            in
                            case valueResult of
                                Err err ->
                                    ( Err ("Error parsing object value: " ++ err), totalConsumedSoFar + valueConsumed )

                                Ok value ->
                                    let
                                        afterValue =
                                            List.drop valueConsumed afterColonTrimmed

                                        afterValueTrimmed =
                                            dropWhileList isCharRemovedOnTrim afterValue

                                        trimmedAfterValueLength =
                                            List.length afterValue - List.length afterValueTrimmed

                                        totalConsumed =
                                            totalConsumedSoFar + valueConsumed + trimmedAfterValueLength
                                    in
                                    case afterValueTrimmed of
                                        ',' :: restAfterComma ->
                                            parseObjectHelper (( keyString, value ) :: previousItems) restAfterComma
                                                |> Tuple.mapSecond ((+) (totalConsumed + 1))

                                        '}' :: restAfterBrace ->
                                            ( Ok (( keyString, value ) :: previousItems), totalConsumed + 1 )

                                        [] ->
                                            ( Err "Unexpected end of input while parsing object", totalConsumed )

                                        _ ->
                                            ( Err "Expected ',' or '}' after object value", totalConsumed )

                        _ ->
                            ( Err "Expected ':' after object key", totalConsumedKey )

        _ ->
            ( Err "Expected '" "' to start object key", 0 )


listStartsWith : List a -> List a -> Bool
listStartsWith prefix list =
    List.take (List.length prefix) list == prefix


dropWhileList : (Char -> Bool) -> List Char -> List Char
dropWhileList predicate stringList =
    case stringList of
        [] ->
            []

        char :: rest ->
            if predicate char then
                dropWhileList predicate rest

            else
                stringList


isCharRemovedOnTrim : Char -> Bool
isCharRemovedOnTrim char =
    if Pine_kernel.equal [ char, ' ' ] then
        True

    else if Pine_kernel.equal [ char, '\t' ] then
        True

    else if Pine_kernel.equal [ char, '\n' ] then
        True

    else if Pine_kernel.equal [ char, '\u{000D}' ] then
        True

    else
        False


errorToString : Error -> String
errorToString err =
    case err of
        Field key subError ->
            "Field `" ++ key ++ "`: " ++ errorToString subError

        Index index subError ->
            "Index " ++ String.fromInt index ++ ": " ++ errorToString subError

        OneOf errors ->
            "One of the following errors occurred:\n\n"
                ++ String.join "\n\n" (List.map errorToString errors)

        Failure message failValue ->
            message ++ "\n\n" ++ Json.Encode.encode 4 failValue


{-| Parse a JSON string from a `List Char`, starting at a given `index`,
accumulating into `soFar`. Returns either:

  - `Ok (parsedString, newIndex)`
  - `Err message`

Example usage:

    parseJsonString (String.toList "\"hello\\nworld\" trailing stuff") 0 []
    --> Ok ("hello\nworld", 13)

-}
parseJsonString : List Char -> Int -> List Char -> Result String ( String, Int )
parseJsonString source index soFar =
    case List.take 1 (List.drop index source) of
        [ c ] ->
            case c of
                -- End of the JSON string if we encounter "
                '"' ->
                    -- Convert collected chars in `soFar` to a String
                    Ok ( String.fromList soFar, index + 1 )

                -- Check for backslash escape
                '\\' ->
                    parseEscape source index soFar

                -- Otherwise, accumulate this character and keep going
                _ ->
                    parseJsonString source (index + 1) (List.concat [ soFar, [ c ] ])

        _ ->
            -- We ran out of characters before finding a closing quote
            Err "Unexpected end of input while reading JSON string"



-- HELPERS


{-| Handle a backslash-escape character.
We consume the `\` at position `index`, so we look at `(index + 1)` for the next char.
-}
parseEscape : List Char -> Int -> List Char -> Result String ( String, Int )
parseEscape source index soFar =
    -- We already know source !! index == '\\'
    case List.take 1 (List.drop (index + 1) source) of
        [ e ] ->
            case e of
                -- Standard JSON escapes
                'n' ->
                    -- Append newline and continue
                    parseJsonString source (index + 2) (soFar ++ [ '\n' ])

                'r' ->
                    parseJsonString source (index + 2) (soFar ++ [ '\u{000D}' ])

                't' ->
                    parseJsonString source (index + 2) (soFar ++ [ '\t' ])

                '"' ->
                    parseJsonString source (index + 2) (soFar ++ [ '"' ])

                '\\' ->
                    parseJsonString source (index + 2) (soFar ++ [ '\\' ])

                '/' ->
                    parseJsonString source (index + 2) (soFar ++ [ '/' ])

                'b' ->
                    -- Typically backspace is ASCII 8, but some folks map it differently.
                    -- For now, let's do ASCII 8 (BS).
                    parseJsonString source (index + 2) (soFar ++ [ Char.fromCode 8 ])

                'f' ->
                    -- Typically form feed is ASCII 12.
                    parseJsonString source (index + 2) (soFar ++ [ Char.fromCode 12 ])

                'u' ->
                    -- JSON allows \uXXXX (4 hex digits)
                    parseUnicodeEscape source (index + 2) soFar

                -- Unrecognized escape
                _ ->
                    Err ("Unrecognized escape sequence: \\" ++ String.fromChar e)

        _ ->
            -- No character after the backslash
            Err "Unexpected end of input after backslash in string escape"


{-| Parse a JSON Unicode escape of the form "\\uXXXX" where XXXX are 4 hex digits.
If it is a high surrogate in [0xD800..0xDBFF], look for a following "\\uXXXX"
as a low surrogate in [0xDC00..0xDFFF]. If both are found, combine them into
a single codepoint (e.g. "\\uD83C\\uDF32" --> 🌲).
-}
parseUnicodeEscape : List Char -> Int -> List Char -> Result String ( String, Int )
parseUnicodeEscape source index soFar =
    let
        -- First, parse the 4 hex digits after the "\u"
        fourHexChars : List Char
        fourHexChars =
            List.take 4 (List.drop index source)

        parseHexResult =
            convert1OrMoreHexadecimal 0 fourHexChars

        hi : Int
        hi =
            parseHexResult.int

        offset : Int
        offset =
            parseHexResult.offset
    in
    if offset /= 4 then
        -- We did not get 4 valid hex digits
        Err "Unexpected end of input in \\u escape (need 4 hex digits)"

    else
    -- We have a potential code unit, see if it's a high surrogate
    if
        0xD800 <= hi && hi <= 0xDBFF
    then
        -- Possibly part of a surrogate pair; check the next 2 chars for "\u"
        case List.take 2 (List.drop (index + 4) source) of
            [ '\\', 'u' ] ->
                -- Parse the next 4 hex digits (the low surrogate)
                let
                    fourHexChars2 : List Char
                    fourHexChars2 =
                        List.take 4 (List.drop (index + 4 + 2) source)

                    parseHexResult2 =
                        convert1OrMoreHexadecimal 0 fourHexChars2

                    lo : Int
                    lo =
                        parseHexResult2.int

                    offset2 : Int
                    offset2 =
                        parseHexResult2.offset
                in
                if offset2 /= 4 then
                    -- The next \u did not have 4 valid hex digits
                    Err "Unexpected end of input in second \\u of a surrogate pair"

                else if 0xDC00 <= lo && lo <= 0xDFFF then
                    -- Combine into a single code point
                    let
                        fullCodePoint =
                            0x00010000
                                + ((hi - 0xD800) * 0x0400)
                                + (lo - 0xDC00)
                    in
                    parseJsonString
                        source
                        -- We used 4 hex digits + 2 extra chars "\u" + 4 more digits
                        (index + 4 + 2 + 4)
                        (soFar ++ [ Char.fromCode fullCodePoint ])

                else
                    -- We found "\u" but it’s not in the low surrogate range.
                    -- Option A: treat `hi` as a normal code unit; ignore the extra "\u"
                    -- Option B: throw an error.
                    -- This example will just treat the high code as a normal char:
                    parseJsonString
                        source
                        (index + 4)
                        (soFar ++ [ Char.fromCode hi ])

            _ ->
                -- No second "\u"—so decode `hi` as-is.
                parseJsonString
                    source
                    (index + 4)
                    (soFar ++ [ Char.fromCode hi ])

    else
        -- Not a high surrogate, just parse `\uXXXX` as a single character
        parseJsonString
            source
            (index + 4)
            (soFar ++ [ Char.fromCode hi ])


convert1OrMoreHexadecimal : Int -> List Char -> { int : Int, offset : Int }
convert1OrMoreHexadecimal offset src =
    case List.take 1 (List.drop offset src) of
        [ '0' ] ->
            convert0OrMoreHexadecimal 0 (offset + 1) src

        [ '1' ] ->
            convert0OrMoreHexadecimal 1 (offset + 1) src

        [ '2' ] ->
            convert0OrMoreHexadecimal 2 (offset + 1) src

        [ '3' ] ->
            convert0OrMoreHexadecimal 3 (offset + 1) src

        [ '4' ] ->
            convert0OrMoreHexadecimal 4 (offset + 1) src

        [ '5' ] ->
            convert0OrMoreHexadecimal 5 (offset + 1) src

        [ '6' ] ->
            convert0OrMoreHexadecimal 6 (offset + 1) src

        [ '7' ] ->
            convert0OrMoreHexadecimal 7 (offset + 1) src

        [ '8' ] ->
            convert0OrMoreHexadecimal 8 (offset + 1) src

        [ '9' ] ->
            convert0OrMoreHexadecimal 9 (offset + 1) src

        [ 'a' ] ->
            convert0OrMoreHexadecimal 10 (offset + 1) src

        [ 'A' ] ->
            convert0OrMoreHexadecimal 10 (offset + 1) src

        [ 'b' ] ->
            convert0OrMoreHexadecimal 11 (offset + 1) src

        [ 'B' ] ->
            convert0OrMoreHexadecimal 11 (offset + 1) src

        [ 'c' ] ->
            convert0OrMoreHexadecimal 12 (offset + 1) src

        [ 'C' ] ->
            convert0OrMoreHexadecimal 12 (offset + 1) src

        [ 'd' ] ->
            convert0OrMoreHexadecimal 13 (offset + 1) src

        [ 'D' ] ->
            convert0OrMoreHexadecimal 13 (offset + 1) src

        [ 'e' ] ->
            convert0OrMoreHexadecimal 14 (offset + 1) src

        [ 'E' ] ->
            convert0OrMoreHexadecimal 14 (offset + 1) src

        [ 'f' ] ->
            convert0OrMoreHexadecimal 15 (offset + 1) src

        [ 'F' ] ->
            convert0OrMoreHexadecimal 15 (offset + 1) src

        _ ->
            { int = 0, offset = -1 }


convert0OrMoreHexadecimal : Int -> Int -> List Char -> { int : Int, offset : Int }
convert0OrMoreHexadecimal soFar offset src =
    case List.take 1 (List.drop offset src) of
        [ '0' ] ->
            convert0OrMoreHexadecimal (soFar * 16) (offset + 1) src

        [ '1' ] ->
            convert0OrMoreHexadecimal (soFar * 16 + 1) (offset + 1) src

        [ '2' ] ->
            convert0OrMoreHexadecimal (soFar * 16 + 2) (offset + 1) src

        [ '3' ] ->
            convert0OrMoreHexadecimal (soFar * 16 + 3) (offset + 1) src

        [ '4' ] ->
            convert0OrMoreHexadecimal (soFar * 16 + 4) (offset + 1) src

        [ '5' ] ->
            convert0OrMoreHexadecimal (soFar * 16 + 5) (offset + 1) src

        [ '6' ] ->
            convert0OrMoreHexadecimal (soFar * 16 + 6) (offset + 1) src

        [ '7' ] ->
            convert0OrMoreHexadecimal (soFar * 16 + 7) (offset + 1) src

        [ '8' ] ->
            convert0OrMoreHexadecimal (soFar * 16 + 8) (offset + 1) src

        [ '9' ] ->
            convert0OrMoreHexadecimal (soFar * 16 + 9) (offset + 1) src

        [ 'a' ] ->
            convert0OrMoreHexadecimal (soFar * 16 + 10) (offset + 1) src

        [ 'A' ] ->
            convert0OrMoreHexadecimal (soFar * 16 + 10) (offset + 1) src

        [ 'b' ] ->
            convert0OrMoreHexadecimal (soFar * 16 + 11) (offset + 1) src

        [ 'B' ] ->
            convert0OrMoreHexadecimal (soFar * 16 + 11) (offset + 1) src

        [ 'c' ] ->
            convert0OrMoreHexadecimal (soFar * 16 + 12) (offset + 1) src

        [ 'C' ] ->
            convert0OrMoreHexadecimal (soFar * 16 + 12) (offset + 1) src

        [ 'd' ] ->
            convert0OrMoreHexadecimal (soFar * 16 + 13) (offset + 1) src

        [ 'D' ] ->
            convert0OrMoreHexadecimal (soFar * 16 + 13) (offset + 1) src

        [ 'e' ] ->
            convert0OrMoreHexadecimal (soFar * 16 + 14) (offset + 1) src

        [ 'E' ] ->
            convert0OrMoreHexadecimal (soFar * 16 + 14) (offset + 1) src

        [ 'f' ] ->
            convert0OrMoreHexadecimal (soFar * 16 + 15) (offset + 1) src

        [ 'F' ] ->
            convert0OrMoreHexadecimal (soFar * 16 + 15) (offset + 1) src

        _ ->
            { int = soFar, offset = offset }
