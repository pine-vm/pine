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
    case ( decoderA jsonValue, decoderB jsonValue, decoderC jsonValue, decoderD jsonValue ) of
        ( Ok a, Ok b, Ok c, Ok d ) ->
            Ok (func a b c d)

        ( Err err, _, _, _ ) ->
            Err err

        ( _, Err err, _, _ ) ->
            Err err

        ( _, _, Err err, _ ) ->
            Err err

        ( _, _, _, Err err ) ->
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
    case ( decoderA jsonValue, decoderB jsonValue, decoderC jsonValue, decoderD jsonValue, decoderE jsonValue ) of
        ( Ok a, Ok b, Ok c, Ok d, Ok e ) ->
            Ok (func a b c d e)

        ( Err err, _, _, _, _ ) ->
            Err err

        ( _, Err err, _, _, _ ) ->
            Err err

        ( _, _, Err err, _, _ ) ->
            Err err

        ( _, _, _, Err err, _ ) ->
            Err err

        ( _, _, _, _, Err err ) ->
            Err err


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
    case ( decoderA jsonValue, decoderB jsonValue, decoderC jsonValue, decoderD jsonValue, decoderE jsonValue, decoderF jsonValue ) of
        ( Ok a, Ok b, Ok c, Ok d, Ok e, Ok f ) ->
            Ok (func a b c d e f)

        ( Err err, _, _, _, _, _ ) ->
            Err err

        ( _, Err err, _, _, _, _ ) ->
            Err err

        ( _, _, Err err, _, _, _ ) ->
            Err err

        ( _, _, _, Err err, _, _ ) ->
            Err err

        ( _, _, _, _, Err err, _ ) ->
            Err err

        ( _, _, _, _, _, Err err ) ->
            Err err


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
    case ( decoderA jsonValue, decoderB jsonValue, decoderC jsonValue, decoderD jsonValue, decoderE jsonValue, decoderF jsonValue, decoderG jsonValue ) of
        ( Ok a, Ok b, Ok c, Ok d, Ok e, Ok f, Ok g ) ->
            Ok (func a b c d e f g)

        ( Err err, _, _, _, _, _, _ ) ->
            Err err

        ( _, Err err, _, _, _, _, _ ) ->
            Err err

        ( _, _, Err err, _, _, _, _ ) ->
            Err err

        ( _, _, _, Err err, _, _, _ ) ->
            Err err

        ( _, _, _, _, Err err, _, _ ) ->
            Err err

        ( _, _, _, _, _, Err err, _ ) ->
            Err err

        ( _, _, _, _, _, _, Err err ) ->
            Err err


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
    case ( decoderA jsonValue, decoderB jsonValue, decoderC jsonValue, decoderD jsonValue, decoderE jsonValue, decoderF jsonValue, decoderG jsonValue, decoderH jsonValue ) of
        ( Ok a, Ok b, Ok c, Ok d, Ok e, Ok f, Ok g, Ok h ) ->
            Ok (func a b c d e f g h)

        ( Err err, _, _, _, _, _, _, _ ) ->
            Err err

        ( _, Err err, _, _, _, _, _, _ ) ->
            Err err

        ( _, _, Err err, _, _, _, _, _ ) ->
            Err err

        ( _, _, _, Err err, _, _, _, _ ) ->
            Err err

        ( _, _, _, _, Err err, _, _, _ ) ->
            Err err

        ( _, _, _, _, _, Err err, _, _ ) ->
            Err err

        ( _, _, _, _, _, _, Err err, _ ) ->
            Err err

        ( _, _, _, _, _, _, _, Err err ) ->
            Err err


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
    case parseJsonString jsonString of
        Err parseErr ->
            Err (Failure ("Bad JSON: " ++ parseErr) NullValue)

        Ok jsonValue ->
            decodeValue decoder jsonValue


parseJsonString : String -> Result String Value
parseJsonString jsonString =
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
    case str of
        [] ->
            ( Err "Unexpected end of input while parsing string", 0 )

        nextChar :: following ->
            case nextChar of
                '"' ->
                    ( Ok [], 1 )

                _ ->
                    parseString following
                        |> Tuple.mapFirst (Result.map ((::) nextChar))
                        |> Tuple.mapSecond ((+) 1)


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
    Char.toCode char <= 32
