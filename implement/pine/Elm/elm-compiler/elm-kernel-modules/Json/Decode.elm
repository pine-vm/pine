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

import Array
import Json.Encode exposing (Value(..))
import String


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


array : Decoder a -> Decoder (Array.Array a)
array decoder jsonValue =
    case jsonValue of
        ArrayValue values ->
            case decodeListRecursively [] decoder values of
                Ok list ->
                    Ok (Array.fromList list)

                Err err ->
                    Err err

        _ ->
            Err (temporaryStubErrorNotImplemented jsonValue)


decodeListRecursively : List a -> Decoder a -> List Value -> Result Error (List a)
decodeListRecursively result decoder values =
    case values of
        [] ->
            Ok (List.reverse result)

        itemValue :: rest ->
            case decoder itemValue of
                Ok a ->
                    decodeListRecursively (a :: result) decoder rest

                Err err ->
                    Err err


field : String -> Decoder a -> Decoder a
field key decoder jsonValue =
    case jsonValue of
        ObjectValue fields ->
            case List.filter (\( k, _ ) -> k == key) fields of
                [ ( _, fieldValue ) ] ->
                    decoder fieldValue

                _ ->
                    Err (Failure ("Expecting an object with a field named `" ++ key ++ "`") jsonValue)

        _ ->
            Err (temporaryStubErrorNotImplemented jsonValue)


float : Decoder Float
float jsonValue =
    case jsonValue of
        FloatValue floatAsString ->
            case String.toFloat floatAsString of
                Just floatVal ->
                    Ok floatVal

                Nothing ->
                    Err (Failure ("Expecting a float, but got: " ++ floatAsString) jsonValue)

        IntValue intVal ->
            Ok (toFloat intVal)

        _ ->
            Err (temporaryStubErrorNotImplemented jsonValue)


index : Int -> Decoder a -> Decoder a
index targetIndex decoder jsonValue =
    case jsonValue of
        ArrayValue values ->
            case List.drop targetIndex values of
                value :: _ ->
                    decoder value

                [] ->
                    Err
                        (Failure
                            ("Expecting an array with at least "
                                ++ String.fromInt (targetIndex + 1)
                                ++ " entries"
                            )
                            jsonValue
                        )

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
    case parseValue jsonString 0 of
        ( Ok ok, consumed ) ->
            let
                afterWhitespaceOffset : Int
                afterWhitespaceOffset =
                    skipWhitespace jsonString consumed
            in
            case getCharAt jsonString afterWhitespaceOffset of
                Just c ->
                    Err
                        ("Unexpected character at end of JSON, at offset "
                            ++ String.fromInt afterWhitespaceOffset
                            ++ ": '"
                            ++ String.fromChar c
                            ++ "'"
                        )

                Nothing ->
                    Ok ok

        ( Err err, consumed ) ->
            Err ("Error at character " ++ String.fromInt consumed ++ ": " ++ err)


type alias Parser a =
    String -> Int -> ( Result String a, Int )


-- Helper function to get character at a specific offset in a String
getCharAt : String -> Int -> Maybe Char
getCharAt (String.String stringBlob) offset =
    let
        charBytes =
            Pine_kernel.take
                [ 4
                , Pine_kernel.skip [ offset * 4, stringBlob ]
                ]
    in
    if Pine_kernel.equal [ Pine_kernel.length charBytes, 0 ] then
        Nothing
    else
        Just charBytes


-- Helper function to check if we have characters remaining
hasCharAt : String -> Int -> Bool
hasCharAt (String.String stringBlob) offset =
    let
        charBytes =
            Pine_kernel.take
                [ 4
                , Pine_kernel.skip [ offset * 4, stringBlob ]
                ]
    in
    Pine_kernel.equal [ Pine_kernel.length charBytes, 4 ]


-- Helper function to check if string matches expected text starting at offset
matchesTextAt : String -> Int -> String -> Bool
matchesTextAt (String.String stringBlob) offset (String.String expectedBlob) =
    let
        expectedLength = Pine_kernel.length expectedBlob
        actualSlice =
            Pine_kernel.take
                [ expectedLength
                , Pine_kernel.skip [ offset * 4, stringBlob ]
                ]
    in
    Pine_kernel.equal [ actualSlice, expectedBlob ]


parseValue : String -> Int -> ( Result String Value, Int )
parseValue src offset0 =
    let
        -- First, skip any whitespace from offset0 forward
        offset1 : Int
        offset1 =
            skipWhitespace src offset0

        -- Peek at the next character
        nextChar =
            getCharAt src offset1
    in
    case nextChar of
        Just c ->
            case c of
                'n' ->
                    -- Attempt to parse `null`
                    parseNull src offset1

                't' ->
                    -- Attempt to parse `true`
                    parseTrue src offset1

                'f' ->
                    -- Attempt to parse `false`
                    parseFalse src offset1

                '"' ->
                    -- Parse a JSON string (the leading quote is consumed here)
                    case parseString src (offset1 + 1) of
                        ( Ok str, offset2 ) ->
                            ( Ok (StringValue (String.fromList str)), offset2 )

                        ( Err err, offset2 ) ->
                            ( Err err, offset2 )

                '[' ->
                    -- Parse an array (leading '[' is already consumed).
                    case parseArray src (offset1 + 1) of
                        ( Ok values, offset2 ) ->
                            ( Ok (ArrayValue values), offset2 )

                        ( Err err, offset2 ) ->
                            ( Err err, offset2 )

                '{' ->
                    -- Parse an object (leading '{' is already consumed).
                    case parseObjectRec [] src (offset1 + 1) of
                        ( Ok fields, offset2 ) ->
                            ( Ok (ObjectValue fields), offset2 )

                        ( Err err, offset2 ) ->
                            ( Err err, offset2 )

                '-' ->
                    -- Could be a negative number
                    parseNumber src offset1

                _ ->
                    if Char.isDigit c then
                        -- It’s some digit, so parse a (positive) number
                        parseNumber src offset1

                    else
                        ( Err ("Unexpected character while parsing value: '" ++ String.fromChar c ++ "'")
                        , offset1
                        )

        Nothing ->
            -- We ran out of input
            ( Err "Unexpected end of input while parsing value", offset1 )


parseNull : String -> Int -> ( Result String Value, Int )
parseNull src offset0 =
    if matchesTextAt src offset0 "null" then
        ( Ok NullValue, offset0 + 4 )
    else
        ( Err "Expecting 'null'", offset0 )


parseTrue : String -> Int -> ( Result String Value, Int )
parseTrue src offset0 =
    if matchesTextAt src offset0 "true" then
        ( Ok (BoolValue True), offset0 + 4 )
    else
        ( Err "Expecting 'true'", offset0 )


parseFalse : String -> Int -> ( Result String Value, Int )
parseFalse src offset0 =
    if matchesTextAt src offset0 "false" then
        ( Ok (BoolValue False), offset0 + 5 )
    else
        ( Err "Expecting 'false'", offset0 )


parseString : Parser (List Char)
parseString str offset =
    parseJsonString str offset []


parseNumber : String -> Int -> ( Result String Value, Int )
parseNumber src offset0 =
    -- We need to parse a number, which could be an integer or a float.
    -- We'll start by parsing an integer, then look for a decimal point.
    let
        ( intResult, offset1 ) =
            parseInt src offset0

        -- <--- assumes you have parseInt (Int) returning (Result String Int, Int)
    in
    case intResult of
        Ok intVal ->
            -- If we successfully parsed an integer, look for a decimal point
            let
                nextChar =
                    getCharAt src offset1
            in
            case nextChar of
                Just '.' ->
                    -- If we see a decimal point, parse the fractional part
                    let
                        ( denominatorResult, offset2 ) =
                            parseUnsignedInt src (offset1 + 1)
                    in
                    case denominatorResult of
                        Ok _ ->
                            -- For now we just take the string as is
                            ( Ok
                                (FloatValue
                                    (String.slice offset0 offset2 src)
                                )
                            , offset2
                            )

                        Err err ->
                            ( Err err, offset2 )

                _ ->
                    -- If no decimal point, we're done: it's an integer
                    ( Ok (IntValue intVal), offset1 )

        Err err ->
            ( Err err, offset1 )


{-|

    parseArray parses a JSON array starting at `offset` in the given `src` (a List Char).

    Examples:

        src = String.toList "[true, false, null]"
        parseArray src 0
        --> ( Ok [ BoolValue True, BoolValue False, NullValue ], finalOffset )

-}
parseArray : List Char -> Int -> ( Result String (List Value), Int )
parseArray src offset0 =
    -- First, skip any whitespace
    let
        offset1 =
            skipWhitespace src offset0

        -- Look at the next character
        nextChar =
            List.take 1 (List.drop offset1 src)
    in
    case nextChar of
        [] ->
            -- We ran out of characters entirely
            ( Err "Unexpected end of input while parsing array", offset1 )

        -- If it's a ']', that means an empty array: "[]"
        [ ']' ] ->
            ( Ok [], offset1 + 1 )

        -- Otherwise, parse one or more items
        _ ->
            parseArrayItems [] src offset1


{-|

    parseArrayItems accumulates items in `acc` until we see a ']' or run out.
    Returns (Ok items, newOffset) or (Err msg, newOffset).

-}
parseArrayItems : List Value -> List Char -> Int -> ( Result String (List Value), Int )
parseArrayItems itemsBefore src offset0 =
    -- First parse a single Value
    let
        ( valResult, offsetAfterVal ) =
            parseValue src offset0

        -- <--- assumes you have parseValue (Value) returning (Result String Value, Int)
    in
    case valResult of
        Err msg ->
            -- If the item fails, we bubble up the error
            ( Err msg, offsetAfterVal )

        Ok val ->
            -- We successfully parsed one item: accumulate it, then look for comma or closing bracket
            let
                offset1 =
                    skipWhitespace src offsetAfterVal

                nextChar =
                    List.take 1 (List.drop offset1 src)

                items : List Value
                items =
                    Pine_kernel.concat [ itemsBefore, [ val ] ]
            in
            case nextChar of
                [ ',' ] ->
                    -- If we see a comma, skip it and parse the next item
                    parseArrayItems items src (offset1 + 1)

                [ ']' ] ->
                    -- End of array
                    ( Ok items, offset1 + 1 )

                [ c ] ->
                    ( Err ("Expecting ',' or ']', got '" ++ String.fromChar c ++ "'"), offset1 )

                _ ->
                    -- We ran out unexpectedly, missing a ']' or another item
                    ( Err "Unclosed array, expected ',' or ']'", offset1 )


parseObjectRec : List ( String, Value ) -> List Char -> Int -> ( Result String (List ( String, Value )), Int )
parseObjectRec fieldsBefore src offset0 =
    -- First, skip any whitespace
    let
        offset1 : Int
        offset1 =
            skipWhitespace src offset0

        -- Look at the next character
        nextChar =
            List.take 1 (List.drop offset1 src)
    in
    case nextChar of
        -- If it's a '}', that means an empty object: "{}"
        [ '}' ] ->
            ( Ok fieldsBefore, offset1 + 1 )

        -- Otherwise, parse one or more key-value pairs
        [ '"' ] ->
            let
                ( keyResult, offsetAfterKey ) =
                    parseString src (offset1 + 1)

                -- <--- assumes you have parseString (List Char) returning (Result String (List Char), Int)
            in
            case keyResult of
                Err msg ->
                    -- If the key fails, we bubble up the error
                    ( Err msg, offsetAfterKey )

                Ok keyChars ->
                    -- We successfully parsed one key: accumulate it, then look for colon and value
                    let
                        keyString : String
                        keyString =
                            String.fromList keyChars

                        offset2 : Int
                        offset2 =
                            skipWhitespace src offsetAfterKey

                        nextChar2 =
                            List.take 1 (List.drop offset2 src)
                    in
                    case nextChar2 of
                        [ ':' ] ->
                            -- If we see a colon, skip it and parse the value
                            let
                                offset3 =
                                    skipWhitespace src (offset2 + 1)

                                ( valResult, offsetAfterVal ) =
                                    parseValue src offset3
                            in
                            case valResult of
                                Err err ->
                                    -- If the value fails, we bubble up the error
                                    ( Err ("Error parsing object value: " ++ err), offsetAfterVal )

                                Ok val ->
                                    -- We successfully parsed one value: accumulate it, then look for comma or closing brace
                                    let
                                        offset4 =
                                            skipWhitespace src offsetAfterVal

                                        nextChar3 =
                                            List.take 1 (List.drop offset4 src)

                                        fields : List ( String, Value )
                                        fields =
                                            Pine_kernel.concat [ fieldsBefore, [ ( keyString, val ) ] ]
                                    in
                                    case nextChar3 of
                                        [ ',' ] ->
                                            -- If we see a comma, skip it and parse the next item
                                            parseObjectRec fields src (offset4 + 1)

                                        [ '}' ] ->
                                            -- End of object
                                            ( Ok fields, offset4 + 1 )

                                        [ c ] ->
                                            ( Err ("Expecting ',' or '}', got '" ++ String.fromChar c ++ "'"), offset4 )

                                        _ ->
                                            -- We ran out unexpectedly, missing a '}' or another item
                                            ( Err "Unclosed object, expected ',' or '}'", offset4 )

                        _ ->
                            ( Err ("Expecting ':' after object key '" ++ keyString ++ "'"), offset2 )

        [ c ] ->
            ( Err ("Expecting '\"' to start object key, got '" ++ String.fromChar c ++ "'"), offset1 )

        _ ->
            -- We ran out of characters entirely
            ( Err "Unexpected end of input while parsing object", offset1 )


errorToString : Error -> String
errorToString err =
    case err of
        Field key subError ->
            "Field `" ++ key ++ "`: " ++ errorToString subError

        Index offset subError ->
            "Index " ++ String.fromInt offset ++ ": " ++ errorToString subError

        OneOf errors ->
            "One of the following errors occurred:\n\n"
                ++ String.join "\n\n" (List.map errorToString errors)

        Failure message failValue ->
            message ++ "\n\n" ++ Json.Encode.encode 4 failValue


{-| Parse a JSON string from a `List Char`, starting at a given `offset`,
accumulating into `soFar`. Returns either:

  - `Ok (parsedString, newIndex)`
  - `Err message`

Example usage:

    parseJsonString (String.toList "\"hello\\nworld\" trailing stuff") 0 []
    --> Ok ("hello\nworld", 13)

-}
parseJsonString : List Char -> Int -> List Char -> ( Result String (List Char), Int )
parseJsonString source offset soFar =
    case List.take 1 (List.drop offset source) of
        [ c ] ->
            case c of
                -- End of the JSON string if we encounter "
                '"' ->
                    ( Ok soFar, offset + 1 )

                -- Check for backslash escape
                '\\' ->
                    parseEscape source offset soFar

                -- Otherwise, accumulate this character and keep going
                _ ->
                    parseJsonString source (offset + 1) (List.concat [ soFar, [ c ] ])

        _ ->
            -- We ran out of characters before finding a closing quote
            ( Err "Unexpected end of input while reading JSON string", offset )



-- HELPERS


{-| Handle a backslash-escape character.
We consume the `\` at position `offset`, so we look at `(offset + 1)` for the next char.
-}
parseEscape : List Char -> Int -> List Char -> ( Result String (List Char), Int )
parseEscape source offset soFar =
    -- We already know source !! offset == '\\'
    case List.take 1 (List.drop (offset + 1) source) of
        [ e ] ->
            case e of
                -- Standard JSON escapes
                'n' ->
                    -- Append newline and continue
                    parseJsonString source (offset + 2) (soFar ++ [ '\n' ])

                'r' ->
                    parseJsonString source (offset + 2) (soFar ++ [ '\u{000D}' ])

                't' ->
                    parseJsonString source (offset + 2) (soFar ++ [ '\t' ])

                '"' ->
                    parseJsonString source (offset + 2) (soFar ++ [ '"' ])

                '\\' ->
                    parseJsonString source (offset + 2) (soFar ++ [ '\\' ])

                '/' ->
                    parseJsonString source (offset + 2) (soFar ++ [ '/' ])

                'b' ->
                    -- Typically backspace is ASCII 8, but some folks map it differently.
                    -- For now, let's do ASCII 8 (BS).
                    parseJsonString source (offset + 2) (soFar ++ [ Char.fromCode 8 ])

                'f' ->
                    -- Typically form feed is ASCII 12.
                    parseJsonString source (offset + 2) (soFar ++ [ Char.fromCode 12 ])

                'u' ->
                    -- JSON allows \uXXXX (4 hex digits)
                    parseUnicodeEscape source (offset + 2) soFar

                -- Unrecognized escape
                _ ->
                    ( Err ("Unrecognized escape sequence: \\" ++ String.fromChar e)
                    , offset + 2
                    )

        _ ->
            -- No character after the backslash
            ( Err "Unexpected end of input after backslash in string escape"
            , offset + 1
            )


{-| Parse a JSON Unicode escape of the form "\\uXXXX" where XXXX are 4 hex digits.
If it is a high surrogate in [0xD800..0xDBFF], look for a following "\\uXXXX"
as a low surrogate in [0xDC00..0xDFFF]. If both are found, combine them into
a single codepoint (e.g. "\\uD83C\\uDF32" --> 🌲).
-}
parseUnicodeEscape : List Char -> Int -> List Char -> ( Result String (List Char), Int )
parseUnicodeEscape source offset soFar =
    let
        -- First, parse the 4 hex digits after the "\u"
        fourHexChars : List Char
        fourHexChars =
            List.take 4 (List.drop offset source)

        ( codeUnit, offsetAfterHex ) =
            convert1OrMoreHexadecimal 0 fourHexChars

        hi : Int
        hi =
            codeUnit
    in
    if offsetAfterHex /= 4 then
        -- We did not get 4 valid hex digits
        ( Err "Unexpected end of input in \\u escape (need 4 hex digits)"
        , offset + 6
        )

    else
    -- We have a potential code unit, see if it's a high surrogate
    if
        0xD800 <= hi && hi <= 0xDBFF
    then
        -- Possibly part of a surrogate pair; check the next 2 chars for "\u"
        case List.take 2 (List.drop (offset + 4) source) of
            [ '\\', 'u' ] ->
                -- Parse the next 4 hex digits (the low surrogate)
                let
                    fourHexChars2 : List Char
                    fourHexChars2 =
                        List.take 4 (List.drop (offset + 4 + 2) source)

                    ( codeUnit2, offsetAfterHex2 ) =
                        convert1OrMoreHexadecimal 0 fourHexChars2

                    lo : Int
                    lo =
                        codeUnit2

                    offset2 : Int
                    offset2 =
                        offsetAfterHex2
                in
                if offset2 /= 4 then
                    -- The next \u did not have 4 valid hex digits
                    ( Err "Unexpected end of input in second \\u of a surrogate pair"
                    , offset + 4 + 2 + 6
                    )

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
                        (offset + 4 + 2 + 4)
                        (soFar ++ [ Char.fromCode fullCodePoint ])

                else
                    -- We found "\u" but it’s not in the low surrogate range.
                    -- Option A: treat `hi` as a normal code unit; ignore the extra "\u"
                    -- Option B: throw an error.
                    -- This example will just treat the high code as a normal char:
                    parseJsonString
                        source
                        (offset + 4)
                        (soFar ++ [ Char.fromCode hi ])

            _ ->
                -- No second "\u"—so decode `hi` as-is.
                parseJsonString
                    source
                    (offset + 4)
                    (soFar ++ [ Char.fromCode hi ])

    else
        -- Not a high surrogate, just parse `\uXXXX` as a single character
        parseJsonString
            source
            (offset + 4)
            (soFar ++ [ Char.fromCode hi ])


parseInt : String -> Int -> ( Result String Int, Int )
parseInt src offset0 =
    let
        nextChar =
            getCharAt src offset0
    in
    case nextChar of
        Just '-' ->
            -- If we see a minus sign, parse the rest as an unsigned integer
            let
                ( unsignedResult, offset1 ) =
                    parseUnsignedInt src (offset0 + 1)
            in
            case unsignedResult of
                Ok unsignedVal ->
                    ( Ok -unsignedVal, offset1 )

                Err err ->
                    ( Err err, offset1 )

        _ ->
            -- If no minus sign, parse the rest as an unsigned integer
            parseUnsignedInt src offset0


parseUnsignedInt : String -> Int -> ( Result String Int, Int )
parseUnsignedInt src offset0 =
    case getCharAt src offset0 of
        Just '0' ->
            ( Ok 0, offset0 + 1 )

        Just '1' ->
            parseUnsignedIntRec 1 src (offset0 + 1)

        Just '2' ->
            parseUnsignedIntRec 2 src (offset0 + 1)

        Just '3' ->
            parseUnsignedIntRec 3 src (offset0 + 1)

        Just '4' ->
            parseUnsignedIntRec 4 src (offset0 + 1)

        Just '5' ->
            parseUnsignedIntRec 5 src (offset0 + 1)

        Just '6' ->
            parseUnsignedIntRec 6 src (offset0 + 1)

        Just '7' ->
            parseUnsignedIntRec 7 src (offset0 + 1)

        Just '8' ->
            parseUnsignedIntRec 8 src (offset0 + 1)

        Just '9' ->
            parseUnsignedIntRec 9 src (offset0 + 1)

        _ ->
            ( Err "Expecting a digit", offset0 )


parseUnsignedIntRec : Int -> String -> Int -> ( Result String Int, Int )
parseUnsignedIntRec upper src offset0 =
    case getCharAt src offset0 of
        Just '0' ->
            parseUnsignedIntRec (upper * 10) src (offset0 + 1)

        Just '1' ->
            parseUnsignedIntRec (upper * 10 + 1) src (offset0 + 1)

        Just '2' ->
            parseUnsignedIntRec (upper * 10 + 2) src (offset0 + 1)

        Just '3' ->
            parseUnsignedIntRec (upper * 10 + 3) src (offset0 + 1)

        Just '4' ->
            parseUnsignedIntRec (upper * 10 + 4) src (offset0 + 1)

        Just '5' ->
            parseUnsignedIntRec (upper * 10 + 5) src (offset0 + 1)

        Just '6' ->
            parseUnsignedIntRec (upper * 10 + 6) src (offset0 + 1)

        Just '7' ->
            parseUnsignedIntRec (upper * 10 + 7) src (offset0 + 1)

        Just '8' ->
            parseUnsignedIntRec (upper * 10 + 8) src (offset0 + 1)

        Just '9' ->
            parseUnsignedIntRec (upper * 10 + 9) src (offset0 + 1)

        _ ->
            ( Ok upper, offset0 )


convert1OrMoreHexadecimal : Int -> List Char -> ( Int, Int )
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
            ( 0, -1 )


convert0OrMoreHexadecimal : Int -> Int -> List Char -> ( Int, Int )
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
            ( soFar, offset )


skipWhitespace : String -> Int -> Int
skipWhitespace str offset =
    case getCharAt str offset of
        Just ' ' ->
            skipWhitespace str (offset + 1)

        Just '\t' ->
            skipWhitespace str (offset + 1)

        Just '\n' ->
            skipWhitespace str (offset + 1)

        Just '\u{000D}' ->
            skipWhitespace str (offset + 1)

        _ ->
            offset
