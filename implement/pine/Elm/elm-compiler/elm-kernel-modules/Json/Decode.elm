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


type String
    = String Int
      -- We need another tag to prevent the compiler from assuming that the condition for tag 'String' is always true.
    | AnyOtherKind_String


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
parseJsonStringToValue (String jsonStringBytes) =
    case parseValue jsonStringBytes 0 of
        ( Ok ok, consumedBytes ) ->
            let
                afterTrimOffsetBytes : Int
                afterTrimOffsetBytes =
                    skipWhitespace
                        jsonStringBytes
                        consumedBytes
            in
            if
                Pine_kernel.equal
                    [ afterTrimOffsetBytes
                    , Pine_kernel.length jsonStringBytes
                    ]
            then
                -- We successfully parsed the entire JSON string
                Ok ok

            else
                let
                    followingChar =
                        Pine_kernel.take
                            [ 4
                            , Pine_kernel.skip [ afterTrimOffsetBytes, jsonStringBytes ]
                            ]
                in
                -- There are still characters left after parsing, which is unexpected
                let
                    afterTrimOffsetChars : Int
                    afterTrimOffsetChars =
                        Pine_kernel.concat
                            [ Pine_kernel.take [ 1, 0 ]
                            , Pine_kernel.bit_shift_right [ 2, Pine_kernel.skip [ 1, afterTrimOffsetBytes ] ]
                            ]
                in
                Err
                    ("Unexpected character at end of JSON, at offset "
                        ++ String.fromInt afterTrimOffsetChars
                        ++ ": '"
                        ++ String.fromChar followingChar
                        ++ "'"
                    )

        ( Err err, consumed ) ->
            Err ("Error at character " ++ String.fromInt consumed ++ ": " ++ err)


type alias Parser a =
    List Char -> Int -> ( Result String a, Int )


parseValue : Int -> Int -> ( Result String Value, Int )
parseValue srcBytes offset0 =
    let
        -- First, skip any whitespace from offset0 forward
        offset1 : Int
        offset1 =
            skipWhitespace srcBytes offset0

        -- Peek at the next character
        nextChar =
            Pine_kernel.take [ 4, Pine_kernel.skip [ offset1, srcBytes ] ]
    in
    if Pine_kernel.equal [ Pine_kernel.length nextChar, 0 ] then
        -- We ran out of input
        ( Err "Unexpected end of input while parsing value", offset1 )

    else
        -- We have at least one character to work with
        case nextChar of
            'n' ->
                -- Attempt to parse `null`
                parseNull srcBytes offset1

            't' ->
                -- Attempt to parse `true`
                parseTrue srcBytes offset1

            'f' ->
                -- Attempt to parse `false`
                parseFalse srcBytes offset1

            '"' ->
                -- Parse a JSON string (the leading quote is consumed here)
                case parseJsonStringLiteral srcBytes (offset1 + 4) of
                    ( Ok str, offset2 ) ->
                        ( Ok (StringValue str), offset2 )

                    ( Err err, offset2 ) ->
                        ( Err err, offset2 )

            '[' ->
                -- Parse an array (leading '[' is already consumed).
                case parseArray srcBytes (offset1 + 4) of
                    ( Ok values, offset2 ) ->
                        ( Ok (ArrayValue values), offset2 )

                    ( Err err, offset2 ) ->
                        ( Err err, offset2 )

            '{' ->
                -- Parse an object (leading '{' is already consumed).
                case parseObjectRec [] srcBytes (offset1 + 4) of
                    ( Ok fields, offset2 ) ->
                        ( Ok (ObjectValue fields), offset2 )

                    ( Err err, offset2 ) ->
                        ( Err err, offset2 )

            '-' ->
                -- Could be a negative number
                parseNumber srcBytes offset1

            _ ->
                if Char.isDigit nextChar then
                    -- It’s some digit, so parse a (positive) number
                    parseNumber srcBytes offset1

                else
                    ( Err ("Unexpected character while parsing value: '" ++ String.fromChar nextChar ++ "'")
                    , offset1
                    )


parseNull : Int -> Int -> ( Result String Value, Int )
parseNull srcBytes offset0 =
    let
        -- We expect the next 4 characters to be "null"
        nextChars =
            Pine_kernel.take [ 16, Pine_kernel.skip [ offset0, srcBytes ] ]
    in
    if Pine_kernel.equal [ nextChars, Pine_kernel.concat [ 'n', 'u', 'l', 'l' ] ] then
        ( Ok NullValue, offset0 + 16 )

    else
        ( Err "Expecting 'null'", offset0 )


parseTrue : Int -> Int -> ( Result String Value, Int )
parseTrue srcBytes offset0 =
    let
        -- We expect the next 4 characters to be "true"
        nextChars =
            Pine_kernel.take [ 16, Pine_kernel.skip [ offset0, srcBytes ] ]
    in
    if Pine_kernel.equal [ nextChars, Pine_kernel.concat [ 't', 'r', 'u', 'e' ] ] then
        ( Ok (BoolValue True), offset0 + 16 )

    else
        ( Err "Expecting 'true'", offset0 )


parseFalse : Int -> Int -> ( Result String Value, Int )
parseFalse srcBytes offset0 =
    let
        -- We expect the next 5 characters to be "false"
        nextChars =
            Pine_kernel.take [ 20, Pine_kernel.skip [ offset0, srcBytes ] ]
    in
    if Pine_kernel.equal [ nextChars, Pine_kernel.concat [ 'f', 'a', 'l', 's', 'e' ] ] then
        ( Ok (BoolValue False), offset0 + 20 )

    else
        ( Err "Expecting 'false'", offset0 )


parseNumber : Int -> Int -> ( Result String Value, Int )
parseNumber srcBytes offset0 =
    -- We need to parse a number, which could be an integer or a float.
    -- We'll start by parsing an integer, then look for a decimal point.
    case parseInt srcBytes offset0 of
        ( Ok intVal, offset1 ) ->
            -- If we successfully parsed an integer, look for a decimal point
            let
                nextChar =
                    Pine_kernel.take
                        [ 4
                        , Pine_kernel.skip [ offset1, srcBytes ]
                        ]
            in
            if Pine_kernel.equal [ nextChar, '.' ] then
                -- If we see a decimal point, parse the fractional part
                let
                    ( denominatorResult, offset2 ) =
                        parseUnsignedInt
                            srcBytes
                            (Pine_kernel.int_add [ offset1, 4 ])
                in
                case denominatorResult of
                    Ok _ ->
                        -- For now we just take the string as is
                        let
                            sliceLength : Int
                            sliceLength =
                                Pine_kernel.int_add
                                    [ offset2
                                    , Pine_kernel.int_mul [ offset0, -1 ]
                                    ]

                            sliceBytes =
                                Pine_kernel.take
                                    [ sliceLength
                                    , Pine_kernel.skip [ offset0, srcBytes ]
                                    ]
                        in
                        ( Ok (FloatValue (String sliceBytes))
                        , offset2
                        )

                    Err err ->
                        ( Err err, offset2 )

            else
                -- If no decimal point, we're done: it's an integer
                ( Ok (IntValue intVal), offset1 )

        ( Err err, offset1 ) ->
            ( Err err, offset1 )


{-|

    parseArray parses a JSON array starting at `offset` in the given `src` (a List Char).

    Examples:

        src = String.toList "[true, false, null]"
        parseArray src 0
        --> ( Ok [ BoolValue True, BoolValue False, NullValue ], finalOffset )

-}
parseArray : Int -> Int -> ( Result String (List Value), Int )
parseArray srcBytes offset0 =
    -- First, skip any whitespace
    let
        offset1 =
            skipWhitespace srcBytes offset0

        -- Look at the next character
        nextChar =
            Pine_kernel.take [ 4, Pine_kernel.skip [ offset1, srcBytes ] ]
    in
    if Pine_kernel.equal [ Pine_kernel.length nextChar, 0 ] then
        -- We ran out of characters entirely
        ( Err "Unexpected end of input while parsing array", offset1 )

    else if Pine_kernel.equal [ nextChar, ']' ] then
        -- If it's a ']', that means an empty array: "[]"
        ( Ok [], offset1 + 4 )

    else
        -- Otherwise, parse one or more items
        parseArrayItems [] srcBytes offset1


parseArrayItems : List Value -> Int -> Int -> ( Result String (List Value), Int )
parseArrayItems itemsBefore srcBytes offset0 =
    let
        ( valResult, offsetAfterVal ) =
            parseValue srcBytes offset0
    in
    case valResult of
        Err msg ->
            -- If the item fails, we bubble up the error
            ( Err msg, offsetAfterVal )

        Ok val ->
            -- We successfully parsed one item: accumulate it, then look for comma or closing bracket
            let
                offset1 =
                    skipWhitespace srcBytes offsetAfterVal

                nextChar =
                    Pine_kernel.take [ 4, Pine_kernel.skip [ offset1, srcBytes ] ]

                items : List Value
                items =
                    Pine_kernel.concat [ itemsBefore, [ val ] ]
            in
            if Pine_kernel.equal [ Pine_kernel.length nextChar, 0 ] then
                -- We ran out unexpectedly, missing a ']' or another item
                ( Err "Unclosed array, expected ',' or ']'", offset1 )

            else
                case nextChar of
                    ',' ->
                        -- If we see a comma, skip it and parse the next item
                        parseArrayItems items srcBytes (offset1 + 4)

                    ']' ->
                        -- End of array
                        ( Ok items, offset1 + 4 )

                    _ ->
                        ( Err ("Expecting ',' or ']', got '" ++ String.fromChar nextChar ++ "'"), offset1 )


parseObjectRec : List ( String, Value ) -> Int -> Int -> ( Result String (List ( String, Value )), Int )
parseObjectRec fieldsBefore srcBytes offset0 =
    -- First, skip any whitespace
    let
        offset1 : Int
        offset1 =
            skipWhitespace srcBytes offset0

        -- Look at the next character
        nextChar =
            Pine_kernel.take [ 4, Pine_kernel.skip [ offset1, srcBytes ] ]
    in
    if Pine_kernel.equal [ Pine_kernel.length nextChar, 0 ] then
        -- We ran out of characters entirely
        ( Err "Unexpected end of input while parsing object", offset1 )

    else
        case nextChar of
            -- If it's a '}', that means an empty object: "{}"
            '}' ->
                ( Ok fieldsBefore, offset1 + 4 )

            -- Otherwise, parse one or more key-value pairs
            '"' ->
                let
                    ( keyResult, offsetAfterKey ) =
                        parseJsonStringLiteral srcBytes (offset1 + 4)
                in
                case keyResult of
                    Err msg ->
                        -- If the key fails, we bubble up the error
                        ( Err msg, offsetAfterKey )

                    Ok keyString ->
                        -- We successfully parsed one key: accumulate it, then look for colon and value
                        let
                            offset2 : Int
                            offset2 =
                                skipWhitespace srcBytes offsetAfterKey

                            nextChar2 =
                                Pine_kernel.take [ 4, Pine_kernel.skip [ offset2, srcBytes ] ]
                        in
                        case nextChar2 of
                            ':' ->
                                -- If we see a colon, skip it and parse the value
                                let
                                    offset3 =
                                        skipWhitespace srcBytes (offset2 + 4)

                                    ( valResult, offsetAfterVal ) =
                                        parseValue srcBytes offset3
                                in
                                case valResult of
                                    Err err ->
                                        -- If the value fails, we bubble up the error
                                        ( Err ("Error parsing object value: " ++ err), offsetAfterVal )

                                    Ok val ->
                                        -- We successfully parsed one value: accumulate it, then look for comma or closing brace
                                        let
                                            offset4 =
                                                skipWhitespace srcBytes offsetAfterVal

                                            nextChar3 =
                                                Pine_kernel.take [ 4, Pine_kernel.skip [ offset4, srcBytes ] ]

                                            fields : List ( String, Value )
                                            fields =
                                                Pine_kernel.concat [ fieldsBefore, [ ( keyString, val ) ] ]
                                        in
                                        if Pine_kernel.equal [ Pine_kernel.length nextChar3, 0 ] then
                                            -- We ran out of characters before finding a comma or closing brace
                                            ( Err "Unexpected end of input while reading JSON object", offset4 )

                                        else
                                            case nextChar3 of
                                                ',' ->
                                                    -- If we see a comma, skip it and parse the next item
                                                    parseObjectRec fields srcBytes (offset4 + 4)

                                                '}' ->
                                                    -- End of object
                                                    ( Ok fields, offset4 + 4 )

                                                _ ->
                                                    ( Err ("Expecting ',' or '}', got '" ++ String.fromChar nextChar3 ++ "'"), offset4 )

                            _ ->
                                ( Err ("Expecting ':' after object key '" ++ keyString ++ "'"), offset2 )

            _ ->
                ( Err ("Expecting '\"' to start object key, got '" ++ String.fromChar nextChar ++ "'"), offset1 )


errorToString : Error -> String
errorToString err =
    case err of
        Field key subError ->
            "Field `" ++ key ++ "`: " ++ errorToString subError

        Index offset subError ->
            "Index " ++ String.fromInt offset ++ ": " ++ errorToString subError

        OneOf errors ->
            "One of the following errors occurred:\\n\\n"
                ++ String.join "\n\n" (List.map errorToString errors)

        Failure message failValue ->
            message ++ "\\n\\n" ++ Json.Encode.encode 4 failValue


parseJsonStringLiteral : Int -> Int -> ( Result String String, Int )
parseJsonStringLiteral sourceBytes offset =
    let
        ( result, newOffset ) =
            parseJsonStringSegments sourceBytes offset []
    in
    case result of
        Ok segments ->
            let
                allStringBytes =
                    Pine_kernel.concat segments
            in
            ( Ok (String allStringBytes), newOffset )

        Err message ->
            ( Err message, newOffset )


parseJsonStringSegments : Int -> Int -> List Int -> ( Result String (List Int), Int )
parseJsonStringSegments sourceBytes offset slicesSoFar =
    let
        simpleSegmentEndOffset =
            parseJsonStringSimpleChars sourceBytes offset

        simpleSegmentSliceLength =
            simpleSegmentEndOffset - offset

        simpleSegmentSlice =
            Pine_kernel.take
                [ simpleSegmentSliceLength
                , Pine_kernel.skip [ offset, sourceBytes ]
                ]

        nextChar =
            Pine_kernel.take
                [ 4
                , Pine_kernel.skip [ simpleSegmentEndOffset, sourceBytes ]
                ]
    in
    if Pine_kernel.equal [ nextChar, '"' ] then
        ( Ok (Pine_kernel.concat [ slicesSoFar, [ simpleSegmentSlice ] ])
        , Pine_kernel.int_add [ simpleSegmentEndOffset, 4 ]
        )

    else if Pine_kernel.equal [ nextChar, '\\' ] then
        -- We have a backslash escape sequence
        case parseEscape sourceBytes simpleSegmentEndOffset of
            ( Ok escapedChar, newOffset ) ->
                parseJsonStringSegments
                    sourceBytes
                    newOffset
                    (Pine_kernel.concat [ slicesSoFar, [ simpleSegmentSlice, escapedChar ] ])

            ( Err message, newOffset ) ->
                ( Err message
                , newOffset
                )

    else
        ( Err "Unexpected end of input while reading JSON string", simpleSegmentEndOffset )


{-| Advance the pointer up to the next char that needs special treatment, like a quote or start of an escape sequence
-}
parseJsonStringSimpleChars : Int -> Int -> Int
parseJsonStringSimpleChars sourceBytes offset =
    let
        nextChar =
            Pine_kernel.take
                [ 4
                , Pine_kernel.skip [ offset, sourceBytes ]
                ]
    in
    if Pine_kernel.equal [ Pine_kernel.length nextChar, 0 ] then
        -- We ran out of characters before finding a closing quote or escape
        offset

    else if Pine_kernel.equal [ nextChar, '"' ] then
        -- Found a quote or backslash escape, stop here
        offset

    else if Pine_kernel.equal [ nextChar, '\\' ] then
        -- Found a backslash escape, stop here
        offset

    else
        -- Keep going to the next character
        parseJsonStringSimpleChars
            sourceBytes
            (Pine_kernel.int_add [ offset, 4 ])


parseEscape : Int -> Int -> ( Result String Int, Int )
parseEscape sourceBytes offset =
    -- We already know source !! offset == '\\'
    let
        nextChar =
            Pine_kernel.take
                [ 4
                , Pine_kernel.skip [ offset + 4, sourceBytes ]
                ]
    in
    case nextChar of
        -- Standard JSON escapes
        'n' ->
            -- Append newline and continue
            ( Ok '\n', offset + 8 )

        'r' ->
            ( Ok '\u{000D}', offset + 8 )

        't' ->
            ( Ok '\t', offset + 8 )

        '"' ->
            ( Ok '"', offset + 8 )

        '\\' ->
            ( Ok '\\', offset + 8 )

        '/' ->
            ( Ok '/', offset + 8 )

        'b' ->
            -- Typically backspace is ASCII 8, but some folks map it differently.
            -- For now, let's do ASCII 8 (BS).
            ( Ok (Char.fromCode 8), offset + 8 )

        'f' ->
            -- Typically form feed is ASCII 12.
            ( Ok (Char.fromCode 12), offset + 8 )

        'u' ->
            -- JSON allows \\uXXXX (4 hex digits)
            parseUnicodeEscape
                sourceBytes
                (offset + 8)

        -- Unrecognized escape
        _ ->
            ( Err ("Unrecognized escape sequence: " ++ String.fromChar nextChar)
            , offset + 8
            )


{-| Parse a JSON Unicode escape of the form "\\\\uXXXX" where XXXX are 4 hex digits.
If it is a high surrogate in [0xD800..0xDBFF], look for a following "\\\\uXXXX"
as a low surrogate in [0xDC00..0xDFFF]. If both are found, combine them into
a single codepoint (e.g. "\\\\uD83C\\\\uDF32" --> 🌲).
-}
parseUnicodeEscape : Int -> Int -> ( Result String Int, Int )
parseUnicodeEscape sourceBytes offset =
    let
        fourHexChars =
            Pine_kernel.take
                [ 16
                , Pine_kernel.skip [ offset, sourceBytes ]
                ]

        ( codeUnit, offsetAfterHex ) =
            convert1OrMoreHexadecimal 0 fourHexChars

        hi : Int
        hi =
            codeUnit

        followingTwoChars =
            Pine_kernel.take
                [ 8
                , Pine_kernel.skip [ offset + 16, sourceBytes ]
                ]
    in
    if Pine_kernel.equal [ offsetAfterHex, 16 ] then
        -- We have a potential code unit, see if it's a high surrogate
        if Pine_kernel.int_is_sorted_asc [ 0xD800, hi, 0xDBFF ] then
            -- Possibly part of a surrogate pair; check the next 2 chars for "\u"
            if Pine_kernel.equal [ followingTwoChars, Pine_kernel.concat [ '\\', 'u' ] ] then
                -- Parse the next 4 hex digits (the low surrogate)
                let
                    fourHexChars2 =
                        Pine_kernel.take
                            [ 16
                            , Pine_kernel.skip [ offset + 24, sourceBytes ]
                            ]

                    ( codeUnit2, offsetAfterHex2 ) =
                        convert1OrMoreHexadecimal 0 fourHexChars2

                    lo : Int
                    lo =
                        codeUnit2

                    offset2 : Int
                    offset2 =
                        offsetAfterHex2
                in
                if Pine_kernel.equal [ offset2, 16 ] then
                    if Pine_kernel.int_is_sorted_asc [ 0xDC00, lo, 0xDFFF ] then
                        -- Combine into a single code point
                        let
                            fullCodePoint =
                                0x00010000
                                    + ((hi - 0xD800) * 0x0400)
                                    + (lo - 0xDC00)
                        in
                        ( Ok (Char.fromCode fullCodePoint)
                        , offset + 16 + 8 + 16
                        )

                    else
                        -- We found "\\u" but it’s not in the low surrogate range.
                        -- Option A: treat `hi` as a normal code unit; ignore the extra "\\u"
                        -- Option B: throw an error.
                        -- This example will just treat the high code as a normal char:
                        ( Ok (Char.fromCode hi)
                        , offset + 16
                        )

                else
                    -- The next \u did not have 4 valid hex digits
                    ( Err "Unexpected end of input in second \\u of a surrogate pair"
                    , offset + 16 + 8 + 24
                    )

            else
                -- No second "\u"—so decode `hi` as-is.
                ( Ok (Char.fromCode hi), offset + 16 )

        else
            -- Not a high surrogate, just parse `\\uXXXX` as a single character
            ( Ok (Char.fromCode hi), offset + 16 )

    else
        -- We did not get 4 valid hex digits
        ( Err "Unexpected end of input in \\u escape (need 4 hex digits)"
        , offset + 6
        )


parseInt : Int -> Int -> ( Result String Int, Int )
parseInt srcBytes offset0 =
    let
        nextChar =
            Pine_kernel.take [ 4, Pine_kernel.skip [ offset0, srcBytes ] ]
    in
    if Pine_kernel.equal [ nextChar, '-' ] then
        -- If we see a minus sign, parse the rest as an unsigned integer
        let
            ( unsignedResult, offset1 ) =
                parseUnsignedInt
                    srcBytes
                    (Pine_kernel.int_add [ offset0, 4 ])
        in
        case unsignedResult of
            Ok unsignedVal ->
                ( Ok -unsignedVal
                , offset1
                )

            Err err ->
                ( Err err
                , offset1
                )

    else
        -- If no minus sign, parse the rest as an unsigned integer
        parseUnsignedInt srcBytes offset0


parseUnsignedInt : Int -> Int -> ( Result String Int, Int )
parseUnsignedInt srcBytes offset0 =
    case Pine_kernel.take [ 4, Pine_kernel.skip [ offset0, srcBytes ] ] of
        '0' ->
            ( Ok 0, offset0 + 4 )

        '1' ->
            parseUnsignedIntRec 1 srcBytes (offset0 + 4)

        '2' ->
            parseUnsignedIntRec 2 srcBytes (offset0 + 4)

        '3' ->
            parseUnsignedIntRec 3 srcBytes (offset0 + 4)

        '4' ->
            parseUnsignedIntRec 4 srcBytes (offset0 + 4)

        '5' ->
            parseUnsignedIntRec 5 srcBytes (offset0 + 4)

        '6' ->
            parseUnsignedIntRec 6 srcBytes (offset0 + 4)

        '7' ->
            parseUnsignedIntRec 7 srcBytes (offset0 + 4)

        '8' ->
            parseUnsignedIntRec 8 srcBytes (offset0 + 4)

        '9' ->
            parseUnsignedIntRec 9 srcBytes (offset0 + 4)

        _ ->
            ( Err "Expecting a digit", offset0 )


parseUnsignedIntRec : Int -> Int -> Int -> ( Result String Int, Int )
parseUnsignedIntRec upper srcBytes offset0 =
    case Pine_kernel.take [ 4, Pine_kernel.skip [ offset0, srcBytes ] ] of
        '0' ->
            parseUnsignedIntRec (upper * 10) srcBytes (offset0 + 4)

        '1' ->
            parseUnsignedIntRec (upper * 10 + 1) srcBytes (offset0 + 4)

        '2' ->
            parseUnsignedIntRec (upper * 10 + 2) srcBytes (offset0 + 4)

        '3' ->
            parseUnsignedIntRec (upper * 10 + 3) srcBytes (offset0 + 4)

        '4' ->
            parseUnsignedIntRec (upper * 10 + 4) srcBytes (offset0 + 4)

        '5' ->
            parseUnsignedIntRec (upper * 10 + 5) srcBytes (offset0 + 4)

        '6' ->
            parseUnsignedIntRec (upper * 10 + 6) srcBytes (offset0 + 4)

        '7' ->
            parseUnsignedIntRec (upper * 10 + 7) srcBytes (offset0 + 4)

        '8' ->
            parseUnsignedIntRec (upper * 10 + 8) srcBytes (offset0 + 4)

        '9' ->
            parseUnsignedIntRec (upper * 10 + 9) srcBytes (offset0 + 4)

        _ ->
            ( Ok upper, offset0 )


convert1OrMoreHexadecimal : Int -> Int -> ( Int, Int )
convert1OrMoreHexadecimal offset srcBytes =
    case Pine_kernel.take [ 4, Pine_kernel.skip [ offset, srcBytes ] ] of
        '0' ->
            convert0OrMoreHexadecimal 0 (offset + 4) srcBytes

        '1' ->
            convert0OrMoreHexadecimal 1 (offset + 4) srcBytes

        '2' ->
            convert0OrMoreHexadecimal 2 (offset + 4) srcBytes

        '3' ->
            convert0OrMoreHexadecimal 3 (offset + 4) srcBytes

        '4' ->
            convert0OrMoreHexadecimal 4 (offset + 4) srcBytes

        '5' ->
            convert0OrMoreHexadecimal 5 (offset + 4) srcBytes

        '6' ->
            convert0OrMoreHexadecimal 6 (offset + 4) srcBytes

        '7' ->
            convert0OrMoreHexadecimal 7 (offset + 4) srcBytes

        '8' ->
            convert0OrMoreHexadecimal 8 (offset + 4) srcBytes

        '9' ->
            convert0OrMoreHexadecimal 9 (offset + 4) srcBytes

        'a' ->
            convert0OrMoreHexadecimal 10 (offset + 4) srcBytes

        'A' ->
            convert0OrMoreHexadecimal 10 (offset + 4) srcBytes

        'b' ->
            convert0OrMoreHexadecimal 11 (offset + 4) srcBytes

        'B' ->
            convert0OrMoreHexadecimal 11 (offset + 4) srcBytes

        'c' ->
            convert0OrMoreHexadecimal 12 (offset + 4) srcBytes

        'C' ->
            convert0OrMoreHexadecimal 12 (offset + 4) srcBytes

        'd' ->
            convert0OrMoreHexadecimal 13 (offset + 4) srcBytes

        'D' ->
            convert0OrMoreHexadecimal 13 (offset + 4) srcBytes

        'e' ->
            convert0OrMoreHexadecimal 14 (offset + 4) srcBytes

        'E' ->
            convert0OrMoreHexadecimal 14 (offset + 4) srcBytes

        'f' ->
            convert0OrMoreHexadecimal 15 (offset + 4) srcBytes

        'F' ->
            convert0OrMoreHexadecimal 15 (offset + 4) srcBytes

        _ ->
            ( 0, -1 )


convert0OrMoreHexadecimal : Int -> Int -> Int -> ( Int, Int )
convert0OrMoreHexadecimal soFar offset srcBytes =
    let
        nextChar =
            Pine_kernel.take [ 4, Pine_kernel.skip [ offset, srcBytes ] ]
    in
    if Pine_kernel.equal [ Pine_kernel.length nextChar, 0 ] then
        -- We ran out of characters, return what we have so far
        ( soFar, offset )

    else
        case nextChar of
            '0' ->
                convert0OrMoreHexadecimal (soFar * 16) (offset + 4) srcBytes

            '1' ->
                convert0OrMoreHexadecimal (soFar * 16 + 1) (offset + 4) srcBytes

            '2' ->
                convert0OrMoreHexadecimal (soFar * 16 + 2) (offset + 4) srcBytes

            '3' ->
                convert0OrMoreHexadecimal (soFar * 16 + 3) (offset + 4) srcBytes

            '4' ->
                convert0OrMoreHexadecimal (soFar * 16 + 4) (offset + 4) srcBytes

            '5' ->
                convert0OrMoreHexadecimal (soFar * 16 + 5) (offset + 4) srcBytes

            '6' ->
                convert0OrMoreHexadecimal (soFar * 16 + 6) (offset + 4) srcBytes

            '7' ->
                convert0OrMoreHexadecimal (soFar * 16 + 7) (offset + 4) srcBytes

            '8' ->
                convert0OrMoreHexadecimal (soFar * 16 + 8) (offset + 4) srcBytes

            '9' ->
                convert0OrMoreHexadecimal (soFar * 16 + 9) (offset + 4) srcBytes

            'a' ->
                convert0OrMoreHexadecimal (soFar * 16 + 10) (offset + 4) srcBytes

            'A' ->
                convert0OrMoreHexadecimal (soFar * 16 + 10) (offset + 4) srcBytes

            'b' ->
                convert0OrMoreHexadecimal (soFar * 16 + 11) (offset + 4) srcBytes

            'B' ->
                convert0OrMoreHexadecimal (soFar * 16 + 11) (offset + 4) srcBytes

            'c' ->
                convert0OrMoreHexadecimal (soFar * 16 + 12) (offset + 4) srcBytes

            'C' ->
                convert0OrMoreHexadecimal (soFar * 16 + 12) (offset + 4) srcBytes

            'd' ->
                convert0OrMoreHexadecimal (soFar * 16 + 13) (offset + 4) srcBytes

            'D' ->
                convert0OrMoreHexadecimal (soFar * 16 + 13) (offset + 4) srcBytes

            'e' ->
                convert0OrMoreHexadecimal (soFar * 16 + 14) (offset + 4) srcBytes

            'E' ->
                convert0OrMoreHexadecimal (soFar * 16 + 14) (offset + 4) srcBytes

            'f' ->
                convert0OrMoreHexadecimal (soFar * 16 + 15) (offset + 4) srcBytes

            'F' ->
                convert0OrMoreHexadecimal (soFar * 16 + 15) (offset + 4) srcBytes

            _ ->
                ( 0, -1 )


skipWhitespace : Int -> Int -> Int
skipWhitespace strBytes offset =
    let
        nextChar =
            Pine_kernel.take
                [ 4
                , Pine_kernel.skip [ offset, strBytes ]
                ]
    in
    case nextChar of
        ' ' ->
            skipWhitespace strBytes (offset + 4)

        '\t' ->
            skipWhitespace strBytes (offset + 4)

        '\n' ->
            skipWhitespace strBytes (offset + 4)

        '\u{000D}' ->
            skipWhitespace strBytes (offset + 4)

        _ ->
            offset
