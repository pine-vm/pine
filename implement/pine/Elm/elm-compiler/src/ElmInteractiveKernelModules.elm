module ElmInteractiveKernelModules exposing (..)


elmKernelModulesTexts : List String
elmKernelModulesTexts =
    [ """
module Bytes exposing (..)


type Bytes
    = Elm_Bytes Int


width : Bytes -> Int
width bytes =
    case bytes of
      Elm_Bytes list ->
        Pine_kernel.length list


type Endianness
  = LE
  | BE

"""
    , """
module Bytes.Encode exposing (..)


import Bytes


type Encoder
  = I8 Int
  | I16 Endianness Int
  | I32 Endianness Int
  | U8 Int
  | U16 Endianness Int
  | U32 Endianness Int
  | SequenceEncoder (List Encoder)
  | BytesEncoder Bytes.Bytes


encode : Encoder -> Bytes.Bytes
encode builder =
    Bytes.Elm_Bytes (encodeBlob builder)


encodeBlob : Encoder -> Int
encodeBlob builder =
    case builder of
        I8 n ->
            if Pine_kernel.int_is_sorted_asc [ 0, n ] then
                Pine_kernel.take [ 1, Pine_kernel.reverse n ]

            else
                Pine_kernel.take
                    [ 1
                    , Pine_kernel.reverse
                        (Pine_kernel.bit_not (Pine_kernel.int_add [ n, 1 ]))
                    ]

        I16 e n ->
            let
                littleEndian =
                    if Pine_kernel.int_is_sorted_asc [ 0, n ] then
                        Pine_kernel.take
                            [ 2
                            , Pine_kernel.concat
                                [ Pine_kernel.reverse
                                    (Pine_kernel.skip [ 1, n ])
                                , Pine_kernel.skip [ 1, 0 ]
                                ]
                            ]

                    else
                        Pine_kernel.take
                            [ 2
                            , Pine_kernel.concat
                                [ Pine_kernel.reverse
                                    (Pine_kernel.skip
                                        [ 1
                                        , Pine_kernel.bit_not (Pine_kernel.int_add [ n, 1 ])
                                        ]
                                    )
                                , Pine_kernel.skip [ 1, 0xFF ]
                                ]
                            ]
            in
            if Pine_kernel.equal [ e, Bytes.LE ] then
                littleEndian

            else
                Pine_kernel.reverse littleEndian

        I32 e n ->
            let
                littleEndian =
                    if Pine_kernel.int_is_sorted_asc [ 0, n ] then
                        Pine_kernel.take
                            [ 4
                            , Pine_kernel.concat
                                [ Pine_kernel.reverse
                                    (Pine_kernel.skip [ 1, n ])
                                , Pine_kernel.skip [ 2, 0x01000000 ]
                                ]
                            ]

                    else
                        Pine_kernel.take
                            [ 4
                            , Pine_kernel.concat
                                [ Pine_kernel.reverse
                                    (Pine_kernel.skip
                                        [ 1
                                        , Pine_kernel.bit_not (Pine_kernel.int_add [ n, 1 ])
                                        ]
                                    )
                                , Pine_kernel.skip [ 1, 0x00FFFFFF ]
                                ]
                            ]
            in
            if Pine_kernel.equal [ e, Bytes.LE ] then
                littleEndian

            else
                Pine_kernel.reverse littleEndian

        U8 n ->
            Pine_kernel.take [ 1, Pine_kernel.reverse n ]

        U16 e n ->
            let
                littleEndian =
                    Pine_kernel.take
                        [ 2
                        , Pine_kernel.concat
                            [ Pine_kernel.reverse
                                (Pine_kernel.skip [ 1, n ])
                            , Pine_kernel.skip [ 1, 0 ]
                            ]
                        ]
            in
            if Pine_kernel.equal [ e, Bytes.LE ] then
                littleEndian

            else
                Pine_kernel.reverse littleEndian

        U32 e n ->
            let
                littleEndian =
                    Pine_kernel.take
                        [ 4
                        , Pine_kernel.concat
                            [ Pine_kernel.reverse
                                (Pine_kernel.skip [ 1, n ])
                            , Pine_kernel.skip [ 2, 0x01000000 ]
                            ]
                        ]
            in
            if Pine_kernel.equal [ e, Bytes.LE ] then
                littleEndian

            else
                Pine_kernel.reverse littleEndian

        SequenceEncoder bs ->
            if Pine_kernel.equal [ bs, [] ] then
                Pine_kernel.take [ 0, 0 ]

            else
                Pine_kernel.concat (List.map encodeBlob bs)

        BytesEncoder (Bytes.Elm_Bytes blob) ->
            blob


-- INTEGERS


{-| Encode integers from `0` to `255` in one byte.
-}
unsignedInt8 : Int -> Encoder
unsignedInt8 int =
  U8 int


signedInt8 : Int -> Encoder
signedInt8 int =
  I8 int


{-| Encode integers from `0` to `65535` in two bytes.
-}
unsignedInt16 : Endianness -> Int -> Encoder
unsignedInt16 int =
  U16 int


signedInt16 : Endianness -> Int -> Encoder
signedInt16 int =
  I16 int


{-| Encode integers from `0` to `4294967295` in four bytes.
-}
unsignedInt32 : Endianness -> Int -> Encoder
unsignedInt32 int =
  U32 int


signedInt32 : Endianness -> Int -> Encoder
signedInt32 int =
  I32 int


bytes : Bytes.Bytes -> Encoder
bytes bytes =
  BytesEncoder bytes


sequence : List Encoder -> Encoder
sequence builders =
    SequenceEncoder builders


string : String -> Encoder
string (String chars) =
    let
        blob =
            encodeCharsAsBlob chars
    in
    BytesEncoder (Bytes.Elm_Bytes blob)


encodeCharsAsBlob : List Char -> Int
encodeCharsAsBlob chars =
    if chars == [] then
        -- 'concat' on an empty list would not yield a blob
        emptyBlob

    else
        Pine_kernel.concat
            (List.map encodeCharAsBlob chars)


encodeCharAsBlob : Char -> Int
encodeCharAsBlob char =
    let
        code =
            Char.toCode char
    in
    if Pine_kernel.int_is_sorted_asc [ code, 0x7f ] then
        -- 1-byte encoding
        char

    else if Pine_kernel.int_is_sorted_asc [ code, 0x7ff ] then
        -- 2-byte encoding
        let
            byte1 =
                Pine_kernel.bit_or
                    [ 0xC0
                    , code // 64
                    ]

            byte2 =
                Pine_kernel.bit_or
                    [ maskSingleByteMSB
                    , Pine_kernel.bit_and [ 63, code ]
                    ]
        in
        Pine_kernel.concat
            [ Pine_kernel.bit_and [ byte1, maskSingleByte ]
            , Pine_kernel.bit_and [ byte2, maskSingleByte ]
            ]

    else if Pine_kernel.int_is_sorted_asc [ code, 0xffff ] then
        -- 3-byte encoding
        let
            byte1 =
                Pine_kernel.bit_or
                    [ 0xE0
                    , code // 4096
                    ]

            byte2 =
                Pine_kernel.bit_or
                    [ maskSingleByteMSB
                    , Pine_kernel.bit_and [ 63, code // 64 ]
                    ]

            byte3 =
                Pine_kernel.bit_or
                    [ maskSingleByteMSB
                    , Pine_kernel.bit_and [ 63, code ]
                    ]
        in
        Pine_kernel.concat
            [ Pine_kernel.bit_and [ byte1, maskSingleByte ]
            , Pine_kernel.bit_and [ byte2, maskSingleByte ]
            , Pine_kernel.bit_and [ byte3, maskSingleByte ]
            ]

    else
        -- 4-byte encoding for code points >= 0x10000
        let
            byte1 =
                Pine_kernel.bit_or
                    [ Pine_kernel.bit_and [ 0xF0, maskSingleByte ]
                    , code // 262144
                    ]

            byte2 =
                Pine_kernel.bit_or
                    [ maskSingleByteMSB
                    , Pine_kernel.bit_and [ 63, code // 4096 ]
                    ]

            byte3 =
                Pine_kernel.bit_or
                    [ maskSingleByteMSB
                    , Pine_kernel.bit_and [ 63, code // 64 ]
                    ]

            byte4 =
                Pine_kernel.bit_or
                    [ maskSingleByteMSB
                    , Pine_kernel.bit_and [ 63, code ]
                    ]
        in
        Pine_kernel.concat
            [ Pine_kernel.bit_and [ byte1, maskSingleByte ]
            , Pine_kernel.bit_and [ byte2, maskSingleByte ]
            , Pine_kernel.bit_and [ byte3, maskSingleByte ]
            , Pine_kernel.bit_and [ byte4, maskSingleByte ]
            ]


getStringWidth : String -> Int
getStringWidth (String chars) =
    Pine_kernel.length
        (encodeCharsAsBlob chars)


maskSingleByte : Int
maskSingleByte =
    Pine_kernel.skip [ 1, 0xff ]


maskSingleByteMSB : Int
maskSingleByteMSB =
    Pine_kernel.skip [ 1, 0x80 ]


emptyBlob : Int
emptyBlob =
    Pine_kernel.take [ 0, 0 ]


"""
    , """
module Bytes.Decode exposing (..)


import Bytes


-- PARSER


{-| Describes how to turn a sequence of bytes into a nice Elm value.
-}
type Decoder a
    = Decoder (Bytes -> Int -> ( Int, a ))


{-| Turn a sequence of bytes into a nice Elm value.

    -- decode (unsignedInt16 BE) <0007> == Just 7
    -- decode (unsignedInt16 LE) <0700> == Just 7
    -- decode (unsignedInt16 BE) <0700> == Just 1792
    -- decode (unsignedInt32 BE) <0700> == Nothing



The `Decoder` specifies exactly how this should happen. This process may fail
if the sequence of bytes is corrupted or unexpected somehow. The examples above
show a case where there are not enough bytes.

-}
decode : Decoder a -> Bytes -> Maybe a
decode (Decoder decoder) bytes =
    let
        (Bytes.Elm_Bytes blob) =
            bytes

        ( offset, result ) =
            decoder bytes 0

        blobLength =
            Pine_kernel.length blob
    in
    if Pine_kernel.int_is_sorted_asc [ 0, offset, blobLength ]
    then
        Just result
    else
        Nothing


{-| Decode one byte into an integer from `0` to `255`.
-}
unsignedInt8 : Decoder Int
unsignedInt8 =
    Decoder
        (\\(Bytes.Elm_Bytes blob) offset ->
            let
                byte =
                    Pine_kernel.take [ 1, Pine_kernel.skip [ offset, blob ] ]
            in
            ( Pine_kernel.int_add [ offset, 1 ]
            , Pine_kernel.concat [ Pine_kernel.take [ 1, 0 ], byte ]
            )
        )


signedInt8 : Decoder Int
signedInt8 =
    Decoder
        (\\(Bytes.Elm_Bytes blob) offset ->
            let
                byte =
                    Pine_kernel.take [ 1, Pine_kernel.skip [ offset, blob ] ]

                asInt =
                    if Pine_kernel.equal [ Pine_kernel.bit_and [ byte, 0x80 ], 0 ] then
                        Pine_kernel.concat [ Pine_kernel.take [ 1, 0 ], byte ]

                    else
                        Pine_kernel.int_add
                            [ -1
                            , Pine_kernel.concat
                                [ Pine_kernel.take [ 1, -1 ]
                                , Pine_kernel.bit_not byte
                                ]
                            ]
            in
            ( Pine_kernel.int_add [ offset, 1 ]
            , asInt
            )
        )


unsignedInt16 : Bytes.Endianness -> Decoder Int
unsignedInt16 endianness =
    Decoder
        (\\(Bytes.Elm_Bytes blob) offset ->
            let
                bytes =
                    Pine_kernel.take [ 2, Pine_kernel.skip [ offset, blob ] ]

                asInt =
                    if Pine_kernel.equal [ endianness, Bytes.LE ] then
                        Pine_kernel.concat
                            [ Pine_kernel.take [ 1, 0 ]
                            , Pine_kernel.reverse bytes
                            ]

                    else
                        Pine_kernel.concat
                            [ Pine_kernel.take [ 1, 0 ]
                            , bytes
                            ]
            in
            ( Pine_kernel.int_add [ offset, 2 ]
            , asInt
            )
        )


signedInt16 : Bytes.Endianness -> Decoder Int
signedInt16 endianness =
    Decoder
        (\\(Bytes.Elm_Bytes blob) offset ->
            let
                bytes =
                    Pine_kernel.take [ 2, Pine_kernel.skip [ offset, blob ] ]

                bytesOrdered =
                    if Pine_kernel.equal [ endianness, Bytes.LE ] then
                        Pine_kernel.reverse bytes

                    else
                        bytes

                asInt =
                    if
                        Pine_kernel.equal
                            [ Pine_kernel.bit_and [ bytesOrdered, 0x8000 ]
                            , Pine_kernel.skip [ 2, 0x00010000 ]
                            ]
                    then
                        Pine_kernel.concat [ Pine_kernel.take [ 1, 0 ], bytesOrdered ]

                    else
                        Pine_kernel.int_add
                            [ -1
                            , Pine_kernel.concat
                                [ Pine_kernel.take [ 1, -1 ]
                                , Pine_kernel.bit_not bytesOrdered
                                ]
                            ]
            in
            ( Pine_kernel.int_add [ offset, 2 ]
            , asInt
            )
        )


unsignedInt32 : Bytes.Endianness -> Decoder Int
unsignedInt32 endianness =
    Decoder
        (\\(Bytes.Elm_Bytes blob) offset ->
            let
                bytes =
                    Pine_kernel.take [ 4, Pine_kernel.skip [ offset, blob ] ]

                asInt =
                    if Pine_kernel.equal [ endianness, Bytes.LE ] then
                        Pine_kernel.concat
                            [ Pine_kernel.take [ 1, 0 ]
                            , Pine_kernel.reverse bytes
                            ]

                    else
                        Pine_kernel.concat [ Pine_kernel.take [ 1, 0 ], bytes ]
            in
            ( Pine_kernel.int_add [ offset, 4 ]
            , asInt
            )
        )


signedInt32 : Bytes.Endianness -> Decoder Int
signedInt32 endianness =
    Decoder
        (\\(Bytes.Elm_Bytes blob) offset ->
            let
                bytes =
                    Pine_kernel.take [ 4, Pine_kernel.skip [ offset, blob ] ]

                bytesOrdered =
                    if Pine_kernel.equal [ endianness, Bytes.LE ] then
                        Pine_kernel.reverse bytes

                    else
                        bytes

                asInt =
                    if
                        Pine_kernel.equal
                            [ Pine_kernel.bit_and [ bytesOrdered, 0x80000000 ]
                            , Pine_kernel.skip [ 2, 0x000100000000 ]
                            ]
                    then
                        Pine_kernel.concat [ Pine_kernel.take [ 1, 0 ], bytesOrdered ]

                    else
                        Pine_kernel.int_add
                            [ -1
                            , Pine_kernel.concat
                                [ Pine_kernel.take [ 1, -1 ]
                                , Pine_kernel.bit_not bytesOrdered
                                ]
                            ]
            in
            ( Pine_kernel.int_add [ offset, 4 ]
            , asInt
            )
        )


string : Int -> Decoder String
string length =
    Decoder
        (\\(Bytes.Elm_Bytes blob) offset ->
            let
                bytes =
                    Pine_kernel.take [ length, Pine_kernel.skip [ offset, blob ] ]
            in
            ( Pine_kernel.int_add [ offset, length ]
            , decodeBlobAsChars bytes
            )
        )


decodeBlobAsChars : Int -> String
decodeBlobAsChars blob =
    decodeBlobAsCharsRec 0 blob []


decodeBlobAsCharsRec : Int -> Int -> List Char -> String
decodeBlobAsCharsRec offset blob chars =
    if Pine_kernel.int_is_sorted_asc [ Pine_kernel.length blob, offset ]
    then
        String.fromList (List.reverse chars)
    else
        let
            ( char, bytesConsumed ) =
                decodeUtf8Char blob offset
        in
        decodeBlobAsCharsRec
            (offset + bytesConsumed)
            blob
            (char :: chars)


decodeUtf8Char : Int -> Int -> ( Int, Int )
decodeUtf8Char blob offset =
    let
        firstByte =
            Pine_kernel.take [ 1, Pine_kernel.skip [ offset, blob ] ]

        firstByteInt =
            Pine_kernel.concat [ Pine_kernel.take [ 1, 0 ], firstByte ]
    in
    if Pine_kernel.int_is_sorted_asc [ firstByteInt, 0x7f ] then
        -- 1-byte character (ASCII)
        ( firstByte, 1 )

    else if Pine_kernel.equal [ Pine_kernel.bit_and [ firstByteInt, 0xE0 ], 0xC0 ] then
        -- 2-byte character
        let
            byte2 =
                Pine_kernel.take [ 1, Pine_kernel.skip [ Pine_kernel.int_add [ offset, 1 ], blob ] ]

            byte2Int =
                Pine_kernel.concat [ Pine_kernel.take [ 1, 0 ], byte2 ]

            firstFiveBits =
                Pine_kernel.bit_and [ firstByteInt, 0x1F ]

            secondSixBits =
                Pine_kernel.bit_and [ byte2Int, 0x3F ]

            charCode =
                Pine_kernel.int_add
                    [ Pine_kernel.int_mul [ firstFiveBits, 64 ] -- Multiply by 2^6
                    , secondSixBits
                    ]
        in
        ( Pine_kernel.skip [ 1, charCode ], 2 )

    else if Pine_kernel.equal [ Pine_kernel.bit_and [ firstByteInt, 0xF0 ], 0xE0 ] then
        -- 3-byte character
        let
            byte2 =
                Pine_kernel.take [ 1, Pine_kernel.skip [ Pine_kernel.int_add [ offset, 1 ], blob ] ]

            byte2Int =
                Pine_kernel.concat [ Pine_kernel.take [ 1, 0 ], byte2 ]

            byte3 =
                Pine_kernel.take [ 1, Pine_kernel.skip [ Pine_kernel.int_add [ offset, 2 ], blob ] ]

            byte3Int =
                Pine_kernel.concat [ Pine_kernel.take [ 1, 0 ], byte3 ]

            firstFourBits =
                Pine_kernel.bit_and [ firstByteInt, 0x0F ]

            secondSixBits =
                Pine_kernel.bit_and [ byte2Int, 0x3F ]

            thirdSixBits =
                Pine_kernel.bit_and [ byte3Int, 0x3F ]

            charCode =
                Pine_kernel.int_add
                    [ Pine_kernel.int_mul [ firstFourBits, 4096 ] -- Multiply by 2^12
                    , Pine_kernel.int_mul [ secondSixBits, 64 ]    -- Multiply by 2^6
                    , thirdSixBits
                    ]
        in
        ( Pine_kernel.skip [ 1, charCode ], 3 )

    else if Pine_kernel.equal [ Pine_kernel.bit_and [ firstByteInt, 0xF8 ], 0xF0 ] then
        -- 4-byte character
        let
            byte2 =
                Pine_kernel.take [ 1, Pine_kernel.skip [ Pine_kernel.int_add [ offset, 1 ], blob ] ]

            byte2Int =
                Pine_kernel.concat [ Pine_kernel.take [ 1, 0 ], byte2 ]

            byte3 =
                Pine_kernel.take [ 1, Pine_kernel.skip [ Pine_kernel.int_add [ offset, 2 ], blob ] ]

            byte3Int =
                Pine_kernel.concat [ Pine_kernel.take [ 1, 0 ], byte3 ]

            byte4 =
                Pine_kernel.take [ 1, Pine_kernel.skip [ Pine_kernel.int_add [ offset, 3 ], blob ] ]

            byte4Int =
                Pine_kernel.concat [ Pine_kernel.take [ 1, 0 ], byte4 ]

            firstThreeBits =
                Pine_kernel.bit_and [ firstByteInt, 0x07 ]

            secondSixBits =
                Pine_kernel.bit_and [ byte2Int, 0x3F ]

            thirdSixBits =
                Pine_kernel.bit_and [ byte3Int, 0x3F ]

            fourthSixBits =
                Pine_kernel.bit_and [ byte4Int, 0x3F ]

            charCode =
                Pine_kernel.int_add
                    [ Pine_kernel.int_mul [ firstThreeBits, 262144 ] -- Multiply by 2^18
                    , Pine_kernel.int_mul [ secondSixBits, 4096 ]    -- Multiply by 2^12
                    , Pine_kernel.int_mul [ thirdSixBits, 64 ]       -- Multiply by 2^6
                    , fourthSixBits
                    ]
        in
        ( Pine_kernel.skip [ 1, charCode ], 4 )

    else
        -- Invalid UTF-8 sequence; use replacement character
        ( Pine_kernel.skip [ 1, 0xFFFD ], 1 )


succeed : a -> Decoder a
succeed a =
    Decoder (\\_ offset -> ( offset, a ))


map : (a -> b) -> Decoder a -> Decoder b
map func (Decoder decodeA) =
    Decoder
        (\\bites offset ->
            let
                ( aOffset, a ) =
                    decodeA bites offset
            in
            ( aOffset, func a )
        )


type Step state a
    = Loop state
    | Done a


loop : state -> (state -> Decoder (Step state a)) -> Decoder a
loop state callback =
    Decoder (loopHelp state callback)


loopHelp : state -> (state -> Decoder (Step state a)) -> Bytes -> Int -> ( Int, a )
loopHelp state callback bites offset =
    let
        (Decoder decoder) =
            callback state

        ( newOffset, step ) =
            decoder bites offset
    in
    case step of
        Loop newState ->
            loopHelp newState callback bites newOffset

        Done result ->
            ( newOffset, result )


map2 : (a -> b -> value) -> Decoder a -> Decoder b -> Decoder value
map2 func (Decoder decodeA) (Decoder decodeB) =
    Decoder
        (\\bites offset ->
            let
                ( offsetA, a ) =
                    decodeA bites offset

                ( offsetB, b ) =
                    decodeB bites offsetA
            in
            ( offsetB, func a b )
        )


map3 : (a -> b -> c -> value) -> Decoder a -> Decoder b -> Decoder c -> Decoder value
map3 func (Decoder decodeA) (Decoder decodeB) (Decoder decodeC) =
    Decoder
        (\\bites offset ->
            let
                ( offsetA, a ) =
                    decodeA bites offset

                ( offsetB, b ) =
                    decodeB bites offsetA

                ( offsetC, c ) =
                    decodeC bites offsetB
            in
            ( offsetC, func a b c )
        )


map4 :
    (a -> b -> c -> d -> value)
    -> Decoder a
    -> Decoder b
    -> Decoder c
    -> Decoder d
    -> Decoder value
map4 func (Decoder decodeA) (Decoder decodeB) (Decoder decodeC) (Decoder decodeD) =
    Decoder
        (\\bites offset ->
            let
                ( offsetA, a ) =
                    decodeA bites offset

                ( offsetB, b ) =
                    decodeB bites offsetA

                ( offsetC, c ) =
                    decodeC bites offsetB

                ( offsetD, d ) =
                    decodeD bites offsetC
            in
            ( offsetD, func a b c d )
        )


map5 :
    (a -> b -> c -> d -> e -> value)
    -> Decoder a
    -> Decoder b
    -> Decoder c
    -> Decoder d
    -> Decoder e
    -> Decoder value
map5 func (Decoder decodeA) (Decoder decodeB) (Decoder decodeC) (Decoder decodeD) (Decoder decodeE) =
    Decoder
        (\\bites offset ->
            let
                ( offsetA, a ) =
                    decodeA bites offset

                ( offsetB, b ) =
                    decodeB bites offsetA

                ( offsetC, c ) =
                    decodeC bites offsetB

                ( offsetD, d ) =
                    decodeD bites offsetC

                ( offsetE, e ) =
                    decodeE bites offsetD
            in
            ( offsetE, func a b c d e )
        )


andThen : (a -> Decoder b) -> Decoder a -> Decoder b
andThen callback (Decoder decodeA) =
    Decoder
        (\\bites offset ->
            let
                ( offsetA, a ) =
                    decodeA bites offset

                (Decoder decodeB) =
                    callback a

                ( offsetB, b ) =
                    decodeB bites offsetA
            in
            ( offsetB, b )
        )


"""
    , """
module Json.Encode exposing (..)


type Value
    = NullValue
    | BoolValue Bool
    | IntValue Int
    | StringValue String
    | ArrayValue (List Value)
    | ObjectValue (List ( String, Value ))


null : Value
null =
    NullValue


bool : Bool -> Value
bool =
    BoolValue


int : Int -> Value
int =
    IntValue


string : String -> Value
string =
    StringValue


list : (item -> Value) -> List item -> Value
list encodeItem items =
    ArrayValue (List.map encodeItem items)


object : List ( String, Value ) -> Value
object =
    ObjectValue


encode : Int -> Value -> String
encode indent value =
    case value of
        NullValue ->
            "null"

        BoolValue boolVal ->
            if boolVal then
                "true"

            else
                "false"

        IntValue intVal ->
            String.fromInt intVal

        StringValue stringVal ->
            "\\"" ++ escapeString stringVal ++ "\\""

        ArrayValue values ->
            "[" ++ String.join "," (List.map (encode indent) values) ++ "]"

        ObjectValue fields ->
            "{" ++ String.join "," (List.map (encodeField indent) fields) ++ "}"


encodeField : Int -> ( String, Value ) -> String
encodeField indent ( key, value ) =
    "\\"" ++ escapeString key ++ "\\":" ++ encode indent value


escapeString : String -> String
escapeString stringVal =
    String.join "" (List.map escapeChar (String.toList stringVal))


escapeChar : Char -> String
escapeChar char =
    case Char.toCode char of
        8 ->
            "\\\\b"

        9 ->
            "\\\\t"

        10 ->
            "\\\\n"

        12 ->
            "\\\\f"

        13 ->
            "\\\\r"

        34 ->
            "\\\\\\""

        92 ->
            "\\\\\\\\"

        _ ->
            String.fromChar char

"""
    , """
module Json.Decode exposing
  ( Decoder, string, bool, int, float
  , nullable, list, array, dict, keyValuePairs, oneOrMore
  , field, at, index
  , maybe, oneOf
  , decodeString, decodeValue, Value, Error(..), errorToString
  , map, map2, map3, map4, map5, map6, map7, map8
  , lazy, value, null, succeed, fail, andThen
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
            case List.filter (\\( k, _ ) -> k == key) fields of
                [ (_, value) ] ->
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
            ( Err "Expected '""' to start object key", 0 )


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


"""
    , """
module Elm.Kernel.Parser exposing (..)

import Char


consumeBase : Int -> Int -> String -> ( Int, Int )
consumeBase base offset (String chars) =
    consumeBaseHelper base offset chars 0


consumeBaseHelper : Int -> Int -> List Char -> Int -> ( Int, Int )
consumeBaseHelper base offset chars total =
    case Pine_kernel.skip [ offset, chars ] of
        [] ->
            ( offset, total )

        nextChar :: _ ->
            let
                digit : Int
                digit =
                    Pine_kernel.int_add [ Char.toCode nextChar, -48 ]

                lastDigit : Int
                lastDigit =
                    Pine_kernel.int_add [ base, -1 ]
            in
            if Pine_kernel.int_is_sorted_asc [ 0, digit, lastDigit ] then
                consumeBaseHelper
                    base
                    (Pine_kernel.int_add [ offset, 1 ])
                    chars
                    (Pine_kernel.int_add [ Pine_kernel.int_mul [ base, total ], digit ])

            else
                ( offset, total )


consumeBase16 : Int -> String -> ( Int, Int )
consumeBase16 offset (String chars) =
    consumeBase16Helper offset chars 0


consumeBase16Helper : Int -> List Char -> Int -> ( Int, Int )
consumeBase16Helper offset chars total =
    let
        char =
            Pine_kernel.head (Pine_kernel.skip [ offset, chars ])
    in
    if Pine_kernel.equal [ char, [] ] then
        ( offset, total )

    else
        let
            code =
                Char.toCode char

            digit =
                if Pine_kernel.int_is_sorted_asc [ 48, code, 57 ] then
                    -- '0' to '9'
                    Just (Pine_kernel.int_add [ code, -48 ])

                else if Pine_kernel.int_is_sorted_asc [ 65, code, 70 ] then
                    -- 'A' to 'F'
                    Just (Pine_kernel.int_add [ code, -55 ])

                else if Pine_kernel.int_is_sorted_asc [ 97, code, 102 ] then
                    -- 'a' to 'f'
                    Just (Pine_kernel.int_add [ code, -87 ])

                else
                    Nothing
        in
        case digit of
            Just d ->
                consumeBase16Helper
                    (Pine_kernel.int_add [ offset, 1 ])
                    chars
                    (Pine_kernel.int_add [ Pine_kernel.int_mul [ 16, total ], d ])

            Nothing ->
                ( offset, total )


chompBase10 : Int -> String -> Int
chompBase10 offset (String chars) =
    chompBase10Helper offset chars


chompBase10Helper : Int -> List Char -> Int
chompBase10Helper offset chars =
    let
        char =
            Pine_kernel.head (Pine_kernel.skip [ offset, chars ])
    in
    if Pine_kernel.equal [ char, [] ] then
        offset

    else
        let
            code =
                Char.toCode char
        in
        if Pine_kernel.int_is_sorted_asc [ 48, code, 57 ] then
            chompBase10Helper
                (Pine_kernel.int_add [ offset, 1 ])
                chars

        else
            offset


isSubString : String -> Int -> Int -> Int -> String -> ( Int, Int, Int )
isSubString (String smallChars) offset row col (String bigChars) =
    let
        expectedLength : Int
        expectedLength =
            Pine_kernel.length smallChars

        sliceFromSourceChars : List Char
        sliceFromSourceChars =
            Pine_kernel.take
                [ expectedLength
                , Pine_kernel.skip [ offset, bigChars ]
                ]
    in
    if Pine_kernel.equal [ sliceFromSourceChars, smallChars ] then
        let
            ( newlineCount, colShift ) =
                countOffsetsInString ( 0, 0 ) smallChars

            newOffset : Int
            newOffset =
                Pine_kernel.int_add [ offset, expectedLength ]

            newRow : Int
            newRow =
                Pine_kernel.int_add [ row, newlineCount ]

            newCol : Int
            newCol =
                if Pine_kernel.equal [ newlineCount, 0 ] then
                    Pine_kernel.int_add [ col, colShift ]

                else
                    Pine_kernel.int_add [ 1, colShift ]
        in
        ( newOffset, newRow, newCol )

    else
        ( -1, row, col )


isSubChar : (Char -> Bool) -> Int -> String -> Int
isSubChar predicate offset (String chars) =
    let
        nextChar =
            Pine_kernel.head (Pine_kernel.skip [ offset, chars ])
    in
    if Pine_kernel.equal [ nextChar, [] ] then
        -1

    else if predicate nextChar then
        if Pine_kernel.equal [ nextChar, newlineChar ] then
            -- Special code for newline
            -2

        else
            Pine_kernel.int_add [ offset, 1 ]

    else
        -1


findSubString : String -> Int -> Int -> Int -> String -> ( Int, Int, Int )
findSubString (String smallChars) offset row col (String bigChars) =
    let
        newOffset : Int
        newOffset =
            indexOf smallChars bigChars offset

        consumedLength : Int
        consumedLength =
            Pine_kernel.int_add
                [ newOffset
                , Pine_kernel.negate offset
                ]

        consumedChars : List Char
        consumedChars =
            Pine_kernel.take
                [ consumedLength
                , Pine_kernel.skip [ offset, bigChars ]
                ]

        targetOffset : Int
        targetOffset =
            if Pine_kernel.equal [ newOffset, -1 ] then
                List.length bigChars

            else
                Pine_kernel.int_add [ newOffset, Pine_kernel.length smallChars ]

        ( newlineCount, colShift ) =
            countOffsetsInString ( 0, 0 ) consumedChars

        newRow : Int
        newRow =
            Pine_kernel.int_add [ row, newlineCount ]

        newCol : Int
        newCol =
            if Pine_kernel.equal [ newlineCount, 0 ] then
                Pine_kernel.int_add [ col, colShift ]

            else
                Pine_kernel.int_add [ 1, colShift ]
    in
    ( newOffset, newRow, newCol )


indexOf : List Char -> List Char -> Int -> Int
indexOf smallChars bigChars offset =
    let
        expectedLength : Int
        expectedLength =
            Pine_kernel.length smallChars

        sliceFromSourceChars : List Char
        sliceFromSourceChars =
            Pine_kernel.take
                [ expectedLength
                , Pine_kernel.skip [ offset, bigChars ]
                ]
    in
    if Pine_kernel.equal [ Pine_kernel.length sliceFromSourceChars, expectedLength ] then
        if Pine_kernel.equal [ sliceFromSourceChars, smallChars ] then
            offset

        else
            indexOf smallChars bigChars (Pine_kernel.int_add [ offset, 1 ])

    else
        -1


startsWith : List Char -> List Char -> Bool
startsWith patternList stringList =
    Pine_kernel.equal
        [ Pine_kernel.take [ Pine_kernel.length patternList, stringList ]
        , patternList
        ]


countOffsetsInString : ( Int, Int ) -> List Char -> ( Int, Int )
countOffsetsInString ( newlines, col ) chars =
    let
        nextChar =
            Pine_kernel.head chars
    in
    if Pine_kernel.equal [ nextChar, [] ] then
        ( newlines, col )

    else if Pine_kernel.equal [ nextChar, '\\n' ] then
        countOffsetsInString
            ( Pine_kernel.int_add [ newlines, 1 ], 0 )
            (Pine_kernel.skip [ 1, chars ])

    else
        countOffsetsInString
            ( newlines, Pine_kernel.int_add [ col, 1 ] )
            (Pine_kernel.skip [ 1, chars ])


newlineChar : Char
newlineChar =
    -- ASCII code for '\\n'
    Pine_kernel.skip [ 1, 10 ]


isAsciiCode : Int -> Int -> String -> Bool
isAsciiCode code offset (String chars) =
    let
        nextChar =
            Pine_kernel.head
                (Pine_kernel.skip [ offset, chars ])
    in
    Pine_kernel.equal [ nextChar, Char.fromCode code ]

"""
    , """
module Parser.Advanced exposing
  ( Parser, run, DeadEnd, inContext, Token(..)
  , int, float, number, symbol, keyword, variable, end
  , succeed, (|=), (|.), lazy, andThen, problem
  , oneOf, map, backtrackable, commit, token
  , sequence, Trailing(..), loop, Step(..)
  , spaces, lineComment, multiComment, Nestable(..)
  , getChompedString, chompIf, chompWhile, chompUntil, chompUntilEndOr, mapChompedString
  , withIndent, getIndent
  , getPosition, getRow, getCol, getOffset, getSource

  , changeIndent
  , keeper
  , ignorer
  )


{-|

# Parsers
@docs Parser, run, DeadEnd, inContext, Token

* * *
**Everything past here works just like in the
[`Parser`](/packages/elm/parser/latest/Parser) module, except that `String`
arguments become `Token` arguments, and you need to provide a `Problem` for
certain scenarios.**
* * *

# Building Blocks
@docs int, float, number, symbol, keyword, variable, end

# Pipelines
@docs succeed, (|=), (|.), lazy, andThen, problem

# Branches
@docs oneOf, map, backtrackable, commit, token

# Loops
@docs sequence, Trailing, loop, Step

# Whitespace
@docs spaces, lineComment, multiComment, Nestable

# Chompers
@docs getChompedString, chompIf, chompWhile, chompUntil, chompUntilEndOr, mapChompedString

# Indentation
@docs withIndent, getIndent

# Positions
@docs getPosition, getRow, getCol, getOffset, getSource
-}


import Char
import Elm.Kernel.Parser
import Set



-- INFIX OPERATORS


infix left 5 (|=) = keeper
infix left 6 (|.) = ignorer


{- NOTE: the (|.) oporator binds tighter to slightly reduce the amount
of recursion in pipelines. For example:

    func
      |. a
      |. b
      |= c
      |. d
      |. e

With the same precedence:

    (ignorer (ignorer (keeper (ignorer (ignorer func a) b) c) d) e)

With higher precedence:

    keeper (ignorer (ignorer func a) b) (ignorer (ignorer c d) e)

So the maximum call depth goes from 5 to 3.
-}



-- PARSERS


{-| An advanced `Parser` gives two ways to improve your error messages:

- `problem` &mdash; Instead of all errors being a `String`, you can create a
custom type like `type Problem = BadIndent | BadKeyword String` and track
problems much more precisely.
- `context` &mdash; Error messages can be further improved when precise
problems are paired with information about where you ran into trouble. By
tracking the context, instead of saying “I found a bad keyword” you can say
“I found a bad keyword when parsing a list” and give folks a better idea of
what the parser thinks it is doing.

I recommend starting with the simpler [`Parser`][parser] module though, and
when you feel comfortable and want better error messages, you can create a type
alias like this:

```elm
import Parser.Advanced

type alias MyParser a =
  Parser.Advanced.Parser Context Problem a

type Context = Definition String | List | Record

type Problem = BadIndent | BadKeyword String
```

All of the functions from `Parser` should exist in `Parser.Advanced` in some
form, allowing you to switch over pretty easily.

[parser]: /packages/elm/parser/latest/Parser
-}
type Parser context problem value =
  Parser (State context -> PStep context problem value)


type PStep context problem value
  = Good Bool value (State context)
  | Bad Bool (Bag context problem)


type alias State context =
  { src : String
  , offset : Int
  , indent : Int
  , context : List (Located context)
  , row : Int
  , col : Int
  }


type alias Located context =
  { row : Int
  , col : Int
  , context : context
  }



-- RUN


{-| This works just like [`Parser.run`](/packages/elm/parser/latest/Parser#run).
The only difference is that when it fails, it has much more precise information
for each dead end.
-}
run : Parser c x a -> String -> Result (List (DeadEnd c x)) a
run (Parser parse) src =
  case parse { src = src, offset = 0, indent = 1, context = [], row = 1, col = 1} of
    Good _ value _ ->
      Ok value

    Bad _ bag ->
      Err (bagToList bag [])



-- PROBLEMS


{-| Say you are parsing a function named `viewHealthData` that contains a list.
You might get a `DeadEnd` like this:

```elm
{ row = 18
, col = 22
, problem = UnexpectedComma
, contextStack =
    [ { row = 14
      , col = 1
      , context = Definition "viewHealthData"
      }
    , { row = 15
      , col = 4
      , context = List
      }
    ]
}
```

We have a ton of information here! So in the error message, we can say that “I
ran into an issue when parsing a list in the definition of `viewHealthData`. It
looks like there is an extra comma.” Or maybe something even better!

Furthermore, many parsers just put a mark where the problem manifested. By
tracking the `row` and `col` of the context, we can show a much larger region
as a way of indicating “I thought I was parsing this thing that starts over
here.” Otherwise you can get very confusing error messages on a missing `]` or
`}` or `)` because “I need more indentation” on something unrelated.

**Note:** Rows and columns are counted like a text editor. The beginning is `row=1`
and `col=1`. The `col` increments as characters are chomped. When a `\\n` is chomped,
`row` is incremented and `col` starts over again at `1`.
-}
type alias DeadEnd context problem =
  { row : Int
  , col : Int
  , problem : problem
  , contextStack : List { row : Int, col : Int, context : context }
  }


type Bag c x
  = Empty
  | AddRight (Bag c x) (DeadEnd c x)
  | Append (Bag c x) (Bag c x)


fromState : State c -> x -> Bag c x
fromState s x =
  AddRight Empty (DeadEnd s.row s.col x s.context)


fromInfo : Int -> Int -> x -> List (Located c) -> Bag c x
fromInfo row col x context =
  AddRight Empty (DeadEnd row col x context)


bagToList : Bag c x -> List (DeadEnd c x) -> List (DeadEnd c x)
bagToList bag list =
  case bag of
    Empty ->
      list

    AddRight bag1 x ->
      bagToList bag1 (x :: list)

    Append bag1 bag2 ->
      bagToList bag1 (bagToList bag2 list)



-- PRIMITIVES


{-| Just like [`Parser.succeed`](Parser#succeed)
-}
succeed : a -> Parser c x a
succeed a =
  Parser <| \\s ->
    Good False a s


{-| Just like [`Parser.problem`](Parser#problem) except you provide a custom
type for your problem.
-}
problem : x -> Parser c x a
problem x =
  Parser <| \\s ->
    Bad False (fromState s x)



-- MAPPING


{-| Just like [`Parser.map`](Parser#map)
-}
map : (a -> b) -> Parser c x a -> Parser c x b
map func (Parser parse) =
  Parser <| \\s0 ->
    case parse s0 of
      Good p a s1 ->
        Good p (func a) s1

      Bad p x ->
        Bad p x


map2 : (a -> b -> value) -> Parser c x a -> Parser c x b -> Parser c x value
map2 func (Parser parseA) (Parser parseB) =
  Parser <| \\s0 ->
    case parseA s0 of
      Bad p x ->
        Bad p x

      Good p1 a s1 ->
        case parseB s1 of
          Bad p2 x ->
            Bad (p1 || p2) x

          Good p2 b s2 ->
            Good (p1 || p2) (func a b) s2


{-| Just like the [`(|=)`](Parser#|=) from the `Parser` module.
-}
keeper : Parser c x (a -> b) -> Parser c x a -> Parser c x b
keeper parseFunc parseArg =
  map2 (<|) parseFunc parseArg


{-| Just like the [`(|.)`](Parser#|.) from the `Parser` module.
-}
ignorer : Parser c x keep -> Parser c x ignore -> Parser c x keep
ignorer keepParser ignoreParser =
  map2 always keepParser ignoreParser



-- AND THEN


{-| Just like [`Parser.andThen`](Parser#andThen)
-}
andThen : (a -> Parser c x b) -> Parser c x a -> Parser c x b
andThen callback (Parser parseA) =
  Parser <| \\s0 ->
    case parseA s0 of
      Bad p x ->
        Bad p x

      Good p1 a s1 ->
        let
          (Parser parseB) =
            callback a
        in
        case parseB s1 of
          Bad p2 x ->
            Bad (p1 || p2) x

          Good p2 b s2 ->
            Good (p1 || p2) b s2



-- LAZY


{-| Just like [`Parser.lazy`](Parser#lazy)
-}
lazy : (() -> Parser c x a) -> Parser c x a
lazy thunk =
  Parser <| \\s ->
    let
      (Parser parse) =
        thunk ()
    in
    parse s



-- ONE OF


{-| Just like [`Parser.oneOf`](Parser#oneOf)
-}
oneOf : List (Parser c x a) -> Parser c x a
oneOf parsers =
  Parser <| \\s -> oneOfHelp s Empty parsers


oneOfHelp : State c -> Bag c x -> List (Parser c x a) -> PStep c x a
oneOfHelp s0 bag parsers =
  case parsers of
    [] ->
      Bad False bag

    Parser parse :: remainingParsers ->
      case parse s0 of
        Good _ _ _ as step ->
          step

        Bad p x as step ->
          if p then
            step
          else
            oneOfHelp s0 (Append bag x) remainingParsers



-- LOOP


{-| Just like [`Parser.Step`](Parser#Step)
-}
type Step state a
  = Loop state
  | Done a


{-| Just like [`Parser.loop`](Parser#loop)
-}
loop : state -> (state -> Parser c x (Step state a)) -> Parser c x a
loop state callback =
  Parser <| \\s ->
    loopHelp False state callback s


loopHelp : Bool -> state -> (state -> Parser c x (Step state a)) -> State c -> PStep c x a
loopHelp p state callback s0 =
  let
    (Parser parse) =
      callback state
  in
  case parse s0 of
    Good p1 step s1 ->
      case step of
        Loop newState ->
          loopHelp (p || p1) newState callback s1

        Done result ->
          Good (p || p1) result s1

    Bad p1 x ->
      Bad (p || p1) x



-- BACKTRACKABLE


{-| Just like [`Parser.backtrackable`](Parser#backtrackable)
-}
backtrackable : Parser c x a -> Parser c x a
backtrackable (Parser parse) =
  Parser <| \\s0 ->
    case parse s0 of
      Bad _ x ->
        Bad False x

      Good _ a s1 ->
        Good False a s1


{-| Just like [`Parser.commit`](Parser#commit)
-}
commit : a -> Parser c x a
commit a =
  Parser <| \\s -> Good True a s



-- SYMBOL


{-| Just like [`Parser.symbol`](Parser#symbol) except you provide a `Token` to
clearly indicate your custom type of problems:

    comma : Parser Context Problem ()
    comma =
      symbol (Token "," ExpectingComma)

-}
symbol : Token x -> Parser c x ()
symbol t =
  token t



-- KEYWORD


{-| Just like [`Parser.keyword`](Parser#keyword) except you provide a `Token`
to clearly indicate your custom type of problems:

    let_ : Parser Context Problem ()
    let_ =
      symbol (Token "let" ExpectingLet)

Note that this would fail to chomp `letter` because of the subsequent
characters. Use `token` if you do not want that last letter check.
-}
keyword : Token x -> Parser c x ()
keyword (Token kwd expecting) =
  let
    progress =
      not (String.isEmpty kwd)
  in
  Parser <| \\s ->
    let
      (newOffset, newRow, newCol) =
        isSubString kwd s.offset s.row s.col s.src
    in
    if newOffset == -1 || 0 <= isSubChar (\\c -> Char.isAlphaNum c || c == '_') newOffset s.src then
      Bad False (fromState s expecting)
    else
      Good progress ()
        { src = s.src
        , offset = newOffset
        , indent = s.indent
        , context = s.context
        , row = newRow
        , col = newCol
        }



-- TOKEN


{-| With the simpler `Parser` module, you could just say `symbol ","` and
parse all the commas you wanted. But now that we have a custom type for our
problems, we actually have to specify that as well. So anywhere you just used
a `String` in the simpler module, you now use a `Token Problem` in the advanced
module:

    type Problem
      = ExpectingComma
      | ExpectingListEnd

    comma : Token Problem
    comma =
      Token "," ExpectingComma

    listEnd : Token Problem
    listEnd =
      Token "]" ExpectingListEnd

You can be creative with your custom type. Maybe you want a lot of detail.
Maybe you want looser categories. It is a custom type. Do what makes sense for
you!
-}
type Token x = Token String x


{-| Just like [`Parser.token`](Parser#token) except you provide a `Token`
specifying your custom type of problems.
-}
token : Token x -> Parser c x ()
token (Token str expecting) =
  let
    progress =
      not (String.isEmpty str)
  in
  Parser <| \\s ->
    let
      (newOffset, newRow, newCol) =
        isSubString str s.offset s.row s.col s.src
    in
    if newOffset == -1 then
      Bad False (fromState s expecting)
    else
      Good progress ()
        { src = s.src
        , offset = newOffset
        , indent = s.indent
        , context = s.context
        , row = newRow
        , col = newCol
        }



-- INT


{-| Just like [`Parser.int`](Parser#int) where you have to handle negation
yourself. The only difference is that you provide a two potential problems:

    int : x -> x -> Parser c x Int
    int expecting invalid =
      number
        { int = Ok identity
        , hex = Err invalid
        , octal = Err invalid
        , binary = Err invalid
        , float = Err invalid
        , invalid = invalid
        , expecting = expecting
        }

You can use problems like `ExpectingInt` and `InvalidNumber`.
-}
int : x -> x -> Parser c x Int
int expecting invalid =
  number
    { int = Ok identity
    , hex = Err invalid
    , octal = Err invalid
    , binary = Err invalid
    , float = Err invalid
    , invalid = invalid
    , expecting = expecting
    }



-- FLOAT


{-| Just like [`Parser.float`](Parser#float) where you have to handle negation
yourself. The only difference is that you provide a two potential problems:

    float : x -> x -> Parser c x Float
    float expecting invalid =
      number
        { int = Ok toFloat
        , hex = Err invalid
        , octal = Err invalid
        , binary = Err invalid
        , float = Ok identity
        , invalid = invalid
        , expecting = expecting
        }

You can use problems like `ExpectingFloat` and `InvalidNumber`.
-}
float : x -> x -> Parser c x Float
float expecting invalid =
  number
    { int = Ok toFloat
    , hex = Err invalid
    , octal = Err invalid
    , binary = Err invalid
    , float = Ok identity
    , invalid = invalid
    , expecting = expecting
    }



-- NUMBER


{-| Just like [`Parser.number`](Parser#number) where you have to handle
negation yourself. The only difference is that you provide all the potential
problems.
-}
number :
    { int : Result x (Int -> a)
    , hex : Result x (Int -> a)
    , octal : Result x (Int -> a)
    , binary : Result x (Int -> a)
    , float : Result x (Float -> a)
    , invalid : x
    , expecting : x
    }
    -> Parser c x a
number c =
    Parser
        (\\s ->
            let
                (String sourceChars) =
                    s.src

                firstChar =
                    Pine_kernel.head (Pine_kernel.skip [ s.offset, sourceChars ])
            in
            if Pine_kernel.equal [ firstChar, '0' ] then
                let
                    zeroOffset =
                        Pine_kernel.int_add [ s.offset, 1 ]

                    secondChar =
                        Pine_kernel.head (Pine_kernel.skip [ zeroOffset, sourceChars ])

                    baseOffset =
                        Pine_kernel.int_add [ zeroOffset, 1 ]
                in
                if Pine_kernel.equal [ secondChar, 'x' ] then
                    finalizeInt c.invalid c.hex baseOffset (consumeBase16 baseOffset s.src) s

                else if Pine_kernel.equal [ secondChar, 'o' ] then
                    finalizeInt c.invalid c.octal baseOffset (consumeBase 8 baseOffset s.src) s

                else if Pine_kernel.equal [ secondChar, 'b' ] then
                    finalizeInt c.invalid c.binary baseOffset (consumeBase 2 baseOffset s.src) s

                else
                    finalizeFloat c.invalid c.expecting c.int c.float ( zeroOffset, 0 ) s

            else
                finalizeFloat c.invalid c.expecting c.int c.float (consumeBase 10 s.offset s.src) s
        )


consumeBase : Int -> Int -> String -> ( Int, Int )
consumeBase base offset string =
    Elm.Kernel.Parser.consumeBase base offset string


consumeBase16 : Int -> String -> ( Int, Int )
consumeBase16 offset string =
    Elm.Kernel.Parser.consumeBase16 offset string


finalizeInt : x -> Result x (Int -> a) -> Int -> ( Int, Int ) -> State c -> PStep c x a
finalizeInt invalid handler startOffset ( endOffset, n ) s =
    case handler of
        Err x ->
            Bad True (fromState s x)

        Ok toValue ->
            if Pine_kernel.equal [ startOffset, endOffset ] then
                Bad
                    (Pine_kernel.negate
                        (Pine_kernel.int_is_sorted_asc [ startOffset, s.offset ])
                    )
                    (fromState s invalid)

            else
                Good True (toValue n) (bumpOffset endOffset s)


bumpOffset : Int -> State c -> State c
bumpOffset newOffset s =
    { src = s.src
    , offset = newOffset
    , indent = s.indent
    , context = s.context
    , row = s.row
    , col = s.col + (newOffset - s.offset)
    }


finalizeFloat : x -> x -> Result x (Int -> a) -> Result x (Float -> a) -> ( Int, Int ) -> State c -> PStep c x a
finalizeFloat invalid expecting intSettings floatSettings intPair s =
    let
        ( intOffset, _ ) =
            intPair

        floatOffset =
            consumeDotAndExp intOffset s.src
    in
    if floatOffset < 0 then
        Bad True (fromInfo s.row (s.col - (floatOffset + s.offset)) invalid s.context)

    else if s.offset == floatOffset then
        Bad False (fromState s expecting)

    else if intOffset == floatOffset then
        finalizeInt invalid intSettings s.offset intPair s

    else
        case floatSettings of
            Err x ->
                Bad True (fromState s invalid)

            Ok toValue ->
                case String.toFloat (String.slice s.offset floatOffset s.src) of
                    Nothing ->
                        Bad True (fromState s invalid)

                    Just n ->
                        Good True (toValue n) (bumpOffset floatOffset s)


--
-- On a failure, returns negative index of problem.
--
consumeDotAndExp : Int -> String -> Int
consumeDotAndExp offset src =
  if isAsciiCode 0x2E {- . -} offset src then
    consumeExp (chompBase10 (offset + 1) src) src
  else
    consumeExp offset src


--
-- On a failure, returns negative index of problem.
--
consumeExp : Int -> String -> Int
consumeExp offset src =
  if isAsciiCode 0x65 {- e -} offset src || isAsciiCode 0x45 {- E -} offset src then
    let
      eOffset = offset + 1

      expOffset =
        if isAsciiCode 0x2B {- + -} eOffset src || isAsciiCode 0x2D {- - -} eOffset src then
          eOffset + 1
        else
          eOffset

      newOffset = chompBase10 expOffset src
    in
    if expOffset == newOffset then
      -newOffset
    else
      newOffset

  else
    offset


chompBase10 : Int -> String -> Int
chompBase10 offset src =
  Elm.Kernel.Parser.chompBase10 offset src



-- END


{-| Just like [`Parser.end`](Parser#end) except you provide the problem that
arises when the parser is not at the end of the input.
-}
end : x -> Parser c x ()
end x =
  Parser <| \\s ->
    if String.length s.src == s.offset then
      Good False () s
    else
      Bad False (fromState s x)



-- CHOMPED STRINGS


{-| Just like [`Parser.getChompedString`](Parser#getChompedString)
-}
getChompedString : Parser c x a -> Parser c x String
getChompedString parser =
  mapChompedString always parser


{-| Just like [`Parser.mapChompedString`](Parser#mapChompedString)
-}
mapChompedString : (String -> a -> b) -> Parser c x a -> Parser c x b
mapChompedString func (Parser parse) =
  Parser <| \\s0 ->
    case parse s0 of
      Bad p x ->
        Bad p x

      Good p a s1 ->
        Good p (func (String.slice s0.offset s1.offset s0.src) a) s1



-- CHOMP IF


{-| Just like [`Parser.chompIf`](Parser#chompIf) except you provide a problem
in case a character cannot be chomped.
-}
chompIf : (Char -> Bool) -> x -> Parser c x ()
chompIf isGood expecting =
  Parser <| \\s ->
    let
      newOffset = isSubChar isGood s.offset s.src
    in
    -- not found
    if newOffset == -1 then
      Bad False (fromState s expecting)

    -- newline
    else if newOffset == -2 then
      Good True ()
        { src = s.src
        , offset = s.offset + 1
        , indent = s.indent
        , context = s.context
        , row = s.row + 1
        , col = 1
        }

    -- found
    else
      Good True ()
        { src = s.src
        , offset = newOffset
        , indent = s.indent
        , context = s.context
        , row = s.row
        , col = s.col + 1
        }



-- CHOMP WHILE


{-| Just like [`Parser.chompWhile`](Parser#chompWhile)
-}
chompWhile : (Char -> Bool) -> Parser c x ()
chompWhile isGood =
  Parser <| \\s ->
    chompWhileHelp isGood s.offset s.row s.col s


chompWhileHelp : (Char -> Bool) -> Int -> Int -> Int -> State c -> PStep c x ()
chompWhileHelp isGood offset row col s0 =
  let
    newOffset = isSubChar isGood offset s0.src
  in
  -- no match
  if newOffset == -1 then
    Good (s0.offset < offset) ()
      { src = s0.src
      , offset = offset
      , indent = s0.indent
      , context = s0.context
      , row = row
      , col = col
      }

  -- matched a newline
  else if newOffset == -2 then
    chompWhileHelp isGood (offset + 1) (row + 1) 1 s0

  -- normal match
  else
    chompWhileHelp isGood newOffset row (col + 1) s0



-- CHOMP UNTIL


{-| Just like [`Parser.chompUntil`](Parser#chompUntil) except you provide a
`Token` in case you chomp all the way to the end of the input without finding
what you need.
-}
chompUntil : Token x -> Parser c x ()
chompUntil (Token str expecting) =
    Parser
        (\\s ->
            let
                ( newOffset, newRow, newCol ) =
                    Elm.Kernel.Parser.findSubString str s.offset s.row s.col s.src
            in
            if Pine_kernel.equal [ newOffset, -1 ] then
                Bad False (fromInfo newRow newCol expecting s.context)

            else
                Good
                    (Pine_kernel.negate
                        (Pine_kernel.int_is_sorted_asc [ newOffset, s.offset ])
                    )
                    ()
                    { src = s.src
                    , offset = newOffset
                    , indent = s.indent
                    , context = s.context
                    , row = newRow
                    , col = newCol
                    }
        )


{-| Just like [`Parser.chompUntilEndOr`](Parser#chompUntilEndOr)
-}
chompUntilEndOr : String -> Parser c x ()
chompUntilEndOr str =
  Parser <| \\s ->
    let
      (newOffset, newRow, newCol) =
        Elm.Kernel.Parser.findSubString str s.offset s.row s.col s.src

      adjustedOffset =
        if newOffset < 0 then String.length s.src else newOffset
    in
    Good (s.offset < adjustedOffset) ()
      { src = s.src
      , offset = adjustedOffset
      , indent = s.indent
      , context = s.context
      , row = newRow
      , col = newCol
      }



-- CONTEXT


{-| This is how you mark that you are in a certain context. For example, here
is a rough outline of some code that uses `inContext` to mark when you are
parsing a specific definition:

    import Char
    import Parser.Advanced exposing (..)
    import Set

    type Context
      = Definition String
      | List

    definition : Parser Context Problem Expr
    definition =
      functionName
        |> andThen definitionBody

    definitionBody : String -> Parser Context Problem Expr
    definitionBody name =
      inContext (Definition name) <|
        succeed (Function name)
          |= arguments
          |. symbol (Token "=" ExpectingEquals)
          |= expression

    functionName : Parser c Problem String
    functionName =
      variable
        { start = Char.isLower
        , inner = Char.isAlphaNum
        , reserved = Set.fromList ["let","in"]
        , expecting = ExpectingFunctionName
        }

First we parse the function name, and then we parse the rest of the definition.
Importantly, we call `inContext` so that any dead end that occurs in
`definitionBody` will get this extra context information. That way you can say
things like, “I was expecting an equals sign in the `view` definition.” Context!
-}
inContext : context -> Parser context x a -> Parser context x a
inContext context (Parser parse) =
  Parser <| \\s0 ->
    case parse (changeContext (Located s0.row s0.col context :: s0.context) s0) of
      Good p a s1 ->
        Good p a (changeContext s0.context s1)

      Bad _ _ as step ->
        step


changeContext : List (Located c) -> State c -> State c
changeContext newContext s =
  { src = s.src
  , offset = s.offset
  , indent = s.indent
  , context = newContext
  , row = s.row
  , col = s.col
  }



-- INDENTATION


{-| Just like [`Parser.getIndent`](Parser#getIndent)
-}
getIndent : Parser c x Int
getIndent =
  Parser <| \\s -> Good False s.indent s


{-| Just like [`Parser.withIndent`](Parser#withIndent)
-}
withIndent : Int -> Parser c x a -> Parser c x a
withIndent newIndent (Parser parse) =
  Parser <| \\s0 ->
    case parse (changeIndent newIndent s0) of
      Good p a s1 ->
        Good p a (changeIndent s0.indent s1)

      Bad p x ->
        Bad p x


changeIndent : Int -> State c -> State c
changeIndent newIndent s =
  { src = s.src
  , offset = s.offset
  , indent = newIndent
  , context = s.context
  , row = s.row
  , col = s.col
  }



-- POSITION


{-| Just like [`Parser.getPosition`](Parser#getPosition)
-}
getPosition : Parser c x (Int, Int)
getPosition =
  Parser <| \\s -> Good False (s.row, s.col) s


{-| Just like [`Parser.getRow`](Parser#getRow)
-}
getRow : Parser c x Int
getRow =
  Parser <| \\s -> Good False s.row s


{-| Just like [`Parser.getCol`](Parser#getCol)
-}
getCol : Parser c x Int
getCol =
  Parser <| \\s -> Good False s.col s


{-| Just like [`Parser.getOffset`](Parser#getOffset)
-}
getOffset : Parser c x Int
getOffset =
  Parser <| \\s -> Good False s.offset s


{-| Just like [`Parser.getSource`](Parser#getSource)
-}
getSource : Parser c x String
getSource =
  Parser <| \\s -> Good False s.src s



-- LOW-LEVEL HELPERS


{-| When making a fast parser, you want to avoid allocation as much as
possible. That means you never want to mess with the source string, only
keep track of an offset into that string.

You use `isSubString` like this:

    isSubString "let" offset row col "let x = 4 in x"
        --==> ( newOffset, newRow, newCol )

You are looking for `"let"` at a given `offset`. On failure, the
`newOffset` is `-1`. On success, the `newOffset` is the new offset. With
our `"let"` example, it would be `offset + 3`.

You also provide the current `row` and `col` which do not align with
`offset` in a clean way. For example, when you see a `\\n` you are at
`row = row + 1` and `col = 1`. Furthermore, some UTF16 characters are
two words wide, so even if there are no newlines, `offset` and `col`
may not be equal.
-}
isSubString : String -> Int -> Int -> Int -> String -> (Int, Int, Int)
isSubString =
  Elm.Kernel.Parser.isSubString


{-| Again, when parsing, you want to allocate as little as possible.
So this function lets you say:

    isSubChar isSpace offset "this is the source string"
        --==> newOffset

The `(Char -> Bool)` argument is called a predicate.
The `newOffset` value can be a few different things:

  - `-1` means that the predicate failed
  - `-2` means the predicate succeeded with a `\\n`
  - otherwise you will get `offset + 1` or `offset + 2`
    depending on whether the UTF16 character is one or two
    words wide.
-}
isSubChar : (Char -> Bool) -> Int -> String -> Int
isSubChar =
  Elm.Kernel.Parser.isSubChar


{-| Check an offset in the string. Is it equal to the given Char? Are they
both ASCII characters?
-}
isAsciiCode : Int -> Int -> String -> Bool
isAsciiCode =
  Elm.Kernel.Parser.isAsciiCode


{-| Find a substring after a given offset.

    findSubString "42" offset row col "Is 42 the answer?"
        --==> (newOffset, newRow, newCol)

If `offset = 0` we would get `(3, 1, 4)`
If `offset = 7` we would get `(-1, 1, 18)`
-}
findSubString : String -> Int -> Int -> Int -> String -> (Int, Int, Int)
findSubString =
  Elm.Kernel.Parser.findSubString



-- VARIABLES


{-| Just like [`Parser.variable`](Parser#variable) except you specify the
problem yourself.
-}
variable :
  { start : Char -> Bool
  , inner : Char -> Bool
  , reserved : Set.Set String
  , expecting : x
  }
  -> Parser c x String
variable i =
  Parser <| \\s ->
    let
      firstOffset =
        isSubChar i.start s.offset s.src
    in
    if firstOffset == -1 then
      Bad False (fromState s i.expecting)
    else
      let
        s1 =
          if firstOffset == -2 then
            varHelp i.inner (s.offset + 1) (s.row + 1) 1 s.src s.indent s.context
          else
            varHelp i.inner firstOffset s.row (s.col + 1) s.src s.indent s.context

        name =
          String.slice s.offset s1.offset s.src
      in
      if Set.member name i.reserved then
        Bad False (fromState s i.expecting)
      else
        Good True name s1


varHelp : (Char -> Bool) -> Int -> Int -> Int -> String -> Int -> List (Located c) -> State c
varHelp isGood offset row col src indent context =
  let
    newOffset = isSubChar isGood offset src
  in
  if newOffset == -1 then
    { src = src
    , offset = offset
    , indent = indent
    , context = context
    , row = row
    , col = col
    }

  else if newOffset == -2 then
    varHelp isGood (offset + 1) (row + 1) 1 src indent context

  else
    varHelp isGood newOffset row (col + 1) src indent context



-- SEQUENCES


{-| Just like [`Parser.sequence`](Parser#sequence) except with a `Token` for
the start, separator, and end. That way you can specify your custom type of
problem for when something is not found.
-}
sequence
  : { start : Token x
    , separator : Token x
    , end : Token x
    , spaces : Parser c x ()
    , item : Parser c x a
    , trailing : Trailing
    }
  -> Parser c x (List a)
sequence i =
  skip (token i.start) <|
  skip i.spaces <|
    sequenceEnd (token i.end) i.spaces i.item (token i.separator) i.trailing


{-| What’s the deal with trailing commas? Are they `Forbidden`?
Are they `Optional`? Are they `Mandatory`? Welcome to [shapes
club](https://poorlydrawnlines.com/comic/shapes-club/)!
-}
type Trailing = Forbidden | Optional | Mandatory


skip : Parser c x ignore -> Parser c x keep -> Parser c x keep
skip iParser kParser =
  map2 revAlways iParser kParser


revAlways : a -> b -> b
revAlways _ b =
  b


sequenceEnd : Parser c x () -> Parser c x () -> Parser c x a -> Parser c x () -> Trailing -> Parser c x (List a)
sequenceEnd ender ws parseItem sep trailing =
  let
    chompRest item =
      case trailing of
        Forbidden ->
          loop [item] (sequenceEndForbidden ender ws parseItem sep)

        Optional ->
          loop [item] (sequenceEndOptional ender ws parseItem sep)

        Mandatory ->
          ignorer
            ( skip ws <| skip sep <| skip ws <|
                loop [item] (sequenceEndMandatory ws parseItem sep)
            )
            ender
  in
  oneOf
    [ parseItem |> andThen chompRest
    , ender |> map (\\_ -> [])
    ]


sequenceEndForbidden : Parser c x () -> Parser c x () -> Parser c x a -> Parser c x () -> List a -> Parser c x (Step (List a) (List a))
sequenceEndForbidden ender ws parseItem sep revItems =
  let
    chompRest item =
      sequenceEndForbidden ender ws parseItem sep (item :: revItems)
  in
  skip ws <|
    oneOf
      [ skip sep <| skip ws <| map (\\item -> Loop (item :: revItems)) parseItem
      , ender |> map (\\_ -> Done (List.reverse revItems))
      ]


sequenceEndOptional : Parser c x () -> Parser c x () -> Parser c x a -> Parser c x () -> List a -> Parser c x (Step (List a) (List a))
sequenceEndOptional ender ws parseItem sep revItems =
  let
    parseEnd =
      map (\\_ -> Done (List.reverse revItems)) ender
  in
  skip ws <|
    oneOf
      [ skip sep <| skip ws <|
          oneOf
            [ parseItem |> map (\\item -> Loop (item :: revItems))
            , parseEnd
            ]
      , parseEnd
      ]


sequenceEndMandatory : Parser c x () -> Parser c x a -> Parser c x () -> List a -> Parser c x (Step (List a) (List a))
sequenceEndMandatory ws parseItem sep revItems =
  oneOf
    [ map (\\item -> Loop (item :: revItems)) <|
        ignorer parseItem (ignorer ws (ignorer sep ws))
    , map (\\_ -> Done (List.reverse revItems)) (succeed ())
    ]



-- WHITESPACE


{-| Just like [`Parser.spaces`](Parser#spaces)
-}
spaces : Parser c x ()
spaces =
  chompWhile (\\c -> c == ' ' || c == '\\n' || c == '\\r')


{-| Just like [`Parser.lineComment`](Parser#lineComment) except you provide a
`Token` describing the starting symbol.
-}
lineComment : Token x -> Parser c x ()
lineComment start =
  ignorer (token start) (chompUntilEndOr "\\n")


{-| Just like [`Parser.multiComment`](Parser#multiComment) except with a
`Token` for the open and close symbols.
-}
multiComment : Token x -> Token x -> Nestable -> Parser c x ()
multiComment open close nestable =
  case nestable of
    NotNestable ->
      ignorer (token open) (chompUntil close)

    Nestable ->
      nestableComment open close


{-| Works just like [`Parser.Nestable`](Parser#nestable) to help distinguish
between unnestable `/*` `*/` comments like in JS and nestable `{-` `-}`
comments like in Elm.
-}
type Nestable = NotNestable | Nestable


nestableComment : Token x -> Token x -> Parser c x ()
nestableComment (Token oStr oX as open) (Token cStr cX as close) =
  case String.uncons oStr of
    Nothing ->
      problem oX

    Just (openChar, _) ->
      case String.uncons cStr of
        Nothing ->
          problem cX

        Just (closeChar, _) ->
          let
            isNotRelevant char =
              char /= openChar && char /= closeChar

            chompOpen =
              token open
          in
          ignorer chompOpen (nestableHelp isNotRelevant chompOpen (token close) cX 1)


nestableHelp : (Char -> Bool) -> Parser c x () -> Parser c x () -> x -> Int -> Parser c x ()
nestableHelp isNotRelevant open close expectingClose nestLevel =
  skip (chompWhile isNotRelevant) <|
    oneOf
      [ if nestLevel == 1 then
          close
        else
          close
            |> andThen (\\_ -> nestableHelp isNotRelevant open close expectingClose (nestLevel - 1))
      , open
          |> andThen (\\_ -> nestableHelp isNotRelevant open close expectingClose (nestLevel + 1))
      , chompIf isChar expectingClose
          |> andThen (\\_ -> nestableHelp isNotRelevant open close expectingClose nestLevel)
      ]


isChar : Char -> Bool
isChar char =
  True

"""
    , """
module Parser exposing
  ( Parser, run
  , int, float, number, symbol, keyword, variable, end
  , succeed, (|=), (|.), lazy, andThen, problem
  , oneOf, map, backtrackable, commit, token
  , sequence, Trailing(..), loop, Step(..)
  , spaces, lineComment, multiComment, Nestable(..)
  , getChompedString, chompIf, chompWhile, chompUntil, chompUntilEndOr, mapChompedString
  , DeadEnd, Problem(..), deadEndsToString
  , withIndent, getIndent
  , getPosition, getRow, getCol, getOffset, getSource
  )


{-|

# Parsers
@docs Parser, run

# Building Blocks
@docs int, float, number, symbol, keyword, variable, end

# Pipelines
@docs succeed, (|=), (|.), lazy, andThen, problem

# Branches
@docs oneOf, map, backtrackable, commit, token

# Loops
@docs sequence, Trailing, loop, Step

# Whitespace
@docs spaces, lineComment, multiComment, Nestable

# Chompers
@docs getChompedString, chompIf, chompWhile, chompUntil, chompUntilEndOr, mapChompedString

# Errors
@docs DeadEnd, Problem, deadEndsToString

# Indentation
@docs withIndent, getIndent

# Positions
@docs getPosition, getRow, getCol, getOffset, getSource
-}


import Char
import Parser.Advanced as A exposing ((|=), (|.))
import Set



-- INFIX OPERATORS - see Parser.Advanced for why 5 and 6 were chosen


infix left 5 (|=) = keeper
infix left 6 (|.) = ignorer



-- PARSERS


{-| A `Parser` helps turn a `String` into nicely structured data. For example,
we can [`run`](#run) the [`int`](#int) parser to turn `String` to `Int`:

    run int "123456" == Ok 123456
    run int "3.1415" == Err ...

The cool thing is that you can combine `Parser` values to handle much more
complex scenarios.
-}
type alias Parser a =
  A.Parser Never Problem a



-- RUN


{-| Try a parser. Here are some examples using the [`keyword`](#keyword)
parser:

    run (keyword "true") "true"  == Ok ()
    run (keyword "true") "True"  == Err ...
    run (keyword "true") "false" == Err ...
    run (keyword "true") "true!" == Ok ()

Notice the last case! A `Parser` will chomp as much as possible and not worry
about the rest. Use the [`end`](#end) parser to ensure you made it to the end
of the string!
-}
run : Parser a -> String -> Result (List DeadEnd) a
run parser source =
  case A.run parser source of
    Ok a ->
      Ok a

    Err problems ->
      Err (List.map problemToDeadEnd problems)


problemToDeadEnd : A.DeadEnd Never Problem -> DeadEnd
problemToDeadEnd p =
  DeadEnd p.row p.col p.problem



-- PROBLEMS


{-| A parser can run into situations where there is no way to make progress.
When that happens, I record the `row` and `col` where you got stuck and the
particular `problem` you ran into. That is a `DeadEnd`!

**Note:** I count rows and columns like a text editor. The beginning is `row=1`
and `col=1`. As I chomp characters, the `col` increments. When I reach a `\\n`
character, I increment the `row` and set `col=1`.
-}
type alias DeadEnd =
  { row : Int
  , col : Int
  , problem : Problem
  }


{-| When you run into a `DeadEnd`, I record some information about why you
got stuck. This data is useful for producing helpful error messages. This is
how [`deadEndsToString`](#deadEndsToString) works!

**Note:** If you feel limited by this type (i.e. having to represent custom
problems as strings) I highly recommend switching to `Parser.Advanced`. It
lets you define your own `Problem` type. It can also track "context" which
can improve error messages a ton! This is how the Elm compiler produces
relatively nice parse errors, and I am excited to see those techniques applied
elsewhere!
-}
type Problem
  = Expecting String
  | ExpectingInt
  | ExpectingHex
  | ExpectingOctal
  | ExpectingBinary
  | ExpectingFloat
  | ExpectingNumber
  | ExpectingVariable
  | ExpectingSymbol String
  | ExpectingKeyword String
  | ExpectingEnd
  | UnexpectedChar
  | Problem String
  | BadRepeat


{-| Turn all the `DeadEnd` data into a string that is easier for people to
read.

**Note:** This is just a baseline of quality. It cannot do anything with colors.
It is not interactivite. It just turns the raw data into strings. I really hope
folks will check out the source code for some inspiration on how to turn errors
into `Html` with nice colors and interaction! The `Parser.Advanced` module lets
you work with context as well, which really unlocks another level of quality!
The "context" technique is how the Elm compiler can say "I think I am parsing a
list, so I was expecting a closing `]` here." Telling users what the parser
_thinks_ is happening can be really helpful!
-}
deadEndsToString : List DeadEnd -> String
deadEndsToString deadEnds =
  "TODO deadEndsToString"



-- PIPELINES


{-| A parser that succeeds without chomping any characters.

    run (succeed 90210  ) "mississippi" == Ok 90210
    run (succeed 3.141  ) "mississippi" == Ok 3.141
    run (succeed ()     ) "mississippi" == Ok ()
    run (succeed Nothing) "mississippi" == Ok Nothing

Seems weird on its own, but it is very useful in combination with other
functions. The docs for [`(|=)`](#|=) and [`andThen`](#andThen) have some neat
examples.
-}
succeed : a -> Parser a
succeed =
  A.succeed


{-| **Keep** values in a parser pipeline. For example, we could say:

    type alias Point = { x : Float, y : Float }

    point : Parser Point
    point =
      succeed Point
        |. symbol "("
        |. spaces
        |= float
        |. spaces
        |. symbol ","
        |. spaces
        |= float
        |. spaces
        |. symbol ")"

All the parsers in this pipeline will chomp characters and produce values. So
`symbol "("` will chomp one paren and produce a `()` value. Similarly, `float`
will chomp some digits and produce a `Float` value. The `(|.)` and `(|=)`
operators just decide whether we give the values to the `Point` function.

So in this case, we skip the `()` from `symbol "("`, we skip the `()` from
`spaces`, we keep the `Float` from `float`, etc.
-}
keeper : Parser (a -> b) -> Parser a -> Parser b
keeper parseFunc =
    {-
    Since compiler currently fails to compile the original form, inline here as a workaround.
    (|=)
    -}
    A.keeper parseFunc


{-| **Skip** values in a parser pipeline. For example, maybe we want to parse
some JavaScript variables:

    var : Parser String
    var =
      getChompedString <|
        succeed ()
          |. chompIf isStartChar
          |. chompWhile isInnerChar

    isStartChar : Char -> Bool
    isStartChar char =
      Char.isAlpha char || char == '_' || char == '$'

    isInnerChar : Char -> Bool
    isInnerChar char =
      isStartChar char || Char.isDigit char

`chompIf isStartChar` can chomp one character and produce a `()` value.
`chompWhile isInnerChar` can chomp zero or more characters and produce a `()`
value. The `(|.)` operators are saying to still chomp all the characters, but
skip the two `()` values that get produced. No one cares about them.
-}
ignorer : Parser keep -> Parser ignore -> Parser keep
ignorer keepParser ignoreParser =
    {-
    Since compiler currently fails to compile the original form, inline here as a workaround.
    (|.)
    -}
    A.ignorer keepParser ignoreParser


{-| Helper to define recursive parsers. Say we want a parser for simple
boolean expressions:

    true
    false
    (true || false)
    (true || (true || false))

Notice that a boolean expression might contain *other* boolean expressions.
That means we will want to define our parser in terms of itself:

    type Boolean
      = MyTrue
      | MyFalse
      | MyOr Boolean Boolean

    boolean : Parser Boolean
    boolean =
      oneOf
        [ succeed MyTrue
            |. keyword "true"
        , succeed MyFalse
            |. keyword "false"
        , succeed MyOr
            |. symbol "("
            |. spaces
            |= lazy (\\_ -> boolean)
            |. spaces
            |. symbol "||"
            |. spaces
            |= lazy (\\_ -> boolean)
            |. spaces
            |. symbol ")"
        ]

**Notice that `boolean` uses `boolean` in its definition!** In Elm, you can
only define a value in terms of itself it is behind a function call. So
`lazy` helps us define these self-referential parsers. (`andThen` can be used
for this as well!)
-}
lazy : (() -> Parser a) -> Parser a
lazy =
  A.lazy


{-| Parse one thing `andThen` parse another thing. This is useful when you want
to check on what you just parsed. For example, maybe you want U.S. zip codes
and `int` is not suitable because it does not allow leading zeros. You could
say:

    zipCode : Parser String
    zipCode =
      getChompedString (chompWhile Char.isDigit)
        |> andThen checkZipCode

    checkZipCode : String -> Parser String
    checkZipCode code =
      if String.length code == 5 then
        succeed code
      else
        problem "a U.S. zip code has exactly 5 digits"

First we chomp digits `andThen` we check if it is a valid U.S. zip code. We
`succeed` if it has exactly five digits and report a `problem` if not.

Check out [`examples/DoubleQuoteString.elm`](https://github.com/elm/parser/blob/master/examples/DoubleQuoteString.elm)
for another example, this time using `andThen` to verify unicode code points.

**Note:** If you are using `andThen` recursively and blowing the stack, check
out the [`loop`](#loop) function to limit stack usage.
-}
andThen : (a -> Parser b) -> Parser a -> Parser b
andThen =
  A.andThen


{-| Indicate that a parser has reached a dead end. "Everything was going fine
until I ran into this problem." Check out the [`andThen`](#andThen) docs to see
an example usage.
-}
problem : String -> Parser a
problem msg =
  A.problem (Problem msg)



-- BACKTRACKING


{-| If you are parsing JSON, the values can be strings, floats, booleans,
arrays, objects, or null. You need a way to pick `oneOf` them! Here is a
sample of what that code might look like:

    type Json
      = Number Float
      | Boolean Bool
      | Null

    json : Parser Json
    json =
      oneOf
        [ map Number float
        , map (\\_ -> Boolean True) (keyword "true")
        , map (\\_ -> Boolean False) (keyword "false")
        , map (\\_ -> Null) keyword "null"
        ]

This parser will keep trying parsers until `oneOf` them starts chomping
characters. Once a path is chosen, it does not come back and try the others.

**Note:** I highly recommend reading [this document][semantics] to learn how
`oneOf` and `backtrackable` interact. It is subtle and important!

[semantics]: https://github.com/elm/parser/blob/master/semantics.md
-}
oneOf : List (Parser a) -> Parser a
oneOf =
  A.oneOf


{-| Transform the result of a parser. Maybe you have a value that is
an integer or `null`:

    nullOrInt : Parser (Maybe Int)
    nullOrInt =
      oneOf
        [ map Just int
        , map (\\_ -> Nothing) (keyword "null")
        ]

    -- run nullOrInt "0"    == Ok (Just 0)
    -- run nullOrInt "13"   == Ok (Just 13)
    -- run nullOrInt "null" == Ok Nothing
    -- run nullOrInt "zero" == Err ...
-}
map : (a -> b) -> Parser a -> Parser b
map =
  A.map


{-| It is quite tricky to use `backtrackable` well! It can be very useful, but
also can degrade performance and error message quality.

Read [this document](https://github.com/elm/parser/blob/master/semantics.md)
to learn how `oneOf`, `backtrackable`, and `commit` work and interact with
each other. It is subtle and important!
-}
backtrackable : Parser a -> Parser a
backtrackable =
  A.backtrackable


{-| `commit` is almost always paired with `backtrackable` in some way, and it
is tricky to use well.

Read [this document](https://github.com/elm/parser/blob/master/semantics.md)
to learn how `oneOf`, `backtrackable`, and `commit` work and interact with
each other. It is subtle and important!
-}
commit : a -> Parser a
commit =
  A.commit



-- TOKEN


{-| Parse exactly the given string, without any regard to what comes next.

A potential pitfall when parsing keywords is getting tricked by variables that
start with a keyword, like `let` in `letters` or `import` in `important`. This
is especially likely if you have a whitespace parser that can consume zero
charcters. So the [`keyword`](#keyword) parser is defined with `token` and a
trick to peek ahead a bit:

    keyword : String -> Parser ()
    keyword kwd =
      succeed identity
        |. backtrackable (token kwd)
        |= oneOf
            [ map (\\_ -> True) (backtrackable (chompIf isVarChar))
            , succeed False
            ]
        |> andThen (checkEnding kwd)

    checkEnding : String -> Bool -> Parser ()
    checkEnding kwd isBadEnding =
      if isBadEnding then
        problem ("expecting the `" ++ kwd ++ "` keyword")
      else
        commit ()

    isVarChar : Char -> Bool
    isVarChar char =
      Char.isAlphaNum char || char == '_'

This definition is specially designed so that (1) if you really see `let` you
commit to that path and (2) if you see `letters` instead you can backtrack and
try other options. If I had just put a `backtrackable` around the whole thing
you would not get (1) anymore.
-}
token : String -> Parser ()
token str =
  A.token (toToken str)


toToken : String -> A.Token Problem
toToken str =
  A.Token str (Expecting str)



-- LOOPS


{-| A parser that can loop indefinitely. This can be helpful when parsing
repeated structures, like a bunch of statements:

    statements : Parser (List Stmt)
    statements =
      loop [] statementsHelp

    statementsHelp : List Stmt -> Parser (Step (List Stmt) (List Stmt))
    statementsHelp revStmts =
      oneOf
        [ succeed (\\stmt -> Loop (stmt :: revStmts))
            |= statement
            |. spaces
            |. symbol ";"
            |. spaces
        , succeed ()
            |> map (\\_ -> Done (List.reverse revStmts))
        ]

    -- statement : Parser Stmt

Notice that the statements are tracked in reverse as we `Loop`, and we reorder
them only once we are `Done`. This is a very common pattern with `loop`!

Check out [`examples/DoubleQuoteString.elm`](https://github.com/elm/parser/blob/master/examples/DoubleQuoteString.elm)
for another example.

**IMPORTANT NOTE:** Parsers like `succeed ()` and `chompWhile Char.isAlpha` can
succeed without consuming any characters. So in some cases you may want to use
[`getOffset`](#getOffset) to ensure that each step actually consumed characters.
Otherwise you could end up in an infinite loop!

**Note:** Anything you can write with `loop`, you can also write as a parser
that chomps some characters `andThen` calls itself with new arguments. The
problem with calling `andThen` recursively is that it grows the stack, so you
cannot do it indefinitely. So `loop` is important because enables tail-call
elimination, allowing you to parse however many repeats you want.
-}
loop : state -> (state -> Parser (Step state a)) -> Parser a
loop state callback =
  A.loop state (\\s -> map toAdvancedStep (callback s))


{-| Decide what steps to take next in your [`loop`](#loop).

If you are `Done`, you give the result of the whole `loop`. If you decide to
`Loop` around again, you give a new state to work from. Maybe you need to add
an item to a list? Or maybe you need to track some information about what you
just saw?

**Note:** It may be helpful to learn about [finite-state machines][fsm] to get
a broader intuition about using `state`. I.e. You may want to create a `type`
that describes four possible states, and then use `Loop` to transition between
them as you consume characters.

[fsm]: https://en.wikipedia.org/wiki/Finite-state_machine
-}
type Step state a
  = Loop state
  | Done a


toAdvancedStep : Step s a -> A.Step s a
toAdvancedStep step =
  case step of
    Loop s -> A.Loop s
    Done a -> A.Done a



-- NUMBERS


{-| Parse integers.

    run int "1"    == Ok 1
    run int "1234" == Ok 1234

    run int "-789" == Err ...
    run int "0123" == Err ...
    run int "1.34" == Err ...
    run int "1e31" == Err ...
    run int "123a" == Err ...
    run int "0x1A" == Err ...

If you want to handle a leading `+` or `-` you should do it with a custom
parser like this:

    myInt : Parser Int
    myInt =
      oneOf
        [ succeed negate
            |. symbol "-"
            |= int
        , int
        ]

**Note:** If you want a parser for both `Int` and `Float` literals, check out
[`number`](#number) below. It will be faster than using `oneOf` to combining
`int` and `float` yourself.
-}
int : Parser Int
int =
  A.int ExpectingInt ExpectingInt


{-| Parse floats.

    run float "123"       == Ok 123
    run float "3.1415"    == Ok 3.1415
    run float "0.1234"    == Ok 0.1234
    run float ".1234"     == Ok 0.1234
    run float "1e-42"     == Ok 1e-42
    run float "6.022e23"  == Ok 6.022e23
    run float "6.022E23"  == Ok 6.022e23
    run float "6.022e+23" == Ok 6.022e23

If you want to disable literals like `.123` (like in Elm) you could write
something like this:

    elmFloat : Parser Float
    elmFloat =
      oneOf
        [ symbol "."
            |. problem "floating point numbers must start with a digit, like 0.25"
        , float
        ]

**Note:** If you want a parser for both `Int` and `Float` literals, check out
[`number`](#number) below. It will be faster than using `oneOf` to combining
`int` and `float` yourself.
-}
float : Parser Float
float =
  A.float ExpectingFloat ExpectingFloat



-- NUMBER


{-| Parse a bunch of different kinds of numbers without backtracking. A parser
for Elm would need to handle integers, floats, and hexadecimal like this:

    type Expr
      = Variable String
      | Int Int
      | Float Float
      | Apply Expr Expr

    elmNumber : Parser Expr
    elmNumber =
      number
        { int = Just Int
        , hex = Just Int    -- 0x001A is allowed
        , octal = Nothing   -- 0o0731 is not
        , binary = Nothing  -- 0b1101 is not
        , float = Just Float
        }

If you wanted to implement the [`float`](#float) parser, it would be like this:

    float : Parser Float
    float =
      number
        { int = Just toFloat
        , hex = Nothing
        , octal = Nothing
        , binary = Nothing
        , float = Just identity
        }

Notice that it actually is processing `int` results! This is because `123`
looks like an integer to me, but maybe it looks like a float to you. If you had
`int = Nothing`, floats would need a decimal like `1.0` in every case. If you
like explicitness, that may actually be preferable!

**Note:** This function does not check for weird trailing characters in the
current implementation, so parsing `123abc` can succeed up to `123` and then
move on. This is helpful for people who want to parse things like `40px` or
`3m`, but it requires a bit of extra code to rule out trailing characters in
other cases.
-}
number
  : { int : Maybe (Int -> a)
    , hex : Maybe (Int -> a)
    , octal : Maybe (Int -> a)
    , binary : Maybe (Int -> a)
    , float : Maybe (Float -> a)
    }
  -> Parser a
number i =
  A.number
    { int = Result.fromMaybe ExpectingInt i.int
    , hex = Result.fromMaybe ExpectingHex i.hex
    , octal = Result.fromMaybe ExpectingOctal i.octal
    , binary = Result.fromMaybe ExpectingBinary i.binary
    , float = Result.fromMaybe ExpectingFloat i.float
    , invalid = ExpectingNumber
    , expecting = ExpectingNumber
    }



-- SYMBOL


{-| Parse symbols like `(` and `,`.

    run (symbol "[") "[" == Ok ()
    run (symbol "[") "4" == Err ... (ExpectingSymbol "[") ...

**Note:** This is good for stuff like brackets and semicolons, but it probably
should not be used for binary operators like `+` and `-` because you can find
yourself in weird situations. For example, is `3--4` a typo? Or is it `3 - -4`?
I have had better luck with `chompWhile isSymbol` and sorting out which
operator it is afterwards.
-}
symbol : String -> Parser ()
symbol str =
  A.symbol (A.Token str (ExpectingSymbol str))



-- KEYWORD


{-| Parse keywords like `let`, `case`, and `type`.

    run (keyword "let") "let"     == Ok ()
    run (keyword "let") "var"     == Err ... (ExpectingKeyword "let") ...
    run (keyword "let") "letters" == Err ... (ExpectingKeyword "let") ...

**Note:** Notice the third case there! `keyword` actually looks ahead one
character to make sure it is not a letter, number, or underscore. The goal is
to help with parsers like this:

    succeed identity
      |. keyword "let"
      |. spaces
      |= elmVar
      |. spaces
      |. symbol "="

The trouble is that `spaces` may chomp zero characters (to handle expressions
like `[1,2]` and `[ 1 , 2 ]`) and in this case, it would mean `letters` could
be parsed as `let ters` and then wonder where the equals sign is! Check out the
[`token`](#token) docs if you need to customize this!
-}
keyword : String -> Parser ()
keyword kwd =
  A.keyword (A.Token kwd (ExpectingKeyword kwd))



-- END


{-| Check if you have reached the end of the string you are parsing.

    justAnInt : Parser Int
    justAnInt =
      succeed identity
        |= int
        |. end

    -- run justAnInt "90210" == Ok 90210
    -- run justAnInt "1 + 2" == Err ...
    -- run int       "1 + 2" == Ok 1

Parsers can succeed without parsing the whole string. Ending your parser
with `end` guarantees that you have successfully parsed the whole string.
-}
end : Parser ()
end =
  A.end ExpectingEnd



-- CHOMPED STRINGS


{-| Sometimes parsers like `int` or `variable` cannot do exactly what you
need. The "chomping" family of functions is meant for that case! Maybe you
need to parse [valid PHP variables][php] like `$x` and `$txt`:

    php : Parser String
    php =
      getChompedString <|
        succeed ()
          |. chompIf (\\c -> c == '$')
          |. chompIf (\\c -> Char.isAlpha c || c == '_')
          |. chompWhile (\\c -> Char.isAlphaNum c || c == '_')

The idea is that you create a bunch of chompers that validate the underlying
characters. Then `getChompedString` extracts the underlying `String` efficiently.

**Note:** Maybe it is helpful to see how you can use [`getOffset`](#getOffset)
and [`getSource`](#getSource) to implement this function:

    getChompedString : Parser a -> Parser String
    getChompedString parser =
      succeed String.slice
        |= getOffset
        |. parser
        |= getOffset
        |= getSource

[php]: https://www.w3schools.com/php/php_variables.asp
-}
getChompedString : Parser a -> Parser String
getChompedString =
  A.getChompedString


{-| This works just like [`getChompedString`](#getChompedString) but gives
a bit more flexibility. For example, maybe you want to parse Elm doc comments
and get (1) the full comment and (2) all of the names listed in the docs.

You could implement `mapChompedString` like this:

    mapChompedString : (String -> a -> b) -> Parser a -> Parser String
    mapChompedString func parser =
      succeed (\\start value end src -> func (String.slice start end src) value)
        |= getOffset
        |= parser
        |= getOffset
        |= getSource

-}
mapChompedString : (String -> a -> b) -> Parser a -> Parser b
mapChompedString =
  A.mapChompedString



{-| Chomp one character if it passes the test.

    chompUpper : Parser ()
    chompUpper =
      chompIf Char.isUpper

So this can chomp a character like `T` and produces a `()` value.
-}
chompIf : (Char -> Bool) -> Parser ()
chompIf isGood =
  A.chompIf isGood UnexpectedChar



{-| Chomp zero or more characters if they pass the test. This is commonly
useful for chomping whitespace or variable names:

    whitespace : Parser ()
    whitespace =
      chompWhile (\\c -> c == ' ' || c == '\\t' || c == '\\n' || c == '\\r')

    elmVar : Parser String
    elmVar =
      getChompedString <|
        succeed ()
          |. chompIf Char.isLower
          |. chompWhile (\\c -> Char.isAlphaNum c || c == '_')

**Note:** a `chompWhile` parser always succeeds! This can lead to tricky
situations, especially if you define your whitespace with it. In that case,
you could accidentally interpret `letx` as the keyword `let` followed by
"spaces" followed by the variable `x`. This is why the `keyword` and `number`
parsers peek ahead, making sure they are not followed by anything unexpected.
-}
chompWhile : (Char -> Bool) -> Parser ()
chompWhile =
  A.chompWhile


{-| Chomp until you see a certain string. You could define C-style multi-line
comments like this:

    comment : Parser ()
    comment =
      symbol "/*"
        |. chompUntil "*/"

I recommend using [`multiComment`](#multiComment) for this particular scenario
though. It can be trickier than it looks!
-}
chompUntil : String -> Parser ()
chompUntil str =
  A.chompUntil (toToken str)


{-| Chomp until you see a certain string or until you run out of characters to
chomp! You could define single-line comments like this:

    elm : Parser ()
    elm =
      symbol "--"
        |. chompUntilEndOr "\\n"

A file may end with a single-line comment, so the file can end before you see
a newline. Tricky!

I recommend just using [`lineComment`](#lineComment) for this particular
scenario.
-}
chompUntilEndOr : String -> Parser ()
chompUntilEndOr =
  A.chompUntilEndOr



-- INDENTATION


{-| Some languages are indentation sensitive. Python cares about tabs. Elm
cares about spaces sometimes. `withIndent` and `getIndent` allow you to manage
"indentation state" yourself, however is necessary in your scenario.
-}
withIndent : Int -> Parser a -> Parser a
withIndent newIndent (A.Parser parse) =
    {-
    Since compiler currently fails to compile the original form, inline A.spaces here as a workaround.
    A.withIndent
    -}

    A.Parser <| \\s0 ->
        case parse (A.changeIndent newIndent s0) of
        A.Good p a s1 ->
            A.Good p a (A.changeIndent s0.indent s1)

        A.Bad p x ->
            A.Bad p x


{-| When someone said `withIndent` earlier, what number did they put in there?

- `getIndent` results in `0`, the default value
- `withIndent 4 getIndent` results in `4`

So you are just asking about things you said earlier. These numbers do not leak
out of `withIndent`, so say we have:

    succeed Tuple.pair
      |= withIndent 4 getIndent
      |= getIndent

Assuming there are no `withIndent` above this, you would get `(4,0)` from this.
-}
getIndent : Parser Int
getIndent =
    {-
    Since compiler currently fails to compile the original form, inline A.getIndent here as a workaround.
    A.getIndent
    -}
    A.Parser <| \\s -> A.Good False s.indent s



-- POSITION


{-| Code editors treat code like a grid, with rows and columns. The start is
`row=1` and `col=1`. As you chomp characters, the `col` increments. When you
run into a `\\n` character, the `row` increments and `col` goes back to `1`.

In the Elm compiler, I track the start and end position of every expression
like this:

    type alias Located a =
      { start : (Int, Int)
      , value : a
      , end : (Int, Int)
      }

    located : Parser a -> Parser (Located a)
    located parser =
      succeed Located
        |= getPosition
        |= parser
        |= getPosition

So if there is a problem during type inference, I use this saved position
information to underline the exact problem!

**Note:** Tabs count as one character, so if you are parsing something like
Python, I recommend sorting that out *after* parsing. So if I wanted the `^^^^`
underline like in Elm, I would find the `row` in the source code and do
something like this:

    makeUnderline : String -> Int -> Int -> String
    makeUnderline row minCol maxCol =
      String.toList row
        |> List.indexedMap (toUnderlineChar minCol maxCol)
        |> String.fromList

    toUnderlineChar : Int -> Int -> Int -> Char -> Char
    toUnderlineChar minCol maxCol col char =
      if minCol <= col && col <= maxCol then
        '^'
      else if char == '\\t' then
        '\\t'
      else
        ' '

So it would preserve any tabs from the source line. There are tons of other
ways to do this though. The point is just that you handle the tabs after
parsing but before anyone looks at the numbers in a context where tabs may
equal 2, 4, or 8.
-}
getPosition : Parser (Int, Int)
getPosition =
    {-
    Since compiler currently fails to compile the original form, inline A.getPosition here as a workaround.
    A.getPosition
    -}
    A.Parser <| \\s -> A.Good False (s.row, s.col) s


{-| This is a more efficient version of `map Tuple.first getPosition`. Maybe
you just want to track the line number for some reason? This lets you do that.

See [`getPosition`](#getPosition) for an explanation of rows and columns.
-}
getRow : Parser Int
getRow =
    {-
    Since compiler currently fails to compile the original form, inline A.getRow here as a workaround.
    A.getRow
    -}
    A.Parser <| \\s -> A.Good False s.row s


{-| This is a more efficient version of `map Tuple.second getPosition`. This
can be useful in combination with [`withIndent`](#withIndent) and
[`getIndent`](#getIndent), like this:

    checkIndent : Parser ()
    checkIndent =
      succeed (\\indent column -> indent <= column)
        |= getIndent
        |= getCol
        |> andThen checkIndentHelp

    checkIndentHelp : Bool -> Parser ()
    checkIndentHelp isIndented =
      if isIndented then
        succeed ()
      else
        problem "expecting more spaces"

So the `checkIndent` parser only succeeds when you are "deeper" than the
current indent level. You could use this to parse Elm-style `let` expressions.
-}
getCol : Parser Int
getCol =
    {-
    Since compiler currently fails to compile the original form, inline A.getCol here as a workaround.
    A.getCol
    -}
    A.Parser <| \\s -> A.Good False s.col s


{-| Editors think of code as a grid, but behind the scenes it is just a flat
array of UTF-16 characters. `getOffset` tells you your index in that flat
array. So if you chomp `"\\n\\n\\n\\n"` you are on row 5, column 1, and offset 4.

**Note:** JavaScript uses a somewhat odd version of UTF-16 strings, so a single
character may take two slots. So in JavaScript, `'abc'.length === 3` but
`'🙈🙉🙊'.length === 6`. Try it out! And since Elm runs in JavaScript, the offset
moves by those rules.
-}
getOffset : Parser Int
getOffset =
    {-
    Since compiler currently fails to compile the original form, inline A.getOffset here as a workaround.
    A.getOffset
    -}
    A.Parser <| \\s -> A.Good False s.offset s


{-| Get the full string that is being parsed. You could use this to define
`getChompedString` or `mapChompedString` if you wanted:

    getChompedString : Parser a -> Parser String
    getChompedString parser =
      succeed String.slice
        |= getOffset
        |. parser
        |= getOffset
        |= getSource
-}
getSource : Parser String
getSource =
    {-
    Since compiler currently fails to compile the original form, inline A.getSource here as a workaround.
    A.getSource
    -}
    A.Parser <| \\s -> A.Good False s.src s



-- VARIABLES


{-| Create a parser for variables. If we wanted to parse type variables in Elm,
we could try something like this:

    import Char
    import Parser exposing (..)
    import Set

    typeVar : Parser String
    typeVar =
      variable
        { start = Char.isLower
        , inner = \\c -> Char.isAlphaNum c || c == '_'
        , reserved = Set.fromList [ "let", "in", "case", "of" ]
        }

This is saying it _must_ start with a lower-case character. After that,
characters can be letters, numbers, or underscores. It is also saying that if
you run into any of these reserved names, it is definitely not a variable.
-}
variable :
  { start : Char -> Bool
  , inner : Char -> Bool
  , reserved : Set.Set String
  }
  -> Parser String
variable i =
  A.variable
    { start = i.start
    , inner = i.inner
    , reserved = i.reserved
    , expecting = ExpectingVariable
    }



-- SEQUENCES


{-| Handle things like lists and records, but you can customize the details
however you need. Say you want to parse C-style code blocks:

    import Parser exposing (Parser, Trailing(..))

    block : Parser (List Stmt)
    block =
      Parser.sequence
        { start = "{"
        , separator = ";"
        , end = "}"
        , spaces = spaces
        , item = statement
        , trailing = Mandatory -- demand a trailing semi-colon
        }

    -- statement : Parser Stmt

**Note:** If you need something more custom, do not be afraid to check
out the implementation and customize it for your case. It is better to
get nice error messages with a lower-level implementation than to try
to hack high-level parsers to do things they are not made for.
-}
sequence
  : { start : String
    , separator : String
    , end : String
    , spaces : Parser ()
    , item : Parser a
    , trailing : Trailing
    }
  -> Parser (List a)
sequence i =
  A.sequence
    { start = toToken i.start
    , separator = toToken i.separator
    , end = toToken i.end
    , spaces = i.spaces
    , item = i.item
    , trailing = toAdvancedTrailing i.trailing
    }


{-| What’s the deal with trailing commas? Are they `Forbidden`?
Are they `Optional`? Are they `Mandatory`? Welcome to [shapes
club](https://poorlydrawnlines.com/comic/shapes-club/)!
-}
type Trailing = Forbidden | Optional | Mandatory


toAdvancedTrailing : Trailing -> A.Trailing
toAdvancedTrailing trailing =
  case trailing of
    Forbidden -> A.Forbidden
    Optional -> A.Optional
    Mandatory -> A.Mandatory



-- WHITESPACE


{-| Parse zero or more `' '`, `'\\n'`, and `'\\r'` characters.

The implementation is pretty simple:

    spaces : Parser ()
    spaces =
      chompWhile (\\c -> c == ' ' || c == '\\n' || c == '\\r')

So if you need something different (like tabs) just define an alternative with
the necessary tweaks! Check out [`lineComment`](#lineComment) and
[`multiComment`](#multiComment) for more complex situations.
-}
spaces : Parser ()
spaces =
    {-
    Since compiler currently fails to compile the original form, inline A.spaces here as a workaround.
    A.spaces
    -}
    A.chompWhile (\\c -> c == ' ' || c == '\\n' || c == '\\r')


{-| Parse single-line comments:

    elm : Parser ()
    elm =
      lineComment "--"

    js : Parser ()
    js =
      lineComment "//"

    python : Parser ()
    python =
      lineComment "#"

This parser is defined like this:

    lineComment : String -> Parser ()
    lineComment str =
      symbol str
        |. chompUntilEndOr "\\n"

So it will consume the remainder of the line. If the file ends before you see
a newline, that is fine too.
-}
lineComment : String -> Parser ()
lineComment str =
  A.lineComment (toToken str)


{-| Parse multi-line comments. So if you wanted to parse Elm whitespace or
JS whitespace, you could say:

    elm : Parser ()
    elm =
      loop 0 <| ifProgress <|
        oneOf
          [ lineComment "--"
          , multiComment "{-" "-}" Nestable
          , spaces
          ]

    js : Parser ()
    js =
      loop 0 <| ifProgress <|
        oneOf
          [ lineComment "//"
          , multiComment "/*" "*/" NotNestable
          , chompWhile (\\c -> c == ' ' || c == '\\n' || c == '\\r' || c == '\\t')
          ]

    ifProgress : Parser a -> Int -> Parser (Step Int ())
    ifProgress parser offset =
      succeed identity
        |. parser
        |= getOffset
        |> map (\\newOffset -> if offset == newOffset then Done () else Loop newOffset)

**Note:** The fact that `spaces` comes last in the definition of `elm` is very
important! It can succeed without consuming any characters, so if it were the
first option, it would always succeed and bypass the others! (Same is true of
`chompWhile` in `js`.) This possibility of success without consumption is also
why wee need the `ifProgress` helper. It detects if there is no more whitespace
to consume.
-}
multiComment : String -> String -> Nestable -> Parser ()
multiComment open close nestable =
  A.multiComment (toToken open) (toToken close) (toAdvancedNestable nestable)


{-| Not all languages handle multi-line comments the same. Multi-line comments
in C-style syntax are `NotNestable`, meaning they can be implemented like this:

    js : Parser ()
    js =
      symbol "/*"
        |. chompUntil "*/"

In fact, `multiComment "/*" "*/" NotNestable` *is* implemented like that! It is
very simple, but it does not allow you to nest comments like this:

```javascript
/*
line1
/* line2 */
line3
*/
```

It would stop on the first `*/`, eventually throwing a syntax error on the
second `*/`. This can be pretty annoying in long files.

Languages like Elm allow you to nest multi-line comments, but your parser needs
to be a bit fancier to handle this. After you start a comment, you have to
detect if there is another one inside it! And then you have to make sure all
the `{-` and `-}` match up properly! Saying `multiComment "{-" "-}" Nestable`
does all that for you.
-}
type Nestable = NotNestable | Nestable


toAdvancedNestable : Nestable -> A.Nestable
toAdvancedNestable nestable =
  case nestable of
    NotNestable -> A.NotNestable
    Nestable -> A.Nestable

"""
    ]
