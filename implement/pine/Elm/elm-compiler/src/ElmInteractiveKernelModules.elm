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


consumeBase : Int -> Int -> List Char -> ( Int, Int )
consumeBase base offset chars =
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


consumeBase16 : Int -> List Char -> ( Int, Int )
consumeBase16 offset chars =
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


chompBase10 : Int -> List Char -> Int
chompBase10 offset chars =
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


isSubString : String -> Int -> Int -> Int -> List Char -> ( Int, Int, Int )
isSubString (String smallChars) offset row col bigChars =
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
                countOffsetsInString ( 0, 0, 0 ) ( smallChars, expectedLength )

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


isSubChar : (Char -> Bool) -> Int -> List Char -> Int
isSubChar predicate offset chars =
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


findSubString : String -> Int -> Int -> Int -> List Char -> ( Int, Int, Int )
findSubString (String smallChars) offset row col bigChars =
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

        ( newlineCount, colShift ) =
            countOffsetsInString ( offset, 0, 0 ) ( bigChars, newOffset )

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


chompWhileHelp : (Char -> Bool) -> ( Int, Int, Int ) -> List Char -> ( Int, Int, Int )
chompWhileHelp isGood ( offset, row, col ) srcChars =
    let
        nextChar =
            Pine_kernel.head (Pine_kernel.skip [ offset, srcChars ])
    in
    if isGood nextChar then
        if Pine_kernel.equal [ nextChar, '\\n' ] then
            -- matched a newline
            chompWhileHelp
                isGood
                ( Pine_kernel.int_add [ offset, 1 ]
                , Pine_kernel.int_add [ row, 1 ]
                , 1
                )
                srcChars

        else
            -- normal match
            chompWhileHelp
                isGood
                ( Pine_kernel.int_add [ offset, 1 ]
                , row
                , Pine_kernel.int_add [ col, 1 ]
                )
                srcChars

    else
        -- no match
        ( offset
        , row
        , col
        )


countOffsetsInString : ( Int, Int, Int ) -> ( List Char, Int ) -> ( Int, Int )
countOffsetsInString ( offset, newlines, col ) ( chars, end ) =
    let
        currentChar =
            Pine_kernel.head
                (Pine_kernel.skip [ offset, chars ])

        nextOffset =
            Pine_kernel.int_add [ offset, 1 ]
    in
    if Pine_kernel.equal [ currentChar, [] ] then
        ( newlines, col )

    else if Pine_kernel.int_is_sorted_asc [ end, offset ] then
        ( newlines, col )

    else if Pine_kernel.equal [ currentChar, '\\n' ] then
        countOffsetsInString
            ( nextOffset, Pine_kernel.int_add [ newlines, 1 ], 0 )
            ( chars, end )

    else
        countOffsetsInString
            ( nextOffset, newlines, Pine_kernel.int_add [ col, 1 ] )
            ( chars, end )


newlineChar : Char
newlineChar =
    -- ASCII code for '\\n'
    Pine_kernel.skip [ 1, 10 ]


isAsciiCode : Int -> Int -> List Char -> Bool
isAsciiCode code offset chars =
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
    , changeIndent, ignorer, keeper
    )

{-|


# Parsers

@docs Parser, run, DeadEnd, inContext, Token


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


infix left  5 (|=) = keeper
infix left  6 (|.) = ignorer



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

    import Parser.Advanced

    type alias MyParser a =
        Parser.Advanced.Parser Context Problem a

    type Context
        = Definition String
        | List
        | Record

    type Problem
        = BadIndent
        | BadKeyword String

All of the functions from `Parser` should exist in `Parser.Advanced` in some
form, allowing you to switch over pretty easily.

[parser]: /packages/elm/parser/latest/Parser

-}
type Parser context problem value
    = Parser (State context -> PStep context problem value)


type String
    = String (List Char.Char)


type PStep context problem value
    = Good Bool value (State context)
    | Bad Bool (Bag context problem)


type State context
    = PState (List Char) Int Int (List (Located context)) Int Int


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
run (Parser parse) (String srcChars) =
    case
        parse
            (PState
                srcChars
                0
                1
                []
                1
                1
            )
    of
        Good _ value _ ->
            Ok value

        Bad _ bag ->
            Err (bagToList bag [])



-- PROBLEMS


{-| Say you are parsing a function named `viewHealthData` that contains a list.
You might get a `DeadEnd` like this:

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
fromState (PState srcChars offset indent context row col) x =
    AddRight Empty (DeadEnd row col x context)


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
    Parser
        (\\s ->
            Good False a s
        )


{-| Just like [`Parser.problem`](Parser#problem) except you provide a custom
type for your problem.
-}
problem : x -> Parser c x a
problem x =
    Parser
        (\\s ->
            Bad False (fromState s x)
        )



-- MAPPING


{-| Just like [`Parser.map`](Parser#map)
-}
map : (a -> b) -> Parser c x a -> Parser c x b
map func (Parser parse) =
    Parser
        (\\s0 ->
            case parse s0 of
                Good p a s1 ->
                    Good p (func a) s1

                Bad p x ->
                    Bad p x
        )


map2 : (a -> b -> value) -> Parser c x a -> Parser c x b -> Parser c x value
map2 func (Parser parseA) (Parser parseB) =
    Parser
        (\\s0 ->
            case parseA s0 of
                Bad p x ->
                    Bad p x

                Good p1 a s1 ->
                    case parseB s1 of
                        Bad p2 x ->
                            Bad (p1 || p2) x

                        Good p2 b s2 ->
                            Good (p1 || p2) (func a b) s2
        )


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
    Parser
        (\\s0 ->
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
        )



-- LAZY


{-| Just like [`Parser.lazy`](Parser#lazy)
-}
lazy : (() -> Parser c x a) -> Parser c x a
lazy thunk =
    Parser
        (\\s ->
            let
                (Parser parse) =
                    thunk ()
            in
            parse s
        )



-- ONE OF


{-| Just like [`Parser.oneOf`](Parser#oneOf)
-}
oneOf : List (Parser c x a) -> Parser c x a
oneOf parsers =
    Parser
        (\\s -> oneOfHelp s Empty parsers)


oneOfHelp : State c -> Bag c x -> List (Parser c x a) -> PStep c x a
oneOfHelp s0 bag parsers =
    case parsers of
        [] ->
            Bad False bag

        (Parser parse) :: remainingParsers ->
            case parse s0 of
                (Good _ _ _) as step ->
                    step

                (Bad p x) as step ->
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
    Parser
        (\\s ->
            loopHelp False state callback s
        )


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
    Parser
        (\\s0 ->
            case parse s0 of
                Bad _ x ->
                    Bad False x

                Good _ a s1 ->
                    Good False a s1
        )


{-| Just like [`Parser.commit`](Parser#commit)
-}
commit : a -> Parser c x a
commit a =
    Parser (\\s -> Good True a s)



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
            kwd /= ""
    in
    Parser
        (\\s ->
            let
                (PState srcChars sOffset sIndent sContext sRow sCol) =
                    s

                ( newOffset, newRow, newCol ) =
                    Elm.Kernel.Parser.isSubString kwd sOffset sRow sCol srcChars
            in
            if Pine_kernel.equal [ newOffset, -1 ] then
                Bad False (fromState s expecting)

            else if
                Pine_kernel.int_is_sorted_asc
                    [ 0
                    , isSubChar
                        (\\c ->
                            Char.isAlphaNum c || Pine_kernel.equal [ c, '_' ]
                        )
                        newOffset
                        srcChars
                    ]
            then
                Bad False (fromState s expecting)

            else
                Good progress
                    ()
                    (PState
                        srcChars
                        newOffset
                        sIndent
                        sContext
                        newRow
                        newCol
                    )
        )



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
type Token x
    = Token String x


{-| Just like [`Parser.token`](Parser#token) except you provide a `Token`
specifying your custom type of problems.
-}
token : Token x -> Parser c x ()
token (Token str expecting) =
    let
        progress =
            str /= ""
    in
    Parser
        (\\s ->
            let
                (PState srcChars sOffset sIndent sContext sRow sCol) =
                    s

                ( newOffset, newRow, newCol ) =
                    Elm.Kernel.Parser.isSubString str sOffset sRow sCol srcChars
            in
            if Pine_kernel.equal [ newOffset, -1 ] then
                Bad False (fromState s expecting)

            else
                Good progress
                    ()
                    (PState
                        srcChars
                        newOffset
                        sIndent
                        sContext
                        newRow
                        newCol
                    )
        )



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
                (PState srcChars sOffset sIndent sContext sRow sCol) =
                    s

                firstChar =
                    Pine_kernel.head (Pine_kernel.skip [ sOffset, srcChars ])
            in
            if Pine_kernel.equal [ firstChar, '0' ] then
                let
                    zeroOffset =
                        Pine_kernel.int_add [ sOffset, 1 ]

                    secondChar =
                        Pine_kernel.head (Pine_kernel.skip [ zeroOffset, srcChars ])

                    baseOffset =
                        Pine_kernel.int_add [ zeroOffset, 1 ]
                in
                if Pine_kernel.equal [ secondChar, 'x' ] then
                    finalizeInt c.invalid c.hex baseOffset (consumeBase16 baseOffset srcChars) s

                else if Pine_kernel.equal [ secondChar, 'o' ] then
                    finalizeInt c.invalid c.octal baseOffset (consumeBase 8 baseOffset srcChars) s

                else if Pine_kernel.equal [ secondChar, 'b' ] then
                    finalizeInt c.invalid c.binary baseOffset (consumeBase 2 baseOffset srcChars) s

                else
                    finalizeFloat c.invalid c.expecting c.int c.float ( zeroOffset, 0 ) s

            else
                finalizeFloat c.invalid c.expecting c.int c.float (consumeBase 10 sOffset srcChars) s
        )


consumeBase : Int -> Int -> List Char -> ( Int, Int )
consumeBase base offset string =
    Elm.Kernel.Parser.consumeBase base offset string


consumeBase16 : Int -> List Char -> ( Int, Int )
consumeBase16 offset string =
    Elm.Kernel.Parser.consumeBase16 offset string


finalizeInt : x -> Result x (Int -> a) -> Int -> ( Int, Int ) -> State c -> PStep c x a
finalizeInt invalid handler startOffset ( endOffset, n ) s =
    case handler of
        Err x ->
            Bad True (fromState s x)

        Ok toValue ->
            let
                (PState srcChars sOffset sIndent sContext sRow sCol) =
                    s
            in
            if Pine_kernel.equal [ startOffset, endOffset ] then
                Bad
                    (Pine_kernel.negate
                        (Pine_kernel.int_is_sorted_asc [ startOffset, sOffset ])
                    )
                    (fromState s invalid)

            else
                Good True (toValue n) (bumpOffset endOffset s)


bumpOffset : Int -> State c -> State c
bumpOffset newOffset (PState srcChars offset indent context row col) =
    PState
        srcChars
        newOffset
        indent
        context
        row
        (Pine_kernel.int_add [ col, newOffset, Pine_kernel.negate offset ])


finalizeFloat : x -> x -> Result x (Int -> a) -> Result x (Float -> a) -> ( Int, Int ) -> State c -> PStep c x a
finalizeFloat invalid expecting intSettings floatSettings intPair s =
    let
        (PState srcChars sOffset sIndent sContext sRow sCol) =
            s

        ( intOffset, _ ) =
            intPair

        floatOffset =
            consumeDotAndExp intOffset srcChars
    in
    if Pine_kernel.int_is_sorted_asc [ 0, floatOffset ] then
        if Pine_kernel.equal [ sOffset, floatOffset ] then
            Bad False (fromState s expecting)

        else if Pine_kernel.equal [ intOffset, floatOffset ] then
            finalizeInt invalid intSettings sOffset intPair s

        else
            case floatSettings of
                Err x ->
                    Bad True (fromState s invalid)

                Ok toValue ->
                    let
                        sliceLength : Int
                        sliceLength =
                            Pine_kernel.int_add [ floatOffset, Pine_kernel.negate sOffset ]

                        sliceChars : List Char
                        sliceChars =
                            Pine_kernel.take
                                [ sliceLength
                                , Pine_kernel.skip
                                    [ sOffset
                                    , srcChars
                                    ]
                                ]
                    in
                    case String.toFloat (String sliceChars) of
                        Nothing ->
                            Bad True (fromState s invalid)

                        Just n ->
                            Good True (toValue n) (bumpOffset floatOffset s)

    else
        Bad True
            (fromInfo
                sRow
                (Pine_kernel.int_add [ sCol, Pine_kernel.negate (Pine_kernel.int_add [ floatOffset, sOffset ]) ])
                invalid
                sContext
            )



--
-- On a failure, returns negative index of problem.
--


consumeDotAndExp : Int -> List Char -> Int
consumeDotAndExp offset chars =
    if Pine_kernel.equal [ Pine_kernel.head (Pine_kernel.skip [ offset, chars ]), '.' ] then
        consumeExp
            (Elm.Kernel.Parser.chompBase10
                (Pine_kernel.int_add [ offset, 1 ])
                chars
            )
            chars

    else
        consumeExp offset chars



--
-- On a failure, returns negative index of problem.
--


consumeExp : Int -> List Char -> Int
consumeExp offset chars =
    let
        nextChar =
            Pine_kernel.head
                (Pine_kernel.skip [ offset, chars ])
    in
    if Pine_kernel.equal [ nextChar, 'e' ] || Pine_kernel.equal [ nextChar, 'E' ] then
        let
            eOffset : Int
            eOffset =
                Pine_kernel.int_add [ offset, 1 ]

            charAfterE =
                Pine_kernel.head
                    (Pine_kernel.skip [ eOffset, chars ])

            expOffset : Int
            expOffset =
                if Pine_kernel.equal [ charAfterE, '+' ] || Pine_kernel.equal [ charAfterE, '-' ] then
                    Pine_kernel.int_add [ eOffset, 1 ]

                else
                    eOffset

            newOffset : Int
            newOffset =
                Elm.Kernel.Parser.chompBase10 expOffset chars
        in
        if Pine_kernel.equal [ expOffset, newOffset ] then
            Pine_kernel.negate newOffset

        else
            newOffset

    else
        offset



-- END


{-| Just like [`Parser.end`](Parser#end) except you provide the problem that
arises when the parser is not at the end of the input.
-}
end : x -> Parser c x ()
end x =
    Parser
        (\\s ->
            let
                (PState srcChars sOffset sIndent sContext sRow sCol) =
                    s
            in
            if Pine_kernel.equal [ Pine_kernel.length srcChars, sOffset ] then
                Good False () s

            else
                Bad False (fromState s x)
        )



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
    Parser
        (\\s0 ->
            case parse s0 of
                Bad p x ->
                    Bad p x

                Good p a s1 ->
                    let
                        (PState srcChars sOffset sIndent sContext sRow sCol) =
                            s0

                        (PState _ s1Offset _ _ _ _) =
                            s1

                        sliceLength : Int
                        sliceLength =
                            Pine_kernel.int_add
                                [ s1Offset
                                , Pine_kernel.negate sOffset
                                ]

                        sliceChars : List Char
                        sliceChars =
                            Pine_kernel.take
                                [ sliceLength
                                , Pine_kernel.skip
                                    [ sOffset
                                    , srcChars
                                    ]
                                ]
                    in
                    Good p (func (String sliceChars) a) s1
        )



-- CHOMP IF


{-| Just like [`Parser.chompIf`](Parser#chompIf) except you provide a problem
in case a character cannot be chomped.
-}
chompIf : (Char -> Bool) -> x -> Parser c x ()
chompIf isGood expecting =
    Parser
        (\\s ->
            let
                (PState srcChars sOffset sIndent sContext sRow sCol) =
                    s

                newOffset =
                    isSubChar isGood sOffset srcChars
            in
            -- not found
            if Pine_kernel.equal [ newOffset, -1 ] then
                Bad False (fromState s expecting)
                -- newline

            else if Pine_kernel.equal [ newOffset, -2 ] then
                Good True
                    ()
                    (PState
                        srcChars
                        (Pine_kernel.int_add [ sOffset, 1 ])
                        sIndent
                        sContext
                        (Pine_kernel.int_add [ sRow, 1 ])
                        1
                    )
                -- found

            else
                Good True
                    ()
                    (PState
                        srcChars
                        newOffset
                        sIndent
                        sContext
                        sRow
                        (Pine_kernel.int_add [ sCol, 1 ])
                    )
        )



-- CHOMP WHILE


{-| Just like [`Parser.chompWhile`](Parser#chompWhile)
-}
chompWhile : (Char -> Bool) -> Parser c x ()
chompWhile isGood =
    Parser
        (\\s ->
            let
                (PState srcChars sOffset sIndent sContext sRow sCol) =
                    s

                ( newOffset, newRow, newCol ) =
                    Elm.Kernel.Parser.chompWhileHelp
                        isGood
                        ( sOffset, sRow, sCol )
                        srcChars
            in
            Good
                (Pine_kernel.negate
                    (Pine_kernel.int_is_sorted_asc [ newOffset, sOffset ])
                )
                ()
                (PState
                    srcChars
                    newOffset
                    sIndent
                    sContext
                    newRow
                    newCol
                )
        )


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
                (PState srcChars sOffset sIndent sContext sRow sCol) =
                    s

                ( newOffset, newRow, newCol ) =
                    Elm.Kernel.Parser.findSubString str sOffset sRow sCol srcChars
            in
            if Pine_kernel.equal [ newOffset, -1 ] then
                Bad False (fromInfo newRow newCol expecting sContext)

            else
                Good
                    (Pine_kernel.negate
                        (Pine_kernel.int_is_sorted_asc [ newOffset, sOffset ])
                    )
                    ()
                    (PState
                        srcChars
                        newOffset
                        sIndent
                        sContext
                        newRow
                        newCol
                    )
        )


{-| Just like [`Parser.chompUntilEndOr`](Parser#chompUntilEndOr)
-}
chompUntilEndOr : String -> Parser c x ()
chompUntilEndOr str =
    Parser
        (\\s ->
            let
                (PState srcChars sOffset sIndent sContext sRow sCol) =
                    s

                ( newOffset, newRow, newCol ) =
                    Elm.Kernel.Parser.findSubString str sOffset sRow sCol srcChars

                adjustedOffset : Int
                adjustedOffset =
                    if Pine_kernel.int_is_sorted_asc [ 0, newOffset ] then
                        newOffset

                    else
                        Pine_kernel.length srcChars
            in
            Good (Pine_kernel.negate (Pine_kernel.int_is_sorted_asc [ adjustedOffset, sOffset ]))
                ()
                (PState
                    srcChars
                    adjustedOffset
                    sIndent
                    sContext
                    newRow
                    newCol
                )
        )



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
            , reserved = Set.fromList [ "let", "in" ]
            , expecting = ExpectingFunctionName
            }

First we parse the function name, and then we parse the rest of the definition.
Importantly, we call `inContext` so that any dead end that occurs in
`definitionBody` will get this extra context information. That way you can say
things like, “I was expecting an equals sign in the `view` definition.” Context!

-}
inContext : context -> Parser context x a -> Parser context x a
inContext context (Parser parse) =
    Parser
        (\\s0 ->
            let
                (PState srcChars offset indent sContext row col) =
                    s0
            in
            case parse (changeContext (Located row col context :: sContext) s0) of
                Good p a s1 ->
                    Good p a (changeContext sContext s1)

                (Bad _ _) as step ->
                    step
        )


changeContext : List (Located c) -> State c -> State c
changeContext newContext (PState srcChars offset indent context row col) =
    PState srcChars offset indent newContext row col



-- INDENTATION


{-| Just like [`Parser.getIndent`](Parser#getIndent)
-}
getIndent : Parser c x Int
getIndent =
    Parser (\\s -> Good False s.indent s)


{-| Just like [`Parser.withIndent`](Parser#withIndent)
-}
withIndent : Int -> Parser c x a -> Parser c x a
withIndent newIndent (Parser parse) =
    Parser
        (\\s0 ->
            let
                (PState srcChars offset s0Indent context row col) =
                    s0
            in
            case parse (changeIndent newIndent s0) of
                Good p a s1 ->
                    Good p a (changeIndent s0Indent s1)

                Bad p x ->
                    Bad p x
        )


changeIndent : Int -> State c -> State c
changeIndent newIndent (PState srcChars offset indent context row col) =
    PState srcChars offset newIndent context row col



-- POSITION


{-| Just like [`Parser.getPosition`](Parser#getPosition)
-}
getPosition : Parser c x ( Int, Int )
getPosition =
    Parser
        (\\s ->
            let
                (PState srcChars offset indent context row col) =
                    s
            in
            Good False ( row, col ) s
        )


{-| Just like [`Parser.getRow`](Parser#getRow)
-}
getRow : Parser c x Int
getRow =
    Parser
        (\\s ->
            let
                (PState srcChars offset indent context row col) =
                    s
            in
            Good False row s
        )


{-| Just like [`Parser.getCol`](Parser#getCol)
-}
getCol : Parser c x Int
getCol =
    Parser
        (\\s ->
            let
                (PState srcChars offset indent context row col) =
                    s
            in
            Good False col s
        )


{-| Just like [`Parser.getOffset`](Parser#getOffset)
-}
getOffset : Parser c x Int
getOffset =
    Parser
        (\\s ->
            let
                (PState srcChars offset indent context row col) =
                    s
            in
            Good False offset s
        )


{-| Just like [`Parser.getSource`](Parser#getSource)
-}
getSource : Parser c x String
getSource =
    Parser
        (\\s ->
            let
                (PState srcChars offset indent context row col) =
                    s
            in
            Good False (String srcChars) s
        )


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
isSubChar : (Char -> Bool) -> Int -> List Char -> Int
isSubChar =
    Elm.Kernel.Parser.isSubChar



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
    Parser
        (\\s ->
            let
                (PState srcChars sOffset indent context row col) =
                    s

                firstChar =
                    Pine_kernel.head
                        (Pine_kernel.skip [ sOffset, srcChars ])
            in
            {-
               First check if we have reached the end of the source string, to account for the possibility of
               a predicate for i.start crashing when given an empty list.
            -}
            if Pine_kernel.equal [ firstChar, [] ] then
                Bad False (fromState s i.expecting)

            else if i.start firstChar then
                let
                    s1 =
                        if Pine_kernel.equal [ firstChar, '\\n' ] then
                            varHelp
                                i.inner
                                (Pine_kernel.int_add [ sOffset, 1 ])
                                (Pine_kernel.int_add [ row, 1 ])
                                1
                                srcChars
                                indent
                                context

                        else
                            varHelp
                                i.inner
                                (Pine_kernel.int_add [ sOffset, 1 ])
                                row
                                (Pine_kernel.int_add [ col, 1 ])
                                srcChars
                                indent
                                context

                    (PState _ s1Offset _ _ _ _) =
                        s1

                    sliceLength : Int
                    sliceLength =
                        Pine_kernel.int_add
                            [ s1Offset
                            , Pine_kernel.negate sOffset
                            ]

                    nameChars : List Char
                    nameChars =
                        Pine_kernel.take
                            [ sliceLength
                            , Pine_kernel.skip [ sOffset, srcChars ]
                            ]

                    name : String
                    name =
                        String nameChars
                in
                if Set.member name i.reserved then
                    Bad False (fromState s i.expecting)

                else
                    Good True name s1

            else
                Bad False (fromState s i.expecting)
        )


varHelp : (Char -> Bool) -> Int -> Int -> Int -> List Char -> Int -> List (Located c) -> State c
varHelp isGood offset row col srcChars indent context =
    let
        newOffset =
            isSubChar isGood offset srcChars
    in
    if Pine_kernel.equal [ newOffset, -1 ] then
        PState
            srcChars
            offset
            indent
            context
            row
            col

    else if Pine_kernel.equal [ newOffset, -2 ] then
        varHelp
            isGood
            (Pine_kernel.int_add [ offset, 1 ])
            (Pine_kernel.int_add [ row, 1 ])
            1
            srcChars
            indent
            context

    else
        varHelp
            isGood
            newOffset
            row
            (Pine_kernel.int_add [ col, 1 ])
            srcChars
            indent
            context



-- SEQUENCES


{-| Just like [`Parser.sequence`](Parser#sequence) except with a `Token` for
the start, separator, and end. That way you can specify your custom type of
problem for when something is not found.
-}
sequence :
    { start : Token x
    , separator : Token x
    , end : Token x
    , spaces : Parser c x ()
    , item : Parser c x a
    , trailing : Trailing
    }
    -> Parser c x (List a)
sequence i =
    skip (token i.start)
        (skip i.spaces
            (sequenceEnd (token i.end) i.spaces i.item (token i.separator) i.trailing)
        )


{-| What’s the deal with trailing commas? Are they `Forbidden`?
Are they `Optional`? Are they `Mandatory`? Welcome to [shapes
club](https://poorlydrawnlines.com/comic/shapes-club/)!
-}
type Trailing
    = Forbidden
    | Optional
    | Mandatory


skip : Parser c x ignore -> Parser c x keep -> Parser c x keep
skip iParser kParser =
    map2 revAlways iParser kParser


revAlways : a -> b -> b
revAlways _ b =
    b


sequenceEnd :
    Parser c x ()
    -> Parser c x ()
    -> Parser c x a
    -> Parser c x ()
    -> Trailing
    -> Parser c x (List a)
sequenceEnd ender ws parseItem sep trailing =
    let
        chompRest item =
            case trailing of
                Forbidden ->
                    loop [ item ] (sequenceEndForbidden ender ws parseItem sep)

                Optional ->
                    loop [ item ] (sequenceEndOptional ender ws parseItem sep)

                Mandatory ->
                    ignorer
                        (skip ws
                            (skip sep
                                (skip ws
                                    (loop [ item ] (sequenceEndMandatory ws parseItem sep))
                                )
                            )
                        )
                        ender
    in
    oneOf
        [ parseItem |> andThen chompRest
        , ender |> map (\\_ -> [])
        ]


sequenceEndForbidden :
    Parser c x ()
    -> Parser c x ()
    -> Parser c x a
    -> Parser c x ()
    -> List a
    -> Parser c x (Step (List a) (List a))
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


sequenceEndOptional :
    Parser c x ()
    -> Parser c x ()
    -> Parser c x a
    -> Parser c x ()
    -> List a
    -> Parser c x (Step (List a) (List a))
sequenceEndOptional ender ws parseItem sep revItems =
    let
        parseEnd =
            map (\\_ -> Done (List.reverse revItems)) ender
    in
    skip ws
        (oneOf
            [ skip sep
                (skip ws
                    (oneOf
                        [ parseItem |> map (\\item -> Loop (item :: revItems))
                        , parseEnd
                        ]
                    )
                )
            , parseEnd
            ]
        )


sequenceEndMandatory :
    Parser c x ()
    -> Parser c x a
    -> Parser c x ()
    -> List a
    -> Parser c x (Step (List a) (List a))
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
    chompWhile
        (\\c ->
            if Pine_kernel.equal [ c, ' ' ] then
                True

            else if Pine_kernel.equal [ c, '\\n' ] then
                True

            else if Pine_kernel.equal [ c, '\\u{000D}' ] then
                True

            else
                False
        )


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
type Nestable
    = NotNestable
    | Nestable


nestableComment : Token x -> Token x -> Parser c x ()
nestableComment ((Token (String openChars) oX) as open) ((Token (String closeChars) cX) as close) =
    let
        openChar =
            Pine_kernel.head openChars

        closeChar =
            Pine_kernel.head closeChars
    in
    if Pine_kernel.equal [ openChar, [] ] then
        problem oX

    else if Pine_kernel.equal [ closeChar, [] ] then
        problem cX

    else
        let
            chompOpen =
                token open
        in
        ignorer
            chompOpen
            (nestableHelp
                (nestableCommentPredicateNotRelevant openChar closeChar)
                chompOpen
                (token close)
                cX
                1
            )


nestableCommentPredicateNotRelevant : Char -> Char -> Char -> Bool
nestableCommentPredicateNotRelevant openChar closeChar char =
    if Pine_kernel.equal [ char, openChar ] then
        False

    else if Pine_kernel.equal [ char, closeChar ] then
        False

    else
        True


nestableHelp : (Char -> Bool) -> Parser c x () -> Parser c x () -> x -> Int -> Parser c x ()
nestableHelp isNotRelevant open close expectingClose nestLevel =
    skip
        (chompWhile isNotRelevant)
        (oneOf
            [ if Pine_kernel.equal [ nestLevel, 1 ] then
                close

              else
                close
                    |> andThen
                        (\\_ ->
                            nestableHelp
                                isNotRelevant
                                open
                                close
                                expectingClose
                                (Pine_kernel.int_add [ nestLevel, -1 ])
                        )
            , open
                |> andThen
                    (\\_ ->
                        nestableHelp
                            isNotRelevant
                            open
                            close
                            expectingClose
                            (Pine_kernel.int_add [ nestLevel, 1 ])
                    )
            , chompIf isChar expectingClose
                |> andThen
                    (\\_ ->
                        nestableHelp
                            isNotRelevant
                            open
                            close
                            expectingClose
                            nestLevel
                    )
            ]
        )


isChar : Char -> Bool
isChar char =
    True

"""
    , """
module Parser exposing
    ( (|.)
    , (|=)
    , DeadEnd
    , Nestable(..)
    , Parser
    , Problem(..)
    , Step(..)
    , Trailing(..)
    , andThen
    , backtrackable
    , chompIf
    , chompUntil
    , chompUntilEndOr
    , chompWhile
    , commit
    , deadEndsToString
    , end
    , float
    , getChompedString
    , getCol
    , getIndent
    , getOffset
    , getPosition
    , getRow
    , getSource
    , int
    , keyword
    , lazy
    , lineComment
    , loop
    , map
    , mapChompedString
    , multiComment
    , number
    , oneOf
    , problem
    , run
    , sequence
    , spaces
    , succeed
    , symbol
    , token
    , variable
    , withIndent
    )

import Char
import Parser.Advanced as A exposing ((|.), (|=))
import Set


infix left  5 (|=) = keeper
infix left  6 (|.) = ignorer


type alias Parser a =
    A.Parser Never Problem a


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


deadEndsToString : List DeadEnd -> String
deadEndsToString deadEnds =
    "TODO deadEndsToString"


succeed : a -> Parser a
succeed =
    A.succeed


keeper : Parser (a -> b) -> Parser a -> Parser b
keeper parseFunc =
    {-
       Since compiler currently fails to compile the original form, inline here as a workaround.
       (|=)
    -}
    A.keeper parseFunc


ignorer : Parser keep -> Parser ignore -> Parser keep
ignorer keepParser ignoreParser =
    {-
       Since compiler currently fails to compile the original form, inline here as a workaround.
       (|.)
    -}
    A.ignorer keepParser ignoreParser


lazy : (() -> Parser a) -> Parser a
lazy =
    A.lazy


andThen : (a -> Parser b) -> Parser a -> Parser b
andThen =
    A.andThen


problem : String -> Parser a
problem msg =
    A.problem (Problem msg)


oneOf : List (Parser a) -> Parser a
oneOf =
    A.oneOf


map : (a -> b) -> Parser a -> Parser b
map =
    A.map


backtrackable : Parser a -> Parser a
backtrackable =
    A.backtrackable


commit : a -> Parser a
commit =
    A.commit


token : String -> Parser ()
token str =
    A.token (toToken str)


toToken : String -> A.Token Problem
toToken str =
    A.Token str (Expecting str)


loop : state -> (state -> Parser (Step state a)) -> Parser a
loop state callback =
    A.loop state (\\s -> map toAdvancedStep (callback s))


type Step state a
    = Loop state
    | Done a


toAdvancedStep : Step s a -> A.Step s a
toAdvancedStep step =
    case step of
        Loop s ->
            A.Loop s

        Done a ->
            A.Done a


int : Parser Int
int =
    A.int ExpectingInt ExpectingInt


float : Parser Float
float =
    A.float ExpectingFloat ExpectingFloat


number :
    { int : Maybe (Int -> a)
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


symbol : String -> Parser ()
symbol str =
    A.symbol (A.Token str (ExpectingSymbol str))


keyword : String -> Parser ()
keyword kwd =
    A.keyword (A.Token kwd (ExpectingKeyword kwd))


end : Parser ()
end =
    A.end ExpectingEnd


getChompedString : Parser a -> Parser String
getChompedString =
    A.getChompedString


mapChompedString : (String -> a -> b) -> Parser a -> Parser b
mapChompedString =
    A.mapChompedString


chompIf : (Char -> Bool) -> Parser ()
chompIf isGood =
    A.chompIf isGood UnexpectedChar


chompWhile : (Char -> Bool) -> Parser ()
chompWhile =
    A.chompWhile


chompUntil : String -> Parser ()
chompUntil str =
    A.chompUntil (toToken str)


chompUntilEndOr : String -> Parser ()
chompUntilEndOr =
    A.chompUntilEndOr


withIndent : Int -> Parser a -> Parser a
withIndent newIndent (A.Parser parse) =
    A.withIndent


getIndent : Parser Int
getIndent =
    A.getIndent


getPosition : Parser ( Int, Int )
getPosition =
    A.getPosition


getRow : Parser Int
getRow =
    A.getRow


getCol : Parser Int
getCol =
    A.getCol


getOffset : Parser Int
getOffset =
    A.getOffset


getSource : Parser String
getSource =
    A.getSource


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


sequence :
    { start : String
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
type Trailing
    = Forbidden
    | Optional
    | Mandatory


toAdvancedTrailing : Trailing -> A.Trailing
toAdvancedTrailing trailing =
    case trailing of
        Forbidden ->
            A.Forbidden

        Optional ->
            A.Optional

        Mandatory ->
            A.Mandatory


spaces : Parser ()
spaces =
    A.spaces


lineComment : String -> Parser ()
lineComment str =
    A.lineComment (toToken str)


multiComment : String -> String -> Nestable -> Parser ()
multiComment open close nestable =
    A.multiComment (toToken open) (toToken close) (toAdvancedNestable nestable)


type Nestable
    = NotNestable
    | Nestable


toAdvancedNestable : Nestable -> A.Nestable
toAdvancedNestable nestable =
    case nestable of
        NotNestable ->
            A.NotNestable

        Nestable ->
            A.Nestable

"""
    ]
