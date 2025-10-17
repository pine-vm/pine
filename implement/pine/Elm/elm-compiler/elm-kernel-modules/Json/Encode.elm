module Json.Encode exposing (..)

import Array
import Char


type String
    = String Int


type Value
    = NullValue
    | BoolValue Bool
    | IntValue Int
    | StringValue String
    | ArrayValue (List Value)
    | ObjectValue (List ( String, Value ))
    | FloatValue String


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


array : (a -> Value) -> Array.Array a -> Value
array encodeItem items =
    ArrayValue (List.map encodeItem (Array.toList items))


object : List ( String, Value ) -> Value
object =
    ObjectValue


encode : Int -> Value -> String
encode indent value =
    String (Pine_kernel.concat (encodeUtf32ChunksWithoutIndent value))


encodeUtf32ChunksWithoutIndent : Value -> List Int
encodeUtf32ChunksWithoutIndent value =
    case value of
        NullValue ->
            [ Pine_kernel.concat [ 'n', 'u', 'l', 'l' ] ]

        BoolValue True ->
            [ Pine_kernel.concat [ 't', 'r', 'u', 'e' ] ]

        BoolValue False ->
            [ Pine_kernel.concat [ 'f', 'a', 'l', 's', 'e' ] ]

        IntValue intVal ->
            String.toList (String.fromInt intVal)

        StringValue (String stringBytes) ->
            [ '"'
            , Pine_kernel.concat (encodeStringUtf32ChunksFromBytes 0 [] stringBytes)
            , '"'
            ]

        ArrayValue values ->
            [ '['
            , Pine_kernel.concat (encodeArrayItemsUtf32ChunksWithoutIndent 0 [] values)
            , ']'
            ]

        ObjectValue fields ->
            [ '{'
            , Pine_kernel.concat (encodeFieldsUtf32ChunksWithoutIndent 0 [] fields)
            , '}'
            ]

        FloatValue (String utf32Bytes) ->
            [ utf32Bytes ]


encodeArrayItemsUtf32ChunksWithoutIndent : Int -> List Value -> List Int -> List Int
encodeArrayItemsUtf32ChunksWithoutIndent itemIndex encodedChunks items =
    if Pine_kernel.equal [ Pine_kernel.length items, itemIndex ] then
        encodedChunks

    else
        let
            nextItem =
                Pine_kernel.head
                    (Pine_kernel.take [ 1, Pine_kernel.skip [ itemIndex, items ] ])
        in
        encodeArrayItemsUtf32ChunksWithoutIndent
            (Pine_kernel.int_add [ itemIndex, 1 ])
            (Pine_kernel.concat
                [ encodedChunks
                , encodeUtf32ChunksWithoutIndent nextItem
                , if Pine_kernel.equal [ Pine_kernel.length items, Pine_kernel.int_add [ itemIndex, 1 ] ] then
                    []

                  else
                    [ ',' ]
                ]
            )
            items


encodeFieldsUtf32ChunksWithoutIndent : Int -> List Int -> List ( String, Value ) -> List Int
encodeFieldsUtf32ChunksWithoutIndent fieldIndex encodedChunks fields =
    if Pine_kernel.equal [ Pine_kernel.length fields, fieldIndex ] then
        encodedChunks

    else
        let
            nextField =
                Pine_kernel.head
                    (Pine_kernel.take [ 1, Pine_kernel.skip [ fieldIndex, fields ] ])
        in
        encodeFieldsUtf32ChunksWithoutIndent
            (Pine_kernel.int_add [ fieldIndex, 1 ])
            (Pine_kernel.concat
                [ encodedChunks
                , encodeFieldUtf32ChunksWithoutIndent nextField
                , if Pine_kernel.equal [ Pine_kernel.length fields, Pine_kernel.int_add [ fieldIndex, 1 ] ] then
                    []

                  else
                    [ ',' ]
                ]
            )
            fields


encodeFieldUtf32ChunksWithoutIndent : ( String, Value ) -> List Int
encodeFieldUtf32ChunksWithoutIndent ( String keyBytes, value ) =
    [ '"'
    , Pine_kernel.concat (encodeStringUtf32ChunksFromBytes 0 [] keyBytes)
    , Pine_kernel.concat [ '"', ':' ]
    , Pine_kernel.concat (encodeUtf32ChunksWithoutIndent value)
    ]


encodeStringUtf32ChunksFromBytes : Int -> List Int -> Int -> List Int
encodeStringUtf32ChunksFromBytes offset encodedChunks sourceBytes =
    let
        simpleEnd : Int
        simpleEnd =
            advanceUtf32OffsetForSimpleChars sourceBytes offset

        simpleLen : Int
        simpleLen =
            Pine_kernel.int_add [ simpleEnd, Pine_kernel.int_mul [ offset, -1 ] ]

        simpleSlice =
            Pine_kernel.take [ simpleLen, Pine_kernel.skip [ offset, sourceBytes ] ]

        chunksWithSimple : List Int
        chunksWithSimple =
            if Pine_kernel.equal [ simpleLen, 0 ] then
                encodedChunks

            else
                Pine_kernel.concat [ encodedChunks, [ simpleSlice ] ]

        nextChar =
            Pine_kernel.take [ 4, Pine_kernel.skip [ simpleEnd, sourceBytes ] ]
    in
    if Pine_kernel.equal [ Pine_kernel.length nextChar, 0 ] then
        chunksWithSimple

    else
        case nextChar of
            '\u{0008}' ->
                encodeStringUtf32ChunksFromBytes
                    (Pine_kernel.int_add [ simpleEnd, 4 ])
                    (Pine_kernel.concat [ chunksWithSimple, [ '\\', 'b' ] ])
                    sourceBytes

            '\t' ->
                encodeStringUtf32ChunksFromBytes
                    (Pine_kernel.int_add [ simpleEnd, 4 ])
                    (Pine_kernel.concat [ chunksWithSimple, [ '\\', '\t' ] ])
                    sourceBytes

            '\n' ->
                encodeStringUtf32ChunksFromBytes
                    (Pine_kernel.int_add [ simpleEnd, 4 ])
                    (Pine_kernel.concat [ chunksWithSimple, [ '\\', '\n' ] ])
                    sourceBytes

            '\u{000C}' ->
                encodeStringUtf32ChunksFromBytes
                    (Pine_kernel.int_add [ simpleEnd, 4 ])
                    (Pine_kernel.concat [ chunksWithSimple, [ '\\', 'f' ] ])
                    sourceBytes

            '\u{000D}' ->
                encodeStringUtf32ChunksFromBytes
                    (Pine_kernel.int_add [ simpleEnd, 4 ])
                    (Pine_kernel.concat [ chunksWithSimple, [ '\\', 'r' ] ])
                    sourceBytes

            '"' ->
                encodeStringUtf32ChunksFromBytes
                    (Pine_kernel.int_add [ simpleEnd, 4 ])
                    (Pine_kernel.concat [ chunksWithSimple, [ '\\', '"' ] ])
                    sourceBytes

            '\\' ->
                encodeStringUtf32ChunksFromBytes
                    (Pine_kernel.int_add [ simpleEnd, 4 ])
                    (Pine_kernel.concat [ chunksWithSimple, [ '\\', '\\' ] ])
                    sourceBytes

            _ ->
                let
                    code : Int
                    code =
                        Char.toCode nextChar

                    unicodeEscape : List Int
                    unicodeEscape =
                        if Pine_kernel.int_is_sorted_asc [ 0, code, 0xFFFF ] then
                            Pine_kernel.concat
                                [ [ '\\', 'u' ]
                                , hex4 code
                                ]

                        else
                            let
                                codePrime =
                                    Pine_kernel.int_add [ code, -0x00010000 ]

                                hi10 =
                                    Pine_kernel.bit_shift_right [ 10, codePrime ]

                                lo10 =
                                    Pine_kernel.bit_and [ 0x03FF, codePrime ]

                                hiUnit =
                                    Pine_kernel.int_add [ 0xD800, hi10 ]

                                loUnit =
                                    Pine_kernel.int_add [ 0xDC00, lo10 ]
                            in
                            Pine_kernel.concat
                                [ [ '\\', 'u' ]
                                , hex4 hiUnit
                                , [ '\\', 'u' ]
                                , hex4 loUnit
                                ]
                in
                encodeStringUtf32ChunksFromBytes
                    (Pine_kernel.int_add [ simpleEnd, 4 ])
                    (Pine_kernel.concat [ chunksWithSimple, unicodeEscape ])
                    sourceBytes


{-| Advance the pointer up to the next char that escaping is needed for.
This is an optimization to skip over simple characters quickly.
It assumes that the input is valid UTF-32.
<https://datatracker.ietf.org/doc/html/rfc8259#section-7>
-}
advanceUtf32OffsetForSimpleChars : Int -> Int -> Int
advanceUtf32OffsetForSimpleChars sourceBytes offset =
    let
        nextChar =
            Pine_kernel.take
                [ 4
                , Pine_kernel.skip [ offset, sourceBytes ]
                ]
    in
    if Pine_kernel.equal [ Pine_kernel.length nextChar, 0 ] then
        offset

    else
        case nextChar of
            '"' ->
                offset

            '\\' ->
                offset

            _ ->
                if
                    Pine_kernel.int_is_sorted_asc
                        [ 0x20

                        -- Prepend sign byte to nextChar to compare as Int
                        , Pine_kernel.concat [ Pine_kernel.take [ 1, 0 ], nextChar ]
                        , 0x0010FFFF
                        ]
                then
                    advanceUtf32OffsetForSimpleChars
                        sourceBytes
                        (Pine_kernel.int_add [ offset, 4 ])

                else
                    offset


float : Float -> Value
float float =
    FloatValue (String.fromFloat float)


hexDigitChar : Int -> Int
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
            '?'


hex4 : Int -> List Int
hex4 n =
    let
        uintBytes =
            Pine_kernel.skip [ 1, n ]

        n3 =
            Pine_kernel.bit_and
                [ Pine_kernel.skip [ 1, 15 ]
                , Pine_kernel.bit_shift_right [ 12, uintBytes ]
                ]

        n2 =
            Pine_kernel.bit_and
                [ Pine_kernel.skip [ 1, 15 ]
                , Pine_kernel.bit_shift_right [ 8, uintBytes ]
                ]

        n1 =
            Pine_kernel.bit_and
                [ Pine_kernel.skip [ 1, 15 ]
                , Pine_kernel.bit_shift_right [ 4, uintBytes ]
                ]

        n0 =
            Pine_kernel.bit_and
                [ Pine_kernel.skip [ 1, 15 ]
                , uintBytes
                ]
    in
    [ hexDigitChar n3
    , hexDigitChar n2
    , hexDigitChar n1
    , hexDigitChar n0
    ]
