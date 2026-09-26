module CompilationInterface.GenerateJsonConverters.Generated_JsonConverters exposing (..)

import Array
import Bytes
import Bytes.Decode
import Bytes.Encode
import Common.EffectOnWindow
import CompilerGenerated.Base64 as Base64
import Dict
import Interface
import Json.Decode
import Json.Encode
import Set


jsonEncode_2190912599 valueToEncode =
    jsonEncode_Interface_Request valueToEncode


jsonDecode_2190912599 =
    jsonDecode_Interface_Request


jsonEncode_Common_EffectOnWindow_MouseButton valueToEncode =
    case valueToEncode of
        Common.EffectOnWindow.MouseButtonLeft ->
            Json.Encode.object [ ( "MouseButtonLeft", Json.Encode.list identity [] ) ]
        Common.EffectOnWindow.MouseButtonRight ->
            Json.Encode.object [ ( "MouseButtonRight", Json.Encode.list identity [] ) ]


jsonDecode_Common_EffectOnWindow_MouseButton =
    Json.Decode.oneOf
        [ Json.Decode.field "MouseButtonLeft" (jsonDecodeSucceedWhenNotNull Common.EffectOnWindow.MouseButtonLeft)
        , Json.Decode.field "MouseButtonRight" (jsonDecodeSucceedWhenNotNull Common.EffectOnWindow.MouseButtonRight)
        ]


jsonEncode_Common_EffectOnWindow_VirtualKeyCode valueToEncode =
    case valueToEncode of
        Common.EffectOnWindow.VirtualKeyCodeFromInt tagArgument0 ->
            Json.Encode.object [ ( "VirtualKeyCodeFromInt", Json.Encode.list identity [ Json.Encode.int tagArgument0 ] ) ]


jsonDecode_Common_EffectOnWindow_VirtualKeyCode =
    Json.Decode.oneOf
        [ Json.Decode.field "VirtualKeyCodeFromInt" (Json.Decode.lazy (\_ -> Json.Decode.map Common.EffectOnWindow.VirtualKeyCodeFromInt (Json.Decode.index 0 Json.Decode.int)))
        ]


jsonEncode_Interface_Request valueToEncode =
    case valueToEncode of
        Interface.KeyDown tagArgument0 ->
            Json.Encode.object [ ( "KeyDown", Json.Encode.list identity [ jsonEncode_Common_EffectOnWindow_VirtualKeyCode tagArgument0 ] ) ]
        Interface.MouseClick tagArgument0 ->
            Json.Encode.object [ ( "MouseClick", Json.Encode.list identity [ jsonEncode_Common_EffectOnWindow_MouseButton tagArgument0 ] ) ]


jsonDecode_Interface_Request =
    Json.Decode.oneOf
        [ Json.Decode.field "KeyDown" (Json.Decode.lazy (\_ -> Json.Decode.map Interface.KeyDown (Json.Decode.index 0 jsonDecode_Common_EffectOnWindow_VirtualKeyCode)))
        , Json.Decode.field "MouseClick" (Json.Decode.lazy (\_ -> Json.Decode.map Interface.MouseClick (Json.Decode.index 0 jsonDecode_Common_EffectOnWindow_MouseButton)))
        ]


jsonEncode__generic_Maybe encodeJust valueToEncode =
    case valueToEncode of
        Nothing ->
            [ ( "Nothing", [] |> Json.Encode.list identity ) ] |> Json.Encode.object

        Just just ->
            [ ( "Just", [ just ] |> Json.Encode.list encodeJust ) ] |> Json.Encode.object


jsonDecode__generic_Maybe decoder =
    Json.Decode.oneOf
        [ Json.Decode.field "Nothing" (Json.Decode.succeed Nothing)
        , Json.Decode.field "Just" ((Json.Decode.index 0 decoder) |> Json.Decode.map Just)
        , Json.Decode.field "Just" (decoder |> Json.Decode.map Just) -- 2020-03-07 Support easy migration of apps: Support decode from older JSON format for now.
        , Json.Decode.null Nothing -- Temporary backwardscompatibility: Map 'null' to Nothing
        ]


jsonEncode__generic_List  = Json.Encode.list


jsonDecode__generic_List  = Json.Decode.list


jsonEncode__generic_Array  = Json.Encode.array


jsonDecode__generic_Array  = Json.Decode.array


jsonEncode__generic_Set encoder =
    Set.toList >> Json.Encode.list encoder


jsonDecode__generic_Set decoder =
    Json.Decode.list decoder |> Json.Decode.map Set.fromList


jsonEncode__generic_Dict encodeKey encodeValue =
    Dict.toList >> Json.Encode.list (jsonEncode__tuple_2 encodeKey encodeValue)


jsonDecode__generic_Dict decodeKey decodeValue =
        (Json.Decode.list (jsonDecode__tuple_2 decodeKey decodeValue))
            |> Json.Decode.map Dict.fromList


jsonEncode__generic_Result encodeErr encodeOk valueToEncode =
    case valueToEncode of
        Err valueToEncodeError ->
            [ ( "Err", [ valueToEncodeError ] |> Json.Encode.list encodeErr ) ] |> Json.Encode.object

        Ok valueToEncodeOk ->
            [ ( "Ok", [ valueToEncodeOk ] |> Json.Encode.list encodeOk ) ] |> Json.Encode.object


jsonDecode__generic_Result decodeErr decodeOk =
    Json.Decode.oneOf
        [ Json.Decode.field "Err" (Json.Decode.index 0 decodeErr) |> Json.Decode.map Err
        , Json.Decode.field "Ok" (Json.Decode.index 0 decodeOk) |> Json.Decode.map Ok
        , Json.Decode.field "Err" decodeErr |> Json.Decode.map Err -- 2020-03-07 Support easy migration of apps: Support decode from older JSON format for now.
        , Json.Decode.field "Ok" decodeOk |> Json.Decode.map Ok -- 2020-03-07 Support easy migration of apps: Support decode from older JSON format for now.
        ]


jsonEncode__tuple_2 encodeA encodeB ( a, b ) =
    [ a |> encodeA, b |> encodeB ]
        |> Json.Encode.list identity


jsonDecode__tuple_2 decodeA decodeB =
    Json.Decode.map2 (\a b -> ( a, b ))
        (Json.Decode.index 0 decodeA)
        (Json.Decode.index 1 decodeB)


jsonEncode__tuple_3 encodeA encodeB encodeC ( a, b, c ) =
    [ a |> encodeA, b |> encodeB, c |> encodeC ]
        |> Json.Encode.list identity


jsonDecode__tuple_3 decodeA decodeB decodeC =
    Json.Decode.map3 (\a b c -> ( a, b, c ))
        (Json.Decode.index 0 decodeA)
        (Json.Decode.index 1 decodeB)
        (Json.Decode.index 2 decodeC)


jsonDecode_andMap : Json.Decode.Decoder a -> Json.Decode.Decoder (a -> b) -> Json.Decode.Decoder b
jsonDecode_andMap =
    Json.Decode.map2 (|>)


json_encode_Bytes : Bytes.Bytes -> Json.Encode.Value
json_encode_Bytes bytes =
    [ ( "AsBase64", bytes |> Base64.fromBytes |> Maybe.withDefault "Error encoding to base64" |> Json.Encode.string ) ]
        |> Json.Encode.object


json_decode_Bytes : Json.Decode.Decoder Bytes.Bytes
json_decode_Bytes =
    Json.Decode.field "AsBase64"
        (Json.Decode.string
            |> Json.Decode.andThen
                (Base64.toBytes >> Maybe.map Json.Decode.succeed >> Maybe.withDefault (Json.Decode.fail "Failed to decode base64."))
        )


jsonDecodeSucceedWhenNotNull : a -> Json.Decode.Decoder a
jsonDecodeSucceedWhenNotNull valueIfNotNull =
    Json.Decode.value
        |> Json.Decode.andThen
            (\asValue ->
                if asValue == Json.Encode.null then
                    Json.Decode.fail "Is null."

                else
                    Json.Decode.succeed valueIfNotNull
            )


jsonDecode_field_withAlternateNames : String -> List String -> Json.Decode.Decoder a -> Json.Decode.Decoder a
jsonDecode_field_withAlternateNames fieldName alternateNames decoder =
    Json.Decode.oneOf
        ((fieldName :: alternateNames)
            |> List.map (\name -> Json.Decode.field name decoder)
        )