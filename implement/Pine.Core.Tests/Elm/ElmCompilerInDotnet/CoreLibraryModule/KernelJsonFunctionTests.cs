using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.Elm;
using Pine.Core.Elm.ElmCompilerInDotnet;
using Pine.Core.Elm.ElmInElm;
using Pine.Core.Files;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Xunit;

namespace Pine.Core.Tests.Elm.ElmCompilerInDotnet.CoreLibraryModule;

public class KernelJsonFunctionTests
{
    /// <summary>
    /// Elm test module that imports Json.Encode and Json.Decode and provides
    /// test functions. Decoders stay inside Elm source so we never have to
    /// pass closures through C#.
    /// </summary>
    private const string TestModuleSource =
        """"
        module JsonTest exposing (..)

        import Dict
        import Json.Decode
        import Json.Encode


        -- ====== Diagnostic: nested constructor pattern tests ======

        type Wrapper
            = WrapBool Bool
            | WrapInt Int

        testNestedPatternTrue : Int -> String
        testNestedPatternTrue _ =
            case WrapBool True of
                WrapBool True ->
                    "matched-true"

                WrapBool False ->
                    "matched-false"

                _ ->
                    "no-match"

        testNestedPatternFalse : Int -> String
        testNestedPatternFalse _ =
            case WrapBool False of
                WrapBool True ->
                    "matched-true"

                WrapBool False ->
                    "matched-false"

                _ ->
                    "no-match"

        testBoolPatternDirect : Bool -> String
        testBoolPatternDirect b =
            case b of
                True ->
                    "is-true"

                False ->
                    "is-false"

        testBoolValueEncode : Bool -> String
        testBoolValueEncode b =
            let
                value = Json.Encode.bool b
            in
            Json.Encode.encode 0 value

        testBoolDirectEncode : Int -> String
        testBoolDirectEncode _ =
            let
                value = Json.Encode.BoolValue True
            in
            case value of
                Json.Encode.BoolValue True ->
                    "got-true"

                Json.Encode.BoolValue False ->
                    "got-false"

                _ ->
                    "other"


        -- ====== Json.Encode tests ======

        -- encode 0 (Json.Encode.string "") == "\"\""
        encodeStringEmpty : Int -> String
        encodeStringEmpty _ =
            Json.Encode.encode 0 (Json.Encode.string "")

        -- encode 0 (Json.Encode.string "abc") == "\"abc\""
        encodeStringAbc : Int -> String
        encodeStringAbc _ =
            Json.Encode.encode 0 (Json.Encode.string "abc")

        -- encode 0 (Json.Encode.string "hello") == "\"hello\""
        encodeStringHello : Int -> String
        encodeStringHello _ =
            Json.Encode.encode 0 (Json.Encode.string "hello")

        -- encode 0 (Json.Encode.int 42) == "42"
        encodeInt42 : Int -> String
        encodeInt42 _ =
            Json.Encode.encode 0 (Json.Encode.int 42)

        -- encode 0 (Json.Encode.int -7) == "-7"
        encodeIntNeg7 : Int -> String
        encodeIntNeg7 _ =
            Json.Encode.encode 0 (Json.Encode.int -7)

        -- encode 0 (Json.Encode.int 0) == "0"
        encodeInt0 : Int -> String
        encodeInt0 _ =
            Json.Encode.encode 0 (Json.Encode.int 0)

        -- encode 0 (Json.Encode.bool True) == "true"
        encodeBoolTrue : Int -> String
        encodeBoolTrue _ =
            Json.Encode.encode 0 (Json.Encode.bool True)

        -- encode 0 (Json.Encode.bool False) == "false"
        encodeBoolFalse : Int -> String
        encodeBoolFalse _ =
            Json.Encode.encode 0 (Json.Encode.bool False)

        -- encode 0 Json.Encode.null == "null"
        encodeNull : Int -> String
        encodeNull _ =
            Json.Encode.encode 0 Json.Encode.null

        -- encode 0 (list int [1,3,4]) == "[1,3,4]"
        encodeListInt : Int -> String
        encodeListInt _ =
            Json.Encode.encode 0 (Json.Encode.list Json.Encode.int [ 1, 3, 4 ])

        -- encode 0 (list bool [True,False]) == "[true,false]"
        encodeListBool : Int -> String
        encodeListBool _ =
            Json.Encode.encode 0 (Json.Encode.list Json.Encode.bool [ True, False ])

        -- encode 0 (list string ["a","b"]) == """["a","b"]"""
        encodeListString : Int -> String
        encodeListString _ =
            Json.Encode.encode 0 (Json.Encode.list Json.Encode.string [ "a", "b" ])

        encodeListEmpty : Int -> String
        encodeListEmpty _ =
            Json.Encode.encode 0 (Json.Encode.list Json.Encode.int [])

        -- Encode.encode 0 tom == """{"name":"Tom","age":42}"""
        encodeObject : Int -> String
        encodeObject _ =
            Json.Encode.encode 0
                (Json.Encode.object
                    [ ( "name", Json.Encode.string "Tom" )
                    , ( "age", Json.Encode.int 42 )
                    ]
                )

        encodeObjectEmpty : Int -> String
        encodeObjectEmpty _ =
            Json.Encode.encode 0 (Json.Encode.object [])

        encodeNestedObject : Int -> String
        encodeNestedObject _ =
            Json.Encode.encode 0
                (Json.Encode.object
                    [ ( "x"
                      , Json.Encode.object
                            [ ( "y", Json.Encode.int 1 ) ]
                      )
                    ]
                )


        -- ====== Json.Decode tests ======

        -- decodeString int "4" == Ok 4
        decodeStringInt4 : Int -> Result Json.Decode.Error Int
        decodeStringInt4 _ =
            Json.Decode.decodeString Json.Decode.int "4"

        -- decodeString int "1 + 2" == Err ...
        decodeStringIntInvalid : Int -> Result Json.Decode.Error Int
        decodeStringIntInvalid _ =
            Json.Decode.decodeString Json.Decode.int "1 + 2"

        -- decodeString int "42" == Ok 42
        decodeStringInt42 : Int -> Result Json.Decode.Error Int
        decodeStringInt42 _ =
            Json.Decode.decodeString Json.Decode.int "42"

        -- decodeString int "true" == Err ...
        decodeStringIntFromBool : Int -> Result Json.Decode.Error Int
        decodeStringIntFromBool _ =
            Json.Decode.decodeString Json.Decode.int "true"

        -- decodeString string "\"hello\"" == Ok "hello"
        decodeStringHello : Int -> Result Json.Decode.Error String
        decodeStringHello _ =
            Json.Decode.decodeString Json.Decode.string "\"hello\""

        -- decodeString string "true" == Err ...
        decodeStringFromBool : Int -> Result Json.Decode.Error String
        decodeStringFromBool _ =
            Json.Decode.decodeString Json.Decode.string "true"

        -- decodeString bool "true" == Ok True
        decodeBoolTrue : Int -> Result Json.Decode.Error Bool
        decodeBoolTrue _ =
            Json.Decode.decodeString Json.Decode.bool "true"

        -- decodeString bool "42" == Err ...
        decodeBoolFromInt : Int -> Result Json.Decode.Error Bool
        decodeBoolFromInt _ =
            Json.Decode.decodeString Json.Decode.bool "42"

        -- decodeString float "42" == Ok 42
        decodeFloatFromInt : Int -> Result Json.Decode.Error Float
        decodeFloatFromInt _ =
            Json.Decode.decodeString Json.Decode.float "42"

        -- decodeString float "3.14" == Ok 3.14
        decodeFloat314 : Int -> Result Json.Decode.Error Float
        decodeFloat314 _ =
            Json.Decode.decodeString Json.Decode.float "3.14"

        -- decodeString float "true" == Err ...
        decodeFloatFromBool : Int -> Result Json.Decode.Error Float
        decodeFloatFromBool _ =
            Json.Decode.decodeString Json.Decode.float "true"

        -- decodeString (null False) "null" == Ok False
        decodeNullFalse : Int -> Result Json.Decode.Error Bool
        decodeNullFalse _ =
            Json.Decode.decodeString (Json.Decode.null False) "null"

        -- decodeString (null 42) "null" == Ok 42
        decodeNull42 : Int -> Result Json.Decode.Error Int
        decodeNull42 _ =
            Json.Decode.decodeString (Json.Decode.null 42) "null"

        -- decodeString (null 42) "42" == Err ...
        decodeNullFromInt : Int -> Result Json.Decode.Error Int
        decodeNullFromInt _ =
            Json.Decode.decodeString (Json.Decode.null 42) "42"

        -- decodeString (succeed 42) "true" == Ok 42
        decodeSucceed42True : Int -> Result Json.Decode.Error Int
        decodeSucceed42True _ =
            Json.Decode.decodeString (Json.Decode.succeed 42) "true"

        -- decodeString (succeed 42) "[1,2,3]" == Ok 42
        decodeSucceed42Array : Int -> Result Json.Decode.Error Int
        decodeSucceed42Array _ =
            Json.Decode.decodeString (Json.Decode.succeed 42) "[1,2,3]"

        -- fail always fails
        decodeFailAlways : Int -> Result Json.Decode.Error Int
        decodeFailAlways _ =
            Json.Decode.decodeString (Json.Decode.fail "my error") "42"

        -- decodeString value "true" gets the raw Value
        decodeValuePassthrough : Int -> Bool
        decodeValuePassthrough _ =
            case Json.Decode.decodeString Json.Decode.value "true" of
                Ok _ ->
                    True

                Err _ ->
                    False

        -- decodeString (nullable int) "13" == Ok (Just 13)
        decodeNullableInt13 : Int -> Result Json.Decode.Error (Maybe Int)
        decodeNullableInt13 _ =
            Json.Decode.decodeString (Json.Decode.nullable Json.Decode.int) "13"

        -- decodeString (nullable int) "42" == Ok (Just 42)
        decodeNullableInt42 : Int -> Result Json.Decode.Error (Maybe Int)
        decodeNullableInt42 _ =
            Json.Decode.decodeString (Json.Decode.nullable Json.Decode.int) "42"

        -- decodeString (nullable int) "null" == Ok Nothing
        decodeNullableIntNull : Int -> Result Json.Decode.Error (Maybe Int)
        decodeNullableIntNull _ =
            Json.Decode.decodeString (Json.Decode.nullable Json.Decode.int) "null"

        -- decodeString (nullable int) "true" == Err ..
        decodeNullableIntBool : Int -> Bool
        decodeNullableIntBool _ =
            case Json.Decode.decodeString (Json.Decode.nullable Json.Decode.int) "true" of
                Err _ ->
                    True

                Ok _ ->
                    False

        -- decodeString (list int) "[1,2,3]" == Ok [1,2,3]
        decodeListInt : Int -> Result Json.Decode.Error (List Int)
        decodeListInt _ =
            Json.Decode.decodeString (Json.Decode.list Json.Decode.int) "[1,2,3]"

        -- decodeString (list bool) "[true,false]" == Ok [True,False]
        decodeListBool : Int -> Result Json.Decode.Error (List Bool)
        decodeListBool _ =
            Json.Decode.decodeString (Json.Decode.list Json.Decode.bool) "[true,false]"

        decodeListEmpty : Int -> Result Json.Decode.Error (List Int)
        decodeListEmpty _ =
            Json.Decode.decodeString (Json.Decode.list Json.Decode.int) "[]"

        -- decodeString (field "x" int) "{ \"x\": 3 }" == Ok 3
        decodeFieldX3 : Int -> Result Json.Decode.Error Int
        decodeFieldX3 _ =
            Json.Decode.decodeString (Json.Decode.field "x" Json.Decode.int) "{ \"x\": 3 }"

        -- decodeString (field "x" int) "{ \"x\": 3, \"y\": 4 }" == Ok 3
        decodeFieldXWithY : Int -> Result Json.Decode.Error Int
        decodeFieldXWithY _ =
            Json.Decode.decodeString (Json.Decode.field "x" Json.Decode.int) "{ \"x\": 3, \"y\": 4 }"

        -- decodeString (field "name" string) "{ \"name\": \"tom\" }" == Ok "tom"
        decodeFieldName : Int -> Result Json.Decode.Error String
        decodeFieldName _ =
            Json.Decode.decodeString (Json.Decode.field "name" Json.Decode.string) "{ \"name\": \"tom\" }"

        -- decodeString (field "x" int) "{ \"y\": 4 }" == Err ...
        decodeFieldMissing : Int -> Bool
        decodeFieldMissing _ =
            case Json.Decode.decodeString (Json.Decode.field "x" Json.Decode.int) "{ \"y\": 4 }" of
                Err _ ->
                    True

                Ok _ ->
                    False

        -- decodeString (index 0 string) """["alice","bob","chuck"]""" == Ok "alice"
        decodeIndex0 : Int -> Result Json.Decode.Error String
        decodeIndex0 _ =
            Json.Decode.decodeString (Json.Decode.index 0 Json.Decode.string) "[\"alice\",\"bob\",\"chuck\"]"

        -- decodeString (index 1 string) """["alice","bob","chuck"]""" == Ok "bob"
        decodeIndex1 : Int -> Result Json.Decode.Error String
        decodeIndex1 _ =
            Json.Decode.decodeString (Json.Decode.index 1 Json.Decode.string) "[\"alice\",\"bob\",\"chuck\"]"

        -- decodeString (index 2 string) """["alice","bob","chuck"]""" == Ok "chuck"
        decodeIndex2 : Int -> Result Json.Decode.Error String
        decodeIndex2 _ =
            Json.Decode.decodeString (Json.Decode.index 2 Json.Decode.string) "[\"alice\",\"bob\",\"chuck\"]"

        -- decodeString (index 3 string) json == Err ...
        decodeIndexOutOfBounds : Int -> Bool
        decodeIndexOutOfBounds _ =
            case Json.Decode.decodeString (Json.Decode.index 3 Json.Decode.string) "[\"alice\",\"bob\",\"chuck\"]" of
                Err _ ->
                    True

                Ok _ ->
                    False

        -- map String.length string
        decodeMapStringLength : Int -> Result Json.Decode.Error Int
        decodeMapStringLength _ =
            Json.Decode.decodeString (Json.Decode.map String.length Json.Decode.string) "\"hello\""

        -- map2 Point (field "x" float) (field "y" float)
        decodeMap2Point : Int -> Result Json.Decode.Error { x : Int, y : Int }
        decodeMap2Point _ =
            let
                decoder =
                    Json.Decode.map2 (\x y -> { x = x, y = y })
                        (Json.Decode.field "x" Json.Decode.int)
                        (Json.Decode.field "y" Json.Decode.int)
            in
            Json.Decode.decodeString decoder "{ \"x\": 3, \"y\": 4 }"

        -- map3 Person (at ["name"] string) (at ["info","age"] int) (at ["info","height"] int)
        decodeMap3 : Int -> Result Json.Decode.Error { name : String, age : Int, height : Int }
        decodeMap3 _ =
            let
                decoder =
                    Json.Decode.map3 (\n a h -> { name = n, age = a, height = h })
                        (Json.Decode.field "name" Json.Decode.string)
                        (Json.Decode.field "age" Json.Decode.int)
                        (Json.Decode.field "height" Json.Decode.int)
            in
            Json.Decode.decodeString decoder "{ \"name\": \"tom\", \"age\": 42, \"height\": 180 }"

        -- andThen: version-based decoding
        decodeAndThenVersion : Int -> Result Json.Decode.Error String
        decodeAndThenVersion _ =
            let
                decoder =
                    Json.Decode.field "version" Json.Decode.int
                        |> Json.Decode.andThen versionHelp
            in
            Json.Decode.decodeString decoder "{ \"version\": 3, \"data\": \"v3-data\" }"

        versionHelp : Int -> Json.Decode.Decoder String
        versionHelp version =
            case version of
                3 ->
                    Json.Decode.field "data" Json.Decode.string

                _ ->
                    Json.Decode.fail "unsupported version"

        decodeAndThenVersionFail : Int -> Bool
        decodeAndThenVersionFail _ =
            let
                decoder =
                    Json.Decode.field "version" Json.Decode.int
                        |> Json.Decode.andThen versionHelp
            in
            case Json.Decode.decodeString decoder "{ \"version\": 99 }" of
                Err _ ->
                    True

                Ok _ ->
                    False

        -- oneOf: badInt decoder from docs
        decodeBadInt : Int -> Result Json.Decode.Error (List Int)
        decodeBadInt _ =
            let
                badInt =
                    Json.Decode.oneOf [ Json.Decode.int, Json.Decode.null 0 ]
            in
            Json.Decode.decodeString (Json.Decode.list badInt) "[1,2,null,4]"

        -- oneOf with empty list fails
        decodeOneOfEmpty : Int -> Bool
        decodeOneOfEmpty _ =
            case Json.Decode.decodeString (Json.Decode.oneOf []) "42" of
                Err _ ->
                    True

                Ok _ ->
                    False

        -- lazy: for recursive structures
        type alias Comment =
            { message : String
            , responses : List Comment
            }

        commentDecoder : Json.Decode.Decoder Comment
        commentDecoder =
            Json.Decode.map2 (\m r -> { message = m, responses = r })
                (Json.Decode.field "message" Json.Decode.string)
                (Json.Decode.field "responses" (Json.Decode.list (Json.Decode.lazy (\_ -> commentDecoder))))

        decodeLazyRecursive : Int -> Result Json.Decode.Error String
        decodeLazyRecursive _ =
            case Json.Decode.decodeString commentDecoder "{ \"message\": \"hello\", \"responses\": [] }" of
                Ok comment ->
                    Ok comment.message

                Err err ->
                    Err err

        decodeLazyNested : Int -> Result Json.Decode.Error Int
        decodeLazyNested _ =
            case Json.Decode.decodeString commentDecoder "{ \"message\": \"a\", \"responses\": [{ \"message\": \"b\", \"responses\": [] }] }" of
                Ok comment ->
                    Ok (List.length comment.responses)

                Err err ->
                    Err err

        -- at: nested field access
        -- decodeString (at ["person", "name"] string) json == Ok "tom"
        decodeAt : Int -> Result Json.Decode.Error String
        decodeAt _ =
            Json.Decode.decodeString
                (Json.Decode.at [ "person", "name" ] Json.Decode.string)
                "{ \"person\": { \"name\": \"tom\", \"age\": 42 } }"

        decodeAtAge : Int -> Result Json.Decode.Error Int
        decodeAtAge _ =
            Json.Decode.decodeString
                (Json.Decode.at [ "person", "age" ] Json.Decode.int)
                "{ \"person\": { \"name\": \"tom\", \"age\": 42 } }"

        -- maybe: optional fields
        -- decodeString (maybe (field "age" int)) json == Ok (Just 42)
        decodeMaybePresent : Int -> Result Json.Decode.Error (Maybe Int)
        decodeMaybePresent _ =
            Json.Decode.decodeString
                (Json.Decode.maybe (Json.Decode.field "age" Json.Decode.int))
                "{ \"name\": \"tom\", \"age\": 42 }"

        -- decodeString (maybe (field "name" int)) json == Ok Nothing
        decodeMaybeWrongType : Int -> Result Json.Decode.Error (Maybe Int)
        decodeMaybeWrongType _ =
            Json.Decode.decodeString
                (Json.Decode.maybe (Json.Decode.field "name" Json.Decode.int))
                "{ \"name\": \"tom\", \"age\": 42 }"

        -- decodeString (maybe (field "height" float)) json == Ok Nothing
        decodeMaybeMissing : Int -> Result Json.Decode.Error (Maybe Int)
        decodeMaybeMissing _ =
            Json.Decode.decodeString
                (Json.Decode.maybe (Json.Decode.field "height" Json.Decode.int))
                "{ \"name\": \"tom\", \"age\": 42 }"

        -- keyValuePairs: decode object as pairs
        -- decodeString (keyValuePairs int) """{"alice":42,"bob":99}""" == Ok [("alice",42),("bob",99)]
        decodeKeyValuePairs : Int -> Result Json.Decode.Error (List ( String, Int ))
        decodeKeyValuePairs _ =
            Json.Decode.decodeString
                (Json.Decode.keyValuePairs Json.Decode.int)
                "{ \"alice\": 42, \"bob\": 99 }"

        decodeKeyValuePairsEmpty : Int -> Result Json.Decode.Error (List ( String, Int ))
        decodeKeyValuePairsEmpty _ =
            Json.Decode.decodeString
                (Json.Decode.keyValuePairs Json.Decode.int)
                "{}"

        -- dict: decode object as Dict
        -- decodeString (dict int) """{"alice":42,"bob":99}""" == Ok (Dict.fromList [("alice",42),("bob",99)])
        decodeDictGet : Int -> Maybe Int
        decodeDictGet _ =
            case Json.Decode.decodeString (Json.Decode.dict Json.Decode.int) "{ \"alice\": 42, \"bob\": 99 }" of
                Ok d ->
                    Dict.get "alice" d

                Err _ ->
                    Nothing

        decodeDictSize : Int -> Int
        decodeDictSize _ =
            case Json.Decode.decodeString (Json.Decode.dict Json.Decode.int) "{ \"alice\": 42, \"bob\": 99 }" of
                Ok d ->
                    Dict.size d

                Err _ ->
                    -1

        -- oneOrMore
        decodeOneOrMoreSuccess : Int -> Result Json.Decode.Error (List Int)
        decodeOneOrMoreSuccess _ =
            let
                decoder =
                    Json.Decode.oneOrMore (\first rest -> first :: rest) Json.Decode.int
            in
            Json.Decode.decodeString decoder "[1,2,3]"

        decodeOneOrMoreFail : Int -> Bool
        decodeOneOrMoreFail _ =
            let
                decoder =
                    Json.Decode.oneOrMore (\first rest -> first :: rest) Json.Decode.int
            in
            case Json.Decode.decodeString decoder "[]" of
                Err _ ->
                    True

                Ok _ ->
                    False

        -- Additional coverage tests

        -- string with escape chars
        decodeStringEscape : Int -> Result Json.Decode.Error String
        decodeStringEscape _ =
            Json.Decode.decodeString Json.Decode.string "\"hello\\nworld\""

        -- nested arrays
        decodeNestedArray : Int -> Result Json.Decode.Error (List (List Int))
        decodeNestedArray _ =
            Json.Decode.decodeString
                (Json.Decode.list (Json.Decode.list Json.Decode.int))
                "[[1,2],[3,4]]"

        -- Encode then decode roundtrip
        encodeDecodeRoundtrip : Int -> Result Json.Decode.Error Int
        encodeDecodeRoundtrip _ =
            let
                encoded = Json.Encode.encode 0 (Json.Encode.int 42)
            in
            Json.Decode.decodeString Json.Decode.int encoded

        encodeDecodeRoundtripString : String -> Result Json.Decode.Error String
        encodeDecodeRoundtripString s =
            let
                encoded = Json.Encode.encode 0 (Json.Encode.string s)
            in
            Json.Decode.decodeString Json.Decode.string encoded

        encodeDecodeRoundtripObject : Int -> Result Json.Decode.Error String
        encodeDecodeRoundtripObject _ =
            let
                val =
                    Json.Encode.object
                        [ ( "name", Json.Encode.string "Tom" )
                        , ( "age", Json.Encode.int 42 )
                        ]

                encoded = Json.Encode.encode 0 val
            in
            Json.Decode.decodeString (Json.Decode.field "name" Json.Decode.string) encoded

        -- Negative integers
        decodeNegativeInt : Int -> Result Json.Decode.Error Int
        decodeNegativeInt _ =
            Json.Decode.decodeString Json.Decode.int "-7"

        -- Whitespace handling
        decodeWithWhitespace : Int -> Result Json.Decode.Error Int
        decodeWithWhitespace _ =
            Json.Decode.decodeString Json.Decode.int "  42  "

        -- Multiple fields with map2
        decodeMap2Fields : Int -> Result Json.Decode.Error Int
        decodeMap2Fields _ =
            let
                decoder =
                    Json.Decode.map2 (\a b -> a + b)
                        (Json.Decode.field "x" Json.Decode.int)
                        (Json.Decode.field "y" Json.Decode.int)
            in
            Json.Decode.decodeString decoder "{ \"x\": 10, \"y\": 20 }"

        -- Encode string with special chars
        encodeStringWithQuote : Int -> String
        encodeStringWithQuote _ =
            Json.Encode.encode 0 (Json.Encode.string "say \"hi\"")

        -- Encode string with newline
        encodeStringWithNewline : Int -> String
        encodeStringWithNewline _ =
            Json.Encode.encode 0 (Json.Encode.string "line1\nline2")

        -- Encode string with backslash
        encodeStringWithBackslash : Int -> String
        encodeStringWithBackslash _ =
            Json.Encode.encode 0 (Json.Encode.string "a\\b")

        -- Decode decodeValue (direct Value decoding)
        decodeValueDirect : Int -> Result Json.Decode.Error Int
        decodeValueDirect _ =
            Json.Decode.decodeValue Json.Decode.int (Json.Encode.int 42)

        decodeValueDirectString : Int -> Result Json.Decode.Error String
        decodeValueDirectString _ =
            Json.Decode.decodeValue Json.Decode.string (Json.Encode.string "hello")

        -- Encode float
        encodeFloat314 : Int -> String
        encodeFloat314 _ =
            Json.Encode.encode 0 (Json.Encode.float 3.14)

        -- Decode large integer
        decodeLargeInt : Int -> Result Json.Decode.Error Int
        decodeLargeInt _ =
            Json.Decode.decodeString Json.Decode.int "1000000"

        -- Decode nested field
        decodeNestedField : Int -> Result Json.Decode.Error Int
        decodeNestedField _ =
            Json.Decode.decodeString
                (Json.Decode.field "outer" (Json.Decode.field "inner" Json.Decode.int))
                "{ \"outer\": { \"inner\": 99 } }"

        -- oneOf first succeeds
        decodeOneOfFirst : Int -> Result Json.Decode.Error Int
        decodeOneOfFirst _ =
            Json.Decode.decodeString (Json.Decode.oneOf [ Json.Decode.int, Json.Decode.null 0 ]) "42"

        -- oneOf second succeeds
        decodeOneOfSecond : Int -> Result Json.Decode.Error Int
        decodeOneOfSecond _ =
            Json.Decode.decodeString (Json.Decode.oneOf [ Json.Decode.int, Json.Decode.null 0 ]) "null"
        """";

    private static readonly Lazy<ElmInteractiveEnvironment.ParsedInteractiveEnvironment> s_env =
        new(
            () =>
            {
                var kernelModulesTree =
                    BundledFiles.CompilerSourceContainerFilesDefault.Value
                    .GetNodeAtPath(["elm-kernel-modules"])
                    ?? throw new Exception("Did not find elm-kernel-modules");

                // Add our test module to the kernel modules tree
                var treeWithTest =
                    kernelModulesTree.SetNodeAtPathSorted(
                        ["JsonTest.elm"],
                        FileTree.File(Encoding.UTF8.GetBytes(TestModuleSource)));

                var rootFilePaths =
                    treeWithTest.EnumerateFilesTransitive()
                    .Where(
                        b =>
                        b.path[^1].Equals("JsonTest.elm", StringComparison.OrdinalIgnoreCase))
                    .Select(b => (IReadOnlyList<string>)b.path)
                    .ToList();

                var compiledEnv =
                    ElmCompiler.CompileInteractiveEnvironment(
                        treeWithTest,
                        rootFilePaths: rootFilePaths,
                        disableInlining: false)
                    .Extract(err => throw new Exception("Failed compiling: " + err));

                return
                    ElmInteractiveEnvironment.ParseInteractiveEnvironment(compiledEnv)
                    .Extract(err => throw new Exception("Failed parsing: " + err));
            });

    private static PineValue GetTestFunction(string name) =>
        s_env.Value.Modules
        .First(m => m.moduleName is "JsonTest")
        .moduleContent.FunctionDeclarations[name];

    private static readonly Core.Interpreter.IntermediateVM.PineVM s_vm =
        ElmCompilerTestHelper.PineVMForProfiling(_ => { });

    private static ElmValue ApplyUnary(PineValue functionValue, ElmValue argument) =>
        CoreLibraryTestHelper.ApplyUnary(functionValue, argument, s_vm);

    private static ElmValue Integer(long i) =>
        ElmValue.Integer(i);

    private static ElmValue String(string s) =>
        ElmValue.StringInstance(s);

    private static ElmValue ElmList(params ElmValue[] items) =>
        ElmValue.ListInstance([.. items]);

    private static ElmValue OkOf(ElmValue inner) =>
        ElmValue.TagInstance("Ok", [inner]);

    private static readonly ElmValue s_true = ElmValue.TagInstance("True", []);
    private static readonly ElmValue s_false = ElmValue.TagInstance("False", []);
    private static readonly ElmValue s_nothing = ElmValue.TagInstance("Nothing", []);

    private static ElmValue JustOf(ElmValue inner) =>
        ElmValue.TagInstance("Just", [inner]);

    private static ElmValue CallThunk(string name) =>
        ApplyUnary(GetTestFunction(name), Integer(0));

    // ===== Diagnostic: nested constructor pattern tests =====

    [Fact]
    public void Diagnostic_nested_pattern_true()
    {
        CallThunk("testNestedPatternTrue").Should().Be(String("matched-true"));
    }

    [Fact]
    public void Diagnostic_nested_pattern_false()
    {
        CallThunk("testNestedPatternFalse").Should().Be(String("matched-false"));
    }

    [Fact]
    public void Diagnostic_bool_pattern_direct_true()
    {
        ApplyUnary(GetTestFunction("testBoolPatternDirect"), s_true).Should().Be(String("is-true"));
    }

    [Fact]
    public void Diagnostic_bool_pattern_direct_false()
    {
        ApplyUnary(GetTestFunction("testBoolPatternDirect"), s_false).Should().Be(String("is-false"));
    }

    [Fact]
    public void Diagnostic_bool_value_encode_true()
    {
        ApplyUnary(GetTestFunction("testBoolValueEncode"), s_true).Should().Be(String("true"));
    }

    [Fact]
    public void Diagnostic_bool_value_encode_false()
    {
        ApplyUnary(GetTestFunction("testBoolValueEncode"), s_false).Should().Be(String("false"));
    }

    [Fact]
    public void Diagnostic_bool_direct_encode()
    {
        CallThunk("testBoolDirectEncode").Should().Be(String("got-true"));
    }

    // ===== Json.Encode: string =====

    [Fact]
    public void Encode_string_empty()
    {
        CallThunk("encodeStringEmpty").Should().Be(String("\"\""));
    }

    [Fact]
    public void Encode_string_abc()
    {
        CallThunk("encodeStringAbc").Should().Be(String("\"abc\""));
    }

    [Fact]
    public void Encode_string_hello()
    {
        CallThunk("encodeStringHello").Should().Be(String("\"hello\""));
    }

    // ===== Json.Encode: int =====

    [Fact]
    public void Encode_int_42()
    {
        CallThunk("encodeInt42").Should().Be(String("42"));
    }

    [Fact]
    public void Encode_int_neg7()
    {
        CallThunk("encodeIntNeg7").Should().Be(String("-7"));
    }

    [Fact]
    public void Encode_int_0()
    {
        CallThunk("encodeInt0").Should().Be(String("0"));
    }

    // ===== Json.Encode: bool =====

    [Fact]
    public void Encode_bool_true()
    {
        CallThunk("encodeBoolTrue").Should().Be(String("true"));
    }

    [Fact]
    public void Encode_bool_false()
    {
        CallThunk("encodeBoolFalse").Should().Be(String("false"));
    }

    // ===== Json.Encode: null =====

    [Fact]
    public void Encode_null()
    {
        CallThunk("encodeNull").Should().Be(String("null"));
    }

    // ===== Json.Encode: list =====

    [Fact]
    public void Encode_list_int()
    {
        CallThunk("encodeListInt").Should().Be(String("[1,3,4]"));
    }

    [Fact]
    public void Encode_list_bool()
    {
        CallThunk("encodeListBool").Should().Be(String("[true,false]"));
    }

    [Fact]
    public void Encode_list_string()
    {
        CallThunk("encodeListString").Should().Be(String("[\"a\",\"b\"]"));
    }

    [Fact]
    public void Encode_list_empty()
    {
        CallThunk("encodeListEmpty").Should().Be(String("[]"));
    }

    // ===== Json.Encode: object =====

    [Fact]
    public void Encode_object()
    {
        CallThunk("encodeObject").Should().Be(String("{\"name\":\"Tom\",\"age\":42}"));
    }

    [Fact]
    public void Encode_object_empty()
    {
        CallThunk("encodeObjectEmpty").Should().Be(String("{}"));
    }

    [Fact]
    public void Encode_nested_object()
    {
        CallThunk("encodeNestedObject").Should().Be(String("{\"x\":{\"y\":1}}"));
    }

    // ===== Json.Encode: special strings =====

    [Fact]
    public void Encode_string_with_quote()
    {
        CallThunk("encodeStringWithQuote").Should().Be(String("\"say \\\"hi\\\"\""));
    }

    [Fact]
    public void Encode_string_with_newline()
    {
        CallThunk("encodeStringWithNewline").Should().Be(String("\"line1\\nline2\""));
    }

    [Fact]
    public void Encode_string_with_backslash()
    {
        CallThunk("encodeStringWithBackslash").Should().Be(String("\"a\\\\b\""));
    }

    // ===== Json.Encode: float =====

    [Fact]
    public void Encode_float_314()
    {
        CallThunk("encodeFloat314").Should().Be(String("3.14"));
    }

    // ===== Json.Decode: decodeString int =====

    [Fact]
    public void Decode_string_int_4()
    {
        CallThunk("decodeStringInt4").Should().Be(OkOf(Integer(4)));
    }

    [Fact]
    public void Decode_string_int_42()
    {
        CallThunk("decodeStringInt42").Should().Be(OkOf(Integer(42)));
    }

    [Fact]
    public void Decode_string_int_invalid()
    {
        var result = CallThunk("decodeStringIntInvalid");
        result.Should().BeOfType<ElmValue.ElmTag>();
        ((ElmValue.ElmTag)result).TagName.Should().Be("Err");
    }

    [Fact]
    public void Decode_string_int_from_bool()
    {
        var result = CallThunk("decodeStringIntFromBool");
        result.Should().BeOfType<ElmValue.ElmTag>();
        ((ElmValue.ElmTag)result).TagName.Should().Be("Err");
    }

    // ===== Json.Decode: decodeString string =====

    [Fact]
    public void Decode_string_hello()
    {
        CallThunk("decodeStringHello").Should().Be(OkOf(String("hello")));
    }

    [Fact]
    public void Decode_string_from_bool()
    {
        var result = CallThunk("decodeStringFromBool");
        result.Should().BeOfType<ElmValue.ElmTag>();
        ((ElmValue.ElmTag)result).TagName.Should().Be("Err");
    }

    // ===== Json.Decode: decodeString bool =====

    [Fact]
    public void Decode_bool_true()
    {
        CallThunk("decodeBoolTrue").Should().Be(OkOf(s_true));
    }

    [Fact]
    public void Decode_bool_from_int()
    {
        var result = CallThunk("decodeBoolFromInt");
        result.Should().BeOfType<ElmValue.ElmTag>();
        ((ElmValue.ElmTag)result).TagName.Should().Be("Err");
    }

    // ===== Json.Decode: decodeString float =====

    [Fact]
    public void Decode_float_from_int()
    {
        // decodeString float "42" == Ok 42.0
        var result = CallThunk("decodeFloatFromInt");
        result.Should().BeOfType<ElmValue.ElmTag>();
        ((ElmValue.ElmTag)result).TagName.Should().Be("Ok");
    }

    [Fact]
    public void Decode_float_314()
    {
        // decodeString float "3.14" == Ok 3.14
        var result = CallThunk("decodeFloat314");
        result.Should().BeOfType<ElmValue.ElmTag>();
        ((ElmValue.ElmTag)result).TagName.Should().Be("Ok");
    }

    [Fact]
    public void Decode_float_from_bool()
    {
        var result = CallThunk("decodeFloatFromBool");
        result.Should().BeOfType<ElmValue.ElmTag>();
        ((ElmValue.ElmTag)result).TagName.Should().Be("Err");
    }

    // ===== Json.Decode: null =====

    [Fact]
    public void Decode_null_false()
    {
        CallThunk("decodeNullFalse").Should().Be(OkOf(s_false));
    }

    [Fact]
    public void Decode_null_42()
    {
        CallThunk("decodeNull42").Should().Be(OkOf(Integer(42)));
    }

    [Fact]
    public void Decode_null_from_int()
    {
        var result = CallThunk("decodeNullFromInt");
        result.Should().BeOfType<ElmValue.ElmTag>();
        ((ElmValue.ElmTag)result).TagName.Should().Be("Err");
    }

    // ===== Json.Decode: succeed =====

    [Fact]
    public void Decode_succeed_42_true()
    {
        CallThunk("decodeSucceed42True").Should().Be(OkOf(Integer(42)));
    }

    [Fact]
    public void Decode_succeed_42_array()
    {
        CallThunk("decodeSucceed42Array").Should().Be(OkOf(Integer(42)));
    }

    // ===== Json.Decode: fail =====

    [Fact]
    public void Decode_fail_always()
    {
        var result = CallThunk("decodeFailAlways");
        result.Should().BeOfType<ElmValue.ElmTag>();
        ((ElmValue.ElmTag)result).TagName.Should().Be("Err");
    }

    // ===== Json.Decode: value =====

    [Fact]
    public void Decode_value_passthrough()
    {
        CallThunk("decodeValuePassthrough").Should().Be(s_true);
    }

    // ===== Json.Decode: nullable =====

    [Fact]
    public void Decode_nullable_int_13()
    {
        CallThunk("decodeNullableInt13").Should().Be(OkOf(JustOf(Integer(13))));
    }

    [Fact]
    public void Decode_nullable_int_42()
    {
        CallThunk("decodeNullableInt42").Should().Be(OkOf(JustOf(Integer(42))));
    }

    [Fact]
    public void Decode_nullable_int_null()
    {
        CallThunk("decodeNullableIntNull").Should().Be(OkOf(s_nothing));
    }

    [Fact]
    public void Decode_nullable_int_bool()
    {
        CallThunk("decodeNullableIntBool").Should().Be(s_true);
    }

    // ===== Json.Decode: list =====

    [Fact]
    public void Decode_list_int()
    {
        CallThunk("decodeListInt").Should().Be(OkOf(ElmList(Integer(1), Integer(2), Integer(3))));
    }

    [Fact]
    public void Decode_list_bool()
    {
        CallThunk("decodeListBool").Should().Be(OkOf(ElmList(s_true, s_false)));
    }

    [Fact]
    public void Decode_list_empty()
    {
        CallThunk("decodeListEmpty").Should().Be(OkOf(ElmList()));
    }

    // ===== Json.Decode: field =====

    [Fact]
    public void Decode_field_x_3()
    {
        CallThunk("decodeFieldX3").Should().Be(OkOf(Integer(3)));
    }

    [Fact]
    public void Decode_field_x_with_y()
    {
        CallThunk("decodeFieldXWithY").Should().Be(OkOf(Integer(3)));
    }

    [Fact]
    public void Decode_field_name()
    {
        CallThunk("decodeFieldName").Should().Be(OkOf(String("tom")));
    }

    [Fact]
    public void Decode_field_missing()
    {
        CallThunk("decodeFieldMissing").Should().Be(s_true);
    }

    // ===== Json.Decode: index =====

    [Fact]
    public void Decode_index_0()
    {
        CallThunk("decodeIndex0").Should().Be(OkOf(String("alice")));
    }

    [Fact]
    public void Decode_index_1()
    {
        CallThunk("decodeIndex1").Should().Be(OkOf(String("bob")));
    }

    [Fact]
    public void Decode_index_2()
    {
        CallThunk("decodeIndex2").Should().Be(OkOf(String("chuck")));
    }

    [Fact]
    public void Decode_index_out_of_bounds()
    {
        CallThunk("decodeIndexOutOfBounds").Should().Be(s_true);
    }

    // ===== Json.Decode: map =====

    [Fact]
    public void Decode_map_string_length()
    {
        CallThunk("decodeMapStringLength").Should().Be(OkOf(Integer(5)));
    }

    // ===== Json.Decode: map2 =====

    [Fact]
    public void Decode_map2_point()
    {
        var result = CallThunk("decodeMap2Point");
        result.Should().BeOfType<ElmValue.ElmTag>();
        ((ElmValue.ElmTag)result).TagName.Should().Be("Ok");
    }

    // ===== Json.Decode: map3 =====

    [Fact]
    public void Decode_map3()
    {
        var result = CallThunk("decodeMap3");
        result.Should().BeOfType<ElmValue.ElmTag>();
        ((ElmValue.ElmTag)result).TagName.Should().Be("Ok");
    }

    // ===== Json.Decode: andThen =====

    [Fact]
    public void Decode_andThen_version()
    {
        CallThunk("decodeAndThenVersion").Should().Be(OkOf(String("v3-data")));
    }

    [Fact]
    public void Decode_andThen_version_fail()
    {
        CallThunk("decodeAndThenVersionFail").Should().Be(s_true);
    }

    // ===== Json.Decode: oneOf =====

    [Fact]
    public void Decode_oneOf_badInt()
    {
        CallThunk("decodeBadInt").Should().Be(OkOf(ElmList(Integer(1), Integer(2), Integer(0), Integer(4))));
    }

    [Fact]
    public void Decode_oneOf_empty()
    {
        CallThunk("decodeOneOfEmpty").Should().Be(s_true);
    }

    [Fact]
    public void Decode_oneOf_first()
    {
        CallThunk("decodeOneOfFirst").Should().Be(OkOf(Integer(42)));
    }

    [Fact]
    public void Decode_oneOf_second()
    {
        CallThunk("decodeOneOfSecond").Should().Be(OkOf(Integer(0)));
    }

    // ===== Json.Decode: lazy =====

    [Fact]
    public void Decode_lazy_recursive()
    {
        CallThunk("decodeLazyRecursive").Should().Be(OkOf(String("hello")));
    }

    [Fact]
    public void Decode_lazy_nested()
    {
        CallThunk("decodeLazyNested").Should().Be(OkOf(Integer(1)));
    }

    // ===== Json.Decode: at =====

    [Fact]
    public void Decode_at_person_name()
    {
        CallThunk("decodeAt").Should().Be(OkOf(String("tom")));
    }

    [Fact]
    public void Decode_at_person_age()
    {
        CallThunk("decodeAtAge").Should().Be(OkOf(Integer(42)));
    }

    // ===== Json.Decode: maybe =====

    [Fact]
    public void Decode_maybe_present()
    {
        CallThunk("decodeMaybePresent").Should().Be(OkOf(JustOf(Integer(42))));
    }

    [Fact]
    public void Decode_maybe_wrong_type()
    {
        CallThunk("decodeMaybeWrongType").Should().Be(OkOf(s_nothing));
    }

    [Fact]
    public void Decode_maybe_missing()
    {
        CallThunk("decodeMaybeMissing").Should().Be(OkOf(s_nothing));
    }

    // ===== Json.Decode: keyValuePairs =====

    [Fact]
    public void Decode_keyValuePairs()
    {
        var result = CallThunk("decodeKeyValuePairs");
        result.Should().BeOfType<ElmValue.ElmTag>();
        ((ElmValue.ElmTag)result).TagName.Should().Be("Ok");
        // The result should be a list of (String, Int) tuples
        var okArgs = ((ElmValue.ElmTag)result).Arguments;
        okArgs.Should().HaveCount(1);
        var list = okArgs[0];
        list.Should().BeOfType<ElmValue.ElmList>();
        ((ElmValue.ElmList)list).Items.Should().HaveCount(2);
    }

    [Fact]
    public void Decode_keyValuePairs_empty()
    {
        CallThunk("decodeKeyValuePairsEmpty").Should().Be(OkOf(ElmList()));
    }

    // ===== Json.Decode: dict =====

    [Fact]
    public void Decode_dict_get()
    {
        CallThunk("decodeDictGet").Should().Be(JustOf(Integer(42)));
    }

    [Fact]
    public void Decode_dict_size()
    {
        CallThunk("decodeDictSize").Should().Be(Integer(2));
    }

    // ===== Json.Decode: oneOrMore =====

    [Fact]
    public void Decode_oneOrMore_success()
    {
        CallThunk("decodeOneOrMoreSuccess").Should().Be(OkOf(ElmList(Integer(1), Integer(2), Integer(3))));
    }

    [Fact]
    public void Decode_oneOrMore_fail()
    {
        CallThunk("decodeOneOrMoreFail").Should().Be(s_true);
    }

    // ===== Json.Decode: string escapes =====

    [Fact]
    public void Decode_string_escape()
    {
        CallThunk("decodeStringEscape").Should().Be(OkOf(String("hello\nworld")));
    }

    // ===== Json.Decode: nested array =====

    [Fact]
    public void Decode_nested_array()
    {
        CallThunk("decodeNestedArray").Should().Be(
            OkOf(ElmList(
                ElmList(Integer(1), Integer(2)),
                ElmList(Integer(3), Integer(4)))));
    }

    // ===== Roundtrip: encode then decode =====

    [Fact]
    public void Roundtrip_encode_decode_int()
    {
        CallThunk("encodeDecodeRoundtrip").Should().Be(OkOf(Integer(42)));
    }

    [Fact]
    public void Roundtrip_encode_decode_string()
    {
        ApplyUnary(GetTestFunction("encodeDecodeRoundtripString"), String("hello"))
            .Should().Be(OkOf(String("hello")));
    }

    [Fact]
    public void Roundtrip_encode_decode_string_empty()
    {
        ApplyUnary(GetTestFunction("encodeDecodeRoundtripString"), String(""))
            .Should().Be(OkOf(String("")));
    }

    [Fact]
    public void Roundtrip_encode_decode_object()
    {
        CallThunk("encodeDecodeRoundtripObject").Should().Be(OkOf(String("Tom")));
    }

    // ===== Json.Decode: negative int =====

    [Fact]
    public void Decode_negative_int()
    {
        CallThunk("decodeNegativeInt").Should().Be(OkOf(Integer(-7)));
    }

    // ===== Json.Decode: whitespace =====

    [Fact]
    public void Decode_with_whitespace()
    {
        CallThunk("decodeWithWhitespace").Should().Be(OkOf(Integer(42)));
    }

    // ===== Json.Decode: map2 sum =====

    [Fact]
    public void Decode_map2_sum()
    {
        CallThunk("decodeMap2Fields").Should().Be(OkOf(Integer(30)));
    }

    // ===== Json.Decode: decodeValue =====

    [Fact]
    public void Decode_value_direct()
    {
        CallThunk("decodeValueDirect").Should().Be(OkOf(Integer(42)));
    }

    [Fact]
    public void Decode_value_direct_string()
    {
        CallThunk("decodeValueDirectString").Should().Be(OkOf(String("hello")));
    }

    // ===== Json.Decode: large integer =====

    [Fact]
    public void Decode_large_int()
    {
        CallThunk("decodeLargeInt").Should().Be(OkOf(Integer(1000000)));
    }

    // ===== Json.Decode: nested field =====

    [Fact]
    public void Decode_nested_field()
    {
        CallThunk("decodeNestedField").Should().Be(OkOf(Integer(99)));
    }
}
