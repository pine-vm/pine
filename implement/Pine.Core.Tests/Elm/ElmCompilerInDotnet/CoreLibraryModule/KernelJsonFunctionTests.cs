using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.Elm;
using Pine.Core.Elm.ElmCompilerInDotnet;
using Pine.Core.Interpreter.IntermediateVM;
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


        -- ====== parseJsonStringToValue wrapper ======

        parseJsonToValue : String -> Result String Json.Encode.Value
        parseJsonToValue s =
            Json.Decode.parseJsonStringToValue s


        -- ====== Generic encode wrapper ======

        encodeValue : Json.Encode.Value -> String
        encodeValue val =
            Json.Encode.encode 0 val


        -- ====== Encode-decode roundtrip ======

        encodeDecodeRoundtrip : Json.Encode.Value -> Result String Json.Encode.Value
        encodeDecodeRoundtrip val =
            Json.Decode.parseJsonStringToValue (Json.Encode.encode 0 val)


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


        -- ====== Generic decoder wrappers ======

        decodeStringInt : String -> Result Json.Decode.Error Int
        decodeStringInt json =
            Json.Decode.decodeString Json.Decode.int json

        decodeStringString : String -> Result Json.Decode.Error String
        decodeStringString json =
            Json.Decode.decodeString Json.Decode.string json

        decodeStringBool : String -> Result Json.Decode.Error Bool
        decodeStringBool json =
            Json.Decode.decodeString Json.Decode.bool json

        decodeStringFloat : String -> Result Json.Decode.Error Float
        decodeStringFloat json =
            Json.Decode.decodeString Json.Decode.float json

        decodeNullBool : String -> Result Json.Decode.Error Bool
        decodeNullBool json =
            Json.Decode.decodeString (Json.Decode.null False) json

        decodeNullInt : String -> Result Json.Decode.Error Int
        decodeNullInt json =
            Json.Decode.decodeString (Json.Decode.null 42) json

        decodeSucceed42 : String -> Result Json.Decode.Error Int
        decodeSucceed42 json =
            Json.Decode.decodeString (Json.Decode.succeed 42) json

        decodeFail : String -> Result Json.Decode.Error Int
        decodeFail json =
            Json.Decode.decodeString (Json.Decode.fail "my error") json

        decodeNullableInt : String -> Result Json.Decode.Error (Maybe Int)
        decodeNullableInt json =
            Json.Decode.decodeString (Json.Decode.nullable Json.Decode.int) json

        decodeListInt : String -> Result Json.Decode.Error (List Int)
        decodeListInt json =
            Json.Decode.decodeString (Json.Decode.list Json.Decode.int) json

        decodeListBool : String -> Result Json.Decode.Error (List Bool)
        decodeListBool json =
            Json.Decode.decodeString (Json.Decode.list Json.Decode.bool) json

        decodeFieldInt : String -> String -> Result Json.Decode.Error Int
        decodeFieldInt fieldName json =
            Json.Decode.decodeString (Json.Decode.field fieldName Json.Decode.int) json

        decodeFieldString : String -> String -> Result Json.Decode.Error String
        decodeFieldString fieldName json =
            Json.Decode.decodeString (Json.Decode.field fieldName Json.Decode.string) json

        decodeIndexString : Int -> String -> Result Json.Decode.Error String
        decodeIndexString idx json =
            Json.Decode.decodeString (Json.Decode.index idx Json.Decode.string) json

        decodeMapStringLength : String -> Result Json.Decode.Error Int
        decodeMapStringLength json =
            Json.Decode.decodeString (Json.Decode.map String.length Json.Decode.string) json

        decodeMap2Point : String -> Result Json.Decode.Error { x : Int, y : Int }
        decodeMap2Point json =
            let
                decoder =
                    Json.Decode.map2 (\x y -> { x = x, y = y })
                        (Json.Decode.field "x" Json.Decode.int)
                        (Json.Decode.field "y" Json.Decode.int)
            in
            Json.Decode.decodeString decoder json

        decodeMap3Person : String -> Result Json.Decode.Error { name : String, age : Int, height : Int }
        decodeMap3Person json =
            let
                decoder =
                    Json.Decode.map3 (\n a h -> { name = n, age = a, height = h })
                        (Json.Decode.field "name" Json.Decode.string)
                        (Json.Decode.field "age" Json.Decode.int)
                        (Json.Decode.field "height" Json.Decode.int)
            in
            Json.Decode.decodeString decoder json

        versionHelp : Int -> Json.Decode.Decoder String
        versionHelp version =
            case version of
                3 ->
                    Json.Decode.field "data" Json.Decode.string

                _ ->
                    Json.Decode.fail "unsupported version"

        decodeAndThenVersion : String -> Result Json.Decode.Error String
        decodeAndThenVersion json =
            let
                decoder =
                    Json.Decode.field "version" Json.Decode.int
                        |> Json.Decode.andThen versionHelp
            in
            Json.Decode.decodeString decoder json

        decodeBadIntList : String -> Result Json.Decode.Error (List Int)
        decodeBadIntList json =
            let
                badInt =
                    Json.Decode.oneOf [ Json.Decode.int, Json.Decode.null 0 ]
            in
            Json.Decode.decodeString (Json.Decode.list badInt) json

        decodeOneOfEmpty : String -> Result Json.Decode.Error Int
        decodeOneOfEmpty json =
            Json.Decode.decodeString (Json.Decode.oneOf []) json

        decodeOneOfIntNull : String -> Result Json.Decode.Error Int
        decodeOneOfIntNull json =
            Json.Decode.decodeString (Json.Decode.oneOf [ Json.Decode.int, Json.Decode.null 0 ]) json

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

        decodeLazyCommentMessage : String -> Result Json.Decode.Error String
        decodeLazyCommentMessage json =
            case Json.Decode.decodeString commentDecoder json of
                Ok comment ->
                    Ok comment.message

                Err err ->
                    Err err

        decodeLazyCommentResponseCount : String -> Result Json.Decode.Error Int
        decodeLazyCommentResponseCount json =
            case Json.Decode.decodeString commentDecoder json of
                Ok comment ->
                    Ok (List.length comment.responses)

                Err err ->
                    Err err

        decodeAtString : List String -> String -> Result Json.Decode.Error String
        decodeAtString path json =
            Json.Decode.decodeString (Json.Decode.at path Json.Decode.string) json

        decodeAtInt : List String -> String -> Result Json.Decode.Error Int
        decodeAtInt path json =
            Json.Decode.decodeString (Json.Decode.at path Json.Decode.int) json

        decodeMaybeFieldInt : String -> String -> Result Json.Decode.Error (Maybe Int)
        decodeMaybeFieldInt fieldName json =
            Json.Decode.decodeString (Json.Decode.maybe (Json.Decode.field fieldName Json.Decode.int)) json

        decodeKeyValuePairsInt : String -> Result Json.Decode.Error (List ( String, Int ))
        decodeKeyValuePairsInt json =
            Json.Decode.decodeString (Json.Decode.keyValuePairs Json.Decode.int) json

        decodeDictGetInt : String -> String -> Maybe Int
        decodeDictGetInt key json =
            case Json.Decode.decodeString (Json.Decode.dict Json.Decode.int) json of
                Ok d ->
                    Dict.get key d

                Err _ ->
                    Nothing

        decodeDictSizeInt : String -> Int
        decodeDictSizeInt json =
            case Json.Decode.decodeString (Json.Decode.dict Json.Decode.int) json of
                Ok d ->
                    Dict.size d

                Err _ ->
                    -1

        decodeOneOrMoreListInt : String -> Result Json.Decode.Error (List Int)
        decodeOneOrMoreListInt json =
            let
                decoder =
                    Json.Decode.oneOrMore (\first rest -> first :: rest) Json.Decode.int
            in
            Json.Decode.decodeString decoder json

        decodeNestedListInt : String -> Result Json.Decode.Error (List (List Int))
        decodeNestedListInt json =
            Json.Decode.decodeString (Json.Decode.list (Json.Decode.list Json.Decode.int)) json

        decodeMap2Sum : String -> Result Json.Decode.Error Int
        decodeMap2Sum json =
            let
                decoder =
                    Json.Decode.map2 (\a b -> a + b)
                        (Json.Decode.field "x" Json.Decode.int)
                        (Json.Decode.field "y" Json.Decode.int)
            in
            Json.Decode.decodeString decoder json

        decodeValueInt : Json.Encode.Value -> Result Json.Decode.Error Int
        decodeValueInt val =
            Json.Decode.decodeValue Json.Decode.int val

        decodeValueString : Json.Encode.Value -> Result Json.Decode.Error String
        decodeValueString val =
            Json.Decode.decodeValue Json.Decode.string val
        """";

    private static readonly Lazy<ElmInteractiveEnvironment.ParsedInteractiveEnvironment> s_env =
        new(
            () =>
            {
                var kernelModulesTree =
                    BundledFiles.ElmKernelModulesDefault.Value;

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
                    .Map(r => r.compiledEnvValue)
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

    private static ElmValue ApplyBinary(PineValue functionValue, ElmValue arg1, ElmValue arg2) =>
        CoreLibraryTestHelper.ApplyBinary(functionValue, arg1, arg2, s_vm);

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

    // ===== Json.Encode.Value construction helpers =====

    private static readonly ElmValue s_jsonNull = ElmValue.TagInstance("NullValue", []);

    private static ElmValue JsonInt(long n) =>
        ElmValue.TagInstance("IntValue", [Integer(n)]);

    private static ElmValue JsonBool(ElmValue b) =>
        ElmValue.TagInstance("BoolValue", [b]);

    private static ElmValue JsonString(string s) =>
        ElmValue.TagInstance("StringValue", [String(s)]);

    private static ElmValue JsonFloat(string s) =>
        ElmValue.TagInstance("FloatValue", [String(s)]);

    private static ElmValue JsonArray(params ElmValue[] items) =>
        ElmValue.TagInstance("ArrayValue", [ElmList(items)]);

    private static ElmValue JsonObject(params ElmValue[] entries) =>
        ElmValue.TagInstance("ObjectValue", [ElmList(entries)]);

    private static ElmValue JsonEntry(string key, ElmValue value) =>
        ElmValue.ListInstance([String(key), value]);

    private static ElmValue ParseJson(string json) =>
        ApplyUnary(GetTestFunction("parseJsonToValue"), String(json));

    private static ElmValue EncodeValue(ElmValue jsonValue) =>
        ApplyUnary(GetTestFunction("encodeValue"), jsonValue);

    private static ElmValue EncodeDecodeRoundtrip(ElmValue jsonValue) =>
        ApplyUnary(GetTestFunction("encodeDecodeRoundtrip"), jsonValue);

    private static void AssertIsErr(ElmValue result)
    {
        result.Should().BeOfType<ElmValue.ElmTag>();
        ((ElmValue.ElmTag)result).TagName.Should().Be("Err");
    }

    // ===== Generic decoder call helpers =====

    private static ElmValue DecodeStringInt(string json) =>
        ApplyUnary(GetTestFunction("decodeStringInt"), String(json));

    private static ElmValue DecodeStringString(string json) =>
        ApplyUnary(GetTestFunction("decodeStringString"), String(json));

    private static ElmValue DecodeStringBool(string json) =>
        ApplyUnary(GetTestFunction("decodeStringBool"), String(json));

    private static ElmValue DecodeStringFloat(string json) =>
        ApplyUnary(GetTestFunction("decodeStringFloat"), String(json));

    private static ElmValue DecodeNullBool(string json) =>
        ApplyUnary(GetTestFunction("decodeNullBool"), String(json));

    private static ElmValue DecodeNullInt(string json) =>
        ApplyUnary(GetTestFunction("decodeNullInt"), String(json));

    private static ElmValue DecodeSucceed42(string json) =>
        ApplyUnary(GetTestFunction("decodeSucceed42"), String(json));

    private static ElmValue DecodeFail(string json) =>
        ApplyUnary(GetTestFunction("decodeFail"), String(json));

    private static ElmValue DecodeNullableInt(string json) =>
        ApplyUnary(GetTestFunction("decodeNullableInt"), String(json));

    private static ElmValue DecodeListInt(string json) =>
        ApplyUnary(GetTestFunction("decodeListInt"), String(json));

    private static ElmValue DecodeListBool(string json) =>
        ApplyUnary(GetTestFunction("decodeListBool"), String(json));

    private static ElmValue DecodeFieldInt(string fieldName, string json) =>
        ApplyBinary(GetTestFunction("decodeFieldInt"), String(fieldName), String(json));

    private static ElmValue DecodeFieldString(string fieldName, string json) =>
        ApplyBinary(GetTestFunction("decodeFieldString"), String(fieldName), String(json));

    private static ElmValue DecodeIndexString(long index, string json) =>
        ApplyBinary(GetTestFunction("decodeIndexString"), Integer(index), String(json));

    private static ElmValue DecodeMapStringLength(string json) =>
        ApplyUnary(GetTestFunction("decodeMapStringLength"), String(json));

    private static ElmValue DecodeMap2Point(string json) =>
        ApplyUnary(GetTestFunction("decodeMap2Point"), String(json));

    private static ElmValue DecodeMap3Person(string json) =>
        ApplyUnary(GetTestFunction("decodeMap3Person"), String(json));

    private static ElmValue DecodeAndThenVersion(string json) =>
        ApplyUnary(GetTestFunction("decodeAndThenVersion"), String(json));

    private static ElmValue DecodeBadIntList(string json) =>
        ApplyUnary(GetTestFunction("decodeBadIntList"), String(json));

    private static ElmValue DecodeOneOfEmpty(string json) =>
        ApplyUnary(GetTestFunction("decodeOneOfEmpty"), String(json));

    private static ElmValue DecodeOneOfIntNull(string json) =>
        ApplyUnary(GetTestFunction("decodeOneOfIntNull"), String(json));

    private static ElmValue DecodeLazyCommentMessage(string json) =>
        ApplyUnary(GetTestFunction("decodeLazyCommentMessage"), String(json));

    private static ElmValue DecodeLazyCommentResponseCount(string json) =>
        ApplyUnary(GetTestFunction("decodeLazyCommentResponseCount"), String(json));

    private static ElmValue DecodeAtString(ElmValue path, string json) =>
        ApplyBinary(GetTestFunction("decodeAtString"), path, String(json));

    private static ElmValue DecodeAtInt(ElmValue path, string json) =>
        ApplyBinary(GetTestFunction("decodeAtInt"), path, String(json));

    private static ElmValue DecodeMaybeFieldInt(string fieldName, string json) =>
        ApplyBinary(GetTestFunction("decodeMaybeFieldInt"), String(fieldName), String(json));

    private static ElmValue DecodeKeyValuePairsInt(string json) =>
        ApplyUnary(GetTestFunction("decodeKeyValuePairsInt"), String(json));

    private static ElmValue DecodeDictGetInt(string key, string json) =>
        ApplyBinary(GetTestFunction("decodeDictGetInt"), String(key), String(json));

    private static ElmValue DecodeDictSizeInt(string json) =>
        ApplyUnary(GetTestFunction("decodeDictSizeInt"), String(json));

    private static ElmValue DecodeOneOrMoreListInt(string json) =>
        ApplyUnary(GetTestFunction("decodeOneOrMoreListInt"), String(json));

    private static ElmValue DecodeNestedListInt(string json) =>
        ApplyUnary(GetTestFunction("decodeNestedListInt"), String(json));

    private static ElmValue DecodeMap2Sum(string json) =>
        ApplyUnary(GetTestFunction("decodeMap2Sum"), String(json));

    private static ElmValue DecodeValueInt(ElmValue value) =>
        ApplyUnary(GetTestFunction("decodeValueInt"), value);

    private static ElmValue DecodeValueString(ElmValue value) =>
        ApplyUnary(GetTestFunction("decodeValueString"), value);

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

    // ===== parseJsonStringToValue: error cases =====

    [Fact]
    public void ParseJson_empty_string()
    {
        AssertIsErr(ParseJson(""));
    }

    [Fact]
    public void ParseJson_invalid_text()
    {
        AssertIsErr(ParseJson("invalid"));
    }

    [Fact]
    public void ParseJson_trailing_text()
    {
        AssertIsErr(ParseJson("42 extra"));
    }

    [Fact]
    public void ParseJson_just_whitespace()
    {
        AssertIsErr(ParseJson("   "));
    }

    [Fact]
    public void ParseJson_unclosed_string()
    {
        AssertIsErr(ParseJson("\"hello"));
    }

    [Fact]
    public void ParseJson_unclosed_array()
    {
        AssertIsErr(ParseJson("[1, 2"));
    }

    [Fact]
    public void ParseJson_unclosed_object()
    {
        AssertIsErr(ParseJson("{\"x\": 1"));
    }

    // ===== parseJsonStringToValue: success cases =====

    [Fact]
    public void ParseJson_null()
    {
        ParseJson("null").Should().Be(OkOf(s_jsonNull));
    }

    [Fact]
    public void ParseJson_true()
    {
        ParseJson("true").Should().Be(OkOf(JsonBool(s_true)));
    }

    [Fact]
    public void ParseJson_false()
    {
        ParseJson("false").Should().Be(OkOf(JsonBool(s_false)));
    }

    [Fact]
    public void ParseJson_int_zero()
    {
        ParseJson("0").Should().Be(OkOf(JsonInt(0)));
    }

    [Fact]
    public void ParseJson_int_positive()
    {
        ParseJson("123").Should().Be(OkOf(JsonInt(123)));
    }

    [Fact]
    public void ParseJson_int_negative()
    {
        ParseJson("-7").Should().Be(OkOf(JsonInt(-7)));
    }

    [Fact]
    public void ParseJson_int_large()
    {
        ParseJson("1000000").Should().Be(OkOf(JsonInt(1000000)));
    }

    [Fact]
    public void ParseJson_float()
    {
        ParseJson("3.14").Should().Be(OkOf(JsonFloat("3.14")));
    }

    [Fact]
    public void ParseJson_string_empty()
    {
        ParseJson("\"\"").Should().Be(OkOf(JsonString("")));
    }

    [Fact]
    public void ParseJson_string_hello()
    {
        ParseJson("\"hello\"").Should().Be(OkOf(JsonString("hello")));
    }

    [Fact]
    public void ParseJson_string_with_escape()
    {
        ParseJson("\"hello\\nworld\"").Should().Be(OkOf(JsonString("hello\nworld")));
    }

    [Fact]
    public void ParseJson_array_empty()
    {
        ParseJson("[]").Should().Be(OkOf(JsonArray()));
    }

    [Fact]
    public void ParseJson_array_ints()
    {
        ParseJson("[1,2,3]").Should().Be(OkOf(JsonArray(JsonInt(1), JsonInt(2), JsonInt(3))));
    }

    [Fact]
    public void ParseJson_array_mixed()
    {
        ParseJson("[true,null,42]").Should().Be(
            OkOf(JsonArray(JsonBool(s_true), s_jsonNull, JsonInt(42))));
    }

    [Fact]
    public void ParseJson_array_nested()
    {
        ParseJson("[[1,2],[3]]").Should().Be(
            OkOf(
                JsonArray(
                    JsonArray(JsonInt(1), JsonInt(2)),
                    JsonArray(JsonInt(3)))));
    }

    [Fact]
    public void ParseJson_object_empty()
    {
        ParseJson("{}").Should().Be(OkOf(JsonObject()));
    }

    [Fact]
    public void ParseJson_object_single()
    {
        ParseJson("{\"x\":1}").Should().Be(
            OkOf(JsonObject(JsonEntry("x", JsonInt(1)))));
    }

    [Fact]
    public void ParseJson_object_multiple()
    {
        ParseJson("{\"name\":\"Tom\",\"age\":42}").Should().Be(
            OkOf(
                JsonObject(
                    JsonEntry("name", JsonString("Tom")),
                    JsonEntry("age", JsonInt(42)))));
    }

    [Fact]
    public void ParseJson_object_nested()
    {
        ParseJson("{\"a\":{\"b\":2}}").Should().Be(
            OkOf(
                JsonObject(
                    JsonEntry("a", JsonObject(JsonEntry("b", JsonInt(2)))))));
    }

    [Fact]
    public void ParseJson_with_whitespace()
    {
        ParseJson("  42  ").Should().Be(OkOf(JsonInt(42)));
    }

    // ===== Json.Encode via encodeValue =====

    [Fact]
    public void Encode_string_empty()
    {
        EncodeValue(JsonString("")).Should().Be(String("\"\""));
    }

    [Fact]
    public void Encode_string_abc()
    {
        EncodeValue(JsonString("abc")).Should().Be(String("\"abc\""));
    }

    [Fact]
    public void Encode_string_hello()
    {
        EncodeValue(JsonString("hello")).Should().Be(String("\"hello\""));
    }

    [Fact]
    public void Encode_int_42()
    {
        EncodeValue(JsonInt(42)).Should().Be(String("42"));
    }

    [Fact]
    public void Encode_int_neg7()
    {
        EncodeValue(JsonInt(-7)).Should().Be(String("-7"));
    }

    [Fact]
    public void Encode_int_0()
    {
        EncodeValue(JsonInt(0)).Should().Be(String("0"));
    }

    [Fact]
    public void Encode_bool_true()
    {
        EncodeValue(JsonBool(s_true)).Should().Be(String("true"));
    }

    [Fact]
    public void Encode_bool_false()
    {
        EncodeValue(JsonBool(s_false)).Should().Be(String("false"));
    }

    [Fact]
    public void Encode_null()
    {
        EncodeValue(s_jsonNull).Should().Be(String("null"));
    }

    [Fact]
    public void Encode_list_int()
    {
        EncodeValue(JsonArray(JsonInt(1), JsonInt(3), JsonInt(4))).Should().Be(String("[1,3,4]"));
    }

    [Fact]
    public void Encode_list_bool()
    {
        EncodeValue(JsonArray(JsonBool(s_true), JsonBool(s_false))).Should().Be(String("[true,false]"));
    }

    [Fact]
    public void Encode_list_string()
    {
        EncodeValue(JsonArray(JsonString("a"), JsonString("b"))).Should().Be(String("[\"a\",\"b\"]"));
    }

    [Fact]
    public void Encode_list_empty()
    {
        EncodeValue(JsonArray()).Should().Be(String("[]"));
    }

    [Fact]
    public void Encode_object()
    {
        EncodeValue(
            JsonObject(
                JsonEntry("name", JsonString("Tom")),
                JsonEntry("age", JsonInt(42))))
            .Should().Be(String("{\"name\":\"Tom\",\"age\":42}"));
    }

    [Fact]
    public void Encode_object_empty()
    {
        EncodeValue(JsonObject()).Should().Be(String("{}"));
    }

    [Fact]
    public void Encode_nested_object()
    {
        EncodeValue(
            JsonObject(
                JsonEntry(
                    "x",
                    JsonObject(
                        JsonEntry("y", JsonInt(1))))))
            .Should().Be(String("{\"x\":{\"y\":1}}"));
    }

    [Fact]
    public void Encode_string_with_quote()
    {
        EncodeValue(JsonString("say \"hi\"")).Should().Be(String("\"say \\\"hi\\\"\""));
    }

    [Fact]
    public void Encode_string_with_newline()
    {
        EncodeValue(JsonString("line1\nline2")).Should().Be(String("\"line1\\nline2\""));
    }

    [Fact]
    public void Encode_string_with_backslash()
    {
        EncodeValue(JsonString("a\\b")).Should().Be(String("\"a\\\\b\""));
    }

    [Fact]
    public void Encode_float_314()
    {
        EncodeValue(JsonFloat("3.14")).Should().Be(String("3.14"));
    }

    // ===== Json.Decode: decodeString int =====

    [Fact]
    public void Decode_string_int_4()
    {
        DecodeStringInt("4").Should().Be(OkOf(Integer(4)));
    }

    [Fact]
    public void Decode_string_int_42()
    {
        DecodeStringInt("42").Should().Be(OkOf(Integer(42)));
    }

    [Fact]
    public void Decode_string_int_invalid()
    {
        AssertIsErr(DecodeStringInt("1 + 2"));
    }

    [Fact]
    public void Decode_string_int_from_bool()
    {
        AssertIsErr(DecodeStringInt("true"));
    }

    // ===== Json.Decode: decodeString string =====

    [Fact]
    public void Decode_string_hello()
    {
        DecodeStringString("\"hello\"").Should().Be(OkOf(String("hello")));
    }

    [Fact]
    public void Decode_string_from_bool()
    {
        AssertIsErr(DecodeStringString("true"));
    }

    // ===== Json.Decode: decodeString bool =====

    [Fact]
    public void Decode_bool_true()
    {
        DecodeStringBool("true").Should().Be(OkOf(s_true));
    }

    [Fact]
    public void Decode_bool_from_int()
    {
        AssertIsErr(DecodeStringBool("42"));
    }

    // ===== Json.Decode: decodeString float =====

    [Fact]
    public void Decode_float_from_int()
    {
        // In the Elm runtime, toFloat on an integer value may return ElmInteger
        // since the rational representation 42/1 normalizes to an integer.
        var result = DecodeStringFloat("42");
        result.Should().Be(OkOf(Integer(42)));
    }

    [Fact]
    public void Decode_float_314()
    {
        var result = DecodeStringFloat("3.14");
        result.Should().Be(OkOf(ElmValue.ElmFloat.Convert(3.14)));
    }

    [Fact]
    public void Decode_float_from_bool()
    {
        AssertIsErr(DecodeStringFloat("true"));
    }

    // ===== Json.Decode: null =====

    [Fact]
    public void Decode_null_false()
    {
        DecodeNullBool("null").Should().Be(OkOf(s_false));
    }

    [Fact]
    public void Decode_null_42()
    {
        DecodeNullInt("null").Should().Be(OkOf(Integer(42)));
    }

    [Fact]
    public void Decode_null_from_int()
    {
        AssertIsErr(DecodeNullInt("42"));
    }

    // ===== Json.Decode: succeed =====

    [Fact]
    public void Decode_succeed_42_true()
    {
        DecodeSucceed42("true").Should().Be(OkOf(Integer(42)));
    }

    [Fact]
    public void Decode_succeed_42_array()
    {
        DecodeSucceed42("[1,2,3]").Should().Be(OkOf(Integer(42)));
    }

    // ===== Json.Decode: fail =====

    [Fact]
    public void Decode_fail_always()
    {
        AssertIsErr(DecodeFail("42"));
    }

    // ===== Json.Decode: value (replaced with parseJsonToValue) =====

    [Fact]
    public void Decode_value_passthrough()
    {
        ParseJson("true").Should().Be(OkOf(JsonBool(s_true)));
    }

    // ===== Json.Decode: nullable =====

    [Fact]
    public void Decode_nullable_int_13()
    {
        DecodeNullableInt("13").Should().Be(OkOf(JustOf(Integer(13))));
    }

    [Fact]
    public void Decode_nullable_int_42()
    {
        DecodeNullableInt("42").Should().Be(OkOf(JustOf(Integer(42))));
    }

    [Fact]
    public void Decode_nullable_int_null()
    {
        DecodeNullableInt("null").Should().Be(OkOf(s_nothing));
    }

    [Fact]
    public void Decode_nullable_int_bool()
    {
        AssertIsErr(DecodeNullableInt("true"));
    }

    // ===== Json.Decode: list =====

    [Fact]
    public void Decode_list_int()
    {
        DecodeListInt("[1,2,3]").Should().Be(OkOf(ElmList(Integer(1), Integer(2), Integer(3))));
    }

    [Fact]
    public void Decode_list_bool()
    {
        DecodeListBool("[true,false]").Should().Be(OkOf(ElmList(s_true, s_false)));
    }

    [Fact]
    public void Decode_list_empty()
    {
        DecodeListInt("[]").Should().Be(OkOf(ElmList()));
    }

    // ===== Json.Decode: field =====

    [Fact]
    public void Decode_field_x_3()
    {
        DecodeFieldInt("x", "{ \"x\": 3 }").Should().Be(OkOf(Integer(3)));
    }

    [Fact]
    public void Decode_field_x_with_y()
    {
        DecodeFieldInt("x", "{ \"x\": 3, \"y\": 4 }").Should().Be(OkOf(Integer(3)));
    }

    [Fact]
    public void Decode_field_name()
    {
        DecodeFieldString("name", "{ \"name\": \"tom\" }").Should().Be(OkOf(String("tom")));
    }

    [Fact]
    public void Decode_field_missing()
    {
        AssertIsErr(DecodeFieldInt("x", "{ \"y\": 4 }"));
    }

    // ===== Json.Decode: index =====

    [Fact]
    public void Decode_index_0()
    {
        DecodeIndexString(0, "[\"alice\",\"bob\",\"chuck\"]").Should().Be(OkOf(String("alice")));
    }

    [Fact]
    public void Decode_index_1()
    {
        DecodeIndexString(1, "[\"alice\",\"bob\",\"chuck\"]").Should().Be(OkOf(String("bob")));
    }

    [Fact]
    public void Decode_index_2()
    {
        DecodeIndexString(2, "[\"alice\",\"bob\",\"chuck\"]").Should().Be(OkOf(String("chuck")));
    }

    [Fact]
    public void Decode_index_out_of_bounds()
    {
        AssertIsErr(DecodeIndexString(3, "[\"alice\",\"bob\",\"chuck\"]"));
    }

    // ===== Json.Decode: map =====

    [Fact]
    public void Decode_map_string_length()
    {
        DecodeMapStringLength("\"hello\"").Should().Be(OkOf(Integer(5)));
    }

    // ===== Json.Decode: map2 =====

    [Fact]
    public void Decode_map2_point()
    {
        DecodeMap2Point("{ \"x\": 3, \"y\": 4 }")
            .Should().Be(OkOf(new ElmValue.ElmRecord([("x", Integer(3)), ("y", Integer(4))])));
    }

    // ===== Json.Decode: map3 =====

    [Fact]
    public void Decode_map3()
    {
        var (value, report) =
            CoreLibraryTestHelper.ApplyAndProfileUnary(
                GetTestFunction("decodeMap3Person"),
                String("{ \"name\": \"tom\", \"age\": 42, \"height\": 180 }"),
                s_vm);

        value.Should().Be(
            OkOf(
                new ElmValue.ElmRecord([("age", Integer(42)), ("height", Integer(180)), ("name", String("tom"))])));

        PerformanceCountersFormatting.FormatCounts(report).Should().Be(
            """
            InstructionCount: 2_382
            InvocationCount: 88
            BuildListCount: 154
            LoopIterationCount: 0
            """);
    }

    // ===== Json.Decode: andThen =====

    [Fact]
    public void Decode_andThen_version()
    {
        var (value, report) =
            CoreLibraryTestHelper.ApplyAndProfileUnary(
                GetTestFunction("decodeAndThenVersion"),
                String("{ \"version\": 3, \"data\": \"v3-data\" }"),
                s_vm);

        value.Should().Be(OkOf(String("v3-data")));

        PerformanceCountersFormatting.FormatCounts(report).Should().Be(
            """
            InstructionCount: 1_414
            InvocationCount: 51
            BuildListCount: 87
            LoopIterationCount: 0
            """);
    }

    [Fact]
    public void Decode_andThen_version_fail()
    {
        AssertIsErr(DecodeAndThenVersion("{ \"version\": 99 }"));
    }

    // ===== Json.Decode: oneOf =====

    [Fact]
    public void Decode_oneOf_badInt()
    {
        DecodeBadIntList("[1,2,null,4]").Should().Be(OkOf(ElmList(Integer(1), Integer(2), Integer(0), Integer(4))));
    }

    [Fact]
    public void Decode_oneOf_empty()
    {
        AssertIsErr(DecodeOneOfEmpty("42"));
    }

    [Fact]
    public void Decode_oneOf_first()
    {
        DecodeOneOfIntNull("42").Should().Be(OkOf(Integer(42)));
    }

    [Fact]
    public void Decode_oneOf_second()
    {
        DecodeOneOfIntNull("null").Should().Be(OkOf(Integer(0)));
    }

    // ===== Json.Decode: lazy =====

    [Fact]
    public void Decode_lazy_recursive()
    {
        var (value, report) =
            CoreLibraryTestHelper.ApplyAndProfileUnary(
                GetTestFunction("decodeLazyCommentMessage"),
                String("{ \"message\": \"hello\", \"responses\": [] }"),
                s_vm);

        value.Should().Be(OkOf(String("hello")));

        var formattedCounts =
            PerformanceCountersFormatting.FormatCounts(report);

        formattedCounts.Should().Be(
            """
            InstructionCount: 1_611
            InvocationCount: 62
            BuildListCount: 109
            LoopIterationCount: 0
            """);
    }

    [Fact]
    public void Decode_lazy_nested()
    {
        var (value, report) =
            CoreLibraryTestHelper.ApplyAndProfileUnary(
                GetTestFunction("decodeLazyCommentResponseCount"),
                String("{ \"message\": \"a\", \"responses\": [{ \"message\": \"b\", \"responses\": [] }] }"),
                s_vm);

        value.Should().Be(OkOf(Integer(1)));

        var formattedCounts =
            PerformanceCountersFormatting.FormatCounts(report);

        formattedCounts.Should().Be(
            """
            InstructionCount: 3_072
            InvocationCount: 116
            BuildListCount: 211
            LoopIterationCount: 0
            """);
    }

    // ===== Json.Decode: at =====

    [Fact]
    public void Decode_at_person_name()
    {
        var (value, report) =
            CoreLibraryTestHelper.ApplyAndProfileBinary(
                GetTestFunction("decodeAtString"),
                ElmList(String("person"), String("name")),
                String("{ \"person\": { \"name\": \"tom\", \"age\": 42 } }"),
                s_vm);

        value.Should().Be(OkOf(String("tom")));

        PerformanceCountersFormatting.FormatCounts(report).Should().Be(
            """
            InstructionCount: 1_598
            InvocationCount: 54
            BuildListCount: 99
            LoopIterationCount: 0
            """);
    }

    [Fact]
    public void Decode_at_person_age()
    {
        var (value, report) =
            CoreLibraryTestHelper.ApplyAndProfileBinary(
                GetTestFunction("decodeAtInt"),
                ElmList(String("person"), String("age")),
                String("{ \"person\": { \"name\": \"tom\", \"age\": 42 } }"),
                s_vm);

        value.Should().Be(OkOf(Integer(42)));

        PerformanceCountersFormatting.FormatCounts(report).Should().Be(
            """
            InstructionCount: 1_609
            InvocationCount: 54
            BuildListCount: 99
            LoopIterationCount: 0
            """);
    }

    // ===== Json.Decode: maybe =====

    [Fact]
    public void Decode_maybe_present()
    {
        DecodeMaybeFieldInt("age", "{ \"name\": \"tom\", \"age\": 42 }").Should().Be(OkOf(JustOf(Integer(42))));
    }

    [Fact]
    public void Decode_maybe_wrong_type()
    {
        DecodeMaybeFieldInt("name", "{ \"name\": \"tom\", \"age\": 42 }").Should().Be(OkOf(s_nothing));
    }

    [Fact]
    public void Decode_maybe_missing()
    {
        DecodeMaybeFieldInt("height", "{ \"name\": \"tom\", \"age\": 42 }").Should().Be(OkOf(s_nothing));
    }

    // ===== Json.Decode: keyValuePairs =====

    [Fact]
    public void Decode_keyValuePairs()
    {
        var result = DecodeKeyValuePairsInt("{ \"alice\": 42, \"bob\": 99 }");

        result.Should().Be(
            OkOf(
                ElmList(
                    ElmList(String("alice"), Integer(42)),
                    ElmList(String("bob"), Integer(99)))));
    }

    [Fact]
    public void Decode_keyValuePairs_empty()
    {
        DecodeKeyValuePairsInt("{}").Should().Be(OkOf(ElmList()));
    }

    // ===== Json.Decode: dict =====

    [Fact]
    public void Decode_dict_get()
    {
        var (value, report) =
            CoreLibraryTestHelper.ApplyAndProfileBinary(
                GetTestFunction("decodeDictGetInt"),
                String("alice"),
                String("{ \"alice\": 42, \"bob\": 99 }"),
                s_vm);

        value.Should().Be(JustOf(Integer(42)));

        PerformanceCountersFormatting.FormatCounts(report).Should().Be(
            """
            InstructionCount: 1_517
            InvocationCount: 47
            BuildListCount: 85
            LoopIterationCount: 0
            """);
    }

    [Fact]
    public void Decode_dict_size()
    {
        var (value, report) =
            CoreLibraryTestHelper.ApplyAndProfileUnary(
                GetTestFunction("decodeDictSizeInt"),
                String("{ \"alice\": 42, \"bob\": 99 }"),
                s_vm);

        value.Should().Be(Integer(2));

        PerformanceCountersFormatting.FormatCounts(report).Should().Be(
            """
            InstructionCount: 1_465
            InvocationCount: 48
            BuildListCount: 85
            LoopIterationCount: 0
            """);
    }

    // ===== Json.Decode: oneOrMore =====

    [Fact]
    public void Decode_oneOrMore_success()
    {
        DecodeOneOrMoreListInt("[1,2,3]").Should().Be(OkOf(ElmList(Integer(1), Integer(2), Integer(3))));
    }

    [Fact]
    public void Decode_oneOrMore_fail()
    {
        AssertIsErr(DecodeOneOrMoreListInt("[]"));
    }

    // ===== Json.Decode: string escapes =====

    [Fact]
    public void Decode_string_escape()
    {
        DecodeStringString("\"hello\\nworld\"").Should().Be(OkOf(String("hello\nworld")));
    }

    // ===== Json.Decode: nested array =====

    [Fact]
    public void Decode_nested_array()
    {
        var (value, report) =
            CoreLibraryTestHelper.ApplyAndProfileUnary(
                GetTestFunction("decodeNestedListInt"),
                String("[[1,2],[3,4]]"),
                s_vm);

        value.Should().Be(
            OkOf(
                ElmList(
                    ElmList(Integer(1), Integer(2)),
                    ElmList(Integer(3), Integer(4)))));

        PerformanceCountersFormatting.FormatCounts(report).Should().Be(
            """
            InstructionCount: 1_442
            InvocationCount: 28
            BuildListCount: 66
            LoopIterationCount: 0
            """);
    }

    // ===== Roundtrip: encode then decode via encodeDecodeRoundtrip =====

    [Fact]
    public void Roundtrip_encode_decode_int()
    {
        EncodeDecodeRoundtrip(JsonInt(42)).Should().Be(OkOf(JsonInt(42)));
    }

    [Fact]
    public void Roundtrip_encode_decode_string()
    {
        EncodeDecodeRoundtrip(JsonString("hello")).Should().Be(OkOf(JsonString("hello")));
    }

    [Fact]
    public void Roundtrip_encode_decode_string_empty()
    {
        EncodeDecodeRoundtrip(JsonString("")).Should().Be(OkOf(JsonString("")));
    }

    [Fact]
    public void Roundtrip_encode_decode_object()
    {
        var obj =
            JsonObject(
                JsonEntry("name", JsonString("Tom")),
                JsonEntry("age", JsonInt(42)));

        var (value, report) =
            CoreLibraryTestHelper.ApplyAndProfileUnary(
                GetTestFunction("encodeDecodeRoundtrip"),
                obj,
                s_vm);

        value.Should().Be(OkOf(obj));

        PerformanceCountersFormatting.FormatCounts(report).Should().Be(
            """
            InstructionCount: 12_476
            InvocationCount: 406
            BuildListCount: 681
            LoopIterationCount: 0
            """);
    }

    // ===== Json.Decode: negative int =====

    [Fact]
    public void Decode_negative_int()
    {
        DecodeStringInt("-7").Should().Be(OkOf(Integer(-7)));
    }

    // ===== Json.Decode: whitespace =====

    [Fact]
    public void Decode_with_whitespace()
    {
        DecodeStringInt("  42  ").Should().Be(OkOf(Integer(42)));
    }

    // ===== Json.Decode: map2 sum =====

    [Fact]
    public void Decode_map2_sum()
    {
        DecodeMap2Sum("{ \"x\": 10, \"y\": 20 }").Should().Be(OkOf(Integer(30)));
    }

    // ===== Json.Decode: decodeValue =====

    [Fact]
    public void Decode_value_direct()
    {
        DecodeValueInt(JsonInt(42)).Should().Be(OkOf(Integer(42)));
    }

    [Fact]
    public void Decode_value_direct_string()
    {
        DecodeValueString(JsonString("hello")).Should().Be(OkOf(String("hello")));
    }

    // ===== Json.Decode: large integer =====

    [Fact]
    public void Decode_large_int()
    {
        DecodeStringInt("1000000").Should().Be(OkOf(Integer(1000000)));
    }

    // ===== Json.Decode: nested field =====

    [Fact]
    public void Decode_nested_field()
    {
        DecodeAtInt(
            ElmList(String("outer"), String("inner")),
            "{ \"outer\": { \"inner\": 99 } }")
            .Should().Be(OkOf(Integer(99)));
    }

}
