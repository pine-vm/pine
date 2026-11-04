using AwesomeAssertions;
using Pine.Core.Elm;
using Pine.Core.Elm.ElmInElm;
using Pine.Core.Elm.Syntax;
using Pine.Core.Files;
using System;
using System.Linq;
using System.Text;
using Xunit;

using Concrete = Pine.Core.Elm.Syntax.Concrete;

namespace Pine.Core.Tests.Elm.ElmCompilerInDotnet.CoreLibraryModule;

public partial class KernelJsonFunctionTests
{
    private const string JsonApiRegressionDeclarations =
        """


        encodeSet : List Int -> String
        encodeSet items =
            Json.Encode.encode 0 (Json.Encode.set (\n -> Json.Encode.string (String.fromInt n)) (Set.fromList items))

        encodeArray : List Int -> String
        encodeArray items =
            Json.Encode.encode 0 (Json.Encode.array Json.Encode.int (Array.fromList items))

        encodeDict : List ( Int, Int ) -> String
        encodeDict items =
            Json.Encode.encode 0 (Json.Encode.dict String.fromInt Json.Encode.int (Dict.fromList items))

        encodeCollidingDict : List ( Int, Int ) -> String
        encodeCollidingDict items =
            Json.Encode.encode 0 (Json.Encode.dict (\_ -> "same") Json.Encode.int (Dict.fromList items))

        encodeObjectPairs : List ( String, Int ) -> String
        encodeObjectPairs items =
            Json.Encode.encode 0 (Json.Encode.object (List.map (\( key, n ) -> ( key, Json.Encode.int n )) items))

        encodeIndented : Int -> Json.Encode.Value -> String
        encodeIndented =
            Json.Encode.encode

        encodeFloat : Float -> String
        encodeFloat number =
            Json.Encode.encode 0 (Json.Encode.float number)

        encodeSpecialFloats : Int -> List String
        encodeSpecialFloats _ =
            List.map encodeFloat [ 0 / 0, 1 / 0, -1 / 0 ]

        decodeValueSpecialFloats : Int -> List String
        decodeValueSpecialFloats _ =
            List.map
                (\number ->
                    case Json.Decode.decodeValue Json.Decode.float (Json.Encode.float number) of
                        Ok decoded ->
                            if isNaN decoded then "NaN" else String.fromFloat decoded
                        Err error -> Json.Decode.errorToString error
                )
                [ 0 / 0, 1 / 0, -1 / 0 ]

        decodeArrayInt : String -> Result Json.Decode.Error (List Int)
        decodeArrayInt json =
            Result.map Array.toList (Json.Decode.decodeString (Json.Decode.array Json.Decode.int) json)

        decodeDictInt : String -> Result Json.Decode.Error (List ( String, Int ))
        decodeDictInt json =
            Result.map Dict.toList (Json.Decode.decodeString (Json.Decode.dict Json.Decode.int) json)

        decodeFieldMaybeInt : String -> Result Json.Decode.Error (Maybe Int)
        decodeFieldMaybeInt json =
            Json.Decode.decodeString (Json.Decode.field "x" (Json.Decode.maybe Json.Decode.int)) json

        decodeMap4 : String -> Result Json.Decode.Error (List Int)
        decodeMap4 json =
            Json.Decode.decodeString
                (Json.Decode.map4 (\a b c d -> [ a, b, c, d ])
                    (Json.Decode.index 0 Json.Decode.int) (Json.Decode.index 1 Json.Decode.int)
                    (Json.Decode.index 2 Json.Decode.int) (Json.Decode.index 3 Json.Decode.int)) json

        decodeMap5 : String -> Result Json.Decode.Error (List Int)
        decodeMap5 json =
            Json.Decode.decodeString
                (Json.Decode.map5 (\a b c d e -> [ a, b, c, d, e ])
                    (Json.Decode.index 0 Json.Decode.int) (Json.Decode.index 1 Json.Decode.int)
                    (Json.Decode.index 2 Json.Decode.int) (Json.Decode.index 3 Json.Decode.int)
                    (Json.Decode.index 4 Json.Decode.int)) json

        decodeMap6 : String -> Result Json.Decode.Error (List Int)
        decodeMap6 json =
            Json.Decode.decodeString
                (Json.Decode.map6 (\a b c d e f -> [ a, b, c, d, e, f ])
                    (Json.Decode.index 0 Json.Decode.int) (Json.Decode.index 1 Json.Decode.int)
                    (Json.Decode.index 2 Json.Decode.int) (Json.Decode.index 3 Json.Decode.int)
                    (Json.Decode.index 4 Json.Decode.int) (Json.Decode.index 5 Json.Decode.int)) json

        decodeMap7 : String -> Result Json.Decode.Error (List Int)
        decodeMap7 json =
            Json.Decode.decodeString
                (Json.Decode.map7 (\a b c d e f g -> [ a, b, c, d, e, f, g ])
                    (Json.Decode.index 0 Json.Decode.int) (Json.Decode.index 1 Json.Decode.int)
                    (Json.Decode.index 2 Json.Decode.int) (Json.Decode.index 3 Json.Decode.int)
                    (Json.Decode.index 4 Json.Decode.int) (Json.Decode.index 5 Json.Decode.int)
                    (Json.Decode.index 6 Json.Decode.int)) json

        decodeMap8 : String -> Result Json.Decode.Error (List Int)
        decodeMap8 json =
            Json.Decode.decodeString
                (Json.Decode.map8 (\a b c d e f g h -> [ a, b, c, d, e, f, g, h ])
                    (Json.Decode.index 0 Json.Decode.int) (Json.Decode.index 1 Json.Decode.int)
                    (Json.Decode.index 2 Json.Decode.int) (Json.Decode.index 3 Json.Decode.int)
                    (Json.Decode.index 4 Json.Decode.int) (Json.Decode.index 5 Json.Decode.int)
                    (Json.Decode.index 6 Json.Decode.int) (Json.Decode.index 7 Json.Decode.int)) json

        errorString : Json.Decode.Error -> String
        errorString =
            Json.Decode.errorToString

        decodeAndRenderError : String -> String
        decodeAndRenderError json =
            case Json.Decode.decodeString (Json.Decode.at [ "outer", "items" ] (Json.Decode.list Json.Decode.int)) json of
                Ok _ -> "unexpected success"
                Err error -> Json.Decode.errorToString error

        decodeAndEncodeValue : String -> Result Json.Decode.Error String
        decodeAndEncodeValue json =
            Result.map (Json.Encode.encode 0) (Json.Decode.decodeString Json.Decode.value json)

        recursiveDecoder : Json.Decode.Decoder Int
        recursiveDecoder =
            Json.Decode.lazy (\_ -> recursiveDecoder)

        decodeMapShortCircuit : Int -> Result Json.Decode.Error Int
        decodeMapShortCircuit arity =
            let
                decoder =
                    case arity of
                        2 -> Json.Decode.map2 (\a b -> a + b) (Json.Decode.fail "first") recursiveDecoder
                        3 -> Json.Decode.map3 (\a b c -> a + b + c) (Json.Decode.fail "first") recursiveDecoder recursiveDecoder
                        _ -> Json.Decode.map4 (\a b c d -> a + b + c + d) (Json.Decode.fail "first") recursiveDecoder recursiveDecoder recursiveDecoder
            in
            Json.Decode.decodeValue decoder Json.Encode.null
        """;

    private static ElmValue ErrOf(ElmValue error) =>
        ElmValue.TagInstance("Err", [error]);

    private static ElmValue Failure(string message, ElmValue value) =>
        ElmValue.TagInstance("Failure", [String(message), value]);

    private static ElmValue FieldError(string field, ElmValue error) =>
        ElmValue.TagInstance("Field", [String(field), error]);

    private static ElmValue IndexError(int index, ElmValue error) =>
        ElmValue.TagInstance("Index", [Integer(index), error]);

    private static ElmValue OneOfError(params ElmValue[] errors) =>
        ElmValue.TagInstance("OneOf", [ElmList(errors)]);

    [Theory]
    [InlineData("encodeSet", "[\"1\",\"2\",\"3\"]")]
    [InlineData("encodeArray", "[3,1,2,1]")]
    public void Json_API_collection_encoders_apply_mapping_and_order(string function, string expected)
    {
        ApplyUnary(GetTestFunction(function), ElmList(Integer(3), Integer(1), Integer(2), Integer(1)))
            .Should().Be(String(expected));

        ApplyUnary(GetTestFunction(function), ElmList()).Should().Be(String("[]"));
    }

    [Fact]
    public void Json_API_dict_encoder_maps_keys_and_values_and_handles_collisions()
    {
        var entries = ElmList(ElmList(Integer(3), Integer(30)), ElmList(Integer(1), Integer(10)));

        ApplyUnary(GetTestFunction("encodeDict"), entries).Should().Be(String("""{"1":10,"3":30}"""));
        ApplyUnary(GetTestFunction("encodeCollidingDict"), entries).Should().Be(String("""{"same":30}"""));
        ApplyUnary(GetTestFunction("encodeDict"), ElmList()).Should().Be(String("{}"));
    }

    [Fact]
    public void Json_API_object_encoder_overwrites_duplicates_and_orders_array_index_keys()
    {
        var entries =
            ElmList(
                JsonEntry("z", Integer(0)),
                JsonEntry("10", Integer(10)),
                JsonEntry("2", Integer(2)),
                JsonEntry("z", Integer(9)),
                JsonEntry("01", Integer(1)),
                JsonEntry("4294967295", Integer(5)));

        ApplyUnary(GetTestFunction("encodeObjectPairs"), entries)
            .Should().Be(String("""{"2":2,"10":10,"z":9,"01":1,"4294967295":5}"""));
    }

    [Theory]
    [InlineData(-2, "{\"a\":[1,{}]}")]
    [InlineData(0, "{\"a\":[1,{}]}")]
    [InlineData(2, "{\n  \"a\": [\n    1,\n    {}\n  ]\n}")]
    [InlineData(4, "{\n    \"a\": [\n        1,\n        {}\n    ]\n}")]
    [InlineData(12, "{\n          \"a\": [\n                    1,\n                    {}\n          ]\n}")]
    public void Json_API_encode_respects_indentation_and_clamps_to_ten(int indent, string expected)
    {
        var value = JsonObject(JsonEntry("a", JsonArray(JsonInt(1), JsonObject())));
        ApplyBinary(GetTestFunction("encodeIndented"), Integer(indent), value).Should().Be(String(expected));
        ApplyBinary(GetTestFunction("encodeIndented"), Integer(indent), JsonArray()).Should().Be(String("[]"));

        ApplyBinary(GetTestFunction("encodeIndented"), Integer(indent), JsonString("a\tb"))
            .Should().Be(String("\"a\\tb\""));
    }

    [Fact]
    public void Json_API_float_encoder_turns_nonfinite_values_into_null()
    {
        CallThunk("encodeSpecialFloats").Should().Be(ElmList(String("null"), String("null"), String("null")));

        CallThunk("decodeValueSpecialFloats").Should().Be(
            ElmList(String("NaN"), String("Infinity"), String("-Infinity")));

        ApplyUnary(GetTestFunction("encodeFloat"), ElmValue.ElmFloat.Convert(1.5)).Should().Be(String("1.5"));
    }

    [Theory]
    [InlineData("decodeArrayInt")]
    [InlineData("decodeListInt")]
    public void Json_API_array_and_list_preserve_order_and_error_index(string function)
    {
        ApplyUnary(GetTestFunction(function), String("[3,1,2]"))
            .Should().Be(OkOf(ElmList(Integer(3), Integer(1), Integer(2))));

        ApplyUnary(GetTestFunction(function), String("[]")).Should().Be(OkOf(ElmList()));

        ApplyUnary(GetTestFunction(function), String("[1,true]"))
            .Should().Be(ErrOf(IndexError(1, Failure("Expecting an INT", JsonBool(s_true)))));
    }

    [Theory]
    [InlineData("decodeKeyValuePairsInt")]
    [InlineData("decodeDictInt")]
    public void Json_API_object_decoders_keep_field_errors(string function)
    {
        ApplyUnary(GetTestFunction(function), String("""{"x":true}"""))
            .Should().Be(ErrOf(FieldError("x", Failure("Expecting an INT", JsonBool(s_true)))));

        ApplyUnary(GetTestFunction(function), String("[]"))
            .Should().Be(ErrOf(Failure("Expecting an OBJECT", JsonArray())));

        ApplyUnary(GetTestFunction(function), String("{}")).Should().Be(OkOf(ElmList()));
    }

    [Fact]
    public void Json_API_parsed_objects_keep_last_duplicate_values()
    {
        const string json = """{"z":0,"10":10,"2":2,"z":9}""";
        DecodeFieldInt("z", json).Should().Be(OkOf(Integer(9)));

        DecodeKeyValuePairsInt(json)
            .Should().Be(
            OkOf(
                ElmList(
                    JsonEntry("2", Integer(2)),
                    JsonEntry("10", Integer(10)),
                    JsonEntry("z", Integer(9)))));

        ApplyUnary(GetTestFunction("decodeAndEncodeValue"), String(json))
            .Should().Be(OkOf(String("""{"2":2,"10":10,"z":9}""")));
    }

    [Theory]
    [InlineData("decodeStringInt", "Expecting an INT")]
    [InlineData("decodeStringString", "Expecting a STRING")]
    [InlineData("decodeStringFloat", "Expecting a FLOAT")]
    [InlineData("decodeListInt", "Expecting a LIST")]
    [InlineData("decodeArrayInt", "Expecting an ARRAY")]
    [InlineData("decodeNullInt", "Expecting null")]
    public void Json_API_primitive_type_errors_are_not_stubs(string function, string message) =>
        ApplyUnary(GetTestFunction(function), String("true"))
        .Should().Be(ErrOf(Failure(message, JsonBool(s_true))));

    [Fact]
    public void Json_API_bool_type_error_is_not_stub() =>
        DecodeStringBool("42").Should().Be(ErrOf(Failure("Expecting a BOOL", JsonInt(42))));

    [Theory]
    [InlineData("1.0", 1)]
    [InlineData("-0.0", 0)]
    [InlineData("12e2", 1200)]
    [InlineData("12E+02", 1200)]
    [InlineData("120e-1", 12)]
    public void Json_API_int_accepts_integral_json_numbers(string json, int expected) =>
        DecodeStringInt(json).Should().Be(OkOf(Integer(expected)));

    [Theory]
    [InlineData("1.01", 1.01)]
    [InlineData("-0.005", -0.005)]
    [InlineData("12e2", 1200)]
    [InlineData("1.25E+02", 125)]
    [InlineData("12e-2", 0.12)]
    public void Json_API_float_accepts_fractions_and_scientific_notation(string json, double expected) =>
        DecodeStringFloat(json).Should().Be(OkOf(ElmValue.ElmFloat.Convert(expected)));

    [Theory]
    [InlineData("1.01")]
    [InlineData("12e-2")]
    public void Json_API_int_rejects_nonintegral_numbers(string json) =>
        DecodeStringInt(json).Should().Be(ErrOf(Failure("Expecting an INT", JsonFloat(json))));

    [Theory]
    [InlineData("{\"x\":1,}")]
    [InlineData("{\"x\":1, }")]
    [InlineData("[1,]")]
    [InlineData("01")]
    [InlineData("-01")]
    [InlineData("1.")]
    [InlineData("1e")]
    [InlineData("1e+")]
    [InlineData("1e-")]
    [InlineData("\"a\nb\"")]
    [InlineData("\"a\tb\"")]
    [InlineData("\"a\u0001b\"")]
    public void Json_API_decodeString_rejects_invalid_json_and_retains_source(string json)
    {
        var result = ApplyUnary(GetTestFunction("decodeAndEncodeValue"), String(json));
        AssertIsErr(result);
        var error = ((ElmValue.ElmTag)result).Arguments[0].Should().BeOfType<ElmValue.ElmTag>().Subject;
        error.TagName.Should().Be("Failure");

        error.Arguments[0].Should().BeOfType<ElmValue.ElmString>().Subject.Value
            .Should().StartWith("This is not valid JSON! ");

        error.Arguments[1].Should().Be(JsonString(json));
    }

    [Theory]
    [InlineData("\"\\uD800\"", "\"\\uD800\"")]
    [InlineData("\"\\uDC00\"", "\"\\uDC00\"")]
    [InlineData("\"\\uD83C\\uDF32\"", "\"🌲\"")]
    public void Json_API_value_roundtrip_preserves_unicode_and_escapes_lone_surrogates(string json, string expected) =>
        ApplyUnary(GetTestFunction("decodeAndEncodeValue"), String(json))
        .Should().Be(OkOf(String(expected)));

    [Fact]
    public void Json_API_field_index_and_at_preserve_nested_error_paths()
    {
        var primitive = Failure("Expecting an INT", JsonBool(s_true));
        DecodeFieldInt("x", """{"x":true}""").Should().Be(ErrOf(FieldError("x", primitive)));

        DecodeAtInt(ElmList(String("a"), String("b")), """{"a":{"b":true}}""")
            .Should().Be(ErrOf(FieldError("a", FieldError("b", primitive))));

        DecodeIndexString(1, "[0,true]")
            .Should().Be(ErrOf(IndexError(1, Failure("Expecting a STRING", JsonBool(s_true)))));

        AssertIsErr(DecodeIndexString(-1, "[\"first\"]"));
        AssertIsErr(DecodeIndexString(0, "[]"));
        DecodeAtInt(ElmList(), "42").Should().Be(OkOf(Integer(42)));
    }

    [Fact]
    public void Json_API_oneOf_and_nullable_keep_all_errors_in_attempt_order()
    {
        DecodeOneOfEmpty("true").Should().Be(ErrOf(OneOfError()));

        DecodeOneOfIntNull("true")
            .Should().Be(
            ErrOf(
                OneOfError(
                    Failure("Expecting an INT", JsonBool(s_true)),
                    Failure("Expecting null", JsonBool(s_true)))));

        DecodeNullableInt("true")
            .Should().Be(
            ErrOf(
                OneOfError(
                    Failure("Expecting null", JsonBool(s_true)),
                    Failure("Expecting an INT", JsonBool(s_true)))));
    }

    [Fact]
    public void Json_API_maybe_does_not_hide_missing_required_fields()
    {
        ApplyUnary(GetTestFunction("decodeFieldMaybeInt"), String("""{"x":true}""")).Should().Be(OkOf(s_nothing));

        ApplyUnary(GetTestFunction("decodeFieldMaybeInt"), String("{}"))
            .Should().Be(ErrOf(Failure("Expecting an OBJECT with a field named `x`", JsonObject())));
    }

    [Theory]
    [InlineData(4)]
    [InlineData(5)]
    [InlineData(6)]
    [InlineData(7)]
    [InlineData(8)]
    public void Json_API_map4_through_map8_apply_arguments_in_order_and_keep_first_error(int arity)
    {
        var function = GetTestFunction("decodeMap" + arity);
        var values = Enumerable.Range(1, arity).Select(n => Integer(n)).ToArray();

        ApplyUnary(function, String("[" + string.Join(",", Enumerable.Range(1, arity)) + "]"))
            .Should().Be(OkOf(ElmList(values)));

        for (var index = 0; index < arity; ++index)
        {
            var items = Enumerable.Range(0, arity).Select(i => i >= index ? "true" : "1");

            ApplyUnary(function, String("[" + string.Join(",", items) + "]"))
                .Should().Be(ErrOf(IndexError(index, Failure("Expecting an INT", JsonBool(s_true)))));
        }
    }

    [Fact]
    public void Json_API_oneOrMore_preserves_empty_and_element_errors()
    {
        DecodeOneOrMoreListInt("[]").Should().Be(ErrOf(Failure("a ARRAY with at least ONE element", JsonArray())));

        DecodeOneOrMoreListInt("[1,true]")
            .Should().Be(ErrOf(IndexError(1, Failure("Expecting an INT", JsonBool(s_true)))));

        DecodeOneOrMoreListInt("[1]").Should().Be(OkOf(ElmList(Integer(1))));
    }

    [Theory]
    [InlineData(2)]
    [InlineData(3)]
    [InlineData(4)]
    public void Json_API_map_stops_before_evaluating_later_recursive_decoders(int arity) =>
        ApplyUnary(GetTestFunction("decodeMapShortCircuit"), Integer(arity))
        .Should().Be(ErrOf(Failure("first", s_jsonNull)));

    [Theory]
    [InlineData("simple", "json.simple")]
    [InlineData("has space", "json['has space']")]
    [InlineData("", "json['']")]
    [InlineData("1a", "json['1a']")]
    public void Json_API_errorToString_renders_field_paths(string key, string path)
    {
        ApplyUnary(GetTestFunction("errorString"), FieldError(key, Failure("custom", JsonInt(42))))
            .Should().Be(String("Problem with the value at " + path + ":\n\n    42\n\ncustom"));
    }

    [Fact]
    public void Json_API_errorToString_renders_nested_paths_and_oneOf_shapes()
    {
        ApplyUnary(GetTestFunction("decodeAndRenderError"), String("""{"outer":{"items":[1,true]}}"""))
            .Should().Be(String("Problem with the value at json.outer.items[1]:\n\n    true\n\nExpecting an INT"));

        ApplyUnary(GetTestFunction("errorString"), OneOfError())
            .Should().Be(String("Ran into a Json.Decode.oneOf with no possibilities!"));

        ApplyUnary(GetTestFunction("errorString"), FieldError("x", OneOfError()))
            .Should().Be(String("Ran into a Json.Decode.oneOf with no possibilities at json.x"));

        ApplyUnary(GetTestFunction("errorString"), OneOfError(Failure("custom", JsonInt(1))))
            .Should().Be(String("Problem with the given value:\n\n1\n\ncustom"));

        ApplyUnary(
            GetTestFunction("errorString"),
            OneOfError(Failure("first", JsonInt(1)), Failure("second", JsonInt(2))))
            .Should().Be(
            String(
                "Json.Decode.oneOf failed in the following 2 ways:\n\n\n\n(1) Problem with the given value:\n    \n    1\n    \n    first\n\n\n\n(2) Problem with the given value:\n    \n    2\n    \n    second"));
    }

    [Theory]
    [InlineData("Encode", "encode string int float bool null list array set object dict", "Value")]
    [InlineData("Decode",
        "string bool int float nullable list array dict keyValuePairs oneOrMore field at index maybe oneOf decodeString decodeValue errorToString map map2 map3 map4 map5 map6 map7 map8 lazy value null succeed fail andThen",
        "Decoder Value Error")]
    public void Json_API_bundled_and_interactive_modules_cover_elm_json_1_1_4(
        string module, string functions, string types)
    {
        var source = LoadSource(BundledFiles.ElmKernelModulesDefault.Value, ["Json", module + ".elm"]);

        var interactiveSource =
            LoadSource(
                BundledFiles.CompilerSourceContainerFilesDefault.Value,
                ["src", "ElmInteractiveKernelModules.elm"]);

        var interactiveFile =
            ElmSyntaxParser.ParseModuleText(interactiveSource)
            .Extract(error => throw new Exception(error.ToString()));

        var declaration =
            interactiveFile.Declarations.Select(node => node.Value)
            .OfType<Concrete.Declaration.FunctionDeclaration>()
            .Single(function => function.Function.Declaration.Value.Name.Value == "elmKernelModulesTexts");

        var literals =
            declaration.Function.Declaration.Value.Expression.Value
            .Should().BeOfType<Concrete.Expression.ListExpr>().Subject;

        var copy =
            literals.Elements.Select(element => element.Value)
            .OfType<Concrete.Expression.MultilineStringLiteral>()
            .Single(
                literal => literal.Value.TrimStart().StartsWith(
                    "module Json." + module + " exposing",
                    StringComparison.Ordinal)).Value;

        copy.Trim().Should().Be(source.Trim(), "interactive JSON copies must not drift");

        var parsed =
            ElmSyntaxParser.ParseModuleText(source)
            .Extract(error => throw new Exception(error.ToString()));

        parsed.IncompleteDeclarations.Should().BeEmpty();

        var names =
            parsed.Declarations.Select(node => node.Value)
            .OfType<Concrete.Declaration.FunctionDeclaration>()
            .Select(function => function.Function.Declaration.Value.Name.Value).ToArray();

        names.Should().Contain(functions.Split(' '), "every elm/json 1.1.4 function must have an implementation");

        var exposing =
            parsed.ModuleDefinition.Value.Should().BeOfType<Concrete.Module.NormalModule>()
            .Subject.ModuleData.ExposingList.Value;

        if (exposing is Concrete.Exposing.Explicit explicitExposing)
        {
            explicitExposing.Nodes.Select(node => node.Value).OfType<Concrete.TopLevelExpose.FunctionExpose>()
                .Select(expose => expose.Name).Should().Contain(functions.Split(' '));

            explicitExposing.Nodes.Select(node => node.Value).OfType<Concrete.TopLevelExpose.TypeOrAliasExpose>()
                .Select(expose => expose.Name).Should().Contain("Decoder", "Value");

            explicitExposing.Nodes.Select(node => node.Value).OfType<Concrete.TopLevelExpose.TypeExpose>()
                .Select(expose => expose.ExposedType.Name).Should().Contain("Error");
        }

        foreach (var type in types.Split(' '))
        {
            source.Should().Contain(
                type == "Value" && module == "Decode"
                ?
                "type alias Value"
                :
                "type " +
                (type == "Decoder" ? "alias " : "") + type);
        }
    }

    private static string LoadSource(FileTree tree, string[] path) =>
        Encoding.UTF8.GetString(
            (tree.GetNodeAtPath(path) as FileTree.FileNode
            ?? throw new Exception("Missing source: " + string.Join("/", path))).Bytes.Span);
}
