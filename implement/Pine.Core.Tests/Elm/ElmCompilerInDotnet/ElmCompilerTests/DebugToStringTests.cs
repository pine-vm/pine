using AwesomeAssertions;
using Pine.Core.CommonEncodings;
using Pine.Core.Elm;
using Pine.Core.Elm.ElmCompilerInDotnet.CoreLibraryModule;
using Pine.Core.Tests.Elm.ElmCompilerInDotnet.CoreLibraryModule;
using System.Collections.Generic;
using System.Linq;
using Xunit;

namespace Pine.Core.Tests.Elm.ElmCompilerInDotnet.ElmCompilerTests;

public class DebugToStringTests
{
    public static IEnumerable<object[]> GenericValues()
    {
        yield return [IntegerEncoding.EncodeSignedInteger(11), "11"];
        yield return [ElmValueEncoding.StringAsPineValue("hello"), "\"hello\""];
        yield return [PineValue.EmptyList, "[]"];
        yield return
            [
            PineValue.List(
                [
                IntegerEncoding.EncodeSignedInteger(11),
                IntegerEncoding.EncodeSignedInteger(13),
                IntegerEncoding.EncodeSignedInteger(17),
                ]),
            "[11,13,17]"
            ];
        yield return
            [
            PineValue.List(
                [
                ElmValueEncoding.StringAsPineValue("one"),
                ElmValueEncoding.StringAsPineValue("two"),
                ]),
            "[\"one\",\"two\"]"
            ];
    }

    [Theory]
    [MemberData(nameof(GenericValues))]
    public void Generic_function_renders_values_without_type_information(
        PineValue value,
        string expected)
    {
        var rendered =
            CoreLibraryTestHelper.ApplyGenericPine(
                CoreDebug.ToString_FunctionValue(),
                [value]);

        DecodeElmValue(rendered).Should().Be(ElmValue.StringInstance(expected));
    }

    [Theory]
    [InlineData("11", "11")]
    [InlineData("\"hello\"", "\"hello\"")]
    [InlineData("[]", "[]")]
    [InlineData("[ 11, 13, 17, 19 ]", "[11,13,17,19]")]
    [InlineData("[ \"one\", \"two\" ]", "[\"one\",\"two\"]")]
    [InlineData("[ [ 1, 2 ], [ 3 ] ]", "[[1,2],[3]]")]
    public void Compiles_and_evaluates_supported_values(
        string elmExpression,
        string expected)
    {
        var elmModuleText =
            $$"""
            module Test exposing (..)

            rendered =
                Debug.toString {{elmExpression}}
            """;

        var parsedEnv =
            ElmCompilerTestHelper.CompileElmModules(
                [elmModuleText],
                disableInlining: false).parsedEnv;

        var value =
            parsedEnv.Modules
            .Single(module => module.moduleName is "Test")
            .moduleContent.FunctionDeclarations["rendered"];

        DecodeElmValue(value).Should().Be(ElmValue.StringInstance(expected));
    }

    [Fact]
    public void Compiles_toString_escaping_as_a_record_field()
    {
        var elmModuleText =
            """
            module Test exposing (..)

            renderer : { fieldName : a -> String }
            renderer =
                { fieldName = Debug.toString }

            rendered =
                renderer.fieldName [ 11, 13, 17 ]
            """;

        var parsedEnv =
            ElmCompilerTestHelper.CompileElmModules(
                [elmModuleText],
                disableInlining: false).parsedEnv;

        var value =
            parsedEnv.Modules
            .Single(module => module.moduleName is "Test")
            .moduleContent.FunctionDeclarations["rendered"];

        DecodeElmValue(value).Should().Be(ElmValue.StringInstance("[11,13,17]"));
    }

    private static ElmValue DecodeElmValue(PineValue value) =>
        ElmValueEncoding.PineValueAsElmValue(value, null, null)
        .Extract(err => throw new System.Exception("Failed decoding Elm value: " + err));
}
