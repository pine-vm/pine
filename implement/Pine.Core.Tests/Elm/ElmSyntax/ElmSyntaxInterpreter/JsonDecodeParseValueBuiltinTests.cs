using AwesomeAssertions;
using Pine.Core.CommonEncodings;
using Pine.Core.Elm.ElmInElm;
using Pine.Core.Elm.ElmSyntax;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Xunit;

using ElmInterpreter = Pine.Core.Elm.ElmSyntax.ElmSyntaxInterpreter;

namespace Pine.Core.Tests.Elm.ElmSyntax.ElmSyntaxInterpreter;

/// <summary>
/// Exercises the <c>Json.Decode.parseValue</c> builtin (registered in
/// <see cref="ElmInterpreter.BuildBuiltinFunctionResolvers"/>) by parsing JSON through the
/// exposed <c>Json.Decode.parseJsonStringToValue</c> wrapper.
/// <para>
/// Each scenario is checked two ways: the parsed value with the builtin enabled is compared
/// against the value produced by the user-defined (Elm-source) parser with the builtin disabled
/// (<see cref="InterpreterTestHelper.EvaluateInModulesWithoutBuiltinsToPineValue"/>). Because the
/// builtin is meant to be a byte-for-byte short-circuit of the Elm implementation, the two paths
/// must agree for every input — including deeply nested structures and the various string escape
/// encodings.
/// </para>
/// <para>
/// A final group of tests asserts that the interpreter's performance counters do not grow with
/// the length of string values, the number of list items, or the magnitude of integers when the
/// builtin is enabled: the whole recursive descent happens inside the C# builtin, so the
/// interpreter only ever dispatches the single top-level application.
/// </para>
/// </summary>
public class JsonDecodeParseValueBuiltinTests
{
    private static readonly Lazy<ElmInterpreter.Prepared> s_prepared =
        new(() => PrepareJsonDecodeModules());

    private static ElmInterpreter.Prepared PrepareJsonDecodeModules()
    {
        // The kernel module sources transitively referenced by Json.Decode.parseValue and the
        // parseJsonStringToValue wrapper, addressed by their tree paths (Json modules live in a
        // sub-directory).
        string[][] modulePaths =
        [
            ["Basics.elm"],
            ["Char.elm"],
            ["String.elm"],
            ["List.elm"],
            ["Maybe.elm"],
            ["Result.elm"],
            ["Tuple.elm"],
            ["Bitwise.elm"],
            ["Array.elm"],
            ["Set.elm"],
            ["Dict.elm"],
            ["Json", "Encode.elm"],
            ["Json", "Decode.elm"],
        ];

        var moduleTexts =
            modulePaths.Select(LoadKernelModuleSourceAtPath).ToList();

        return
            ElmInterpreter.PrepareModules(moduleTexts)
            .Extract(err => throw new Exception(err.ToString()));
    }

    private static string LoadKernelModuleSourceAtPath(IReadOnlyList<string> path)
    {
        var node =
            BundledFiles.ElmKernelModulesDefault.Value.GetNodeAtPath(path)
            ?? throw new Exception(
                "Did not find elm-kernel-modules/" + string.Join("/", path) + " in bundled files.");

        if (node is not Files.FileTree.FileNode fileNode)
        {
            throw new Exception(
                "Expected elm-kernel-modules/" + string.Join("/", path) + " to be a file node, but got: " + node.GetType());
        }

        return Encoding.UTF8.GetString(fileNode.Bytes.Span);
    }

    /// <summary>
    /// Wraps the JSON source text into the <c>Json.Decode.parseJsonStringToValue "..."</c>
    /// expression, escaping it as an Elm string literal.
    /// </summary>
    private static string ParseExpression(string jsonText) =>
        "Json.Decode.parseJsonStringToValue \"" + EscapeForElmStringLiteral(jsonText) + "\"";

    private static string EscapeForElmStringLiteral(string text)
    {
        var builder = new StringBuilder(text.Length + 8);

        foreach (var c in text)
        {
            switch (c)
            {
                case '\\':
                    builder.Append("\\\\");
                    break;
                case '"':
                    builder.Append("\\\"");
                    break;
                case '\n':
                    builder.Append("\\n");
                    break;
                case '\r':
                    builder.Append("\\r");
                    break;
                case '\t':
                    builder.Append("\\t");
                    break;
                default:
                    builder.Append(c);
                    break;
            }
        }

        return builder.ToString();
    }

    private static PineValue EvaluateWithBuiltin(string jsonText) =>
        InterpreterTestHelper.EvaluateInModulesToPineValue(ParseExpression(jsonText), s_prepared.Value);

    private static PineValue EvaluateWithoutBuiltin(string jsonText) =>
        InterpreterTestHelper.EvaluateInModulesWithoutBuiltinsToPineValue(ParseExpression(jsonText), s_prepared.Value);

    /// <summary>
    /// Asserts that the builtin and the user-defined Elm parser agree on the result of parsing
    /// <paramref name="jsonText"/>.
    /// </summary>
    private static PineValue AssertBuiltinMatchesElm(string jsonText)
    {
        var withBuiltin = EvaluateWithBuiltin(jsonText);
        var withoutBuiltin = EvaluateWithoutBuiltin(jsonText);

        withBuiltin.Should().Be(
            withoutBuiltin,
            "the parseValue builtin must produce the same value as the user-defined Elm implementation for input: " + jsonText);

        return withBuiltin;
    }

    // ============================================================
    // Primitive and simple composite values.
    // ============================================================

    [Theory]
    [InlineData("null")]
    [InlineData("true")]
    [InlineData("false")]
    [InlineData("0")]
    [InlineData("42")]
    [InlineData("-7")]
    [InlineData("123456789012345678901234567890")]
    [InlineData("3.14")]
    [InlineData("-0.5")]
    [InlineData("\"\"")]
    [InlineData("\"hello\"")]
    [InlineData("[]")]
    [InlineData("[1, 2, 3]")]
    [InlineData("[true, false, null]")]
    [InlineData("{}")]
    [InlineData("{\"a\": 1}")]
    [InlineData("{\"a\": 1, \"b\": [2, 3], \"c\": {\"d\": null}}")]
    [InlineData("  \t\n  42  ")]
    [InlineData("[ 1 , 2 , 3 ]")]
    public void Builtin_matches_Elm_for_well_formed_json(string jsonText) =>
        AssertBuiltinMatchesElm(jsonText);

    // ============================================================
    // Error cases: the builtin must reproduce the Elm error messages and offsets.
    // ============================================================

    [Theory]
    [InlineData("")]
    [InlineData("nul")]
    [InlineData("tru")]
    [InlineData("fals")]
    [InlineData("[1, 2")]
    [InlineData("[1 2]")]
    [InlineData("{\"a\" 1}")]
    [InlineData("{\"a\": }")]
    [InlineData("{\"a\": 1 \"b\": 2}")]
    [InlineData("xyz")]
    [InlineData("[1,]")]
    [InlineData("\"unterminated")]
    public void Builtin_matches_Elm_for_malformed_json(string jsonText) =>
        AssertBuiltinMatchesElm(jsonText);

    // ============================================================
    // Deeply nested structures.
    // ============================================================

    [Fact]
    public void Builtin_matches_Elm_for_deeply_nested_arrays()
    {
        var depth = 40;

        var json = new string('[', depth) + "0" + new string(']', depth);

        AssertBuiltinMatchesElm(json);
    }

    [Fact]
    public void Builtin_matches_Elm_beyond_previous_native_nesting_limit()
    {
        var depth = 129;
        var json = new string('[', depth) + "0" + new string(']', depth);

        ElmInterpreter.JsonDecodeParseValue(
            StringEncoding.ValueFromString(json),
            IntegerEncoding.EncodeSignedInteger(0))
            .Should().NotBeNull();

        AssertBuiltinMatchesElm(json);
    }

    [Fact]
    public void Builtin_matches_Elm_for_deeply_nested_objects()
    {
        var depth = 30;

        var builder = new StringBuilder();

        for (var i = 0; i < depth; ++i)
        {
            builder.Append("{\"k\": ");
        }

        builder.Append("123");

        for (var i = 0; i < depth; ++i)
        {
            builder.Append('}');
        }

        AssertBuiltinMatchesElm(builder.ToString());
    }

    [Fact]
    public void Builtin_matches_Elm_for_mixed_deeply_nested_structure()
    {
        const string json =
            """
            {
              "name": "root",
              "children": [
                { "id": 1, "tags": ["a", "b"], "meta": { "active": true, "score": -3.5 } },
                { "id": 2, "tags": [], "meta": { "active": false, "nested": [ [ [ 1, 2 ], [ 3 ] ], [] ] } }
              ],
              "count": 2,
              "empty": null
            }
            """;

        AssertBuiltinMatchesElm(json);
    }

    // ============================================================
    // String escape encodings.
    // ============================================================

    [Theory]
    // Standard two-character escapes.
    [InlineData("\"a\\nb\"")]
    [InlineData("\"a\\rb\"")]
    [InlineData("\"a\\tb\"")]
    [InlineData("\"a\\\"b\"")]
    [InlineData("\"a\\\\b\"")]
    [InlineData("\"a\\/b\"")]
    [InlineData("\"\\b\\f\"")]
    // Unicode BMP escapes.
    [InlineData("\"\\u0041\\u0042\\u0043\"")]
    [InlineData("\"snowman \\u2603\"")]
    [InlineData("\"null char \\u0000 here\"")]
    [InlineData("\"control \\u001f end\"")]
    // Surrogate-pair escapes combining into an astral code point (e.g. U+1F332 🌲).
    [InlineData("\"tree \\uD83C\\uDF32\"")]
    [InlineData("\"face \\uD83D\\uDE00 done\"")]
    // Mixed escapes and simple characters.
    [InlineData("\"line1\\nline2\\ttabbed \\u0021\"")]
    public void Builtin_matches_Elm_for_string_escape_encodings(string jsonText) =>
        AssertBuiltinMatchesElm(jsonText);

    [Theory]
    // Escapes used inside object keys as well as values.
    [InlineData("{\"key\\n1\": \"val\\u0041\"}")]
    [InlineData("{\"\\uD83C\\uDF32\": [\"\\t\", \"\\\"\"]}")]
    public void Builtin_matches_Elm_for_escapes_in_object_keys_and_values(string jsonText) =>
        AssertBuiltinMatchesElm(jsonText);

    // ============================================================
    // Non-ASCII source characters (multi-byte UTF-32 code points carried verbatim).
    // ============================================================

    [Theory]
    [InlineData("\"héllo wörld\"")]
    [InlineData("\"日本語\"")]
    [InlineData("\"emoji 🌲 inside\"")]
    [InlineData("[\"αβγ\", \"δεζ\"]")]
    public void Builtin_matches_Elm_for_non_ascii_source_characters(string jsonText) =>
        AssertBuiltinMatchesElm(jsonText);

    // ============================================================
    // Performance counters do not grow with input size.
    // ============================================================

    private static string CountersSnapshotWithBuiltin(string jsonText)
    {
        var (result, counters) =
            ElmInterpreter.ParseAndInterpretWithCounters(
                ParseExpression(jsonText),
                s_prepared.Value,
                onApplication: null,
                enableDefaultBuiltins: true);

        // Make sure the scenario actually parsed successfully before comparing counters.
        result.Extract(err => throw new Exception(err.ToString()));

        return ElmSyntaxInterpreterPerformanceCountersFormatting.FormatCounts(counters);
    }

    [Fact]
    public void Performance_counters_do_not_increase_with_string_length()
    {
        var shortString = "\"" + new string('a', 5) + "\"";
        var longString = "\"" + new string('a', 5000) + "\"";

        // Sanity check: both inputs parse to the expected (different) values.
        AssertBuiltinMatchesElm(shortString);
        AssertBuiltinMatchesElm(longString);

        CountersSnapshotWithBuiltin(longString)
            .Should().Be(CountersSnapshotWithBuiltin(shortString));
    }

    [Fact]
    public void Performance_counters_do_not_increase_with_number_of_list_items()
    {
        var shortList = "[" + string.Join(",", Enumerable.Repeat("1", 5)) + "]";
        var longList = "[" + string.Join(",", Enumerable.Repeat("1", 1000)) + "]";

        AssertBuiltinMatchesElm(shortList);
        AssertBuiltinMatchesElm(longList);

        CountersSnapshotWithBuiltin(longList)
            .Should().Be(CountersSnapshotWithBuiltin(shortList));
    }

    [Fact]
    public void Performance_counters_do_not_increase_with_integer_length()
    {
        var shortInt = "5";
        var longInt = new string('9', 1000);

        AssertBuiltinMatchesElm(shortInt);
        AssertBuiltinMatchesElm(longInt);

        CountersSnapshotWithBuiltin(longInt)
            .Should().Be(CountersSnapshotWithBuiltin(shortInt));
    }
}
