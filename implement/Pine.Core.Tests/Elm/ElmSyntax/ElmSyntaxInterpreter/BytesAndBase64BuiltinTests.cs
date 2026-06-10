using AwesomeAssertions;
using Pine.Core.Elm.ElmInElm;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Xunit;

using ElmInterpreter = Pine.Core.Elm.ElmSyntax.ElmSyntaxInterpreter;

namespace Pine.Core.Tests.Elm.ElmSyntax.ElmSyntaxInterpreter;

/// <summary>
/// Exercises the builtin short-circuits for the <c>Bytes</c> / base64 kernel and core functions
/// registered in <c>ElmSyntaxInterpreter.BuildBuiltinFunctionResolvers</c>:
/// <list type="bullet">
///   <item><c>Bytes.Encode.encodeCharsAsBlob</c> (UTF-32 code points → UTF-8 blob)</item>
///   <item><c>Bytes.Encode.encodeBlob</c> (<c>Encoder</c> → raw blob)</item>
///   <item><c>Bytes.Decode.decodeBlobAsCharsRec</c> (UTF-8 blob → <c>String</c>)</item>
///   <item><c>Base64.Encode.toBytes</c> (base64 <c>String</c> → <c>Maybe Bytes</c>)</item>
///   <item><c>Base64.Decode.fromBytes</c> (<c>Bytes</c> → <c>Maybe String</c>)</item>
/// </list>
/// The kernel modules (plus the <c>src/Base64</c> source-container modules) are canonicalized into a
/// single <see cref="ElmInterpreter.Prepared"/> program. Each scenario is asserted in two ways:
/// <c>AssertBuiltinMatchesElm</c> verifies the builtin produces exactly the same
/// <see cref="PineValue"/> as the user-defined Elm-source implementation (builtins disabled), and a
/// handful of explicit value assertions pin down the observable behaviour against known vectors.
/// </summary>
public class BytesAndBase64BuiltinTests
{
    private static readonly Lazy<ElmInterpreter.Prepared> s_prepared =
        new(BuildPrepared);

    private static ElmInterpreter.Prepared BuildPrepared()
    {
        // Kernel modules transitively referenced by the Bytes/Base64 functions under test,
        // addressed by their tree paths (the Bytes modules live in a sub-directory).
        string[][] kernelModulePaths =
        [
            ["Basics.elm"],
            ["Char.elm"],
            ["String.elm"],
            ["List.elm"],
            ["Maybe.elm"],
            ["Result.elm"],
            ["Tuple.elm"],
            ["Bitwise.elm"],
            ["Bytes.elm"],
            ["Bytes", "Encode.elm"],
            ["Bytes", "Decode.elm"],
        ];

        var moduleTexts = new List<string>();

        foreach (var path in kernelModulePaths)
        {
            moduleTexts.Add(LoadKernelModuleSourceAtPath(path));
        }

        // The Base64 modules are not kernel modules; they live in the compiler source container.
        moduleTexts.Add(InterpreterTestHelper.LoadCompilerSourceModule("src", "Base64", "Encode.elm"));
        moduleTexts.Add(InterpreterTestHelper.LoadCompilerSourceModule("src", "Base64", "Decode.elm"));

        return InterpreterTestHelper.PrepareModulesFromSources(moduleTexts);
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

    private static PineValue Evaluate(string expression) =>
        InterpreterTestHelper.EvaluateInModulesToPineValue(expression, s_prepared.Value);

    private static void AssertEvaluatesEqual(string expression, string expectedExpression) =>
        Evaluate(expression).Should().Be(Evaluate(expectedExpression));

    /// <summary>
    /// Asserts that the builtin short-circuit produces exactly the same <see cref="PineValue"/> as
    /// the user-defined Elm-source implementation (builtins disabled), proving the builtin is a
    /// faithful replacement.
    /// </summary>
    private static void AssertBuiltinMatchesElm(string expression) =>
        InterpreterTestHelper.EvaluateInModulesToPineValue(expression, s_prepared.Value)
            .Should()
            .Be(InterpreterTestHelper.EvaluateInModulesWithoutBuiltinsToPineValue(expression, s_prepared.Value));

    /// <summary>
    /// Builds an Elm expression evaluating to a <c>Bytes</c> value made of the given raw bytes,
    /// using a sequence of <c>unsignedInt8</c> encoders.
    /// </summary>
    private static string BytesOf(params int[] bytes)
    {
        if (bytes.Length is 0)
        {
            return "Bytes.Encode.encode (Bytes.Encode.sequence [])";
        }

        var encoders =
            string.Join(", ", bytes.Select(b => "Bytes.Encode.unsignedInt8 " + b));

        return "Bytes.Encode.encode (Bytes.Encode.sequence [ " + encoders + " ])";
    }

    // ============================================================
    // Bytes.Encode.encodeBlob (via Bytes.Encode.encode)
    // ============================================================

    [Theory]
    [InlineData("Bytes.Encode.signedInt8 0")]
    [InlineData("Bytes.Encode.signedInt8 5")]
    [InlineData("Bytes.Encode.signedInt8 127")]
    [InlineData("Bytes.Encode.signedInt8 -1")]
    [InlineData("Bytes.Encode.signedInt8 -5")]
    [InlineData("Bytes.Encode.signedInt8 -128")]
    [InlineData("Bytes.Encode.unsignedInt8 0")]
    [InlineData("Bytes.Encode.unsignedInt8 200")]
    [InlineData("Bytes.Encode.unsignedInt8 255")]
    public void EncodeBlob_int8_matches_Elm(string encoder) =>
        AssertBuiltinMatchesElm("Bytes.Encode.encode (" + encoder + ")");

    [Theory]
    [InlineData("Bytes.Encode.signedInt16 Bytes.LE 0")]
    [InlineData("Bytes.Encode.signedInt16 Bytes.BE 0")]
    [InlineData("Bytes.Encode.signedInt16 Bytes.LE 258")]
    [InlineData("Bytes.Encode.signedInt16 Bytes.BE 258")]
    [InlineData("Bytes.Encode.signedInt16 Bytes.LE 32767")]
    [InlineData("Bytes.Encode.signedInt16 Bytes.BE -1")]
    [InlineData("Bytes.Encode.signedInt16 Bytes.LE -1")]
    [InlineData("Bytes.Encode.signedInt16 Bytes.BE -258")]
    [InlineData("Bytes.Encode.signedInt16 Bytes.LE -32768")]
    [InlineData("Bytes.Encode.unsignedInt16 Bytes.LE 0")]
    [InlineData("Bytes.Encode.unsignedInt16 Bytes.BE 258")]
    [InlineData("Bytes.Encode.unsignedInt16 Bytes.LE 65535")]
    [InlineData("Bytes.Encode.unsignedInt16 Bytes.BE 65535")]
    public void EncodeBlob_int16_matches_Elm(string encoder) =>
        AssertBuiltinMatchesElm("Bytes.Encode.encode (" + encoder + ")");

    [Theory]
    [InlineData("Bytes.Encode.signedInt32 Bytes.LE 0")]
    [InlineData("Bytes.Encode.signedInt32 Bytes.BE 16909060")]
    [InlineData("Bytes.Encode.signedInt32 Bytes.LE 16909060")]
    [InlineData("Bytes.Encode.signedInt32 Bytes.BE 2147483647")]
    [InlineData("Bytes.Encode.signedInt32 Bytes.BE -1")]
    [InlineData("Bytes.Encode.signedInt32 Bytes.LE -1")]
    [InlineData("Bytes.Encode.signedInt32 Bytes.BE -16909060")]
    [InlineData("Bytes.Encode.signedInt32 Bytes.LE -2147483648")]
    [InlineData("Bytes.Encode.unsignedInt32 Bytes.LE 0")]
    [InlineData("Bytes.Encode.unsignedInt32 Bytes.BE 16909060")]
    [InlineData("Bytes.Encode.unsignedInt32 Bytes.LE 4294967295")]
    [InlineData("Bytes.Encode.unsignedInt32 Bytes.BE 4294967295")]
    public void EncodeBlob_int32_matches_Elm(string encoder) =>
        AssertBuiltinMatchesElm("Bytes.Encode.encode (" + encoder + ")");

    [Fact]
    public void EncodeBlob_empty_sequence_matches_Elm() =>
        AssertBuiltinMatchesElm("Bytes.Encode.encode (Bytes.Encode.sequence [])");

    [Fact]
    public void EncodeBlob_nested_sequence_matches_Elm() =>
        AssertBuiltinMatchesElm(
            "Bytes.Encode.encode (Bytes.Encode.sequence " +
            "[ Bytes.Encode.unsignedInt8 1" +
            ", Bytes.Encode.signedInt16 Bytes.BE -300" +
            ", Bytes.Encode.sequence [ Bytes.Encode.unsignedInt32 Bytes.LE 70000, Bytes.Encode.signedInt8 -2 ]" +
            ", Bytes.Encode.string \"ok\"" +
            " ])");

    [Fact]
    public void EncodeBlob_bytes_encoder_round_trips_blob() =>
        AssertBuiltinMatchesElm(
            "Bytes.Encode.encode (Bytes.Encode.bytes (" + BytesOf(1, 2, 3, 255) + "))");

    [Theory]
    [InlineData("Bytes.Encode.signedInt8 -5", new[] { 0xFB })]
    [InlineData("Bytes.Encode.unsignedInt8 255", new[] { 0xFF })]
    [InlineData("Bytes.Encode.unsignedInt16 Bytes.BE 258", new[] { 0x01, 0x02 })]
    [InlineData("Bytes.Encode.unsignedInt16 Bytes.LE 258", new[] { 0x02, 0x01 })]
    [InlineData("Bytes.Encode.unsignedInt32 Bytes.BE 16909060", new[] { 0x01, 0x02, 0x03, 0x04 })]
    [InlineData("Bytes.Encode.unsignedInt32 Bytes.LE 16909060", new[] { 0x04, 0x03, 0x02, 0x01 })]
    [InlineData("Bytes.Encode.signedInt16 Bytes.BE -1", new[] { 0xFF, 0xFF })]
    public void EncodeBlob_produces_expected_bytes(string encoder, int[] expectedBytes) =>
        AssertEvaluatesEqual("Bytes.Encode.encode (" + encoder + ")", BytesOf(expectedBytes));

    // ============================================================
    // Bytes.Encode.encodeCharsAsBlob (via Bytes.Encode.string and directly)
    // ============================================================

    [Theory]
    [InlineData("")]
    [InlineData("a")]
    [InlineData("hello")]
    [InlineData("Hello, World!")]
    [InlineData("h\u00e9llo")]
    [InlineData("\u00e0\u00e8\u00ec\u00f2\u00f9")]
    [InlineData("\u65e5\u672c\u8a9e")]
    [InlineData("\ud83d\ude00\ud83c\udf89")]
    [InlineData("mixed \u00e9 \u65e5 \ud83d\ude00 end")]
    public void EncodeCharsAsBlob_via_string_matches_Elm(string text) =>
        AssertBuiltinMatchesElm("Bytes.Encode.encode (Bytes.Encode.string \"" + EscapeForElmStringLiteral(text) + "\")");

    [Theory]
    // 4-byte big-endian code points fed directly as the chars blob.
    [InlineData(0x41)]
    [InlineData(0x7F)]
    [InlineData(0x80)]
    [InlineData(0x07FF)]
    [InlineData(0x0800)]
    [InlineData(0xFFFF)]
    [InlineData(0x10000)]
    [InlineData(0x1F600)]
    public void EncodeCharsAsBlob_direct_single_code_point_matches_Elm(int codePoint) =>
        AssertBuiltinMatchesElm(
            "(case " + BytesOf(
                (codePoint >> 24) & 0xFF,
                (codePoint >> 16) & 0xFF,
                (codePoint >> 8) & 0xFF,
                codePoint & 0xFF) +
            " of Bytes.Elm_Bytes blob -> Bytes.Encode.encodeCharsAsBlob blob)");

    [Fact]
    public void EncodeCharsAsBlob_direct_multiple_code_points_matches_Elm() =>
        AssertBuiltinMatchesElm(
            "(case " + BytesOf(
                0, 0, 0, 0x41,
                0, 0, 0, 0xE9,
                0, 0, 0x67, 0x2C,
                0, 0x01, 0xF6, 0x00) +
            " of Bytes.Elm_Bytes blob -> Bytes.Encode.encodeCharsAsBlob blob)");

    // ============================================================
    // Bytes.Decode.decodeBlobAsCharsRec (via Bytes.Decode.decode and directly)
    // ============================================================

    [Theory]
    [InlineData("")]
    [InlineData("a")]
    [InlineData("hello")]
    [InlineData("Hello, World!")]
    [InlineData("h\u00e9llo")]
    [InlineData("\u65e5\u672c\u8a9e")]
    [InlineData("\ud83d\ude00\ud83c\udf89")]
    [InlineData("mixed \u00e9 \u65e5 \ud83d\ude00 end")]
    public void DecodeBlobAsCharsRec_round_trips_string(string text)
    {
        var literal = "\"" + EscapeForElmStringLiteral(text) + "\"";

        // Encode the string to Bytes, then decode the whole blob back; should reproduce the string.
        AssertEvaluatesEqual(
            "(case Bytes.Encode.encode (Bytes.Encode.string " + literal + ") of " +
            "Bytes.Elm_Bytes blob -> Bytes.Decode.decodeBlobAsCharsRec 0 blob [])",
            literal);
    }

    [Theory]
    [InlineData("hello")]
    [InlineData("h\u00e9llo")]
    [InlineData("\u65e5\u672c\u8a9e")]
    public void DecodeBlobAsCharsRec_matches_Elm(string text) =>
        AssertBuiltinMatchesElm(
            "(case Bytes.Encode.encode (Bytes.Encode.string \"" + EscapeForElmStringLiteral(text) + "\") of " +
            "Bytes.Elm_Bytes blob -> Bytes.Decode.decodeBlobAsCharsRec 0 blob [])");

    [Fact]
    public void DecodeBlobAsCharsRec_with_initial_accumulator_matches_Elm() =>
        // A non-empty initial accumulator is reversed and prepended, so the result begins with "BA".
        AssertBuiltinMatchesElm(
            "(case Bytes.Encode.encode (Bytes.Encode.string \"hello\") of " +
            "Bytes.Elm_Bytes blob -> Bytes.Decode.decodeBlobAsCharsRec 0 blob [ 'A', 'B' ])");

    [Fact]
    public void DecodeBlobAsCharsRec_with_initial_accumulator_produces_expected_string() =>
        AssertEvaluatesEqual(
            "(case Bytes.Encode.encode (Bytes.Encode.string \"hello\") of " +
            "Bytes.Elm_Bytes blob -> Bytes.Decode.decodeBlobAsCharsRec 0 blob [ 'A', 'B' ])",
            "\"BAhello\"");

    [Fact]
    public void DecodeBlobAsCharsRec_with_nonzero_offset_matches_Elm() =>
        AssertBuiltinMatchesElm(
            "(case Bytes.Encode.encode (Bytes.Encode.string \"hello\") of " +
            "Bytes.Elm_Bytes blob -> Bytes.Decode.decodeBlobAsCharsRec 2 blob [])");

    [Fact]
    public void Decode_string_decoder_round_trips() =>
        AssertEvaluatesEqual(
            "Bytes.Decode.decode (Bytes.Decode.string 5) (Bytes.Encode.encode (Bytes.Encode.string \"hello\"))",
            "Just \"hello\"");

    // ============================================================
    // Base64.Encode.toBytes (base64 String -> Maybe Bytes)
    // ============================================================

    [Theory]
    [InlineData("")]
    [InlineData("TWFu")]
    [InlineData("TWE=")]
    [InlineData("TQ==")]
    [InlineData("aGVsbG8=")]
    [InlineData("aGVsbG8gd29ybGQ=")]
    [InlineData("Zm9vYmFy")]
    [InlineData("Zg==")]
    [InlineData("Zm8=")]
    [InlineData("+/+/")]
    public void Base64EncodeToBytes_valid_matches_Elm(string base64) =>
        AssertBuiltinMatchesElm("Base64.Encode.toBytes \"" + base64 + "\"");

    [Theory]
    // Invalid characters and lengths that the Elm function rejects as Nothing.
    [InlineData("!!!!")]
    [InlineData("A")]
    [InlineData("ABCDE")]
    [InlineData("====")]
    [InlineData("a b ")]
    public void Base64EncodeToBytes_invalid_matches_Elm(string base64) =>
        AssertBuiltinMatchesElm("Base64.Encode.toBytes \"" + base64 + "\"");

    [Theory]
    [InlineData("!!!!")]
    [InlineData("A")]
    [InlineData("ABCDE")]
    public void Base64EncodeToBytes_invalid_is_Nothing(string base64) =>
        AssertEvaluatesEqual("Base64.Encode.toBytes \"" + base64 + "\"", "Nothing");

    [Theory]
    [InlineData("TWFu", new[] { 0x4D, 0x61, 0x6E })]
    [InlineData("TWE=", new[] { 0x4D, 0x61 })]
    [InlineData("TQ==", new[] { 0x4D })]
    public void Base64EncodeToBytes_produces_expected_bytes(string base64, int[] expectedBytes) =>
        AssertEvaluatesEqual(
            "Base64.Encode.toBytes \"" + base64 + "\"",
            "Just (" + BytesOf(expectedBytes) + ")");

    // ============================================================
    // Base64.Decode.fromBytes (Bytes -> Maybe String)
    // ============================================================

    [Theory]
    [InlineData(new int[] { })]
    [InlineData(new[] { 0x4D })]
    [InlineData(new[] { 0x4D, 0x61 })]
    [InlineData(new[] { 0x4D, 0x61, 0x6E })]
    [InlineData(new[] { 0x00, 0x10, 0x83, 0xFF })]
    [InlineData(new[] { 0xDE, 0xAD, 0xBE, 0xEF, 0x00, 0x11, 0x22 })]
    public void Base64DecodeFromBytes_matches_Elm(int[] bytes) =>
        AssertBuiltinMatchesElm("Base64.Decode.fromBytes (" + BytesOf(bytes) + ")");

    [Theory]
    [InlineData("Just \"TWFu\"", new[] { 0x4D, 0x61, 0x6E })]
    [InlineData("Just \"TWE=\"", new[] { 0x4D, 0x61 })]
    [InlineData("Just \"TQ==\"", new[] { 0x4D })]
    [InlineData("Just \"\"", new int[] { })]
    public void Base64DecodeFromBytes_produces_expected_string(string expected, int[] bytes) =>
        AssertEvaluatesEqual("Base64.Decode.fromBytes (" + BytesOf(bytes) + ")", expected);

    // ============================================================
    // Round trips between the base64 encode/decode builtins
    // ============================================================

    [Theory]
    [InlineData(new int[] { })]
    [InlineData(new[] { 0x4D })]
    [InlineData(new[] { 0x4D, 0x61 })]
    [InlineData(new[] { 0x4D, 0x61, 0x6E })]
    [InlineData(new[] { 0x00, 0x01, 0x02, 0x03, 0x04, 0x05 })]
    [InlineData(new[] { 0xFF, 0xFE, 0xFD, 0xFC, 0xFB })]
    public void Base64_round_trip_bytes_to_string_to_bytes(int[] bytes) =>
        // fromBytes always yields Just, and toBytes of that base64 string recovers the bytes.
        AssertEvaluatesEqual(
            "(case Base64.Decode.fromBytes (" + BytesOf(bytes) + ") of " +
            "Just s -> Base64.Encode.toBytes s" +
            "\n Nothing -> Nothing)",
            "Just (" + BytesOf(bytes) + ")");

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
}
