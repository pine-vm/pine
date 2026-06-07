using AwesomeAssertions;
using Pine.Core.Elm.ElmSyntax;
using System;
using Xunit;

namespace Pine.Core.Tests.Elm.ElmSyntax;

/// <summary>
/// Tests covering the requirements documented in <c>explore/2026-06-07-elm-format.md</c>:
/// the <c>elm-format</c> subcommand preserves the varied representations of characters in
/// string literals and the varied representations of float literals, formatting them as-is.
/// </summary>
public class ElmFormatPreservesLiteralsTests
{
    private static string Format(string moduleText) =>
        ElmFormat.FormatModuleText(moduleText)
        .Extract(err => throw new Exception("Formatting failed: " + err));

    [Fact]
    public void Preserves_varied_representations_of_characters_in_triple_quoted_string_literal()
    {
        // Example taken from explore/2026-06-07-elm-format.md. The triple-quoted string mixes
        // literal newlines, escape sequences ("\n"), unicode escapes ("\u{000A}") and a lone
        // surrogate escape ("\uDF32") next to an actual code point ("🌲"). All of these
        // representations must survive a formatting round-trip unchanged.
        var moduleText =
            "module Test exposing (..)\n" +
            "\n" +
            "\n" +
            "test =\n" +
            "    \"\"\"\n" +
            "line A\n" +
            "\n" +
            "line B\\n\\nline C\n" +
            "\\u{000A}\n" +
            "\\uDF32 \U0001F332\n" +
            "\"\"\"\n";

        var formatted = Format(moduleText);

        formatted.Should().Be(moduleText);
    }

    [Theory]
    [InlineData("\"line1\\nline2\"")]
    [InlineData("\"tab\\tend\"")]
    [InlineData("\"unicode \\u{0041}\\u{1F332}\"")]
    [InlineData("\"backslash \\\\ done\"")]
    [InlineData("\"quote \\\" done\"")]
    public void Preserves_varied_representations_of_characters_in_single_line_string_literal(string literal)
    {
        var moduleText =
            "module Test exposing (..)\n\n\ntest =\n    " + literal + "\n";

        var formatted = Format(moduleText);

        formatted.Should().Be(moduleText);
    }

    [Theory]
    [InlineData("1.00e3")]
    [InlineData("1000.0")]
    [InlineData("3.14")]
    [InlineData("0.5")]
    [InlineData("1.0e-3")]
    [InlineData("2.5E10")]
    public void Preserves_varied_representations_of_float_literals(string literal)
    {
        // Example from explore/2026-06-07-elm-format.md: both "1.00e3" and "1000.0" are preserved.
        var moduleText =
            "module Test exposing (..)\n\n\nvalue =\n    " + literal + "\n";

        var formatted = Format(moduleText);

        formatted.Should().Be(moduleText);
    }

    [Fact]
    public void Distinct_float_representations_of_same_value_are_each_preserved()
    {
        var moduleText =
            "module Test exposing (..)\n" +
            "\n" +
            "\n" +
            "a =\n" +
            "    1.00e3\n" +
            "\n" +
            "\n" +
            "b =\n" +
            "    1000.0\n";

        var formatted = Format(moduleText);

        formatted.Should().Be(moduleText);
    }
}
