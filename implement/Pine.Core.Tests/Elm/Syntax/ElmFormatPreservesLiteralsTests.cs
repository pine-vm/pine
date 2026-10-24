using AwesomeAssertions;
using Pine.Core.Elm.Syntax;
using System;
using Xunit;

namespace Pine.Core.Tests.Elm.Syntax;

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
    [InlineData("\"\\r\"")]
    [InlineData("\"\\r\\n\"")]
    [InlineData("\"before\\rbetween\\r\\nafter\"")]
    [InlineData("\"\\u{0000D}\"")]
    [InlineData("\"\\u{000D}\"")]
    [InlineData("\"\\u{000d}\"")]
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
    [InlineData("\"\\r\"")]
    [InlineData("\"\\r\\n\"")]
    [InlineData("\"before\\rbetween\\r\\nafter\"")]
    [InlineData("\"\\\\r\"")]
    [InlineData("\"\\u{0000D}\"")]
    [InlineData("\"\\u{000D}\"")]
    [InlineData("\"\\u{000d}\"")]
    [InlineData("\"\\r\\u{000D}\\n\"")]
    [InlineData("\"\\\\u{00D}\"")]
    [InlineData("\"\\u{000A}\\u{0009}\\u{0041}\"")]
    [InlineData("\"\"\"\\r\\u{000D}\"\"\"")]
    [InlineData("\"\"\"first\\r\nsecond\\u{000D}\"\"\"")]
    public void Preserves_original_representation_of_string_patterns(string literal)
    {
        foreach (var newline in new[] { "\n", "\r\n" })
        {
            var moduleText =
                ("module Test exposing (..)\n\n\n" +
                "test text =\n" +
                "    case text of\n" +
                "        " + literal + " ->\n" +
                "            1\n\n" +
                "        _ ->\n" +
                "            0\n").Replace("\n", newline);

            var result =
                ElmFormat.FormatModuleTextReportingSyntaxErrors(moduleText)
                .Extract(err => throw new Exception("Formatting failed: " + err));

            result.SyntaxErrors.Should().BeEmpty();
            var formatted = result.FormattedText;

            formatted.Should().Be(moduleText);
            Format(formatted).Should().Be(formatted);
        }
    }

    [Fact]
    public void Preserves_carriage_return_escapes_in_triple_quoted_string_literal()
    {
        var moduleText =
            "module Test exposing (..)\n\n\ntest =\n" +
            "    \"\"\"before\\r\nbetween\\r\\nafter\\u{000D}\"\"\"\n";

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
