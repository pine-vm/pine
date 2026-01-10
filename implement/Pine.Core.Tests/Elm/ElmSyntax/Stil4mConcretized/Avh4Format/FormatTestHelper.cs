using AwesomeAssertions;
using Pine.Core.Elm.ElmSyntax;
using Pine.Core.Elm.ElmSyntax.Stil4mConcretized;
using System;

namespace Pine.Core.Tests.Elm.ElmSyntax.Stil4mConcretized.Avh4Format;

using Avh4Format = Core.Elm.ElmSyntax.Stil4mConcretized.Avh4Format;

public class FormatTestHelper
{
    public static File ParseFile(string input)
    {
        return ElmSyntaxParser.ParseModuleText(input)
            .Extract(err => throw new Exception($"Parsing failed: {err}"));
    }

    public static string FormatString(
        string input)
    {
        var parsed =
            ElmSyntaxParser.ParseModuleText(input)
            .Extract(err => throw new Exception($"Parsing failed: {err}"));

        // Detect linebreak style from input and use it for output
        var linebreakStyle = Rendering.DetectLinebreakStyle(input);

        return Avh4Format.FormatToString(parsed, linebreakStyle);
    }

    public static void AssertModuleTextFormatsToItself(
        string elmModuleText)
    {
        /*
         * Verify formatting is stable with various linebreak styles:
         * One variant is using LF linebreaks only,
         * the other variant is using CRLF linebreaks only.
         * */

        var elmModuleText_LF =
            elmModuleText.Replace("\r\n", "\n");

        var elmModuleText_CRLF =
            elmModuleText.Replace("\n", "\r\n");

        try
        {
            AssertSyntaxNodesValueEqualityForModuleText(elmModuleText);
        }
        catch (Exception ex)
        {
            throw new Exception(
                "Value equality assertion failed for original module text.",
                ex);
        }

        try
        {
            AssertModuleTextFormatsToItselfOnlyWithGivenLinebreaks(
                elmModuleText_LF);
        }
        catch (Exception ex)
        {
            throw new Exception(
                "Formatting stability assertion failed for LF-only linebreaks.",
                ex);
        }

        try
        {
            AssertModuleTextFormatsToItselfOnlyWithGivenLinebreaks(
                elmModuleText_CRLF);
        }
        catch (Exception ex)
        {
            throw new Exception(
                "Formatting stability assertion failed for CRLF-only linebreaks.",
                ex);
        }
    }

    private static void AssertModuleTextFormatsToItselfOnlyWithGivenLinebreaks(
        string elmModuleText)
    {
        AssertSyntaxNodesValueEqualityForModuleText(elmModuleText);

        var formatted = FormatString(elmModuleText);

        formatted.Trim().Should().Be(elmModuleText.Trim());
    }

    public static void AssertModuleTextFormatsToExpected(
        string elmModuleText,
        string expectedFormattedElmModuleText)
    {
        var formatted = FormatString(elmModuleText);

        formatted.Trim().Should().Be(expectedFormattedElmModuleText.Trim());

        // A single call of the API should always yield a stable result
        AssertModuleTextFormatsToItself(formatted);
    }

    public static void AssertSyntaxNodesValueEqualityForModuleText(
        string elmModuleText)
    {
        /*
         * Verify the C# type declarations implement value-based equality and
         * hash code generation correctly, by parsing the same module text twice
         * and comparing the resulting syntax nodes.
         * */

        var parsed_0 =
            ElmSyntaxParser.ParseModuleText(elmModuleText)
            .Extract(err => throw new Exception("Parsing failed: " + err.ToString()));

        var parsed_1 =
            ElmSyntaxParser.ParseModuleText(elmModuleText)
            .Extract(err => throw new Exception("Reparsing failed: " + err.ToString()));

        parsed_0.Should().Be(parsed_1);

        parsed_0.GetHashCode().Should().Be(parsed_1.GetHashCode());
    }
}
