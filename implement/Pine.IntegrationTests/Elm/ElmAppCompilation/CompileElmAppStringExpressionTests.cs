using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.Elm;
using Pine.Core.Elm.ElmSyntax;
using Pine.Core.Internal;
using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Text;
using Xunit;

namespace Pine.IntegrationTests.Elm.ElmAppCompilation;

/// <summary>
/// Regression tests for <c>CompileElmApp.stringExpressionFromString</c>, which renders arbitrary
/// text (for example the complete JavaScript output of <c>elm make</c> or UTF-8 source files) as
/// an Elm string literal expression in generated modules. The function is evaluated with
/// <see cref="ElmSyntaxInterpreter"/> on the same prepared compiler modules that
/// <see cref="ElmTime.ElmAppCompilation"/> uses.
/// </summary>
public class CompileElmAppStringExpressionTests
{
    private static readonly DeclQualifiedName s_functionName =
        DeclQualifiedName.Create(["CompileElmApp"], "stringExpressionFromString");

    private static readonly IReadOnlyDictionary<DeclQualifiedName, Core.Elm.ElmSyntax.SyntaxModel.Declaration> s_noDeclarations =
        new Dictionary<DeclQualifiedName, Core.Elm.ElmSyntax.SyntaxModel.Declaration>();

    private static string StringExpressionFromString(string input)
    {
        var result =
            ElmSyntaxInterpreter.Interpret(
                s_functionName,
                [PineValueInProcess.Create(ElmValueEncoding.StringAsPineValue(input))],
                ElmTime.ElmAppCompilation.CompilerModulesPreparedForInterpreter.Value);

        var resultValue =
            result
            .Extract(err => throw new Exception("Failed interpreting: " + err.Message))
            .Evaluate();

        var asElmValue =
            ElmValueEncoding.PineValueAsElmValue(resultValue, null, null)
            .Extract(err => throw new Exception("Failed decoding result: " + err));

        return
            asElmValue is ElmValue.ElmString elmString
            ?
            elmString.Value
            :
            throw new Exception("Unexpected result type: " + asElmValue);
    }

    [Theory]
    [InlineData("", "\"\"")]
    [InlineData("abc", "\"abc\"")]
    [InlineData("say \"hi\"", "\"say \\\"hi\\\"\"")]
    [InlineData("back\\slash", "\"back\\\\slash\"")]
    [InlineData("line1\nline2", "\"line1\\nline2\"")]
    [InlineData("crlf\r\n", "\"crlf\\r\\n\"")]
    [InlineData("\\n is not a newline", "\"\\\\n is not a newline\"")]
    [InlineData("\\\"", "\"\\\\\\\"\"")]
    [InlineData("\"\"\"", "\"\\\"\\\"\\\"\"")]
    [InlineData("tab\tstays", "\"tab\tstays\"")]
    [InlineData("{ x = 'a' }", "\"{ x = 'a' }\"")]
    [InlineData("äöü € 😀", "\"äöü € 😀\"")]
    public void Renders_expected_string_literal(string input, string expectedExpression)
    {
        StringExpressionFromString(input).Should().Be(expectedExpression);
    }

    [Theory]
    [InlineData("")]
    [InlineData("plain text")]
    [InlineData("quotes \" and backslashes \\ and \\\" combined")]
    [InlineData("multi\nline\r\ntext\n\n")]
    [InlineData("trailing backslash \\")]
    [InlineData("unicode: äöü € 😀 𝄞")]
    [InlineData("var x = \"a\\\\b\";\nconsole.log('\\n');\r\n")]
    public void Rendered_expression_evaluates_back_to_input(string input)
    {
        var expression = StringExpressionFromString(input);

        var evaluated =
            ElmSyntaxInterpreter.ParseAndInterpret(expression, s_noDeclarations)
            .Extract(err => throw new Exception("Failed evaluating rendered expression: " + err.Message))
            .Evaluate();

        evaluated.Should().Be(ElmValueEncoding.StringAsPineValue(input));
    }

    [Fact]
    public void Renders_all_ASCII_characters()
    {
        var input =
            new string([.. Enumerable.Range(0, 128).Select(i => (char)i)]);

        var expected = new StringBuilder("\"");

        foreach (var c in input)
        {
            expected.Append(
                c switch
                {
                    '\\' => "\\\\",
                    '\n' => "\\n",
                    '\r' => "\\r",
                    '"' => "\\\"",
                    _ => c.ToString(),
                });
        }

        expected.Append('"');

        StringExpressionFromString(input).Should().Be(expected.ToString());
    }

    /// <summary>
    /// Emitting <c>elm make</c> output means rendering strings with hundreds of thousands of
    /// characters (678,229 bytes for the Sanderling alternate-ui app). Asserts the rendering stays
    /// fast for such inputs.
    /// </summary>
    [Fact]
    public void Renders_large_input_within_time_limit()
    {
        var line = "var _v0 = \"x\\\\y\"; // äö\r\n";

        var input =
            string.Concat(Enumerable.Repeat(line, 700_000 / line.Length));

        // Warm up preparation of the compiler modules so it does not count towards the time limit.
        StringExpressionFromString("warm up");

        var stopwatch = Stopwatch.StartNew();

        var expression = StringExpressionFromString(input);

        stopwatch.Stop();

        stopwatch.Elapsed.Should().BeLessThan(TimeSpan.FromSeconds(30));

        var expectedLine = "var _v0 = \\\"x\\\\\\\\y\\\"; // äö\\r\\n";

        expression.Should().Be(
            "\"" + string.Concat(Enumerable.Repeat(expectedLine, 700_000 / line.Length)) + "\"");
    }
}
