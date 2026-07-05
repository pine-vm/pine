using AwesomeAssertions;
using System;
using System.Collections.Generic;
using System.Linq;
using Xunit;

using ElmInterpreter = Pine.Core.Elm.ElmSyntax.ElmSyntaxInterpreter;
using ApplicationLogEntry = Pine.Core.Elm.ElmSyntax.ApplicationLogEntry;

namespace Pine.Core.Tests.Elm.ElmSyntax.ElmSyntaxInterpreter;

/// <summary>
/// Exercises the <c>Char</c> classification builtins (<c>isDigit</c>, <c>isOctDigit</c>,
/// <c>isHexDigit</c>, <c>isUpper</c>, <c>isLower</c>, <c>isAlpha</c>, <c>isAlphaNum</c>)
/// registered in <c>ElmSyntaxInterpreter.BuildBuiltinFunctionResolvers</c>. Each builtin
/// computes the character range check directly on the interpreter's value model, so it must
/// produce the same <c>Bool</c> result as the recursive Elm implementation in
/// <c>elm-kernel-modules/Char.elm</c> — which it does by mirroring the same code-point ranges.
/// <para>
/// These functions are among the hottest in the Elm-syntax parser, so serving them directly
/// removes the nested kernel applications (<c>take</c>, <c>concat</c>,
/// <c>int_is_sorted_asc</c>) the interpreter would otherwise dispatch per character.
/// </para>
/// </summary>
public class CharBuiltinTests
{
    private static readonly Lazy<ElmInterpreter.Prepared> s_prepared =
        new(() => InterpreterTestHelper.PrepareKernelModules(
            "Char.elm",
            "Basics.elm"));

    private static PineValue EvaluateWithBuiltins(string expression) =>
        InterpreterTestHelper.EvaluateInModulesToPineValue(expression, s_prepared.Value);

    private static PineValue EvaluateWithoutBuiltins(string expression) =>
        InterpreterTestHelper.EvaluateInModulesWithoutBuiltinsToPineValue(expression, s_prepared.Value);

    private static readonly IReadOnlyList<string> s_classificationFunctions =
        [
            "Char.isDigit",
            "Char.isOctDigit",
            "Char.isHexDigit",
            "Char.isUpper",
            "Char.isLower",
            "Char.isAlpha",
            "Char.isAlphaNum",
        ];

    // Representative code points including every boundary of the ASCII ranges the
    // classification functions test, plus a few non-ASCII characters.
    private static readonly IReadOnlyList<char> s_sampleChars =
        [
            ' ', '!', '/', '0', '5', '7', '8', '9', ':', '@',
            'A', 'F', 'G', 'Z', '[', '_', '`', 'a', 'f', 'g',
            'z', '{', '~', 'É', 'é', '€', '中',
        ];

    private static string CharLiteral(char c) =>
        "'" + (c is '\'' or '\\' ? "\\" + c : c.ToString()) + "'";

    [Fact]
    public void Builtins_match_Elm_semantics_across_sample_characters()
    {
        foreach (var functionName in s_classificationFunctions)
        {
            foreach (var c in s_sampleChars)
            {
                var expression = functionName + " " + CharLiteral(c);

                EvaluateWithBuiltins(expression)
                    .Should().Be(
                        EvaluateWithoutBuiltins(expression),
                        because: "builtin " + expression + " must match the Elm implementation");
            }
        }
    }

    [Theory]
    [InlineData("Char.isDigit '0'", "True")]
    [InlineData("Char.isDigit '9'", "True")]
    [InlineData("Char.isDigit 'a'", "False")]
    [InlineData("Char.isDigit 'A'", "False")]
    [InlineData("Char.isOctDigit '7'", "True")]
    [InlineData("Char.isOctDigit '8'", "False")]
    [InlineData("Char.isHexDigit 'f'", "True")]
    [InlineData("Char.isHexDigit 'F'", "True")]
    [InlineData("Char.isHexDigit 'g'", "False")]
    [InlineData("Char.isUpper 'A'", "True")]
    [InlineData("Char.isUpper 'Z'", "True")]
    [InlineData("Char.isUpper 'a'", "False")]
    [InlineData("Char.isLower 'a'", "True")]
    [InlineData("Char.isLower 'z'", "True")]
    [InlineData("Char.isLower 'A'", "False")]
    [InlineData("Char.isAlpha 'a'", "True")]
    [InlineData("Char.isAlpha 'Z'", "True")]
    [InlineData("Char.isAlpha '0'", "False")]
    [InlineData("Char.isAlphaNum 'a'", "True")]
    [InlineData("Char.isAlphaNum 'Z'", "True")]
    [InlineData("Char.isAlphaNum '5'", "True")]
    [InlineData("Char.isAlphaNum '_'", "False")]
    [InlineData("Char.isAlphaNum ' '", "False")]
    public void Builtins_produce_expected_bool(string expression, string expectedExpression) =>
        EvaluateWithBuiltins(expression).Should().Be(EvaluateWithBuiltins(expectedExpression));

    [Fact]
    public void Builtin_short_circuits_nested_kernel_applications()
    {
        var withBuiltin = LogDirectApplications("Char.isAlphaNum 'a'", enableDefaultBuiltins: true);

        // The builtin resolves the call directly: exactly one Char.isAlphaNum application and
        // no nested Pine_kernel.int_is_sorted_asc range checks.
        withBuiltin.Count(name => name is "Char.isAlphaNum").Should().Be(1);
        withBuiltin.Should().NotContain("Pine_kernel.int_is_sorted_asc");

        var withoutBuiltin = LogDirectApplications("Char.isAlphaNum 'a'", enableDefaultBuiltins: false);

        // The Elm implementation re-enters the interpreter for its range checks.
        withoutBuiltin.Should().Contain("Pine_kernel.int_is_sorted_asc");
    }

    private static IReadOnlyList<string> LogDirectApplications(string expression, bool enableDefaultBuiltins)
    {
        var log = new List<ApplicationLogEntry>();

        var (result, _) =
            ElmInterpreter.ParseAndInterpretWithCounters(
                expression,
                s_prepared.Value,
                log.Add,
                enableDefaultBuiltins: enableDefaultBuiltins);

        result.Extract(err => throw new Exception(err.ToString()));

        return
            [.. log
            .OfType<ApplicationLogEntry.Direct>()
            .Select(direct => direct.Application.FunctionName.FullName)];
    }
}
