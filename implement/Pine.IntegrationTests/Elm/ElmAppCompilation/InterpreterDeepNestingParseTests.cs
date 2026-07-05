using AwesomeAssertions;
using Pine.Core;
using Pine.Core.Elm;
using Pine.Core.Elm.ElmSyntax;
using Pine.Core.CodeAnalysis;
using Pine.Core.Internal;
using System.Text;
using Xunit;

namespace Pine.IntegrationTests.Elm.ElmAppCompilation;

/// <summary>
/// Minimal reproductions distilled from the compiler-generated JSON-converters module that used to
/// fail parsing through <see cref="ElmSyntaxInterpreter"/> with "Case expression did not match any
/// arm". The root cause was the interpreter's infinite-recursion detector false-positiving on the
/// many <c>ParserFast</c> combinator closures that share a single lambda AST node while capturing
/// different free variables; the resulting error was then silently swallowed by the local-binding
/// call fast path, surfacing later as the confusing case-mismatch.
/// <para>
/// These deterministic generators (deeply nested applications and parentheses) reproduced the bug
/// at modest nesting depths and are retained as regression coverage: every one must now parse
/// without error.
/// </para>
/// </summary>
public class InterpreterDeepNestingParseTests
{
    private static ElmSyntaxInterpreter.Prepared Prepared =>
        ElmTime.ElmAppCompilation.CompilerModulesPreparedForInterpreter.Value;

    private static string? InterpretParseError(string moduleText)
    {
        var arg = PineValueInProcess.Create(ElmValueEncoding.StringAsPineValue(moduleText));

        var result =
            ElmSyntaxInterpreter.Interpret(
                DeclQualifiedName.Create(["Elm", "Parser"], "parseToFile"),
                [arg],
                Prepared);

        return result.IsErrOrNull() is { } err ? err.ToString() : null;
    }

    /// <summary>
    /// Builds a module whose single declaration is a deeply nested function application
    /// <c>f (g (g … x))</c>. Nesting to depth 27 was the minimal case that reproduced the bug.
    /// </summary>
    private static string NestedApplication(int depth)
    {
        var sb = new StringBuilder();
        sb.AppendLine("module Test exposing (..)");
        sb.AppendLine();
        sb.AppendLine("decoder =");
        sb.Append("    f");
        for (var i = 0; i < depth; ++i)
            sb.Append(" (g");
        sb.Append(" x");
        for (var i = 0; i < depth; ++i)
            sb.Append(')');
        sb.AppendLine();
        return sb.ToString();
    }

    /// <summary>
    /// Builds a module whose single declaration is a deeply parenthesized value
    /// <c>(((… x …)))</c>. Nesting to depth 26 was the minimal parentheses-only reproduction.
    /// </summary>
    private static string NestedParentheses(int depth)
    {
        var sb = new StringBuilder();
        sb.AppendLine("module Test exposing (..)");
        sb.AppendLine();
        sb.Append("decoder =\n    ");
        for (var i = 0; i < depth; ++i)
            sb.Append('(');
        sb.Append('x');
        for (var i = 0; i < depth; ++i)
            sb.Append(')');
        sb.AppendLine();
        return sb.ToString();
    }

    [Theory]
    [InlineData(1)]
    [InlineData(26)]
    [InlineData(27)]
    [InlineData(40)]
    [InlineData(80)]
    public void Nested_application_parses_through_interpreter(int depth)
    {
        InterpretParseError(NestedApplication(depth)).Should().BeNull();
    }

    [Theory]
    [InlineData(1)]
    [InlineData(25)]
    [InlineData(26)]
    [InlineData(40)]
    [InlineData(80)]
    public void Nested_parentheses_parses_through_interpreter(int depth)
    {
        InterpretParseError(NestedParentheses(depth)).Should().BeNull();
    }
}
