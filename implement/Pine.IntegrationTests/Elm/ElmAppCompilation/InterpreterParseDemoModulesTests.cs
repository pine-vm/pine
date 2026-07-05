using AwesomeAssertions;
using Pine.Core;
using Pine.Core.Elm;
using Pine.Core.Elm.ElmSyntax;
using Pine.Core.CodeAnalysis;
using Pine.Core.Internal;
using System.IO;
using System.Linq;
using System.Text;
using Xunit;

namespace Pine.IntegrationTests.Elm.ElmAppCompilation;

/// <summary>
/// Diagnostic tests introduced while switching <see cref="ElmTime.ElmAppCompilation"/> to run the
/// Elm app compiler through <see cref="ElmSyntaxInterpreter"/> instead of the compiled compiler
/// environment. They exercise the compiled elm-syntax parser (Elm.Parser.parseToFile) through the
/// interpreter directly, which is what the compiler does internally for every module it processes.
/// </summary>
public class InterpreterParseDemoModulesTests
{
    private static string DemoDir(string leaf) =>
        Path.Combine(
            TestResultSummary.FindTestDataDirectory("ElmAppCompilationSnapshot"),
            "demo-backend-state", leaf, "src");

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

        return result.IsErrOrNull() is { } err ? err.Message : null;
    }

    /// <summary>
    /// Regression: every hand-written source module of the demo-backend-state snapshot must parse
    /// successfully when the compiled elm-syntax parser is driven through the interpreter.
    /// </summary>
    [Fact]
    public void Demo_source_modules_parse_through_interpreter()
    {
        var failures = new StringBuilder();

        foreach (var file in
            Directory.EnumerateFiles(DemoDir("source"), "*.elm", SearchOption.AllDirectories)
            .OrderBy(p => p))
        {
            if (InterpretParseError(File.ReadAllText(file)) is { } err)
                failures.AppendLine(Path.GetFileName(file) + ": " + err.Split('\n')[0]);
        }

        failures.ToString().Should().Be("");
    }

    /// <summary>
    /// Regression: the large compiler-generated JSON-converters module must parse successfully
    /// through the interpreter. It previously failed with a "Case expression did not match any
    /// arm" error caused by the infinite-recursion detector false-positiving on deeply nested
    /// <c>ParserFast</c> combinator closures (distinct closures sharing one lambda AST node),
    /// whose error was then silently swallowed by the local-binding call fast path. See
    /// <see cref="ElmSyntaxInterpreter"/> (<c>CheckForInfiniteRecursion</c> now compares the
    /// captured environment; the fast path now propagates the call error).
    /// </summary>
    [Fact]
    public void Demo_generated_json_converters_module_parses_through_interpreter()
    {
        var file =
            Path.Combine(
                DemoDir("expected"),
                "Backend", "InterfaceToHost_Root", "Generated_JsonConverters.elm");

        var err = InterpretParseError(File.ReadAllText(file));

        err.Should().BeNull();
    }
}
