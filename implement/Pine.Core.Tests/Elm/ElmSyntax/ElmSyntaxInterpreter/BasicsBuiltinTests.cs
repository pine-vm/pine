using AwesomeAssertions;
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
/// Exercises the <c>Basics.compare</c> builtin registered in
/// <c>ElmSyntaxInterpreter.BuildBuiltinFunctionResolvers</c>. The builtin implements Elm's
/// <c>compare</c> directly on the interpreter's value model, so it must produce the same
/// <c>Order</c> result as the recursive Elm implementation in <c>Basics.elm</c> — including
/// for structurally nested values such as lists and tuples.
/// <para>
/// The scenarios are dispatched through the new
/// <see cref="ElmInterpreter.ParseAndInterpretWithCounters(string, ElmInterpreter.Prepared, Action{ApplicationLogEntry}, bool)"/>
/// API, which both enables the default builtins and logs every function application. This
/// lets the tests assert that the number of <c>Basics.compare</c> applications stays constant
/// (a single direct application) regardless of how deeply the compared values are nested —
/// the builtin performs the recursion in .NET rather than re-entering the interpreter.
/// </para>
/// <para>
/// The suite also covers the infix comparison operators (<c>&lt;</c>, <c>&gt;</c>,
/// <c>&lt;=</c>, <c>&gt;=</c>): the interpreter lowers each <c>OperatorApplication</c> to the
/// function named by the matching <c>infix</c> declaration in <c>Basics.elm</c> (e.g. <c>&lt;</c>
/// to <c>lt</c>, <c>&gt;=</c> to <c>ge</c>), which in turn dispatch through the
/// <c>Basics.compare</c> builtin.
/// </para>
/// </summary>
public class BasicsBuiltinTests
{
    private static readonly Lazy<ElmInterpreter.Prepared> s_prepared = new(LoadPrepared);

    private static ElmInterpreter.Prepared LoadPrepared()
    {
        // Basics.elm is the module under test; it provides the user-defined `compare`
        // implementation that the builtin short-circuits. Loading it also lets the
        // builtins-disabled scenarios fall back to the recursive Elm implementation.
        var modules = new[] { LoadKernelModuleSource("Basics.elm") };

        return
            ElmInterpreter.PrepareModules(modules)
            .Extract(err => throw new Exception(err.ToString()));
    }

    private static string LoadKernelModuleSource(string fileName)
    {
        var node =
            BundledFiles.ElmKernelModulesDefault.Value
            .GetNodeAtPath([fileName])
            ?? throw new Exception("Did not find elm-kernel-modules/" + fileName + " in bundled files.");

        if (node is not Files.FileTree.FileNode fileNode)
        {
            throw new Exception(
                "Expected elm-kernel-modules/" + fileName + " to be a file node, but got: " + node.GetType());
        }

        return Encoding.UTF8.GetString(fileNode.Bytes.Span);
    }

    /// <summary>
    /// Evaluates <paramref name="expression"/> with default builtins enabled and returns the
    /// rendered Elm-expression form of the result alongside the number of direct
    /// <c>Basics.compare</c> applications the interpreter dispatched.
    /// </summary>
    private static (string Rendered, int BasicsCompareApplications) EvaluateWithBuiltins(string expression)
    {
        var log = new List<ApplicationLogEntry>();

        var (result, _) =
            ElmInterpreter.ParseAndInterpretWithCounters(
                expression,
                s_prepared.Value,
                log.Add,
                enableDefaultBuiltins: true);

        var value = result.Extract(err => throw new Exception(err.ToString()));

        return
            (ElmInterpreter.RenderAsElmExpression(value).expressionString,
            CountBasicsCompareApplications(log));
    }

    /// <summary>
    /// As <see cref="EvaluateWithBuiltins(string)"/>, but with the default builtins disabled so
    /// the call resolves to the recursive Elm <c>Basics.compare</c> implementation.
    /// </summary>
    private static (string Rendered, int BasicsCompareApplications) EvaluateWithoutBuiltins(string expression)
    {
        var log = new List<ApplicationLogEntry>();

        var (result, _) =
            ElmInterpreter.ParseAndInterpretWithCounters(
                expression,
                s_prepared.Value,
                log.Add,
                enableDefaultBuiltins: false);

        var value = result.Extract(err => throw new Exception(err.ToString()));

        return
            (ElmInterpreter.RenderAsElmExpression(value).expressionString,
            CountBasicsCompareApplications(log));
    }

    private static int CountBasicsCompareApplications(IReadOnlyList<ApplicationLogEntry> log) =>
        log.Count(entry =>
            entry is ApplicationLogEntry.Direct direct &&
            direct.Application.FunctionName.FullName is "Basics.compare");

    // ============================================================
    // Semantics: the builtin matches Elm's compare for primitives.
    // ============================================================

    [Theory]
    [InlineData("compare 1 2", "LT")]
    [InlineData("compare 2 2", "EQ")]
    [InlineData("compare 3 2", "GT")]
    [InlineData("compare -5 4", "LT")]
    [InlineData("compare 'b' 'b'", "EQ")]
    [InlineData("compare \"abc\" \"abd\"", "LT")]
    [InlineData("compare \"abc\" \"abc\"", "EQ")]
    [InlineData("compare \"abd\" \"abc\"", "GT")]
    public void Builtin_compare_matches_Elm_semantics_for_primitives(string expression, string expected)
    {
        var (rendered, applications) = EvaluateWithBuiltins(expression);

        rendered.Should().Be(expected);

        // The builtin resolves the call directly: exactly one Basics.compare application.
        applications.Should().Be(1);
    }

    // ============================================================
    // Semantics: lists and tuples.
    // ============================================================

    [Theory]
    [InlineData("compare [ 1, 2, 3 ] [ 1, 2, 4 ]", "LT")]
    [InlineData("compare [ 1, 2, 3 ] [ 1, 2, 3 ]", "EQ")]
    [InlineData("compare [ 1, 2, 4 ] [ 1, 2, 3 ]", "GT")]
    [InlineData("compare [ 1, 2 ] [ 1, 2, 3 ]", "LT")]
    [InlineData("compare [ 1, 2, 3 ] [ 1, 2 ]", "GT")]
    [InlineData("compare ( 1, 2 ) ( 1, 3 )", "LT")]
    [InlineData("compare ( 1, 2 ) ( 1, 2 )", "EQ")]
    [InlineData("compare ( 2, 1 ) ( 1, 9 )", "GT")]
    [InlineData("compare ( 1, \"a\" ) ( 1, \"b\" )", "LT")]
    public void Builtin_compare_matches_Elm_semantics_for_lists_and_tuples(string expression, string expected)
    {
        var (rendered, applications) = EvaluateWithBuiltins(expression);

        rendered.Should().Be(expected);

        applications.Should().Be(1);
    }

    // ============================================================
    // Infix comparison operators are lowered to their Basics
    // declarations (lt/gt/le/ge), which in turn dispatch through the
    // Basics.compare builtin.
    // ============================================================

    [Theory]
    [InlineData("1 < 2", "True")]
    [InlineData("2 < 2", "False")]
    [InlineData("3 < 2", "False")]
    [InlineData("1 > 2", "False")]
    [InlineData("2 > 2", "False")]
    [InlineData("3 > 2", "True")]
    [InlineData("1 <= 2", "True")]
    [InlineData("3 <= 2", "False")]
    [InlineData("1 >= 2", "False")]
    [InlineData("3 >= 2", "True")]
    [InlineData("\"abc\" < \"abd\"", "True")]
    [InlineData("\"abd\" >= \"abc\"", "True")]
    [InlineData("[ 1, 2 ] < [ 1, 3 ]", "True")]
    [InlineData("( 2, 1 ) >= ( 1, 9 )", "True")]
    public void Infix_comparison_operators_match_Elm_semantics(string expression, string expected)
    {
        var (rendered, _) = EvaluateWithBuiltins(expression);

        rendered.Should().Be(expected);
    }

    // The strict comparison operators (`<` and `>`) always delegate to `compare`,
    // so with the builtin enabled each use dispatches exactly one Basics.compare
    // application — regardless of how deeply nested the compared values are.
    [Theory]
    [InlineData("1 < 2")]
    [InlineData("3 > 2")]
    [InlineData("[ [ 1, 2 ], [ 3, 4 ] ] < [ [ 1, 2 ], [ 3, 5 ] ]")]
    [InlineData("( ( 1, 2 ), 3 ) > ( ( 1, 1 ), 3 )")]
    public void Strict_infix_comparison_operator_dispatches_a_single_Basics_compare_application(string expression)
    {
        var (_, applications) = EvaluateWithBuiltins(expression);

        applications.Should().Be(1);
    }

    [Theory]
    [InlineData("1 < 2")]
    [InlineData("2 < 2")]
    [InlineData("3 > 2")]
    [InlineData("2 <= 2")]
    [InlineData("3 >= 2")]
    [InlineData("\"abc\" < \"abd\"")]
    [InlineData("[ 1, 2 ] < [ 1, 3 ]")]
    [InlineData("( 2, 1 ) >= ( 1, 9 )")]
    public void Infix_comparison_operators_agree_with_recursive_Elm_implementation(string expression)
    {
        var (renderedWithBuiltin, _) = EvaluateWithBuiltins(expression);

        var (renderedWithoutBuiltin, _) = EvaluateWithoutBuiltins(expression);

        renderedWithBuiltin.Should().Be(renderedWithoutBuiltin);
    }

    // ============================================================
    // The builtin matches the recursive Elm implementation exactly.
    // ============================================================

    [Theory]
    [InlineData("compare ( ( 1, 2 ), 3 ) ( ( 1, 9 ), 3 )")]
    [InlineData("compare [ [ 1, 2 ], [ 3, 4 ] ] [ [ 1, 2 ], [ 3, 5 ] ]")]
    [InlineData("compare ( 1, ( 2, 3 ) ) ( 1, ( 2, 3 ) )")]
    public void Builtin_compare_agrees_with_recursive_Elm_implementation(string expression)
    {
        var (renderedWithBuiltin, _) = EvaluateWithBuiltins(expression);

        var (renderedWithoutBuiltin, _) = EvaluateWithoutBuiltins(expression);

        renderedWithBuiltin.Should().Be(renderedWithoutBuiltin);
    }

    // ============================================================
    // Nesting: the builtin keeps the Basics.compare application count
    // constant, while the recursive Elm implementation grows it.
    // ============================================================

    [Fact]
    public void Builtin_compare_application_count_is_constant_across_nesting_levels()
    {
        // Two pairs of nested tuples whose only difference is a distinctive item in the
        // middle of the structure. The deeper pair adds nesting levels around that item.
        const string Shallow =
            "compare ( ( 1, 2 ), 9 ) ( ( 1, 5 ), 9 )";

        const string Deep =
            "compare ( ( ( ( 1, 2 ), 7 ), 8 ), 9 ) ( ( ( ( 1, 5 ), 7 ), 8 ), 9 )";

        var (shallowRendered, shallowApplications) = EvaluateWithBuiltins(Shallow);
        var (deepRendered, deepApplications) = EvaluateWithBuiltins(Deep);

        // Same Order result in both cases (the distinctive middle item makes the left smaller).
        shallowRendered.Should().Be("LT");
        deepRendered.Should().Be("LT");

        // The builtin recurses into the nested structure in .NET, so the interpreter only ever
        // dispatches a single Basics.compare application regardless of nesting depth.
        shallowApplications.Should().Be(1);
        deepApplications.Should().Be(1);
        deepApplications.Should().Be(shallowApplications);
    }

    [Fact]
    public void Recursive_Elm_compare_application_count_grows_with_nesting_levels()
    {
        // The same two pairs as above, but resolved against the recursive Elm implementation.
        // Without the builtin, each additional nesting level adds further Basics.compare calls,
        // demonstrating the runtime work the builtin avoids.
        const string Shallow =
            "compare ( ( 1, 2 ), 9 ) ( ( 1, 5 ), 9 )";

        const string Deep =
            "compare ( ( ( ( 1, 2 ), 7 ), 8 ), 9 ) ( ( ( ( 1, 5 ), 7 ), 8 ), 9 )";

        var (_, shallowApplications) = EvaluateWithoutBuiltins(Shallow);
        var (_, deepApplications) = EvaluateWithoutBuiltins(Deep);

        deepApplications.Should().BeGreaterThan(shallowApplications);
    }
}
