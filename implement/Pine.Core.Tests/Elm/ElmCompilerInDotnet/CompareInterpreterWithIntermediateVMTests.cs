using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.Elm;
using Pine.Core.Elm.ElmSyntax;
using System;
using System.Collections.Generic;
using System.Linq;
using Xunit;

namespace Pine.Core.Tests.Elm.ElmCompilerInDotnet;

/// <summary>
/// Exercises <see cref="CompareInterpreterWithIntermediateVM"/> end-to-end: prepares the
/// framework against a small custom Elm module, evaluates several root expressions, and
/// asserts on both the agreed evaluated value and the metrics returned from the two
/// evaluation paths. At least one test renders the application trace as a string so the
/// rendering machinery is exercised by CI.
/// </summary>
public class CompareInterpreterWithIntermediateVMTests
{
    /// <summary>
    /// Custom Elm module hosting all entry points used by these tests. Kept intentionally
    /// small so the produced metrics and traces are easy to reason about in snapshot form.
    /// Each entry point covers one of the framework's intended use-cases:
    /// <list type="bullet">
    /// <item><c>identityChar</c>: zero-cost forwarding of a single argument; serves as the
    /// minimum-counter baseline.</item>
    /// <item><c>incrementInt</c>: invokes one Pine_kernel arithmetic function so the
    /// <c>PineBuiltinInvocationCount</c> column moves separately from the user-defined
    /// counts.</item>
    /// <item><c>sumList</c>: recursive case-of with user-defined recursion, exercising the
    /// direct-application count and the trace's user-function entries.</item>
    /// <item><c>mapWithLambda</c>: applies a higher-order argument internally, so the trace
    /// contains both direct and function-value entries.</item>
    /// </list>
    /// </summary>
    private const string TestModuleSource =
        """
        module CompareInterpreterTestModule exposing (..)


        identityChar : Char -> Char
        identityChar c =
            c


        incrementInt : Int -> Int
        incrementInt n =
            n + 1


        sumList : List Int -> Int
        sumList xs =
            case xs of
                [] ->
                    0

                head :: tail ->
                    head + sumList tail


        applyToEach : (Int -> Int) -> List Int -> List Int
        applyToEach f xs =
            case xs of
                [] ->
                    []

                head :: tail ->
                    f head :: applyToEach f tail


        mapWithLambda : List Int -> List Int
        mapWithLambda xs =
            applyToEach (\x -> x * 2) xs
        """;

    private static readonly Lazy<CompareInterpreterWithIntermediateVM> s_framework =
        new(BuildFramework);

    private static CompareInterpreterWithIntermediateVM BuildFramework()
    {
        var moduleNs = new[] { "CompareInterpreterTestModule" };

        return
            CompareInterpreterWithIntermediateVM.Prepare(
                elmModuleTexts: [TestModuleSource],
                entryPoints:
                [
                new DeclQualifiedName(moduleNs, "identityChar"),
                new DeclQualifiedName(moduleNs, "incrementInt"),
                new DeclQualifiedName(moduleNs, "sumList"),
                new DeclQualifiedName(moduleNs, "mapWithLambda"),
                ],
                maxOptimizationRounds: 1);
    }

    /// <summary>
    /// Baseline: a single-argument forwarding function returns its argument unchanged. This
    /// proves that the framework's parse-and-dispatch path handles a literal argument
    /// expression and that the VM and interpreter agree even on the cheapest possible call.
    /// </summary>
    [Fact]
    public void Eval_identity_function_returns_input_and_both_paths_agree()
    {
        var report = s_framework.Value.Eval("identityChar 'X'");

        ElmValue.RenderAsElmExpression(report.Value).expressionString.Should().Be("'X'");

        // The interpreter must observe at least one direct application: the call to
        // identityChar itself.
        report.InterpreterCounters.DirectFunctionApplicationCount.Should().BeGreaterThan(0);
    }

    /// <summary>
    /// Demonstrates that a Pine_kernel-using function increments the
    /// <see cref="ElmSyntaxInterpreterPerformanceCounters.PineBuiltinInvocationCount"/> on the
    /// interpreter side while the VM's <see cref="PerformanceCounters.InstructionCount"/>
    /// reflects the same arithmetic at a much lower level.
    /// </summary>
    [Fact]
    public void Eval_incrementInt_records_pine_builtin_invocation()
    {
        var report = s_framework.Value.Eval("incrementInt 41");

        ElmValue.RenderAsElmExpression(report.Value).expressionString.Should().Be("42");

        report.InterpreterCounters.PineBuiltinInvocationCount.Should().BeGreaterThan(0);
    }

    /// <summary>
    /// A recursive user-defined function should produce a sequence of direct applications in
    /// the trace, one for each list element processed plus the terminating empty-list call.
    /// This test reaches into <see cref="CompareInterpreterWithIntermediateVM.EvalReport.ApplicationLog"/>
    /// to confirm the trace shape and renders the trace via
    /// <see cref="CompareInterpreterWithIntermediateVM.RenderApplicationLog(IReadOnlyList{ApplicationLogEntry})"/>
    /// so the rendering machinery is also exercised.
    /// </summary>
    [Fact]
    public void Eval_sumList_records_one_direct_application_per_recursion_step()
    {
        var report = s_framework.Value.Eval("sumList [ 1, 2, 3 ]");

        ElmValue.RenderAsElmExpression(report.Value).expressionString.Should().Be("6");

        // Filter the trace for sumList recursion entries; expect 4 (lengths 3, 2, 1, 0).
        var sumListEntries =
            report.ApplicationLog
            .OfType<ApplicationLogEntry.Direct>()
            .Where(d => d.Application.FunctionName.DeclName == "sumList")
            .ToList();

        sumListEntries.Should().HaveCount(4);

        // Render the full trace and assert that it parses as a non-empty multi-line string
        // listing one direct application per line. We only assert on simple structural
        // properties to keep the test stable across cosmetic compiler changes.
        var rendered = CompareInterpreterWithIntermediateVM.RenderApplicationLog(report.ApplicationLog);

        rendered.Should().NotBeNullOrEmpty();

        var lines = rendered.Split('\n');

        lines.Should().AllSatisfy(
            line =>
            (line.StartsWith("direct ") || line.StartsWith("fnvalue "))
            .Should().BeTrue(because: "every trace line should start with 'direct ' or 'fnvalue '"));
    }

    /// <summary>
    /// A higher-order function exercises the function-value branch of the trace as well as
    /// the direct branch — at least when the optimizer leaves the closure in place. Even
    /// after specialization removes the explicit closure, the recursive direct-application
    /// trace and the agreed result demonstrate that the framework correctly evaluates a
    /// higher-order call site through both paths.
    /// </summary>
    [Fact]
    public void Eval_mapWithLambda_doubles_each_input_element()
    {
        var report = s_framework.Value.Eval("mapWithLambda [ 10, 20, 30 ]");

        ElmValue.RenderAsElmExpression(report.Value).expressionString.Should().Be("[ 20, 40, 60 ]");

        // The traced applications should contain at least one direct call into the test
        // module — either to mapWithLambda itself or to its specialized companion that the
        // optimizer may have generated.
        report.ApplicationLog
            .OfType<ApplicationLogEntry.Direct>()
            .Where(
            d =>
            d.Application.FunctionName.Namespaces.Count is 1
            && d.Application.FunctionName.Namespaces[0] == "CompareInterpreterTestModule")
            .Should().NotBeEmpty();
    }

    /// <summary>
    /// Misuse test: looking up a function name that is not a registered entry point must
    /// throw with a descriptive message rather than silently dispatching to whatever happens
    /// to share that name in the post-pipeline declarations dictionary.
    /// </summary>
    [Fact]
    public void Eval_unregistered_function_throws_descriptive_error()
    {
        var act = () => s_framework.Value.Eval("notAnEntryPoint 0");

        act.Should().Throw<Exception>()
            .Which.Message.Should().Contain("notAnEntryPoint");
    }
}
