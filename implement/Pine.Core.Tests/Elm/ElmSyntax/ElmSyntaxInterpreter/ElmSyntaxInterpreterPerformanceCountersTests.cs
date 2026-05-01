using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.Elm;
using Pine.Core.Elm.ElmInElm;
using Pine.Core.Elm.ElmSyntax;
using System;
using System.Collections.Generic;
using System.Text;
using Xunit;

using ElmInterpreter = Pine.Core.Elm.ElmSyntax.ElmSyntaxInterpreter;
using SyntaxTypes = Pine.Core.Elm.ElmSyntax.SyntaxModel;

namespace Pine.Core.Tests.Elm.ElmSyntax.ElmSyntaxInterpreter;

/// <summary>
/// Exercises <see cref="ElmInterpreter.ParseAndInterpretWithCounters(string, IReadOnlyDictionary{DeclQualifiedName, SyntaxTypes.Declaration})"/>
/// and asserts on the resulting <see cref="ElmSyntaxInterpreterPerformanceCounters"/> snapshot,
/// rendered via <see cref="ElmSyntaxInterpreterPerformanceCountersFormatting.FormatCounts"/>.
/// This is the <see cref="ElmInterpreter"/> analog of the IR VM
/// <c>Expression_int_literal</c> snapshot tests in
/// <c>ElmParserExpressionTests</c>: each scenario asserts both the evaluated value and a
/// fixed counter snapshot so regressions in interpreter cost are visible at test-review time.
/// <para />
/// The scenarios are arranged from "no higher-order arguments at all" up to
/// "branch-dependent application of a higher-order argument" so the differences between
/// the four counters — total trampoline iterations, direct (named) function applications,
/// applications of evaluated function values, and Pine_builtin invocations — are visible
/// in the snapshots.
/// </summary>
public class ElmSyntaxInterpreterPerformanceCountersTests
{
    /// <summary>
    /// Custom test module: declares <c>mapPositives f xs</c>, a List.map analog that
    /// only applies <c>f</c> to elements <c>x</c> for which <c>x &gt;= 0</c> and leaves
    /// negative elements unchanged. The branching test uses this to demonstrate that
    /// runtime application counts of a higher-order argument depend on the input.
    /// </summary>
    private const string TestModuleSource =
        """
        module ElmSyntaxInterpreterPerformanceCountersTestModule exposing (..)


        mapPositives : (Int -> Int) -> List Int -> List Int
        mapPositives f xs =
            case xs of
                [] ->
                    []

                x :: rest ->
                    if x >= 0 then
                        f x :: mapPositives f rest

                    else
                        x :: mapPositives f rest
        """;

    private static readonly Lazy<IReadOnlyDictionary<DeclQualifiedName, SyntaxTypes.Declaration>> s_declarations =
        new(LoadDeclarations);

    private static IReadOnlyDictionary<DeclQualifiedName, SyntaxTypes.Declaration> LoadDeclarations()
    {
        // Merge Basics.elm (operators, comparisons, arithmetic) and List.elm (map / foldl)
        // and the custom test module above into a single empty-namespace declarations
        // dictionary, in the same style as CoreBasicsTests.LoadBasicsDeclarations.
        var merged = new Dictionary<DeclQualifiedName, SyntaxTypes.Declaration>();

        foreach (var (key, value) in LoadKernelModuleDeclarations("Basics.elm"))
        {
            merged[key] = value;
        }

        foreach (var (key, value) in LoadKernelModuleDeclarations("List.elm"))
        {
            merged[key] = value;
        }

        foreach (var (key, value) in InterpreterTestHelper.ParseDeclarationsRemovingModuleNames(TestModuleSource))
        {
            merged[key] = value;
        }

        return merged;
    }

    private static IReadOnlyDictionary<DeclQualifiedName, SyntaxTypes.Declaration>
        LoadKernelModuleDeclarations(string fileName)
    {
        var moduleNode =
            BundledFiles.ElmKernelModulesDefault.Value
                .GetNodeAtPath([fileName])
            ?? throw new Exception("Did not find elm-kernel-modules/" + fileName + " in bundled files.");

        if (moduleNode is not Files.FileTree.FileNode moduleFile)
        {
            throw new Exception(
                "Expected elm-kernel-modules/" + fileName + " to be a file node, but got: " + moduleNode.GetType());
        }

        var moduleSource = Encoding.UTF8.GetString(moduleFile.Bytes.Span);

        return InterpreterTestHelper.ParseDeclarationsRemovingModuleNames(moduleSource);
    }

    /// <summary>
    /// Evaluates the supplied Elm <paramref name="expression"/> against the merged
    /// Basics + List + test-module declarations, returning the rendered Elm-expression
    /// form of the result alongside a formatted snapshot of the
    /// <see cref="ElmSyntaxInterpreterPerformanceCounters"/> recorded for the run.
    /// </summary>
    private static (string Rendered, string CountersSnapshot) EvaluateAndFormatCounters(string expression)
    {
        var (result, counters) =
            ElmInterpreter.ParseAndInterpretWithCounters(expression, s_declarations.Value);

        var value =
            result.Extract(err => throw new Exception(err.ToString()));

        return
            (ElmValue.RenderAsElmExpression(value).expressionString,
             ElmSyntaxInterpreterPerformanceCountersFormatting.FormatCounts(counters));
    }

    // ============================================================
    // Scenario 1: simple evaluation, no higher-order arguments at all.
    // ============================================================

    [Fact]
    public void Counters_for_simple_arithmetic_without_higher_order_arguments()
    {
        var (value, snapshot) = EvaluateAndFormatCounters("1 + 2 + 3");

        value.Should().Be("6");

        snapshot.Should().Be(
            """
            InstructionLoopCount: 28
            DirectFunctionApplicationCount: 4
            FunctionValueApplicationCount: 0
            PineBuiltinInvocationCount: 2
            """);
    }

    // ============================================================
    // Scenario 2: higher-order argument with arity 1 (List.map).
    // ============================================================

    [Fact]
    public void Counters_for_List_map_with_arity_1_higher_order_argument_over_ten_items()
    {
        var (value, snapshot) =
            EvaluateAndFormatCounters("map (\\x -> x + 1) [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]");

        value.Should().Be("[ 2, 3, 4, 5, 6, 7, 8, 9, 10, 11 ]");

        snapshot.Should().Be(
            """
            InstructionLoopCount: 412
            DirectFunctionApplicationCount: 43
            FunctionValueApplicationCount: 10
            PineBuiltinInvocationCount: 21
            """);
    }

    // ============================================================
    // Scenario 3: higher-order argument with arity 2 (List.foldl).
    // ============================================================

    [Fact]
    public void Counters_for_List_foldl_with_arity_2_higher_order_argument_over_ten_items()
    {
        var (value, snapshot) =
            EvaluateAndFormatCounters("foldl (+) 0 [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]");

        value.Should().Be("55");

        snapshot.Should().Be(
            """
            InstructionLoopCount: 274
            DirectFunctionApplicationCount: 22
            FunctionValueApplicationCount: 10
            PineBuiltinInvocationCount: 10
            """);
    }

    // ============================================================
    // Scenario 4: branch-dependent application of a higher-order argument.
    //
    // `mapPositives f xs` only applies `f` to elements `x >= 0`. Running the same
    // function against two different ten-element lists demonstrates that the number
    // of *function-value* applications differs based on the runtime branch taken,
    // even though the Elm source code is identical for both calls.
    // ============================================================

    [Fact]
    public void Counters_for_branching_higher_order_with_all_ten_items_passing_predicate()
    {
        var (value, snapshot) =
            EvaluateAndFormatCounters(
                "mapPositives (\\x -> x * 10) [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]");

        value.Should().Be("[ 10, 20, 30, 40, 50, 60, 70, 80, 90, 100 ]");

        snapshot.Should().Be(
            """
            InstructionLoopCount: 1_262
            DirectFunctionApplicationCount: 161
            FunctionValueApplicationCount: 10
            PineBuiltinInvocationCount: 80
            """);
    }

    [Fact]
    public void Counters_for_branching_higher_order_with_only_half_of_ten_items_passing_predicate()
    {
        var (value, snapshot) =
            EvaluateAndFormatCounters(
                "mapPositives (\\x -> x * 10) [-1, -2, -3, -4, -5, 6, 7, 8, 9, 10]");

        value.Should().Be("[ -1, -2, -3, -4, -5, 60, 70, 80, 90, 100 ]");

        // Note: FunctionValueApplicationCount drops from 10 to 5 compared to the
        // all-positive variant above — the higher-order argument is only applied
        // for the five items that pass the runtime predicate. The other counters
        // stay close to the all-positive variant because the same number of
        // recursive calls and predicate checks are performed in both cases.
        snapshot.Should().Be(
            """
            InstructionLoopCount: 1_152
            DirectFunctionApplicationCount: 151
            FunctionValueApplicationCount: 5
            PineBuiltinInvocationCount: 75
            """);
    }
}
