using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.CommonEncodings;
using Pine.Core.Elm;
using Pine.Core.Interpreter;
using Pine.Core.Interpreter.IntermediateVM;
using Pine.Core.Interpreter.IntermediateVM.Frontend;
using System;
using System.Collections.Immutable;
using System.Linq;
using Xunit;

namespace Pine.Core.Tests.Elm.ElmCompilerInDotnet;

public class GraphFirstCompilationCharacterizationTests
{
    private const string ExampleAlfaModule =
        """
        module Test exposing (..)


        skipIdentifier source offset =
            if isIdentifierStart (String.left 1 (String.dropLeft offset source)) then
                skipToIdentifierEnd source (offset + 1)

            else
                offset


        isIdentifierStart character =
            case character of
                "_" ->
                    True

                "a" ->
                    True

                "Z" ->
                    True

                _ ->
                    False


        skipToIdentifierEnd source offset =
            if isIdentifierChar (String.left 1 (String.dropLeft offset source)) then
                skipToIdentifierEnd source (offset + 1)

            else
                offset


        isIdentifierChar character =
            case character of
                "_" ->
                    True

                "0" ->
                    True

                "a" ->
                    True

                "Z" ->
                    True

                _ ->
                    False
        """;

    [Fact]
    public void GraphValue_actual_Alfa_wrapper_carries_only_the_function_table_not_runtime_arguments()
    {
        var parsed = ElmCompilerTestHelper.CompileElmModules([ExampleAlfaModule], disableInlining: true).parsedEnv;
        var functionValue = parsed.Modules.Single(module => module.moduleName is "Test")
            .moduleContent.FunctionDeclarations["skipIdentifier"];
        var function = FunctionRecord.ParseFunctionRecordTagged(functionValue, new PineVMParseCache())
            .Extract(error => throw new InvalidOperationException(error));
        function.ParameterCount.Should().Be(2);
        function.ArgumentsAlreadyCollected.Length.Should().Be(0);
        var table = new Expression.Litral(PineValue.List(function.EnvFunctions));
        var source = Argument(0);
        var offset = Argument(1);
        var environment = function.UsesNestedArgFormat
            ? new Expression.List([table, new Expression.List([source, offset])])
            : new Expression.List([table, source, offset]);
        var wrapper = new Expression.Eval(
            new Expression.Litral(ExpressionEncoding.EncodeExpressionAsValue(function.InnerFunction)), environment);
        // The real declaration is larger than the deliberately small default inline-body policy.
        var policy = new GraphOptimizerOptions(
            MaxCandidates: 8, MaxExpansionUnits: 100_000, MaxWorkUnits: 1_000_000,
            MaxBodyNodes: 4096, MaxDepth: 128, MaxAnalysisWorkUnits: 1_000_000);
        var compiled = ExpressionGraphOptimizer.Compile(
            CompilationRequest.Capture(wrapper, new(DisableReduction: true)), policy, CompilerMemo.Empty)
            .Extract(errors => throw new InvalidOperationException(string.Join(", ", errors)));
        Console.WriteLine("Actual Alfa closed-table wrapper: " + compiled.Stats);
        compiled.Stats.InlinedCalls.Should().Be(3);
        compiled.Stats.SelfTailCalls.Should().Be(0);
        compiled.Stats.AnalysisWorkUnits.Should().Be(policy.MaxAnalysisWorkUnits);
        compiled.Stats.BudgetLimitReached.Should().BeTrue();
        var baselineVM = ExpressionGraphVM.Create();
        var optimizedVM = ExpressionGraphVM.Create(optimizerOptions: policy);
        ImmutableArray<(string Source, int Offset, int Expected)> cases =
            [
            ("", 0, 0), ("!", 0, 0), ("0a", 0, 0), ("a", 0, 1), ("_", 0, 1),
            ("Z0_a!", 0, 4), ("!a0_Z!", 1, 5), ("a!Z", 0, 1), ("a", 1, 1),
            ("a", 3, 3), ("λa", 0, 0), ("λa", 1, 2), ("a0000000000000000!", 0, 17),
            ];
        var reports = cases.Select(Evaluate).ToImmutableArray();
        Console.WriteLine("Actual Alfa baseline invocations: " + reports.Sum(pair => pair.Before.InvocationCount) +
            "; optimized invocations: " + reports.Sum(pair => pair.After.InvocationCount));
        var baselineCounters = PerformanceCounters.Aggregate(reports.Select(pair => pair.Before.Counters));
        var optimizedCounters = PerformanceCounters.Aggregate(reports.Select(pair => pair.After.Counters));
        Console.WriteLine("Actual Alfa graph optimizer disabled (not the legacy production baseline):\n" +
            PerformanceCountersFormatting.FormatCounts(baselineCounters));
        Console.WriteLine("Actual Alfa graph optimizer enabled:\n" +
            PerformanceCountersFormatting.FormatCounts(optimizedCounters));
        reports.Sum(pair => pair.After.BuildListCount).Should().Be(reports.Sum(pair => pair.Before.BuildListCount));
        reports.Sum(pair => pair.Before.InvocationCount).Should().Be(42);
        reports.Sum(pair => pair.After.InvocationCount).Should().Be(19);

        static Expression Argument(int index) => new Expression.Builtin("head",
            new Expression.Builtin("skip", new Expression.List([
                new Expression.Litral(IntegerEncoding.EncodeSignedInteger(index)), Expression.EnvironmentInstance])));

        (EvaluationReport Before, EvaluationReport After) Evaluate((string Source, int Offset, int Expected) testCase)
        {
            var input = PineValue.List([
                ElmValueEncoding.ElmValueAsPineValue(ElmValue.StringInstance(testCase.Source)),
                IntegerEncoding.EncodeSignedInteger(testCase.Offset)]);
            var expected = IntegerEncoding.EncodeSignedInteger(testCase.Expected);
            new DirectInterpreter(new PineVMParseCache(), evalCache: null)
                .EvaluateExpressionDefault(wrapper, input).Should().Be(expected);
            var before = baselineVM.EvaluateExpressionOnCustomStack(wrapper, input, new(100_000, 100_000, 1000))
                .Extract(error => throw new InvalidOperationException(error.ToString()));
            var after = optimizedVM.EvaluateExpressionOnCustomStack(wrapper, input, new(100_000, 100_000, 1000))
                .Extract(error => throw new InvalidOperationException(error.ToString()));
            before.ReturnValue.Evaluate().Should().Be(expected);
            after.ReturnValue.Evaluate().Should().Be(expected);
            return (before, after);
        }
    }

    [Theory]
    [InlineData(false, false)]
    [InlineData(false, true)]
    [InlineData(true, false)]
    [InlineData(true, true)]
    public void Example_alfa_matches_direct_interpretation_and_records_cost(
        bool disableInlining,
        bool enableTailRecursionOptimization)
    {
        var parsedEnvironment =
            ElmCompilerTestHelper.CompileElmModules(
                [ExampleAlfaModule],
                disableInlining).parsedEnv;

        var functionValue =
            parsedEnvironment.Modules
            .Single(module => module.moduleName is "Test")
            .moduleContent.FunctionDeclarations["skipIdentifier"];

        var functionRecord =
            FunctionRecord.ParseFunctionRecordTagged(functionValue, new PineVMParseCache())
            .Extract(error => throw new InvalidOperationException(error));

        ImmutableArray<(string Source, int Offset, int Expected)> cases =
            [
            ("", 0, 0),
            ("!", 0, 0),
            ("0a", 0, 0),
            ("a", 0, 1),
            ("_", 0, 1),
            ("Z0_a!", 0, 4),
            ("!a0_Z!", 1, 5),
            ("a!Z", 0, 1),
            ("a", 1, 1),
            ("a", 3, 3),
            ("λa", 0, 0),
            ("λa", 1, 2),
            ("a0000000000000000!", 0, 17),
            ];

        EvaluationReport Evaluate((string Source, int Offset, int Expected) testCase)
        {
            var composed =
                ElmInteractiveEnvironment.ApplyFunctionArgumentsForEvalExpr(
                    functionRecord,
                    appendArguments:
                    [
                    ElmValueEncoding.ElmValueAsPineValue(ElmValue.StringInstance(testCase.Source)),
                    IntegerEncoding.EncodeSignedInteger(testCase.Offset),
                    ])
                .Extract(error => throw new InvalidOperationException(error));

            var expected = IntegerEncoding.EncodeSignedInteger(testCase.Expected);

            var interpreted =
                new DirectInterpreter(new PineVMParseCache(), evalCache: null)
                .EvaluateExpressionDefault(composed.expression, composed.environment);

            interpreted.Should().Be(expected, $"source '{testCase.Source}', offset {testCase.Offset}");

            // A fresh VM and disabled invocation caching keep the counters independent of case order.
            var vm =
                ElmCompilerTestHelper.PineVMForProfiling(
                    reportFunctionApplication: _ => { },
                    enableTailRecursionOptimization);

            var report =
                vm.EvaluateExpressionOnCustomStack(
                    composed.expression,
                    composed.environment,
                    new Core.Interpreter.IntermediateVM.PineVM.EvaluationConfig(
                        InvocationCountLimit: 100_000,
                        LoopIterationCountLimit: 100_000,
                        StackDepthLimit: 1_000))
                .Extract(error => throw new InvalidOperationException(error.ToString()));

            report.ReturnValue.Evaluate().Should().Be(interpreted);

            return report;
        }

        var reports = cases.Select(Evaluate).ToImmutableArray();
        var counters = PerformanceCounters.Aggregate(reports.Select(report => report.Counters));

        var expectedCounters =
            enableTailRecursionOptimization
            ?
            """
            InvocationCount: 3
            BuildListCount: 3
            LoopIterationCount: 16
            InstructionCount: 418
            """
            :
            """
            InvocationCount: 19
            BuildListCount: 19
            LoopIterationCount: 0
            InstructionCount: 370
            """;

        PerformanceCountersFormatting.FormatCounts(counters).Should().Be(
            expectedCounters,
            "the characterization counters are:\n{0}",
            PerformanceCountersFormatting.FormatCounts(counters));
    }
}
