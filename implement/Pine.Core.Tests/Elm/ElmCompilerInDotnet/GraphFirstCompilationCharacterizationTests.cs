using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.CommonEncodings;
using Pine.Core.Elm;
using Pine.Core.Interpreter;
using Pine.Core.Interpreter.IntermediateVM;
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
