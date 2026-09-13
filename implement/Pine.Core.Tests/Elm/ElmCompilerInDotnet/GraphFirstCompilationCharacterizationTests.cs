using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.CommonEncodings;
using Pine.Core.Elm;
using Pine.Core.Interpreter;
using Pine.Core.Internal;
using Pine.Core.Interpreter.IntermediateVM;
using Pine.Core.Interpreter.IntermediateVM.Backend;
using Pine.Core.Interpreter.IntermediateVM.Frontend;
using Pine.Core.Interpreter.IntermediateVM.Semantic;
using System;
using System.Collections.Generic;
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
    [InlineData(false)]
    [InlineData(true)]
    public void GraphValue_actual_Alfa_wrapper_carries_only_the_function_table_not_runtime_arguments(bool capturedUsageSite)
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
        var call = new Expression.Eval(
            new Expression.Litral(ExpressionEncoding.EncodeExpressionAsValue(function.InnerFunction)), environment);
        Expression wrapper = capturedUsageSite
            ? new Expression.Builtin("int_add", new Expression.List([call, offset]))
            : call;
        // Inspect generous diagnostic bounds and independently verify the default policy below.
        var policy = new GraphOptimizerOptions(
            MaxCandidates: 8, MaxExpansionUnits: 100_000, MaxWorkUnits: 1_000_000,
            MaxBodyNodes: 4096, MaxDepth: 128, MaxAnalysisWorkUnits: 1_000_000);
        var compiled = ExpressionGraphOptimizer.Compile(
            CompilationRequest.Capture(wrapper, new(DisableReduction: true)), policy, CompilerMemo.Empty)
            .Extract(errors => throw new InvalidOperationException(string.Join(", ", errors)));
        Console.WriteLine("Actual Alfa closed-table wrapper: " + compiled.Stats);
        compiled.Stats.BudgetLimitReached.Should().BeFalse();
        compiled.Stats.InlinedCalls.Should().Be(2);
        compiled.Graph.Graph.Blocks.Values.Where(block =>
            block.Terminator is Terminator.Invoke or Terminator.TailInvoke).Should().BeEmpty();
        var defaults = ExpressionGraphOptimizer.Compile(
            CompilationRequest.Capture(wrapper, new(DisableReduction: true)), new(), CompilerMemo.Empty)
            .Extract(errors => throw new InvalidOperationException(string.Join(", ", errors)));
        defaults.Graph.Graph.Blocks.Values.Where(block =>
            block.Terminator is Terminator.Invoke or Terminator.TailInvoke).Should().BeEmpty();
        var artifact = GraphCompiler.Compile(compiled.Graph, fuseScalarBuiltins: true, compact: true)
            .Extract(error => throw new InvalidOperationException(error.ToString()));
        var instructions = GraphVMAdapter.ToStackFrame(artifact).Instructions;
        instructions.Where(instruction => instruction.Kind is StackInstructionKind.Build_List or StackInstructionKind.Build_List_With_Prefix)
            .Should().BeEmpty();
        instructions.Count.Should().BeLessThan(1500);
        var baselineVM = ExpressionGraphVM.Create();
        var optimizedVM = ExpressionGraphVM.Create(optimizerOptions: policy);
        var productionVM = ElmCompilerTestHelper.PineVMForProfiling(
            reportFunctionApplication: _ => { }, enableTailRecursionOptimization: true);
        var tableValue = PineValue.List(function.EnvFunctions);
        var tableConstraint = PineValueClass.Create([
            new KeyValuePair<IReadOnlyList<int>, PineValue>(ImmutableArray.Create(0), tableValue)]);
        var specializedVM = Core.Interpreter.IntermediateVM.PineVM.CreateCustom(
            evalCache: null, evaluationConfigDefault: null, reportFunctionApplication: null,
            compilationEnvClasses: ImmutableDictionary<Expression, IReadOnlyList<PineValueClass>>.Empty.Add(
                function.InnerFunction, [tableConstraint]),
            disableReductionInCompilation: false, selectPrecompiled: null, skipInlineForExpression: _ => false,
            enableTailRecursionOptimization: true, parseCache: null, precompiledLeaves: null,
            reportEnterPrecompiledLeaf: null, reportExitPrecompiledLeaf: null,
            optimizationParametersSerial: null, cacheFileStore: null);
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
        var productionCounters = PerformanceCounters.Aggregate(reports.Select(pair => pair.Production.Counters));
        Console.WriteLine("Actual Alfa ordinary production compiler:\n" +
            PerformanceCountersFormatting.FormatCounts(productionCounters));
        productionCounters.InvocationCount.Should().Be(0);
        productionCounters.BuildListCount.Should().Be(0);
        productionCounters.InstructionCount.Should().Be(capturedUsageSite ? 434 : 384);
        productionCounters.LoopIterationCount.Should().Be(22);
        Console.WriteLine("Actual Alfa graph optimizer disabled (not the legacy production baseline):\n" +
            PerformanceCountersFormatting.FormatCounts(baselineCounters));
        Console.WriteLine("Actual Alfa graph optimizer enabled:\n" +
            PerformanceCountersFormatting.FormatCounts(optimizedCounters));
        reports.Sum(pair => pair.After.BuildListCount).Should().Be(0);
        reports.Sum(pair => pair.Before.InvocationCount).Should().Be(42);
        reports.Sum(pair => pair.After.InvocationCount).Should().Be(0);
        optimizedCounters.InstructionCount.Should().BeLessThan(baselineCounters.InstructionCount);

        foreach (var longCase in ImmutableArray.Create(
            (Source: "a" + new string('0', 4096) + "!", Offset: 0, Expected: 4097),
            (Source: "!Z" + new string('_', 2048) + "!", Offset: 1, Expected: 2050)))
        {
            var longInput = PineValue.List([
                ElmValueEncoding.ElmValueAsPineValue(ElmValue.StringInstance(longCase.Source)),
                IntegerEncoding.EncodeSignedInteger(longCase.Offset)]);
            var longReport = optimizedVM.EvaluateExpressionOnCustomStack(wrapper, longInput, new(0, 100_000, 1))
                .Extract(error => throw new InvalidOperationException(error.ToString()));
            longReport.ReturnValue.Evaluate().Should().Be(
                IntegerEncoding.EncodeSignedInteger(longCase.Expected + (capturedUsageSite ? longCase.Offset : 0)));
            longReport.InvocationCount.Should().Be(0);
            longReport.BuildListCount.Should().Be(0);
            longReport.LoopIterationCount.Should().BeGreaterThanOrEqualTo(longCase.Expected - longCase.Offset - 1);
            optimizedVM.EvaluateExpressionOnCustomStack(wrapper, longInput, new(0, 8, 1))
                .IsErrOrNull()!.Reason.Should().BeOfType<EvaluationErrorReason.QuotaExhausted>()
                .Which.QuotaKind.Should().Be(EvaluationQuotaKind.LoopIterationCount);
            var productionLong = productionVM.EvaluateExpressionOnCustomStack(wrapper, longInput, new(0, 100_000, 1))
                .Extract(error => throw new InvalidOperationException(error.ToString()));
            productionLong.ReturnValue.Evaluate().Should().Be(longReport.ReturnValue.Evaluate());
            productionLong.InvocationCount.Should().Be(0);
            productionLong.BuildListCount.Should().Be(0);
            productionVM.EvaluateExpressionOnCustomStack(wrapper, longInput, new(0, 8, 1))
                .IsErrOrNull()!.Reason.Should().BeOfType<EvaluationErrorReason.QuotaExhausted>()
                .Which.QuotaKind.Should().Be(EvaluationQuotaKind.LoopIterationCount);
            var specializedInput = DeclarationEnvironment(tableValue, longInput);
            var specializedLong = specializedVM.EvaluateExpressionOnCustomStack(
                    function.InnerFunction, specializedInput, new(0, 100_000, 1))
                .Extract(error => throw new InvalidOperationException(error.ToString()));
            specializedLong.ReturnValue.Evaluate().Should().Be(IntegerEncoding.EncodeSignedInteger(longCase.Expected));
            specializedLong.BuildListCount.Should().Be(0);
            specializedVM.EvaluateExpressionOnCustomStack(function.InnerFunction, specializedInput, new(0, 8, 1))
                .IsErrOrNull()!.Reason.Should().BeOfType<EvaluationErrorReason.QuotaExhausted>()
                .Which.QuotaKind.Should().Be(EvaluationQuotaKind.LoopIterationCount);
        }

        var mismatch = DeclarationEnvironment(PineValue.List([.. function.EnvFunctions.Span, PineValue.EmptyList]), PineValue.List([
            ElmValueEncoding.ElmValueAsPineValue(ElmValue.StringInstance("a000!")), IntegerEncoding.EncodeSignedInteger(0)]));
        var declarationCompilation = ExpressionCompilation.CompileExpression(
            function.InnerFunction, [tableConstraint], new(), false, true, (_, _) => false);
        declarationCompilation.SelectInstructionsForEnvironment(PineValueInProcess.Create(mismatch))
            .Should().Be(declarationCompilation.Generic);
        specializedVM.EvaluateExpressionOnCustomStack(function.InnerFunction, mismatch, new(100, 100, 10))
            .Extract(error => throw new InvalidOperationException(error.ToString()))
            .ReturnValue.Evaluate().Should().Be(
                new DirectInterpreter(new(), null).EvaluateExpressionDefault(function.InnerFunction, mismatch));

        PineValue DeclarationEnvironment(PineValue functions, PineValue arguments) =>
            function.UsesNestedArgFormat
                ? PineValue.List([functions, arguments])
                : PineValue.List([functions, .. ((PineValue.ListValue)arguments).Items.ToArray()]);

        static Expression Argument(int index) => new Expression.Builtin("head",
            new Expression.Builtin("skip", new Expression.List([
                new Expression.Litral(IntegerEncoding.EncodeSignedInteger(index)), Expression.EnvironmentInstance])));

        (EvaluationReport Before, EvaluationReport After, EvaluationReport Production) Evaluate((string Source, int Offset, int Expected) testCase)
        {
            var input = PineValue.List([
                ElmValueEncoding.ElmValueAsPineValue(ElmValue.StringInstance(testCase.Source)),
                IntegerEncoding.EncodeSignedInteger(testCase.Offset)]);
            var expected = IntegerEncoding.EncodeSignedInteger(testCase.Expected + (capturedUsageSite ? testCase.Offset : 0));
            new DirectInterpreter(new PineVMParseCache(), evalCache: null)
                .EvaluateExpressionDefault(wrapper, input).Should().Be(expected);
            var before = baselineVM.EvaluateExpressionOnCustomStack(wrapper, input, new(100_000, 100_000, 1000))
                .Extract(error => throw new InvalidOperationException(error.ToString()));
            var after = optimizedVM.EvaluateExpressionOnCustomStack(wrapper, input, new(0, 100_000, 1))
                .Extract(error => throw new InvalidOperationException(error.ToString()));
            before.ReturnValue.Evaluate().Should().Be(expected);
            after.ReturnValue.Evaluate().Should().Be(expected);
            var production = productionVM.EvaluateExpressionOnCustomStack(wrapper, input, new(100_000, 100_000, 1000))
                .Extract(error => throw new InvalidOperationException(error.ToString()));
            production.ReturnValue.Evaluate().Should().Be(expected);
            var specialized = specializedVM.EvaluateExpressionOnCustomStack(
                    function.InnerFunction, DeclarationEnvironment(tableValue, input), new(0, 100_000, 1))
                .Extract(error => throw new InvalidOperationException(error.ToString()));
            specialized.ReturnValue.Evaluate().Should().Be(IntegerEncoding.EncodeSignedInteger(testCase.Expected));
            specialized.InvocationCount.Should().Be(0);
            specialized.BuildListCount.Should().Be(0);
            return (before, after, production);
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

        // The former tail-enabled snapshot was 3/3/16/418. Production now removes all
        // calls/lists without peeling initial iterations, at five additional instructions.
        var expectedCounters =
            enableTailRecursionOptimization
            ?
            """
            InvocationCount: 0
            BuildListCount: 0
            LoopIterationCount: 22
            InstructionCount: 423
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
