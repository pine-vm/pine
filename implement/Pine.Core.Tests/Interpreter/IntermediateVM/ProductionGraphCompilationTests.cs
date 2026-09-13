using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.CommonEncodings;
using Pine.Core.Internal;
using Pine.Core.Interpreter;
using Pine.Core.Interpreter.IntermediateVM;
using Pine.Core.PineVM;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using Xunit;
using VM = Pine.Core.Interpreter.IntermediateVM.PineVM;

namespace Pine.Core.Tests.Interpreter.IntermediateVM;

public class ProductionGraphCompilationTests
{
    private static Expression Env => Expression.EnvironmentInstance;
    private static Expression Literal(PineValue value) => new Expression.Litral(value);
    private static Expression Integer(int value) => Literal(IntegerEncoding.EncodeSignedInteger(value));
    private static Expression List(params Expression[] items) => new Expression.List(items);
    private static Expression Add(Expression left, Expression right) => new Expression.Builtin("int_add", List(left, right));
    private static Expression At(int index) => new Expression.Builtin("head",
        new Expression.Builtin("skip", List(Integer(index), Env)));
    private static Expression Counter(int increment = 1) => new Expression.Conditional(
        new Expression.Builtin("equal", List(At(1), At(2))),
        new Expression.Eval(At(0), List(At(0), Add(At(1), Integer(increment)), At(2))),
        At(1));
    private static Expression InvokeCounter(Expression body, Expression start, Expression end)
    {
        var encoded = Literal(ExpressionEncoding.EncodeExpressionAsValue(body));
        return new Expression.Eval(encoded, List(encoded, start, end));
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void Ordinary_production_compiler_inlines_two_and_nested_counter_calls_without_frames_or_lists(bool nested)
    {
        var first = Counter();
        var second = Counter(2);
        var firstCall = InvokeCounter(first, At(0), At(1));
        var expression = nested
            ? InvokeCounter(second, firstCall, At(2))
            : Add(firstCall, InvokeCounter(second, At(0), At(2)));
        var input = Values(0, 4, 10);
        var vm = Create();
        var report = vm.EvaluateExpressionOnCustomStack(expression, input, new(0, 100, 1))
            .Extract(error => throw new InvalidOperationException(error.ToString()));
        report.ReturnValue.Evaluate().Should().Be(
            new DirectInterpreter(new(), null).EvaluateExpressionDefault(expression, input));
        report.InvocationCount.Should().Be(0);
        report.BuildListCount.Should().Be(0);
        report.LoopIterationCount.Should().BeGreaterThan(0);
        vm.EvaluateExpressionOnCustomStack(expression, Values(0, 4096, 8192), new(0, 4, 1))
            .IsErrOrNull()!.Reason.Should().BeOfType<EvaluationErrorReason.QuotaExhausted>()
            .Which.QuotaKind.Should().Be(EvaluationQuotaKind.LoopIterationCount);
    }

    [Theory]
    [InlineData("reduction")]
    [InlineData("tail")]
    [InlineData("excluded")]
    [InlineData("threshold")]
    [InlineData("consolidation")]
    public void Explicit_production_policies_retain_the_legacy_route(string policy)
    {
        var body = Counter();
        var expression = InvokeCounter(body, At(0), At(1));
        var input = Values(0, 32);
        var vm = Create(
            disableReduction: policy == "reduction",
            tail: policy != "tail",
            excluded: policy == "excluded" ? body : null,
            nondefaultThreshold: policy == "threshold",
            disableConsolidation: policy == "consolidation");
        var report = vm.EvaluateExpressionOnCustomStack(expression, input, new(1000, 1000, 100))
            .Extract(error => throw new InvalidOperationException(error.ToString()));
        report.ReturnValue.Evaluate().Should().Be(IntegerEncoding.EncodeSignedInteger(32));
        report.InvocationCount.Should().BeGreaterThan(0);
    }

    [Fact]
    public void Excluding_a_nested_callee_cannot_be_bypassed_by_an_allowed_outer_callee()
    {
        var first = Counter();
        var excluded = Counter(2);
        var expression = Add(InvokeCounter(first, At(0), At(1)), InvokeCounter(excluded, At(0), At(2)));
        var report = Create(excluded: excluded).EvaluateExpressionOnCustomStack(expression, Values(0, 8, 16), new(1000, 1000, 100))
            .Extract(error => throw new InvalidOperationException(error.ToString()));
        report.ReturnValue.Evaluate().Should().Be(IntegerEncoding.EncodeSignedInteger(24));
        report.InvocationCount.Should().BeGreaterThan(0);
    }

    [Fact]
    public void Specialized_entry_selection_does_not_specialize_changed_recursive_arguments()
    {
        var body = Counter();
        var encoding = ExpressionEncoding.EncodeExpressionAsValue(body);
        var root = new Expression.Eval(At(0), List(At(0), At(1), At(2)));
        var general = PineValueClass.Create([
            new KeyValuePair<IReadOnlyList<int>, PineValue>(ImmutableArray.Create(0), encoding)]);
        var specific = PineValueClass.Create([
            new KeyValuePair<IReadOnlyList<int>, PineValue>(ImmutableArray.Create(0), encoding),
            new KeyValuePair<IReadOnlyList<int>, PineValue>(ImmutableArray.Create(1), IntegerEncoding.EncodeSignedInteger(0))]);
        var compilation = ExpressionCompilation.CompileExpression(root, [general, specific], new(), false, true, (_, _) => false);
        var matching = PineValue.List([encoding, IntegerEncoding.EncodeSignedInteger(0), IntegerEncoding.EncodeSignedInteger(32)]);
        compilation.SelectInstructionsForEnvironment(PineValueInProcess.Create(matching)).TrackEnvConstraint.Should().Be(specific);
        var classes = ImmutableDictionary<Expression, IReadOnlyList<PineValueClass>>.Empty.Add(root, [general, specific]);
        var vm = Create(classes: classes);
        var report = vm.EvaluateExpressionOnCustomStack(root, matching, new(0, 100, 1))
            .Extract(error => throw new InvalidOperationException(error.ToString()));
        report.ReturnValue.Evaluate().Should().Be(IntegerEncoding.EncodeSignedInteger(32));
        report.InvocationCount.Should().Be(0);
        report.BuildListCount.Should().Be(0);
        var anotherStart = PineValue.List([encoding, IntegerEncoding.EncodeSignedInteger(5), IntegerEncoding.EncodeSignedInteger(32)]);
        compilation.SelectInstructionsForEnvironment(PineValueInProcess.Create(anotherStart)).TrackEnvConstraint.Should().Be(general);
        vm.EvaluateExpressionOnCustomStack(root, anotherStart, new(0, 100, 1))
            .Extract(error => throw new InvalidOperationException(error.ToString()))
            .ReturnValue.Evaluate().Should().Be(IntegerEncoding.EncodeSignedInteger(32));
        var differentTarget = PineValue.List([
            ExpressionEncoding.EncodeExpressionAsValue(Integer(71)), IntegerEncoding.EncodeSignedInteger(0), IntegerEncoding.EncodeSignedInteger(32)]);
        compilation.SelectInstructionsForEnvironment(PineValueInProcess.Create(differentTarget)).Should().Be(compilation.Generic);
        vm.EvaluateExpressionOnCustomStack(root, differentTarget, new(100, 100, 10))
            .Extract(error => throw new InvalidOperationException(error.ToString()))
            .ReturnValue.Evaluate().Should().Be(IntegerEncoding.EncodeSignedInteger(71));
    }

    [Fact]
    public void Cold_and_warm_reduction_caches_preserve_policy_specific_compilation()
    {
        Check();

        void Check()
        {
            var body = Counter();
            var expression = InvokeCounter(body, At(0), At(1));
            var cache = new Dictionary<(Expression, ReductionConfig), Expression>();
            var cold = ExpressionCompilation.CompileExpression(expression, [], new(), false, true, (_, _) => false, cache);
            var warm = ExpressionCompilation.CompileExpression(expression, [], new(), false, true, (_, _) => false, cache);
            warm.Generic.Should().Be(cold.Generic);
            var excluded = ExpressionCompilation.CompileExpression(expression, [], new(), false, true, (candidate, _) => candidate == body, cache);
            excluded.Generic.Should().NotBe(cold.Generic);
            var replay = ExpressionCompilation.CompileExpression(expression, [], new(), false, true, (_, _) => false, cache);
            replay.Generic.Should().Be(cold.Generic);
        }
    }

    [Fact]
    public void Malformed_unselected_targets_decline_without_becoming_compile_time_failures()
    {
        var call = InvokeCounter(Counter(), Integer(0), At(1));
        var malformed = new Expression.Eval(Literal(PineValue.EmptyList), Env);
        var root = new Expression.Conditional(At(0), malformed, call);
        var input = PineValue.List([PineKernelValues.TrueValue, IntegerEncoding.EncodeSignedInteger(16)]);
        var vm = Create();
        vm.EvaluateExpressionOnCustomStack(root, input, new(100, 100, 10))
            .Extract(error => throw new InvalidOperationException(error.ToString()))
            .ReturnValue.Evaluate().Should().Be(IntegerEncoding.EncodeSignedInteger(16));
        vm.EvaluateExpressionOnCustomStack(root,
            PineValue.List([PineKernelValues.FalseValue, IntegerEncoding.EncodeSignedInteger(16)]), new(100, 100, 10))
            .IsErrOrNull().Should().NotBeNull();
    }

    private static PineValue Values(int first, int second, int third = 0) =>
        PineValue.List([
            IntegerEncoding.EncodeSignedInteger(first), IntegerEncoding.EncodeSignedInteger(second), IntegerEncoding.EncodeSignedInteger(third)]);

    private static VM Create(
        bool disableReduction = false, bool tail = true, Expression? excluded = null,
        bool nondefaultThreshold = false, bool disableConsolidation = false,
        IReadOnlyDictionary<Expression, IReadOnlyList<PineValueClass>>? classes = null) =>
        VM.CreateCustom(
            evalCache: null, evaluationConfigDefault: null, reportFunctionApplication: null,
            compilationEnvClasses: classes, disableReductionInCompilation: disableReduction, selectPrecompiled: null,
            skipInlineForExpression: expression => expression == excluded, enableTailRecursionOptimization: tail, parseCache: null,
            precompiledLeaves: null, reportEnterPrecompiledLeaf: null, reportExitPrecompiledLeaf: null,
            optimizationParametersSerial: null, cacheFileStore: null,
            pathMaxLowExclusive: nondefaultThreshold ? 0 : ExpressionCompilation.DefaultPathMaxLowExclusive,
            disableGenericApplicationChainConsolidation: disableConsolidation);
}
