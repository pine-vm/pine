using AwesomeAssertions;
using Pine.Core.CommonEncodings;
using Pine.Core.Interpreter;
using Pine.Core.Interpreter.IntermediateVM;
using Pine.Core.Interpreter.IntermediateVM.Backend;
using Pine.Core.Interpreter.IntermediateVM.Frontend;
using Pine.Core.Interpreter.IntermediateVM.Semantic;
using Pine.Core.PineVM;
using System;
using System.Collections.Immutable;
using System.Linq;
using System.Reflection;
using Xunit;

namespace Pine.Core.Tests.Interpreter.IntermediateVM;

public class GraphScalarOptimizationTests
{
    private static Expression Env => Expression.EnvironmentInstance;
    private static Expression Lit(long value) => Value(IntegerEncoding.EncodeSignedInteger(value));
    private static Expression Value(PineValue value) => new Expression.Litral(value);
    private static Expression List(params ImmutableArray<Expression> items) => new Expression.List(items.ToArray());
    private static Expression Builtin(string name, params ImmutableArray<Expression> items) => new Expression.Builtin(name, List(items));
    private static Expression At(Expression source, int index) =>
        new Expression.Builtin("head", Builtin("skip", Lit(index), source));
    private static Expression Call(Expression body, Expression environment) =>
        new Expression.Eval(Value(ExpressionEncoding.EncodeExpressionAsValue(body)), environment);
    private static GraphOptimizationResult Compile(Expression expression, GraphOptimizerOptions? options = null) =>
        ExpressionGraphOptimizer.Compile(CompilationRequest.Capture(expression, new(DisableReduction: true)),
            options ?? new(), CompilerMemo.Empty).Extract(errors => throw new Exception(string.Join(", ", errors)));

    [Theory]
    [InlineData("skip", false)]
    [InlineData("skip", true)]
    [InlineData("take", false)]
    [InlineData("take", true)]
    public void Scalar_slice_selection_preserves_big_integers_invalid_counts_and_lazy_sources(string name, bool nested)
    {
        var source = nested ? Builtin("skip", Lit(1), At(Env, 1)) : At(Env, 1);
        var expression = Builtin(name, At(Env, 0), source);
        var vm = ExpressionGraphVM.Create(optimizerOptions: new());
        foreach (var count in new[]
        {
            IntegerEncoding.EncodeSignedInteger((long)int.MinValue - 1),
            IntegerEncoding.EncodeSignedInteger(-3), IntegerEncoding.EncodeSignedInteger(0),
            IntegerEncoding.EncodeSignedInteger(1), IntegerEncoding.EncodeSignedInteger(3),
            IntegerEncoding.EncodeSignedInteger(int.MaxValue),
            IntegerEncoding.EncodeSignedInteger((long)int.MaxValue + 1),
            PineValue.Blob([4, 0, 1]), PineValue.Blob([2, 0, 1]),
            PineValue.EmptyList, PineValue.Blob([255, 1]),
        })
            foreach (var inputSource in new[]
            {
                PineValue.EmptyList, PineValue.EmptyBlob, PineValue.Blob([17, 29, 31]),
                PineValue.List([IntegerEncoding.EncodeSignedInteger(7), PineValue.Blob([255]), PineValue.EmptyList]),
            })
            {
                var input = PineValue.List([count, inputSource]);
                PineValue? expected = null;
                var failure = Record.Exception(() => expected = new DirectInterpreter(new(), null).EvaluateExpressionDefault(expression, input));
                if (failure is not null)
                {
                    var actual = Record.Exception(() => vm.EvaluateExpressionOnCustomStack(expression, input, new(1000, 10_000, 100)));
                    actual.Should().BeOfType<InvalidIntermediateCodeException>().Which.InnerException!.GetType().Should().Be(failure.GetType());
                    continue;
                }
                var report = vm.EvaluateExpressionOnCustomStack(expression, input, new(1000, 10_000, 100))
                    .Extract(error => throw new Exception(error.ToString()));
                report.ReturnValue.Evaluate().Should().Be(expected);
                report.BuildListCount.Should().Be(0);
                report.InvocationCount.Should().Be(0);
            }
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void Fused_slice_equality_preserves_nonempty_literal_type_and_extreme_count_semantics(bool listLiteral)
    {
        var literal = listLiteral ? PineValue.List([IntegerEncoding.EncodeSignedInteger(7)]) : PineValue.Blob([29]);
        var expression = Builtin("equal", Value(literal),
            Builtin("take", Lit(1), Builtin("skip", At(Env, 0), Builtin("skip", Lit(1), At(Env, 1)))));
        var artifact = GraphCompiler.Compile(Compile(expression).Graph, fuseScalarBuiltins: true)
            .Extract(error => throw new Exception(error.ToString()));
        artifact.Blocks.SelectMany(block => block.Instructions).OfType<SelectedInstruction.Builtin>()
            .Where(instruction => instruction.Kind == StackInstructionKind.Slice_Skip_Var_Equal_Const).Should().ContainSingle();
        var vm = ExpressionGraphVM.Create(optimizerOptions: new());
        foreach (var count in new[]
        {
            IntegerEncoding.EncodeSignedInteger((long)int.MinValue - 1), IntegerEncoding.EncodeSignedInteger(0),
            IntegerEncoding.EncodeSignedInteger(1), IntegerEncoding.EncodeSignedInteger(int.MaxValue),
            IntegerEncoding.EncodeSignedInteger((long)int.MaxValue + 1), PineValue.EmptyList, PineValue.Blob([4, 0, 1]),
        })
            foreach (var source in new[]
            {
                PineValue.EmptyList, PineValue.EmptyBlob, PineValue.Blob([17, 29, 31]),
                PineValue.List([PineValue.EmptyList, IntegerEncoding.EncodeSignedInteger(7), PineValue.EmptyList]),
            })
            {
                var input = PineValue.List([count, source]);
                PineValue? expected = null;
                var failure = Record.Exception(() => expected = new DirectInterpreter(new(), null).EvaluateExpressionDefault(expression, input));
                if (failure is not null)
                {
                    Record.Exception(() => vm.EvaluateExpressionOnCustomStack(expression, input, new(100, 1000, 100)))
                        .Should().BeOfType<InvalidIntermediateCodeException>().Which.InnerException!.GetType().Should().Be(failure.GetType());
                    continue;
                }
                var report = vm.EvaluateExpressionOnCustomStack(expression, input, new(100, 1000, 100))
                    .Extract(error => throw new Exception(error.ToString()));
                report.ReturnValue.Evaluate().Should().Be(expected);
                report.BuildListCount.Should().Be(0);
            }
    }

    [Theory]
    [InlineData("equal")]
    [InlineData("int_add")]
    [InlineData("int_mul")]
    public void Scalar_binary_selection_preserves_noncanonical_and_malformed_values(string name)
    {
        var expression = Builtin(name, At(Env, 0), At(Env, 1));
        var vm = ExpressionGraphVM.Create(optimizerOptions: new());
        var values = new[]
        {
            PineValue.EmptyList, PineValue.EmptyBlob, IntegerEncoding.EncodeSignedInteger(0),
            IntegerEncoding.EncodeSignedInteger(-17), IntegerEncoding.EncodeSignedInteger((long)int.MaxValue + 1),
            PineValue.Blob([4, 0, 1]), PineValue.Blob([255, 1]),
        };
        foreach (var left in values)
            foreach (var right in values)
            {
                var input = PineValue.List([left, right]);
                var expected = new DirectInterpreter(new(), null).EvaluateExpressionDefault(expression, input);
                var report = vm.EvaluateExpressionOnCustomStack(expression, input, new(1000, 10_000, 100))
                    .Extract(error => throw new Exception(error.ToString()));
                report.ReturnValue.Evaluate().Should().Be(expected);
                report.BuildListCount.Should().Be(0);
            }
    }

    [Fact]
    public void Guarded_root_recursion_uses_exact_identity_and_keeps_malformed_and_other_targets_dynamic()
    {
        var body = new Expression.Conditional(Builtin("equal", At(Env, 1), Lit(0)),
            new Expression.Eval(At(Env, 0), List(At(Env, 0), Builtin("int_add", At(Env, 1), Lit(-1)))),
            At(Env, 1));
        var vm = ExpressionGraphVM.Create(optimizerOptions: new(InlineLiteralCalls: false));
        var encoded = ExpressionEncoding.EncodeExpressionAsValue(body);
        var report = vm.EvaluateExpressionOnCustomStack(body, PineValue.List([encoded, IntegerEncoding.EncodeSignedInteger(30)]), new(0, 1000, 1))
            .Extract(error => throw new Exception(error.ToString()));
        report.ReturnValue.Evaluate().Should().Be(IntegerEncoding.EncodeSignedInteger(0));
        report.InvocationCount.Should().Be(0);
        report.LoopIterationCount.Should().BeGreaterThan(0);
        var other = ExpressionEncoding.EncodeExpressionAsValue(Lit(99));
        var fallback = vm.EvaluateExpressionOnCustomStack(body, PineValue.List([other, IntegerEncoding.EncodeSignedInteger(3)]), new(1000, 10_000, 100))
            .Extract(error => throw new Exception(error.ToString()));
        fallback.ReturnValue.Evaluate().Should().Be(IntegerEncoding.EncodeSignedInteger(99));
        fallback.InvocationCount.Should().Be(1);
        var malformed = PineValue.Blob([255, 71]);
        vm.EvaluateExpressionOnCustomStack(body, PineValue.List([malformed, IntegerEncoding.EncodeSignedInteger(3)]), new(1000, 10_000, 100))
            .IsErrOrNull()!.Reason.Should().BeOfType<EvaluationErrorReason.ParseExpressionFailed>()
            .Which.ExpressionValue.Should().Be(malformed);
    }

    [Fact]
    public void Scalar_loop_parameters_are_simultaneous_and_two_inlined_loops_preserve_captures()
    {
        var body = new Expression.Conditional(Builtin("equal", At(Env, 1), Lit(0)),
            new Expression.Eval(At(Env, 0), List(At(Env, 0), Builtin("int_add", At(Env, 1), Lit(-1)), At(Env, 3), At(Env, 2))),
            Builtin("int_add", Builtin("int_mul", Lit(100), At(Env, 2)), At(Env, 3)));
        var encoded = Value(ExpressionEncoding.EncodeExpressionAsValue(body));
        var first = Call(body, List(encoded, At(Env, 0), Lit(3), Lit(7)));
        var second = Call(body, List(encoded, At(Env, 1), Lit(11), Lit(13)));
        var expression = Builtin("int_add", first, second);
        var optimized = Compile(expression);
        optimized.Graph.Graph.Blocks.Values.Where(block => block.Terminator is Terminator.Invoke or Terminator.TailInvoke).Should().BeEmpty();
        var vm = ExpressionGraphVM.Create(optimizerOptions: new());
        foreach (var left in new[] { 0, 1, 2, 9 })
            foreach (var right in new[] { 0, 1, 2, 10 })
            {
                var input = PineValue.List([IntegerEncoding.EncodeSignedInteger(left), IntegerEncoding.EncodeSignedInteger(right)]);
                var expected = new DirectInterpreter(new(), null).EvaluateExpressionDefault(expression, input);
                var report = vm.EvaluateExpressionOnCustomStack(expression, input, new(0, 10_000, 1))
                    .Extract(error => throw new Exception(error.ToString()));
                report.ReturnValue.Evaluate().Should().Be(expected);
                report.InvocationCount.Should().Be(0);
                report.BuildListCount.Should().Be(0);
            }
    }

    [Fact]
    public void Nested_inlined_loops_keep_outer_iteration_values_across_inner_continuations()
    {
        var inner = new Expression.Conditional(Builtin("equal", At(Env, 1), Lit(0)),
            new Expression.Eval(At(Env, 0), List(At(Env, 0), Builtin("int_add", At(Env, 1), Lit(-1)), At(Env, 3), At(Env, 2))),
            Builtin("int_add", Builtin("int_mul", Lit(100), At(Env, 2)), At(Env, 3)));
        var innerEncoding = Value(ExpressionEncoding.EncodeExpressionAsValue(inner));
        var innerCall = Call(inner, List(innerEncoding, Lit(3), Lit(3), Lit(7)));
        var outer = new Expression.Conditional(Builtin("equal", At(Env, 1), Lit(0)),
            new Expression.Eval(At(Env, 0), List(At(Env, 0), Builtin("int_add", At(Env, 1), Lit(-1)),
                Builtin("int_add", At(Env, 2), innerCall))),
            At(Env, 2));
        var expression = Call(outer, List(Value(ExpressionEncoding.EncodeExpressionAsValue(outer)), Env, Lit(0)));
        var optimized = Compile(expression);
        optimized.Graph.Graph.Blocks.Values.Where(block => block.Terminator is Terminator.Invoke or Terminator.TailInvoke).Should().BeEmpty();
        var vm = ExpressionGraphVM.Create(optimizerOptions: new());
        foreach (var count in new[] { 0, 1, 2, 10 })
        {
            var input = IntegerEncoding.EncodeSignedInteger(count);
            var expected = new DirectInterpreter(new(), null).EvaluateExpressionDefault(expression, input);
            var report = vm.EvaluateExpressionOnCustomStack(expression, input, new(0, 10_000, 1))
                .Extract(error => throw new Exception(error.ToString()));
            report.ReturnValue.Evaluate().Should().Be(expected);
            report.InvocationCount.Should().Be(0);
            report.BuildListCount.Should().Be(0);
        }
    }

    [Fact]
    public void Scalar_replacement_keeps_observable_lists_and_failing_discarded_dependencies()
    {
        var failing = Builtin("bit_shift_left", Lit(-8), Value(PineValue.Blob([1])));
        var expression = At(List(failing, Lit(19)), 1);
        var graph = Compile(expression).Graph.Graph;
        graph.Blocks.Values.SelectMany(block => block.Operations).OfType<Operation.Builtin>().Should().ContainSingle();
        Record.Exception(() => ExpressionGraphVM.Create(optimizerOptions: new()).EvaluateExpressionOnCustomStack(expression, PineValue.EmptyList, new(1000, 10_000, 100)))
            .Should().BeOfType<InvalidIntermediateCodeException>();

        var observable = Call(List(At(Env, 0), At(Env, 1)), List(At(Env, 1), At(Env, 0)));
        var input = PineValue.List([PineValue.Blob([9]), PineValue.Blob([7])]);
        var report = ExpressionGraphVM.Create(optimizerOptions: new()).EvaluateExpressionOnCustomStack(observable, input, new(1000, 10_000, 100))
            .Extract(error => throw new Exception(error.ToString()));
        report.ReturnValue.Evaluate().Should().Be(PineValue.List([PineValue.Blob([7]), PineValue.Blob([9])]));
        report.BuildListCount.Should().Be(1);
        report.InvocationCount.Should().Be(0);
    }

    [Fact]
    public void Scalar_budget_refuses_cleanly_and_does_not_modify_input_evidence()
    {
        var baseline = Compile(Call(At(Env, 0), List(Env)), new(Enabled: false)).Graph;
        GraphScalarReplacement.Rewrite(baseline, maxRounds: 0).Should().BeSameAs(baseline);
        GraphScalarReplacement.Rewrite(baseline, maxWorkUnits: 1).Should().BeSameAs(baseline);
        GraphScalarReplacement.Rewrite(baseline, maxExpansionUnits: 0).Should().BeSameAs(baseline);
        foreach (var options in new[]
        {
            new GraphOptimizerOptions(Enabled: false),
            new(InlineLiteralCalls: false, SelfTailLoops: false, MaxScalarWorkUnits: 0),
            new(InlineLiteralCalls: false, SelfTailLoops: false, MaxScalarExpansionUnits: 0),
        })
            ExpressionGraphOptimizer.Optimize(baseline, options, CompilerMemo.Empty).Graph.Should().BeSameAs(baseline);
        var expression = new Expression.Eval(Env, Env);
        Compile(expression, new(MaxCandidates: 0)).Stats.SelfTailCalls.Should().Be(0);
        foreach (var options in new[]
        {
            new GraphOptimizerOptions(Enabled: false),
            new(InlineLiteralCalls: false, MaxGuardWorkUnits: 0),
            new(InlineLiteralCalls: false, MaxGuardExpansionUnits: 0),
        })
        {
            var unchanged = Compile(expression, options);
            unchanged.Stats.SelfTailCalls.Should().Be(0);
            unchanged.Graph.Graph.Should().Be(ExpressionGraphCompiler.Compile(expression));
        }
        var other = Compile(Call(Lit(7), Env), new(InlineLiteralCalls: false));
        other.Stats.SelfTailCalls.Should().Be(0);
        other.Graph.Graph.Blocks.Values.Should().ContainSingle();
    }

    [Fact]
    public void Scalar_identity_allocation_handles_extreme_and_negative_ids_without_partial_rewrites()
    {
        var body = new Expression.Eval(Env, Env);
        foreach (var ids in new[] { (Block: int.MaxValue, Value: 0), (Block: 0, Value: int.MaxValue), (Block: -7, Value: -11) })
        {
            var block = new BasicBlock(new(ids.Block), [new(new(ids.Value))], [],
                new Terminator.TailInvoke(new(new(-2), new CallTarget.Dynamic(new(ids.Value)), FunctionSignature.Canonical, [new(ids.Value)])));
            var input = ValidatedFunctionGraph.ValidateGraph(
                new(new(-7), FunctionSignature.Canonical, block.Id, ImmutableDictionary<PineBlockId, BasicBlock>.Empty.Add(block.Id, block)),
                ImmutableDictionary<FunctionId, FunctionSignature>.Empty).Extract(errors => throw new Exception(string.Join(", ", errors)));
            GraphScalarReplacement.Rewrite(input).Should().BeSameAs(input);
        }
        foreach (var id in new[] { int.MinValue, -7, int.MaxValue })
        {
            var prepared = FunctionPreparation.PrepareFunction(
                CompilationRequest.Capture(body, new(DisableReduction: true), new(id)), CompilerMemo.Empty).Function;
            var guarded = ExpressionSelfTailLoops.Compile(prepared,
                OwnedExpression.CaptureValue(ExpressionEncoding.EncodeExpressionAsValue(body)));
            guarded.RewrittenCalls.Should().Be(1);
            guarded.Graph.Graph.Id.Should().Be(new FunctionId(id));
            var mismatch = ExpressionSelfTailLoops.Compile(prepared,
                OwnedExpression.CaptureValue(ExpressionEncoding.EncodeExpressionAsValue(Lit(8))));
            mismatch.RewrittenCalls.Should().Be(0);
            mismatch.Graph.Graph.Should().Be(ExpressionGraphCompiler.CompileExpressionToGraph(prepared));
        }
        var returning = FunctionPreparation.PrepareFunction(
            CompilationRequest.Capture(Env, new(DisableReduction: true)), CompilerMemo.Empty).Function;
        var graph = ExpressionGraphCompiler.CompileExpressionToGraph(returning, CompilerMemo.Empty).Graph
            .Extract(errors => throw new Exception(string.Join(", ", errors)));
        ExpressionSelfTailLoops.Compile(returning, OwnedExpression.CaptureValue(ExpressionEncoding.EncodeExpressionAsValue(Env)))
            .Graph.Graph.Should().Be(graph.Graph);
    }

    [Fact]
    public void Private_guard_allocator_uses_last_available_ids_and_declines_only_insufficient_capacity()
    {
        var body = new Expression.Eval(Env, Env);
        var prepared = FunctionPreparation.PrepareFunction(
            CompilationRequest.Capture(body, new(DisableReduction: true), new(7)), CompilerMemo.Empty).Function;
        var encoding = OwnedExpression.CaptureValue(ExpressionEncoding.EncodeExpressionAsValue(body));
        var rewrite = typeof(ExpressionSelfTailLoops).GetMethod("Rewrite", BindingFlags.Static | BindingFlags.NonPublic)!;
        foreach (var ids in ImmutableArray.Create(
            (Block: int.MaxValue - 1, Value: int.MaxValue - 2, Fits: true),
            (Block: int.MaxValue, Value: int.MaxValue - 2, Fits: false),
            (Block: int.MaxValue - 1, Value: int.MaxValue - 1, Fits: false),
            (Block: int.MinValue, Value: int.MinValue, Fits: true)))
        {
            // This is the raw Env/Env invocation with renamed IDs; reflection keeps the
            // production API unable to accept arbitrary graph/source pairs.
            var block = new BasicBlock(new(ids.Block), [new(new(ids.Value))], [],
                new Terminator.TailInvoke(new(new(0), new CallTarget.Dynamic(new(ids.Value)), FunctionSignature.Canonical, [new(ids.Value)])));
            var input = ValidatedFunctionGraph.ValidateGraph(
                new(new(7), FunctionSignature.Canonical, block.Id, ImmutableDictionary<PineBlockId, BasicBlock>.Empty.Add(block.Id, block)),
                ImmutableDictionary<FunctionId, FunctionSignature>.Empty).Extract(errors => throw new Exception(string.Join(", ", errors)));
            var rewritten = ((ValidatedFunctionGraph Graph, int RewrittenCalls))rewrite.Invoke(null,
                [input, prepared, encoding, 65_536L, 20_000L, 64])!;
            rewritten.RewrittenCalls.Should().Be(ids.Fits ? 1 : 0);
            if (ids.Fits)
                rewritten.Graph.Graph.Blocks.Should().HaveCount(2);
            else
                rewritten.Graph.Should().BeSameAs(input);
        }
    }

    [Fact]
    public void Specialized_root_never_receives_unspecialized_identity_guards_even_when_body_is_unchanged()
    {
        var expression = new Expression.Eval(Env, Env);
        var request = CompilationRequest.Capture(expression, new(DisableReduction: true)) with
        {
            Specialization = new([new(new([5]), OwnedExpression.CaptureValue(IntegerEncoding.EncodeSignedInteger(7)))]),
        };
        var prepared = FunctionPreparation.PrepareFunction(request, CompilerMemo.Empty);
        prepared.Function.Body.Should().Be(prepared.Function.Source);
        ExpressionSelfTailLoops.Compile(prepared.Function,
            OwnedExpression.CaptureValue(ExpressionEncoding.EncodeExpressionAsValue(expression))).RewrittenCalls.Should().Be(0);
        var compiled = ExpressionGraphOptimizer.Compile(request,
            new(InlineLiteralCalls: false, ScalarReplacement: false), CompilerMemo.Empty)
            .Extract(errors => throw new Exception(string.Join(", ", errors)));
        compiled.Stats.SelfTailCalls.Should().Be(0);
        compiled.Graph.Graph.Blocks.Values.Should().ContainSingle().Which.Terminator.Should().BeOfType<Terminator.TailInvoke>();
    }

    [Fact]
    public void Guarded_tail_operand_failures_still_evaluate_environment_before_target()
    {
        var environmentFailure = PineValue.Blob([255, 31]);
        var targetFailure = PineValue.Blob([255, 41]);
        var expression = new Expression.Eval(new Expression.Eval(Value(targetFailure), Env), new Expression.Eval(Value(environmentFailure), Env));
        ExpressionGraphVM.Create(optimizerOptions: new()).EvaluateExpressionOnCustomStack(expression, PineValue.EmptyList, new(1000, 10_000, 100))
            .IsErrOrNull()!.Reason.Should().BeOfType<EvaluationErrorReason.ParseExpressionFailed>()
            .Which.ExpressionValue.Should().Be(environmentFailure);
    }
}
