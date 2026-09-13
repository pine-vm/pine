using AwesomeAssertions;
using Pine.Core.CommonEncodings;
using Pine.Core.Interpreter;
using Pine.Core.Interpreter.IntermediateVM;
using Pine.Core.Interpreter.IntermediateVM.Frontend;
using Pine.Core.Interpreter.IntermediateVM.Semantic;
using Pine.Core.PineVM;
using System;
using System.Collections.Immutable;
using System.Linq;
using System.Runtime.InteropServices;
using Xunit;

namespace Pine.Core.Tests.Interpreter.IntermediateVM;

public class ExpressionGraphOptimizerTests
{
    private static Expression Env => Expression.EnvironmentInstance;
    private static Expression Lit(int value) => new Expression.Litral(IntegerEncoding.EncodeSignedInteger(value));
    private static Expression Value(PineValue value) => new Expression.Litral(value);
    private static Expression List(params ImmutableArray<Expression> items) => new Expression.List(items.ToArray());
    private static Expression Builtin(string name, Expression input) => new Expression.Builtin(name, input);
    private static Expression At(Expression source, int index) => Builtin("head", Builtin("skip", List(Lit(index), source)));
    private static Expression Call(Expression body, Expression environment) =>
        new Expression.Eval(Value(ExpressionEncoding.EncodeExpressionAsValue(body)), environment);
    private static Expression Conditional(Expression condition, Expression whenFalse, Expression whenTrue) =>
        new Expression.Conditional(condition, whenFalse, whenTrue);
    private static CompilationRequest Request(Expression expression) =>
        CompilationRequest.Capture(expression, new(DisableReduction: true));
    // Retain characterization of the original bounded literal/value-analysis policy.
    private static GraphOptimizerOptions LiteralPolicy(GraphOptimizerOptions options) =>
        options with { ScalarReplacement = false, GuardDynamicSelfTailCalls = false };
    private static GraphOptimizationResult Optimize(ValidatedFunctionGraph graph, GraphOptimizerOptions options, CompilerMemo memo) =>
        ExpressionGraphOptimizer.Optimize(graph, LiteralPolicy(options), memo);
    private static GraphOptimizationResult Compile(Expression expression, GraphOptimizerOptions? options = null, CompilerMemo? memo = null) =>
        ExpressionGraphOptimizer.Compile(Request(expression), LiteralPolicy(options ?? new()), memo ?? CompilerMemo.Empty)
            .Extract(errors => throw new Exception(string.Join(", ", errors)));
    private static ImmutableList<Call> Calls(FunctionGraph graph) => graph.Blocks.Values
        .OrderBy(block => block.Id.Value).SelectMany(block => block.Terminator switch
        {
            Terminator.Return or Terminator.Jump or Terminator.Branch or Terminator.Switch => ImmutableList<Call>.Empty,
            Terminator.Invoke invoke => [invoke.Call],
            Terminator.TailInvoke tail => [tail.Call],
            _ => throw new NotImplementedException("Calls does not handle terminator variant: " + block.Terminator.GetType().Name),
        }).ToImmutableList();
    private static EvaluationReport Run(Expression expression, PineValue input, GraphOptimizerOptions? options) =>
        ExpressionGraphVM.Create(optimizerOptions: options is null ? null : LiteralPolicy(options))
            .EvaluateExpressionOnCustomStack(expression, input, new(100, 10_000, 100))
            .Extract(error => throw new Exception(error.ToString()));

    [Theory]
    [InlineData(0)]
    [InlineData(1)]
    [InlineData(2)]
    public void GraphValue_function_table_crosses_wrapper_edges_captures_and_nested_inlining(int variant)
    {
        var leaf = List(Env, Lit(91));
        var table = Value(PineValue.List([ExpressionEncoding.EncodeExpressionAsValue(leaf)]));
        var wrapper = new Expression.Eval(At(At(Env, 0), 0), At(Env, 1));
        var environment = variant switch
        {
            0 => List(table, Env),
            1 => Conditional(Env, List(table, Env), List(table, Env)),
            2 => List(table, Call(Env, Env)),
            _ => throw new NotImplementedException("GraphValue_function_table_crosses_wrapper_edges_captures_and_nested_inlining: " + variant),
        };
        var expression = List(Env, Call(Call(wrapper, Env), environment), Env);
        var cold = Compile(expression);
        var warm = Compile(expression, memo: cold.Memo);
        cold.Stats.InlinedCalls.Should().Be(variant == 2 ? 4 : 3);
        Calls(cold.Graph.Graph).Should().BeEmpty();
        cold.Stats.AnalysisPasses.Should().BeGreaterThan(0);
        warm.Stats.Should().Be(cold.Stats);
        warm.Graph.Graph.Should().Be(cold.Graph.Graph);
        foreach (var input in ImmutableList.Create(PineKernelValues.TrueValue, PineKernelValues.FalseValue, PineValue.Blob([7])))
        {
            var before = Run(expression, input, null);
            var after = Run(expression, input, new());
            after.ReturnValue.Evaluate().Should().Be(new DirectInterpreter(new(), null).EvaluateExpressionDefault(expression, input));
            after.InvocationCount.Should().Be(before.InvocationCount - cold.Stats.InlinedCalls);
            after.BuildListCount.Should().Be(before.BuildListCount);
        }
    }

    [Fact]
    public void GraphValue_reanalysis_after_inlining_exposes_a_returned_function_target()
    {
        var encoded = Value(ExpressionEncoding.EncodeExpressionAsValue(List(Env, Lit(9))));
        var expression = new Expression.Eval(Call(encoded, Env), Env);
        var result = Compile(expression);
        result.Stats.InlinedCalls.Should().Be(2);
        Calls(result.Graph.Graph).Should().BeEmpty();
        Run(expression, PineValue.EmptyList, new()).ReturnValue.Evaluate()
            .Should().Be(Run(expression, PineValue.EmptyList, null).ReturnValue.Evaluate());
    }

    [Fact]
    public void GraphValue_revisits_an_earlier_unknown_site_after_its_defining_call_is_inlined()
    {
        var encoded = Value(ExpressionEncoding.EncodeExpressionAsValue(List(Env, Lit(9))));
        var entry = new BasicBlock(new(10), [new(new(0))],
            [new Operation.Literal(new(new(1)), OwnedExpression.CaptureValue(ExpressionEncoding.EncodeExpressionAsValue(encoded)))],
            new Terminator.Invoke(new(new(0), new CallTarget.Dynamic(new(1)), FunctionSignature.Canonical, [new(0)]),
                new(new(0), [new ContinuationBinding.ReturnedResult(0), new ContinuationBinding.CallerValue(new(0))])));
        var continuation = new BasicBlock(new(0), [new(new(10)), new(new(11))], [],
            new Terminator.TailInvoke(new(new(1), new CallTarget.Dynamic(new(10)), FunctionSignature.Canonical, [new(11)])));
        var graph = ValidatedFunctionGraph.ValidateGraph(
            new(new(0), FunctionSignature.Canonical, entry.Id,
                ImmutableDictionary<PineBlockId, BasicBlock>.Empty.Add(entry.Id, entry).Add(continuation.Id, continuation)),
            ImmutableDictionary<FunctionId, FunctionSignature>.Empty).Extract(errors => throw new Exception(string.Join(", ", errors)));
        var result = Optimize(graph, new(), CompilerMemo.Empty);
        result.Stats.InlinedCalls.Should().Be(2);
        result.Stats.Candidates.Should().Be(3);
        result.Stats.AnalysisPasses.Should().Be(2);
        result.Stats.Declines.Single().Code.Should().Be(GraphOptimizationDeclineCode.UnknownTarget);
        Calls(result.Graph.Graph).Should().BeEmpty();
        var firstPass = Optimize(graph, new(MaxCandidates: 1), CompilerMemo.Empty);
        var budget = firstPass.Stats.AnalysisWorkUnits + 1;
        var bounded = Optimize(graph, new(MaxAnalysisWorkUnits: budget), CompilerMemo.Empty);
        bounded.Stats.AnalysisPasses.Should().Be(2);
        bounded.Stats.AnalysisWorkUnits.Should().Be(budget);
        bounded.Stats.InlinedCalls.Should().Be(1);
        bounded.Stats.BudgetLimitReached.Should().BeTrue();
        Calls(bounded.Graph.Graph).Single().Target.Should().BeOfType<CallTarget.Dynamic>();
    }

    [Fact]
    public void GraphValue_analysis_budget_is_cumulative_and_fallback_preserves_dynamic_calls()
    {
        var leaf = List(Env, Lit(91));
        var wrapper = new Expression.Eval(At(At(Env, 0), 0), At(Env, 1));
        var table = Value(PineValue.List([ExpressionEncoding.EncodeExpressionAsValue(leaf)]));
        var expression = Call(wrapper, List(table, Env));
        var tiny = new GraphOptimizerOptions(MaxAnalysisWorkUnits: 1);
        var result = Compile(expression, tiny);
        result.Stats.InlinedCalls.Should().Be(1);
        result.Stats.AnalysisWorkUnits.Should().Be(1);
        result.Stats.BudgetLimitReached.Should().BeTrue();
        Calls(result.Graph.Graph).Should().ContainSingle();
        result.Graph.KnownFunctionSignatures.Should().Equal(
            Compile(expression, new(MaxCandidates: 1)).Graph.KnownFunctionSignatures);
        Compile(expression, tiny, result.Memo).Stats.Should().Be(result.Stats);
        var input = PineValue.Blob([7]);
        Run(expression, input, tiny).ReturnValue.Evaluate()
            .Should().Be(Run(expression, input, null).ReturnValue.Evaluate());
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void GraphValue_known_target_projection_does_not_remove_a_failing_unknown_list_item(bool selected)
    {
        var failing = Builtin("bit_shift_left", List(Lit(-8), Value(PineValue.Blob([1]))));
        var target = At(List(failing, Value(ExpressionEncoding.EncodeExpressionAsValue(Lit(99)))), 1);
        var expression = Conditional(Env, Lit(8), new Expression.Eval(target, Env));
        Compile(expression).Stats.InlinedCalls.Should().Be(1);
        foreach (var options in ImmutableList.Create(new GraphOptimizerOptions(Enabled: false), new GraphOptimizerOptions()))
        {
            if (selected)
            {
                Action run = () => Run(expression, PineKernelValues.TrueValue, options);
                run.Should().Throw<InvalidIntermediateCodeException>();
            }
            else
                Run(expression, PineKernelValues.FalseValue, options).ReturnValue.Evaluate()
                    .Should().Be(IntegerEncoding.EncodeSignedInteger(8));
        }
    }

    [Fact]
    public void GraphValue_malformed_projected_encoding_remains_dynamic_without_a_temporary_signature()
    {
        var target = At(List(Env, Value(PineValue.Blob([255]))), 1);
        var result = Compile(new Expression.Eval(target, Env));
        result.Stats.InlinedCalls.Should().Be(0);
        result.Stats.Declines.Single().Code.Should().Be(GraphOptimizationDeclineCode.InvalidEncoding);
        result.Graph.KnownFunctionSignatures.Should().BeEmpty();
        Calls(result.Graph.Graph).Single().Target.Should().BeOfType<CallTarget.Dynamic>();
    }

    [Theory]
    [InlineData(0)]
    [InlineData(1)]
    [InlineData(2)]
    [InlineData(3)]
    [InlineData(4)]
    public void Literal_eval_expands_real_frontend_graph_with_two_nested_levels_and_preserves_lists(int variant)
    {
        var leaf = variant switch
        {
            0 => Env,
            1 => List(Env, Lit(7)),
            2 => Builtin("int_add", List(Env, Lit(7))),
            3 => Conditional(Env, Lit(8), Lit(9)),
            4 => new Expression.Label("owned-label", Env),
            _ => throw new NotImplementedException(
                "Literal_eval_expands_real_frontend_graph_with_two_nested_levels_and_preserves_lists does not handle corpus variant: " + variant),
        };
        var expression = List(Lit(91), Call(Call(leaf, Env), Builtin("reverse", List(Lit(2), Lit(3)))), Env);
        var input = IntegerEncoding.EncodeSignedInteger(17);
        var optimized = Compile(expression);
        optimized.Stats.InlinedCalls.Should().Be(2);
        Calls(optimized.Graph.Graph).Should().BeEmpty();
        var original = Run(expression, input, null);
        var actual = Run(expression, input, new());
        actual.ReturnValue.Evaluate().Should().Be(new DirectInterpreter(new(), null).EvaluateExpressionDefault(expression, input));
        actual.ReturnValue.Evaluate().Should().Be(original.ReturnValue.Evaluate());
        actual.InvocationCount.Should().Be(original.InvocationCount - 2);
        actual.BuildListCount.Should().Be(original.BuildListCount);
    }

    [Fact]
    public void Non_tail_arithmetic_call_after_conditional_environment_preserves_scope_and_returned_noncanonical_boolean()
    {
        var noncanonical = PineValue.Blob([4, 0]);
        var expression = List(
            Builtin("int_add", List(Lit(12), Call(Env, Conditional(Env, Lit(3), Lit(4))))),
            Conditional(Call(Value(noncanonical), List(Env, Lit(99))), Lit(8), Lit(9)),
            Env);
        foreach (var input in ImmutableList.Create(PineKernelValues.TrueValue, PineKernelValues.FalseValue, noncanonical))
        {
            var before = Run(expression, input, null);
            var after = Run(expression, input, new());
            after.ReturnValue.Evaluate().Should().Be(new DirectInterpreter(new(), null).EvaluateExpressionDefault(expression, input));
            after.ReturnValue.Evaluate().Should().Be(before.ReturnValue.Evaluate());
            after.InvocationCount.Should().Be(before.InvocationCount - 2);
            after.BuildListCount.Should().Be(before.BuildListCount);
        }
        Compile(expression).Stats.InlinedCalls.Should().Be(2);
    }

    [Fact]
    public void Unknown_environment_target_stays_dynamic_even_with_equal_literal_elsewhere()
    {
        var expression = List(Value(ExpressionEncoding.EncodeExpressionAsValue(Env)), new Expression.Eval(Env, Lit(7)));
        var result = Compile(expression);
        result.Stats.InlinedCalls.Should().Be(0);
        result.Stats.Declines.Single().Code.Should().Be(GraphOptimizationDeclineCode.UnknownTarget);
        Calls(result.Graph.Graph).Single().Target.Should().BeOfType<CallTarget.Dynamic>();
        var input = ExpressionEncoding.EncodeExpressionAsValue(Env);
        Run(expression, input, new()).ReturnValue.Evaluate().Should().Be(Run(expression, input, null).ReturnValue.Evaluate());
    }

    [Fact]
    public void Conditional_target_aliases_are_not_speculated_and_keep_preceding_environment_in_scope()
    {
        var expression = new Expression.Eval(
            Conditional(Env, Value(ExpressionEncoding.EncodeExpressionAsValue(List(Env, Lit(2)))),
                Value(ExpressionEncoding.EncodeExpressionAsValue(List(Lit(3), Env)))),
            List(Lit(71), Env));
        Compile(expression).Stats.InlinedCalls.Should().Be(0);
        foreach (var input in ImmutableList.Create(PineKernelValues.TrueValue, PineKernelValues.FalseValue))
            Run(expression, input, new()).ReturnValue.Evaluate()
                .Should().Be(new DirectInterpreter(new(), null).EvaluateExpressionDefault(expression, input));
    }

    [Fact]
    public void Unsupported_callee_is_refused_without_poisoning_an_unselected_branch()
    {
        var expression = Conditional(Env, Lit(8), Call(Builtin("unsupported-graph-builtin", Env), Env));
        var result = Compile(expression);
        result.Stats.Declines.Single().Code.Should().Be(GraphOptimizationDeclineCode.UnsupportedBody);
        result.Graph.KnownFunctionSignatures.Should().BeEmpty();
        Calls(result.Graph.Graph).Single().Target.Should().BeOfType<CallTarget.Dynamic>();
        Run(expression, PineKernelValues.FalseValue, new()).ReturnValue.Evaluate()
            .Should().Be(IntegerEncoding.EncodeSignedInteger(8));
    }

    [Theory]
    [InlineData(2147483648L)]
    [InlineData(-2147483649L)]
    public void Speculative_parameter_path_overflow_preserves_unselected_dynamic_call_and_memo_parity(long count)
    {
        var body = Builtin("head", Builtin("skip", List(Value(IntegerEncoding.EncodeSignedInteger(count)), Env)));
        var expression = Conditional(Env, Lit(8), Call(body, Env));
        var baseline = Compile(expression, new(Enabled: false));
        var cold = Optimize(baseline.Graph, new(), baseline.Memo);
        var warm = Optimize(baseline.Graph, new(), cold.Memo);
        cold.Stats.Declines.Single().Code.Should().Be(GraphOptimizationDeclineCode.UnsupportedBody);
        cold.Stats.InlinedCalls.Should().Be(0);
        cold.Graph.Should().BeSameAs(baseline.Graph);
        warm.Graph.Should().BeSameAs(baseline.Graph);
        warm.Stats.Should().Be(cold.Stats);
        warm.Memo.Should().Be(cold.Memo);
        cold.Memo.Parses.Should().HaveCount(1);
        cold.Memo.Preparations.Should().Equal(baseline.Memo.Preparations);
        cold.Memo.Graphs.Should().Equal(baseline.Memo.Graphs);
        cold.Graph.KnownFunctionSignatures.Should().BeEmpty();
        Calls(cold.Graph.Graph).Should().Equal(Calls(baseline.Graph.Graph));
        foreach (var options in ImmutableList.Create(new GraphOptimizerOptions(Enabled: false), new GraphOptimizerOptions()))
            Run(expression, PineKernelValues.FalseValue, options).ReturnValue.Evaluate()
                .Should().Be(IntegerEncoding.EncodeSignedInteger(8));
    }

    [Fact]
    public void Nested_wrappers_stop_at_candidate_limit_and_environment_carried_recursion_stays_dynamic()
    {
        var nested = Enumerable.Range(0, 6).Aggregate(Env, (inner, _) => Call(inner, Env));
        var bounded = Compile(nested, new(MaxCandidates: 2));
        bounded.Stats.Candidates.Should().Be(2);
        bounded.Stats.InlinedCalls.Should().Be(2);
        bounded.Stats.BudgetLimitReached.Should().BeTrue();
        Calls(bounded.Graph.Graph).Single().Target.Should().BeOfType<CallTarget.Dynamic>();
        var recursiveBody = new Expression.Eval(Env, Env);
        var expression = Call(recursiveBody, Env);
        var result = Compile(expression);
        result.Stats.InlinedCalls.Should().Be(1);
        result.Stats.SelfTailCalls.Should().Be(0);
        result.Stats.Declines.Single().Code.Should().Be(GraphOptimizationDeclineCode.UnknownTarget);
        foreach (var options in ImmutableList.Create(new GraphOptimizerOptions(Enabled: false), new GraphOptimizerOptions()))
            ExpressionGraphVM.Create(optimizerOptions: LiteralPolicy(options)).EvaluateExpressionOnCustomStack(
                expression, ExpressionEncoding.EncodeExpressionAsValue(recursiveBody), new(3, 1000, 10))
                .IsErrOrNull()!.Reason.Should().BeOfType<EvaluationErrorReason.QuotaExhausted>()
                .Which.QuotaKind.Should().Be(EvaluationQuotaKind.InvocationCount);
    }

    [Fact]
    public void Malformed_nested_inlinee_call_keeps_dynamic_fallback_after_outer_expansion()
    {
        var malformed = new Expression.Eval(Value(PineValue.Blob([255])), Env);
        var expression = Call(Conditional(Env, Lit(8), malformed), Env);
        var result = Compile(expression);
        result.Stats.InlinedCalls.Should().Be(1);
        result.Stats.Declines.Single().Code.Should().Be(GraphOptimizationDeclineCode.InvalidEncoding);
        Calls(result.Graph.Graph).Single().Target.Should().BeOfType<CallTarget.Dynamic>();
        Run(expression, PineKernelValues.FalseValue, new()).ReturnValue.Evaluate()
            .Should().Be(new DirectInterpreter(new(), null).EvaluateExpressionDefault(expression, PineKernelValues.FalseValue));
        ExpressionGraphVM.Create(optimizerOptions: new()).EvaluateExpressionOnCustomStack(
            expression, PineKernelValues.TrueValue, new(100, 1000, 100)).IsErrOrNull().Should().NotBeNull();
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void Malformed_target_is_not_a_compile_failure_in_selected_or_unselected_branch(bool selected)
    {
        var malformed = new Expression.Eval(Value(PineValue.Blob([255, 17])), List(Env, Lit(19)));
        var expression = Conditional(Env, Lit(8), malformed);
        var result = Compile(expression);
        result.Stats.Declines.Single().Code.Should().Be(GraphOptimizationDeclineCode.InvalidEncoding);
        Calls(result.Graph.Graph).Single().Target.Should().BeOfType<CallTarget.Dynamic>();
        var input = selected ? PineKernelValues.TrueValue : PineKernelValues.FalseValue;
        foreach (var options in ImmutableList.Create(new GraphOptimizerOptions(Enabled: false), new GraphOptimizerOptions()))
        {
            var evaluation = ExpressionGraphVM.Create(optimizerOptions: options)
                .EvaluateExpressionOnCustomStack(expression, input, new(100, 1000, 100));
            if (selected)
                evaluation.IsErrOrNull().Should().NotBeNull();
            else
                evaluation.IsOkOrNull()!.ReturnValue.Evaluate().Should().Be(IntegerEncoding.EncodeSignedInteger(8));
        }
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void Failed_callee_operations_remain_lazy_after_successful_inline(bool selected)
    {
        var failing = Builtin("bit_shift_left", List(Lit(-8), Value(PineValue.Blob([1]))));
        var expression = Conditional(Env, Lit(8), Call(failing, Env));
        Compile(expression).Stats.InlinedCalls.Should().Be(1);
        foreach (var options in ImmutableList.Create(new GraphOptimizerOptions(Enabled: false), new GraphOptimizerOptions()))
        {
            if (selected)
            {
                Action run = () => Run(expression, PineKernelValues.TrueValue, options);
                run.Should().Throw<InvalidIntermediateCodeException>();
            }
            else
                Run(expression, PineKernelValues.FalseValue, options).ReturnValue.Evaluate()
                    .Should().Be(IntegerEncoding.EncodeSignedInteger(8));
        }
    }

    [Fact]
    public void Environment_computation_is_retained_even_when_inlinee_ignores_it_and_runs_before_target_failure()
    {
        var failing = Builtin("bit_shift_left", List(Lit(-8), Value(PineValue.Blob([1]))));
        foreach (var expression in ImmutableList.Create(
            Call(Lit(99), failing),
            new Expression.Eval(Value(PineValue.Blob([255])), failing),
            Call(Lit(99), new Expression.Eval(Value(PineValue.Blob([255])), Env))))
        {
            foreach (var options in ImmutableList.Create(new GraphOptimizerOptions(Enabled: false), new GraphOptimizerOptions()))
            {
                if (expression is Expression.Eval { Environment: Expression.Eval })
                {
                    ExpressionGraphVM.Create(optimizerOptions: options)
                        .EvaluateExpressionOnCustomStack(expression, PineValue.EmptyList, new(100, 1000, 100))
                        .IsErrOrNull().Should().NotBeNull();
                }
                else
                {
                    Action run = () => Run(expression, PineValue.EmptyList, options);
                    run.Should().Throw<InvalidIntermediateCodeException>();
                }
            }
        }
        Compile(Call(Lit(99), failing)).Graph.Graph.Blocks.Values.SelectMany(block => block.Operations)
            .OfType<Operation.Builtin>().Single().Name.Should().Be("bit_shift_left");
    }

    [Fact]
    public void Disabled_policy_and_zero_budgets_preserve_exact_input_without_parsing()
    {
        var baseline = Compile(Call(Env, Env), new(Enabled: false));
        foreach (var options in ImmutableList.Create(
            new GraphOptimizerOptions(Enabled: false), new(MaxCandidates: 0), new(MaxWorkUnits: 0),
            new(MaxExpansionUnits: 0), new(MaxBodyNodes: 0), new(MaxDepth: 0)))
        {
            var result = Optimize(baseline.Graph, options, baseline.Memo);
            result.Graph.Should().BeSameAs(baseline.Graph);
            result.Memo.Should().BeSameAs(baseline.Memo);
            result.Stats.Candidates.Should().Be(0);
            result.Stats.WorkUnits.Should().Be(0);
            result.Memo.Parses.Should().BeEmpty();
        }
    }

    [Fact]
    public void Refused_expansion_never_publishes_known_target_or_signature_and_work_is_cumulative()
    {
        var expression = List(Call(Env, Env), Call(Env, Env), Call(Env, Env));
        var baseline = Compile(expression, new(Enabled: false));
        var refused = Optimize(baseline.Graph, new(MaxExpansionUnits: 1), baseline.Memo);
        refused.Graph.Should().BeSameAs(baseline.Graph);
        refused.Graph.KnownFunctionSignatures.Should().BeEmpty();
        refused.Stats.Declines.Should().HaveCount(3).And.OnlyContain(item =>
            item.InlineReason == GraphInliningDeclineCode.BudgetExhausted);
        Calls(refused.Graph.Graph).Should().Equal(Calls(baseline.Graph.Graph));
        var one = Compile(expression, new(MaxCandidates: 1));
        one.Stats.InlinedCalls.Should().Be(1);
        one.Stats.BudgetLimitReached.Should().BeTrue();
        Calls(one.Graph.Graph).Should().HaveCount(2).And.OnlyContain(call => call.Target is CallTarget.Dynamic);
        var limited = Compile(expression, new(MaxWorkUnits: one.Stats.WorkUnits));
        limited.Stats.InlinedCalls.Should().Be(1);
        limited.Stats.WorkUnits.Should().Be(one.Stats.WorkUnits);
        var growthLimited = Compile(expression, new(MaxExpansionUnits: one.Stats.GraphGrowthUnits + 1));
        growthLimited.Stats.InlinedCalls.Should().BeLessThan(3);
        growthLimited.Stats.GraphGrowthUnits.Should().BeLessThanOrEqualTo(one.Stats.GraphGrowthUnits + 1);
        foreach (var options in ImmutableList.Create(new GraphOptimizerOptions(MaxCandidates: 1),
            new(MaxWorkUnits: 1), new(MaxExpansionUnits: 1)))
            Run(expression, PineValue.EmptyList, options).ReturnValue.Evaluate()
                .Should().Be(Run(expression, PineValue.EmptyList, null).ReturnValue.Evaluate());
    }

    [Fact]
    public void Owned_body_and_encoding_limits_precede_graph_building_on_cold_and_warm_memos()
    {
        var expression = Call(List(Env, Env, Env, Env), Env);
        var warm = Compile(expression);
        foreach (var options in ImmutableList.Create(
            new GraphOptimizerOptions(MaxBodyNodes: 2), new(MaxDepth: 1), new(MaxWorkUnits: 1)))
        {
            var coldLimited = Compile(expression, options);
            var warmLimited = Compile(expression, options, warm.Memo);
            coldLimited.Stats.Should().Be(warmLimited.Stats);
            coldLimited.Graph.Graph.Should().Be(warmLimited.Graph.Graph);
            coldLimited.Stats.InlinedCalls.Should().Be(0);
            coldLimited.Memo.Graphs.Should().HaveCount(1);
            coldLimited.Graph.KnownFunctionSignatures.Should().BeEmpty();
        }
        var nestedSpecialForm = Enumerable.Range(0, 15).Aggregate(Env,
            (inner, _) => Builtin("head", Builtin("skip", List(Lit(0), inner))));
        var bounded = Compile(Call(nestedSpecialForm, Env), new(MaxDepth: 256, MaxBodyNodes: 128));
        bounded.Stats.Declines.Single().Code.Should().Be(GraphOptimizationDeclineCode.BodyLimit);
        bounded.Memo.Graphs.Should().HaveCount(1);
    }

    [Fact]
    public void Cold_warm_memo_stats_and_rendering_are_deterministic_and_graph_memo_is_unoptimized()
    {
        var expression = List(Call(Call(Env, Env), Env), Call(List(Env, Lit(9)), Env));
        var cold = Compile(expression);
        var warm = Compile(expression, memo: cold.Memo);
        warm.Graph.Graph.Should().Be(cold.Graph.Graph);
        warm.Memo.Should().Be(cold.Memo);
        warm.Stats.Should().Be(cold.Stats);
        warm.Stats.GetHashCode().Should().Be(cold.Stats.GetHashCode());
        GraphRendering.Render(warm.Graph.Graph).Should().Be(GraphRendering.Render(cold.Graph.Graph));
        var baseline = Compile(expression, new(Enabled: false), cold.Memo);
        Calls(baseline.Graph.Graph).Should().HaveCount(2);
        baseline.Memo.Graphs[baseline.Memo.Preparations[Request(expression)]].Should().Be(baseline.Graph.Graph);
        baseline.Memo.Reductions.Should().BeEmpty();
        Optimize(baseline.Graph, new(InlineLiteralCalls: false), baseline.Memo)
            .Graph.Should().BeSameAs(baseline.Graph);
        var reordered = ValidatedFunctionGraph.ValidateGraph(
            new(baseline.Graph.Graph.Id, baseline.Graph.Graph.Signature, baseline.Graph.Graph.Entry,
                baseline.Graph.Graph.Blocks.Reverse().ToImmutableDictionary()),
            baseline.Graph.KnownFunctionSignatures).Extract(_ => throw new Exception());
        var replay = Optimize(reordered, new(), baseline.Memo);
        replay.Graph.Graph.Should().Be(cold.Graph.Graph);
        replay.Stats.Should().Be(cold.Stats);
        CompilerMemo.Empty.Parses.Should().BeEmpty();
    }

    [Fact]
    public void Allocated_function_ids_avoid_root_and_all_reserved_table_ids()
    {
        var baseline = Compile(Call(Call(Env, Env), Env), new(Enabled: false));
        var input = ValidatedFunctionGraph.ValidateGraph(
            new(new(int.MaxValue), baseline.Graph.Graph.Signature, baseline.Graph.Graph.Entry, baseline.Graph.Graph.Blocks),
            ImmutableDictionary<FunctionId, FunctionSignature>.Empty
                .Add(new(0), FunctionSignature.Canonical).Add(new(1), FunctionSignature.Canonical)
                .Add(new(int.MinValue), FunctionSignature.Canonical)).Extract(_ => throw new Exception());
        var result = Optimize(input, new(), CompilerMemo.Empty);
        result.Stats.InlinedCalls.Should().Be(2);
        result.Memo.Graphs.Keys.Select(item => item.Request.Id.Value).Order().Should().Equal(2, 3);
        result.Graph.KnownFunctionSignatures[new(0)].Should().Be(FunctionSignature.Canonical);
    }

    [Fact]
    public void Owned_results_do_not_alias_legacy_input_or_exported_literal_memory()
    {
        Check();
        void Check()
        {
            byte[] bytes = [71, 72, 73];
            var request = Request(Call(Value(new PineValue.BlobValue(bytes)), Env));
            var first = ExpressionGraphOptimizer.Compile(request, new(), CompilerMemo.Empty).Extract(_ => throw new Exception());
            var snapshot = GraphRendering.Render(first.Graph.Graph);
            bytes[0] = 255;
            var literal = first.Graph.Graph.Blocks.Values.SelectMany(block => block.Operations).OfType<Operation.Literal>()
                .First(item => item.Value is LiteralValue.Blob { Bytes.Count: 3 });
            var exported = (PineValue.BlobValue)OwnedExpression.ToValue(literal.Value);
            MemoryMarshal.TryGetArray(exported.Bytes, out var array).Should().BeTrue();
            array.Array![array.Offset] = 254;
            GraphRendering.Render(first.Graph.Graph).Should().Be(snapshot);
            var second = ExpressionGraphOptimizer.Compile(request, new(), first.Memo).Extract(_ => throw new Exception());
            second.Graph.Graph.Should().Be(first.Graph.Graph);
            second.Stats.Should().Be(first.Stats);
        }
    }

    [Theory]
    [InlineData(false, false)]
    [InlineData(false, true)]
    [InlineData(true, false)]
    [InlineData(true, true)]
    public void Preparation_and_graph_options_are_independent_and_default_graph_factory_is_unoptimized(bool reduce, bool optimize)
    {
        var expression = Call(List(Env, Lit(7)), Env);
        var input = IntegerEncoding.EncodeSignedInteger(5);
        var actual = ExpressionGraphVM.Create(new(DisableReduction: !reduce), null, new(Enabled: optimize))
            .EvaluateExpressionOnCustomStack(expression, input, new(100, 1000, 100)).Extract(_ => throw new Exception());
        actual.ReturnValue.Evaluate().Should().Be(new DirectInterpreter(new(), null).EvaluateExpressionDefault(expression, input));
        Run(expression, input, null).InvocationCount.Should().Be(1);
        Run(expression, input, new()).InvocationCount.Should().Be(0);
    }
}
