using AwesomeAssertions;
using Pine.Core.CommonEncodings;
using Pine.Core.Interpreter.IntermediateVM;
using Pine.Core.Interpreter.IntermediateVM.Backend;
using Pine.Core.Interpreter.IntermediateVM.Frontend;
using Pine.Core.Interpreter.IntermediateVM.Semantic;
using System;
using System.Collections.Immutable;
using System.Linq;
using Xunit;
using VM = Pine.Core.Interpreter.IntermediateVM.PineVM;
using ValueType = Pine.Core.Interpreter.IntermediateVM.Semantic.ValueType;

namespace Pine.Core.Tests.Interpreter.IntermediateVM;

public class GraphInliningTests
{
    private static PineVirtualValueId V(int id) => new(id);
    private static ValueDefinition D(int id) => new(V(id));
    private static LiteralValue I(int value) => Own(IntegerEncoding.EncodeSignedInteger(value));
    private static LiteralValue L(params ImmutableArray<LiteralValue> values) => new LiteralValue.List(values.ToImmutableList());
    private static Operation Lit(int id, LiteralValue value) => new Operation.Literal(D(id), value);
    private static Terminator Ret(params ImmutableArray<int> values) => new Terminator.Return(values.Select(V).ToImmutableList());
    private static Edge E(int target, params ImmutableArray<int> args) => new(new(target), args.Select(V).ToImmutableList());
    private static ContinuationBinding C(int value) => new ContinuationBinding.CallerValue(V(value));
    private static ContinuationBinding R(int slot = 0) => new ContinuationBinding.ReturnedResult(slot);
    private static BasicBlock B(int id, ImmutableList<int> parameters, ImmutableList<Operation> operations, Terminator terminator) =>
        new(new(id), parameters.Select(D).ToImmutableList(), operations, terminator);
    private static FunctionGraph G(int id, int entry, ImmutableList<BasicBlock> blocks, FunctionSignature? signature = null) =>
        new(new(id), signature ?? FunctionSignature.Canonical, new(entry), blocks.ToImmutableDictionary(block => block.Id));
    private static Call K(int site, int function, FunctionSignature signature, params ImmutableArray<int> args) =>
        new(new(site), new CallTarget.Known(new(function)), signature, args.Select(V).ToImmutableList());
    private static Call Dynamic(int site, int target, int argument) =>
        new(new(site), new CallTarget.Dynamic(V(target)), FunctionSignature.Canonical, [V(argument)]);
    private static FunctionSignature Triple { get; } =
        new([new(new([2])), new(new([0])), new(new([1]))], [ValueType.PineValue]);
    private static LiteralValue IdentityEncoding => Own(ExpressionEncoding.EncodeExpressionAsValue(Expression.EnvironmentInstance));
    private static LiteralValue Own(PineValue value) => value switch
    {
        PineValue.BlobValue blob => new LiteralValue.Blob(blob.Bytes.ToArray().ToImmutableList()),
        PineValue.ListValue list => new LiteralValue.List(list.Items.ToArray().Select(Own).ToImmutableList()),
        _ => throw new NotImplementedException("Own does not handle value variant: " + value.GetType().Name),
    };
    private static ValidatedFunctionGraph Validate(FunctionGraph graph,
        ImmutableDictionary<FunctionId, FunctionSignature>? signatures = null) =>
        ValidatedFunctionGraph.ValidateGraph(graph, signatures ?? ImmutableDictionary<FunctionId, FunctionSignature>.Empty)
            .Extract(errors => throw new Exception(string.Join(", ", errors)));
    private static ValidatedFunctionGraph Inline(FunctionGraph caller, FunctionGraph callee, int site = 0) =>
        Inline(Validate(caller, ImmutableDictionary<FunctionId, FunctionSignature>.Empty.Add(callee.Id, callee.Signature)),
            Validate(callee), site);
    private static ValidatedFunctionGraph Inline(ValidatedFunctionGraph caller, ValidatedFunctionGraph callee, int site = 0) =>
        GraphInliner.Inline(caller, callee, new(site), new(100_000))
            .Extract(error => throw new Exception(error.ToString()));
    private static FunctionGraph Identity(int id = 3) => G(id, 0, [B(0, [1], [], Ret(1))]);

    // Entry is also the loop header. Every iteration swaps both payload parameters.
    private static FunctionGraph Loop(bool residual = false) => G(2, 100,
        [
            B(100, [1000, 1001, 1002], [],
                new Terminator.Switch(V(1000), [new(I(0), E(-10, 1001, 1002)), new(I(99), E(-20, 1001, 1002))],
                    E(200, 1000, 1001, 1002))),
            B(200, [2000, 2001, 2002],
                [Lit(2003, I(-1)), new Operation.MakeList(D(2004), [V(2000), V(2003)]),
                 new Operation.Builtin(D(2005), "int_add", V(2004)), Lit(2006, IdentityEncoding)],
                residual
                    ? new Terminator.Invoke(Dynamic(0, 2006, 2005), new(new(100), [R(), C(2002), C(2001)]))
                    : new Terminator.Jump(E(100, 2005, 2002, 2001))),
            B(-10, [3000, 3001], [new Operation.MakeList(D(3002), [V(3000), V(3001)])], Ret(3002)),
            B(-20, [4000, 4001], [new Operation.MakeList(D(4002), [V(4000), V(4000)])], Ret(4002)),
        ], Triple);

    private static FunctionGraph Twice() => G(1, 0,
        [
            B(0, [1], [Lit(2, I(10)), Lit(3, I(20))],
                new Terminator.Invoke(K(0, 2, Triple, 1, 2, 3), new(new(1), [R(), C(1), C(1)]))),
            B(1, [4, 5, 6], [Lit(7, I(30)), Lit(8, I(40))],
                new Terminator.Invoke(K(1, 2, Triple, 5, 7, 8), new(new(2), [C(4), R(), C(6)]))),
            B(2, [9, 10, 11], [new Operation.MakeList(D(12), [V(9), V(10), V(11)])], Ret(12)),
        ]);

    private static FunctionGraph RecursiveLoop(int id = 2)
    {
        var loop = Loop();
        var body = loop.Blocks[new(200)];
        return new(new(id), loop.Signature, loop.Entry, loop.Blocks.SetItem(body.Id, body with
        {
            Terminator = new Terminator.TailInvoke(K(0, id, loop.Signature, 2005, 2002, 2001)),
        }));
    }

    [Theory]
    [InlineData(0)]
    [InlineData(1)]
    [InlineData(2)]
    [InlineData(5)]
    public void Semantic_self_tail_pass_preserves_entry_parameter_swaps_operations_and_reduces_invocations(int count)
    {
        var recursive = RecursiveLoop(1);
        var converted = GraphSelfTailLoops.Rewrite(Validate(recursive));
        converted.RewrittenCalls.Should().Be(1);
        converted.Graph.Graph.Blocks[new(200)].Operations.Should().BeSameAs(recursive.Blocks[new(200)].Operations);
        converted.Graph.Graph.Entry.Should().Be(recursive.Entry);
        converted.Graph.Graph.Blocks[new(200)].Terminator.Should().Be(new Terminator.Jump(E(100, 2005, 2002, 2001)));
        GraphSelfTailLoops.Rewrite(converted.Graph).Graph.Should().BeSameAs(converted.Graph);
        // Triple projects [2], [0], [1]; resetting the canonical prologue would lose these bindings.
        Compare(recursive, converted.Graph.Graph, [], L(I(10), I(20), I(count)),
           count % 2 == 0 ? L(I(10), I(20)) : L(I(20), I(10)), count);
        Run([converted.Graph.Graph], L(I(10), I(20), I(count)), new(2, 1000, 10))
           .IsOkOrNull().Should().NotBeNull();
        if (count > 0)
            Run([converted.Graph.Graph], L(I(10), I(20), I(count))).IsOkOrNull()!
                .LoopIterationCount.Should().BeGreaterThan(0);
    }

    [Theory]
    [InlineData(0)]
    [InlineData(3)]
    public void Semantic_callee_to_loop_then_inline_preserves_caller_state_and_list_counts(int count)
    {
        var recursive = RecursiveLoop();
        var loop = GraphSelfTailLoops.Rewrite(Validate(recursive)).Graph;
        var first = Inline(Validate(Twice(), ImmutableDictionary<FunctionId, FunctionSignature>.Empty.Add(loop.Graph.Id, loop.Graph.Signature)), loop);
        var second = Inline(first, loop, 1);
        Compare(Twice(), second.Graph, [recursive], I(count),
           L(count % 2 == 0 ? L(I(10), I(20)) : L(I(20), I(10)),
               count % 2 == 0 ? L(I(30), I(40)) : L(I(40), I(30)), I(count)), 2 + 2 * count);
        Calls(second.Graph).Should().BeEmpty();
    }

    [Fact]
    public void Semantic_self_tail_pass_ignores_dynamic_nonroot_and_nontail_calls()
    {
        var graph = G(1, 0,
           [
               B(0, [1], [], new Terminator.Invoke(K(0, 1, FunctionSignature.Canonical, 1), new(new(1), [R()]))),
               B(1, [2], [], new Terminator.TailInvoke(K(1, 2, FunctionSignature.Canonical, 2))),
               B(2, [3], [Lit(4, IdentityEncoding)], new Terminator.TailInvoke(Dynamic(2, 4, 3))),
           ]);
        var input = Validate(graph, ImmutableDictionary<FunctionId, FunctionSignature>.Empty.Add(new(2), FunctionSignature.Canonical));
        var result = GraphSelfTailLoops.Rewrite(input);
        result.Graph.Should().BeSameAs(input);
        result.RewrittenCalls.Should().Be(0);
    }

    [Fact]
    public void Semantic_root_self_tail_cycle_retains_existing_finite_quota_checks()
    {
        var recursive = G(1, 0, [B(0, [1], [], new Terminator.TailInvoke(K(0, 1, FunctionSignature.Canonical, 1)))]);
        var converted = GraphSelfTailLoops.Rewrite(Validate(recursive)).Graph.Graph;
        Run([recursive], I(1), new(3, 1000, 10)).IsErrOrNull()!.Reason
           .Should().BeOfType<EvaluationErrorReason.QuotaExhausted>().Which.QuotaKind.Should().Be(EvaluationQuotaKind.InvocationCount);
        foreach (var reverse in ImmutableList.Create(false, true))
            Run([converted], I(1), new(100, 3, 10), reverse).IsErrOrNull()!.Reason
                .Should().BeOfType<EvaluationErrorReason.QuotaExhausted>().Which.QuotaKind.Should().Be(EvaluationQuotaKind.LoopIterationCount);
    }

    [Fact]
    public void Semantic_self_tail_pipeline_policy_is_explicit_and_zero_budget_disables_all_work()
    {
        var input = Validate(RecursiveLoop(1));
        var enabled = ExpressionGraphOptimizer.Optimize(input, new(InlineLiteralCalls: false), CompilerMemo.Empty);
        enabled.Stats.SelfTailCalls.Should().Be(1);
        enabled.Stats.Candidates.Should().Be(0);
        enabled.Memo.Should().BeSameAs(CompilerMemo.Empty);
        enabled.Stats.BudgetLimitReached.Should().BeFalse();
        foreach (var options in ImmutableList.Create(new GraphOptimizerOptions(SelfTailLoops: false, ScalarReplacement: false),
            new(Enabled: false), new(MaxCandidates: 0), new(MaxExpansionUnits: 0)))
            ExpressionGraphOptimizer.Optimize(input, options, CompilerMemo.Empty).Graph.Should().BeSameAs(input);
    }

    [Theory]
    [InlineData(0, false)]
    [InlineData(1, false)]
    [InlineData(2, false)]
    [InlineData(3, false)]
    [InlineData(99, false)]
    [InlineData(3, true)]
    public void Two_consecutive_loop_expansions_preserve_entry_backedges_swaps_and_residual_calls(int count, bool residual)
    {
        var caller = Twice();
        var callee = Loop(residual);
        var first = Inline(caller, callee);
        var second = Inline(first, Validate(callee), 1);
        var reorderedCallee = new FunctionGraph(callee.Id, callee.Signature, callee.Entry,
            callee.Blocks.OrderByDescending(pair => pair.Key.Value).ToImmutableDictionary());
        Inline(caller, reorderedCallee).Graph.Should().Be(first.Graph);
        var expected = L(
            count == 99 ? L(I(10), I(10)) : count % 2 == 0 ? L(I(10), I(20)) : L(I(20), I(10)),
            count == 99 ? L(I(30), I(30)) : count % 2 == 0 ? L(I(30), I(40)) : L(I(40), I(30)), I(count));
        Compare(caller, second.Graph, [callee], I(count), expected, 2);
        second.Graph.Blocks.Count.Should().Be(caller.Blocks.Count + 2 * callee.Blocks.Count);
        var firstCloneIds = first.Graph.Blocks.Keys.Except(caller.Blocks.Keys).ToImmutableHashSet();
        second.Graph.Blocks.Keys.Except(first.Graph.Blocks.Keys).Should().NotIntersectWith(firstCloneIds);
        Calls(second.Graph).Should().OnlyContain(call => call.Target is CallTarget.Dynamic);
        Calls(second.Graph).Select(call => call.Site).Distinct().Count().Should().Be(residual ? 2 : 0);
        Calls(second.Graph).Select(call => call.Site.Value).Should().NotContain(0).And.NotContain(1);
        second.KnownFunctionSignatures[new(2)].Should().Be(Triple);
    }

    [Theory]
    [InlineData(0)]
    [InlineData(3)]
    public void Loop_callee_in_one_conditional_arm_keeps_other_arm_lazy(int count)
    {
        var caller = G(1, 0,
            [
                B(0, [1], [], new Terminator.Branch(V(1), I(0), E(5, 1), E(10, 1))),
                B(5, [2], [], Ret(2)),
                B(10, [3], [Lit(4, I(10)), Lit(5, I(20))],
                    new Terminator.Invoke(K(0, 2, Triple, 3, 4, 5), new(new(20), [R(), C(3)]))),
                B(20, [6, 7], [new Operation.MakeList(D(8), [V(6), V(7)])], Ret(8)),
            ]);
        var rewritten = Inline(caller, Loop()).Graph;
        Compare(caller, rewritten, [Loop()], I(count), count == 0 ? I(0) : L(L(I(20), I(10)), I(3)),
            count == 0 ? 0 : 1);
    }

    [Theory]
    [InlineData(0)]
    [InlineData(1)]
    [InlineData(4)]
    public void Inlined_inner_loop_preserves_outer_loop_state_and_ignored_result(int count)
    {
        var caller = G(1, 0,
            [
                B(0, [1], [Lit(2, I(10)), Lit(3, I(20))], new Terminator.Jump(E(10, 1, 2, 3))),
                B(10, [10, 11, 12], [], new Terminator.Branch(V(10), I(0), E(40, 11, 12), E(20, 10, 11, 12))),
                B(20, [20, 21, 22],
                    [Lit(23, I(-1)), new Operation.MakeList(D(24), [V(20), V(23)]),
                     new Operation.Builtin(D(25), "int_add", V(24)), Lit(26, I(2))],
                    new Terminator.Invoke(K(0, 2, Triple, 26, 21, 22), new(new(30), [C(25), C(22), C(21), R()]))),
                B(30, [30, 31, 32, 33], [], new Terminator.Jump(E(10, 30, 31, 32))),
                B(40, [40, 41], [new Operation.MakeList(D(42), [V(40), V(41)])], Ret(42)),
            ]);
        Compare(caller, Inline(caller, Loop()).Graph, [Loop()], I(count),
            count % 2 == 0 ? L(I(10), I(20)) : L(I(20), I(10)), count);
    }

    [Theory]
    [InlineData("a_b,XY9;!", ";!")]
    [InlineData(",x!", "!")]
    [InlineData("abc", "")]
    [InlineData("!,end", ",end")]
    public void Explicit_wrapper_then_two_identifier_scanner_expansions_remove_calls_without_extra_lists(string text, string remaining)
    {
        // A hand-built skipIdentifier-shaped list scanner, not an expression-frontend
        // or production Alfa optimization. The stop character remains in the result.
        var scanner = G(3, 10,
            [
                B(10, [10], [new Operation.Builtin(D(11), "head", V(10))],
                    new Terminator.Switch(V(11),
                        "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_"
                            .Select(character => new SwitchCase(I(character), E(20, 10))).ToImmutableList(),
                        E(30, 10))),
                B(20, [20], [Lit(21, I(1)), new Operation.MakeList(D(22), [V(21), V(20)]),
                    new Operation.Builtin(D(23), "skip", V(22))], new Terminator.Jump(E(10, 23))),
                B(30, [30], [], Ret(30)),
            ]);
        var wrapper = G(2, 0,
            [
                B(0, [1], [], new Terminator.Invoke(K(0, 3, FunctionSignature.Canonical, 1), new(new(1), [R()]))),
                B(1, [2], [Lit(3, I(1)), new Operation.MakeList(D(4), [V(3), V(2)]),
                    new Operation.Builtin(D(5), "skip", V(4))], new Terminator.TailInvoke(K(1, 3, FunctionSignature.Canonical, 5))),
            ]);
        var caller = G(1, 0,
            [
                B(0, [1], [], new Terminator.Invoke(K(0, 2, FunctionSignature.Canonical, 1), new(new(1), [C(1), R()]))),
                B(1, [2, 3], [new Operation.MakeList(D(4), [V(2), V(3)])], Ret(4)),
            ]);
        var signatures = ImmutableDictionary<FunctionId, FunctionSignature>.Empty
            .Add(scanner.Id, scanner.Signature).Add(wrapper.Id, wrapper.Signature);
        var first = Inline(Validate(caller, signatures), Validate(wrapper, signatures));
        Calls(first.Graph).Should().OnlyContain(call => call.Target == new CallTarget.Known(scanner.Id));
        Calls(first.Graph).Select(call => call.Site.Value).Order().Should().Equal(2, 3);
        var second = Inline(first, Validate(scanner), 2);
        var third = Inline(second, Validate(scanner), 3);
        Calls(third.Graph).Should().BeEmpty();
        var input = new LiteralValue.List(text.Select(character => I(character)).ToImmutableList());
        var suffix = new LiteralValue.List(remaining.Select(character => I(character)).ToImmutableList());
        Compare(caller, first.Graph, [wrapper, scanner], input, L(input, suffix), 1);
        Compare(caller, second.Graph, [wrapper, scanner], input, L(input, suffix), 2);
        Compare(caller, third.Graph, [wrapper, scanner], input, L(input, suffix), 3);
    }

    [Theory]
    [InlineData(false, false)]
    [InlineData(false, true)]
    [InlineData(true, false)]
    [InlineData(true, true)]
    public void Callee_tail_invoke_returns_to_outer_continuation_unless_site_is_tail(bool dynamic, bool tailSite)
    {
        var middle = G(2, 0, [B(0, [1], [Lit(2, IdentityEncoding)], new Terminator.TailInvoke(
            dynamic ? Dynamic(0, 2, 1) : K(0, 3, FunctionSignature.Canonical, 1)))]);
        var caller = G(1, 0,
            [B(0, [1], [], tailSite ? new Terminator.TailInvoke(K(0, 2, FunctionSignature.Canonical, 1)) :
                new Terminator.Invoke(K(0, 2, FunctionSignature.Canonical, 1), new(new(1), [C(1), R(), R()]))),
             B(1, [2, 3, 4], [new Operation.MakeList(D(5), [V(2), V(3), V(4)])], Ret(5))]);
        var dependencies = ImmutableDictionary<FunctionId, FunctionSignature>.Empty.Add(new(3), FunctionSignature.Canonical);
        var rewritten = Inline(Validate(caller, dependencies.Add(new(2), middle.Signature)), Validate(middle, dependencies));
        Compare(caller, rewritten.Graph, [middle, Identity()], I(7), tailSite ? I(7) : L(I(7), I(7), I(7)), 1);
        Calls(rewritten.Graph).Single().Site.Should().NotBe(new CallSiteId(0));
        rewritten.Graph.Blocks.Values.Count(block => block.Terminator is Terminator.TailInvoke).Should().Be(tailSite ? 1 : 0);
    }

    [Theory]
    [InlineData(0)]
    [InlineData(3)]
    [InlineData(99)]
    public void Tail_site_inline_loop_keeps_caller_return_contract(int count)
    {
        var caller = G(1, 0,
            [B(0, [1], [Lit(2, I(10)), Lit(3, I(20))], new Terminator.TailInvoke(K(0, 2, Triple, 1, 2, 3)))]);
        Compare(caller, Inline(caller, Loop()).Graph, [Loop()], I(count),
            count == 99 ? L(I(10), I(10)) : count % 2 == 0 ? L(I(10), I(20)) : L(I(20), I(10)), 1);
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void Multiple_result_slots_and_tail_adapter_are_semantic_only_and_keep_slot_order(bool tail)
    {
        var pair = new FunctionSignature(FunctionSignature.Canonical.Parameters, [ValueType.PineValue, ValueType.PineValue]);
        var middle = G(2, 0, [B(0, [1], [Lit(2, I(7))],
            tail ? new Terminator.TailInvoke(K(0, 3, pair, 1)) : Ret(1, 2))], pair);
        var caller = G(1, 0,
            [B(0, [1], [], new Terminator.Invoke(K(0, 2, pair, 1), new(new(1), [R(1), C(1), R(0), R(1)]))),
             B(1, [2, 3, 4, 5], [new Operation.MakeList(D(6), [V(2), V(3), V(4), V(5)])], Ret(6))]);
        var signatures = ImmutableDictionary<FunctionId, FunctionSignature>.Empty.Add(new(2), pair).Add(new(3), pair);
        var rewritten = Inline(Validate(caller, signatures), Validate(middle, signatures));
        if (tail)
        {
            var invoke = rewritten.Graph.Blocks.Values.Select(block => block.Terminator).OfType<Terminator.Invoke>().Single();
            invoke.Continuation.Bindings.Take(2).Should().Equal(R(0), R(1));
            var adapter = rewritten.Graph.Blocks[invoke.Continuation.Target];
            ((Terminator.Jump)adapter.Terminator).Edge.Arguments.Should().Equal(
                adapter.Parameters[1].Id, adapter.Parameters[2].Id, adapter.Parameters[0].Id, adapter.Parameters[1].Id);
            GraphCompiler.Compile(rewritten).IsErrOrNull()!.Code.Should().Be(GraphBackendDiagnosticCode.UnsupportedCall);
        }
        else
        {
            // Eliminating the multi-result call leaves only ordinary edge bindings;
            // the existing single-result backend can now execute the enclosing function.
            var report = Run([rewritten.Graph], I(4)).Extract(error => throw new Exception(error.ToString()));
            report.ReturnValue.Evaluate().Should().Be(StraightLineVMAdapter.ToPineValue(L(I(7), I(4), I(4), I(7))));
        }
    }

    [Fact]
    public void Projections_and_unreachable_operations_are_cloned_without_evaluation_or_pruning()
    {
        var callee = G(2, 0,
            [
                B(0, [1], [new Operation.Project(D(2), V(1), new([1, 0]))], Ret(2)),
                B(10, [3], [Lit(4, L(I(-8), new LiteralValue.Blob([1]))),
                    new Operation.Builtin(D(5), "bit_shift_left", V(4))], Ret(3)),
            ]);
        var caller = G(1, 0,
            [B(0, [1], [], new Terminator.Invoke(K(0, 2, FunctionSignature.Canonical, 1), new(new(1), [R(), C(1)]))),
             B(1, [2, 3], [new Operation.MakeList(D(4), [V(2), V(3)])], Ret(4))]);
        var input = L(I(8), L(I(9)));
        var rewritten = Inline(caller, callee);
        rewritten.Graph.Blocks.Count.Should().Be(caller.Blocks.Count + callee.Blocks.Count);
        rewritten.Graph.Blocks.Values.SelectMany(block => block.Operations).OfType<Operation.Builtin>().Should().ContainSingle();
        Compare(caller, rewritten.Graph, [callee], input, L(I(9), input), 1);
    }

    [Fact]
    public void Residual_recursive_calls_keep_callee_identity_when_its_validation_table_omits_root()
    {
        var callee = G(2, 0,
            [
                B(0, [1], [], new Terminator.Branch(V(1), I(0), E(1, 1), E(2, 1))),
                B(1, [2], [], Ret(2)),
                B(2, [3], [Lit(4, I(-1)), new Operation.MakeList(D(5), [V(3), V(4)]),
                    new Operation.Builtin(D(6), "int_add", V(5))], new Terminator.TailInvoke(K(0, 2, FunctionSignature.Canonical, 6))),
            ]);
        var caller = G(1, 0,
            [B(0, [1], [], new Terminator.Invoke(K(0, 2, FunctionSignature.Canonical, 1), new(new(1), [R(), C(1)]))),
             B(1, [2, 3], [new Operation.MakeList(D(4), [V(2), V(3)])], Ret(4))]);
        var validatedCallee = Validate(callee);
        validatedCallee.KnownFunctionSignatures.Should().BeEmpty();
        var rewritten = Inline(Validate(caller, ImmutableDictionary<FunctionId, FunctionSignature>.Empty.Add(new(2), callee.Signature)), validatedCallee);
        Calls(rewritten.Graph).Single().Target.Should().Be(new CallTarget.Known(new(2)));
        rewritten.KnownFunctionSignatures[new(2)].Should().Be(callee.Signature);
        Compare(caller, rewritten.Graph, [callee], I(3), L(I(0), I(3)), 1);
    }

    [Fact]
    public void Failing_unused_operations_remain_executable_after_inlining()
    {
        var callee = G(2, 0,
            [B(0, [1], [Lit(2, L(I(-8), new LiteralValue.Blob([1]))),
                new Operation.Builtin(D(3), "bit_shift_left", V(2))], Ret(1))]);
        var caller = G(1, 0,
            [B(0, [1], [], new Terminator.Invoke(K(0, 2, FunctionSignature.Canonical, 1), new(new(1), [C(1)]))),
             B(1, [2], [], Ret(2))]);
        var rewritten = Inline(caller, callee).Graph;
        rewritten.Blocks.Values.SelectMany(block => block.Operations).OfType<Operation.Builtin>().Single().Name.Should().Be("bit_shift_left");
        Action before = () => Run([caller, callee], I(4));
        Action after = () => Run([rewritten], I(4));
        before.Should().Throw<InvalidIntermediateCodeException>();
        after.Should().Throw<InvalidIntermediateCodeException>();
    }

    [Fact]
    public void Empty_self_cycle_stays_bounded_by_existing_loop_quota_after_inline()
    {
        var callee = G(2, 0, [B(0, [1], [], new Terminator.Jump(E(0, 1)))]);
        var caller = G(1, 0, [B(0, [1], [], new Terminator.TailInvoke(K(0, 2, FunctionSignature.Canonical, 1)))]);
        var rewritten = Inline(caller, callee).Graph;
        foreach (var graphs in ImmutableList.Create(ImmutableList.Create(caller, callee), ImmutableList.Create(rewritten)))
        {
            var error = Run(graphs, I(1), new(100, 3, 10)).IsErrOrNull()!;
            error.Reason.Should().BeOfType<EvaluationErrorReason.QuotaExhausted>().Which.QuotaKind
                .Should().Be(EvaluationQuotaKind.LoopIterationCount);
        }
    }

    [Fact]
    public void Sparse_extreme_ids_are_fresh_deterministic_and_inputs_and_evidence_are_immutable()
    {
        var caller = G(1, int.MinValue,
            [B(int.MinValue, [int.MaxValue], [], new Terminator.Invoke(K(int.MaxValue, 2, FunctionSignature.Canonical, int.MaxValue),
                new(new(int.MaxValue), [R(), C(int.MaxValue)]))),
             B(int.MaxValue, [int.MinValue, -9], [new Operation.MakeList(D(100), [V(int.MinValue), V(-9)])], Ret(100))]);
        var callee = G(2, int.MaxValue,
            [B(int.MaxValue, [int.MaxValue], [Lit(int.MinValue, IdentityEncoding)],
                new Terminator.TailInvoke(Dynamic(int.MaxValue, int.MinValue, int.MaxValue)))]);
        var beforeCaller = GraphRendering.Render(caller);
        var beforeCallee = GraphRendering.Render(callee);
        var hashCaller = caller.GetHashCode();
        var hashCallee = callee.GetHashCode();
        var validated = Validate(caller, ImmutableDictionary<FunctionId, FunctionSignature>.Empty.Add(new(2), callee.Signature));
        var first = Inline(validated, Validate(callee), int.MaxValue);
        var reordered = new FunctionGraph(callee.Id, callee.Signature, callee.Entry,
            callee.Blocks.Reverse().ToImmutableDictionary());
        var second = Inline(validated, Validate(reordered), int.MaxValue);
        first.Graph.Should().Be(second.Graph);
        GraphRendering.Render(first.Graph).Should().Be(GraphRendering.Render(second.Graph));
        first.Graph.GetHashCode().Should().Be(second.Graph.GetHashCode());
        first.Graph.Should().BeSameAs(Validate(first.Graph, first.KnownFunctionSignatures).Graph);
        caller.GetHashCode().Should().Be(hashCaller);
        callee.GetHashCode().Should().Be(hashCallee);
        GraphRendering.Render(caller).Should().Be(beforeCaller);
        GraphRendering.Render(callee).Should().Be(beforeCallee);
        Calls(first.Graph).Single().Site.Value.Should().Be(0);
        first.Graph.Blocks.Keys.Except(caller.Blocks.Keys).Should().OnlyContain(id => id.Value >= 0 && id.Value < int.MaxValue);
        Compare(caller, first.Graph, [callee], I(6), L(I(6), I(6)), 1);
    }

    [Fact]
    public void Budget_charges_capture_plumbing_and_refuses_without_changing_inputs()
    {
        var caller = Validate(Twice(), ImmutableDictionary<FunctionId, FunctionSignature>.Empty.Add(new(2), Triple));
        var callee = Validate(Loop());
        var snapshot = GraphRendering.Render(caller.Graph);
        foreach (var budget in ImmutableList.Create<long>(-1, 0, 1, 20))
        {
            var error = GraphInliner.Inline(caller, callee, new(0), new(budget)).IsErrOrNull()!;
            error.Code.Should().Be(GraphInliningDeclineCode.BudgetExhausted);
            error.UnchangedCaller.Should().BeSameAs(caller);
        }
        GraphRendering.Render(caller.Graph).Should().Be(snapshot);
        var threshold = Enumerable.Range(21, 500).First(limit =>
            GraphInliner.Inline(caller, callee, new(0), new(limit)).IsErrOrNull() is null);
        GraphInliner.Inline(caller, callee, new(0), new(threshold - 1)).IsErrOrNull()!.Code
            .Should().Be(GraphInliningDeclineCode.BudgetExhausted);
        GraphInliner.Inline(caller, callee, new(0), new(threshold)).Extract(_ => throw new Exception()).Graph
            .Should().Be(Inline(caller, callee).Graph);
    }

    [Fact]
    public void Missing_dynamic_mismatched_and_same_identity_calls_decline_precisely()
    {
        var known = G(1, 0, [B(0, [1], [], new Terminator.TailInvoke(K(0, 2, FunctionSignature.Canonical, 1)))]);
        var signatures = ImmutableDictionary<FunctionId, FunctionSignature>.Empty.Add(new(2), FunctionSignature.Canonical);
        Declines(Validate(known, signatures), Validate(Identity(2)), GraphInliningDeclineCode.MissingCallSite, 9);
        Declines(Validate(known, signatures), Validate(Identity(3)), GraphInliningDeclineCode.TargetMismatch);
        var dynamic = G(1, 0, [B(0, [1], [Lit(2, IdentityEncoding)], new Terminator.TailInvoke(Dynamic(0, 2, 1)))]);
        Declines(Validate(dynamic), Validate(Identity(2)), GraphInliningDeclineCode.DynamicTarget);
        var differentSignature = new FunctionSignature([new(new([0]))], [ValueType.PineValue]);
        var different = G(2, 0, [B(0, [1], [], Ret(1))], differentSignature);
        Declines(Validate(known, signatures), Validate(different), GraphInliningDeclineCode.SignatureMismatch);
        var self = G(1, 0, [B(0, [1], [], new Terminator.TailInvoke(K(0, 1, FunctionSignature.Canonical, 1)))]);
        Declines(Validate(self), Validate(self), GraphInliningDeclineCode.RecursiveIdentity);
        Declines(Validate(self), Validate(Identity(1)), GraphInliningDeclineCode.RecursiveIdentity);
    }

    [Theory]
    [InlineData(1)]
    [InlineData(9)]
    public void Signature_context_conflicts_including_caller_root_are_not_silently_overwritten(int conflictId)
    {
        var caller = G(1, 0, [B(0, [1], [], new Terminator.TailInvoke(K(0, 2, FunctionSignature.Canonical, 1)))]);
        var different = new FunctionSignature([new(new([0]))], [ValueType.PineValue]);
        var signatures = ImmutableDictionary<FunctionId, FunctionSignature>.Empty.Add(new(2), FunctionSignature.Canonical);
        Declines(Validate(caller, signatures.Add(new(9), FunctionSignature.Canonical)),
            Validate(Identity(2), ImmutableDictionary<FunctionId, FunctionSignature>.Empty.Add(new(conflictId), different)),
            GraphInliningDeclineCode.SignatureConflict);
    }

    private static void Declines(ValidatedFunctionGraph caller, ValidatedFunctionGraph callee, GraphInliningDeclineCode code, int site = 0)
    {
        var result = GraphInliner.Inline(caller, callee, new(site), new(100_000)).IsErrOrNull()!;
        result.Code.Should().Be(code);
        result.UnchangedCaller.Should().BeSameAs(caller);
        result.Diagnostics.Should().BeEmpty();
    }

    private static ImmutableList<Call> Calls(FunctionGraph graph) =>
        graph.Blocks.Values.SelectMany(block => block.Terminator switch
        {
            Terminator.Return => ImmutableList<Call>.Empty,
            Terminator.Jump => [],
            Terminator.Branch => [],
            Terminator.Switch => [],
            Terminator.Invoke invoke => [invoke.Call],
            Terminator.TailInvoke tail => [tail.Call],
            _ => throw new NotImplementedException("Calls does not handle terminator variant: " + block.Terminator.GetType().Name),
        }).ToImmutableList();

    private static void Compare(FunctionGraph before, FunctionGraph after, ImmutableList<FunctionGraph> dependencies,
        LiteralValue input, LiteralValue expected, int removedInvocations)
    {
        foreach (var reverse in ImmutableList.Create(false, true))
        {
            var original = Run(dependencies.Insert(0, before), input, reverse: reverse).Extract(error => throw new Exception(error.ToString()));
            var rewritten = Run(dependencies.Insert(0, after), input, reverse: reverse).Extract(error => throw new Exception(error.ToString()));
            original.ReturnValue.Evaluate().Should().Be(StraightLineVMAdapter.ToPineValue(expected));
            rewritten.ReturnValue.Evaluate().Should().Be(original.ReturnValue.Evaluate());
            rewritten.InvocationCount.Should().Be(original.InvocationCount - removedInvocations);
            rewritten.BuildListCount.Should().Be(original.BuildListCount);
        }
    }

    private static Result<EvaluationError, EvaluationReport> Run(ImmutableList<FunctionGraph> graphs, LiteralValue input,
        VM.EvaluationConfig? config = null, bool reverse = false)
    {
        var signatures = graphs.ToImmutableDictionary(graph => graph.Id, graph => graph.Signature);
        var program = new GraphProgram(graphs.Select(graph =>
            GraphCompiler.Compile(Validate(graph, signatures),
                (reverse ? graph.Blocks.Keys.OrderByDescending(id => id.Value) : graph.Blocks.Keys.OrderBy(id => id.Value)).ToImmutableList())
            .Extract(error => throw new Exception(error.ToString()))).ToImmutableDictionary(function => function.Id));
        var expression = Expression.LitralInst(PineValue.EmptyList);
        ExpressionCompilation CompileExpression(Expression requested)
        {
            if (requested == expression)
                return new(GraphVMAdapter.ToStackFrame(program.Functions[new(1)], program), []);
            if (requested == Expression.EnvironmentInstance)
                return new(GraphVMAdapter.ToStackFrame(GraphCompiler.Compile(Validate(Identity())).Extract(_ => throw new Exception())), []);
            throw new InvalidOperationException("No instruction-to-graph or legacy compiler fallback.");
        }
        var vm = VM.CreateCustom(
            evalCache: null, evaluationConfigDefault: null, reportFunctionApplication: null,
            compilationEnvClasses: null, disableReductionInCompilation: true, selectPrecompiled: null,
            skipInlineForExpression: _ => true, enableTailRecursionOptimization: false, parseCache: null,
            precompiledLeaves: null, reportEnterPrecompiledLeaf: null, reportExitPrecompiledLeaf: null,
            optimizationParametersSerial: null, cacheFileStore: null, disableDirectContinueForSimpleEval: true,
            disableDirectEvalForSimpleTemplate: true, compileExpression: CompileExpression);
        return vm.EvaluateExpressionOnCustomStack(expression, StraightLineVMAdapter.ToPineValue(input),
            config ?? new(100, 2000, 20));
    }
}
