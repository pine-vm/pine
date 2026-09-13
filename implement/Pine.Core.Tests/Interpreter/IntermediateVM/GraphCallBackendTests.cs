using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.CommonEncodings;
using Pine.Core.Interpreter;
using Pine.Core.Interpreter.IntermediateVM;
using Pine.Core.Interpreter.IntermediateVM.Backend;
using Pine.Core.Interpreter.IntermediateVM.Semantic;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Threading;
using Xunit;
using VM = Pine.Core.Interpreter.IntermediateVM.PineVM;
using ValueType = Pine.Core.Interpreter.IntermediateVM.Semantic.ValueType;

namespace Pine.Core.Tests.Interpreter.IntermediateVM;

public class GraphCallBackendTests
{
    private static PineVirtualValueId V(int id) => new(id);
    private static ValueDefinition D(int id) => new(V(id));
    private static LiteralValue I(byte value) => new LiteralValue.Blob([4, value]);
    private static Operation Lit(int id, LiteralValue value) => new Operation.Literal(D(id), value);
    private static Terminator Ret(int id) => new Terminator.Return([V(id)]);
    private static BasicBlock B(int id, ImmutableList<int> parameters, ImmutableList<Operation> operations, Terminator terminator) =>
        new(new(id), parameters.Select(D).ToImmutableList(), operations, terminator);
    private static FunctionGraph G(int id, int entry, ImmutableList<BasicBlock> blocks, FunctionSignature? signature = null) =>
        new(new(id), signature ?? FunctionSignature.Canonical, new(entry), blocks.ToImmutableDictionary(block => block.Id));
    private static ContinuationBinding C(int id) => new ContinuationBinding.CallerValue(V(id));
    private static ContinuationBinding R => new ContinuationBinding.ReturnedResult(0);
    private static LiteralValue Own(PineValue value) =>
        value switch
        {
            PineValue.BlobValue blob => new LiteralValue.Blob(blob.Bytes.ToArray().ToImmutableList()),
            PineValue.ListValue list => new LiteralValue.List(list.Items.ToArray().Select(Own).ToImmutableList()),
            _ => throw new NotImplementedException("Own does not handle value variant: " + value.GetType().Name),
        };
    private static LiteralValue IdentityEncoding => Own(ExpressionEncoding.EncodeExpressionAsValue(Expression.EnvironmentInstance));
    private static Call Dynamic(int site, int encoded, int environment) =>
        new(new(site), new CallTarget.Dynamic(V(encoded)), FunctionSignature.Canonical, [V(environment)]);
    private static Call Known(int site, int function, FunctionSignature signature, params int[] args) =>
        new(new(site), new CallTarget.Known(new(function)), signature, args.Select(V).ToImmutableList());
    private static GraphProgram Compile(ImmutableList<FunctionGraph> graphs, bool reverse = false)
    {
        var signatures = graphs.ToImmutableDictionary(graph => graph.Id, graph => graph.Signature);
        return new(graphs.Select(graph =>
            GraphCompiler.Compile(
                ValidatedFunctionGraph.ValidateGraph(graph, signatures).Extract(errors => throw new Exception(string.Join(", ", errors))),
                (reverse ? graph.Blocks.Keys.OrderByDescending(id => id.Value) : graph.Blocks.Keys.OrderBy(id => id.Value)).ToImmutableList())
            .Extract(error => throw new Exception(error.ToString()))).ToImmutableDictionary(function => function.Id));
    }

    [Theory]
    [InlineData(false, false)]
    [InlineData(false, true)]
    [InlineData(true, false)]
    [InlineData(true, true)]
    public void Calls_preserve_caller_values_and_duplicate_results_with_permuted_layouts(bool known, bool reverse)
    {
        Check();
        void Check()
        {
            var root = G(1, 90,
                [
                    B(90, [1], [Lit(2, IdentityEncoding), Lit(3, I(9))],
                        new Terminator.Invoke(known ? Known(0, 2, FunctionSignature.Canonical, 1) : Dynamic(0, 2, 1),
                            new(new(-7), [C(3), R, R, C(1)]))),
                    B(-7, [4, 5, 6, 7], [new Operation.MakeList(D(8), [V(4), V(5), V(6), V(7)])], Ret(8)),
                ]);
            var program = Compile([root, Identity()], reverse);
            var execution = Run(program, I(3));
            Value(execution).Should().Be(StraightLineVMAdapter.ToPineValue(new LiteralValue.List([I(9), I(3), I(3), I(3)])));
            execution.Depths.Should().Equal(1, 2);
            var compiled = program.Functions[new(1)];
            compiled.Resources.Should().Be(new FrameResourceUsage(10, 4));
            execution.Trace.Max(item => item.EvaluationStackDepth).Should().Be(4);
            compiled.Layout.Count(fragment => fragment.Label.Kind == LayoutLabelKind.Return).Should().Be(1);
            Compile([root, Identity()], reverse).Should().Be(program);
        }
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void Projected_known_calls_preserve_declared_unsorted_paths_without_reconstructing_environment(bool reverse)
    {
        var signature = new FunctionSignature([new(new([7])), new(new([1])), new(new([3, 8]))], [ValueType.PineValue]);
        var callee = G(2, -5,
            [B(-5, [10, 11, 12], [new Operation.MakeList(D(13), [V(10), V(11), V(12)])], Ret(13))], signature);
        var root = G(1, 9, [B(9, [1], [Lit(2, I(20)), Lit(3, I(30))],
            new Terminator.TailInvoke(Known(0, 2, signature, 3, 1, 2)))]);
        var execution = Run(Compile([root, callee], reverse), I(10));
        Value(execution).Should().Be(StraightLineVMAdapter.ToPineValue(new LiteralValue.List([I(30), I(10), I(20)])));
        execution.Depths.Should().Equal(1, 1);
    }

    [Theory]
    [InlineData(false, false)]
    [InlineData(false, true)]
    [InlineData(true, false)]
    [InlineData(true, true)]
    public void Tail_calls_replace_frames_but_nested_nontail_calls_preserve_continuations(bool dynamicTail, bool reverse)
    {
        var root = G(1, 50,
            [B(50, [1], [Lit(2, IdentityEncoding)],
                new Terminator.Invoke(Known(0, 3, FunctionSignature.Canonical, 1), new(new(-9), [R, C(1)]))),
             B(-9, [3, 4], [new Operation.MakeList(D(5), [V(3), V(4)])], Ret(5))]);
        var middle = G(3, 7,
            [B(7, [1], [Lit(2, IdentityEncoding)],
                new Terminator.TailInvoke(dynamicTail ? Dynamic(0, 2, 1) : Known(0, 2, FunctionSignature.Canonical, 1)))]);
        var result = Run(Compile([root, Identity(), middle], reverse), I(6));
        Value(result).Should().Be(StraightLineVMAdapter.ToPineValue(new LiteralValue.List([I(6), I(6)])));
        result.Depths.Should().Equal(1, 2, 2);
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void Shared_continuation_and_self_target_parallel_swaps_survive_multiple_calls_in_loop(bool reverse)
    {
        var root = G(1, 90,
            [
                B(90, [1], [Lit(2, I(10)), Lit(3, I(20))],
                    new Terminator.Jump(new(new(5), [V(1), V(2), V(3)]))),
                B(5, [10, 11, 12],
                    [Lit(13, new LiteralValue.Blob([2, 1])),
                     new Operation.MakeList(D(14), [V(10), V(13)]),
                     new Operation.Builtin(D(15), "int_add", V(14))],
                    new Terminator.Branch(V(10), I(0), new(new(-4), [V(11), V(12)]), new(new(6), [V(15), V(11), V(12)]))),
                B(6, [20, 21, 22], [],
                    new Terminator.Invoke(Known(0, 2, FunctionSignature.Canonical, 20), new(new(7), [R, C(22), C(21)]))),
                B(7, [30, 31, 32], [],
                    new Terminator.Invoke(Known(1, 2, FunctionSignature.Canonical, 30), new(new(5), [R, C(31), C(32)]))),
                B(-4, [40, 41], [new Operation.MakeList(D(42), [V(40), V(41)])], Ret(42)),
            ]);
        var result = Run(Compile([root, Identity()], reverse), I(3));
        Value(result).Should().Be(StraightLineVMAdapter.ToPineValue(new LiteralValue.List([I(20), I(10)])));
        result.Depths.Count(depth => depth == 2).Should().Be(6);
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void Self_continuation_can_swap_inputs_and_return_the_encoded_expression(bool reverse)
    {
        // First call evaluates identity and returns an encoding of a constant. The self
        // continuation swaps caller locals and installs that returned encoding for the second call.
        var signature = new FunctionSignature([new(new([0])), new(new([1])), new(new([2])), new(new([3]))], [ValueType.PineValue]);
        var constant = Expression.LitralInst(StraightLineVMAdapter.ToPineValue(I(0)));
        var constantEncoding = Own(ExpressionEncoding.EncodeExpressionAsValue(constant));
        var root = G(1, 8,
            [
                B(8, [1, 2, 3, 4], [], new Terminator.Invoke(Dynamic(0, 1, 2), new(new(8), [R, C(1), C(4), C(3)]))),
            ], signature);
        // The third iteration attempts to evaluate integer zero: the error proves the first two
        // successful self returns neither clobbered the encoded-expression input nor lost the result.
        var result = Run(Compile([root, Identity()], reverse),
            new LiteralValue.List([IdentityEncoding, constantEncoding, I(10), I(20)]), extraExpression: constant);
        result.Result.IsErrOrNull()!.Reason.Should().BeOfType<EvaluationErrorReason.ParseExpressionFailed>();
        result.Depths.Should().Equal(1, 2, 2);
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void Recursive_known_ids_obey_tail_frame_and_invocation_quotas(bool tail)
    {
        var recursive = G(1, 0,
            [B(0, [1], [], tail
                ? new Terminator.TailInvoke(Known(0, 1, FunctionSignature.Canonical, 1))
                : new Terminator.Invoke(Known(0, 1, FunctionSignature.Canonical, 1), new(new(1), [R]))),
             B(1, [2], [], Ret(2))]);
        var result = Run(Compile([recursive]), I(4), new(5, 100, 3));
        var error = result.Result.IsErrOrNull()!.Reason.Should().BeOfType<EvaluationErrorReason.QuotaExhausted>().Which;
        error.QuotaKind.Should().Be(tail ? EvaluationQuotaKind.InvocationCount : EvaluationQuotaKind.StackDepth);
        result.Depths.Max().Should().Be(tail ? 1 : 3);
        EvaluationError.RenderDisplayString(result.Result.IsErrOrNull()!).Should().Contain("graph function 1");
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void Malformed_dynamic_literal_remains_runtime_parse_failure(bool tail)
    {
        var graph = G(1, 0,
            [B(0, [1], [Lit(2, I(77))], tail ? new Terminator.TailInvoke(Dynamic(0, 2, 1))
                : new Terminator.Invoke(Dynamic(0, 2, 1), new(new(1), [R]))),
             B(1, [3], [], Ret(3))]);
        Run(Compile([graph]), I(0)).Result.IsErrOrNull()!.Reason.Should().BeOfType<EvaluationErrorReason.ParseExpressionFailed>();
    }

    [Fact]
    public void Known_call_result_vectors_are_declined_and_missing_program_targets_are_rejected()
    {
        var signature = new FunctionSignature(FunctionSignature.Canonical.Parameters, [ValueType.PineValue, ValueType.PineValue]);
        var root = G(1, 0,
            [B(0, [1], [], new Terminator.Invoke(Known(0, 2, signature, 1), new(new(1), [R]))),
             B(1, [2], [], Ret(2))]);
        var validated = ValidatedFunctionGraph.ValidateGraph(root,
            ImmutableDictionary<FunctionId, FunctionSignature>.Empty.Add(new(2), signature)).Extract(_ => throw new Exception());
        GraphCompiler.Compile(validated).IsErrOrNull()!.Code.Should().Be(GraphBackendDiagnosticCode.UnsupportedCall);
        var goodRoot = G(1, 0, [B(0, [1], [], new Terminator.TailInvoke(Known(0, 2, FunctionSignature.Canonical, 1)))]);
        var program = Compile([goodRoot, Identity()]);
        Action invalid = () => _ = new GraphProgram(program.Functions.Remove(new(2)));
        invalid.Should().Throw<ArgumentException>();
    }

    [Fact]
    public void Cancellation_is_checked_on_the_opt_in_pipeline()
    {
        var recursive = G(1, 0, [B(0, [1], [], new Terminator.TailInvoke(Known(0, 1, FunctionSignature.Canonical, 1)))]);
        Run(Compile([recursive]), I(0), cancellation: new CancellationToken(true))
            .Result.IsErrOrNull()!.Reason.Should().BeOfType<EvaluationErrorReason.CancellationRequested>();
    }

    [Theory]
    [InlineData(false, false)]
    [InlineData(false, true)]
    [InlineData(true, false)]
    [InlineData(true, true)]
    public void Dynamic_cache_hits_and_uncached_known_calls_preserve_success_and_tail_returns(bool known, bool tail)
    {
        Check();
        void Check()
        {
            var call = known ? Known(0, 2, FunctionSignature.Canonical, 1) : Dynamic(0, 2, 1);
            var root = G(1, 0,
                [B(0, [1], [Lit(2, IdentityEncoding)],
                    tail ? new Terminator.TailInvoke(call) : new Terminator.Invoke(call, new(new(1), [R]))),
                 B(1, [3], [], Ret(3))]);
            var program = Compile([root, Identity()]);
            var input = StraightLineVMAdapter.ToPineValue(I(6));
            var cache = new Dictionary<EvalCacheEntryKey, PineValue>
            {
                [new(ExpressionEncoding.EncodeExpressionAsValue(Expression.EnvironmentInstance),
                    StackFrameInput.FromEnvironmentValue(input, StaticFunctionInterface.Generic))] = input,
            };
            var cold = Run(program, I(6));
            var warm = Run(program, I(6), evalCache: cache);
            Value(warm).Should().Be(Value(cold));
            warm.Depths.Count.Should().Be(known ? 2 : 1);
            cold.Depths.Count.Should().Be(2);
            warm.Result.IsOkOrNull()!.Counters.InvocationCount.Should().Be(1);
            if (known)
            {
                warm.Trace.Where(instruction => instruction.GraphFunctionId == new FunctionId(2))
                    .Should().NotBeEmpty().And.OnlyContain(instruction => instruction.FrameExpression == null);
            }
        }
    }

    [Fact]
    public void Opt_in_compiler_rejects_shared_legacy_compilation_cache_delegates()
    {
        Action run = () => Run(Compile([G(1, 0, [B(0, [1], [], Ret(1))])]), I(0),
            compilationCache: new ConcurrentExpressionCompilationCache());
        run.Should().Throw<ArgumentException>().WithMessage("*isolated*");
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void Known_constant_graph_neither_reads_nor_populates_the_language_expression_cache(bool tail)
    {
        Check();
        void Check()
        {
            var call = Known(0, 2, FunctionSignature.Canonical, 1);
            var root = G(1, 0,
                [B(0, [1], [], tail ? new Terminator.TailInvoke(call) : new Terminator.Invoke(call, new(new(1), [R]))),
                 B(1, [2], [], Ret(2))]);
            var constant = G(2, 0, [B(0, [1], [Lit(2, I(42))], Ret(2))]);
            var program = Compile([root, constant]);
            var cache = new Dictionary<EvalCacheEntryKey, PineValue>();
            var admitEveryFrame = new InvocationCacheConfiguration(0, 0, 0, 0);
            Value(Run(program, I(19), evalCache: cache, invocationCacheConfiguration: admitEveryFrame))
                .Should().Be(StraightLineVMAdapter.ToPineValue(I(42)));
            cache.Should().BeEmpty("unproven graph IDs must not publish canonical expression cache entries");

            var input = StraightLineVMAdapter.ToPineValue(I(19));
            var identityKey = new EvalCacheEntryKey(
                ExpressionEncoding.EncodeExpressionAsValue(Expression.EnvironmentInstance),
                StackFrameInput.FromEnvironmentValue(input, StaticFunctionInterface.Generic));
            cache.Add(identityKey, input);
            Value(Run(program, I(19), evalCache: cache, invocationCacheConfiguration: admitEveryFrame))
                .Should().Be(StraightLineVMAdapter.ToPineValue(I(42)));
            cache.Should().ContainSingle().Which.Value.Should().Be(input);

            var dynamicRoot = G(1, 0,
                [B(0, [1], [Lit(2, IdentityEncoding)], new Terminator.TailInvoke(Dynamic(0, 2, 1)))]);
            var dynamicResult = Run(Compile([dynamicRoot, Identity()]), I(19), evalCache: cache);
            Value(dynamicResult).Should().Be(input);
            dynamicResult.Depths.Should().Equal([1], "canonical dynamic calls still use the valid language cache");
        }
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void Public_graph_factory_prepares_and_compiles_dynamic_callees_without_legacy_shortcuts(bool disableReduction)
    {
        Check();
        void Check()
        {
            var enteredExpressions = ImmutableList.CreateBuilder<Expression>();
            var root = new Expression.Eval(Expression.EnvironmentInstance,
                Expression.LitralInst(StraightLineVMAdapter.ToPineValue(I(19))));
            var vm = Pine.Core.Interpreter.IntermediateVM.Frontend.ExpressionGraphVM.Create(
                preparationOptions: new(DisableReduction: disableReduction));
            var result = vm.EvaluateExpressionOnCustomStack(root, StraightLineVMAdapter.ToPineValue(IdentityEncoding),
                new(100, 100, 10),
                reportEnteredStackFrame: (in EnteredStackFrame frame) =>
                    enteredExpressions.Add(frame.FrameExpression ?? throw new Exception("Canonical frame missing source expression.")))
                .Extract(error => throw new Exception(error.ToString())).ReturnValue.Evaluate();
            result.Should().Be(StraightLineVMAdapter.ToPineValue(I(19)));
            enteredExpressions.Should().Equal(root, Expression.EnvironmentInstance);
        }
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void Recursive_projected_calls_replace_frames_and_return_correct_accumulator(bool reverse)
    {
        var signature = new FunctionSignature([new(new([2])), new(new([0]))], [ValueType.PineValue]);
        var root = G(1, 20,
            [B(20, [1], [Lit(2, I(0))], new Terminator.TailInvoke(Known(0, 2, signature, 1, 2)))]);
        var callee = G(2, 9,
            [
                B(9, [1, 2], [], new Terminator.Branch(V(1), I(0), new(new(-2), [V(2)]), new(new(30), [V(1), V(2)]))),
                B(-2, [3], [], Ret(3)),
                B(30, [4, 5],
                    [Lit(6, new LiteralValue.Blob([2, 1])), Lit(7, I(1)),
                     new Operation.MakeList(D(8), [V(4), V(6)]), new Operation.Builtin(D(9), "int_add", V(8)),
                     new Operation.MakeList(D(10), [V(5), V(7)]), new Operation.Builtin(D(11), "int_add", V(10))],
                    new Terminator.TailInvoke(Known(0, 2, signature, 9, 11))),
            ], signature);
        var result = Run(Compile([root, callee], reverse), I(5));
        Value(result).Should().Be(StraightLineVMAdapter.ToPineValue(I(5)));
        result.Depths.Should().OnlyContain(depth => depth == 1);
        result.Depths.Count.Should().Be(7);
    }

    [Theory]
    [InlineData(0)]
    [InlineData(1)]
    public void Distinct_invokes_can_share_the_same_continuation(byte selector)
    {
        var root = G(1, 0,
            [
                B(0, [1], [Lit(2, I(10)), Lit(3, I(20))],
                    new Terminator.Branch(V(1), I(0), new(new(10), [V(2), V(3)]), new(new(20), [V(3), V(2)]))),
                B(10, [4, 5], [], new Terminator.Invoke(Known(0, 2, FunctionSignature.Canonical, 4), new(new(30), [R, C(5)]))),
                B(20, [6, 7], [], new Terminator.Invoke(Known(1, 2, FunctionSignature.Canonical, 6), new(new(30), [R, C(7)]))),
                B(30, [8, 9], [new Operation.MakeList(D(10), [V(8), V(9)])], Ret(10)),
            ]);
        var result = Run(Compile([root, Identity()], reverse: true), I(selector));
        Value(result).Should().Be(StraightLineVMAdapter.ToPineValue(
            new LiteralValue.List(selector == 0 ? [I(10), I(20)] : [I(20), I(10)])));
    }

    private static FunctionGraph Identity() => G(2, 0, [B(0, [1], [], Ret(1))]);
    private sealed record Execution(Result<EvaluationError, EvaluationReport> Result,
        ImmutableList<int> Depths, ImmutableList<ExecutedStackInstruction> Trace);
    private static PineValue Value(Execution execution) =>
        execution.Result.Extract(error => throw new Exception(error.ToString())).ReturnValue.Evaluate();

    private static Execution Run(GraphProgram program, LiteralValue input, VM.EvaluationConfig? config = null,
        CancellationToken cancellation = default, Expression? extraExpression = null,
        IDictionary<EvalCacheEntryKey, PineValue>? evalCache = null,
        ConcurrentExpressionCompilationCache? compilationCache = null,
        InvocationCacheConfiguration? invocationCacheConfiguration = null)
    {
        return Execute();
        Execution Execute()
        {
            var depths = ImmutableList.CreateBuilder<int>();
            var trace = ImmutableList.CreateBuilder<ExecutedStackInstruction>();
            var expression = Expression.LitralInst(PineValue.EmptyList);
            ExpressionCompilation CompileExpression(Expression requested)
            {
                if (requested == expression)
                    return new(GraphVMAdapter.ToStackFrame(program.Functions[new(1)], program), []);
                if (requested == Expression.EnvironmentInstance)
                    return new(GraphVMAdapter.ToStackFrame(program.Functions[new(2)], program), []);
                if (requested == extraExpression)
                {
                    var constant = G(7, 0, [B(0, [1], [Lit(2, I(0))], Ret(2))]);
                    var constantProgram = Compile([constant]);
                    return new(GraphVMAdapter.ToStackFrame(constantProgram.Functions[new(7)], constantProgram), []);
                }
                throw new InvalidOperationException("No legacy compiler fallback is allowed.");
            }
            var vm = VM.CreateCustom(
                evalCache: evalCache, evaluationConfigDefault: null, reportFunctionApplication: null,
                compilationEnvClasses: null, disableReductionInCompilation: true, selectPrecompiled: null,
                skipInlineForExpression: _ => true, enableTailRecursionOptimization: false, parseCache: null,
                precompiledLeaves: null, reportEnterPrecompiledLeaf: null, reportExitPrecompiledLeaf: null,
                optimizationParametersSerial: null, cacheFileStore: null,
                disableDirectContinueForSimpleEval: true, disableDirectEvalForSimpleTemplate: true,
                reportExecutedStackInstruction: (in ExecutedStackInstruction instruction) => trace.Add(instruction),
                compileExpression: CompileExpression,
                invocationCacheConfiguration: invocationCacheConfiguration,
                tryGetExpressionCompilation: compilationCache is null ? null : compilationCache.TryGet,
                getOrAddExpressionCompilation: compilationCache is null ? null : compilationCache.GetOrAdd);
            var result = vm.EvaluateExpressionOnCustomStack(expression, StraightLineVMAdapter.ToPineValue(input),
                config ?? new(100, 200, 10),
                reportEnteredStackFrame: (in EnteredStackFrame frame) =>
                {
                    if (frame.Instructions.GraphFunctionId is not null)
                        frame.FrameExpression.Should().BeNull();
                    depths.Add(frame.StackFrameDepth);
                },
                cancellationToken: cancellation);
            return new(result, depths.ToImmutable(), trace.ToImmutable());
        }
    }
}
