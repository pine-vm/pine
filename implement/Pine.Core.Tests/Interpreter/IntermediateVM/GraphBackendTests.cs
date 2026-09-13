using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.Interpreter;
using Pine.Core.Interpreter.IntermediateVM;
using Pine.Core.Interpreter.IntermediateVM.Backend;
using Pine.Core.Interpreter.IntermediateVM.Semantic;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Reflection;
using System.Runtime.InteropServices;
using Xunit;
using VM = Pine.Core.Interpreter.IntermediateVM.PineVM;
using ValueType = Pine.Core.Interpreter.IntermediateVM.Semantic.ValueType;

namespace Pine.Core.Tests.Interpreter.IntermediateVM;

public class GraphBackendTests
{
    private static LiteralValue Empty => new LiteralValue.List([]);
    private static LiteralValue Integer(byte value) => new LiteralValue.Blob([4, value]);
    private static PineVirtualValueId V(int id) => new(id);
    private static ValueDefinition D(int id) => new(V(id));
    private static Edge E(int target, params ImmutableArray<int> arguments) =>
        new(new(target), arguments.Select(V).ToImmutableList());
    private static Terminator Return(int value) => new Terminator.Return([V(value)]);
    private static Operation Lit(int id, LiteralValue value) => new Operation.Literal(D(id), value);
    private static BasicBlock Block(
        int id, ImmutableList<int> parameters, ImmutableList<Operation> operations, Terminator terminator) =>
        new(new(id), parameters.Select(D).ToImmutableList(), operations, terminator);
    private static FunctionGraph Graph(
        int entry, ImmutableList<BasicBlock> blocks, FunctionSignature? signature = null) =>
        new(new(-719), signature ?? FunctionSignature.Canonical, new(entry),
            blocks.ToImmutableDictionary(block => block.Id));
    private static FunctionSignature Projected(int count) =>
        new(Enumerable.Range(0, count).Select(index => new FunctionParameter(new([index]))).ToImmutableList(),
            [ValueType.PineValue]);
    private static ValidatedFunctionGraph Validate(FunctionGraph graph) =>
        ValidatedFunctionGraph.ValidateGraph(graph, ImmutableDictionary<FunctionId, FunctionSignature>.Empty)
        .Extract(errors => throw new InvalidOperationException(string.Join(", ", errors)));
    private static GraphFunction Compile(FunctionGraph graph, ImmutableList<PineBlockId>? order = null) =>
        GraphCompiler.Compile(Validate(graph), order).Extract(error => throw new InvalidOperationException(error.ToString()));

    [Theory]
    [InlineData(1, 0, 2)]
    [InlineData(1, 2, 0)]
    [InlineData(2, 2, 0)]
    [InlineData(0, 1, 2)]
    public void Entry_backedges_perform_simultaneous_swap_three_cycle_duplicate_and_self_copy(int a, int b, int c)
    {
        Check();
        void Check()
        {
            var graph = Graph(900,
                [
                    Block(900, [31, 200, 7, 99],
                        [Lit(100, new LiteralValue.Blob([2, 1])),
                        new Operation.MakeList(D(101), [V(99), V(100)]),
                        new Operation.Builtin(D(102), "int_add", V(101))],
                        new Terminator.Branch(V(99), Integer(0), E(-80, 31, 200, 7),
                            E(900, new[] { 31, 200, 7 }[a], new[] { 31, 200, 7 }[b], new[] { 31, 200, 7 }[c], 102))),
                    Block(-80, [13, 17, 400],
                        [new Operation.MakeList(D(11), [V(13), V(17), V(400)])], Return(11)),
                ], Projected(4));
            var input = new LiteralValue.List([Integer(10), Integer(20), Integer(30), Integer(1)]);
            var expected = new LiteralValue.List(
                [Integer((byte)((a + 1) * 10)), Integer((byte)((b + 1) * 10)), Integer((byte)((c + 1) * 10))]);
            var rendering = GraphRendering.Render(graph);
            foreach (var order in Orders(graph))
            {
                var compiled = Compile(graph, order);
                var result = Evaluate(compiled, input);
                Value(result).Should().Be(StraightLineVMAdapter.ToPineValue(expected));
                AssertDiscipline(compiled, result.Trace);
                result.Trace.Count(item => item.Instruction.Kind == StackInstructionKind.Local_Get &&
                    item.Instruction.LocalIndex == 0).Should().Be(4, "entry initialization only executes in the prologue");
                result.Trace.Max(item => item.EvaluationStackDepth).Should().Be(4);
                compiled.Resources.Should().Be(new FrameResourceUsage(12, 4));
                Compile(graph, order).Should().Be(compiled);
                Compile(graph, order).GetHashCode().Should().Be(compiled.GetHashCode());
            }
            GraphRendering.Render(graph).Should().Be(rendering);
        }
    }

    [Fact]
    public void Nested_and_consecutive_cycles_execute_with_renamed_nonordered_ids_and_shuffled_layouts()
    {
        Check();
        void Check()
        {
            foreach (var ids in ImmutableList.Create(
                ImmutableList.Create(900, -4, 77, 8000, 6),
                ImmutableList.Create(-901, 2700, -71, 1, 999)))
            {
                var graph = NestedCycles(ids);
                foreach (var order in Orders(graph))
                {
                    var compiled = Compile(graph, order);
                    var result = Evaluate(compiled, new LiteralValue.List([Integer(2), Integer(3), Integer(0)]));
                    Value(result).Should().Be(StraightLineVMAdapter.ToPineValue(Integer(8)));
                    AssertDiscipline(compiled, result.Trace);
                    result.Trace.Any(item => item.Instruction.JumpOffset < 0).Should().BeTrue();
                }
            }
        }
    }

    private static FunctionGraph NestedCycles(ImmutableList<int> ids) =>
        Graph(ids[0],
            [
                Block(ids[0], [10, 11, 12], [Lit(13, Integer(2))],
                    new Terminator.Branch(V(10), Integer(0), E(ids[3], 13, 12), E(ids[1], 10, 11, 12))),
                Block(ids[1], [20, 21, 22],
                    [Lit(23, new LiteralValue.Blob([2, 1])), Lit(24, Integer(1)),
                    new Operation.MakeList(D(25), [V(21), V(23)]),
                    new Operation.Builtin(D(26), "int_add", V(25)),
                    new Operation.MakeList(D(27), [V(22), V(24)]),
                    new Operation.Builtin(D(28), "int_add", V(27))],
                    new Terminator.Branch(V(21), Integer(0), E(ids[2], 20, 22), E(ids[1], 20, 26, 28))),
                Block(ids[2], [30, 31],
                    [Lit(32, new LiteralValue.Blob([2, 1])), Lit(33, Integer(3)),
                    new Operation.MakeList(D(34), [V(30), V(32)]),
                    new Operation.Builtin(D(35), "int_add", V(34))],
                    new Terminator.Jump(E(ids[0], 35, 33, 31))),
                Block(ids[3], [40, 41],
                    [Lit(42, new LiteralValue.Blob([2, 1])), Lit(43, Integer(1)),
                    new Operation.MakeList(D(44), [V(40), V(42)]),
                    new Operation.Builtin(D(45), "int_add", V(44)),
                    new Operation.MakeList(D(46), [V(41), V(43)]),
                    new Operation.Builtin(D(47), "int_add", V(46))],
                    new Terminator.Branch(V(40), Integer(0), E(ids[4], 41), E(ids[3], 45, 47))),
                Block(ids[4], [50], [], Return(50)),
            ], Projected(3));

    [Theory]
    [InlineData(0, 10)]
    [InlineData(1, 20)]
    [InlineData(2, 30)]
    public void Critical_edges_and_shared_targets_keep_distinct_arguments(byte selector, byte expected)
    {
        Check();
        void Check()
        {
            var graph = Graph(200,
                [
                    Block(200, [1], [Lit(2, Integer(10))],
                        new Terminator.Branch(V(1), Integer(0), E(-30, 2), E(400, 1))),
                    Block(400, [3], [Lit(4, Integer(20)), Lit(5, Integer(30))],
                        new Terminator.Branch(V(3), Integer(1), E(-30, 4), E(-30, 5))),
                    Block(-30, [6], [], Return(6)),
                ]);
            foreach (var order in Orders(graph))
            {
                var compiled = Compile(graph, order);
                var result = Evaluate(compiled, Integer(selector));
                Value(result).Should().Be(StraightLineVMAdapter.ToPineValue(Integer(expected)));
                AssertDiscipline(compiled, result.Trace);
                compiled.Resources.Should().Be(new FrameResourceUsage(7, 1));
                result.Trace.Where(item => item.Instruction.Kind == StackInstructionKind.Jump_If_Equal_Const)
                    .Should().OnlyContain(item => item.EvaluationStackDepth == 1);
            }
        }
    }

    [Fact]
    public void Switch_preserves_order_exact_noncanonical_literals_and_default_with_shared_targets()
    {
        Check();
        void Check()
        {
            ImmutableList<LiteralValue> literals =
                [Empty, new LiteralValue.Blob([]), Integer(1), new LiteralValue.Blob([4, 0, 1]),
                new LiteralValue.Blob([4]), new LiteralValue.Blob([2]),
                new LiteralValue.List([Integer(1), Empty])];
            var graph = Graph(18,
                [
                    Block(18, [1],
                        Enumerable.Range(0, literals.Count + 1).Select(index => Lit(index + 20, Integer((byte)index))).ToImmutableList(),
                        new Terminator.Switch(V(1), literals.Select((literal, index) =>
                            new SwitchCase(literal, E(-100, index + 20))).ToImmutableList(), E(-100, literals.Count + 20))),
                    Block(-100, [2], [], Return(2)),
                ]);
            foreach (var order in Orders(graph))
            {
                var compiled = Compile(graph, order);
                ((SelectedTerminator.Match)compiled.Blocks.Single(block => block.Id == graph.Entry).Terminator)
                    .Cases.Select(@case => @case.Literal).Should().Equal(literals);
                foreach (var (input, index) in literals.Add(new LiteralValue.Blob([255])).Select((literal, index) => (literal, index)))
                {
                    var result = Evaluate(compiled, input);
                    Value(result).Should().Be(StraightLineVMAdapter.ToPineValue(Integer((byte)index)));
                    AssertDiscipline(compiled, result.Trace);
                    result.Trace.Count(item => item.Instruction.Kind == StackInstructionKind.Jump_If_Equal_Const)
                        .Should().Be(Math.Min(index + 1, literals.Count));
                }
            }
        }
    }

    [Fact]
    public void Empty_switch_uses_explicit_default_and_zero_argument_edges_work()
    {
        var graph = Graph(8,
            [
                Block(8, [1], [], new Terminator.Switch(V(1), [], E(-1))),
                Block(-1, [], [Lit(3, Integer(42))], Return(3)),
            ]);
        var compiled = Compile(graph, [new(-1), new(8)]);
        var result = Evaluate(compiled, Empty);
        Value(result).Should().Be(StraightLineVMAdapter.ToPineValue(Integer(42)));
        AssertDiscipline(compiled, result.Trace);
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void Pure_self_cycle_is_finite_under_legacy_quota_even_without_parameters_or_operations(bool parameterized)
    {
        Check();
        void Check()
        {
            var graph = parameterized
                ? Graph(int.MinValue, [Block(int.MinValue, [123], [], new Terminator.Jump(E(int.MinValue, 123)))])
                : Graph(int.MinValue, [Block(int.MinValue, [], [], new Terminator.Jump(E(int.MinValue)))],
                    new([], [ValueType.PineValue]));
            var compiled = Compile(graph);
            var result = Evaluate(compiled, Empty, 3);
            result.Result.Should().BeOfType<Result<EvaluationError, EvaluationReport>.Err>()
                .Which.Value.Reason.Should().Be(new EvaluationErrorReason.QuotaExhausted(EvaluationQuotaKind.LoopIterationCount, 3));
            result.Trace.Count(item => item.Instruction.JumpOffset < 0).Should().Be(4);
            GraphVMAdapter.ToStackFrame(compiled).Instructions
                .Where(instruction => instruction.Kind == StackInstructionKind.Jump_Const)
                .Should().OnlyContain(instruction => instruction.JumpOffset != 0);
            AssertDiscipline(compiled, result.Trace);
            compiled.Resources.Should().Be(new FrameResourceUsage(parameterized ? 2 : 1, parameterized ? 1 : 0));
        }
    }

    [Fact]
    public void Unreachable_blocks_are_selected_and_accounted_but_not_executed()
    {
        Check();
        void Check()
        {
            var unreachable = Block(-900, [],
                [
                    Lit(3, new LiteralValue.List([new LiteralValue.Blob([2, 8]), new LiteralValue.Blob([1])])),
                    new Operation.Builtin(D(4), "bit_shift_left", V(3)),
                    new Operation.MakeList(D(5), [V(3), V(3), V(3), V(3), V(3), V(3), V(3)]),
                ], Return(5));
            var graph = Graph(900, [Block(900, [1], [], Return(1)), unreachable]);
            var compiled = Compile(graph);
            compiled.Blocks.Select(block => block.Id).Should().Equal(new PineBlockId(-900), new(900));
            compiled.Resources.Should().Be(new FrameResourceUsage(5, 7));
            var result = Evaluate(compiled, Integer(42));
            Value(result).Should().Be(StraightLineVMAdapter.ToPineValue(Integer(42)));
            AssertDiscipline(compiled, result.Trace);
            result.Trace.Should().NotContain(item => item.Instruction.Kind == StackInstructionKind.Bit_Shift_Left_Generic);
            result.Trace.Max(item => item.EvaluationStackDepth).Should().Be(1);
            Compile(Graph(900, [unreachable, graph.Blocks[new(900)]]))
                .Should().Be(compiled, "dictionary insertion order is not a physical layout input");
        }
    }

    [Theory]
    [InlineData("equal")]
    [InlineData("length")]
    [InlineData("head")]
    [InlineData("skip")]
    [InlineData("take")]
    [InlineData("concat")]
    [InlineData("reverse")]
    [InlineData("negate")]
    [InlineData("int_add")]
    [InlineData("int_mul")]
    [InlineData("int_is_sorted_asc")]
    [InlineData("bit_and")]
    [InlineData("bit_or")]
    [InlineData("bit_xor")]
    [InlineData("bit_not")]
    [InlineData("bit_shift_left")]
    [InlineData("bit_shift_right")]
    public void Multiblock_operations_reuse_selection_and_match_direct_interpreter(string name)
    {
        Check();
        void Check()
        {
            var graph = Graph(90,
                [
                    Block(90, [1], [new Operation.Project(D(2), V(1), new([1]))], new Terminator.Jump(E(-5, 2))),
                    Block(-5, [3], [new Operation.Builtin(D(4), name, V(3))], Return(4)),
                ]);
            var input = name switch
            {
                "skip" or "take" => new LiteralValue.List([Integer(1), new LiteralValue.List([Integer(2), Integer(3)])]),
                "bit_shift_left" or "bit_shift_right" => new LiteralValue.List([Integer(2), new LiteralValue.Blob([255, 1])]),
                "negate" => (LiteralValue)Integer(3),
                _ => new LiteralValue.List([Integer(2), Integer(3)]),
            };
            foreach (var operand in ImmutableList.Create(input, Empty, new LiteralValue.Blob([9])))
            {
                var compiled = Compile(graph);
                var result = Evaluate(compiled, new LiteralValue.List([Empty, operand]));
                var expected = new DirectInterpreter(new PineVMParseCache(), null).EvaluateExpressionDefault(
                    new Expression.Builtin(name, Expression.EnvironmentInstance), StraightLineVMAdapter.ToPineValue(operand));
                Value(result).Should().Be(expected);
                AssertDiscipline(compiled, result.Trace);
                compiled.Resources.Should().Be(new FrameResourceUsage(5, 1));
            }
        }
    }

    [Fact]
    public void Unused_failing_operations_in_reachable_blocks_are_not_eliminated()
    {
        var negativeShift = new LiteralValue.List([new LiteralValue.Blob([2, 8]), new LiteralValue.Blob([1])]);
        var graph = Graph(1,
            [
                Block(1, [1], [], new Terminator.Jump(E(2, 1))),
                Block(2, [2],
                    [Lit(3, negativeShift),
                    new Operation.Builtin(D(4), "bit_shift_left", V(3))], Return(2)),
            ]);
        Action direct = () => new DirectInterpreter(new PineVMParseCache(), null).EvaluateExpressionDefault(
            new Expression.Builtin("bit_shift_left", Expression.EnvironmentInstance),
            StraightLineVMAdapter.ToPineValue(negativeShift));
        direct.Should().Throw<IndexOutOfRangeException>();
        Action run = () => Evaluate(Compile(graph), Empty);
        run.Should().Throw<InvalidIntermediateCodeException>().Which.InnerException.Should().BeOfType<IndexOutOfRangeException>();
    }

    [Fact]
    public void Layout_order_must_be_an_exact_permutation()
    {
        Check();
        void Check()
        {
            var graph = Graph(1, [Block(1, [1], [], new Terminator.Jump(E(2, 1))), Block(2, [2], [], Return(2))]);
            foreach (var order in ImmutableList.Create<ImmutableList<PineBlockId>>(
                [], [new(1)], [new(1), new(1)], [new(1), new(3)], [new(1), new(2), new(2)]))
                GraphCompiler.Compile(Validate(graph), order)
                    .Should().BeOfType<Result<GraphBackendDiagnostic, GraphFunction>.Err>()
                    .Which.Value.Code.Should().Be(GraphBackendDiagnosticCode.InvalidBlockOrder);
            Value(Evaluate(Compile(graph, [new(2), new(1)]), Integer(7)))
                .Should().Be(StraightLineVMAdapter.ToPineValue(Integer(7)));
        }
    }

    [Theory]
    [InlineData(false, false)]
    [InlineData(false, true)]
    [InlineData(true, false)]
    [InlineData(true, true)]
    public void Dynamic_and_known_calls_are_gracefully_declined_including_unreachable_calls(bool known, bool tail)
    {
        Check();
        void Check()
        {
            var call = new Call(new(7), known ? new CallTarget.Known(new(-719)) : new CallTarget.Dynamic(V(1)),
                FunctionSignature.Canonical, [V(1)]);
            var terminator = tail
                ? (Terminator)new Terminator.TailInvoke(call)
                : new Terminator.Invoke(call, new(new(80), [new ContinuationBinding.ReturnedResult(0)]));
            var graph = Graph(8,
                [Block(8, [1], [], terminator), Block(80, [2], [], Return(2))]);
            foreach (var candidate in ImmutableList.Create(graph,
                Graph(80, graph.Blocks.Values.ToImmutableList())))
                GraphCompiler.Compile(Validate(candidate))
                    .Should().BeOfType<Result<GraphBackendDiagnostic, GraphFunction>.Err>()
                    .Which.Value.Should().Be(new GraphBackendDiagnostic(GraphBackendDiagnosticCode.UnsupportedCall, graph.Id, new(8)));
        }
    }

    [Theory]
    [InlineData(0)]
    [InlineData(2)]
    public void Noncanonical_result_counts_are_declined(int count)
    {
        var graph = Graph(1,
            [Block(1, [1], [], new Terminator.Return(Enumerable.Repeat(V(1), count).ToImmutableList()))],
            new(FunctionSignature.Canonical.Parameters, Enumerable.Repeat(ValueType.PineValue, count).ToImmutableList()));
        GraphCompiler.Compile(Validate(graph)).Should().BeOfType<Result<GraphBackendDiagnostic, GraphFunction>.Err>()
            .Which.Value.Code.Should().Be(GraphBackendDiagnosticCode.UnsupportedResultArity);
    }

    [Fact]
    public void Fresh_adapter_payloads_cannot_mutate_owned_switch_literals_or_artifact_equality()
    {
        Check();
        void Check()
        {
            var literal = new LiteralValue.List([new LiteralValue.Blob([4, 0, 1]), Empty]);
            var graph = Graph(9,
                [
                    Block(9, [1], [Lit(2, Integer(7)), Lit(3, Integer(8))],
                        new Terminator.Switch(V(1), [new(literal, E(7, 2))], E(7, 3))),
                    Block(7, [4], [], Return(4)),
                ]);
            var compiled = Compile(graph);
            var originalHash = compiled.GetHashCode();
            var first = GraphVMAdapter.ToStackFrame(compiled);
            var second = GraphVMAdapter.ToStackFrame(compiled);
            var firstLiteral = (PineValue.ListValue)first.Instructions
                .Single(instruction => instruction.Kind == StackInstructionKind.Jump_If_Equal_Const).Literal!;
            MemoryMarshal.TryGetArray(((PineValue.BlobValue)firstLiteral.Items.Span[0]).Bytes, out var bytes).Should().BeTrue();
            bytes.Array![bytes.Offset] = 99;
            second.Instructions.Single(instruction => instruction.Kind == StackInstructionKind.Jump_If_Equal_Const)
                .Literal.Should().Be(StraightLineVMAdapter.ToPineValue(literal));
            compiled.GetHashCode().Should().Be(originalHash);
            Compile(graph).Should().Be(compiled);
            Value(Evaluate(compiled, literal)).Should().Be(StraightLineVMAdapter.ToPineValue(Integer(7)));
        }
    }

    [Fact]
    public void Graph_adapter_uses_precomputed_resources_without_analyzing_instructions()
    {
        var constructor = typeof(GraphFunction).GetConstructors(BindingFlags.Instance | BindingFlags.NonPublic)
            .Single(candidate => candidate.GetParameters().Length == 6);
        var sentinel = (GraphFunction)constructor.Invoke(
            [new FunctionId(1), FunctionSignature.Canonical, ImmutableList<StorageBinding>.Empty,
            ImmutableList<SelectedBlock>.Empty, ImmutableList<LayoutFragment>.Empty, new FrameResourceUsage(7, 5)]);
        var frame = GraphVMAdapter.ToStackFrame(sentinel);
        frame.Instructions.Should().BeEmpty();
        frame.LocalsCount.Should().Be(7);
        frame.MaxStackUsage.Should().Be(5);
    }

    [Fact]
    public void Uninitialized_cross_block_reads_and_missing_edge_arguments_fail_before_selection()
    {
        Check();
        void Check()
        {
            var graph = Graph(1,
                [
                    Block(1, [1], [], new Terminator.Jump(E(2))),
                    Block(2, [2], [new Operation.MakeList(D(3), [V(1)])], Return(3)),
                ]);
            var errors = ValidatedFunctionGraph.ValidateGraph(graph, ImmutableDictionary<FunctionId, FunctionSignature>.Empty)
                .Should().BeOfType<Result<ImmutableList<GraphDiagnostic>, ValidatedFunctionGraph>.Err>().Which.Value;
            errors.Select(error => error.Code).Should().Equal(GraphDiagnosticCode.ArityMismatch, GraphDiagnosticCode.UndefinedValue);
        }
    }

    private static ImmutableList<ImmutableList<PineBlockId>> Orders(FunctionGraph graph)
    {
        var ids = graph.Blocks.Keys.OrderBy(id => id.Value).ToImmutableList();
        return [ids, ids.Reverse().ToImmutableList(), ids.Skip(1).Concat(ids.Take(1)).ToImmutableList()];
    }

    private sealed record Execution(
        Result<EvaluationError, EvaluationReport> Result, ImmutableList<ExecutedStackInstruction> Trace);

    private static PineValue Value(Execution execution) =>
        execution.Result.Extract(error => throw new InvalidOperationException(error.ToString())).ReturnValue.Evaluate();

    private static Execution Evaluate(GraphFunction compiled, LiteralValue input, int quota = 100)
    {
        return Run();
        Execution Run()
        {
            var trace = ImmutableList.CreateBuilder<ExecutedStackInstruction>();
            var expression = Expression.EnvironmentInstance;
            var vm = VM.CreateCustom(
                evalCache: null, evaluationConfigDefault: null, reportFunctionApplication: null,
                compilationEnvClasses: null, disableReductionInCompilation: true, selectPrecompiled: null,
                skipInlineForExpression: _ => false, enableTailRecursionOptimization: false, parseCache: null,
                precompiledLeaves: null, reportEnterPrecompiledLeaf: null, reportExitPrecompiledLeaf: null,
                optimizationParametersSerial: null, cacheFileStore: null,
                reportExecutedStackInstruction: (in ExecutedStackInstruction instruction) => trace.Add(instruction),
                expressionCompilationOverrides: new Dictionary<Expression, ExpressionCompilation>
                {
                    [expression] = new(GraphVMAdapter.ToStackFrame(compiled), []),
                });
            var result = vm.EvaluateExpressionOnCustomStack(expression, StraightLineVMAdapter.ToPineValue(input),
                new VM.EvaluationConfig(10, quota, 10));
            return new(result, trace.ToImmutable());
        }
    }

    private static void AssertDiscipline(GraphFunction compiled, ImmutableList<ExecutedStackInstruction> trace)
    {
        Check();
        void Check()
        {
            var initialized = ImmutableHashSet.Create(0);
            var boundaries = ImmutableHashSet<int>.Empty;
            var offset = 0;
            foreach (var fragment in compiled.Layout)
            {
                boundaries = boundaries.Add(offset);
                offset += fragment.Instructions.Count + (fragment.Transfer switch
                {
                    LayoutTransfer.Return => 2,
                    LayoutTransfer.Jump => 1,
                    LayoutTransfer.Branch => 3,
                    _ => throw new NotImplementedException(
                        "AssertDiscipline does not handle transfer variant: " + fragment.Transfer.GetType().Name),
                });
            }
            foreach (var executed in trace)
            {
                if (boundaries.Contains(executed.InstructionPointer))
                    executed.EvaluationStackDepth.Should().Be(0, "all known fragment boundaries have empty stacks");
                if (executed.Instruction.LocalIndex is { } local)
                    local.Should().BeInRange(0, compiled.Resources.LocalsCount - 1);
                if (executed.Instruction.Kind == StackInstructionKind.Local_Get)
                    initialized.Should().Contain(executed.Instruction.LocalIndex!.Value);
                if (executed.Instruction.Kind == StackInstructionKind.Local_Set)
                {
                    initialized = initialized.Add(executed.Instruction.LocalIndex!.Value);
                    executed.EvaluationStackDepth.Should().BeGreaterThan(0, "Local_Set reads but does not pop");
                }
                if (executed.Instruction.Kind == StackInstructionKind.Return)
                    executed.EvaluationStackDepth.Should().Be(1);
                executed.EvaluationStackDepth.Should().BeLessThanOrEqualTo(compiled.Resources.MaxStackUsage);
            }
        }
    }
}
