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
using ValueType = Pine.Core.Interpreter.IntermediateVM.Semantic.ValueType;

namespace Pine.Core.Tests.Interpreter.IntermediateVM;

public class StraightLineBackendTests
{
    private static LiteralValue Empty => new LiteralValue.List([]);
    private static LiteralValue Integer(byte value) => new LiteralValue.Blob([4, value]);

    private static FunctionGraph Graph(
        ImmutableList<Operation> operations,
        Terminator terminator,
        FunctionSignature? signature = null,
        ImmutableList<ValueDefinition>? parameters = null) =>
        new(new(19), signature ?? FunctionSignature.Canonical, new(800),
            ImmutableDictionary<PineBlockId, BasicBlock>.Empty.Add(new(800),
                new(new(800), parameters ?? [new(new(901))], operations, terminator)));

    private static ValidatedFunctionGraph Validate(FunctionGraph graph) =>
        ValidatedFunctionGraph.ValidateGraph(graph, [])
        .Extract(errors => throw new InvalidOperationException(string.Join(", ", errors)));

    private static StraightLineFunction Compile(FunctionGraph graph) =>
        StraightLineCompiler.Compile(Validate(graph))
        .Extract(error => throw new InvalidOperationException(error.ToString()));

    private static StraightLineFunction Builtin(string name) =>
        Compile(Graph(
            [new Operation.Builtin(new(new(3)), name, new(901))],
            new Terminator.Return([new(3)])));

    private static LiteralValue ValidInput(string name) =>
        name switch
        {
            "skip" or "take" => new LiteralValue.List([Integer(1), new LiteralValue.List([Integer(2), Integer(3)])]),
            "bit_shift_left" or "bit_shift_right" => new LiteralValue.List([Integer(3), new LiteralValue.Blob([255, 1])]),
            "concat" => new LiteralValue.List([new LiteralValue.List([Integer(2)]), new LiteralValue.List([Integer(3)])]),
            "negate" => Integer(17),
            "bit_not" => new LiteralValue.Blob([5, 255, 0]),
            _ => new LiteralValue.List([Integer(2), Integer(3), Integer(7)]),
        };

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
    public void Every_builtin_matches_direct_interpretation_for_valid_and_malformed_inputs(string name)
    {
        Check();

        void Check()
        {
            var compiled = Builtin(name);
            ImmutableList<LiteralValue> inputs =
            [
                ValidInput(name), Empty, new LiteralValue.Blob([]), new LiteralValue.Blob([9]),
                new LiteralValue.Blob([2, 2, 2]), new LiteralValue.Blob([4, 9, 8]),
                new LiteralValue.List([Empty]),
                new LiteralValue.List([Integer(2), Empty]),
                new LiteralValue.List([Empty, Integer(2)]),
                new LiteralValue.List([Integer(2), Integer(2)]),
                new LiteralValue.List([Integer(2), Integer(2), Empty]),
                new LiteralValue.List([new LiteralValue.Blob([0]), new LiteralValue.Blob([255])]),
                new LiteralValue.List([new LiteralValue.Blob([2, 1]), new LiteralValue.Blob([128, 1])]),
            ];

            foreach (var input in inputs)
                Execute(compiled, input).Should().Be(DirectBuiltin(name, input),
                    name + " with " + GraphRendering.RenderLiteral(input));

            compiled.Resources.Should().Be(new FrameResourceUsage(3, 1));
        }
    }

    [Fact]
    public void Lists_literals_sparse_ids_and_definition_order_have_exact_resources()
    {
        Check();

        void Check()
        {
            var literal = new LiteralValue.List(
                [new LiteralValue.Blob([1, 2, 3, 4, 5]), Empty, new LiteralValue.List([Integer(5)])]);
            var graph = Graph(
                [
                    new Operation.Literal(new(new(400)), literal),
                    new Operation.Literal(new(new(8)), Integer(9)),
                    new Operation.MakeList(new(new(79)), [new(8), new(400), new(8), new(901)]),
                    new Operation.MakeList(new(new(17)), []),
                    new Operation.MakeList(new(new(42)), [new(17), new(79), new(400)]),
                ],
                new Terminator.Return([new(42)]));
            var rendering = GraphRendering.Render(graph);
            var hash = graph.GetHashCode();
            var compiled = Compile(graph);
            var duplicate = Compile(graph);
            var expected = new LiteralValue.List(
                [Empty, new LiteralValue.List([Integer(9), literal, Integer(9), Integer(6)]), literal]);

            Execute(compiled, Integer(6)).Should().Be(StraightLineVMAdapter.ToPineValue(expected));
            var directLiteral = Expression.LitralInst(StraightLineVMAdapter.ToPineValue(literal));
            var directNine = Expression.LitralInst(StraightLineVMAdapter.ToPineValue(Integer(9)));
            var direct = new Expression.List(
                [new Expression.List([]),
                new Expression.List([directNine, directLiteral, directNine, Expression.EnvironmentInstance]),
                directLiteral]);
            Execute(compiled, Integer(6)).Should().Be(
                new DirectInterpreter(new PineVMParseCache(), null)
                .EvaluateExpressionDefault(direct, StraightLineVMAdapter.ToPineValue(Integer(6))));
            compiled.Should().Be(duplicate);
            compiled.GetHashCode().Should().Be(duplicate.GetHashCode());
            compiled.Storage.Should().Equal(
                new StorageBinding(new(901), 1), new(new(400), 2), new(new(8), 3),
                new(new(79), 4), new(new(17), 5), new(new(42), 6));
            compiled.Resources.Should().Be(new FrameResourceUsage(7, 4));
            GraphRendering.Render(graph).Should().Be(rendering);
            graph.GetHashCode().Should().Be(hash);
            AssertStackDiscipline(compiled, Integer(6));
        }
    }

    [Theory]
    [InlineData(0)]
    [InlineData(1)]
    [InlineData(8)]
    [InlineData(int.MaxValue)]
    public void List_projections_match_direct_paths_including_blobs_and_out_of_range(int index)
    {
        Check();

        void Check()
        {
            ImmutableList<LiteralValue> inputs =
                [Empty, new LiteralValue.Blob([]), new LiteralValue.Blob([5, 6]),
                new LiteralValue.List([new LiteralValue.Blob([7]), new LiteralValue.List([Integer(8)])])];
            ImmutableList<EnvironmentPath> paths = [new([]), new([index]), new([index, 0]), new([index, 0, 0])];
            foreach (var input in inputs)
                foreach (var path in paths)
                {
                    var compiled = Compile(Graph(
                        [new Operation.Project(new(new(2)), new(901), path)],
                        new Terminator.Return([new(2)])));
                    Execute(compiled, input).Should().Be(DirectProjection(input, path));
                    compiled.Resources.Should().Be(new FrameResourceUsage(3, 1));
                }
        }
    }

    [Fact]
    public void Entry_parameters_preserve_empty_nested_duplicate_and_unsorted_paths()
    {
        Check();

        void Check()
        {
            ImmutableList<EnvironmentPath> paths = [new([2]), new([]), new([0, 1]), new([2]), new([0]), new([9]), new([1, 0])];
            var signature = new FunctionSignature([.. paths.Select(path => new FunctionParameter(path))], [ValueType.PineValue]);
            var parameters = paths.Select((_, i) => new ValueDefinition(new(999 - i * 33))).ToImmutableList();
            var graph = Graph(
                [new Operation.MakeList(new(new(7)), [.. parameters.Select(p => p.Id)])],
                new Terminator.Return([new(7)]), signature, parameters);
            var environment = new LiteralValue.List(
                [new LiteralValue.List([Integer(2), Integer(3)]), new LiteralValue.Blob([4]), Integer(5)]);
            var compiled = Compile(graph);
            var expected = PineValue.List([.. paths.Select(path => DirectProjection(environment, path))]);

            Execute(compiled, environment).Should().Be(expected);
            compiled.Storage.Take(parameters.Count).Select(binding => binding.Value).Should().Equal(parameters.Select(p => p.Id));
            compiled.Resources.Should().Be(new FrameResourceUsage(9, 7));
            var frame = StraightLineVMAdapter.ToStackFrame(compiled);
            frame.Parameters.ParamsPaths.Should().ContainSingle().Which.Should().BeEmpty();
            AssertStackDiscipline(compiled, environment);
        }
    }

    [Fact]
    public void Semantic_list_projection_is_distinct_from_builtin_head_on_blobs()
    {
        Check();

        void Check()
        {
            var blob = new LiteralValue.Blob([12, 34]);
            var projection = Compile(Graph(
                [new Operation.Project(new(new(2)), new(901), new([0]))],
                new Terminator.Return([new(2)])));
            Execute(projection, blob).Should().Be(PineValue.EmptyList);
            Execute(Builtin("head"), blob).Should().Be(StraightLineVMAdapter.ToPineValue(new LiteralValue.Blob([12])));
            Execute(Builtin("head"), blob).Should().Be(DirectBuiltin("head", blob));
            Execute(projection, blob).Should().Be(DirectProjection(blob, new([0])));

            var instruction = StraightLineVMAdapter.ToStackFrame(projection)
                .Instructions.Single(i => i.Kind == StackInstructionKind.List_Project_Const);
            instruction.ToString().Should().Be("List_Project_Const (0)");
        }
    }

    [Fact]
    public void No_parameters_and_single_literal_return_need_one_stack_slot()
    {
        var compiled = Compile(Graph(
            [new Operation.Literal(new(new(670)), Empty)],
            new Terminator.Return([new(670)]),
            new([], [ValueType.PineValue]), []));
        compiled.Resources.Should().Be(new FrameResourceUsage(2, 1));
        Execute(compiled, Integer(4)).Should().Be(PineValue.EmptyList);
    }

    [Fact]
    public void Empty_operation_sequence_returns_an_initialized_parameter()
    {
        var compiled = Compile(Graph([], new Terminator.Return([new(901)])));
        compiled.Resources.Should().Be(new FrameResourceUsage(2, 1));
        compiled.Instructions.Should().Equal(
            new SelectedInstruction.Load(0), new SelectedInstruction.Store(1),
            new SelectedInstruction.Pop(), new SelectedInstruction.Load(1), new SelectedInstruction.Return());
        Execute(compiled, Integer(9)).Should().Be(StraightLineVMAdapter.ToPineValue(Integer(9)));
    }

    [Fact]
    public void Unsupported_valid_graphs_are_declined_without_fallback()
    {
        Check();

        void Check()
        {
            var edge = new Edge(new(800), [new(901)]);
            var call = new Call(new(1), new CallTarget.Dynamic(new(901)), FunctionSignature.Canonical, [new(901)]);
            ImmutableList<Terminator> terminators =
            [
                new Terminator.Jump(edge),
                new Terminator.Branch(new(901), Empty, edge, edge),
                new Terminator.Switch(new(901), [new(Empty, edge)], edge),
                new Terminator.Invoke(call, new(new(800), [new ContinuationBinding.ReturnedResult(0)])),
                new Terminator.TailInvoke(call),
            ];
            foreach (var terminator in terminators)
                Decline(Graph([], terminator)).Should().Be(StraightLineDiagnosticCode.UnsupportedTerminator);

            var graph = Graph([], new Terminator.Return([new(901)]));
            var extra = new BasicBlock(new(2), [new(new(4))], [], new Terminator.Return([new(4)]));
            var unreachable = new FunctionGraph(graph.Id, graph.Signature, graph.Entry, graph.Blocks.Add(extra.Id, extra));
            Decline(unreachable).Should().Be(StraightLineDiagnosticCode.MultipleBlocks);

            foreach (var count in ImmutableList.Create(0, 2))
                Decline(Graph([], new Terminator.Return([.. Enumerable.Repeat(new PineVirtualValueId(901), count)]),
                    new(FunctionSignature.Canonical.Parameters, [.. Enumerable.Repeat(ValueType.PineValue, count)])))
                    .Should().Be(StraightLineDiagnosticCode.UnsupportedResultArity);
        }
    }

    [Fact]
    public void Adaptation_owns_nested_literal_payloads_without_interned_arrays()
    {
        Check();

        void Check()
        {
            var literal = new LiteralValue.List([new LiteralValue.Blob([4]), Empty, new LiteralValue.Blob([1, 2, 3, 4, 5])]);
            var compiled = Compile(Graph([new Operation.Literal(new(new(2)), literal)], new Terminator.Return([new(2)])));
            var before = compiled.GetHashCode();
            var first = StraightLineVMAdapter.ToStackFrame(compiled);
            var second = StraightLineVMAdapter.ToStackFrame(compiled);
            var firstLiteral = (PineValue.ListValue)first.Instructions.Single(i => i.Kind == StackInstructionKind.Push_Literal).Literal!;
            var secondLiteral = (PineValue.ListValue)second.Instructions.Single(i => i.Kind == StackInstructionKind.Push_Literal).Literal!;
            var blob = (PineValue.BlobValue)firstLiteral.Items.Span[0];
            MemoryMarshal.TryGetArray(blob.Bytes, out var bytes).Should().BeTrue();
            bytes.Array![bytes.Offset] = 99;
            MemoryMarshal.TryGetArray(firstLiteral.Items, out var items).Should().BeTrue();
            items.Array![items.Offset + 1] = PineValue.Blob([5]);
            secondLiteral.Should().Be(StraightLineVMAdapter.ToPineValue(literal));
            compiled.GetHashCode().Should().Be(before);
            Compile(Graph([new Operation.Literal(new(new(2)), literal)], new Terminator.Return([new(2)])))
                .Should().Be(compiled);
            Execute(compiled, Empty).Should().Be(StraightLineVMAdapter.ToPineValue(literal));
        }
    }

    [Fact]
    public void Precomputed_metadata_constructor_does_not_inspect_instructions()
    {
        Check();

        void Check()
        {
            // Empty code is intentionally invalid for legacy analysis. This tests the boundary,
            // not execution, and would fail even if an initializer overwrote computed metadata.
            var frame = new StackFrameInstructions(StaticFunctionInterface.FromPathsSorted([[]]), [], 4, 3);
            frame.LocalsCount.Should().Be(4);
            frame.MaxStackUsage.Should().Be(3);
            var (parameters, instructions, constraint) = frame;
            parameters.Should().BeSameAs(frame.Parameters);
            instructions.Should().BeSameAs(frame.Instructions);
            constraint.Should().BeNull();
            Action legacy = () => new StackFrameInstructions(StaticFunctionInterface.FromPathsSorted([[]]), []);
            legacy.Should().Throw<IndexOutOfRangeException>();
        }
    }

    [Fact]
    public void Adapter_uses_precomputed_bounds_without_analyzing_code()
    {
        Check();

        void Check()
        {
            // Deliberately bypass publication only for this boundary sentinel: empty instructions
            // would make legacy topology analysis throw. No malformed graph is compiled or executed.
            var constructor = typeof(StraightLineFunction).GetConstructors(BindingFlags.Instance | BindingFlags.NonPublic)
                .Single(candidate => candidate.GetParameters().Length == 5);
            var sentinel = (StraightLineFunction)constructor.Invoke(
                [new FunctionId(1), FunctionSignature.Canonical,
                ImmutableList<StorageBinding>.Empty, ImmutableList<SelectedInstruction>.Empty,
                new FrameResourceUsage(7, 5)]);
            var frame = StraightLineVMAdapter.ToStackFrame(sentinel);
            frame.Instructions.Should().BeEmpty();
            frame.LocalsCount.Should().Be(7);
            frame.MaxStackUsage.Should().Be(5);
        }
    }

    [Fact]
    public void Unused_operations_are_evaluated_in_source_order()
    {
        Check();

        void Check()
        {
            var compiled = Compile(Graph(
                [
                    new Operation.Builtin(new(new(4)), "reverse", new(901)),
                    new Operation.Builtin(new(new(2)), "int_add", new(901)),
                    new Operation.Builtin(new(new(7)), "bit_and", new(901)),
                ],
                new Terminator.Return([new(901)])));
            var trace = Trace(compiled, ValidInput("int_add"));
            trace.Where(i => i.Instruction.Kind is StackInstructionKind.Reverse or StackInstructionKind.Int_Add_Generic or StackInstructionKind.Bit_And_Generic)
                .Select(i => i.Instruction.Kind).Should().Equal(
                    StackInstructionKind.Reverse, StackInstructionKind.Int_Add_Generic, StackInstructionKind.Bit_And_Generic);
            compiled.Storage.Should().HaveCount(4);
        }
    }

    [Theory]
    [InlineData("bit_shift_left")]
    [InlineData("bit_shift_right")]
    public void Unused_failing_builtins_are_not_eliminated_and_first_failure_wins(string name)
    {
        Check();

        void Check()
        {
            var negative = new LiteralValue.List([new LiteralValue.Blob([2, 8]), new LiteralValue.Blob([1])]);
            var huge = new LiteralValue.List([new LiteralValue.Blob([4, 255, 255, 255, 255, 255]), new LiteralValue.Blob([1])]);
            var compiled = Compile(Graph(
                [
                    new Operation.Literal(new(new(5)), negative),
                    new Operation.Literal(new(new(6)), huge),
                    new Operation.Builtin(new(new(7)), name, new(5)),
                    new Operation.Builtin(new(new(8)), name, new(6)),
                ], new Terminator.Return([new(901)])));
            Action directNegative = () => DirectBuiltin(name, negative);
            Action directHuge = () => DirectBuiltin(name, huge);
            directNegative.Should().Throw<IndexOutOfRangeException>();
            directHuge.Should().Throw<OverflowException>();
            Action evaluate = () => Execute(compiled, Empty);
            evaluate.Should().Throw<InvalidIntermediateCodeException>()
                .Which.InnerException.Should().BeOfType<IndexOutOfRangeException>();
            Action hugeOnly = () => Execute(Builtin(name), huge);
            hugeOnly.Should().Throw<InvalidIntermediateCodeException>()
                .Which.InnerException.Should().BeOfType<OverflowException>();
        }
    }

    private static StraightLineDiagnosticCode Decline(FunctionGraph graph) =>
        StraightLineCompiler.Compile(Validate(graph))
        .Should().BeOfType<Result<StraightLineDiagnostic, StraightLineFunction>.Err>().Subject.Value.Code;

    private static PineValue DirectBuiltin(string name, LiteralValue input)
    {
        return Evaluate();
        PineValue Evaluate() =>
            new DirectInterpreter(new PineVMParseCache(), null).EvaluateExpressionDefault(
                new Expression.Builtin(name, Expression.EnvironmentInstance), StraightLineVMAdapter.ToPineValue(input));
    }

    private static PineValue DirectProjection(LiteralValue input, EnvironmentPath path)
    {
        return Evaluate();
        PineValue Evaluate()
        {
            var expression = path.Indices.Aggregate(
                (Expression)Expression.EnvironmentInstance,
                (source, index) => new Expression.Builtin("head",
                    new Expression.Builtin("skip", new Expression.List(
                        [Expression.LitralInst(Core.CommonEncodings.IntegerEncoding.EncodeSignedInteger(index)), source]))));
            return new DirectInterpreter(new PineVMParseCache(), null)
                .EvaluateExpressionDefault(expression, StraightLineVMAdapter.ToPineValue(input));
        }
    }

    private static PineValue Execute(StraightLineFunction compiled, LiteralValue input) =>
        Evaluate(compiled, input).Value;

    private static ImmutableList<ExecutedStackInstruction> Trace(StraightLineFunction compiled, LiteralValue input) =>
        Evaluate(compiled, input).Trace;

    private static (PineValue Value, ImmutableList<ExecutedStackInstruction> Trace) Evaluate(
        StraightLineFunction compiled, LiteralValue input)
    {
        return Run();

        (PineValue Value, ImmutableList<ExecutedStackInstruction> Trace) Run()
        {
            var trace = ImmutableList.CreateBuilder<ExecutedStackInstruction>();
            var expression = Expression.EnvironmentInstance;
            var vm = Core.Interpreter.IntermediateVM.PineVM.CreateCustom(
                evalCache: null, evaluationConfigDefault: null, reportFunctionApplication: null,
                compilationEnvClasses: null, disableReductionInCompilation: true, selectPrecompiled: null,
                skipInlineForExpression: _ => false, enableTailRecursionOptimization: false, parseCache: null,
                precompiledLeaves: null, reportEnterPrecompiledLeaf: null, reportExitPrecompiledLeaf: null,
                optimizationParametersSerial: null, cacheFileStore: null,
                reportExecutedStackInstruction: (in ExecutedStackInstruction instruction) => trace.Add(instruction),
                expressionCompilationOverrides: new Dictionary<Expression, ExpressionCompilation>
                {
                    [expression] = new(StraightLineVMAdapter.ToStackFrame(compiled), []),
                });
            var report = vm.EvaluateExpressionOnCustomStack(expression, StraightLineVMAdapter.ToPineValue(input),
                new Core.Interpreter.IntermediateVM.PineVM.EvaluationConfig(null, null, null))
                .Extract(error => throw new InvalidOperationException(error.ToString()));
            return (report.ReturnValue.Evaluate(), trace.ToImmutable());
        }
    }

    private static void AssertStackDiscipline(StraightLineFunction compiled, LiteralValue input)
    {
        Check();

        void Check()
        {
            var trace = Trace(compiled, input);
            var initialized = ImmutableHashSet.Create(0);
            foreach (var executed in trace)
            {
                var instruction = executed.Instruction;
                if (instruction.Kind == StackInstructionKind.Local_Get)
                    initialized.Should().Contain(instruction.LocalIndex!.Value);
                if (instruction.Kind == StackInstructionKind.Local_Set)
                {
                    initialized = initialized.Add(instruction.LocalIndex!.Value);
                    executed.EvaluationStackDepth.Should().Be(1);
                }
                if (instruction.Kind == StackInstructionKind.Pop)
                    executed.EvaluationStackDepth.Should().Be(1);
                if (instruction.Kind == StackInstructionKind.Return)
                    executed.EvaluationStackDepth.Should().Be(1);
            }
            trace.Max(i => i.EvaluationStackDepth).Should().Be(compiled.Resources.MaxStackUsage);
            initialized.Count.Should().Be(compiled.Resources.LocalsCount);
        }
    }
}
