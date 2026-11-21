using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.DotNet;
using Pine.Core.Encodings;
using Pine.Core.Functional;
using Pine.Core.Internal;
using Pine.Core.PineLanguage;
using Pine.Core.Runtime.IntermediateVM;
using Pine.Core.Tests.DotNet;
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Runtime.CompilerServices;
using Xunit;

using VM = Pine.Core.Runtime.IntermediateVM.PineVM;

namespace Pine.Core.Tests.Runtime.IntermediateVM;

public class PineControlFlowGraphLoopTests
{
    [Fact]
    public void Natural_loop_forest_records_nested_headers_latches_preheaders_and_exits()
    {
        var graph =
            Graph(
                Jump(0, 1),
                Branch(1, 2, 6),
                Branch(2, 3, 4),
                Jump(3, 2),
                Jump(4, 5),
                Jump(5, 1),
                Return(6));

        var analysis = graph.AnalyzeLoops();
        analysis.IrreducibleCycles.Should().BeEmpty();
        var outer = analysis.Loops.Single(loop => loop.Header.Value == 1);
        var inner = analysis.Loops.Single(loop => loop.Header.Value == 2);
        outer.Blocks.Select(block => block.Value).Should().Equal(1, 2, 3, 4, 5);
        outer.ParentHeader.Should().BeNull();
        outer.Preheader.Should().Be(new PineBlockId(0));
        outer.Backedges.Should().Equal(new PineControlFlowEdge(new(5), new(1)));
        outer.Exits.Should().Equal(new PineControlFlowEdge(new(1), new(6)));
        inner.ParentHeader.Should().Be(outer.Header);
        inner.Preheader.Should().BeNull();
        inner.Blocks.Select(block => block.Value).Should().Equal(2, 3);
        inner.Backedges.Should().Equal(new PineControlFlowEdge(new(3), new(2)));
        inner.Exits.Should().Equal(new PineControlFlowEdge(new(2), new(4)));
        analysis.Dominates(new(1), new(5)).Should().BeTrue();
        analysis.Dominates(new(2), new(4)).Should().BeTrue();
        analysis.Dominates(new(2), new(6)).Should().BeFalse();
        AssertSnapshot("nested-loops.txt", graph.Render(_ => "true"));
    }

    [Fact]
    public void Natural_loops_keep_multiple_latches_and_infinite_or_single_block_cycles()
    {
        var multiple = Graph(Jump(0, 1), Branch(1, 2, 4), Branch(2, 3, 1), Jump(3, 1), Return(4));
        var loop = multiple.AnalyzeLoops().Loops.Single();
        loop.Backedges.Select(edge => edge.Source.Value).Should().Equal(2, 3);
        loop.Blocks.Select(block => block.Value).Should().Equal(1, 2, 3);
        var self = Graph(Jump(0, 0)).AnalyzeLoops();
        self.HasCycles.Should().BeTrue();
        self.Loops.Single().Blocks.Should().Equal(new PineBlockId(0));
        self.Loops.Single().Exits.Should().BeEmpty();
        self.Loops.Single().Preheader.Should().BeNull();
    }

    [Fact]
    public void Irreducible_cycles_are_distinct_from_natural_loops_even_when_nested()
    {
        var irreducible = Graph(Branch(0, 1, 2), Jump(1, 2), Branch(2, 1, 3), Return(3));
        var analysis = irreducible.AnalyzeLoops();
        analysis.Loops.Should().BeEmpty();
        analysis.HasCycles.Should().BeTrue();
        analysis.IrreducibleCycles.Single().Blocks.Select(block => block.Value).Should().Equal(1, 2);
        analysis.IrreducibleCycles.Single().Entries.Select(block => block.Value).Should().Equal(1, 2);
        AssertSnapshot("irreducible-cycle.txt", irreducible.Render(_ => "true"));

        var nested =
            Graph(
                Jump(0, 1),
                Branch(1, 2, 5),
                Branch(2, 3, 4),
                Jump(3, 4),
                Branch(4, 3, 1),
                Return(5));

        var nestedAnalysis = nested.AnalyzeLoops();
        nestedAnalysis.Loops.Single().Header.Should().Be(new PineBlockId(1));
        nestedAnalysis.IrreducibleCycles.Single().Blocks.Select(block => block.Value).Should().Equal(3, 4);
        nestedAnalysis.IrreducibleCycles.Single().Entries.Select(block => block.Value).Should().Equal(3, 4);
    }

    [Fact]
    public void Unreachable_cycles_are_not_loops_and_deep_graph_analysis_uses_no_recursive_traversal()
    {
        var unreachable = Graph(Return(0), Jump(1, 1)).AnalyzeLoops();
        unreachable.HasCycles.Should().BeFalse();
        unreachable.Dominates(new(1), new(1)).Should().BeFalse();

        var blocks =
            Enumerable.Range(0, 20000)
            .Select(index => index == 19999 ? Return(index) : Jump(index, index + 1)).ToArray();

        var analysis = Graph(blocks).AnalyzeLoops();
        analysis.Loops.Should().BeEmpty();
        analysis.Dominates(new(0), new(19999)).Should().BeTrue();
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void Invariant_header_bindings_reuse_values_without_first_iteration_overhead(bool constructedList)
    {
        var graph = InvariantLoop(constructedList);
        var hoisted = graph.HoistLoopInvariantStores(2);
        hoisted.Should().NotBeSameAs(graph);
        hoisted.HoistLoopInvariantStores(2).Should().BeSameAs(hoisted);
        var loop = hoisted.AnalyzeLoops().Loops.Single();
        loop.Preheader.Should().Be(hoisted.Entry);

        hoisted.Blocks[loop.Header.Value].Operations.Should().NotContain(
            operation => operation.Instruction.Kind == StackInstructionKind.Local_Set);

        var environment = PineValue.List([Number(20), PineValue.List([Number(10), Number(20), Number(30)])]);
        var original = Evaluate(graph, environment);
        var optimized = Evaluate(hoisted, environment);
        optimized.ReturnValue.Evaluate().Should().Be(original.ReturnValue.Evaluate());
        optimized.InstructionCount.Should().BeLessThan(original.InstructionCount);
        optimized.InvocationCount.Should().Be(original.InvocationCount);
        optimized.LoopIterationCount.Should().Be(original.LoopIterationCount);

        if (constructedList)
        {
            original.BuildListCount.Should().Be(21);
            optimized.BuildListCount.Should().Be(1);
        }

        foreach (var count in new[] { -1, 0, 1 })
        {
            var input = PineValue.List([Number(count), PineValue.List([Number(10), Number(20), Number(30)])]);
            var before = Evaluate(graph, input);
            var after = Evaluate(hoisted, input);
            after.ReturnValue.Evaluate().Should().Be(before.ReturnValue.Evaluate());
            after.InstructionCount.Should().BeLessThanOrEqualTo(before.InstructionCount);
        }

        AssertSnapshot(
            constructedList ? "invariant-list-counters.txt" : "invariant-length-counters.txt",
            "before\n" + EvalWorkCountersFormatting.FormatCounts(original.Counters) +
            "\nafter\n" + EvalWorkCountersFormatting.FormatCounts(optimized.Counters) + "\n");

        AssertSnapshot(
            constructedList ? "invariant-list-cfg.txt" : "invariant-length-cfg.txt",
            hoisted.Render(
                value =>
                IntegerEncoding.ParseSignedIntegerRelaxed(value).IsOkOrNullable()?.ToString() ?? value.ToString()));

        using var assembly = CompileGraph(hoisted);

        assembly.Evaluate(PineValueInProcess.Create(environment))!.Evaluate().Should().Be(
            optimized.ReturnValue.Evaluate());
    }

    [Fact]
    public void Scheduler_preserves_loop_list_scalar_replacement_and_hoists_immutable_bindings()
    {
        var graph = InvariantLoop(constructedList: true);
        var optimized = graph.OptimizeForExecution(2);
        var input = PineValue.List([Number(30), PineValue.List([Number(1), Number(2)])]);
        var before = Evaluate(graph, input);
        var after = Evaluate(optimized, input);
        after.ReturnValue.Evaluate().Should().Be(before.ReturnValue.Evaluate());
        after.BuildListCount.Should().BeLessThan(before.BuildListCount);
        after.InstructionCount.Should().BeLessThan(before.InstructionCount);
        var returnBlock = graph.Blocks.Single(block => block.Terminator is PineControlFlowTerminator.Return);

        var projection =
            new PineControlFlowOperation(StackInstruction.Length, [returnBlock.Parameters[^1]], [new(1000)]);

        var projected =
            graph with
            {
                Blocks =
                graph.Blocks.SetItem(
                    returnBlock.Id.Value,
                    returnBlock with { Operations = returnBlock.Operations.Add(projection) })
            };

        var scalarized = projected.OptimizeForExecution(2);
        Evaluate(scalarized, input).ReturnValue.Evaluate().Should().Be(Number(2));

        scalarized.LowerToStackInstructions().Should().NotContain(
            instruction => instruction.Kind == StackInstructionKind.Build_List);
    }

    [Fact]
    public void Header_binding_cannot_move_a_write_ahead_of_a_read_of_the_old_value()
    {
        var prefix =
            Ops(
                StackInstruction.Local_Get(1),
                StackInstruction.Local_Set([2], popCount: 1),
                StackInstruction.Local_Set_Literal(1, Number(5)));

        var graph = PineControlFlowGraph.FromFragment(prefix.Append(LoopBody(returnLocal: 2)));
        graph.HoistLoopInvariantStores(2).Should().BeSameAs(graph);
        Evaluate(graph, PineValue.List([Number(0), Number(7)])).ReturnValue.Evaluate().Should().Be(Number(7));
    }

    [Fact]
    public void Tail_argument_self_copies_preserve_invariance_but_writes_between_load_and_store_invalidate_it()
    {
        var graph = InvariantLoop(constructedList: false);

        var backedge =
            graph.Blocks.Single(
                block => block.Terminator is PineControlFlowTerminator.Jump jump && jump.Target == graph.Entry);

        var copy =
            PineControlFlowGraph.FromFragment(Ops(
            StackInstruction.Local_Get(1), StackInstruction.Local_Set([1], popCount: 1)), returnValueCount: 0).Blocks[0].Operations;

        var unchanged =
            graph with
            {
                Blocks =
                graph.Blocks.SetItem(
                    backedge.Id.Value,
                    backedge with { Operations = backedge.Operations.AddRange(copy) })
            };

        var hoisted = unchanged.HoistLoopInvariantStores(2);
        hoisted.Should().NotBeSameAs(unchanged);
        var input = PineValue.List([Number(4), PineValue.List([Number(7), Number(8)])]);
        Evaluate(hoisted, input).ReturnValue.Evaluate().Should().Be(Evaluate(unchanged, input).ReturnValue.Evaluate());

        var changedCopy = copy.Insert(1, new(StackInstruction.Local_Set_Literal(1, Number(7)), [], []));

        var changing =
            graph with
            {
                Blocks =
                graph.Blocks.SetItem(
                    backedge.Id.Value,
                    backedge with { Operations = backedge.Operations.AddRange(changedCopy) })
            };

        changing.HoistLoopInvariantStores(2).Should().BeSameAs(changing);
    }

    [Fact]
    public void Peeked_invariant_binding_becomes_a_loop_parameter_without_recomputing_the_initial_value()
    {
        var graph =
            PineControlFlowGraph.FromFragment(
                Ops(
                    StackInstruction.Local_Get(1),
                    StackInstruction.Length,
                    StackInstruction.Local_Set(2),
                    StackInstruction.Int_Less_Than_Or_Equal_Const(0))
                .Append(
                    new PineControlFlowNode.Conditional(
                        PineKernelValues.TrueValue,
                        LoopBody(2),
                        Ops(StackInstruction.Local_Get(2)))));

        var hoisted = graph.HoistLoopInvariantStores(2);
        var loop = hoisted.AnalyzeLoops().Loops.Single();
        hoisted.Blocks[loop.Header.Value].Parameters.Should().ContainSingle();

        hoisted.Blocks[hoisted.Entry.Value].Terminator.Should().BeOfType<PineControlFlowTerminator.Jump>()
            .Which.Arguments.Should().NotEqual(hoisted.Blocks[loop.Header.Value].Parameters);

        var definitions =
            hoisted.Blocks
            .SelectMany(
                block => block.Parameters.Concat(block.Operations.SelectMany(item => item.Results)))
            .ToArray();

        definitions.Distinct().Count().Should().Be(definitions.Length);

        foreach (var count in new[] { -1, 0, 1, 20 })
        {
            var input = PineValue.List([Number(count), PineValue.List([Number(7), Number(8)])]);
            var before = Evaluate(graph, input);
            var after = Evaluate(hoisted, input);
            after.ReturnValue.Evaluate().Should().Be(before.ReturnValue.Evaluate());
            after.InstructionCount.Should().BeLessThanOrEqualTo(before.InstructionCount);
            after.LoopIterationCount.Should().Be(before.LoopIterationCount);
        }

        var environment = PineValue.List([Number(20), PineValue.List([Number(7), Number(8)])]);
        var original = Evaluate(graph, environment);
        var optimized = Evaluate(hoisted, environment);
        optimized.InstructionCount.Should().BeLessThan(original.InstructionCount);
        AssertSnapshot("carried-invariant-cfg.txt", hoisted.Render());

        AssertSnapshot(
            "carried-invariant-counters.txt",
            "before\n" + EvalWorkCountersFormatting.FormatCounts(original.Counters) +
            "\nafter\n" + EvalWorkCountersFormatting.FormatCounts(optimized.Counters) + "\n");

        using var assembly = CompileGraph(hoisted);

        assembly.Evaluate(PineValueInProcess.Create(environment))!.Evaluate().Should().Be(
            original.ReturnValue.Evaluate());
    }

    [Fact]
    public void Duplicate_cache_destinations_do_not_supply_distinct_loop_parameters()
    {
        var graph =
            PineControlFlowGraph.FromFragment(
                Ops(
                    StackInstruction.Local_Get(1),
                    StackInstruction.Length,
                    StackInstruction.Push_Literal(Number(9)),
                    StackInstruction.Local_Set([2, 2]),
                    StackInstruction.PopMultiple(2))
                .Append(LoopBody(2)));

        var hoisted = graph.HoistLoopInvariantStores(2);
        hoisted.Blocks[hoisted.AnalyzeLoops().Loops.Single().Header.Value].Parameters.Should().BeEmpty();
        var input = PineValue.List([Number(4), PineValue.List([Number(7), Number(8)])]);
        Evaluate(hoisted, input).ReturnValue.Evaluate().Should().Be(Number(2));
    }

    [Fact]
    public void Loop_variant_reads_uninitialized_reads_and_trapping_primitives_are_not_hoisted()
    {
        foreach (var prefix in new[]
        {
            Ops(StackInstruction.Local_Get(0), StackInstruction.Length, StackInstruction.Local_Set([2], popCount: 1)),
            Ops(StackInstruction.Local_Get(9), StackInstruction.Length, StackInstruction.Local_Set([2], popCount: 1)),
            Ops(
                StackInstruction.Local_Get(1),
                StackInstruction.Local_Get(1),
                StackInstruction.Skip_Binary,
                StackInstruction.Local_Set([2], popCount: 1))
        })
        {
            var graph = PineControlFlowGraph.FromFragment(prefix.Append(LoopBody(returnLocal: 2)));
            graph.HoistLoopInvariantStores(2).Should().BeSameAs(graph);
        }
    }

    [Fact]
    public void A_binding_used_only_in_the_loop_body_is_not_speculatively_evaluated_on_zero_trips()
    {
        var graph =
            PineControlFlowGraph.FromFragment(
                Ops(StackInstruction.Local_Get(0), StackInstruction.Int_Less_Than_Or_Equal_Const(0))
                .Append(
                    new PineControlFlowNode.Conditional(
                        PineKernelValues.TrueValue,
                        Ops(
                            StackInstruction.Local_Get(1),
                            StackInstruction.Length,
                            StackInstruction.Local_Set([2], popCount: 1),
                            new(StackInstructionKind.Local_Int_Add_Const, IntegerLiteral: -1, LocalIndices: [0]))
                        .Append(PineControlFlowNode.JumpToEntry.Instance),
                        Ops(StackInstruction.Local_Get(1)))));

        graph.HoistLoopInvariantStores(2).Should().BeSameAs(graph);
        Evaluate(graph, PineValue.List([Number(0), Number(7)])).ReturnValue.Evaluate().Should().Be(Number(7));
    }

    private static PineControlFlowGraph InvariantLoop(bool constructedList)
    {
        var prefix =
            constructedList
            ?
            Ops(
                StackInstruction.Local_Get(1),
                StackInstruction.Local_Get(1),
                StackInstruction.Build_List(2),
                StackInstruction.Local_Set([2], popCount: 1))
            :
            Ops(StackInstruction.Local_Get(1), StackInstruction.Length, StackInstruction.Local_Set([2], popCount: 1));

        return PineControlFlowGraph.FromFragment(prefix.Append(LoopBody(returnLocal: 2)));
    }

    private static PineControlFlowFragment LoopBody(int returnLocal) =>
        Ops(StackInstruction.Local_Get(0), StackInstruction.Int_Less_Than_Or_Equal_Const(0))
        .Append(
            new PineControlFlowNode.Conditional(
                PineKernelValues.TrueValue,
                Ops(
                    new StackInstruction(
                        StackInstructionKind.Local_Int_Add_Const,
                        IntegerLiteral: -1,
                        LocalIndices:
                        [
                            0
                        ]))
                .Append(PineControlFlowNode.JumpToEntry.Instance),
                Ops(StackInstruction.Local_Get(returnLocal))));

    private static PineControlFlowFragment Ops(params StackInstruction[] instructions) =>
        PineControlFlowFragment.FromOperations(instructions);

    private static PineValue Number(int number) => IntegerEncoding.EncodeSignedInteger(number);

    private static PineControlFlowGraph Graph(params PineBasicBlock[] blocks) => new(new(0), [.. blocks]);

    private static PineBasicBlock Jump(int source, int target) =>
        new(new(source), [], [], new PineControlFlowTerminator.Jump(new(target), [], IsFallThrough: false));

    private static PineBasicBlock Branch(int source, int fallThrough, int branch) =>
        new(
            new(source),
            [],
            [new(StackInstruction.Push_Literal(PineKernelValues.TrueValue), [], [new(source)])],
            new PineControlFlowTerminator.ConditionalJump(
                new(fallThrough),
                new(branch),
                [],
                [],
                PineKernelValues.TrueValue));

    private static PineBasicBlock Return(int block) =>
        new(
            new(block),
            [],
            [new(StackInstruction.Push_Literal(Number(0)), [], [new(block)])],
            PineControlFlowTerminator.Return.Instance);

    private static EvaluationReport Evaluate(PineControlFlowGraph graph, PineValue environment)
    {
        var expression = Expression.ListInst([Expression.EnvironmentInstance]);
        var parameters = StaticFunctionInterface.FromPathsSorted([[0], [1]]);
        var retained = Core.Runtime.InternalExpression.ExpressionConversion.FromCanonicalCached(expression);

        var vm =
            VM.CreateCustom(
                evalCache: null,
                evaluationConfigDefault: VM.EvaluationConfig.Unbounded,
                reportFunctionApplication: null,
                compilationEnvClasses: null,
                disableReductionInCompilation: true,
                selectPrecompiled: null,
                skipInlineForExpression: _ => true,
                enableTailRecursionOptimization: false,
                parseCache: null,
                precompiledLeaves: null,
                reportEnterPrecompiledLeaf: null,
                reportExitPrecompiledLeaf: null,
                optimizationParametersSerial: null,
                cacheFileStore: null,
                expressionCompilationOverrides:
                new Dictionary<Core.Runtime.InternalExpression.Expression, ExpressionCompilation>
                {
                    [retained] = new(new StackFrameInstructions(parameters, graph.LowerToStackInstructions()), [])
                });

        return
            vm.EvaluateExpressionOnCustomStack(expression, environment, VM.EvaluationConfig.Unbounded)
            .Extract(error => throw new Exception(error.ToString()));
    }

    private static PineCSharpAssembly CompileGraph(PineControlFlowGraph graph)
    {
        var retained =
            Core.Runtime.InternalExpression.ExpressionConversion.FromCanonicalCached(Expression.EnvironmentInstance);

        var identity = new PineCSharpFunction(0, retained, null);
        var parameters = StaticFunctionInterface.FromPathsSorted([[0], [1]]);

        var prepared =
            new InternalExpressionCompilation.StaticFunction(
                retained,
                parameters,
                ReturnValueLayout.Canonical,
                () => graph,
                new(() => new StackFrameInstructions(parameters, graph.LowerToStackInstructions())));

        var function = new PineCSharpCompiler.Function(identity, prepared, new(identity, null, null, 0));
        var compilation = new PineCSharpEmitter([function], [function], new(EnableCancellation: false)).Emit();
        return PineCSharpAssembly.Load(compilation).Extract(errors => throw new Exception(string.Join("\n", errors)));
    }

    private static void AssertSnapshot(string name, string value, [CallerFilePath] string file = "")
    {
        var directory =
            Path.GetFullPath(
                Path.Combine(
                    Path.GetDirectoryName(file)!,
                    "..",
                    "..",
                    "TestData",
                    "ControlFlowGraphLoops",
                    Path.GetFileNameWithoutExtension(name),
                    "expect"));

        var mismatches =
            PineCSharpScenarios.CompareSnapshots(
                directory,
                new Dictionary<string, string> { [name] = value });

        mismatches.Should().BeEmpty(string.Join("\n", mismatches));
    }
}
