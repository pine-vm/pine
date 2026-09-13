using AwesomeAssertions;
using Pine.Core.Interpreter.IntermediateVM;
using Pine.Core.Interpreter.IntermediateVM.Semantic;
using System;
using System.Collections.Immutable;
using System.Globalization;
using System.Linq;
using Xunit;
using ValueType = Pine.Core.Interpreter.IntermediateVM.Semantic.ValueType;

namespace Pine.Core.Tests.Interpreter.IntermediateVM;

public class SemanticGraphTests
{
    [Fact]
    public void Literal_graph_has_a_lossless_snapshot_and_explicit_return_operand()
    {
        var graph = LiteralGraph();

        GraphRendering.Render(graph).Should().Be(
            """
            function f7 () -> (pine) entry b90
            block b90():
              v12:pine = list(blob(), list(), blob(0001feff))
              return (v12)

            """);
        ((Terminator.Return)graph.Blocks[new(90)].Terminator).Values.Should().Equal(new PineVirtualValueId(12));
    }

    [Fact]
    public void Branch_graph_has_explicit_inputs_results_and_separate_edge_arguments()
    {
        var graph = BranchGraph();

        GraphRendering.Render(graph).Should().Be(
            """
            function f8 ([]:pine) -> (pine) entry b80
            block b2(v20:pine, v21:pine):
              return (v20)
            block b80(v1:pine):
              v2:pine = project v1[0,2]
              v3:pine = blob(01)
              v4:pine = make-list (v2, v3)
              v5:pine = builtin "equal" v4
              branch v5 == blob(04) then b2(v2, v3) else b2(v3, v2)

            """);

        var branch = (Terminator.Branch)graph.Blocks[new(80)].Terminator;
        branch.IfEqual.Target.Should().Be(branch.IfNotEqual.Target);
        branch.IfEqual.Arguments.Should().Equal(new PineVirtualValueId(2), new PineVirtualValueId(3));
        branch.IfNotEqual.Arguments.Should().Equal(new PineVirtualValueId(3), new PineVirtualValueId(2));
        branch.IfEqual.Should().NotBe(branch.IfNotEqual);
    }

    [Fact]
    public void Switch_cases_and_default_keep_different_arguments_to_the_same_target()
    {
        var graph = SwitchGraph();

        GraphRendering.Render(graph).Should().Be(
            """
            function f8 ([]:pine) -> (pine) entry b80
            block b2(v20:pine, v21:pine):
              return (v20)
            block b80(v1:pine):
              v2:pine = project v1[0,2]
              v3:pine = blob(01)
              v4:pine = make-list (v2, v3)
              v5:pine = builtin "equal" v4
              switch v5 {blob(04) => b2(v2, v3); list() => b2(v3, v2)} default b2(v2, v2)

            """);
        var selection = (Terminator.Switch)graph.Blocks[new(80)].Terminator;
        selection.Cases.Select(@case => @case.Edge.Target).Should().OnlyContain(id => id == new PineBlockId(2));
        selection.Cases[0].Edge.Should().NotBe(selection.Cases[1].Edge);
        selection.Default.Should().NotBe(selection.Cases[0].Edge).And.NotBe(selection.Cases[1].Edge);
    }

    [Theory]
    [InlineData(false, false)]
    [InlineData(false, true)]
    [InlineData(true, false)]
    [InlineData(true, true)]
    public void Calls_have_explicit_contracts_and_only_non_tail_calls_have_continuations(bool known, bool tail)
    {
        var graph = CallGraph(known, tail);
        var target = known ? "known f19" : "dynamic v2";
        var transfer =
            tail
            ? "tail-invoke c6 " + target + "(v1) ([]:pine) -> (pine)"
            : "invoke c6 " + target + "(v1) ([]:pine) -> (pine) continue b5(caller v1, returned[0])";
        var continuationBlock =
            tail
            ? ""
            : "block b5(v30:pine, v31:pine):\n  v32:pine = make-list (v30, v31)\n  return (v32)\n";

        GraphRendering.Render(graph).Should().Be(
            "function f19 ([]:pine) -> (pine) entry b100\n" +
            continuationBlock +
            "block b100(v1:pine):\n  v2:pine = blob(ff)\n  " + transfer + "\n");

        if (!tail)
        {
            var invoke = (Terminator.Invoke)graph.Blocks[new(100)].Terminator;
            invoke.Continuation.Bindings.Should().Equal(
                new ContinuationBinding.CallerValue(new(1)), new ContinuationBinding.ReturnedResult(0));
        }
    }

    [Fact]
    public void Known_call_and_function_signatures_preserve_projected_paths_and_multiple_return_slots()
    {
        var signature = new FunctionSignature(
            [new(new([0, 2])), new(new([1]))], [ValueType.PineValue, ValueType.PineValue]);
        var graph = new FunctionGraph(
            new(41), signature, new(9),
            ImmutableDictionary<PineBlockId, BasicBlock>.Empty
            .Add(new(9), new(new(9), [new(new(1)), new(new(2))], [],
                new Terminator.Invoke(
                    new(new(0), new CallTarget.Known(new(42)), signature, [new(2), new(1)]),
                    new(new(3),
                        [new ContinuationBinding.ReturnedResult(1),
                         new ContinuationBinding.CallerValue(new(1)),
                         new ContinuationBinding.ReturnedResult(0)]))))
            .Add(new(3), new(new(3), [new(new(10)), new(new(11)), new(new(12))], [],
                new Terminator.Return([new(12), new(10)]))));

        GraphRendering.Render(graph).Should().Be(
            """
            function f41 ([0,2]:pine, [1]:pine) -> (pine, pine) entry b9
            block b3(v10:pine, v11:pine, v12:pine):
              return (v12, v10)
            block b9(v1:pine, v2:pine):
              invoke c0 known f42(v2, v1) ([0,2]:pine, [1]:pine) -> (pine, pine) continue b3(returned[1], caller v1, returned[0])

            """);
    }

    [Fact]
    public void Nested_and_consecutive_loops_are_ordinary_parameterized_cycles()
    {
        var graph = LoopGraph();

        GraphRendering.Render(graph).Should().Be(
            """
            function f50 ([]:pine) -> (pine) entry b90
            block b3(v3:pine):
              branch v3 == blob(00) then b7(v3) else b60(v3)
            block b7(v7:pine):
              jump b3(v7)
            block b15(v15:pine):
              return (v15)
            block b20(v20:pine):
              branch v20 == blob(00) then b20(v20) else b15(v20)
            block b60(v60:pine):
              branch v60 == blob(00) then b3(v60) else b20(v60)
            block b90(v90:pine):
              jump b60(v90)

            """);
        // The entry adapter is outside both the outer cycle (60 -> 3 -> 60)
        // and its inner cycle (3 -> 7 -> 3); the independent cycle at 20 follows them.
        graph.Blocks.Count.Should().Be(6);
        graph.Entry.Should().NotBe(new PineBlockId(60));
        var inner = (Terminator.Branch)graph.Blocks[new(3)].Terminator;
        inner.IfEqual.Target.Should().Be(new PineBlockId(7));
        inner.IfNotEqual.Target.Should().Be(new PineBlockId(60));
    }

    [Fact]
    public void Irreducible_cycles_and_simultaneous_parameter_swaps_are_representable()
    {
        var graph = new FunctionGraph(
            new(1), new([new(new([0])), new(new([1]))], [ValueType.PineValue]), new(99),
            ImmutableDictionary<PineBlockId, BasicBlock>.Empty
            .Add(new(99), new(new(99), [new(new(1)), new(new(2))], [],
                new Terminator.Branch(new(1), new LiteralValue.List([]),
                    new(new(8), [new(1), new(2)]), new(new(4), [new(2), new(1)]))))
            .Add(new(8), new(new(8), [new(new(3)), new(new(4))], [],
                new Terminator.Jump(new(new(4), [new(4), new(3)]))))
            .Add(new(4), new(new(4), [new(new(5)), new(new(6))], [],
                new Terminator.Jump(new(new(8), [new(6), new(5)])))));

        GraphRendering.Render(graph).Should().Contain("jump b4(v4, v3)").And.Contain("jump b8(v6, v5)");
        graph.Blocks[new(99)].Terminator.Should().BeOfType<Terminator.Branch>();
    }

    [Theory]
    [InlineData(0)]
    [InlineData(1)]
    [InlineData(2)]
    [InlineData(3)]
    [InlineData(4)]
    [InlineData(5)]
    [InlineData(6)]
    [InlineData(7)]
    public void Independently_allocated_nested_graphs_have_structural_equality_hashes_and_rendering(int fixture)
    {
        var first = Fixture(fixture);
        var second = Fixture(fixture);
        var reordered = new FunctionGraph(
            second.Id, second.Signature, second.Entry,
            second.Blocks.OrderByDescending(pair => pair.Key.Value)
            .Aggregate(ImmutableDictionary<PineBlockId, BasicBlock>.Empty,
                (blocks, pair) => blocks.Add(pair.Key, pair.Value)));

        ReferenceEquals(first, second).Should().BeFalse();
        first.Equals(second).Should().BeTrue();
        (first == second).Should().BeTrue();
        first.GetHashCode().Should().Be(second.GetHashCode());
        first.Equals(reordered).Should().BeTrue();
        first.GetHashCode().Should().Be(reordered.GetHashCode());
        GraphRendering.Render(first).Should().Be(GraphRendering.Render(reordered));
    }

    [Fact]
    public void Equality_observes_nested_collection_contents_not_just_shape()
    {
        var graph = BranchGraph();
        var block = graph.Blocks[new(80)];
        var changedPath = (Operation.Project)block.Operations[0] with { Path = new([0, 3]) };
        var changedBlock = block with { Operations = block.Operations.SetItem(0, changedPath) };
        var different = new FunctionGraph(graph.Id, graph.Signature, graph.Entry, graph.Blocks.SetItem(block.Id, changedBlock));

        graph.Should().NotBe(different);
        new EnvironmentPath([0, 2]).Should().NotBe(new EnvironmentPath([2, 0]));
        new LiteralValue.Blob([0, 1]).Should().NotBe(new LiteralValue.Blob([1, 0]));
        new LiteralValue.List([new LiteralValue.Blob([0])])
            .Should().NotBe(new LiteralValue.List([new LiteralValue.Blob([1])]));
        new Terminator.Return([new(1), new(2)]).Should().NotBe(new Terminator.Return([new(2), new(1)]));
        new InvokeContinuation(new(1), [new ContinuationBinding.CallerValue(new(0))])
            .Should().NotBe(new InvokeContinuation(new(1), [new ContinuationBinding.ReturnedResult(0)]));
        graph.Signature.Should().NotBe(graph.Signature with { Parameters = [new(new([0]))] });
    }

    [Fact]
    public void Input_builders_and_nested_sources_cannot_mutate_published_graphs()
    {
        var rawBytes = new byte[] { 0, 1, 2, 3 };
        var rawPath = new[] { 4, 5 };
        var bytes = rawBytes.ToImmutableList().ToBuilder();
        var path = rawPath.ToImmutableList().ToBuilder();
        var literals = ImmutableList.CreateBuilder<LiteralValue>();
        literals.Add(new LiteralValue.Blob(bytes.ToImmutable()));
        var parameters = ImmutableList.CreateBuilder<FunctionParameter>();
        parameters.Add(new(new(path.ToImmutable())));
        var operations = ImmutableList.CreateBuilder<Operation>();
        operations.Add(new Operation.Literal(new(new(2)), new LiteralValue.List(literals.ToImmutable())));
        operations.Add(new Operation.Project(new(new(3)), new(1), new(path.ToImmutable())));
        var arguments = ImmutableList.CreateBuilder<PineVirtualValueId>();
        arguments.Add(new(3));
        var blocks = ImmutableDictionary.CreateBuilder<PineBlockId, BasicBlock>();
        blocks.Add(new(10), new(new(10), [new(new(1))], operations.ToImmutable(),
            new Terminator.Jump(new(new(20), arguments.ToImmutable()))));
        blocks.Add(new(20), new(new(20), [new(new(4))], [], new Terminator.Return([new(4)])));
        var graph = new FunctionGraph(new(1), new(parameters.ToImmutable(), [ValueType.PineValue]),
            new(10), blocks.ToImmutable());
        var before = GraphRendering.Render(graph);
        var hash = graph.GetHashCode();

        rawBytes[0] = 255;
        rawPath[0] = 99;
        bytes[1] = 255;
        path[1] = 99;
        literals.Clear();
        parameters.Clear();
        operations.Clear();
        arguments.Clear();
        blocks.Clear();

        GraphRendering.Render(graph).Should().Be(before);
        graph.GetHashCode().Should().Be(hash);
        graph.Signature.Parameters[0].Path.Indices.Should().Equal(4, 5);
        var literal = (LiteralValue.List)((Operation.Literal)graph.Blocks[new(10)].Operations[0]).Value;
        ((LiteralValue.Blob)literal.Items[0]).Bytes.Should().Equal(0, 1, 2, 3);
        graph.Blocks.Count.Should().Be(2);
        graph.Blocks.Clear().Should().BeEmpty();
        graph.Blocks.Count.Should().Be(2);
        graph.Signature.Parameters[0].Path.Indices.SetItem(0, 9).Should().Equal(9, 5);
        graph.Signature.Parameters[0].Path.Indices.Should().Equal(4, 5);
    }

    [Fact]
    public void Rendering_is_culture_invariant_escapes_names_and_does_not_truncate_literals()
    {
        var originalCulture = CultureInfo.CurrentCulture;
        var culture = (CultureInfo)CultureInfo.InvariantCulture.Clone();
        culture.NumberFormat.NegativeSign = "~";
        var bytes = Enumerable.Range(0, 256).Select(index => (byte)index).ToImmutableList();
        var literal = new LiteralValue.Blob(bytes);
        var graph = new FunctionGraph(new(-7), new([], [ValueType.PineValue]), new(-3),
            ImmutableDictionary<PineBlockId, BasicBlock>.Empty.Add(new(-3),
                new(new(-3), [], [
                    new Operation.Literal(new(new(-2)), literal),
                    new Operation.Builtin(new(new(-1)), "a\"\nb", new(-2))],
                    new Terminator.Return([new(-1)]))));
        var invariant = GraphRendering.Render(graph);

        try
        {
            CultureInfo.CurrentCulture = culture;
            GraphRendering.Render(graph).Should().Be(invariant);
        }
        finally
        {
            CultureInfo.CurrentCulture = originalCulture;
        }

        invariant.Should().StartWith("function f-7").And.Contain("v-2").And.Contain("a\\u0022\\nb");
        GraphRendering.RenderLiteral(literal).Should().HaveLength(6 + 512);
        GraphRendering.RenderLiteral(new LiteralValue.Blob([])).Should().Be("blob()");
        GraphRendering.RenderLiteral(new LiteralValue.List([])).Should().Be("list()");
    }

    [Fact]
    public void Construction_allocations_are_repeatable_independent_and_function_local()
    {
        var empty = GraphBuildState.Empty;
        var block = empty.AllocateBlock();
        var value = block.State.AllocateValue();
        var call = value.State.AllocateCallSite();

        block.Should().Be(empty.AllocateBlock());
        block.Id.Should().Be(new PineBlockId(0));
        value.Id.Should().Be(new PineVirtualValueId(0));
        call.Id.Should().Be(new CallSiteId(0));
        call.State.AllocateBlock().Id.Should().Be(new PineBlockId(1));
        call.State.AllocateValue().Id.Should().Be(new PineVirtualValueId(1));
        call.State.AllocateCallSite().Id.Should().Be(new CallSiteId(1));
        empty.NextBlockId.Should().Be(0);
        empty.NextValueId.Should().Be(0);
        empty.NextCallSiteId.Should().Be(0);
        empty.CompletedBlocks.Should().BeEmpty();
        empty.Fragment.Should().BeNull();
    }

    [Fact]
    public void Construction_fragments_and_completed_states_are_persistent_structural_data_not_validated_graphs()
    {
        var allocated = GraphBuildState.Empty.AllocateBlock();
        var value = allocated.State.AllocateValue();
        var opened = value.State.OpenBlock(allocated.Id, []);
        var appended = opened.AppendOperation(new Operation.Literal(new(value.Id), new LiteralValue.Blob([9])));
        var completed = appended.CompleteBlock(new Terminator.Return([value.Id]));
        var independentlyCompleted = value.State.OpenBlock(allocated.Id, [])
            .AppendOperation(new Operation.Literal(new(value.Id), new LiteralValue.Blob([9])))
            .CompleteBlock(new Terminator.Return([value.Id]));

        value.State.Fragment.Should().BeNull();
        opened.Fragment!.Operations.Should().BeEmpty();
        appended.Fragment!.Operations.Should().HaveCount(1);
        appended.CompletedBlocks.Should().BeEmpty();
        completed.Fragment.Should().BeNull();
        completed.CompletedBlocks.Should().HaveCount(1);
        completed.Should().Be(independentlyCompleted);
        completed.GetHashCode().Should().Be(independentlyCompleted.GetHashCode());
        opened.Should().Be(value.State.OpenBlock(allocated.Id, []));
        appended.Should().Be(value.State.OpenBlock(allocated.Id, [])
            .AppendOperation(new Operation.Literal(new(value.Id), new LiteralValue.Blob([9]))));

        // A syntactically completed fragment is not certified: dangling references survive for validation.
        var unresolved = opened.CompleteBlock(new Terminator.Jump(new(new(900), [new(800)])));
        ((Terminator.Jump)unresolved.CompletedBlocks[allocated.Id].Terminator)
            .Edge.Target.Should().Be(new PineBlockId(900));
    }

    [Fact]
    public void Construction_protocol_prevents_losing_open_fragments_or_overwriting_completed_blocks()
    {
        var allocated = GraphBuildState.Empty.AllocateBlock();
        var opened = allocated.State.OpenBlock(allocated.Id, []);
        var completed = opened.CompleteBlock(new Terminator.Return([]));

        Action openUnallocated = () => GraphBuildState.Empty.OpenBlock(new(0), []);
        Action openNegative = () => allocated.State.OpenBlock(new(-1), []);
        Action openTwice = () => opened.OpenBlock(allocated.Id, []);
        Action overwrite = () => completed.OpenBlock(allocated.Id, []);
        Action appendWithoutBlock = () => allocated.State.AppendOperation(new Operation.Literal(new(new(1)), new LiteralValue.List([])));
        Action completeWithoutBlock = () => allocated.State.CompleteBlock(new Terminator.Return([]));

        openUnallocated.Should().Throw<InvalidOperationException>();
        openNegative.Should().Throw<InvalidOperationException>();
        openTwice.Should().Throw<InvalidOperationException>();
        overwrite.Should().Throw<InvalidOperationException>();
        appendWithoutBlock.Should().Throw<InvalidOperationException>();
        completeWithoutBlock.Should().Throw<InvalidOperationException>();
    }

    private static FunctionGraph Fixture(int fixture) =>
        fixture switch
        {
            0 => LiteralGraph(),
            1 => BranchGraph(),
            2 => SwitchGraph(),
            3 => CallGraph(false, false),
            4 => CallGraph(false, true),
            5 => CallGraph(true, false),
            6 => CallGraph(true, true),
            7 => LoopGraph(),
            _ => throw new ArgumentOutOfRangeException(nameof(fixture)),
        };

    private static FunctionGraph LiteralGraph() =>
        new(new(7), new([], [ValueType.PineValue]), new(90),
            ImmutableDictionary<PineBlockId, BasicBlock>.Empty.Add(new(90),
                new(new(90), [], [
                    new Operation.Literal(new(new(12)),
                        new LiteralValue.List([
                            new LiteralValue.Blob([]),
                            new LiteralValue.List([]),
                            new LiteralValue.Blob([0, 1, 254, 255])]))],
                    new Terminator.Return([new(12)]))));

    private static FunctionGraph BranchGraph() =>
        new(new(8), new([new(new([]))], [ValueType.PineValue]), new(80),
            ImmutableDictionary<PineBlockId, BasicBlock>.Empty
            .Add(new(80), new(new(80), [new(new(1))], [
                new Operation.Project(new(new(2)), new(1), new([0, 2])),
                new Operation.Literal(new(new(3)), new LiteralValue.Blob([1])),
                new Operation.MakeList(new(new(4)), [new(2), new(3)]),
                new Operation.Builtin(new(new(5)), "equal", new(4))],
                new Terminator.Branch(new(5), new LiteralValue.Blob([4]),
                    new(new(2), [new(2), new(3)]),
                    new(new(2), [new(3), new(2)]))))
            .Add(new(2), new(new(2), [new(new(20)), new(new(21))], [], new Terminator.Return([new(20)]))));

    private static FunctionGraph SwitchGraph()
    {
        var branch = BranchGraph();
        var block = branch.Blocks[new(80)];
        var selection = new Terminator.Switch(new(5), [
            new(new LiteralValue.Blob([4]), new(new(2), [new(2), new(3)])),
            new(new LiteralValue.List([]), new(new(2), [new(3), new(2)]))],
            new(new(2), [new(2), new(2)]));

        return new(branch.Id, branch.Signature, branch.Entry,
            branch.Blocks.SetItem(block.Id, block with { Terminator = selection }));
    }

    private static FunctionGraph CallGraph(bool known, bool tail)
    {
        var signature = new FunctionSignature([new(new([]))], [ValueType.PineValue]);
        var call = new Call(new(6),
            known ? new CallTarget.Known(new(19)) : new CallTarget.Dynamic(new(2)),
            signature, [new(1)]);
        var block = new BasicBlock(new(100), [new(new(1))],
            [new Operation.Literal(new(new(2)), new LiteralValue.Blob([255]))],
            tail
            ? new Terminator.TailInvoke(call)
            : new Terminator.Invoke(call,
                new(new(5), [new ContinuationBinding.CallerValue(new(1)), new ContinuationBinding.ReturnedResult(0)])));
        var blocks = ImmutableDictionary<PineBlockId, BasicBlock>.Empty.Add(block.Id, block);
        var withContinuation =
            tail ? blocks :
            blocks.Add(new(5), new(new(5), [new(new(30)), new(new(31))],
                [new Operation.MakeList(new(new(32)), [new(30), new(31)])],
                new Terminator.Return([new(32)])));

        return new(new(19), signature, block.Id, withContinuation);
    }

    private static FunctionGraph LoopGraph() =>
        new(new(50), new([new(new([]))], [ValueType.PineValue]), new(90),
            ImmutableDictionary<PineBlockId, BasicBlock>.Empty
            .Add(new(90), new(new(90), [new(new(90))], [], new Terminator.Jump(new(new(60), [new(90)]))))
            .Add(new(60), new(new(60), [new(new(60))], [],
                new Terminator.Branch(new(60), new LiteralValue.Blob([0]),
                    new(new(3), [new(60)]), new(new(20), [new(60)]))))
            .Add(new(3), new(new(3), [new(new(3))], [],
                new Terminator.Branch(new(3), new LiteralValue.Blob([0]),
                    new(new(7), [new(3)]), new(new(60), [new(3)]))))
            .Add(new(7), new(new(7), [new(new(7))], [], new Terminator.Jump(new(new(3), [new(7)]))))
            .Add(new(20), new(new(20), [new(new(20))], [],
                new Terminator.Branch(new(20), new LiteralValue.Blob([0]),
                    new(new(20), [new(20)]), new(new(15), [new(20)]))))
            .Add(new(15), new(new(15), [new(new(15))], [], new Terminator.Return([new(15)]))));
}
