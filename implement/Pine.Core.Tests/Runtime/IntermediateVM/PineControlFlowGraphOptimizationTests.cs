using AwesomeAssertions;
using Pine.Core.PineLanguage;
using Pine.Core.Runtime.IntermediateVM;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Reflection;
using Xunit;

namespace Pine.Core.Tests.Runtime.IntermediateVM;

public class PineControlFlowGraphOptimizationTests
{
    private static object CreatePrivate(string name) =>
        Activator.CreateInstance(
            typeof(PineControlFlowGraph).GetNestedType(name, BindingFlags.NonPublic)!,
            nonPublic: true)!;

    private static object? Call(object instance, string method, params object?[] arguments) =>
        instance.GetType().GetMethod(method)!.Invoke(instance, arguments);

    private static int Count(object context, string property) =>
        (int)context.GetType().GetProperty(property)!.GetValue(context)!;

    private static PineControlFlowFragment Ops(params StackInstruction[] instructions) =>
        PineControlFlowFragment.FromOperations(instructions);

    private static PineControlFlowGraph Scheduled(PineControlFlowGraph graph, int parameters, object? context = null) =>
        (PineControlFlowGraph)Call(context ?? CreatePrivate("OptimizationContext"), "Optimize", graph, parameters)!;

    private static PineControlFlowGraph Unscheduled(PineControlFlowGraph graph, int parameters) =>
        graph.RemoveEmptyForwardingBlocks()
        .ForwardJumpsToReturn()
        .ForwardConstantBooleanBranches()
        .RemoveUnreachableBlocks()
        .ReplaceNonEscapingLists(parameters)
        .EliminateLocalCopies()
        .EliminateRedundantLocalWrites(parameters)
        .ForwardEquivalentLocalReads()
        .FuseLocalListProjections()
        .ForwardProvenEqualityBranches()
        .EliminateDeadLocalStores()
        .EliminateRedundantLocalWrites(parameters)
        .EliminateDiscardedStackValues(parameters)
        .EliminateDeadLocalStores()
        .EliminateRedundantLocalWrites(parameters)
        .EliminateDiscardedStackValues(parameters)
        .RemoveRedundantForwardJumps()
        .RemoveUnreachableBlocks()
        .FuseDescendingLocalIntegerAdditions(parameters)
        .FuseLocalIntegerAdditions()
        .FoldKnownLiteralValues(parameters)
        .FuseLiteralLocalStores()
        .EliminateDeadLocalStores()
        .EliminateRedundantLocalWrites(parameters)
        .EliminateDiscardedStackValues(parameters);

    [Fact]
    public void Compact_local_sets_match_sparse_set_operations_without_aliasing_clones()
    {
        var values = new[] { -1, 0, 1, 63, 64, 127, 128, 4095, 4096, 1000000, int.MaxValue };
        var left = CreatePrivate("LocalSet");
        var right = CreatePrivate("LocalSet");
        var expectedLeft = new HashSet<int>();
        var expectedRight = new HashSet<int>();

        foreach (var value in values)
        {
            Call(left, "Add", value);
            expectedLeft.Add(value);

            if ((value & 1) != 0)
            {
                Call(right, "Add", value);
                expectedRight.Add(value);
            }
        }

        var clone = Call(left, "Clone")!;

        foreach (var value in values)
            Call(left, "Remove", value);

        foreach (var value in values)
            ((bool)Call(clone, "Contains", value)!).Should().BeTrue();

        Call(left, "CopyFrom", clone);
        Call(left, "ExceptWith", right);
        expectedLeft.ExceptWith(expectedRight);

        foreach (var value in values)
            ((bool)Call(left, "Contains", value)!).Should().Be(expectedLeft.Contains(value));

        Call(left, "UnionWith", right);
        expectedLeft.UnionWith(expectedRight);
        Call(left, "IntersectWith", right);
        expectedLeft.IntersectWith(expectedRight);

        foreach (var value in values)
            ((bool)Call(left, "Contains", value)!).Should().Be(expectedLeft.Contains(value));

        ((bool)Call(left, "SetEquals", right)!).Should().BeTrue();
        Call(left, "Clear");

        foreach (var value in values)
            ((bool)Call(clone, "Contains", value)!).Should().BeTrue();
    }

    [Theory]
    [InlineData(63)]
    [InlineData(64)]
    [InlineData(4095)]
    [InlineData(4096)]
    [InlineData(1000000)]
    public void Sparse_and_word_boundary_locals_preserve_initialization_and_liveness(int local)
    {
        var value = PineValue.Blob([31]);

        var graph =
            PineControlFlowGraph.FromFragment(
                Ops(StackInstruction.Local_Set_Literal(local, value), StackInstruction.Local_Get(local)));

        graph.EliminateDeadLocalStores().Should().BeSameAs(graph);
        graph.EliminateRedundantLocalWrites(0).Should().BeSameAs(graph);

        var discarded =
            PineControlFlowGraph.FromFragment(
                Ops(
                    StackInstruction.Local_Set_Literal(local, value),
                    StackInstruction.Local_Get(local),
                    StackInstruction.Pop,
                    StackInstruction.Push_Literal(value)));

        discarded.EliminateDiscardedStackValues(0).LowerToStackInstructions().Should().Equal(
            StackInstruction.Local_Set_Literal(local, value),
            StackInstruction.Push_Literal(value),
            StackInstruction.Return);

        Scheduled(discarded, 0).LowerToStackInstructions().Should().Equal(
            Unscheduled(discarded, 0).LowerToStackInstructions());
    }

    [Fact]
    public void Analysis_reuse_invalidates_on_definitions_uses_parameters_and_edges()
    {
        var value = PineValue.Blob([32]);

        var graph =
            PineControlFlowGraph.FromFragment(
                Ops(StackInstruction.Local_Set_Literal(64, value), StackInstruction.Local_Get(0))
                .Append(
                    new PineControlFlowNode.Conditional(
                        PineKernelValues.TrueValue,
                        Ops(StackInstruction.Local_Get(64)),
                        Ops(StackInstruction.Local_Get(64)))));

        var context = CreatePrivate("OptimizationContext");
        var initialized = Call(context, "GetInitializedLocals", graph, 1);
        Call(context, "GetInitializedLocals", graph, 1).Should().BeSameAs(initialized);

        var entry = graph.Blocks[graph.Entry.Value];

        var literalEdit =
            graph with
            {
                Blocks =
                graph.Blocks.SetItem(
                    entry.Id.Value,
                    entry with
                    {
                        Operations =
                        entry.Operations.SetItem(
                            0,
                            entry.Operations[0] with
                            { Instruction = StackInstruction.Local_Set_Literal(64, PineValue.EmptyList) })
                    })
            };

        Call(context, "GetInitializedLocals", literalEdit, 1).Should().BeSameAs(initialized);
        Call(context, "GetInitializedLocals", literalEdit, 2).Should().NotBeSameAs(initialized);

        var removedDefinition =
            graph with
            { Blocks = graph.Blocks.SetItem(entry.Id.Value, entry with { Operations = entry.Operations.RemoveAt(0) }) };

        var changed = Call(context, "GetInitializedLocals", removedDefinition, 1);
        changed.Should().NotBeSameAs(initialized);

        var live = Call(context, "GetLiveness", graph);
        Call(context, "GetLiveness", literalEdit).Should().BeSameAs(live);

        var changedRead =
            graph with
            {
                Blocks =
                graph.Blocks.SetItem(
                    entry.Id.Value,
                    entry with
                    {
                        Operations =
                        entry.Operations.SetItem(
                            1,
                            entry.Operations[1] with
                            { Instruction = StackInstruction.Local_Get(1) })
                    })
            };

        Call(context, "GetLiveness", changedRead).Should().NotBeSameAs(live);

        var predecessors = Call(context, "GetPredecessors", graph);
        Call(context, "GetPredecessors", literalEdit).Should().BeSameAs(predecessors);
        var conditional = (PineControlFlowTerminator.ConditionalJump)entry.Terminator;

        var changedEdges =
            graph with
            {
                Blocks =
                graph.Blocks.SetItem(
                    entry.Id.Value,
                    entry with
                    { Terminator = conditional with { Branch = conditional.FallThrough } })
            };

        Call(context, "GetPredecessors", changedEdges).Should().NotBeSameAs(predecessors);
        Count(context, "InitializationHits").Should().Be(2);
        Count(context, "LivenessHits").Should().Be(1);
    }

    [Fact]
    public void Public_validation_and_private_snapshot_validation_reject_changed_invalid_graphs()
    {
        var graph = PineControlFlowGraph.FromFragment(Ops(StackInstruction.Local_Get(0)));
        var context = CreatePrivate("OptimizationContext");
        Call(context, "Validate", graph);
        Call(context, "Validate", graph);
        Count(context, "ValidationRuns").Should().Be(1);
        Count(context, "ValidationHits").Should().Be(1);

        var missingTarget =
            graph with
            {
                Blocks =
                graph.Blocks.SetItem(
                    0,
                    graph.Blocks[0] with
                    { Terminator = new PineControlFlowTerminator.Jump(new(100), [], false) })
            };

        var validate = () => missingTarget.Validate();
        validate.Should().Throw<InvalidOperationException>().WithMessage("*targets missing block 100*");
        var validateInContext = () => Call(context, "Validate", missingTarget);

        validateInContext.Should().Throw<TargetInvocationException>()
            .Which.InnerException.Should().BeOfType<InvalidOperationException>();

        var unknownValue =
            graph with
            {
                Blocks =
                graph.Blocks.SetItem(
                    0,
                    graph.Blocks[0] with
                    { Operations = [graph.Blocks[0].Operations[0] with { Inputs = [new(99)] }] })
            };

        var validateScope = () => unknownValue.Validate();
        validateScope.Should().Throw<InvalidOperationException>().WithMessage("*before its definition*");

        var negativeParameters = () => graph.EliminateDiscardedStackValues(-1);
        negativeParameters.Should().Throw<ArgumentOutOfRangeException>();
    }

    [Fact]
    public void Scheduled_passes_match_the_unconditional_chain_including_newly_created_candidates()
    {
        var tag = PineValue.Blob([33]);

        var graphs =
            new List<PineControlFlowGraph>
            {
                PineControlFlowGraph.FromFragment(Ops(StackInstruction.Local_Get(0))),
                PineControlFlowGraph.FromFragment(
                    Ops(
                        StackInstruction.Push_Literal(tag),
                        StackInstruction.Local_Set(2),
                        StackInstruction.Pop,
                        StackInstruction.Local_Get(2),
                        StackInstruction.Local_Set(3),
                        StackInstruction.Pop,
                        StackInstruction.Local_Get(3))),
                PineControlFlowGraph.FromFragment(
                    Ops(
                        StackInstruction.Local_Get(0),
                        StackInstruction.Build_List_With_Prefix(PineValue.List([tag]), 1),
                        StackInstruction.Local_Set(2),
                        StackInstruction.Pop,
                        StackInstruction.Local_Get_Skip_Head_Const(2, 0))),
                PineControlFlowGraph.FromFragment(
                    Ops(
                        StackInstruction.Local_Get(0),
                        StackInstruction.Int_Add_Const(1),
                        StackInstruction.Local_Set(0),
                        StackInstruction.Pop,
                        StackInstruction.Local_Get(0)))
            };

        for (var item = 0; item < 20; item++)
        {
            var literal = PineValue.Blob([(byte)item]);

            graphs.Add(
                PineControlFlowGraph.FromFragment(
                    Ops(StackInstruction.Local_Get(0))
                    .Append(
                        new PineControlFlowNode.Conditional(
                            literal,
                            Ops(
                                StackInstruction.Local_Get(0),
                                StackInstruction.Build_List_With_Prefix(PineValue.List([tag]), 1)),
                            Ops(StackInstruction.Push_Literal(PineValue.List([tag, literal])))))
                    .AppendOperations(
                        [
                            StackInstruction.Local_Set(2), StackInstruction.Pop,
                            StackInstruction.Local_Get_Skip_Head_Const(2, item % 3)
                        ])));
        }

        foreach (var graph in graphs)
        {
            Scheduled(graph, 1).LowerToStackInstructions().Should()
                .Equal(Unscheduled(graph, 1).LowerToStackInstructions());
        }

        var context = CreatePrivate("OptimizationContext");
        Scheduled(graphs[0], 1, context);
        Count(context, "SkippedPasses").Should().BeGreaterThan(10);
        Count(context, "ValidationHits").Should().BeGreaterThan(10);
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void Value_fact_meet_retains_explicit_prefix_proofs_without_expanding_exact_lists(bool partialList)
    {
        var tag = PineValue.Blob([34]);
        var first = PineValue.Blob([35]);
        var second = PineValue.Blob([36]);

        var otherBranch =
            partialList
            ?
            Ops(StackInstruction.Local_Get(0), StackInstruction.Build_List_With_Prefix(PineValue.List([tag]), 1))
            :
            Ops(StackInstruction.Push_Literal(PineValue.List([tag, second])));

        var graph =
            PineControlFlowGraph.FromFragment(
                Ops(StackInstruction.Local_Get(0))
                .Append(
                    new PineControlFlowNode.Conditional(
                        PineKernelValues.TrueValue,
                        Ops(StackInstruction.Push_Literal(PineValue.List([tag, first]))),
                        otherBranch))
                .AppendOperations(
                    [
                        StackInstruction.Local_Set(2), StackInstruction.Pop,
                        StackInstruction.Local_Get_Skip_Head_Const(2, 0)
                    ])
                .Append(
                    new PineControlFlowNode.Conditional(
                        tag,
                        Ops(StackInstruction.Push_Literal(first)),
                        Ops(StackInstruction.Push_Literal(second)))));

        var optimized = graph.ForwardProvenEqualityBranches();

        optimized.Blocks.Count(block => block.Terminator is PineControlFlowTerminator.ConditionalJump)
            .Should().Be(partialList ? 1 : 2);
    }

    [Fact]
    public void Stateless_CFG_nodes_and_unchanged_fragments_are_reused_without_dropping_instruction_fields()
    {
        var first = PineControlFlowGraph.FromFragment(Ops(StackInstruction.Local_Get(0)));
        var second = PineControlFlowGraph.FromFragment(Ops(StackInstruction.Local_Get(0)));
        first.Blocks.Single().Terminator.Should().BeSameAs(second.Blocks.Single().Terminator);

        var fragment = Ops(StackInstruction.Head_Generic);
        fragment.Nodes.Single().Should().BeSameAs(Ops(StackInstruction.Head_Generic).Nodes.Single());
        fragment.Append(PineControlFlowFragment.Empty).Should().BeSameAs(fragment);
        PineControlFlowFragment.Empty.Append(fragment).Should().BeSameAs(fragment);
        fragment.AppendOperations([]).Should().BeSameAs(fragment);
        fragment.ReplaceLastOperation(StackInstruction.Head_Generic).Should().BeSameAs(fragment);

        var custom = StackInstruction.Head_Generic with { SkipCount = 99 };
        var replaced = fragment.ReplaceLastOperation(custom);
        replaced.LastOperationOrNull.Should().BeSameAs(custom);
        replaced.Nodes.Single().Should().NotBeSameAs(fragment.Nodes.Single());

        var badInstruction = () => PineControlFlowFragment.Empty.AppendOperation(StackInstruction.Return);
        badInstruction.Should().Throw<ArgumentException>().WithMessage("*modeled structurally*");
    }
}
