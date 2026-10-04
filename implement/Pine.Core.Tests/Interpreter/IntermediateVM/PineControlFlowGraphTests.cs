using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.CommonEncodings;
using Pine.Core.Internal;
using Pine.Core.Interpreter.IntermediateVM;
using Pine.Core.PineVM;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using Xunit;

namespace Pine.Core.Tests.Interpreter.IntermediateVM;

public class PineControlFlowGraphTests
{
    private static PineControlFlowFragment Ops(params StackInstruction[] instructions) =>
        PineControlFlowFragment.FromOperations(instructions);

    private static PineControlFlowNode.Conditional Conditional(
        PineValue literal,
        PineControlFlowFragment fallThrough,
        PineControlFlowFragment branch) =>
        new(literal, fallThrough, branch);

    private static PineControlFlowNode.Switch Switch(
        PineControlFlowFragment defaultBranch,
        params (PineValue Literal, PineControlFlowFragment Branch)[] cases) =>
        new(
            PineSwitchKind.Equal,
            [.. cases.Select((switchCase, index) => new PineSwitchFragmentCase(switchCase.Literal, index))],
            defaultBranch,
            [.. cases.Select(switchCase => switchCase.Branch)],
            SkipCountMultiplier: 1);

    private static ImmutableArray<StackInstruction> Optimize(PineControlFlowFragment fragment) =>
        PineControlFlowGraph
        .FromFragment(fragment)
        .ForwardJumpsToReturn()
        .ForwardConstantBooleanBranches()
        .LowerToStackInstructions();

    [Fact]
    public void Length_jump_fusion_preserves_branches_for_lists_and_blobs()
    {
        var trueValue = PineValue.Blob([41]);
        var falseValue = PineValue.Blob([43]);

        var graph =
            PineControlFlowGraph.FromFragment(
                Ops(StackInstruction.Local_Get(0), StackInstruction.Length)
                .Append(
                    Conditional(
                        IntegerEncoding.EncodeSignedInteger(2),
                        Ops(StackInstruction.Push_Literal(falseValue)),
                        Ops(StackInstruction.Push_Literal(trueValue)))));

        var instructions = graph.LowerToStackInstructions();

        instructions.Should().ContainSingle(
            instruction =>
            instruction.Kind == StackInstructionKind.Length_Jump_If_Equal_Const &&
            instruction.IntegerLiteral == 2);

        instructions.Should().NotContain(instruction => instruction.Kind == StackInstructionKind.Length);

        Evaluate(graph, PineValue.Blob([1, 2])).Should().Be(trueValue);
        Evaluate(graph, PineValue.Blob([1])).Should().Be(falseValue);
        Evaluate(graph, PineValue.List([PineValue.EmptyList, PineValue.EmptyBlob])).Should().Be(trueValue);
        Evaluate(graph, PineValue.EmptyList).Should().Be(falseValue);
    }

    [Fact]
    public void Known_literal_projections_release_unused_descending_local_stores()
    {
        var elements =
            new[]
            {
                PineValue.Blob([65]),
                PineValue.Blob([66]),
                PineValue.Blob([67])
            };

        var list = PineValue.List(elements);

        var original =
            PineControlFlowGraph.FromFragment(
                Ops(
                    StackInstruction.Push_Literal(list),
                    StackInstruction.Head_Generic,
                    StackInstruction.Push_Literal(list),
                    StackInstruction.Skip_Head_Const(1),
                    StackInstruction.Push_Literal(list),
                    StackInstruction.Skip_Head_Const(2),
                    StackInstruction.Local_Set_Descending(3, 3),
                    StackInstruction.PopMultiple(3),
                    StackInstruction.Local_Get(2)));

        var folded = original.FoldKnownLiteralValues(parameterCount: 0);

        folded.LowerToStackInstructions().Should().Equal(
            StackInstruction.Push_Literal(elements[0]),
            StackInstruction.Push_Literal(elements[1]),
            StackInstruction.Push_Literal(elements[2]),
            StackInstruction.Local_Set_Descending(3, 3),
            StackInstruction.PopMultiple(3),
            StackInstruction.Push_Literal(elements[1]),
            StackInstruction.Return);

        var cleaned =
            folded.EliminateDeadLocalStores()
            .EliminateDiscardedStackValues(parameterCount: 0);

        cleaned.LowerToStackInstructions().Should().Equal(
            StackInstruction.Push_Literal(elements[1]),
            StackInstruction.Return);

        Evaluate(cleaned, PineValue.EmptyList).Should().Be(Evaluate(original, PineValue.EmptyList));
    }

    [Fact]
    public void Literal_local_stores_omit_dead_interior_slots_and_preserve_unstored_stack_values()
    {
        var marker = PineValue.Blob([70]);
        var first = PineValue.Blob([71]);
        var unused = PineValue.Blob([72]);
        var last = PineValue.Blob([73]);

        var original =
            PineControlFlowGraph.FromFragment(
                Ops(
                    StackInstruction.Push_Literal(marker),
                    StackInstruction.Push_Literal(first),
                    StackInstruction.Push_Literal(unused),
                    StackInstruction.Push_Literal(last),
                    StackInstruction.Local_Set_Descending(3, 3),
                    StackInstruction.PopMultiple(4),
                    StackInstruction.Local_Get(1),
                    StackInstruction.Local_Get(3),
                    StackInstruction.Build_List(2)));

        var optimized =
            original.FuseLiteralLocalStores()
            .EliminateDeadLocalStores()
            .EliminateDiscardedStackValues(parameterCount: 0);

        optimized.LowerToStackInstructions().Should().Equal(
            StackInstruction.Local_Set_Literal(1, first),
            StackInstruction.Local_Set_Literal(3, last),
            StackInstruction.Local_Get(1),
            StackInstruction.Local_Get(3),
            StackInstruction.Build_List(2),
            StackInstruction.Return);

        var details = StackInstruction.GetDetails(StackInstruction.Local_Set_Literal(3, last));
        details.PopCount.Should().Be(0);
        details.PushCount.Should().Be(0);

        StackFrameInstructions.ComputeLocalsCount(
            [StackInstruction.Local_Set_Literal(3, last)],
            StaticFunctionInterface.Generic).Should().BeGreaterThan(3);

        Evaluate(optimized, PineValue.EmptyList).Should().Be(Evaluate(original, PineValue.EmptyList));
    }

    [Fact]
    public void Known_literal_projections_match_blob_and_out_of_bounds_semantics()
    {
        var blob = PineValue.Blob([10, 20]);

        foreach (var (index, expected) in new[]
        {
            (-1, PineValue.Blob([10])),
            (1, PineValue.Blob([20])),
            (5, PineValue.EmptyBlob)
        })
        {
            var original =
                PineControlFlowGraph.FromFragment(
                    Ops(
                        StackInstruction.Push_Literal(blob),
                        StackInstruction.Skip_Head_Const(index)));

            var folded = original.FoldKnownLiteralValues(parameterCount: 0);

            folded.LowerToStackInstructions().Should().Equal(
                StackInstruction.Push_Literal(expected),
                StackInstruction.Return);

            Evaluate(folded, PineValue.EmptyList).Should().Be(Evaluate(original, PineValue.EmptyList));
        }
    }

    [Fact]
    public void Unknown_or_uninitialized_locals_are_not_replaced_by_literals()
    {
        var first = PineValue.Blob([12]);
        var second = PineValue.Blob([13]);

        var original =
            PineControlFlowGraph.FromFragment(
                Ops(StackInstruction.Local_Get(0))
                .Append(
                    Conditional(
                        first,
                        Ops(StackInstruction.Push_Literal(first), StackInstruction.Local_Set(1)),
                        Ops(StackInstruction.Push_Literal(second), StackInstruction.Local_Set(1))))
                .AppendOperation(StackInstruction.Pop)
                .AppendOperation(StackInstruction.Local_Get(1)));

        var folded = original.FoldKnownLiteralValues(parameterCount: 1);

        folded.LowerToStackInstructions().Should().Contain(StackInstruction.Local_Get(1));
        folded.LowerToStackInstructions().Should().Contain(StackInstruction.Local_Get(0));

        foreach (var input in new[] { first, second })
        {
            Evaluate(folded, input).Should().Be(Evaluate(original, input));
        }
    }

    [Fact]
    public void Descending_store_fuses_independent_local_increments_without_stack_values()
    {
        var values =
            Enumerable.Range(5, 5)
            .Select(value => PineValueInProcess.CreateInteger(value).Evaluate())
            .ToArray();

        var original =
            PineControlFlowGraph.FromFragment(
                Ops(
                    StackInstruction.Push_Literal(values[0]),
                    StackInstruction.Push_Literal(values[1]),
                    StackInstruction.Push_Literal(values[2]),
                    StackInstruction.Push_Literal(values[3]),
                    StackInstruction.Push_Literal(values[4]),
                    StackInstruction.Local_Set_Descending(9, 5),
                    StackInstruction.PopMultiple(5),
                    StackInstruction.Local_Get(5),
                    StackInstruction.Int_Add_Const(4),
                    StackInstruction.Local_Get(6),
                    StackInstruction.Local_Get(7),
                    StackInstruction.Int_Add_Const(1),
                    StackInstruction.Local_Get(8),
                    StackInstruction.Local_Get(9),
                    StackInstruction.Local_Set_Descending(9, 5),
                    StackInstruction.PopMultiple(5),
                    StackInstruction.Local_Get(5),
                    StackInstruction.Local_Get(6),
                    StackInstruction.Local_Get(7),
                    StackInstruction.Local_Get(8),
                    StackInstruction.Local_Get(9),
                    StackInstruction.Build_List(5)));

        var fused = original.FuseDescendingLocalIntegerAdditions(parameterCount: 0);

        fused.LowerToStackInstructions().Should().Equal(
            [
                .. values.Select(StackInstruction.Push_Literal),
                StackInstruction.Local_Set_Descending(9, 5),
                StackInstruction.PopMultiple(5),
                StackInstruction.Local_Int_Add_Const(5, 4),
                StackInstruction.Local_Int_Add_Const(7, 1),
                .. Enumerable.Range(5, 5).Select(StackInstruction.Local_Get),
                StackInstruction.Build_List(5),
                StackInstruction.Return
            ]);

        Evaluate(fused, PineValue.EmptyList).Should().Be(Evaluate(original, PineValue.EmptyList));
    }

    [Fact]
    public void Descending_store_does_not_fuse_uninitialized_or_different_local_sources()
    {
        var uninitialized =
            PineControlFlowGraph.FromFragment(
                Ops(
                    StackInstruction.Local_Get(0),
                    StackInstruction.Int_Add_Const(1),
                    StackInstruction.Local_Get(1),
                    StackInstruction.Local_Set_Descending(1, 2),
                    StackInstruction.PopMultiple(2),
                    StackInstruction.Local_Get(0)));

        uninitialized.FuseDescendingLocalIntegerAdditions(parameterCount: 0)
            .LowerToStackInstructions().Should().Equal(uninitialized.LowerToStackInstructions());

        var differentSource =
            PineControlFlowGraph.FromFragment(
                Ops(
                    StackInstruction.Local_Get(0),
                    StackInstruction.Int_Add_Const(1),
                    StackInstruction.Local_Get(0),
                    StackInstruction.Local_Set_Descending(1, 2),
                    StackInstruction.PopMultiple(2),
                    StackInstruction.Local_Get(1)));

        differentSource.FuseDescendingLocalIntegerAdditions(parameterCount: 2)
            .LowerToStackInstructions().Should().Equal(differentSource.LowerToStackInstructions());
    }

    [Fact]
    public void Descending_store_fuses_consecutive_independent_groups()
    {
        var zero = PineValueInProcess.CreateInteger(0).Evaluate();
        var one = PineValueInProcess.CreateInteger(1).Evaluate();

        var original =
            PineControlFlowGraph.FromFragment(
                Ops(
                    StackInstruction.Push_Literal(zero),
                    StackInstruction.Push_Literal(one),
                    StackInstruction.Local_Set_Descending(1, 2),
                    StackInstruction.PopMultiple(2),
                    StackInstruction.Local_Get(0),
                    StackInstruction.Int_Add_Const(1),
                    StackInstruction.Local_Get(1),
                    StackInstruction.Local_Set_Descending(1, 2),
                    StackInstruction.PopMultiple(2),
                    StackInstruction.Local_Get(0),
                    StackInstruction.Local_Get(1),
                    StackInstruction.Int_Add_Const(2),
                    StackInstruction.Local_Set_Descending(1, 2),
                    StackInstruction.PopMultiple(2),
                    StackInstruction.Local_Get(0),
                    StackInstruction.Local_Get(1),
                    StackInstruction.Build_List(2)));

        var fused = original.FuseDescendingLocalIntegerAdditions(parameterCount: 0);

        fused.LowerToStackInstructions().Should().Equal(
            StackInstruction.Push_Literal(zero),
            StackInstruction.Push_Literal(one),
            StackInstruction.Local_Set_Descending(1, 2),
            StackInstruction.PopMultiple(2),
            StackInstruction.Local_Int_Add_Const(0, 1),
            StackInstruction.Local_Int_Add_Const(1, 2),
            StackInstruction.Local_Get(0),
            StackInstruction.Local_Get(1),
            StackInstruction.Build_List(2),
            StackInstruction.Return);

        Evaluate(fused, PineValue.EmptyList).Should().Be(Evaluate(original, PineValue.EmptyList));
    }

    [Theory]
    [InlineData(1)]
    [InlineData(-2)]
    public void Fuse_local_integer_addition_preserves_stack_and_local_value(int increment)
    {
        var marker = PineValue.Blob([72]);

        var original =
            PineControlFlowGraph.FromFragment(
                Ops(
                    StackInstruction.Push_Literal(marker),
                    StackInstruction.Local_Get(0),
                    StackInstruction.Int_Add_Const(increment),
                    StackInstruction.Local_Set_Descending(0, 1),
                    StackInstruction.Pop,
                    StackInstruction.Local_Get(0),
                    StackInstruction.Build_List(2)));

        var fused = original.FuseLocalIntegerAdditions();

        fused.LowerToStackInstructions().Should().Equal(
            StackInstruction.Push_Literal(marker),
            StackInstruction.Local_Int_Add_Const(0, increment),
            StackInstruction.Local_Get(0),
            StackInstruction.Build_List(2),
            StackInstruction.Return);

        var details = StackInstruction.GetDetails(StackInstruction.Local_Int_Add_Const(0, increment));
        details.PopCount.Should().Be(0);
        details.PushCount.Should().Be(0);

        StackInstruction.Local_Int_Add_Const(0, increment).ToString().Should()
            .Contain($"Local_Int_Add_Const (0, {increment})");

        StackFrameInstructions.ComputeLocalsCount(
            [StackInstruction.Local_Int_Add_Const(4, increment)],
            StaticFunctionInterface.Generic).Should().BeGreaterThan(4);

        foreach (var input in new PineValue[] { PineValueInProcess.CreateInteger(4).Evaluate(), PineValue.EmptyList })
        {
            Evaluate(fused, input).Should().Be(Evaluate(original, input));
        }
    }

    [Fact]
    public void Fuse_local_integer_addition_requires_a_discarded_result_and_same_destination()
    {
        var retained =
            PineControlFlowGraph.FromFragment(
                Ops(
                    StackInstruction.Local_Get(0),
                    StackInstruction.Int_Add_Const(1),
                    StackInstruction.Local_Set_Descending(0, 1)));

        retained.FuseLocalIntegerAdditions().LowerToStackInstructions()
            .Should().Equal(retained.LowerToStackInstructions());

        var differentDestination =
            PineControlFlowGraph.FromFragment(
                Ops(
                    StackInstruction.Local_Get(0),
                    StackInstruction.Int_Add_Const(1),
                    StackInstruction.Local_Set_Descending(1, 1),
                    StackInstruction.Pop,
                    StackInstruction.Local_Get(1)));

        differentDestination.FuseLocalIntegerAdditions().LowerToStackInstructions()
            .Should().Equal(differentDestination.LowerToStackInstructions());
    }

    [Fact]
    public void Local_integer_addition_invalidates_facts_and_local_aliases()
    {
        var four = PineValueInProcess.CreateInteger(4).Evaluate();

        var original =
            PineControlFlowGraph.FromFragment(
                Ops(
                    StackInstruction.Push_Literal(four),
                    StackInstruction.Local_Set(1),
                    StackInstruction.Pop,
                    StackInstruction.Local_Int_Add_Const(1, 1),
                    StackInstruction.Local_Get(1))
                .Append(
                    Conditional(
                        four,
                        Ops(StackInstruction.Push_Literal(PineValue.EmptyList)),
                        Ops(StackInstruction.Push_Literal(PineValue.EmptyBlob)))));

        var optimized = original.ForwardProvenEqualityBranches();

        optimized.Blocks.Select(block => block.Terminator)
            .Should().Contain(terminator => terminator is PineControlFlowTerminator.ConditionalJump);

        Evaluate(optimized, PineValue.EmptyList).Should().Be(Evaluate(original, PineValue.EmptyList));

        var aliases =
            PineControlFlowGraph.FromFragment(
                Ops(
                    StackInstruction.Local_Get(0),
                    StackInstruction.Local_Set(1),
                    StackInstruction.Pop,
                    StackInstruction.Local_Int_Add_Const(1, 1),
                    StackInstruction.Local_Get(1)));

        aliases.ForwardEquivalentLocalReads().LowerToStackInstructions()
            .Should().Contain(StackInstruction.Local_Get(1));

        Evaluate(aliases.ForwardEquivalentLocalReads(), four).Should().Be(Evaluate(aliases, four));
    }

    [Fact]
    public void Fused_integer_addition_keeps_its_local_initialization_live()
    {
        var four = PineValueInProcess.CreateInteger(4).Evaluate();

        var original =
            PineControlFlowGraph.FromFragment(
                Ops(
                    StackInstruction.Push_Literal(four),
                    StackInstruction.Local_Set(1),
                    StackInstruction.Pop,
                    StackInstruction.Local_Get(1),
                    StackInstruction.Int_Add_Const(1),
                    StackInstruction.Local_Set_Descending(1, 1),
                    StackInstruction.Pop,
                    StackInstruction.Push_Literal(four)));

        var fused = original.FuseLocalIntegerAdditions();
        var withDeadStoresRemoved = fused.EliminateDeadLocalStores();

        withDeadStoresRemoved.LowerToStackInstructions()
            .Should().Contain(StackInstruction.Local_Set(1));

        Evaluate(withDeadStoresRemoved, PineValue.EmptyList)
            .Should().Be(Evaluate(original, PineValue.EmptyList));
    }

    [Fact]
    public void Descending_store_drops_unchanged_bottom_locals_and_their_reads()
    {
        var first = PineValue.Blob([70]);
        var second = PineValue.Blob([71]);
        var number = PineValueInProcess.CreateInteger(4).Evaluate();

        var original =
            PineControlFlowGraph.FromFragment(
                Ops(
                    StackInstruction.Push_Literal(first),
                    StackInstruction.Local_Set(0),
                    StackInstruction.Pop,
                    StackInstruction.Push_Literal(second),
                    StackInstruction.Local_Set(1),
                    StackInstruction.Pop,
                    StackInstruction.Push_Literal(number),
                    StackInstruction.Local_Set(2),
                    StackInstruction.Pop,
                    StackInstruction.Local_Get(0),
                    StackInstruction.Local_Get(1),
                    StackInstruction.Local_Get(2),
                    StackInstruction.Int_Add_Const(1),
                    StackInstruction.Local_Set_Descending(2, 3),
                    StackInstruction.PopMultiple(3),
                    StackInstruction.Local_Get(2)));

        var optimized = original.EliminateRedundantLocalWrites(parameterCount: 0);

        optimized.LowerToStackInstructions().Should().Equal(
            StackInstruction.Push_Literal(first),
            StackInstruction.Local_Set(0),
            StackInstruction.Pop,
            StackInstruction.Push_Literal(second),
            StackInstruction.Local_Set(1),
            StackInstruction.Pop,
            StackInstruction.Push_Literal(number),
            StackInstruction.Local_Set(2),
            StackInstruction.Pop,
            StackInstruction.Local_Get(2),
            StackInstruction.Int_Add_Const(1),
            StackInstruction.Local_Set_Descending(2, 1),
            StackInstruction.Pop,
            StackInstruction.Local_Get(2),
            StackInstruction.Return);

        Evaluate(optimized, PineValue.EmptyList).Should()
            .Be(Evaluate(original, PineValue.EmptyList));
    }

    [Fact]
    public void Descending_store_keeps_a_read_that_might_fail_or_become_stale()
    {
        var uninitialized =
            PineControlFlowGraph.FromFragment(
                Ops(
                    StackInstruction.Local_Get(2),
                    StackInstruction.Local_Get(0),
                    StackInstruction.Local_Set_Descending(3, 2),
                    StackInstruction.PopMultiple(2),
                    StackInstruction.Local_Get(3)));

        uninitialized.EliminateRedundantLocalWrites(parameterCount: 1)
            .LowerToStackInstructions().Should().Equal(uninitialized.LowerToStackInstructions());

        var stale =
            PineControlFlowGraph.FromFragment(
                Ops(
                    StackInstruction.Local_Get(0),
                    StackInstruction.Push_Literal(PineValue.EmptyList),
                    StackInstruction.Local_Set(0),
                    StackInstruction.Pop,
                    StackInstruction.Local_Get(0),
                    StackInstruction.Local_Set_Descending(1, 2),
                    StackInstruction.PopMultiple(2),
                    StackInstruction.Local_Get(1)));

        stale.EliminateRedundantLocalWrites(parameterCount: 1)
            .LowerToStackInstructions().Should().Equal(stale.LowerToStackInstructions());
    }

    [Fact]
    public void Descending_store_releases_inputs_when_dead_store_cleanup_trims_the_bottom()
    {
        var first = PineValue.Blob([74]);
        var second = PineValue.Blob([75]);

        var original =
            PineControlFlowGraph.FromFragment(
                Ops(
                    StackInstruction.Push_Literal(first),
                    StackInstruction.Push_Literal(second),
                    StackInstruction.Local_Set_Descending(2, 2),
                    StackInstruction.PopMultiple(2),
                    StackInstruction.Local_Get(2)));

        var optimized =
            original.EliminateDeadLocalStores().EliminateRedundantLocalWrites(parameterCount: 0);

        optimized.LowerToStackInstructions().Should().Equal(
            StackInstruction.Push_Literal(second),
            StackInstruction.Local_Set_Descending(2, 1),
            StackInstruction.Pop,
            StackInstruction.Local_Get(2),
            StackInstruction.Return);

        Evaluate(optimized, PineValue.EmptyList).Should()
            .Be(Evaluate(original, PineValue.EmptyList));
    }

    [Fact]
    public void Equivalent_local_reads_cross_blocks_and_release_the_redundant_copy()
    {
        var first = PineValue.List([PineValue.Blob([72]), PineValue.Blob([73])]);

        var original =
            PineControlFlowGraph.FromFragment(
                Ops(
                    StackInstruction.Local_Get(0),
                    StackInstruction.Local_Set_Descending(2, 1),
                    StackInstruction.Pop,
                    StackInstruction.Local_Get(0))
                .Append(
                    Conditional(
                        first,
                        Ops(StackInstruction.Local_Get_Skip_Head_Const(2, 0)),
                        Ops(StackInstruction.Local_Get_Skip_Head_Const(2, 1)))));

        var optimized =
            original.ForwardEquivalentLocalReads()
            .EliminateDeadLocalStores()
            .EliminateDiscardedStackValues(parameterCount: 1);

        optimized.LowerToStackInstructions().Should()
            .NotContain(instruction => instruction.LocalIndex == 2);

        foreach (var input in new PineValue[] { first, PineValue.EmptyList })
        {
            Evaluate(optimized, input).Should().Be(Evaluate(original, input));
        }
    }

    [Fact]
    public void Equivalent_local_reads_keep_a_copy_when_source_changes_on_one_path()
    {
        var original =
            PineControlFlowGraph.FromFragment(
                Ops(
                    StackInstruction.Local_Get(0),
                    StackInstruction.Local_Set_Descending(2, 1),
                    StackInstruction.Pop,
                    StackInstruction.Local_Get(0))
                .Append(
                    Conditional(
                        PineValue.EmptyList,
                        Ops(
                            StackInstruction.Push_Literal(PineValue.EmptyBlob),
                            StackInstruction.Local_Set(0)),
                        Ops(StackInstruction.Push_Literal(PineValue.EmptyList))))
                .AppendOperation(StackInstruction.Pop)
                .AppendOperation(StackInstruction.Local_Get(2)));

        original.ForwardEquivalentLocalReads().LowerToStackInstructions()
            .Should().Contain(StackInstruction.Local_Get(2));

        foreach (var input in new PineValue[] { PineValue.EmptyList, PineValue.EmptyBlob })
        {
            Evaluate(original.ForwardEquivalentLocalReads(), input).Should()
                .Be(Evaluate(original, input));
        }
    }

    [Fact]
    public void Equivalent_local_reads_transfer_through_stack_block_parameters()
    {
        var original =
            new PineControlFlowGraph(
                new PineBlockId(0),
                [
                    new PineBasicBlock(
                        new PineBlockId(0),
                        [],
                        [new(StackInstruction.Local_Get(0), [], [new PineVirtualValueId(0)])],
                        new PineControlFlowTerminator.Jump(
                            new PineBlockId(1),
                            [new PineVirtualValueId(0)],
                            true)),
                    new PineBasicBlock(
                        new PineBlockId(1),
                        [new PineVirtualValueId(1)],
                        [
                            new(StackInstruction.Local_Set(2), [], []),
                            new(StackInstruction.Pop, [new PineVirtualValueId(1)], []),
                            new(StackInstruction.Local_Get(2), [], [new PineVirtualValueId(2)])
                        ],
                        new PineControlFlowTerminator.Return())
                ]);

        var optimized = original.ForwardEquivalentLocalReads();

        optimized.Blocks[1].Operations[^1].Instruction.Should().Be(StackInstruction.Local_Get(0));

        var input = PineValue.Blob([77]);
        Evaluate(optimized, input).Should().Be(Evaluate(original, input));
    }

    [Fact]
    public void Equivalent_local_reads_do_not_cross_a_join_with_different_sources()
    {
        var original =
            PineControlFlowGraph.FromFragment(
                Ops(StackInstruction.Local_Get(0))
                .Append(
                    Conditional(
                        PineValue.EmptyBlob,
                        Ops(StackInstruction.Local_Get(0), StackInstruction.Local_Set(2)),
                        Ops(
                            StackInstruction.Push_Literal(PineValue.Blob([78])),
                            StackInstruction.Local_Set(2))))
                .AppendOperation(StackInstruction.Pop)
                .AppendOperation(StackInstruction.Local_Get(2)));

        original.ForwardEquivalentLocalReads().LowerToStackInstructions()
            .Should().Contain(StackInstruction.Local_Get(2));

        foreach (var input in new PineValue[] { PineValue.EmptyBlob, PineValue.EmptyList })
        {
            Evaluate(original.ForwardEquivalentLocalReads(), input).Should()
                .Be(Evaluate(original, input));
        }
    }

    [Fact]
    public void Equivalent_local_reads_cross_a_loop_back_edge_only_when_the_source_is_stable()
    {
        var zero = PineValueInProcess.CreateInteger(0).Evaluate();
        var two = PineValueInProcess.CreateInteger(2).Evaluate();

        var original =
            new PineControlFlowGraph(
                new PineBlockId(0),
                [
                    new PineBasicBlock(
                        new PineBlockId(0),
                        [],
                        [
                            new(StackInstruction.Local_Get(0), [], [new PineVirtualValueId(0)]),
                            new(StackInstruction.Local_Set_Descending(2, 1), [], []),
                            new(StackInstruction.Pop, [new PineVirtualValueId(0)], []),
                            new(StackInstruction.Push_Literal(zero), [], [new PineVirtualValueId(1)]),
                            new(StackInstruction.Local_Set(3), [], []),
                            new(StackInstruction.Pop, [new PineVirtualValueId(1)], [])
                        ],
                        new PineControlFlowTerminator.Jump(new PineBlockId(1), [], true)),
                    new PineBasicBlock(
                        new PineBlockId(1),
                        [],
                        [new(StackInstruction.Local_Get(3), [], [new PineVirtualValueId(2)])],
                        new PineControlFlowTerminator.ConditionalJump(
                            new PineBlockId(2),
                            new PineBlockId(3),
                            [],
                            [],
                            two)),
                    new PineBasicBlock(
                        new PineBlockId(2),
                        [],
                        [
                            new(StackInstruction.Local_Get(2), [], [new PineVirtualValueId(3)]),
                            new(StackInstruction.Pop, [new PineVirtualValueId(3)], []),
                            new(StackInstruction.Local_Get(3), [], [new PineVirtualValueId(4)]),
                            new(
                                StackInstruction.Int_Add_Const(1),
                                [new PineVirtualValueId(4)],
                                [new PineVirtualValueId(5)]),
                            new(StackInstruction.Local_Set(3), [], []),
                            new(StackInstruction.Pop, [new PineVirtualValueId(5)], [])
                        ],
                        new PineControlFlowTerminator.Jump(new PineBlockId(1), [], false)),
                    new PineBasicBlock(
                        new PineBlockId(3),
                        [],
                        [new(StackInstruction.Local_Get(2), [], [new PineVirtualValueId(6)])],
                        new PineControlFlowTerminator.Return())
                ]);

        var optimized =
            original.ForwardEquivalentLocalReads()
            .EliminateDeadLocalStores()
            .EliminateDiscardedStackValues(parameterCount: 1);

        optimized.LowerToStackInstructions().Should()
            .NotContain(instruction => instruction.LocalIndex == 2);

        var input = PineValue.List([PineValue.Blob([76])]);
        Evaluate(optimized, input).Should().Be(Evaluate(original, input));
    }

    [Fact]
    public void Discarded_projection_and_local_read_leave_the_return_value_intact()
    {
        var result = PineValue.Blob([91]);

        var original =
            PineControlFlowGraph.FromFragment(
                Ops(
                    StackInstruction.Push_Literal(result),
                    StackInstruction.Local_Get(0),
                    StackInstruction.Head_Generic,
                    StackInstruction.Pop));

        var optimized = original.EliminateDiscardedStackValues(parameterCount: 1);

        optimized.LowerToStackInstructions().Should().Equal(
            StackInstruction.Push_Literal(result),
            StackInstruction.Return);

        foreach (var input in new PineValue[] { PineValue.EmptyList, PineValue.Blob([1]) })
        {
            Evaluate(optimized, input).Should().Be(Evaluate(original, input));
        }
    }

    [Fact]
    public void Discarded_list_build_removes_only_unused_inputs()
    {
        var result = PineValue.Blob([92]);
        var prefix = PineValue.List([PineValue.EmptyBlob, PineValue.EmptyList]);

        var original =
            PineControlFlowGraph.FromFragment(
                Ops(
                    StackInstruction.Push_Literal(result),
                    StackInstruction.Local_Get(0),
                    StackInstruction.Build_List_With_Prefix(prefix, 1),
                    StackInstruction.Pop,
                    StackInstruction.Push_Literal(PineValue.EmptyBlob),
                    StackInstruction.Local_Get(0),
                    StackInstruction.Build_List(2),
                    StackInstruction.Pop));

        var optimized = original.EliminateDiscardedStackValues(parameterCount: 1);

        optimized.LowerToStackInstructions().Should().Equal(
            StackInstruction.Push_Literal(result),
            StackInstruction.Return);

        Evaluate(optimized, PineValue.EmptyBlob).Should().Be(Evaluate(original, PineValue.EmptyBlob));
    }

    [Fact]
    public void Discarded_result_keeps_uninitialized_local_read_and_observed_store()
    {
        var result = PineValue.Blob([93]);

        var uninitialized =
            PineControlFlowGraph.FromFragment(
                Ops(
                    StackInstruction.Push_Literal(result),
                    StackInstruction.Local_Get(2),
                    StackInstruction.Head_Generic,
                    StackInstruction.Pop));

        uninitialized.EliminateDiscardedStackValues(parameterCount: 1)
            .LowerToStackInstructions().Should().Equal(
            StackInstruction.Push_Literal(result),
            StackInstruction.Local_Get(2),
            StackInstruction.Pop,
            StackInstruction.Return);

        var observedStore =
            PineControlFlowGraph.FromFragment(
                Ops(
                    StackInstruction.Local_Get(0),
                    StackInstruction.Local_Set(2),
                    StackInstruction.Pop,
                    StackInstruction.Local_Get(2)));

        observedStore.EliminateDiscardedStackValues(parameterCount: 1)
            .LowerToStackInstructions().Should().Equal(observedStore.LowerToStackInstructions());

        Evaluate(observedStore.EliminateDiscardedStackValues(1), result).Should()
            .Be(Evaluate(observedStore, result));
    }

    [Fact]
    public void Discarded_local_read_requires_initialization_on_every_incoming_path()
    {
        var result = PineValue.Blob([94]);

        var original =
            PineControlFlowGraph.FromFragment(
                Ops(StackInstruction.Local_Get(0))
                .Append(
                    Conditional(
                        PineKernelValues.TrueValue,
                        Ops(StackInstruction.Push_Literal(result), StackInstruction.Local_Set(2)),
                        Ops(StackInstruction.Push_Literal(result))))
                .AppendOperation(StackInstruction.Pop)
                .AppendOperation(StackInstruction.Push_Literal(result))
                .AppendOperation(StackInstruction.Local_Get(2))
                .AppendOperation(StackInstruction.Pop));

        original.EliminateDiscardedStackValues(parameterCount: 1)
            .LowerToStackInstructions().Should().Contain(StackInstruction.Local_Get(2));
    }

    [Fact]
    public void Discarded_local_read_is_removed_after_both_paths_initialize_the_slot()
    {
        var result = PineValue.Blob([96]);

        var original =
            PineControlFlowGraph.FromFragment(
                Ops(StackInstruction.Local_Get(0))
                .Append(
                    Conditional(
                        PineKernelValues.TrueValue,
                        Ops(StackInstruction.Local_Get(0), StackInstruction.Local_Set(2)),
                        Ops(StackInstruction.Local_Get(0), StackInstruction.Local_Set_Descending(2, 1))))
                .AppendOperation(StackInstruction.Pop)
                .AppendOperation(StackInstruction.Push_Literal(result))
                .AppendOperation(StackInstruction.Local_Get(2))
                .AppendOperation(StackInstruction.Pop));

        var optimized = original.EliminateDiscardedStackValues(parameterCount: 1);

        optimized.LowerToStackInstructions().Should()
            .NotContain(StackInstruction.Local_Get(2));

        foreach (var input in new PineValue[] { PineKernelValues.TrueValue, PineKernelValues.FalseValue })
        {
            Evaluate(optimized, input).Should().Be(Evaluate(original, input));
        }
    }

    [Fact]
    public void Adjacent_explicit_jumps_and_empty_forwarders_are_removed()
    {
        var result = PineValue.Blob([95]);
        var valueId = new PineVirtualValueId(0);

        var original =
            new PineControlFlowGraph(
                new PineBlockId(0),
                [
                    new PineBasicBlock(
                        new PineBlockId(0),
                        [],
                        [new(StackInstruction.Push_Literal(result), [], [valueId])],
                        new PineControlFlowTerminator.Jump(new PineBlockId(1), [valueId], false)),
                    new PineBasicBlock(
                        new PineBlockId(1),
                        [new PineVirtualValueId(1)],
                        [],
                        new PineControlFlowTerminator.Jump(
                            new PineBlockId(2),
                            [new PineVirtualValueId(1)],
                            false)),
                    new PineBasicBlock(
                        new PineBlockId(2),
                        [new PineVirtualValueId(2)],
                        [],
                        new PineControlFlowTerminator.Return())
                ]);

        var optimized = original.RemoveRedundantForwardJumps();

        optimized.LowerToStackInstructions().Should().Equal(
            StackInstruction.Push_Literal(result),
            StackInstruction.Return);

        Evaluate(optimized, PineValue.EmptyList).Should().Be(Evaluate(original, PineValue.EmptyList));
    }

    [Fact]
    public void Value_facts_cross_blocks_and_local_projections()
    {
        var tag = PineValue.Blob([10]);
        var otherTag = PineValue.Blob([11]);
        var known = PineValue.List([tag, PineValue.Blob([12])]);
        var matched = PineValue.Blob([20]);
        var unmatched = PineValue.Blob([21]);

        var original =
            PineControlFlowGraph.FromFragment(
                Ops(
                    StackInstruction.Push_Literal(known),
                    StackInstruction.Local_Set(2),
                    StackInstruction.Pop,
                    StackInstruction.Local_Get(0))
                .Append(
                    Conditional(
                        tag,
                        Ops(StackInstruction.Push_Literal(tag)),
                        Ops(StackInstruction.Push_Literal(tag))))
                .AppendOperation(StackInstruction.Pop)
                .AppendOperation(StackInstruction.Local_Get_Skip_Head_Const(2, 0))
                .Append(
                    Conditional(
                        tag,
                        Ops(StackInstruction.Push_Literal(unmatched)),
                        Ops(StackInstruction.Push_Literal(matched)))));

        var optimized = original.ForwardProvenEqualityBranches();

        optimized.LowerToStackInstructions().Should()
            .NotContain(StackInstruction.Local_Get_Skip_Head_Const(2, 0));

        optimized.Blocks.Select(block => block.Terminator)
            .OfType<PineControlFlowTerminator.ConditionalJump>().Should().ContainSingle();

        foreach (var input in new[] { tag, otherTag })
        {
            Evaluate(optimized, input).Should().Be(matched);
            Evaluate(optimized, input).Should().Be(Evaluate(original, input));
        }
    }

    [Fact]
    public void Value_facts_meet_different_local_definitions_as_unknown()
    {
        var first = PineValue.Blob([30]);
        var second = PineValue.Blob([31]);

        var original =
            PineControlFlowGraph.FromFragment(
                Ops(StackInstruction.Local_Get(0))
                .Append(
                    Conditional(
                        first,
                        Ops(StackInstruction.Push_Literal(first), StackInstruction.Local_Set(2)),
                        Ops(StackInstruction.Push_Literal(second), StackInstruction.Local_Set(2))))
                .AppendOperation(StackInstruction.Pop)
                .AppendOperation(StackInstruction.Local_Get(2))
                .Append(
                    Conditional(
                        first,
                        Ops(StackInstruction.Push_Literal(second)),
                        Ops(StackInstruction.Push_Literal(first)))));

        var optimized = original.ForwardProvenEqualityBranches();

        optimized.Blocks.Select(block => block.Terminator)
            .OfType<PineControlFlowTerminator.ConditionalJump>().Should().HaveCount(2);

        foreach (var input in new[] { first, second })
        {
            Evaluate(optimized, input).Should().Be(Evaluate(original, input));
        }
    }

    [Fact]
    public void Value_facts_follow_descending_local_writes_and_switches()
    {
        var tag = PineValue.List([PineValue.Blob([40])]);
        var known = PineValue.List([PineValue.List([PineValue.Blob([40])])]);
        var result = PineValue.Blob([41]);

        var original =
            PineControlFlowGraph.FromFragment(
                Ops(
                    StackInstruction.Push_Literal(known),
                    StackInstruction.Push_Literal(PineValue.EmptyBlob),
                    StackInstruction.Local_Set_Descending(4, 2),
                    StackInstruction.PopMultiple(2),
                    StackInstruction.Local_Get_Skip_Head_Const(3, 0))
                .Append(
                    Switch(
                        Ops(StackInstruction.Push_Literal(PineValue.EmptyList)),
                        (tag, Ops(StackInstruction.Push_Literal(result))))));

        var optimized = original.ForwardProvenEqualityBranches();

        optimized.LowerToStackInstructions().Should()
            .NotContain(StackInstruction.Local_Get_Skip_Head_Const(3, 0));

        optimized.Blocks.Select(block => block.Terminator)
            .Should().NotContain(terminator => terminator is PineControlFlowTerminator.Switch);

        Evaluate(optimized, PineValue.EmptyList).Should().Be(result);
        Evaluate(optimized, PineValue.EmptyList).Should().Be(Evaluate(original, PineValue.EmptyList));
    }

    [Fact]
    public void Value_facts_drop_loop_carried_local_constants_on_unknown_back_edges()
    {
        var first = PineValue.Blob([50]);
        var next = PineValue.Blob([51]);
        var result = PineValue.Blob([52]);

        var original =
            new PineControlFlowGraph(
                new PineBlockId(0),
                [
                new PineBasicBlock(
                    new PineBlockId(0),
                    [],
                    [
                    new(StackInstruction.Push_Literal(first), [], [new PineVirtualValueId(0)]),
                    new(StackInstruction.Local_Set(2), [], []),
                    new(StackInstruction.Pop, [new PineVirtualValueId(0)], [])
                    ],
                    new PineControlFlowTerminator.Jump(new PineBlockId(1), [], true)),
                new PineBasicBlock(
                    new PineBlockId(1),
                    [],
                    [new(StackInstruction.Local_Get(2), [], [new PineVirtualValueId(1)])],
                    new PineControlFlowTerminator.ConditionalJump(
                        new PineBlockId(2),
                        new PineBlockId(3),
                        [],
                        [],
                        first)),
                new PineBasicBlock(
                    new PineBlockId(2),
                    [],
                    [new(StackInstruction.Push_Literal(result), [], [new PineVirtualValueId(2)])],
                    new PineControlFlowTerminator.Return()),
                new PineBasicBlock(
                    new PineBlockId(3),
                    [],
                    [
                    new(StackInstruction.Local_Get(0), [], [new PineVirtualValueId(3)]),
                    new(StackInstruction.Local_Set(2), [], []),
                    new(StackInstruction.Pop, [new PineVirtualValueId(3)], [])
                    ],
                    new PineControlFlowTerminator.Jump(new PineBlockId(1), [], false))
                ]);

        var optimized = original.ForwardProvenEqualityBranches();

        optimized.Blocks[1].Terminator.Should()
            .BeOfType<PineControlFlowTerminator.ConditionalJump>();

        Evaluate(optimized, next).Should().Be(result);
        Evaluate(optimized, next).Should().Be(Evaluate(original, next));
    }

    [Fact]
    public void Value_facts_consume_projected_stack_input_before_a_taken_branch()
    {
        var tag = PineValue.Blob([60]);
        var result = PineValue.Blob([61]);

        var original =
            PineControlFlowGraph.FromFragment(
                Ops(StackInstruction.Push_Literal(PineValue.List([tag])), StackInstruction.Head_Generic)
                .Append(
                    Conditional(
                        tag,
                        Ops(StackInstruction.Push_Literal(PineValue.EmptyList)),
                        Ops(StackInstruction.Push_Literal(result)))));

        var optimized = original.ForwardProvenEqualityBranches();

        optimized.LowerToStackInstructions().Should().Contain(StackInstruction.Pop);
        optimized.LowerToStackInstructions().Should().NotContain(StackInstruction.Head_Generic);
        Evaluate(optimized, PineValue.EmptyList).Should().Be(result);
        Evaluate(optimized, PineValue.EmptyList).Should().Be(Evaluate(original, PineValue.EmptyList));
    }

    [Fact]
    public void List_length_fact_survives_stack_join_and_local_store()
    {
        var result = PineValue.Blob([71]);
        var unreachable = PineValue.Blob([72]);

        var original =
            PineControlFlowGraph.FromFragment(
                Ops(StackInstruction.Local_Get(0))
                .Append(
                    Conditional(
                        PineKernelValues.TrueValue,
                        Ops(StackInstruction.Local_Get(0), StackInstruction.Build_List(1)),
                        Ops(
                            StackInstruction.Push_Literal(PineValue.EmptyBlob),
                            StackInstruction.Build_List(1))))
                .AppendOperation(StackInstruction.Local_Set(2))
                .Append(
                    Conditional(
                        PineValue.EmptyList,
                        Ops(StackInstruction.Push_Literal(result)),
                        Ops(StackInstruction.Push_Literal(unreachable)))));

        var optimized = original.ForwardProvenEqualityBranches();

        optimized.Blocks.Select(block => block.Terminator)
            .OfType<PineControlFlowTerminator.ConditionalJump>().Should().ContainSingle();

        foreach (var input in new PineValue[] { PineKernelValues.TrueValue, PineKernelValues.FalseValue })
        {
            Evaluate(optimized, input).Should().Be(result);
            Evaluate(optimized, input).Should().Be(Evaluate(original, input));
        }
    }

    [Fact]
    public void List_shape_facts_survive_join_of_literal_and_constructed_list()
    {
        var tag = PineValue.Blob([79]);
        var result = PineValue.Blob([80]);

        var original =
            PineControlFlowGraph.FromFragment(
                Ops(StackInstruction.Local_Get(0))
                .Append(
                    Conditional(
                        PineKernelValues.TrueValue,
                        Ops(StackInstruction.Push_Literal(PineValue.List([tag, PineValue.EmptyBlob]))),
                        Ops(
                            StackInstruction.Local_Get(0),
                            StackInstruction.Build_List_With_Prefix(PineValue.List([tag]), 1))))
                .AppendOperation(StackInstruction.Local_Set(2))
                .AppendOperation(StackInstruction.Pop)
                .AppendOperation(StackInstruction.Local_Get_Skip_Head_Const(2, 0))
                .Append(
                    Conditional(
                        tag,
                        Ops(StackInstruction.Push_Literal(PineValue.EmptyList)),
                        Ops(StackInstruction.Push_Literal(result)))));

        var optimized = original.ForwardProvenEqualityBranches();

        optimized.Blocks.Select(block => block.Terminator)
            .OfType<PineControlFlowTerminator.ConditionalJump>().Should().ContainSingle();

        foreach (var input in new PineValue[] { PineKernelValues.TrueValue, PineKernelValues.FalseValue })
        {
            Evaluate(optimized, input).Should().Be(result);
            Evaluate(optimized, input).Should().Be(Evaluate(original, input));
        }
    }

    [Fact]
    public void List_shape_equality_requires_every_element_to_be_known()
    {
        var tag = PineValue.Blob([81]);
        var value = PineValue.Blob([82]);
        var result = PineValue.Blob([83]);

        var original =
            PineControlFlowGraph.FromFragment(
                Ops(
                    StackInstruction.Push_Literal(tag),
                    StackInstruction.Local_Get(0),
                    StackInstruction.Build_List(2))
                .Append(
                    Conditional(
                        PineValue.List([tag, value]),
                        Ops(StackInstruction.Push_Literal(PineValue.EmptyList)),
                        Ops(StackInstruction.Push_Literal(result)))));

        var unknown = original.ForwardProvenEqualityBranches();

        unknown.Blocks.Select(block => block.Terminator)
            .OfType<PineControlFlowTerminator.ConditionalJump>().Should().ContainSingle();

        foreach (var input in new PineValue[] { value, PineValue.EmptyBlob })
        {
            Evaluate(unknown, input).Should().Be(Evaluate(original, input));
        }

        var known =
            PineControlFlowGraph.FromFragment(
                Ops(
                    StackInstruction.Push_Literal(tag),
                    StackInstruction.Push_Literal(value),
                    StackInstruction.Build_List(2))
                .Append(
                    Conditional(
                        PineValue.List([tag, value]),
                        Ops(StackInstruction.Push_Literal(PineValue.EmptyList)),
                        Ops(StackInstruction.Push_Literal(result)))));

        var optimized = known.ForwardProvenEqualityBranches();

        optimized.Blocks.Select(block => block.Terminator)
            .Should().NotContain(terminator => terminator is PineControlFlowTerminator.ConditionalJump);

        Evaluate(optimized, PineValue.EmptyList).Should().Be(result);
        Evaluate(optimized, PineValue.EmptyList).Should().Be(Evaluate(known, PineValue.EmptyList));
    }

    [Fact]
    public void List_prefix_fact_proves_unequal_case_without_knowing_dynamic_element()
    {
        var tag = PineValue.Blob([73]);
        var otherTag = PineValue.Blob([74]);
        var result = PineValue.Blob([75]);

        var original =
            PineControlFlowGraph.FromFragment(
                Ops(
                    StackInstruction.Local_Get(0),
                    StackInstruction.Build_List_With_Prefix(PineValue.List([tag]), 1),
                    StackInstruction.Local_Set(2))
                .Append(
                    Switch(
                        Ops(StackInstruction.Push_Literal(result)),
                        (PineValue.List([otherTag, PineValue.EmptyBlob]),
                        Ops(StackInstruction.Push_Literal(PineValue.EmptyList))))));

        var optimized = original.ForwardProvenEqualityBranches();

        optimized.Blocks.Select(block => block.Terminator)
            .Should().NotContain(terminator => terminator is PineControlFlowTerminator.Switch);

        foreach (var input in new PineValue[] { PineValue.EmptyBlob, PineValue.List([tag]) })
        {
            Evaluate(optimized, input).Should().Be(result);
            Evaluate(optimized, input).Should().Be(Evaluate(original, input));
        }
    }

    [Fact]
    public void List_shape_switch_requires_a_proven_case_or_all_cases_disproven()
    {
        var tag = PineValue.Blob([84]);
        var item = PineValue.Blob([85]);
        var result = PineValue.Blob([86]);
        var fallback = PineValue.Blob([87]);

        PineControlFlowGraph BuildGraph(StackInstruction input) =>
            PineControlFlowGraph.FromFragment(
                Ops(
                    input,
                    StackInstruction.Build_List_With_Prefix(PineValue.List([tag]), 1))
                .Append(
                    Switch(
                        Ops(StackInstruction.Push_Literal(fallback)),
                        (PineValue.EmptyList, Ops(StackInstruction.Push_Literal(PineValue.EmptyBlob))),
                        (PineValue.List([tag, item]), Ops(StackInstruction.Push_Literal(result))))));

        var unknown = BuildGraph(StackInstruction.Local_Get(0));
        var optimizedUnknown = unknown.ForwardProvenEqualityBranches();

        optimizedUnknown.Blocks.Select(block => block.Terminator)
            .Should().Contain(terminator => terminator is PineControlFlowTerminator.Switch);

        foreach (var input in new PineValue[] { item, PineValue.EmptyBlob })
        {
            Evaluate(optimizedUnknown, input).Should().Be(Evaluate(unknown, input));
        }

        var known = BuildGraph(StackInstruction.Push_Literal(item));
        var optimizedKnown = known.ForwardProvenEqualityBranches();

        optimizedKnown.Blocks.Select(block => block.Terminator)
            .Should().NotContain(terminator => terminator is PineControlFlowTerminator.Switch);

        Evaluate(optimizedKnown, PineValue.EmptyBlob).Should().Be(result);
        Evaluate(optimizedKnown, PineValue.EmptyBlob).Should().Be(Evaluate(known, PineValue.EmptyBlob));
    }

    [Fact]
    public void List_prefix_fact_projects_across_local_without_materializing_an_exact_list()
    {
        var tag = PineValue.Blob([76]);
        var result = PineValue.Blob([77]);

        var original =
            PineControlFlowGraph.FromFragment(
                Ops(
                    StackInstruction.Local_Get(0),
                    StackInstruction.Build_List_With_Prefix(PineValue.List([tag]), 1),
                    StackInstruction.Local_Set(2),
                    StackInstruction.Pop,
                    StackInstruction.Local_Get_Skip_Head_Const(2, 0))
                .Append(
                    Conditional(
                        tag,
                        Ops(StackInstruction.Push_Literal(PineValue.EmptyList)),
                        Ops(StackInstruction.Push_Literal(result)))));

        var optimized = original.ForwardProvenEqualityBranches();

        optimized.LowerToStackInstructions().Should()
            .NotContain(StackInstruction.Local_Get_Skip_Head_Const(2, 0));

        foreach (var input in new PineValue[] { PineValue.EmptyBlob, PineValue.EmptyList })
        {
            Evaluate(optimized, input).Should().Be(result);
            Evaluate(optimized, input).Should().Be(Evaluate(original, input));
        }
    }

    [Fact]
    public void List_length_fact_folds_length_test_but_not_an_unknown_local_reassignment()
    {
        var result = PineValue.Blob([78]);

        var lengthTest =
            PineControlFlowGraph.FromFragment(
                Ops(
                    StackInstruction.Local_Get(0),
                    StackInstruction.Build_List(1),
                    StackInstruction.Length_Equal_Const(1))
                .Append(
                    Conditional(
                        PineKernelValues.TrueValue,
                        Ops(StackInstruction.Push_Literal(PineValue.EmptyList)),
                        Ops(StackInstruction.Push_Literal(result)))));

        var optimizedLengthTest = lengthTest.ForwardProvenEqualityBranches();

        optimizedLengthTest.LowerToStackInstructions().Should()
            .NotContain(StackInstruction.Length_Equal_Const(1));

        Evaluate(optimizedLengthTest, PineValue.EmptyBlob).Should().Be(result);

        Evaluate(optimizedLengthTest, PineValue.EmptyBlob)
            .Should().Be(Evaluate(lengthTest, PineValue.EmptyBlob));

        var overwritten =
            PineControlFlowGraph.FromFragment(
                Ops(
                    StackInstruction.Local_Get(0),
                    StackInstruction.Build_List(1),
                    StackInstruction.Local_Set(2),
                    StackInstruction.Pop,
                    StackInstruction.Local_Get(0),
                    StackInstruction.Local_Set(2),
                    StackInstruction.Pop,
                    StackInstruction.Local_Get(2))
                .Append(
                    Conditional(
                        PineValue.EmptyList,
                        Ops(StackInstruction.Push_Literal(result)),
                        Ops(StackInstruction.Push_Literal(PineValue.EmptyBlob)))));

        overwritten.ForwardProvenEqualityBranches().Blocks.Select(block => block.Terminator)
            .OfType<PineControlFlowTerminator.ConditionalJump>().Should().ContainSingle();

        foreach (var input in new PineValue[] { PineValue.EmptyList, PineValue.EmptyBlob })
        {
            Evaluate(overwritten.ForwardProvenEqualityBranches(), input).Should()
                .Be(Evaluate(overwritten, input));
        }
    }

    [Fact]
    public void Dead_local_store_preserves_its_stack_value_and_result()
    {
        var input = PineValue.Blob([11]);

        var original =
            PineControlFlowGraph.FromFragment(
                Ops(StackInstruction.Local_Get(0), StackInstruction.Local_Set(3)));

        var optimized = original.EliminateDeadLocalStores();

        optimized.LowerToStackInstructions().Should().Equal(
            StackInstruction.Local_Get(0),
            StackInstruction.Return);

        Evaluate(optimized, input).Should().Be(Evaluate(original, input));
    }

    [Fact]
    public void Dead_local_store_preserves_effectful_invocation()
    {
        var original =
            PineControlFlowGraph.FromFragment(
                Ops(
                    StackInstruction.Push_Literal(PineValue.EmptyList),
                    StackInstruction.Push_Literal(PineValue.EmptyList),
                    StackInstruction.Eval_Binary,
                    StackInstruction.Local_Set(3)));

        var optimized = original.EliminateDeadLocalStores();

        optimized.LowerToStackInstructions().Should().Equal(
            StackInstruction.Push_Literal(PineValue.EmptyList),
            StackInstruction.Push_Literal(PineValue.EmptyList),
            StackInstruction.Eval_Binary,
            StackInstruction.Return);
    }

    [Fact]
    public void Dead_local_store_tracks_reads_across_branches_and_overwrites()
    {
        var original =
            PineControlFlowGraph.FromFragment(
                Ops(
                    StackInstruction.Local_Get(0),
                    StackInstruction.Local_Set(2),
                    StackInstruction.Local_Set(3),
                    StackInstruction.Pop,
                    StackInstruction.Push_Literal(PineKernelValues.TrueValue))
                .Append(
                    Conditional(
                        PineKernelValues.TrueValue,
                        Ops(
                            StackInstruction.Local_Get(0),
                            StackInstruction.Local_Set(2),
                            StackInstruction.Pop,
                            StackInstruction.Local_Get(2)),
                        Ops(StackInstruction.Local_Get(2)))));

        var optimized = original.EliminateDeadLocalStores();

        optimized.LowerToStackInstructions().Should().NotContain(
            instruction => instruction.Kind == StackInstructionKind.Local_Set &&
                instruction.LocalIndex == 3);

        optimized.LowerToStackInstructions().Count(
            instruction => instruction.Kind == StackInstructionKind.Local_Set &&
                instruction.LocalIndex == 2).Should().Be(2);

        Evaluate(optimized, PineValue.Blob([1])).Should()
            .Be(Evaluate(original, PineValue.Blob([1])));
    }

    [Fact]
    public void Dead_descending_store_is_removed_only_when_all_of_its_slots_are_dead()
    {
        var original =
            PineControlFlowGraph.FromFragment(
                Ops(
                    StackInstruction.Local_Get(0),
                    StackInstruction.Local_Get(0),
                    StackInstruction.Local_Set_Descending(3, 2),
                    StackInstruction.PopMultiple(2),
                    StackInstruction.Local_Get(0)));

        var optimized = original.EliminateDeadLocalStores();

        optimized.LowerToStackInstructions().Should().NotContain(
            instruction => instruction.Kind == StackInstructionKind.Local_Set_Descending);

        Evaluate(optimized, PineValue.Blob([11])).Should()
            .Be(Evaluate(original, PineValue.Blob([11])));

        var partiallyLive =
            PineControlFlowGraph.FromFragment(
                Ops(
                    StackInstruction.Local_Get(0),
                    StackInstruction.Local_Get(0),
                    StackInstruction.Local_Set_Descending(3, 2),
                    StackInstruction.PopMultiple(2),
                    StackInstruction.Local_Get(2)));

        partiallyLive.EliminateDeadLocalStores().LowerToStackInstructions()
            .Should().Contain(StackInstruction.Local_Set_Descending(3, 2));
    }

    [Theory]
    [InlineData(5, 1)]
    [InlineData(4, 2)]
    public void Partially_dead_descending_store_trims_only_unread_bottom_slots(
        int readLocal,
        int expectedCount)
    {
        var original =
            PineControlFlowGraph.FromFragment(
                Ops(
                    StackInstruction.Push_Literal(PineValue.Blob([1])),
                    StackInstruction.Push_Literal(PineValue.Blob([2])),
                    StackInstruction.Push_Literal(PineValue.Blob([3])),
                    StackInstruction.Local_Set_Descending(5, 3),
                    StackInstruction.PopMultiple(3),
                    StackInstruction.Local_Get(readLocal)));

        var optimized = original.EliminateDeadLocalStores();

        optimized.LowerToStackInstructions().Should()
            .Contain(StackInstruction.Local_Set_Descending(5, expectedCount));

        optimized.LowerToStackInstructions().Should().Contain(StackInstruction.PopMultiple(3));
        Evaluate(optimized, PineValue.EmptyList).Should().Be(Evaluate(original, PineValue.EmptyList));
    }

    [Fact]
    public void Partially_dead_descending_store_keeps_bottom_slot_when_read()
    {
        var original =
            PineControlFlowGraph.FromFragment(
                Ops(
                    StackInstruction.Local_Get(0),
                    StackInstruction.Local_Get(0),
                    StackInstruction.Local_Set_Descending(5, 2),
                    StackInstruction.PopMultiple(2),
                    StackInstruction.Local_Get(4)));

        original.EliminateDeadLocalStores().LowerToStackInstructions().Should()
            .Contain(StackInstruction.Local_Set_Descending(5, 2));
    }

    [Fact]
    public void Dead_local_store_liveness_converges_across_loop_back_edges()
    {
        var original =
            PineControlFlowGraph.FromFragment(
                Ops(
                    StackInstruction.Local_Get(0),
                    StackInstruction.Local_Set(2),
                    StackInstruction.Pop,
                    StackInstruction.Local_Get(0),
                    StackInstruction.Local_Set(3),
                    StackInstruction.Pop,
                    StackInstruction.Local_Get(0))
                .Append(
                    Conditional(
                        PineKernelValues.TrueValue,
                        Ops(StackInstruction.Local_Get(2)),
                        PineControlFlowFragment.Empty.Append(new PineControlFlowNode.JumpToEntry()))));

        var optimized = original.EliminateDeadLocalStores().LowerToStackInstructions();

        optimized.Should().Contain(StackInstruction.Local_Set(2));
        optimized.Should().NotContain(StackInstruction.Local_Set(3));
    }

    [Fact]
    public void Remove_empty_forwarding_blocks_after_dead_store_elimination()
    {
        var original =
            PineControlFlowGraph.FromFragment(
                Ops(
                    StackInstruction.Local_Get(0),
                    StackInstruction.Push_Literal(PineKernelValues.TrueValue))
                .Append(
                    Conditional(
                        PineKernelValues.TrueValue,
                        Ops(StackInstruction.Local_Set(2)),
                        Ops(StackInstruction.Local_Set(3)))));

        var withoutStores = original.EliminateDeadLocalStores();
        var optimized = withoutStores.RemoveEmptyForwardingBlocks();

        optimized.Blocks.Length.Should().BeLessThan(withoutStores.Blocks.Length);

        Evaluate(optimized, PineValue.Blob([11])).Should()
            .Be(Evaluate(original, PineValue.Blob([11])));
    }

    [Fact]
    public void Remove_unreachable_blocks_preserves_conditional_fallthrough_and_jump_targets()
    {
        var falseResult = PineValue.Blob([11]);
        var trueResult = PineValue.Blob([22]);

        PineBasicBlock ResultBlock(int id, PineValue value) =>
            new(
                new PineBlockId(id),
                [],
                [
                new PineControlFlowOperation(
                    StackInstruction.Push_Literal(value),
                    [],
                    [new PineVirtualValueId(id)])
                ],
                new PineControlFlowTerminator.Return());

        var original =
            new PineControlFlowGraph(
                new PineBlockId(0),
                [
                    new PineBasicBlock(
                        new PineBlockId(0),
                        [],
                        [
                        new PineControlFlowOperation(
                            StackInstruction.Local_Get(0),
                            [],
                            [new PineVirtualValueId(0)])
                        ],
                        new PineControlFlowTerminator.ConditionalJump(
                            new PineBlockId(1),
                            new PineBlockId(3),
                            [],
                            [],
                            PineKernelValues.TrueValue)),
                    ResultBlock(1, falseResult),
                    ResultBlock(2, PineValue.Blob([33])),
                    ResultBlock(3, trueResult)
                ]);

        original.Validate();
        var optimized = original.RemoveUnreachableBlocks();

        optimized.Blocks.Should().HaveCount(3);

        optimized.Blocks[0].Terminator.Should()
            .BeOfType<PineControlFlowTerminator.ConditionalJump>()
            .Which.Branch.Should().Be(new PineBlockId(2));

        foreach (var condition in new[] { PineKernelValues.FalseValue, PineKernelValues.TrueValue })
        {
            Evaluate(optimized, condition).Should().Be(Evaluate(original, condition));
        }

        optimized.RemoveUnreachableBlocks().Should().BeSameAs(optimized);
    }

    [Theory]
    [InlineData(0)]
    [InlineData(2)]
    [InlineData(6)]
    public void Fuse_local_list_projection_preserves_result_without_building_a_list(int index)
    {
        var items = Enumerable.Range(0, 7).Select(i => PineValue.Blob([(byte)i])).ToArray();

        var original =
            PineControlFlowGraph.FromFragment(
                Ops(
                    StackInstruction.Local_Get(0),
                    StackInstruction.Skip_Head_Const(index)));

        var optimized = original.FuseLocalListProjections();

        optimized.LowerToStackInstructions().Should().Equal(
            StackInstruction.Local_Get_Skip_Head_Const(0, index),
            StackInstruction.Return);

        Evaluate(optimized, PineValue.List(items)).Should()
            .Be(Evaluate(original, PineValue.List(items)));
    }

    [Fact]
    public void Fuse_local_list_projection_does_not_cross_an_intervening_operation()
    {
        var original =
            PineControlFlowGraph.FromFragment(
                Ops(
                    StackInstruction.Local_Get(0),
                    StackInstruction.Local_Set(1),
                    StackInstruction.Skip_Head_Const(1)));

        original.FuseLocalListProjections().LowerToStackInstructions()
            .Should().Equal(original.LowerToStackInstructions());
    }

    [Theory]
    [InlineData(0)]
    [InlineData(2)]
    [InlineData(4)]
    public void Eliminate_local_copy_and_fuse_repeated_projections(int index)
    {
        var input =
            PineValue.List(
                [.. Enumerable.Range(0, 5).Select(i => PineValue.Blob([(byte)i]))]);

        var original =
            PineControlFlowGraph.FromFragment(
                Ops(
                    StackInstruction.Local_Get(0),
                    StackInstruction.Local_Set_Descending(2, 1),
                    StackInstruction.Pop,
                    StackInstruction.Local_Get(2),
                    StackInstruction.Skip_Head_Const(index),
                    StackInstruction.Local_Get(2),
                    StackInstruction.Skip_Head_Const(1),
                    StackInstruction.Build_List(2)));

        var optimized = original.EliminateLocalCopies().FuseLocalListProjections();

        optimized.LowerToStackInstructions().Should().Equal(
            StackInstruction.Local_Get_Skip_Head_Const(0, index),
            StackInstruction.Local_Get_Skip_Head_Const(0, 1),
            StackInstruction.Build_List(2),
            StackInstruction.Return);

        Evaluate(optimized, input).Should().Be(Evaluate(original, input));
    }

    [Fact]
    public void Eliminate_local_copy_preserves_alias_when_source_is_overwritten()
    {
        var original =
            PineControlFlowGraph.FromFragment(
                Ops(
                    StackInstruction.Local_Get(0),
                    StackInstruction.Local_Set_Descending(2, 1),
                    StackInstruction.Pop,
                    StackInstruction.Push_Literal(PineValue.EmptyList),
                    StackInstruction.Local_Set(0),
                    StackInstruction.Pop,
                    StackInstruction.Local_Get(2),
                    StackInstruction.Skip_Head_Const(1)));

        original.EliminateLocalCopies().LowerToStackInstructions()
            .Should().Equal(original.LowerToStackInstructions());
    }

    [Fact]
    public void Eliminate_local_copy_preserves_alias_when_descending_write_overwrites_source()
    {
        var input = PineValue.List([PineValue.Blob([11]), PineValue.Blob([22])]);

        var original =
            PineControlFlowGraph.FromFragment(
                Ops(
                    StackInstruction.Local_Get(0),
                    StackInstruction.Local_Set_Descending(3, 1),
                    StackInstruction.Pop,
                    StackInstruction.Push_Literal(PineValue.EmptyBlob),
                    StackInstruction.Push_Literal(PineValue.EmptyList),
                    StackInstruction.Local_Set_Descending(1, 2),
                    StackInstruction.PopMultiple(2),
                    StackInstruction.Local_Get_Skip_Head_Const(3, 1)));

        original.EliminateLocalCopies().LowerToStackInstructions()
            .Should().Equal(original.LowerToStackInstructions());

        Evaluate(original.EliminateLocalCopies(), input).Should().Be(Evaluate(original, input));
    }

    [Fact]
    public void Scalar_replacement_and_local_copy_elimination_share_an_existing_local()
    {
        var item = PineValue.Blob([11]);

        var original =
            PineControlFlowGraph.FromFragment(
                Ops(
                    StackInstruction.Local_Get(0),
                    StackInstruction.Build_List(1),
                    StackInstruction.Skip_Head_Const(0)));

        var optimized =
            original
            .ReplaceNonEscapingLists(parameterCount: 1)
            .EliminateLocalCopies()
            .FuseLocalListProjections();

        optimized.LowerToStackInstructions().Should().NotContain(
            instruction =>
            instruction.Kind == StackInstructionKind.Build_List ||
            instruction.Kind == StackInstructionKind.Local_Set_Descending);

        Evaluate(optimized, item).Should().Be(Evaluate(original, item));
    }

    [Fact]
    public void Eliminate_local_copy_preserves_alias_used_in_another_block()
    {
        var original =
            PineControlFlowGraph.FromFragment(
                Ops(
                    StackInstruction.Local_Get(0),
                    StackInstruction.Local_Set_Descending(2, 1),
                    StackInstruction.Pop,
                    StackInstruction.Push_Literal(PineKernelValues.TrueValue))
                .Append(
                    Conditional(
                        PineKernelValues.TrueValue,
                        Ops(StackInstruction.Local_Get_Skip_Head_Const(2, 0)),
                        Ops(StackInstruction.Local_Get_Skip_Head_Const(2, 1)))));

        original.EliminateLocalCopies().LowerToStackInstructions()
            .Should().Equal(original.LowerToStackInstructions());
    }

    [Fact]
    public void Invoke_ends_block_and_continues_in_next_block()
    {
        var graph =
            PineControlFlowGraph.FromFragment(
                Ops(
                    StackInstruction.Local_Get(0),
                    StackInstruction.Local_Get(1),
                    StackInstruction.Eval_Binary));

        graph.Blocks.Should().HaveCount(2);

        var invoke =
            graph.Blocks[0].Terminator.Should().BeOfType<PineControlFlowTerminator.Invoke>().Subject;

        invoke.Continuation.Should().Be(new PineBlockId(1));
        invoke.Arguments.Should().HaveCount(1);
        graph.Blocks[1].Parameters.Should().HaveSameCount(invoke.Arguments);
        graph.Blocks[1].Terminator.Should().BeOfType<PineControlFlowTerminator.Return>();

        graph.LowerToStackInstructions().Should().Equal(
            StackInstruction.Local_Get(0),
            StackInstruction.Local_Get(1),
            StackInstruction.Eval_Binary,
            StackInstruction.Return);
    }

    [Fact]
    public void Constant_expression_invoke_ends_block()
    {
        var graph =
            PineControlFlowGraph.FromFragment(
                Ops(
                    StackInstruction.Local_Get(0),
                    StackInstruction.Eval_Const(PineValue.EmptyList)));

        graph.Blocks.Should().HaveCount(2);
        graph.Blocks[0].Terminator.Should().BeOfType<PineControlFlowTerminator.Invoke>();
        graph.Blocks[1].Terminator.Should().BeOfType<PineControlFlowTerminator.Return>();

        graph.LowerToStackInstructions().Should().Equal(
            StackInstruction.Local_Get(0),
            StackInstruction.Eval_Const(PineValue.EmptyList),
            StackInstruction.Return);
    }

    [Fact]
    public void Operation_rejects_control_transfer_instructions()
    {
        StackInstruction[] controlTransfers =
            [
            StackInstruction.Return,
            StackInstruction.Jump_Unconditional(1),
            StackInstruction.Jump_If_Equal(1, PineKernelValues.TrueValue),
            new StackInstruction(
                StackInstructionKind.Switch_Jump_If_Equal_Const,
                SwitchJumpTable: []),
            ];

        foreach (var instruction in controlTransfers)
        {
            var act = () => new PineControlFlowNode.Operation(instruction);

            act.Should().Throw<ArgumentException>();
        }
    }

    [Fact]
    public void Conditional_models_edges_with_block_arguments_and_lowers_to_offsets()
    {
        var conditionLiteral = PineValue.Blob([1]);

        var fragment =
            Ops(StackInstruction.Local_Get(1), StackInstruction.Local_Get(0))
            .Append(
                Conditional(
                    conditionLiteral,
                    Ops(StackInstruction.Push_Literal(PineValue.EmptyList)),
                    Ops(StackInstruction.Push_Literal(PineValue.EmptyBlob))))
            .AppendOperation(StackInstruction.Build_List(2));

        var graph = PineControlFlowGraph.FromFragment(fragment);

        graph.Blocks.Should().HaveCount(4);

        var conditional =
            graph.Blocks[0].Terminator.Should().BeOfType<PineControlFlowTerminator.ConditionalJump>().Subject;

        conditional.Literal.Should().Be(conditionLiteral);
        conditional.FallThrough.Should().Be(new PineBlockId(1));
        conditional.Branch.Should().Be(new PineBlockId(2));

        // The value below the condition is passed along both edges.
        conditional.FallThroughArguments.Should().Equal(graph.Blocks[0].Operations[0].Results);
        conditional.BranchArguments.Should().Equal(graph.Blocks[0].Operations[0].Results);
        graph.Blocks[1].Parameters.Should().HaveCount(1);
        graph.Blocks[2].Parameters.Should().HaveCount(1);

        graph.Blocks[1].Terminator
            .Should().BeOfType<PineControlFlowTerminator.Jump>()
            .Which.IsFallThrough.Should().BeFalse();

        graph.Blocks[2].Terminator
            .Should().BeOfType<PineControlFlowTerminator.Jump>()
            .Which.IsFallThrough.Should().BeTrue();

        graph.Blocks[3].Parameters.Should().HaveCount(2);

        graph.LowerToStackInstructions().Should().Equal(
            StackInstruction.Local_Get(1),
            StackInstruction.Local_Get(0),
            StackInstruction.Jump_If_Equal(3, conditionLiteral),
            StackInstruction.Push_Literal(PineValue.EmptyList),
            StackInstruction.Jump_Unconditional(2),
            StackInstruction.Push_Literal(PineValue.EmptyBlob),
            StackInstruction.Build_List(2),
            StackInstruction.Return);
    }

    [Fact]
    public void Jumps_to_return_are_replaced_with_return()
    {
        var conditionLiteral = PineValue.Blob([1]);

        var fragment =
            Ops(StackInstruction.Local_Get(0))
            .Append(
                Conditional(
                    conditionLiteral,
                    Ops(StackInstruction.Push_Literal(PineValue.EmptyList)),
                    Ops(StackInstruction.Push_Literal(PineValue.EmptyBlob))));

        var graph = PineControlFlowGraph.FromFragment(fragment);

        graph.LowerToStackInstructions().Should().Equal(
            StackInstruction.Local_Get(0),
            StackInstruction.Jump_If_Equal(3, conditionLiteral),
            StackInstruction.Push_Literal(PineValue.EmptyList),
            StackInstruction.Jump_Unconditional(2),
            StackInstruction.Push_Literal(PineValue.EmptyBlob),
            StackInstruction.Return);

        var forwarded = graph.ForwardJumpsToReturn();

        forwarded.Blocks[1].Terminator.Should().BeOfType<PineControlFlowTerminator.Return>();

        forwarded.LowerToStackInstructions().Should().Equal(
            StackInstruction.Local_Get(0),
            StackInstruction.Jump_If_Equal(3, conditionLiteral),
            StackInstruction.Push_Literal(PineValue.EmptyList),
            StackInstruction.Return,
            StackInstruction.Push_Literal(PineValue.EmptyBlob),
            StackInstruction.Return);

        forwarded.ForwardJumpsToReturn().Should().BeSameAs(forwarded);
    }

    [Fact]
    public void Empty_join_of_nested_conditional_in_last_branch_is_removed()
    {
        var outerLiteral = PineValue.Blob([1]);
        var innerLiteral = PineValue.Blob([2]);

        var fragment =
            Ops(StackInstruction.Local_Get(0))
            .Append(
                Conditional(
                    outerLiteral,
                    Ops(StackInstruction.Push_Literal(PineValue.EmptyList)),
                    Ops(StackInstruction.Local_Get(1))
                    .Append(
                        Conditional(
                            innerLiteral,
                            Ops(StackInstruction.Push_Literal(PineValue.EmptyBlob)),
                            Ops(StackInstruction.Local_Get(2))))));

        var graph = PineControlFlowGraph.FromFragment(fragment);

        graph.Blocks.Should().NotContain(
            block =>
            block.Operations.IsEmpty &&
            block.Terminator is PineControlFlowTerminator.Jump);

        graph.Blocks.Should().HaveCount(6);

        var innerFallThroughJump =
            graph.Blocks[3].Terminator.Should().BeOfType<PineControlFlowTerminator.Jump>().Subject;

        innerFallThroughJump.Target.Should().Be(graph.Blocks[^1].Id);

        graph.ForwardJumpsToReturn().LowerToStackInstructions().Should().Equal(
            StackInstruction.Local_Get(0),
            StackInstruction.Jump_If_Equal(3, outerLiteral),
            StackInstruction.Push_Literal(PineValue.EmptyList),
            StackInstruction.Return,
            StackInstruction.Local_Get(1),
            StackInstruction.Jump_If_Equal(3, innerLiteral),
            StackInstruction.Push_Literal(PineValue.EmptyBlob),
            StackInstruction.Return,
            StackInstruction.Local_Get(2),
            StackInstruction.Return);
    }

    [Fact]
    public void Invoke_at_end_of_last_branch_continues_in_join()
    {
        var fragment =
            Ops(StackInstruction.Local_Get(0))
            .Append(
                Conditional(
                    PineKernelValues.TrueValue,
                    Ops(StackInstruction.Push_Literal(PineValue.EmptyList)),
                    Ops(
                        StackInstruction.Local_Get(0),
                        StackInstruction.Eval_Const(PineValue.EmptyList))))
            .AppendOperation(StackInstruction.Skip_Head_Const(1));

        var graph = PineControlFlowGraph.FromFragment(fragment);

        var invoke =
            graph.Blocks
            .Select(block => block.Terminator)
            .OfType<PineControlFlowTerminator.Invoke>()
            .Single();

        var join = graph.Blocks[invoke.Continuation.Value];

        join.Operations.Select(operation => operation.Instruction)
            .Should().Equal(StackInstruction.Skip_Head_Const(1));

        graph.Blocks[1].Terminator
            .Should().BeOfType<PineControlFlowTerminator.Jump>()
            .Which.Target.Should().Be(join.Id);
    }

    [Fact]
    public void Switch_lowers_cases_sharing_a_branch_to_the_same_offset()
    {
        var selectorA = PineValue.Blob([1]);
        var selectorB = PineValue.Blob([2]);
        var selectorC = PineValue.Blob([3]);

        var fragment =
            Ops(StackInstruction.Local_Get(0))
            .Append(
                new PineControlFlowNode.Switch(
                    PineSwitchKind.Equal,
                    [
                    new PineSwitchFragmentCase(selectorA, 0),
                    new PineSwitchFragmentCase(selectorB, 1),
                    new PineSwitchFragmentCase(selectorC, 0),
                    ],
                    Default: Ops(StackInstruction.Push_Literal(PineValue.EmptyList)),
                    Branches:
                    [
                    Ops(StackInstruction.Push_Literal(PineValue.EmptyBlob)),
                    Ops(StackInstruction.Local_Get(1)),
                    ],
                    SkipCountMultiplier: 1));

        var graph = PineControlFlowGraph.FromFragment(fragment);

        var switchTerminator =
            graph.Blocks[0].Terminator.Should().BeOfType<PineControlFlowTerminator.Switch>().Subject;

        switchTerminator.Cases.Select(switchCase => switchCase.Literal)
            .Should().Equal(selectorA, selectorB, selectorC);

        switchTerminator.Cases[0].Target.Should().Be(switchTerminator.Cases[2].Target);

        graph.ForwardJumpsToReturn().LowerToStackInstructions()
            .Select(instruction => instruction.ToString())
            .Should().Equal(
            StackInstruction.Local_Get(0).ToString(),
            new StackInstruction(
                StackInstructionKind.Switch_Jump_If_Equal_Const,
                SwitchJumpTable:
                ImmutableDictionary<PineValue, int>.Empty
                .Add(selectorA, 3)
                .Add(selectorB, 5)
                .Add(selectorC, 3)).ToString(),
            StackInstruction.Push_Literal(PineValue.EmptyList).ToString(),
            StackInstruction.Return.ToString(),
            StackInstruction.Push_Literal(PineValue.EmptyBlob).ToString(),
            StackInstruction.Return.ToString(),
            StackInstruction.Local_Get(1).ToString(),
            StackInstruction.Return.ToString());
    }

    [Fact]
    public void Slice_switch_consumes_two_values_and_preserves_case_order()
    {
        var first = PineValue.Blob([4, 5]);
        var second = PineValue.Blob([6, 7]);

        var fragment =
            Ops(StackInstruction.Local_Get(0), StackInstruction.Local_Get(1))
            .Append(
                new PineControlFlowNode.Switch(
                    PineSwitchKind.SliceSkipVarEqual,
                    [
                    new PineSwitchFragmentCase(second, 1),
                    new PineSwitchFragmentCase(first, 0),
                    ],
                    Default: Ops(StackInstruction.Push_Literal(PineValue.EmptyList)),
                    Branches:
                    [
                    Ops(StackInstruction.Push_Literal(first)),
                    Ops(StackInstruction.Push_Literal(second)),
                    ],
                    SkipCountMultiplier: 1));

        var graph = PineControlFlowGraph.FromFragment(fragment);

        graph.Blocks[0].Terminator
            .Should().BeOfType<PineControlFlowTerminator.Switch>()
            .Which.Arguments.Should().BeEmpty();

        graph.ForwardJumpsToReturn().LowerToStackInstructions()[2]
            .SliceSwitchCases.Should().Equal(
            new SliceSwitchCase(second, 5),
            new SliceSwitchCase(first, 3));
    }

    [Fact]
    public void Jump_to_entry_with_empty_stack_forms_loop()
    {
        var fragment =
            Ops(StackInstruction.Local_Get(0))
            .Append(
                Conditional(
                    PineKernelValues.TrueValue,
                    Ops(StackInstruction.Push_Literal(PineValue.EmptyList)),
                    PineControlFlowFragment.Empty.Append(new PineControlFlowNode.JumpToEntry())));

        var graph = PineControlFlowGraph.FromFragment(fragment);

        graph.Blocks[2].Terminator
            .Should().BeEquivalentTo(
            new PineControlFlowTerminator.Jump(
                Target: graph.Entry,
                Arguments: [],
                IsFallThrough: false));

        graph.LowerToStackInstructions().Should().Equal(
            StackInstruction.Local_Get(0),
            StackInstruction.Jump_If_Equal(3, PineKernelValues.TrueValue),
            StackInstruction.Push_Literal(PineValue.EmptyList),
            StackInstruction.Jump_Unconditional(2),
            StackInstruction.Jump_Unconditional(-4),
            StackInstruction.Return);
    }

    [Fact]
    public void Jump_to_entry_with_live_stack_value_is_rejected()
    {
        var fragment =
            Ops(StackInstruction.Push_Literal(PineValue.EmptyList))
            .Append(new PineControlFlowNode.JumpToEntry());

        var act = () => PineControlFlowGraph.FromFragment(fragment);

        act.Should()
            .Throw<InvalidOperationException>()
            .WithMessage("*Edge 0 -> 0 supplies 1 arguments for 0 parameters*");
    }

    [Fact]
    public void Node_after_jump_to_entry_is_rejected()
    {
        var fragment =
            PineControlFlowFragment.Empty
            .Append(new PineControlFlowNode.JumpToEntry())
            .AppendOperation(StackInstruction.Push_Literal(PineValue.EmptyList));

        var act = () => PineControlFlowGraph.FromFragment(fragment);

        act.Should()
            .Throw<InvalidOperationException>()
            .WithMessage("*after a control transfer*");
    }

    [Fact]
    public void Branches_with_inconsistent_stack_depth_are_rejected()
    {
        var fragment =
            Ops(StackInstruction.Local_Get(0))
            .Append(
                Conditional(
                    PineKernelValues.TrueValue,
                    Ops(StackInstruction.Push_Literal(PineValue.EmptyList)),
                    Ops(
                        StackInstruction.Push_Literal(PineValue.EmptyList),
                        StackInstruction.Push_Literal(PineValue.EmptyList))));

        var act = () => PineControlFlowGraph.FromFragment(fragment);

        act.Should()
            .Throw<InvalidOperationException>()
            .WithMessage("*Inconsistent stack depth*");
    }

    [Fact]
    public void Stack_underflow_is_rejected()
    {
        var act = () => PineControlFlowGraph.FromFragment(Ops(StackInstruction.Pop));

        act.Should()
            .Throw<InvalidOperationException>()
            .WithMessage("*underflow*");
    }

    [Fact]
    public void Last_operation_is_only_visible_at_top_level()
    {
        var fragment =
            PineControlFlowFragment.Empty
            .Append(
                Conditional(
                    PineKernelValues.TrueValue,
                    Ops(StackInstruction.Local_Get(1)),
                    Ops(StackInstruction.Local_Get(2))));

        fragment.LastOperationOrNull.Should().BeNull();

        var act = () => fragment.ReplaceLastOperation(StackInstruction.Local_Get(3));

        act.Should().Throw<InvalidOperationException>();

        fragment
            .AppendOperation(StackInstruction.Local_Get(4))
            .ReplaceLastOperation(StackInstruction.Local_Get(5))
            .LastOperationOrNull.Should().Be(StackInstruction.Local_Get(5));
    }

    [Theory]
    [InlineData(true)]
    [InlineData(false)]
    public void Constant_boolean_switch_branches_are_forwarded(bool branchWhenTrue)
    {
        var selectorA = PineValue.Blob([1]);
        var selectorB = PineValue.Blob([2]);
        var falseResult = PineValue.Blob([3]);
        var trueResult = PineValue.Blob([4]);
        var comparedBoolean = branchWhenTrue ? PineKernelValues.TrueValue : PineKernelValues.FalseValue;

        var fragment =
            Ops(StackInstruction.Local_Get(0))
            .Append(
                new PineControlFlowNode.Switch(
                    PineSwitchKind.Equal,
                    [
                    new PineSwitchFragmentCase(selectorA, 0),
                    new PineSwitchFragmentCase(selectorB, 0),
                    ],
                    Default: Ops(StackInstruction.Push_Literal(PineKernelValues.FalseValue)),
                    Branches: [Ops(StackInstruction.Push_Literal(PineKernelValues.TrueValue))],
                    SkipCountMultiplier: 1))
            .Append(
                Conditional(
                    comparedBoolean,
                    Ops(StackInstruction.Push_Literal(falseResult)),
                    Ops(StackInstruction.Push_Literal(trueResult))));

        Optimize(fragment).Select(instruction => instruction.ToString()).Should().Equal(
            [
                StackInstruction.Local_Get(0).ToString(),
                new StackInstruction(
                    StackInstructionKind.Switch_Jump_If_Equal_Const,
                    SwitchJumpTable:
                    ImmutableDictionary<PineValue, int>.Empty
                    .Add(selectorA, 3)
                    .Add(selectorB, 3)).ToString(),
                StackInstruction.Push_Literal(branchWhenTrue ? falseResult : trueResult).ToString(),
                StackInstruction.Return.ToString(),
                StackInstruction.Push_Literal(branchWhenTrue ? trueResult : falseResult).ToString(),
                StackInstruction.Return.ToString(),
            ]);
    }

    [Fact]
    public void Constant_boolean_conditional_branches_are_forwarded()
    {
        var conditionValue = PineValue.Blob([1]);
        var falseResult = PineValue.Blob([2]);
        var trueResult = PineValue.Blob([3]);

        var fragment =
            Ops(StackInstruction.Local_Get(0))
            .Append(
                Conditional(
                    conditionValue,
                    Ops(StackInstruction.Push_Literal(PineKernelValues.FalseValue)),
                    Ops(StackInstruction.Push_Literal(PineKernelValues.TrueValue))))
            .Append(
                Conditional(
                    PineKernelValues.TrueValue,
                    Ops(StackInstruction.Push_Literal(falseResult)),
                    Ops(StackInstruction.Push_Literal(trueResult))));

        Optimize(fragment).Should().Equal(
            [
                StackInstruction.Local_Get(0),
                StackInstruction.Jump_If_Equal(3, conditionValue),
                StackInstruction.Push_Literal(falseResult),
                StackInstruction.Return,
                StackInstruction.Push_Literal(trueResult),
                StackInstruction.Return,
            ]);
    }

    [Fact]
    public void Constant_boolean_branches_preserve_other_stack_values()
    {
        var selector = PineValue.Blob([1]);
        var falseResult = PineValue.Blob([2]);
        var trueResult = PineValue.Blob([3]);

        var fragment =
            Ops(StackInstruction.Local_Get(0), StackInstruction.Local_Get(1))
            .Append(
                Switch(
                    Ops(StackInstruction.Push_Literal(PineKernelValues.FalseValue)),
                    (selector, Ops(StackInstruction.Push_Literal(PineKernelValues.TrueValue)))))
            .Append(
                Conditional(
                    PineKernelValues.TrueValue,
                    Ops(StackInstruction.Pop, StackInstruction.Push_Literal(falseResult)),
                    Ops(StackInstruction.Pop, StackInstruction.Push_Literal(trueResult))));

        Optimize(fragment).Select(instruction => instruction.ToString()).Should().Equal(
            [
                StackInstruction.Local_Get(0).ToString(),
                StackInstruction.Local_Get(1).ToString(),
                new StackInstruction(
                    StackInstructionKind.Switch_Jump_If_Equal_Const,
                    SwitchJumpTable:
                    ImmutableDictionary<PineValue, int>.Empty.Add(selector, 4)).ToString(),
                StackInstruction.Pop.ToString(),
                StackInstruction.Push_Literal(falseResult).ToString(),
                StackInstruction.Return.ToString(),
                StackInstruction.Pop.ToString(),
                StackInstruction.Push_Literal(trueResult).ToString(),
                StackInstruction.Return.ToString(),
            ]);
    }

    [Fact]
    public void Constant_boolean_branches_with_an_unknown_predecessor_are_not_forwarded()
    {
        var selectorA = PineValue.Blob([1]);
        var selectorB = PineValue.Blob([2]);

        var fragment =
            Ops(StackInstruction.Local_Get(0))
            .Append(
                Switch(
                    Ops(StackInstruction.Push_Literal(PineKernelValues.FalseValue)),
                    (selectorA, Ops(StackInstruction.Push_Literal(PineKernelValues.TrueValue))),
                    (selectorB, Ops(StackInstruction.Local_Get(1)))))
            .Append(
                Conditional(
                    PineKernelValues.TrueValue,
                    Ops(StackInstruction.Push_Literal(PineValue.EmptyList)),
                    Ops(StackInstruction.Push_Literal(PineValue.EmptyBlob))));

        var graph = PineControlFlowGraph.FromFragment(fragment);

        graph.ForwardConstantBooleanBranches().Should().BeSameAs(graph);
    }

    [Fact]
    public void Constant_boolean_provider_with_multiple_predecessors_is_forwarded()
    {
        var selector = PineValue.Blob([1]);
        var innerLiteral = PineValue.Blob([2]);

        var fragment =
            Ops(StackInstruction.Local_Get(0))
            .Append(
                Switch(
                    Ops(StackInstruction.Push_Literal(PineKernelValues.FalseValue)),
                    (selector,
                    Ops(StackInstruction.Local_Get(1))
                    .Append(
                        Conditional(
                            innerLiteral,
                            Ops(StackInstruction.Push_Literal(PineValue.EmptyList), StackInstruction.Pop),
                            Ops(StackInstruction.Push_Literal(PineValue.EmptyBlob), StackInstruction.Pop)))
                    .AppendOperation(StackInstruction.Push_Literal(PineKernelValues.TrueValue)))))
            .Append(
                Conditional(
                    PineKernelValues.TrueValue,
                    Ops(StackInstruction.Push_Literal(PineValue.EmptyList)),
                    Ops(StackInstruction.Push_Literal(PineValue.EmptyBlob))));

        var graph = PineControlFlowGraph.FromFragment(fragment);

        var providerWithMultiplePredecessors =
            graph.Blocks.Single(
                block =>
                block.Operations is [{ Instruction.Kind: StackInstructionKind.Push_Literal } pushTrue] &&
                IsBooleanLiteral(pushTrue.Instruction.Literal) &&
                PineValueInProcess.AreEqual(pushTrue.Instruction.Literal!, PineKernelValues.TrueValue));

        graph.Blocks
            .Count(
            block =>
            block.Terminator is PineControlFlowTerminator.Jump jump &&
            jump.Target == providerWithMultiplePredecessors.Id)
            .Should().Be(2);

        var optimized = graph.ForwardConstantBooleanBranches();

        optimized.Blocks
            .SelectMany(block => block.Operations)
            .Should()
            .NotContain(operation => IsBooleanLiteral(operation.Instruction.Literal));

        optimized.Blocks
            .Should()
            .NotContain(
            block =>
            block.Terminator is PineControlFlowTerminator.ConditionalJump &&
            ((PineControlFlowTerminator.ConditionalJump)block.Terminator).Literal == PineKernelValues.TrueValue);

        var switchTerminator =
            optimized.Blocks[0].Terminator.Should().BeOfType<PineControlFlowTerminator.Switch>().Subject;

        var trueResultBlock =
            optimized.Blocks.Single(
                block =>
                block.Operations is [{ Instruction.Kind: StackInstructionKind.Push_Literal } push] &&
                PineValueInProcess.AreEqual(push.Instruction.Literal!, PineValue.EmptyBlob));

        // Both inner branches now continue directly in the block for the true result.
        optimized.Blocks
            .Count(
            block =>
            block.Terminator is PineControlFlowTerminator.Jump jump &&
            jump.Target == trueResultBlock.Id)
            .Should().Be(2);

        switchTerminator.Cases.Should().ContainSingle();
    }

    [Fact]
    public void Constant_boolean_branch_forwarding_preserves_backward_edges()
    {
        var selector = PineValue.Blob([1]);

        var fragment =
            Ops(StackInstruction.Local_Get(0))
            .Append(
                Switch(
                    Ops(StackInstruction.Push_Literal(PineKernelValues.FalseValue)),
                    (selector, Ops(StackInstruction.Push_Literal(PineKernelValues.TrueValue)))))
            .Append(
                Conditional(
                    PineKernelValues.TrueValue,
                    Ops(StackInstruction.Push_Literal(PineValue.EmptyList)),
                    PineControlFlowFragment.Empty.Append(new PineControlFlowNode.JumpToEntry())));

        Optimize(fragment).Select(instruction => instruction.ToString()).Should().Equal(
            [
                StackInstruction.Local_Get(0).ToString(),
                new StackInstruction(
                    StackInstructionKind.Switch_Jump_If_Equal_Const,
                    SwitchJumpTable:
                    ImmutableDictionary<PineValue, int>.Empty.Add(selector, 3)).ToString(),
                StackInstruction.Push_Literal(PineValue.EmptyList).ToString(),
                StackInstruction.Return.ToString(),
                StackInstruction.Jump_Unconditional(-4).ToString(),
                StackInstruction.Return.ToString(),
            ]);
    }

    [Theory]
    [InlineData(-1)]
    [InlineData(0)]
    [InlineData(1)]
    [InlineData(2)]
    public void List_scalar_replacement_preserves_prefix_and_out_of_bounds_elements(int index)
    {
        var prefix = PineValue.Blob([11]);
        var item = PineValue.Blob([22]);

        var fragment =
            Ops(
                StackInstruction.Push_Literal(item),
                StackInstruction.Build_List_With_Prefix(PineValue.List([prefix]), 1),
                StackInstruction.Skip_Head_Const(index));

        var original = PineControlFlowGraph.FromFragment(fragment);
        var optimized = original.ReplaceNonEscapingLists(parameterCount: 1);

        optimized.LowerToStackInstructions().Should()
            .NotContain(instruction => instruction.Kind == StackInstructionKind.Build_List_With_Prefix);

        var expected = index <= 0 ? prefix : index is 1 ? item : PineValue.EmptyList;
        Evaluate(optimized, PineValue.EmptyBlob).Should().Be(expected);
        Evaluate(original, PineValue.EmptyBlob).Should().Be(expected);
    }

    [Fact]
    public void List_scalar_replacement_accepts_an_entry_block_parameter()
    {
        var input = new PineVirtualValueId(0);
        var list = new PineVirtualValueId(1);
        var item = new PineVirtualValueId(2);

        var original =
            new PineControlFlowGraph(
                new PineBlockId(0),
                [
                    new PineBasicBlock(
                        new PineBlockId(0),
                        [input],
                        [
                            new PineControlFlowOperation(StackInstruction.Build_List(1), [input], [list]),
                            new PineControlFlowOperation(StackInstruction.Head_Generic, [list], [item]),
                        ],
                        new PineControlFlowTerminator.Return()),
                ]);

        var optimized = original.ReplaceNonEscapingLists(1);

        optimized.Blocks[0].Operations.Should().NotContain(
            operation => operation.Instruction.Kind == StackInstructionKind.Build_List);

        optimized.Validate();
    }

    [Fact]
    public void List_scalar_replacement_ignores_an_unreachable_cyclic_builder()
    {
        var reachable =
            PineControlFlowGraph.FromFragment(
                Ops(StackInstruction.Push_Literal(PineValue.EmptyBlob)));

        var pushed = new PineVirtualValueId(10);
        var built = new PineVirtualValueId(11);
        var isolatedId = new PineBlockId(reachable.Blocks.Length);

        var isolated =
            new PineBasicBlock(
                isolatedId,
                [],
                [
                    new PineControlFlowOperation(
                        StackInstruction.Push_Literal(PineValue.EmptyBlob),
                        [],
                        [pushed]),
                    new PineControlFlowOperation(StackInstruction.Build_List(1), [pushed], [built]),
                    new PineControlFlowOperation(StackInstruction.Pop, [built], []),
                ],
                new PineControlFlowTerminator.Jump(isolatedId, [], IsFallThrough: false));

        var graph = reachable with { Blocks = [.. reachable.Blocks, isolated] };

        graph.Validate();
        graph.ReplaceNonEscapingLists(1).Should().BeSameAs(graph);
    }

    [Theory]
    [InlineData(true)]
    [InlineData(false)]
    public void List_scalar_replacement_follows_block_arguments_and_branch_specific_projections(bool branch)
    {
        var first = PineValue.Blob([11]);
        var second = PineValue.Blob([22]);

        var fragment =
            Ops(
                StackInstruction.Push_Literal(first),
                StackInstruction.Push_Literal(second),
                StackInstruction.Build_List(2),
                StackInstruction.Local_Get(0))
            .Append(
                Conditional(
                    PineKernelValues.TrueValue,
                    Ops(StackInstruction.Skip_Head_Const(0)),
                    Ops(StackInstruction.Skip_Head_Const(1))));

        var optimized = PineControlFlowGraph.FromFragment(fragment).ReplaceNonEscapingLists(1);

        optimized.LowerToStackInstructions().Should()
            .NotContain(instruction => instruction.Kind == StackInstructionKind.Build_List);

        Evaluate(optimized, branch ? PineKernelValues.TrueValue : PineKernelValues.FalseValue)
            .Should().Be(branch ? second : first);
    }

    [Theory]
    [InlineData(true)]
    [InlineData(false)]
    public void List_scalar_replacement_tracks_local_aliases_and_reassignments(bool branch)
    {
        var first = PineValue.Blob([11]);
        var second = PineValue.Blob([22]);
        var other = PineValue.Blob([33]);

        var fragment =
            Ops(
                StackInstruction.Push_Literal(first),
                StackInstruction.Push_Literal(second),
                StackInstruction.Build_List(2),
                StackInstruction.Local_Set(1),
                StackInstruction.Pop,
                StackInstruction.Local_Get(0))
            .Append(
                Conditional(
                    PineKernelValues.TrueValue,
                    Ops(
                        StackInstruction.Push_Literal(PineValue.List([other])),
                        StackInstruction.Local_Set(1),
                        StackInstruction.Pop,
                        StackInstruction.Local_Get(1),
                        StackInstruction.Skip_Head_Const(0)),
                    Ops(StackInstruction.Local_Get_Skip_Head_Const(1, 1))));

        var optimized = PineControlFlowGraph.FromFragment(fragment).ReplaceNonEscapingLists(1);

        optimized.LowerToStackInstructions().Should()
            .NotContain(instruction => instruction.Kind == StackInstructionKind.Build_List);

        Evaluate(optimized, branch ? PineKernelValues.TrueValue : PineKernelValues.FalseValue)
            .Should().Be(branch ? second : other);
    }

    [Fact]
    public void List_scalar_replacement_preserves_two_distinct_local_aliases()
    {
        var first = PineValue.Blob([11]);
        var second = PineValue.Blob([22]);

        var fragment =
            Ops(
                StackInstruction.Push_Literal(first),
                StackInstruction.Build_List(1),
                StackInstruction.Local_Set(1),
                StackInstruction.Pop,
                StackInstruction.Push_Literal(second),
                StackInstruction.Build_List(1),
                StackInstruction.Local_Set(2),
                StackInstruction.Pop,
                StackInstruction.Local_Get_Skip_Head_Const(1, 0),
                StackInstruction.Local_Get_Skip_Head_Const(2, 0),
                StackInstruction.Build_List(2));

        var original = PineControlFlowGraph.FromFragment(fragment);
        var optimized = original.ReplaceNonEscapingLists(1);

        optimized.LowerToStackInstructions().Count(
            instruction => instruction.Kind == StackInstructionKind.Build_List).Should().Be(1);

        Evaluate(optimized, PineValue.EmptyBlob).Should().Be(Evaluate(original, PineValue.EmptyBlob));
    }

    [Fact]
    public void List_scalar_replacement_supports_head_length_and_length_comparison()
    {
        var item = PineValue.Blob([11]);

        foreach (var projection in new[]
        {
            StackInstruction.Head_Generic,
            StackInstruction.Length,
            StackInstruction.Length_Equal_Const(2),
            StackInstruction.Length_Equal_Const(3),
        })
        {
            var graph =
                PineControlFlowGraph.FromFragment(
                    Ops(
                        StackInstruction.Push_Literal(item),
                        StackInstruction.Build_List_With_Prefix(PineValue.List([PineValue.EmptyBlob]), 1),
                        projection));

            var optimized = graph.ReplaceNonEscapingLists(1);

            optimized.LowerToStackInstructions().Should().NotContain(
                instruction => instruction.Kind == StackInstructionKind.Build_List_With_Prefix);

            Evaluate(optimized, PineValue.EmptyBlob).Should().Be(Evaluate(graph, PineValue.EmptyBlob));
        }
    }

    [Fact]
    public void List_scalar_replacement_follows_local_set_descending()
    {
        var item = PineValue.Blob([11]);

        var graph =
            PineControlFlowGraph.FromFragment(
                Ops(
                    StackInstruction.Push_Literal(item),
                    StackInstruction.Build_List(1),
                    StackInstruction.Local_Set_Descending(2, 1),
                    StackInstruction.Pop,
                    StackInstruction.Local_Get_Skip_Head_Const(2, 0)));

        var optimized = graph.ReplaceNonEscapingLists(1);

        optimized.LowerToStackInstructions().Should().NotContain(
            instruction => instruction.Kind == StackInstructionKind.Build_List);

        Evaluate(optimized, PineValue.EmptyBlob).Should().Be(item);
    }

    [Fact]
    public void List_scalar_replacement_respects_descending_local_indices_for_multiple_values()
    {
        var first = PineValue.Blob([11]);
        var second = PineValue.Blob([22]);

        var original =
            PineControlFlowGraph.FromFragment(
                Ops(
                    StackInstruction.Push_Literal(first),
                    StackInstruction.Push_Literal(second),
                    StackInstruction.Build_List(1),
                    StackInstruction.Local_Set_Descending(7, 2),
                    StackInstruction.PopMultiple(2),
                    StackInstruction.Local_Get_Skip_Head_Const(7, 0),
                    StackInstruction.Local_Get(6),
                    StackInstruction.Build_List(2)));

        var optimized = original.ReplaceNonEscapingLists(parameterCount: 1);

        optimized.LowerToStackInstructions().Count(
            instruction => instruction.Kind == StackInstructionKind.Build_List).Should().Be(1);

        Evaluate(optimized, PineValue.EmptyBlob).Should().Be(PineValue.List([second, first]));
        Evaluate(optimized, PineValue.EmptyBlob).Should().Be(Evaluate(original, PineValue.EmptyBlob));
    }

    [Fact]
    public void List_scalar_replacement_handles_prefixed_list_stored_in_a_reassigned_parameter_local()
    {
        var items =
            Enumerable.Range(0, 7)
            .Select(index => PineValue.Blob([(byte)(index + 1)]))
            .ToArray();

        var fragment =
            Ops(
                StackInstruction.Push_Literal(items[2]),
                StackInstruction.Push_Literal(items[3]),
                StackInstruction.Push_Literal(items[4]),
                StackInstruction.Push_Literal(items[5]),
                StackInstruction.Push_Literal(items[6]),
                StackInstruction.Build_List_With_Prefix(PineValue.List([items[0], items[1]]), 5),
                StackInstruction.Local_Set(0),
                StackInstruction.Skip_Head_Const(1),
                StackInstruction.Local_Get_Skip_Head_Const(0, 3),
                StackInstruction.Build_List(2));

        var original = PineControlFlowGraph.FromFragment(fragment);
        var optimized = original.ReplaceNonEscapingLists(parameterCount: 1);
        var instructions = optimized.LowerToStackInstructions();

        instructions.Should().NotContain(
            instruction => instruction.Kind == StackInstructionKind.Build_List_With_Prefix);

        instructions.Count(instruction => instruction.Kind == StackInstructionKind.Build_List)
            .Should().Be(1);

        Evaluate(optimized, PineValue.EmptyBlob).Should().Be(PineValue.List([items[1], items[3]]));
        Evaluate(optimized, PineValue.EmptyBlob).Should().Be(Evaluate(original, PineValue.EmptyBlob));
    }

    [Theory]
    [InlineData(true)]
    [InlineData(false)]
    public void List_scalar_replacement_follows_prefixed_list_through_jumps_and_tag_check(bool branch)
    {
        var tag = PineValue.Blob([1]);
        var first = PineValue.Blob([2]);
        var second = PineValue.Blob([3]);

        var fragment =
            Ops(
                StackInstruction.Push_Literal(first),
                StackInstruction.Push_Literal(second),
                StackInstruction.Build_List_With_Prefix(PineValue.List([PineValue.EmptyBlob, tag]), 2),
                StackInstruction.Local_Get(0))
            .Append(
                Conditional(
                    PineKernelValues.TrueValue,
                    Ops(StackInstruction.Push_Literal(PineValue.EmptyBlob), StackInstruction.Pop),
                    Ops(StackInstruction.Push_Literal(PineValue.EmptyList), StackInstruction.Pop)))
            .Append(
                Ops(
                    StackInstruction.Local_Set(0),
                    StackInstruction.Skip_Head_Const(1)))
            .Append(
                Conditional(
                    tag,
                    Ops(StackInstruction.Local_Get_Skip_Head_Const(0, 2)),
                    Ops(StackInstruction.Local_Get_Skip_Head_Const(0, 3))));

        var original = PineControlFlowGraph.FromFragment(fragment);
        var optimized = original.ReplaceNonEscapingLists(parameterCount: 1);

        optimized.LowerToStackInstructions().Should().NotContain(
            instruction => instruction.Kind == StackInstructionKind.Build_List_With_Prefix);

        var environment = branch ? PineKernelValues.TrueValue : PineKernelValues.FalseValue;
        Evaluate(optimized, environment).Should().Be(second);
        Evaluate(optimized, environment).Should().Be(Evaluate(original, environment));
    }

    [Theory]
    [InlineData(true)]
    [InlineData(false)]
    public void List_scalar_replacement_handles_a_cross_block_local_set_and_multiple_projections(bool branch)
    {
        var first = PineValue.Blob([11]);
        var second = PineValue.Blob([22]);
        var third = PineValue.Blob([33]);

        var fragment =
            Ops(
                StackInstruction.Push_Literal(first),
                StackInstruction.Push_Literal(second),
                StackInstruction.Push_Literal(third),
                StackInstruction.Build_List(3),
                StackInstruction.Local_Get(0))
            .Append(
                Conditional(
                    PineKernelValues.TrueValue,
                    Ops(StackInstruction.Push_Literal(PineValue.EmptyBlob), StackInstruction.Pop),
                    Ops(StackInstruction.Push_Literal(PineValue.EmptyList), StackInstruction.Pop)))
            .Append(
                Ops(
                    StackInstruction.Local_Set(7),
                    StackInstruction.Head_Generic,
                    StackInstruction.Local_Get_Skip_Head_Const(7, 1),
                    StackInstruction.Local_Get_Skip_Head_Const(7, 2),
                    StackInstruction.Build_List(3)));

        var original = PineControlFlowGraph.FromFragment(fragment);
        var optimized = original.ReplaceNonEscapingLists(parameterCount: 1);
        var instructions = optimized.LowerToStackInstructions();

        instructions.Count(instruction => instruction.Kind == StackInstructionKind.Build_List)
            .Should().Be(1);

        var environment = branch ? PineKernelValues.TrueValue : PineKernelValues.FalseValue;
        Evaluate(optimized, environment).Should().Be(PineValue.List([first, second, third]));
        Evaluate(optimized, environment).Should().Be(Evaluate(original, environment));
    }

    [Theory]
    [InlineData(true)]
    [InlineData(false)]
    public void List_scalar_replacement_shares_slots_between_choice_alternatives(bool just)
    {
        var choice = PineValue.Blob([1]);
        var justTag = PineValue.Blob([2]);
        var nothingTag = PineValue.Blob([3]);
        var argument = PineValue.Blob([4]);

        var original =
            PineControlFlowGraph.FromFragment(
                Ops(StackInstruction.Local_Get(0))
                .Append(
                    Conditional(
                        PineKernelValues.TrueValue,
                        Ops(StackInstruction.Push_Literal(PineValue.List([choice, nothingTag]))),
                        Ops(
                            StackInstruction.Push_Literal(argument),
                            StackInstruction.Build_List_With_Prefix(PineValue.List([choice, justTag]), 1))))
                .Append(
                    Ops(
                        StackInstruction.Local_Set(1),
                        StackInstruction.Pop,
                        StackInstruction.Local_Get_Skip_Head_Const(1, 1)))
                .Append(
                    Conditional(
                        justTag,
                        Ops(StackInstruction.Push_Literal(nothingTag)),
                        Ops(StackInstruction.Local_Get_Skip_Head_Const(1, 2)))));

        var optimized = original.ReplaceNonEscapingLists(1);
        var instructions = optimized.LowerToStackInstructions();

        instructions.Should().NotContain(
            instruction => instruction.Kind == StackInstructionKind.Build_List_With_Prefix);

        var environment = just ? PineKernelValues.TrueValue : PineKernelValues.FalseValue;
        Evaluate(optimized, environment).Should().Be(just ? argument : nothingTag);
        Evaluate(optimized, environment).Should().Be(Evaluate(original, environment));
    }

    [Theory]
    [InlineData(true)]
    [InlineData(false)]
    public void List_scalar_replacement_initializes_missing_elements_at_a_join(bool just)
    {
        var item = PineValue.Blob([11]);

        var original =
            PineControlFlowGraph.FromFragment(
                Ops(StackInstruction.Local_Get(0))
                .Append(
                    Conditional(
                        PineKernelValues.TrueValue,
                        Ops(StackInstruction.Push_Literal(PineValue.List([item]))),
                        Ops(
                            StackInstruction.Push_Literal(item),
                            StackInstruction.Build_List_With_Prefix(PineValue.List([item]), 1))))
                .AppendOperation(StackInstruction.Skip_Head_Const(1)));

        var optimized = original.ReplaceNonEscapingLists(1);

        optimized.LowerToStackInstructions().Should().NotContain(
            instruction => instruction.Kind == StackInstructionKind.Build_List_With_Prefix);

        var environment = just ? PineKernelValues.TrueValue : PineKernelValues.FalseValue;
        Evaluate(optimized, environment).Should().Be(just ? item : PineValue.EmptyList);
        Evaluate(optimized, environment).Should().Be(Evaluate(original, environment));
    }

    [Theory]
    [InlineData(true)]
    [InlineData(false)]
    public void List_scalar_replacement_preserves_variable_lengths_at_a_join(bool longer)
    {
        var item = PineValue.Blob([11]);

        foreach (var projection in new[]
        {
            StackInstruction.Length,
            StackInstruction.Length_Equal_Const(1),
            StackInstruction.Length_Equal_Const(2),
        })
        {
            var original =
                PineControlFlowGraph.FromFragment(
                    Ops(StackInstruction.Local_Get(0))
                    .Append(
                        Conditional(
                            PineKernelValues.TrueValue,
                            Ops(StackInstruction.Push_Literal(PineValue.List([item]))),
                            Ops(
                                StackInstruction.Push_Literal(item),
                                StackInstruction.Build_List_With_Prefix(PineValue.List([item]), 1))))
                    .AppendOperation(projection));

            var optimized = original.ReplaceNonEscapingLists(1);
            var environment = longer ? PineKernelValues.TrueValue : PineKernelValues.FalseValue;

            optimized.LowerToStackInstructions().Should().NotContain(
                instruction => instruction.Kind == StackInstructionKind.Build_List_With_Prefix);

            Evaluate(optimized, environment).Should().Be(Evaluate(original, environment));
        }
    }

    [Fact]
    public void List_scalar_replacement_keeps_joined_lists_that_escape()
    {
        var item = PineValue.Blob([11]);

        var original =
            PineControlFlowGraph.FromFragment(
                Ops(StackInstruction.Local_Get(0))
                .Append(
                    Conditional(
                        PineKernelValues.TrueValue,
                        Ops(StackInstruction.Push_Literal(PineValue.List([item]))),
                        Ops(
                            StackInstruction.Push_Literal(item),
                            StackInstruction.Build_List_With_Prefix(PineValue.List([item]), 1)))));

        var optimized = original.ReplaceNonEscapingLists(1);

        optimized.LowerToStackInstructions().Should().Contain(
            instruction => instruction.Kind == StackInstructionKind.Build_List_With_Prefix);

        foreach (var environment in new[] { PineKernelValues.TrueValue, PineKernelValues.FalseValue })
        {
            Evaluate(optimized, environment).Should().Be(Evaluate(original, environment));
        }
    }

    [Theory]
    [InlineData(true)]
    [InlineData(false)]
    public void List_scalar_replacement_preserves_both_live_lists_before_a_join(bool selectSecond)
    {
        var first = PineValue.Blob([11]);
        var second = PineValue.Blob([22]);

        var original =
            PineControlFlowGraph.FromFragment(
                Ops(
                    StackInstruction.Push_Literal(first),
                    StackInstruction.Build_List(1),
                    StackInstruction.Local_Set(1),
                    StackInstruction.Pop,
                    StackInstruction.Push_Literal(second),
                    StackInstruction.Build_List(1),
                    StackInstruction.Local_Set(2),
                    StackInstruction.Pop,
                    StackInstruction.Local_Get(0))
                .Append(
                    Conditional(
                        PineKernelValues.TrueValue,
                        Ops(StackInstruction.Local_Get(1)),
                        Ops(StackInstruction.Local_Get(2))))
                .AppendOperation(StackInstruction.Head_Generic));

        var optimized = original.ReplaceNonEscapingLists(1);
        var environment = selectSecond ? PineKernelValues.TrueValue : PineKernelValues.FalseValue;

        optimized.LowerToStackInstructions().Should().Contain(
            instruction => instruction.Kind == StackInstructionKind.Build_List);

        Evaluate(optimized, environment).Should().Be(selectSecond ? second : first);
        Evaluate(optimized, environment).Should().Be(Evaluate(original, environment));
    }

    [Fact]
    public void List_scalar_replacement_keeps_ambiguous_alias_at_a_join()
    {
        var first = PineValue.Blob([11]);
        var second = PineValue.Blob([22]);

        var fragment =
            Ops(
                StackInstruction.Push_Literal(first),
                StackInstruction.Build_List(1),
                StackInstruction.Local_Set(1),
                StackInstruction.Pop,
                StackInstruction.Local_Get(0))
            .Append(
                Conditional(
                    PineKernelValues.TrueValue,
                    Ops(StackInstruction.Push_Literal(PineValue.EmptyList), StackInstruction.Pop),
                    Ops(
                        StackInstruction.Push_Literal(PineValue.List([second])),
                        StackInstruction.Local_Set(1),
                        StackInstruction.Pop,
                        StackInstruction.Push_Literal(PineValue.EmptyList),
                        StackInstruction.Pop)))
            .AppendOperation(StackInstruction.Local_Get_Skip_Head_Const(1, 0));

        var original = PineControlFlowGraph.FromFragment(fragment);
        var optimized = original.ReplaceNonEscapingLists(1);

        optimized.LowerToStackInstructions().Should()
            .Contain(instruction => instruction.Kind == StackInstructionKind.Build_List);

        foreach (var condition in new[] { PineKernelValues.TrueValue, PineKernelValues.FalseValue })
        {
            Evaluate(optimized, condition).Should().Be(Evaluate(original, condition));
        }
    }

    [Fact]
    public void List_scalar_replacement_keeps_a_list_that_escapes_on_one_branch()
    {
        var item = PineValue.Blob([11]);

        var original =
            PineControlFlowGraph.FromFragment(
                Ops(
                    StackInstruction.Push_Literal(item),
                    StackInstruction.Build_List(1),
                    StackInstruction.Local_Set(1),
                    StackInstruction.Pop,
                    StackInstruction.Local_Get(0))
                .Append(
                    Conditional(
                        PineKernelValues.TrueValue,
                        Ops(StackInstruction.Local_Get(1)),
                        Ops(StackInstruction.Local_Get_Skip_Head_Const(1, 0)))));

        var optimized = original.ReplaceNonEscapingLists(parameterCount: 1);

        optimized.LowerToStackInstructions().Should().Contain(
            instruction => instruction.Kind == StackInstructionKind.Build_List);

        foreach (var environment in new[] { PineKernelValues.TrueValue, PineKernelValues.FalseValue })
        {
            Evaluate(optimized, environment).Should().Be(Evaluate(original, environment));
        }
    }

    [Fact]
    public void List_scalar_replacement_ignores_unrelated_tail_invocation()
    {
        var item = PineValue.Blob([11]);

        var fragment =
            Ops(StackInstruction.Local_Get(0))
            .Append(
                Conditional(
                    PineKernelValues.TrueValue,
                    Ops(StackInstruction.Push_Literal(PineValue.EmptyBlob)),
                    Ops(
                        StackInstruction.Push_Literal(item),
                        StackInstruction.Build_List(1),
                        StackInstruction.Head_Generic)));

        var original = PineControlFlowGraph.FromFragment(fragment);
        var blocks = original.Blocks.ToArray();

        blocks[1] =
            blocks[1] with
            {
                Terminator =
                new PineControlFlowTerminator.TailInvoke(
                    StackInstruction.Eval_Const(PineValue.EmptyList))
            };

        var withTailInvoke = new PineControlFlowGraph(original.Entry, [.. blocks]);
        var optimized = withTailInvoke.ReplaceNonEscapingLists(parameterCount: 1);

        optimized.LowerToStackInstructions().Should().NotContain(
            instruction => instruction.Kind == StackInstructionKind.Build_List);

        Evaluate(optimized, PineKernelValues.TrueValue).Should().Be(item);
    }

    [Fact]
    public void List_scalar_replacement_keeps_a_list_passed_to_tail_invocation()
    {
        var original =
            PineControlFlowGraph.FromFragment(
                Ops(
                    StackInstruction.Push_Literal(PineValue.EmptyBlob),
                    StackInstruction.Build_List(1)));

        var blocks = original.Blocks.ToArray();

        blocks[0] =
            blocks[0] with
            {
                Terminator =
                new PineControlFlowTerminator.TailInvoke(
                    StackInstruction.Eval_Const(PineValue.EmptyList))
            };

        var withTailInvoke = new PineControlFlowGraph(original.Entry, [.. blocks]);

        withTailInvoke.ReplaceNonEscapingLists(parameterCount: 1)
            .Should().BeSameAs(withTailInvoke);
    }

    [Fact]
    public void List_scalar_replacement_keeps_escaping_but_allows_dead_cyclic_builds()
    {
        var escaping =
            PineControlFlowGraph.FromFragment(
                Ops(StackInstruction.Local_Get(0), StackInstruction.Build_List(1)));

        escaping.ReplaceNonEscapingLists(1).Should().BeSameAs(escaping);

        var cyclic =
            PineControlFlowGraph.FromFragment(
                Ops(
                    StackInstruction.Push_Literal(PineValue.EmptyBlob),
                    StackInstruction.Build_List(1),
                    StackInstruction.Pop,
                    StackInstruction.Local_Get(0))
                .Append(
                    Conditional(
                        PineKernelValues.TrueValue,
                        Ops(StackInstruction.Push_Literal(PineValue.EmptyList)),
                        PineControlFlowFragment.Empty.Append(new PineControlFlowNode.JumpToEntry()))));

        cyclic.ReplaceNonEscapingLists(1).LowerToStackInstructions().Should()
            .NotContain(instruction => instruction.Kind == StackInstructionKind.Build_List);

        Evaluate(cyclic.ReplaceNonEscapingLists(1), PineKernelValues.FalseValue)
            .Should().Be(Evaluate(cyclic, PineKernelValues.FalseValue));
    }

    [Fact]
    public void List_scalar_replacement_keeps_a_cyclic_build_with_an_old_live_alias()
    {
        var cyclic =
            PineControlFlowGraph.FromFragment(
                Ops(
                    StackInstruction.Push_Literal(PineValue.Blob([11])),
                    StackInstruction.Build_List(1),
                    StackInstruction.Local_Set(1),
                    StackInstruction.Pop,
                    StackInstruction.Local_Get(0))
                .Append(
                    Conditional(
                        PineKernelValues.TrueValue,
                        Ops(StackInstruction.Local_Get_Skip_Head_Const(2, 0)),
                        Ops(
                            StackInstruction.Local_Get(1),
                            StackInstruction.Local_Set(2),
                            StackInstruction.Pop)
                        .Append(new PineControlFlowNode.JumpToEntry()))));

        cyclic.ReplaceNonEscapingLists(1).LowerToStackInstructions().Should()
            .Contain(instruction => instruction.Kind == StackInstructionKind.Build_List);
    }

    [Fact]
    public void Expression_compilation_replaces_a_deconstructed_list()
    {
        var expression =
            Expression.BuiltinInst(
                nameof(BuiltinFunction.head),
                Expression.ListInst([Expression.EnvironmentInstance, Expression.LitralInst(PineValue.EmptyBlob)]));

        var unoptimized =
            ExpressionCompilation.ControlFlowGraphFromExpression(
                expression,
                rootExprAlternativeForms: [],
                envClass: null,
                parametersAsLocals: StaticFunctionInterface.FromExpression(expression),
                parseCache: new());

        unoptimized.LowerToStackInstructions().Should()
            .Contain(instruction => instruction.Kind == StackInstructionKind.Build_List);

        var compiled =
            ExpressionCompilation.CompileExpression(
                expression,
                specializations: [],
                parseCache: new(),
                disableReduction: true,
                enableTailRecursionOptimization: false,
                skipInlining: (_, _) => false);

        compiled.Generic.Instructions.Should()
            .NotContain(instruction => instruction.Kind == StackInstructionKind.Build_List);

        var vm = CreateVm();

        foreach (var environment in new[] { PineValue.EmptyList, PineValue.Blob([1, 2]) })
        {
            vm.EvaluateExpression(expression, environment).IsOkOrNull().Should().Be(environment);
        }
    }

    private static PineValue Evaluate(PineControlFlowGraph graph, PineValue environment)
    {
        var expression = Expression.ListInst([Expression.EnvironmentInstance]);

        var compilation =
            new ExpressionCompilation(
                new StackFrameInstructions(StaticFunctionInterface.Generic, graph.LowerToStackInstructions()),
                Specialized: []);

        var vm =
            CreateVm(new Dictionary<Expression, ExpressionCompilation> { [expression] = compilation });

        return vm.EvaluateExpression(expression, environment).IsOkOrNull()!;
    }

    private static Core.Interpreter.IntermediateVM.PineVM CreateVm(
        IReadOnlyDictionary<Expression, ExpressionCompilation>? overrides = null) =>
        Core.Interpreter.IntermediateVM.PineVM.CreateCustom(
            evalCache: null,
            evaluationConfigDefault: null,
            reportFunctionApplication: null,
            compilationEnvClasses: null,
            disableReductionInCompilation: true,
            selectPrecompiled: null,
            skipInlineForExpression: _ => false,
            enableTailRecursionOptimization: false,
            parseCache: null,
            precompiledLeaves: null,
            reportEnterPrecompiledLeaf: null,
            reportExitPrecompiledLeaf: null,
            optimizationParametersSerial: null,
            cacheFileStore: null,
            expressionCompilationOverrides: overrides);

    private static bool IsBooleanLiteral(PineValueInProcess? literal) =>
        literal is not null &&
        (PineValueInProcess.AreEqual(literal, PineKernelValues.TrueValue) ||
        PineValueInProcess.AreEqual(literal, PineKernelValues.FalseValue));
}
