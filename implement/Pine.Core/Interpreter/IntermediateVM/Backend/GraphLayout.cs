using System;
using System.Collections.Immutable;
using System.Linq;

namespace Pine.Core.Interpreter.IntermediateVM.Backend;

/// <summary>Schedules already-known blocks and edge stubs without assigning numeric addresses.</summary>
internal static class GraphLayout
{
    internal static ImmutableList<LayoutFragment> Schedule(
        PineBlockId entry, ImmutableList<SelectedInstruction> prologue,
        ImmutableList<SelectedBlock> blocks, ImmutableList<PineBlockId> order)
    {
        var byId = blocks.ToImmutableDictionary(block => block.Id);
        return ImmutableList.Create(new LayoutFragment(
            new(LayoutLabelKind.Prologue, entry), prologue, new LayoutTransfer.Jump(new(LayoutLabelKind.Block, entry))))
            .AddRange(order.SelectMany(id => ScheduleBlock(byId[id])));
    }

    private static ImmutableList<LayoutFragment> ScheduleBlock(SelectedBlock block)
    {
        var label = new LayoutLabel(LayoutLabelKind.Block, block.Id);
        return block.Terminator switch
        {
            SelectedTerminator.Return ret =>
                [new(label, block.Instructions, new LayoutTransfer.Return(ret.Local))],
            SelectedTerminator.Jump jump =>
                [new(label, block.Instructions, new LayoutTransfer.Jump(EdgeLabel(0))),
                Stub(jump.Edge, 0)],
            SelectedTerminator.Invoke invoke =>
                [new(label, block.Instructions, new LayoutTransfer.Invoke(invoke.Call, new(LayoutLabelKind.Return, block.Id))),
                new(new(LayoutLabelKind.Return, block.Id), InstructionSelection.Store(invoke.ResultLocal),
                    new LayoutTransfer.Jump(EdgeLabel(0))),
                Stub(invoke.Continuation, 0)],
            SelectedTerminator.TailInvoke invoke =>
                [new(label, block.Instructions, new LayoutTransfer.TailInvoke(invoke.Call))],
            SelectedTerminator.Match match =>
                ImmutableList.Create(new LayoutFragment(label, block.Instructions,
                    new LayoutTransfer.Jump(match.Cases.Count == 0 ? EdgeLabel(0) : TestLabel(0))))
                .AddRange(match.Cases.Select((@case, index) => new LayoutFragment(
                    TestLabel(index), [], new LayoutTransfer.Branch(match.Local, @case.Literal, EdgeLabel(index),
                        index + 1 < match.Cases.Count ? TestLabel(index + 1) : EdgeLabel(match.Cases.Count)))))
                .AddRange(match.Cases.Select((@case, index) => Stub(@case.Edge, index)))
                .Add(Stub(match.Default, match.Cases.Count)),
            _ => throw new NotImplementedException(
                "ScheduleBlock does not handle selected terminator variant: " + block.Terminator.GetType().Name),
        };

        LayoutLabel EdgeLabel(int index) => new(LayoutLabelKind.Edge, block.Id, index);
        LayoutLabel TestLabel(int index) => new(LayoutLabelKind.Test, block.Id, index);
        LayoutFragment Stub(EdgeCopyPlan edge, int index) =>
            new(EdgeLabel(index),
                edge.Copies.Select(copy => (SelectedInstruction)new SelectedInstruction.Load(copy.Source)).ToImmutableList()
                .AddRange(edge.Copies.Reverse().SelectMany(copy => InstructionSelection.Store(copy.Destination))),
                new LayoutTransfer.Jump(new(LayoutLabelKind.Block, edge.Target)));
    }
}
