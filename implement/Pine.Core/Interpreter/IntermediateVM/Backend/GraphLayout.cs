using System;
using System.Collections.Immutable;
using System.Linq;

namespace Pine.Core.Interpreter.IntermediateVM.Backend;

/// <summary>Schedules already-known blocks and edge stubs without assigning numeric addresses.</summary>
internal static class GraphLayout
{
    internal static ImmutableList<LayoutFragment> Compact(ImmutableList<LayoutFragment> layout)
    {
        return Run();

        ImmutableList<LayoutFragment> Run()
        {
            var current = layout;
            for (var round = 0; round < 8; ++round)
            {
                var next = GraphLayoutLiveness.RemoveDeadStores(CompactOnce(current));
                if (next.SequenceEqual(current))
                    return next;
                current = next;
            }
            return current;
        }
    }

    private static ImmutableList<LayoutFragment> CompactOnce(ImmutableList<LayoutFragment> layout)
    {
        var byLabel = layout.ToImmutableDictionary(fragment => fragment.Label);
        var threaded = layout.Select(fragment => fragment with
        {
            Transfer = fragment.Transfer switch
            {
                LayoutTransfer.Return or LayoutTransfer.TailInvoke => fragment.Transfer,
                LayoutTransfer.Jump jump => jump with { Target = Resolve(fragment.Label, jump.Target) },
                LayoutTransfer.Branch branch => branch with
                {
                    Equal = Resolve(fragment.Label, branch.Equal),
                    NotEqual = Resolve(fragment.Label, branch.NotEqual),
                },
                LayoutTransfer.Match match => match with
                {
                    Cases = match.Cases.Select(@case => (@case.Literal, Resolve(fragment.Label, @case.Target))).ToImmutableList(),
                    Default = Resolve(fragment.Label, match.Default),
                },
                LayoutTransfer.Invoke invoke => invoke with { Success = Resolve(fragment.Label, invoke.Success) },
                _ => throw new NotImplementedException("CompactOnce does not handle transfer variant: " + fragment.Transfer.GetType().Name),
            },
        }).ToImmutableList();
        var threadedByLabel = threaded.ToImmutableDictionary(fragment => fragment.Label);
        var predecessors = threaded.SelectMany(fragment => GraphLayoutLiveness.Successors(fragment.Transfer))
            .GroupBy(label => label).ToImmutableDictionary(group => group.Key, group => group.Count());
        return threaded.Select(Fuse).ToImmutableList();

        LayoutFragment Fuse(LayoutFragment fragment)
        {
            if (fragment.Label.Kind != LayoutLabelKind.Prologue &&
                fragment.Transfer is LayoutTransfer.Jump targetJump && targetJump.Target != fragment.Label &&
                predecessors[targetJump.Target] == 1 &&
                threadedByLabel[targetJump.Target] is { Label.Kind: not (LayoutLabelKind.Prologue or LayoutLabelKind.Return) } merge &&
                !(fragment.Instructions.Count == 0 && merge.Instructions.Count == 0 &&
                    merge.Transfer is LayoutTransfer.Jump cycle && cycle.Target == fragment.Label))
                return fragment with
                {
                    Instructions = fragment.Instructions.AddRange(merge.Instructions),
                    Transfer = merge.Transfer,
                };
            var transfer = fragment.Transfer is LayoutTransfer.Jump jump &&
                threadedByLabel[jump.Target] is { Instructions.Count: 0, Transfer: LayoutTransfer.Branch or LayoutTransfer.Return } target
                ? target.Transfer : fragment.Transfer;
            if (transfer is LayoutTransfer.Branch branch &&
                fragment.Instructions.Count >= 3 &&
                fragment.Instructions[^3] is SelectedInstruction.Literal literal &&
                fragment.Instructions[^2] is SelectedInstruction.Store store &&
                fragment.Instructions[^1] is SelectedInstruction.Pop && store.Local == branch.Local)
                transfer = new LayoutTransfer.Jump(literal.Value == branch.Literal ? branch.Equal : branch.NotEqual);
            return fragment with { Transfer = transfer };
        }

        LayoutLabel Resolve(LayoutLabel source, LayoutLabel target)
        {
            return Follow();

            LayoutLabel Follow()
            {
                var current = target;
                var visited = ImmutableHashSet<LayoutLabel>.Empty.Add(source);
                while (byLabel[current] is { Instructions.Count: 0, Transfer: LayoutTransfer.Jump jump })
                {
                    if (!visited.Contains(current) && !visited.Contains(jump.Target))
                    {
                        visited = visited.Add(current);
                        current = jump.Target;
                        continue;
                    }
                    return target;
                }
                return current;
            }
        }
    }

    internal static ImmutableList<LayoutFragment> Schedule(
        PineBlockId entry, ImmutableList<SelectedInstruction> prologue,
        ImmutableList<SelectedBlock> blocks, ImmutableList<PineBlockId> order, bool compact = false)
    {
        var byId = blocks.ToImmutableDictionary(block => block.Id);
        return ImmutableList.Create(new LayoutFragment(
            new(LayoutLabelKind.Prologue, entry), prologue, new LayoutTransfer.Jump(new(LayoutLabelKind.Block, entry))))
            .AddRange(order.SelectMany(id => ScheduleBlock(byId[id], compact)));
    }

    private static ImmutableList<LayoutFragment> ScheduleBlock(SelectedBlock block, bool compact)
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
            SelectedTerminator.Match match when compact && match.Cases.Count > 1 =>
                ImmutableList.Create(new LayoutFragment(label, block.Instructions,
                    new LayoutTransfer.Match(match.Local,
                        match.Cases.Select((@case, index) => (@case.Literal, EdgeLabel(index))).ToImmutableList(),
                        EdgeLabel(match.Cases.Count), match.SliceSourceLocal)))
                .AddRange(match.Cases.Select((@case, index) => Stub(@case.Edge, index)))
                .Add(Stub(match.Default, match.Cases.Count)),
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
