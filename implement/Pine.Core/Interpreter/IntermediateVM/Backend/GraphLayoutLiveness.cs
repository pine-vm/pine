using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

namespace Pine.Core.Interpreter.IntermediateVM.Backend;

/// <summary>Local liveness on the already explicit symbolic layout, before numeric emission.</summary>
internal static class GraphLayoutLiveness
{
    internal static bool IsDeadAfterTransfer(ImmutableList<LayoutFragment> layout, LayoutFragment source, int local)
    {
        return Search();

        bool Search()
        {
            var byLabel = layout.ToImmutableDictionary(fragment => fragment.Label);
            var visited = new HashSet<LayoutLabel>();
            var pending = new Stack<LayoutLabel>(Successors(source.Transfer));
            while (pending.TryPop(out var label))
            {
                if (!visited.Add(label))
                    continue;
                var fragment = byLabel[label];
                var killed = false;
                foreach (var instruction in fragment.Instructions)
                {
                    if (instruction is SelectedInstruction.Store store && store.Local == local)
                    {
                        killed = true;
                        break;
                    }
                    if (Before(instruction, ImmutableHashSet<int>.Empty).Contains(local))
                        return false;
                }
                if (killed)
                    continue;
                if (TransferUses(fragment.Transfer).Contains(local))
                    return false;
                foreach (var successor in Successors(fragment.Transfer))
                    pending.Push(successor);
            }
            return true;
        }
    }

    internal static ImmutableList<LayoutFragment> RemoveDeadStores(ImmutableList<LayoutFragment> layout)
    {
        return Run();

        ImmutableList<LayoutFragment> Run()
        {
            if (layout.Count == 0)
                return layout;
            var byLabel = layout.ToImmutableDictionary(fragment => fragment.Label);
            var reachable = new HashSet<LayoutLabel>();
            var pending = new Stack<LayoutLabel>([layout[0].Label]);
            while (pending.TryPop(out var label))
                if (reachable.Add(label))
                    foreach (var successor in Successors(byLabel[label].Transfer))
                        pending.Push(successor);
            var fragments = layout.Where(fragment => reachable.Contains(fragment.Label)).ToImmutableList();
            var incoming = fragments.ToDictionary(fragment => fragment.Label, _ => ImmutableHashSet<int>.Empty);
            var changed = true;
            long remaining = 1_000_000;
            while (changed)
            {
                changed = false;
                foreach (var fragment in fragments.Reverse())
                {
                    if (fragment.Instructions.Count + 1 > remaining)
                        return fragments;
                    remaining -= fragment.Instructions.Count + 1;
                    var live = LiveOut(fragment);
                    foreach (var instruction in fragment.Instructions.Reverse())
                        live = Before(instruction, live);
                    if (!live.SetEquals(incoming[fragment.Label]))
                    {
                        incoming[fragment.Label] = live;
                        changed = true;
                    }
                }
            }
            return fragments.Select(Remove).ToImmutableList();

            ImmutableHashSet<int> LiveOut(LayoutFragment fragment) =>
                Successors(fragment.Transfer).SelectMany(successor => incoming[successor]).ToImmutableHashSet()
                    .Union(TransferUses(fragment.Transfer));

            LayoutFragment Remove(LayoutFragment fragment)
            {
                var live = LiveOut(fragment);
                var retained = ImmutableList<SelectedInstruction>.Empty;
                foreach (var instruction in fragment.Instructions.Reverse())
                {
                    if (fragment.Label.Kind == LayoutLabelKind.Prologue ||
                        instruction is not SelectedInstruction.Store store || live.Contains(store.Local))
                        retained = retained.Insert(0, instruction);
                    live = Before(instruction, live);
                }
                var compact = ImmutableList.CreateBuilder<SelectedInstruction>();
                foreach (var instruction in retained)
                {
                    if (instruction is SelectedInstruction.Load load && compact.Count >= 2 &&
                        compact[^1] is SelectedInstruction.Pop && compact[^2] is SelectedInstruction.Store store &&
                        store.Local == load.Local)
                        compact.RemoveAt(compact.Count - 1);
                    else if (instruction is SelectedInstruction.Pop && compact.Count > 0 &&
                        compact[^1] is SelectedInstruction.Literal or SelectedInstruction.Load)
                        compact.RemoveAt(compact.Count - 1);
                    else
                        compact.Add(instruction);
                }
                return fragment with { Instructions = compact.ToImmutable() };
            }
        }
    }

    private static ImmutableHashSet<int> Before(SelectedInstruction instruction, ImmutableHashSet<int> after) =>
        instruction switch
        {
            SelectedInstruction.Store store => after.Remove(store.Local),
            SelectedInstruction.Load load => after.Add(load.Local),
            SelectedInstruction.Builtin builtin => after.Union(
                ImmutableList<int?>.Empty.Add(builtin.CountLocal).Add(builtin.SourceLocal).OfType<int>()),
            SelectedInstruction.Literal or SelectedInstruction.Pop or SelectedInstruction.MakeList or
                SelectedInstruction.Project or SelectedInstruction.Return => after,
            _ => throw new NotImplementedException("Before does not handle instruction variant: " + instruction.GetType().Name),
        };

    private static ImmutableList<int> TransferUses(LayoutTransfer transfer) =>
        transfer switch
        {
            LayoutTransfer.Return ret => [ret.Local],
            LayoutTransfer.Branch branch => [branch.Local],
            LayoutTransfer.Match match => match.SliceSourceLocal is { } source ? [source, match.Local] : [match.Local],
            LayoutTransfer.Jump => [],
            LayoutTransfer.Invoke invoke => CallUses(invoke.Call),
            LayoutTransfer.TailInvoke invoke => CallUses(invoke.Call),
            _ => throw new NotImplementedException("TransferUses does not handle transfer variant: " + transfer.GetType().Name),
        };

    private static ImmutableList<int> CallUses(SelectedCall call) => call.Arguments.AddRange(call.Target switch
    {
        SelectedCallTarget.Dynamic target => ImmutableList.Create(target.Local),
        SelectedCallTarget.Known => [],
        _ => throw new NotImplementedException("CallUses does not handle target variant: " + call.Target.GetType().Name),
    });

    internal static ImmutableList<LayoutLabel> Successors(LayoutTransfer transfer) =>
        transfer switch
        {
            LayoutTransfer.Return or LayoutTransfer.TailInvoke => [],
            LayoutTransfer.Jump jump => [jump.Target],
            LayoutTransfer.Branch branch => [branch.Equal, branch.NotEqual],
            LayoutTransfer.Match match => [.. match.Cases.Select(@case => @case.Target), match.Default],
            LayoutTransfer.Invoke invoke => [invoke.Success],
            _ => throw new NotImplementedException("Successors does not handle transfer variant: " + transfer.GetType().Name),
        };
}
