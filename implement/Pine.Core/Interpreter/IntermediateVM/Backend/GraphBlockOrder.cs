using Pine.Core.Interpreter.IntermediateVM.Semantic;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

namespace Pine.Core.Interpreter.IntermediateVM.Backend;

/// <summary>Reverse postorder keeps acyclic inlined continuations after their callers.</summary>
internal static class GraphBlockOrder
{
    internal static ImmutableList<PineBlockId> ReversePostorder(FunctionGraph graph)
    {
        return Traverse();

        ImmutableList<PineBlockId> Traverse()
        {
            var visited = new HashSet<PineBlockId>();
            var pending = new Stack<(PineBlockId Id, bool Finish)>();
            var completed = new List<PineBlockId>();
            pending.Push((graph.Entry, false));
            while (pending.TryPop(out var item))
            {
                if (item.Finish)
                {
                    completed.Add(item.Id);
                    continue;
                }
                if (!visited.Add(item.Id))
                    continue;
                pending.Push((item.Id, true));
                foreach (var successor in Successors(graph.Blocks[item.Id].Terminator).Reverse())
                    if (!visited.Contains(successor))
                        pending.Push((successor, false));
            }
            return completed.AsEnumerable().Reverse()
                .Concat(graph.Blocks.Keys.Where(id => !visited.Contains(id)).OrderBy(id => id.Value)).ToImmutableList();
        }
    }

    private static ImmutableList<PineBlockId> Successors(Terminator terminator) =>
        terminator switch
        {
            Terminator.Return or Terminator.TailInvoke => [],
            Terminator.Jump jump => [jump.Edge.Target],
            Terminator.Branch branch => [branch.IfEqual.Target, branch.IfNotEqual.Target],
            Terminator.Switch selection => [.. selection.Cases.Select(@case => @case.Edge.Target), selection.Default.Target],
            Terminator.Invoke invoke => [invoke.Continuation.Target],
            _ => throw new NotImplementedException("Successors does not handle terminator variant: " + terminator.GetType().Name),
        };
}
