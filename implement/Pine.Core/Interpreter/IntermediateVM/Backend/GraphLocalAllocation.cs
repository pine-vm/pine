using Pine.Core.Interpreter.IntermediateVM.Semantic;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

namespace Pine.Core.Interpreter.IntermediateVM.Backend;

/// <summary>
/// Conservative block-local interference and edge-copy coalescing over semantic definitions.
/// Definitions in one block remain distinct, allowing selection to fuse and reschedule pure
/// operand computations. Edge assignments remain simultaneous; no instructions are inspected.
/// </summary>
internal static class GraphLocalAllocation
{
    internal static ImmutableList<StorageBinding> Allocate(FunctionGraph graph, int firstLocal)
    {
        return Run();

        ImmutableList<StorageBinding> Run()
        {
            var blocks = graph.Blocks.Values.OrderBy(block => block.Id.Value).ToImmutableList();
            var definitions = blocks.SelectMany(block => block.Parameters.Concat(block.Operations.Select(InstructionSelection.Result)))
                .Select(definition => definition.Id).ToImmutableList();
            if (blocks.Sum(block =>
                (long)(block.Parameters.Count + block.Operations.Count) * (block.Parameters.Count + block.Operations.Count)) > 1_000_000)
                return definitions.Select((id, index) => new StorageBinding(id, checked(firstLocal + index))).ToImmutableList();
            var parents = definitions.ToDictionary(id => id, id => id);
            var conflicts = definitions.ToDictionary(id => id, _ => new HashSet<PineVirtualValueId>());
            foreach (var block in blocks)
            {
                var localDefinitions = block.Parameters.Concat(block.Operations.Select(InstructionSelection.Result)).ToImmutableList();
                foreach (var definition in localDefinitions)
                    foreach (var other in localDefinitions)
                        Interfere(definition.Id, other.Id);
            }
            foreach (var block in blocks)
                foreach (var (source, target) in Copies(block.Terminator))
                    Coalesce(source, target);
            var colors = new Dictionary<PineVirtualValueId, int>();
            foreach (var definition in definitions)
            {
                var group = Find(definition);
                if (colors.ContainsKey(group))
                    continue;
                var unavailable = conflicts[group].Select(Find).Where(colors.ContainsKey).Select(neighbor => colors[neighbor]).ToHashSet();
                var color = firstLocal;
                while (unavailable.Contains(color))
                    color = checked(color + 1);
                colors.Add(group, color);
            }
            return definitions.Select(id => new StorageBinding(id, colors[Find(id)])).ToImmutableList();

            PineVirtualValueId Find(PineVirtualValueId id)
            {
                while (parents[id] != id)
                    id = parents[id];
                return id;
            }

            void Interfere(PineVirtualValueId left, PineVirtualValueId right)
            {
                if (left == right)
                    return;
                conflicts[left].Add(right);
                conflicts[right].Add(left);
            }

            void Coalesce(PineVirtualValueId source, PineVirtualValueId target)
            {
                var left = Find(source);
                var right = Find(target);
                if (left == right || conflicts[left].Contains(right))
                    return;
                var keep = left.Value < right.Value ? left : right;
                var remove = keep == left ? right : left;
                parents[remove] = keep;
                foreach (var neighbor in conflicts[remove])
                {
                    conflicts[neighbor].Remove(remove);
                    conflicts[neighbor].Add(keep);
                    conflicts[keep].Add(neighbor);
                }
                conflicts.Remove(remove);
            }

            ImmutableList<(PineVirtualValueId, PineVirtualValueId)> EdgeCopies(Edge edge) =>
                edge.Arguments.Select((argument, index) => (argument, graph.Blocks[edge.Target].Parameters[index].Id)).ToImmutableList();

            ImmutableList<(PineVirtualValueId, PineVirtualValueId)> Copies(Terminator terminator) =>
                terminator switch
                {
                    Terminator.Return or Terminator.TailInvoke => [],
                    Terminator.Jump jump => EdgeCopies(jump.Edge),
                    Terminator.Branch branch => EdgeCopies(branch.IfEqual).AddRange(EdgeCopies(branch.IfNotEqual)),
                    Terminator.Switch selection => selection.Cases.SelectMany(@case => EdgeCopies(@case.Edge)).ToImmutableList()
                        .AddRange(EdgeCopies(selection.Default)),
                    Terminator.Invoke invoke => invoke.Continuation.Bindings.SelectMany((binding, index) => binding switch
                    {
                        ContinuationBinding.CallerValue caller => ImmutableList.Create(
                            (caller.Value, graph.Blocks[invoke.Continuation.Target].Parameters[index].Id)),
                        ContinuationBinding.ReturnedResult => [],
                        _ => throw new NotImplementedException("Copies does not handle binding variant: " + binding.GetType().Name),
                    }).ToImmutableList(),
                    _ => throw new NotImplementedException("Copies does not handle terminator variant: " + terminator.GetType().Name),
                };
        }
    }
}
