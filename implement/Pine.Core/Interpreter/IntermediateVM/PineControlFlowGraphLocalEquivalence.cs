using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

namespace Pine.Core.Interpreter.IntermediateVM;

public sealed partial record PineControlFlowGraph
{
    private readonly record struct LocalPair(int Lower, int Higher)
    {
        public static LocalPair Of(int first, int second) =>
            first < second ? new(first, second) : new(second, first);
    }

    private sealed record LocalEquivalenceState(
        ImmutableHashSet<LocalPair> EqualLocals,
        ImmutableArray<ImmutableHashSet<int>> StackParameters);

    /// <summary>
    /// Forwards reads of locals proven equal on every incoming path. Stack values carry the
    /// same equality proof as locals; writes invalidate only proofs involving changed slots.
    /// </summary>
    public PineControlFlowGraph ForwardEquivalentLocalReads()
    {
        Validate();

        var states = new LocalEquivalenceState?[Blocks.Length];
        states[Entry.Value] = new([], []);

        var pending = new Queue<PineBlockId>();
        var queued = new HashSet<PineBlockId> { Entry };
        pending.Enqueue(Entry);

        while (pending.TryDequeue(out var id))
        {
            queued.Remove(id);
            var block = Blocks[id.Value];
            var (equalLocals, values) = TraceLocalEquivalence(block, states[id.Value]!, null);

            foreach (var (target, arguments) in Successors(block.Terminator))
            {
                var incomingParameters =
                    arguments.Select(value => values.GetValueOrDefault(value) ?? []).ToImmutableArray();

                if (states[target.Value] is not { } previous)
                {
                    states[target.Value] = new(equalLocals, incomingParameters);
                }
                else
                {
                    var commonLocals = previous.EqualLocals.Intersect(equalLocals);

                    var commonParameters =
                        previous.StackParameters.Zip(
                            incomingParameters,
                            (left, right) => left.Intersect(right)).ToImmutableArray();

                    if (previous.EqualLocals.SetEquals(commonLocals) &&
                        previous.StackParameters.Zip(commonParameters)
                        .All(pair => pair.First.SetEquals(pair.Second)))
                    {
                        continue;
                    }

                    states[target.Value] = new(commonLocals, commonParameters);
                }

                if (queued.Add(target))
                {
                    pending.Enqueue(target);
                }
            }
        }

        var changed = false;

        var blocks =
            Blocks.Select(
                block =>
                {
                    if (states[block.Id.Value] is not { } state)
                    {
                        return block;
                    }

                    var operations = block.Operations.ToBuilder();

                    TraceLocalEquivalence(
                        block,
                        state,
                        (index, replacement) =>
                        {
                            operations[index] =
                                operations[index] with
                                {
                                    Instruction =
                                    operations[index].Instruction.WithLocalIndex(replacement)
                                };

                            changed = true;
                        });

                    return block with { Operations = operations.ToImmutable() };
                })
            .ToImmutableArray();

        if (!changed)
        {
            return this;
        }

        var result = this with { Blocks = blocks };
        result.Validate();
        return result;
    }

    private static (
        ImmutableHashSet<LocalPair> EqualLocals,
        Dictionary<PineVirtualValueId, ImmutableHashSet<int>> Values)
        TraceLocalEquivalence(
        PineBasicBlock block,
        LocalEquivalenceState state,
        Action<int, int>? rewriteRead)
    {
        var equalLocals = state.EqualLocals;
        var values = new Dictionary<PineVirtualValueId, ImmutableHashSet<int>>();
        var stack = block.Parameters.ToList();

        for (var index = 0; index < stack.Count; index++)
        {
            values[stack[index]] = state.StackParameters[index];
        }

        for (var index = 0; index < block.Operations.Length; index++)
        {
            var operation = block.Operations[index];
            var instruction = operation.Instruction;

            if (instruction.Kind is
                StackInstructionKind.Local_Get or StackInstructionKind.Local_Get_Skip_Head_Const &&
                instruction.LocalIndices.Length is 1 &&
                instruction.SingleLocalIndex is >= 0 and var local)
            {
                var equivalents = EquivalentLocals(equalLocals, local);
                var replacement = equivalents.Min();

                if (replacement != local)
                {
                    rewriteRead?.Invoke(index, replacement);
                }

                if (instruction.Kind is StackInstructionKind.Local_Get &&
                    operation.Results.Length is 1)
                {
                    values[operation.Results[0]] = equivalents;
                }
            }

            if (instruction.Kind is StackInstructionKind.Local_Set &&
                stack.Count >= instruction.LocalIndices.Length)
            {
                var assignments =
                    instruction.LocalIndices
                    .Select((local, depth) => (local, stack[^(depth + 1)]))
                    .ToArray();

                equalLocals = StoreEqualLocals(equalLocals, values, assignments);
            }
            else if (instruction.Kind is
                StackInstructionKind.Local_Int_Add_Const or StackInstructionKind.Local_Set_Literal)
            {
                equalLocals =
                    [
                    .. equalLocals.Where(
                        pair =>
                        !instruction.LocalIndices.Contains(pair.Lower) &&
                        !instruction.LocalIndices.Contains(pair.Higher))
                    ];

                foreach (var value in values.Keys.ToArray())
                {
                    values[value] = values[value].Except(instruction.LocalIndices);
                }
            }

            if (!operation.Inputs.IsEmpty)
            {
                stack.RemoveRange(stack.Count - operation.Inputs.Length, operation.Inputs.Length);

                foreach (var input in operation.Inputs)
                {
                    values.Remove(input);
                }
            }

            stack.AddRange(operation.Results);
        }

        return (equalLocals, values);
    }

    private static ImmutableHashSet<int> EquivalentLocals(
        ImmutableHashSet<LocalPair> equalLocals,
        int local) =>
        [
            .. equalLocals
            .Where(pair => pair.Lower == local || pair.Higher == local)
            .Select(pair => pair.Lower == local ? pair.Higher : pair.Lower),
            local,
        ];

    private static ImmutableHashSet<LocalPair> StoreEqualLocals(
        ImmutableHashSet<LocalPair> equalLocals,
        Dictionary<PineVirtualValueId, ImmutableHashSet<int>> values,
        IReadOnlyList<(int Destination, PineVirtualValueId Value)> assignments)
    {
        var finalAssignments =
            assignments.GroupBy(assignment => assignment.Destination)
            .Select(group => group.Last())
            .ToArray();

        var written = finalAssignments.Select(assignment => assignment.Destination).ToHashSet();

        var sources =
            finalAssignments.Select(
                assignment => (assignment.Destination,
                assignment.Value,
                Equivalents: values.GetValueOrDefault(assignment.Value) ?? [])).ToArray();

        equalLocals =
            [.. equalLocals.Where(pair => !written.Contains(pair.Lower) && !written.Contains(pair.Higher))];

        foreach (var value in values.Keys.ToArray())
        {
            values[value] = values[value].Except(written);
        }

        foreach (var source in sources)
        {
            foreach (var other in source.Equivalents.Except(written))
            {
                equalLocals = equalLocals.Add(LocalPair.Of(source.Destination, other));
            }

            values[source.Value] =
                (values.GetValueOrDefault(source.Value) ?? []).Add(source.Destination);
        }

        for (var first = 0; first < sources.Length; first++)
        {
            for (var second = first + 1; second < sources.Length; second++)
            {
                if (sources[first].Value == sources[second].Value ||
                    sources[first].Equivalents.Overlaps(sources[second].Equivalents))
                {
                    equalLocals =
                        equalLocals.Add(LocalPair.Of(sources[first].Destination, sources[second].Destination));
                }
            }
        }

        return equalLocals;
    }
}
