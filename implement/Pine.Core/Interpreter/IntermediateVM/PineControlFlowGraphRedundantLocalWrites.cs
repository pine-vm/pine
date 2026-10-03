using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

namespace Pine.Core.Interpreter.IntermediateVM;

public sealed partial record PineControlFlowGraph
{
    /// <summary>
    /// Trims the bottom of a descending store when its values are already in the
    /// corresponding locals (or are not stored at all), and removes their discarded pushes.
    /// </summary>
    public PineControlFlowGraph EliminateRedundantLocalWrites(int parameterCount)
    {
        Validate();

        var initializedAtEntry = DefinitelyInitializedLocals(parameterCount);
        var changed = false;
        var blocks = Blocks.ToBuilder();

        foreach (var block in Blocks)
        {
            var initialized = new HashSet<int>(initializedAtEntry[block.Id.Value] ?? []);
            var initializedBefore = new HashSet<int>[block.Operations.Length];
            var producerByValue = new Dictionary<PineVirtualValueId, int>();

            for (var index = 0; index < block.Operations.Length; index++)
            {
                initializedBefore[index] = [.. initialized];
                var operation = block.Operations[index];

                foreach (var value in operation.Results)
                {
                    producerByValue[value] = index;
                }

                AddWrittenLocals(operation.Instruction, initialized);
            }

            var removed = new HashSet<int>();
            var replacements = new Dictionary<int, PineControlFlowOperation>();

            for (var index = 0; index + 1 < block.Operations.Length; index++)
            {
                var store = block.Operations[index];
                var pop = block.Operations[index + 1];

                if (store.Instruction.Kind is not StackInstructionKind.Local_Set_Descending ||
                    store.Instruction.LocalIndex is not { } highest ||
                    store.Instruction.TakeCount is not { } count ||
                    count < 0 || highest < count - 1 ||
                    !store.Inputs.IsEmpty || !store.Results.IsEmpty ||
                    pop.Instruction.Kind is not StackInstructionKind.Pop ||
                    pop.Instruction.SkipCount is not { } popCount ||
                    popCount < count || popCount != pop.Inputs.Length ||
                    !pop.Results.IsEmpty)
                {
                    continue;
                }

                var unstored = popCount - count;
                var removable = new List<int>();

                for (var position = 0; position < popCount; position++)
                {
                    if (!producerByValue.TryGetValue(pop.Inputs[position], out var producerIndex) ||
                        producerIndex >= index || removed.Contains(producerIndex))
                    {
                        break;
                    }

                    var producer = block.Operations[producerIndex];
                    var instruction = producer.Instruction;
                    var local = position < unstored ? null : (int?)(highest - popCount + position + 1);

                    if (producer.Results.Length is not 1 ||
                        producer.Results[0] != pop.Inputs[position] ||
                        !producer.Inputs.IsEmpty ||
                        (local is { } destination
                        ?
                        instruction.Kind is not StackInstructionKind.Local_Get ||
                        instruction.LocalIndex != destination ||
                        !initializedBefore[producerIndex].Contains(destination)
                        :
                        instruction.Kind switch
                        {
                            StackInstructionKind.Push_Literal => instruction.Literal is null,

                            StackInstructionKind.Local_Get =>
                            instruction.LocalIndex is not { } source ||
                            !initializedBefore[producerIndex].Contains(source),

                            _ =>
                            true
                        }) ||
                        Enumerable.Range(producerIndex + 1, index - producerIndex - 1)
                        .Any(
                            otherIndex =>
                            block.Operations[otherIndex].Instruction.Kind is
                                StackInstructionKind.Local_Set or StackInstructionKind.Local_Set_Descending ||
                            block.Operations[otherIndex].Inputs.Contains(producer.Results[0])))
                    {
                        break;
                    }

                    removable.Add(producerIndex);
                }

                if (removable.Count is 0)
                {
                    continue;
                }

                changed = true;
                removed.UnionWith(removable);
                var remainingStoreCount = count - Math.Max(0, removable.Count - unstored);
                var remainingPopCount = popCount - removable.Count;

                if (remainingStoreCount is 0)
                {
                    removed.Add(index);
                }
                else if (remainingStoreCount != count)
                {
                    replacements[index] =
                        store with
                        {
                            Instruction = StackInstruction.Local_Set_Descending(highest, remainingStoreCount)
                        };
                }

                if (remainingPopCount is 0)
                {
                    removed.Add(index + 1);
                }
                else
                {
                    replacements[index + 1] =
                        pop with
                        {
                            Instruction = StackInstruction.PopMultiple(remainingPopCount),
                            Inputs = pop.Inputs[removable.Count..]
                        };
                }
            }

            if (removed.Count is 0)
            {
                continue;
            }

            blocks[block.Id.Value] =
                block with
                {
                    Operations =
                    [
                        .. block.Operations
                        .Select((operation, index) => (operation, index))
                        .Where(item => !removed.Contains(item.index))
                        .Select(item => replacements.GetValueOrDefault(item.index, item.operation))
                    ]
                };
        }

        if (!changed)
        {
            return this;
        }

        var result = this with { Blocks = blocks.ToImmutable() };
        result.Validate();
        return result;
    }
}
