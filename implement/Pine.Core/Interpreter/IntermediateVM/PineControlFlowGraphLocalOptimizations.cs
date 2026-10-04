using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Numerics;

namespace Pine.Core.Interpreter.IntermediateVM;

public sealed partial record PineControlFlowGraph
{
    /// <summary>
    /// Eliminates a local-to-local copy when all reads of the destination stay in the
    /// same block and the source retains its value until the last read.
    /// </summary>
    public PineControlFlowGraph EliminateLocalCopies()
    {
        var blocks = Blocks;

        foreach (var originalBlock in Blocks)
        {
            var block = originalBlock;

            for (var index = 0; index + 2 < block.Operations.Length; index++)
            {
                var get = block.Operations[index];
                var set = block.Operations[index + 1];
                var pop = block.Operations[index + 2];

                if (get.Instruction.Kind is not StackInstructionKind.Local_Get ||
                    get.Instruction.LocalIndex is not { } source ||
                    get.Results.Length is not 1 ||
                    set.Instruction.Kind is not StackInstructionKind.Local_Set_Descending ||
                    set.Instruction.TakeCount is not 1 ||
                    set.Instruction.LocalIndex is not { } destination ||
                    !set.Inputs.IsEmpty ||
                    !set.Results.IsEmpty ||
                    source == destination ||
                    pop.Instruction.Kind is not StackInstructionKind.Pop ||
                    pop.Instruction.SkipCount is not 1 ||
                    pop.Inputs.Length is not 1 ||
                    pop.Inputs[0] != get.Results[0] ||
                    !pop.Results.IsEmpty)
                {
                    continue;
                }

                var lastRead = -1;
                var safe = true;

                foreach (var otherBlock in blocks)
                {
                    for (var otherIndex = 0; otherIndex < otherBlock.Operations.Length; otherIndex++)
                    {
                        var instruction = otherBlock.Operations[otherIndex].Instruction;

                        if (WritesLocal(instruction, destination) &&
                            (otherBlock.Id != block.Id || otherIndex != index + 1))
                        {
                            safe = false;
                        }

                        if (instruction.LocalIndex != destination ||
                            instruction.Kind is not
                            (StackInstructionKind.Local_Get or
                             StackInstructionKind.Local_Get_Skip_Head_Const))
                        {
                            continue;
                        }

                        if (otherBlock.Id != block.Id || otherIndex <= index + 2)
                        {
                            safe = false;
                        }

                        lastRead = otherIndex;
                    }
                }

                if (!safe || lastRead < 0 ||
                    block.Operations.Skip(index + 3).Take(lastRead - index - 2)
                    .Any(operation => WritesLocal(operation.Instruction, source)))
                {
                    continue;
                }

                var operations = ImmutableArray.CreateBuilder<PineControlFlowOperation>();

                for (var operationIndex = 0; operationIndex < block.Operations.Length; operationIndex++)
                {
                    if (operationIndex >= index && operationIndex <= index + 2)
                    {
                        continue;
                    }

                    var operation = block.Operations[operationIndex];

                    if (operationIndex > index + 2 &&
                        operationIndex <= lastRead &&
                        operation.Instruction.LocalIndex == destination &&
                        operation.Instruction.Kind is
                        StackInstructionKind.Local_Get or
                        StackInstructionKind.Local_Get_Skip_Head_Const)
                    {
                        operation =
                            operation with
                            {
                                Instruction = operation.Instruction with { LocalIndex = source }
                            };
                    }

                    operations.Add(operation);
                }

                block = block with { Operations = operations.ToImmutable() };
                blocks = blocks.SetItem(block.Id.Value, block);
                index = -1;
            }
        }

        var result = this with { Blocks = blocks };
        result.Validate();
        return result;
    }

    private static bool WritesLocal(StackInstruction instruction, int index) =>
        instruction.Kind is StackInstructionKind.Local_Set or StackInstructionKind.Local_Int_Add_Const &&
        instruction.LocalIndex == index ||
        instruction.Kind is StackInstructionKind.Local_Set_Descending &&
        instruction.LocalIndex is { } highest &&
        instruction.TakeCount is { } count &&
        index <= highest &&
        index > highest - count;

    /// <summary>
    /// Fuses local reads followed by a constant list projection before assigning jump offsets.
    /// </summary>
    public PineControlFlowGraph FuseLocalListProjections()
    {
        var blocks =
            Blocks.Select(
                block =>
                {
                    var operations = ImmutableArray.CreateBuilder<PineControlFlowOperation>();

                    for (var index = 0; index < block.Operations.Length; index++)
                    {
                        var get = block.Operations[index];

                        if (index + 1 < block.Operations.Length &&
                            get.Instruction.Kind is StackInstructionKind.Local_Get &&
                            get.Instruction.LocalIndex is { } local &&
                            get.Results.Length is 1 &&
                            block.Operations[index + 1] is { } projection &&
                            projection.Instruction.Kind is StackInstructionKind.Skip_Head_Const &&
                            projection.Instruction.SkipCount is { } skip &&
                            projection.Inputs.Length is 1 &&
                            projection.Inputs[0] == get.Results[0])
                        {
                            operations.Add(
                                new(
                                    StackInstruction.Local_Get_Skip_Head_Const(local, skip),
                                    [],
                                    projection.Results));

                            index++;
                            continue;
                        }

                        operations.Add(get);
                    }

                    return block with { Operations = operations.ToImmutable() };
                }).ToImmutableArray();

        var result = this with { Blocks = blocks };
        result.Validate();
        return result;
    }

    private readonly record struct LocalUpdateSource(int LocalIndex, BigInteger? Increment);

    /// <summary>
    /// Replaces a discarded descending store of unchanged locals and independent constant
    /// increments with updates to just the affected locals.
    /// </summary>
    public PineControlFlowGraph FuseDescendingLocalIntegerAdditions(int parameterCount)
    {
        Validate();

        var initializedAtEntry = DefinitelyInitializedLocals(parameterCount);
        var changed = false;
        var blocks = Blocks.ToBuilder();

        foreach (var block in Blocks)
        {
            var initialized = new HashSet<int>(initializedAtEntry[block.Id.Value] ?? []);
            var operations = ImmutableArray.CreateBuilder<PineControlFlowOperation>();
            var blockChanged = false;

            for (var index = 0; index < block.Operations.Length; index++)
            {
                var store = block.Operations[index];

                if (store.Instruction.Kind is StackInstructionKind.Local_Set_Descending &&
                    store.Instruction.LocalIndex is { } highest &&
                    store.Instruction.TakeCount is { } count &&
                    count > 0 && highest >= count - 1 &&
                    store.Inputs.IsEmpty && store.Results.IsEmpty &&
                    index + 1 < block.Operations.Length &&
                    block.Operations[index + 1] is { } pop &&
                    pop.Instruction.Kind is StackInstructionKind.Pop &&
                    pop.Instruction.SkipCount == count &&
                    pop.Inputs.Length == count &&
                    pop.Results.IsEmpty &&
                    TryGetDiscardedLocalUpdates(
                        block.Operations,
                        index,
                        highest,
                        count,
                        pop.Inputs,
                        initialized,
                        out var start,
                        out var updates))
                {
                    // Only remove operations that are still immediately before this store.
                    var removedCount = index - start;

                    if (removedCount <= operations.Count &&
                        Enumerable.Range(0, removedCount).All(
                            offset =>
                            ReferenceEquals(
                                operations[operations.Count - removedCount + offset],
                                block.Operations[start + offset])))
                    {
                        operations.RemoveRange(operations.Count - removedCount, removedCount);

                        foreach (var update in updates)
                        {
                            var instruction =
                                StackInstruction.Local_Int_Add_Const(
                                    update.LocalIndex,
                                    update.Increment!.Value);

                            operations.Add(new PineControlFlowOperation(instruction, [], []));
                            AddWrittenLocals(instruction, initialized);
                        }

                        index++;
                        changed = true;
                        blockChanged = true;
                        continue;
                    }
                }

                operations.Add(store);
                AddWrittenLocals(store.Instruction, initialized);
            }

            if (blockChanged)
            {
                blocks[block.Id.Value] = block with { Operations = operations.ToImmutable() };
            }
        }

        if (!changed)
        {
            return this;
        }

        var result = this with { Blocks = blocks.ToImmutable() };
        result.Validate();
        return result;
    }

    private static bool TryGetDiscardedLocalUpdates(
        ImmutableArray<PineControlFlowOperation> operations,
        int storeIndex,
        int highest,
        int count,
        ImmutableArray<PineVirtualValueId> discarded,
        HashSet<int> initialized,
        out int start,
        out List<LocalUpdateSource> updates)
    {
        start = storeIndex;
        updates = [];

        var sources = new LocalUpdateSource[count];

        for (var position = count - 1; position >= 0; position--)
        {
            BigInteger? increment = null;

            if (start > 0 &&
                operations[start - 1] is { } add &&
                add.Instruction.Kind is StackInstructionKind.Int_Add_Const &&
                add.Instruction.IntegerLiteral is { } literal &&
                add.Inputs.Length == 1 &&
                add.Results.Length == 1 &&
                add.Results[0] == discarded[position])
            {
                increment = literal;
                start--;
            }

            var value =
                increment is null
                ?
                discarded[position]
                :
                operations[start].Inputs[0];

            var destination = highest - count + position + 1;

            if (start == 0 ||
                operations[start - 1] is not { } get ||
                get.Instruction.Kind is not StackInstructionKind.Local_Get ||
                get.Instruction.LocalIndex != destination ||
                !get.Inputs.IsEmpty ||
                get.Results.Length != 1 ||
                get.Results[0] != value ||
                !initialized.Contains(destination))
            {
                return false;
            }

            start--;
            sources[position] = new LocalUpdateSource(destination, increment);
        }

        if (!sources.Any(source => source.Increment is not null))
        {
            return false;
        }

        updates.AddRange(sources.Where(source => source.Increment is not null));
        return true;
    }

    /// <summary>
    /// Fuses a local increment and its immediately discarded stack result into one local update.
    /// </summary>
    public PineControlFlowGraph FuseLocalIntegerAdditions()
    {
        var blocks =
            Blocks.Select(
                block =>
                {
                    var operations = ImmutableArray.CreateBuilder<PineControlFlowOperation>();

                    for (var index = 0; index < block.Operations.Length; index++)
                    {
                        if (index + 3 < block.Operations.Length &&
                            block.Operations[index] is { } get &&
                            get.Instruction.Kind is StackInstructionKind.Local_Get &&
                            get.Instruction.LocalIndex is >= 0 and var local &&
                            get.Inputs.IsEmpty &&
                            get.Results.Length is 1 &&
                            block.Operations[index + 1] is { } add &&
                            add.Instruction.Kind is StackInstructionKind.Int_Add_Const &&
                            add.Instruction.IntegerLiteral is { } increment &&
                            add.Inputs.Length is 1 &&
                            add.Inputs[0] == get.Results[0] &&
                            add.Results.Length is 1 &&
                            block.Operations[index + 2] is { } set &&
                            set.Instruction.Kind is StackInstructionKind.Local_Set_Descending &&
                            set.Instruction.LocalIndex == local &&
                            set.Instruction.TakeCount is 1 &&
                            set.Inputs.IsEmpty &&
                            set.Results.IsEmpty &&
                            block.Operations[index + 3] is { } pop &&
                            pop.Instruction.Kind is StackInstructionKind.Pop &&
                            pop.Instruction.SkipCount is 1 &&
                            pop.Inputs.Length is 1 &&
                            pop.Inputs[0] == add.Results[0] &&
                            pop.Results.IsEmpty)
                        {
                            operations.Add(
                                new(
                                    StackInstruction.Local_Int_Add_Const(local, increment),
                                    [],
                                    []));

                            index += 3;
                            continue;
                        }

                        operations.Add(block.Operations[index]);
                    }

                    return block with { Operations = operations.ToImmutable() };
                }).ToImmutableArray();

        var result = this with { Blocks = blocks };
        result.Validate();
        return result;
    }
}
