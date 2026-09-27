using System.Collections.Immutable;
using System.Linq;

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
                        (StackInstructionKind.Local_Get or
                         StackInstructionKind.Local_Get_Skip_Head_Const))
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
        instruction.Kind is StackInstructionKind.Local_Set &&
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
}
