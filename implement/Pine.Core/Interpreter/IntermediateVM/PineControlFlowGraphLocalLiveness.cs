using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

namespace Pine.Core.Interpreter.IntermediateVM;

public sealed partial record PineControlFlowGraph
{
    /// <summary>
    /// Removes writes to locals that cannot be read before their next write or the end of the frame.
    /// Stack values and operations producing them remain unchanged.
    /// </summary>
    public PineControlFlowGraph EliminateDeadLocalStores()
    {
        var uses = new HashSet<int>[Blocks.Length];
        var definitions = new HashSet<int>[Blocks.Length];
        var liveIn = new HashSet<int>[Blocks.Length];
        var liveOut = new HashSet<int>[Blocks.Length];

        foreach (var block in Blocks)
        {
            var readBeforeWrite = new HashSet<int>();
            var written = new HashSet<int>();

            foreach (var operation in block.Operations)
            {
                var instruction = operation.Instruction;

                foreach (var read in ReadLocals(instruction))
                {
                    if (!written.Contains(read))
                        readBeforeWrite.Add(read);
                }

                AddWrittenLocals(instruction, written);
            }

            uses[block.Id.Value] = readBeforeWrite;
            definitions[block.Id.Value] = written;
            liveIn[block.Id.Value] = [.. readBeforeWrite];
            liveOut[block.Id.Value] = [];
        }

        var predecessors = BuildPredecessors();
        var pending = new Queue<PineBlockId>(Blocks.Select(block => block.Id));
        var queued = new HashSet<PineBlockId>(pending);

        while (pending.TryDequeue(out var blockId))
        {
            queued.Remove(blockId);
            var index = blockId.Value;
            var successorLive = new HashSet<int>();

            foreach (var (target, _) in Successors(Blocks[index].Terminator))
            {
                successorLive.UnionWith(liveIn[target.Value]);
            }

            liveOut[index] = successorLive;

            var incoming = new HashSet<int>(successorLive);
            incoming.ExceptWith(definitions[index]);
            incoming.UnionWith(uses[index]);

            if (liveIn[index].SetEquals(incoming))
            {
                continue;
            }

            liveIn[index] = incoming;

            if (predecessors.TryGetValue(blockId, out var incomingBlocks))
            {
                foreach (var predecessor in incomingBlocks)
                {
                    if (queued.Add(predecessor))
                    {
                        pending.Enqueue(predecessor);
                    }
                }
            }
        }

        var changed = false;

        var blocks =
            Blocks.Select(
                block =>
                {
                    var live = new HashSet<int>(liveOut[block.Id.Value]);
                    var operations = block.Operations.ToBuilder();

                    for (var index = operations.Count - 1; index >= 0; index--)
                    {
                        var instruction = operations[index].Instruction;

                        if (instruction.Kind is
                            StackInstructionKind.Local_Set or StackInstructionKind.Local_Set_Literal &&
                            !instruction.LocalIndices.Any(live.Contains))
                        {
                            if (instruction.PopCount is > 0)
                            {
                                instruction = StackInstruction.PopMultiple(instruction.PopCount.Value);
                                operations[index] = operations[index] with { Instruction = instruction };
                            }
                            else
                            {
                                operations.RemoveAt(index);
                                changed = true;
                                continue;
                            }

                            changed = true;
                        }

                        if (instruction.Kind is StackInstructionKind.Local_Set &&
                            instruction.LocalIndices.Length > 1)
                        {
                            var lastLiveDepth = -1;

                            for (var depth = 0; depth < instruction.LocalIndices.Length; depth++)
                            {
                                if (live.Contains(instruction.LocalIndices[depth]))
                                {
                                    lastLiveDepth = depth;
                                }
                            }

                            if (lastLiveDepth < 0)
                            {
                                if (instruction.PopCount is > 0)
                                {
                                    instruction = StackInstruction.PopMultiple(instruction.PopCount.Value);
                                    operations[index] = operations[index] with { Instruction = instruction };
                                }
                                else
                                {
                                    operations.RemoveAt(index);
                                    changed = true;
                                    continue;
                                }

                                changed = true;
                            }

                            if (lastLiveDepth + 1 < instruction.LocalIndices.Length)
                            {
                                instruction =
                                    instruction with
                                    {
                                        LocalIndices = instruction.LocalIndices[..(lastLiveDepth + 1)]
                                    };

                                operations[index] =
                                    operations[index] with
                                    {
                                        Instruction = instruction
                                    };

                                changed = true;
                            }
                        }

                        var written = new HashSet<int>();
                        AddWrittenLocals(instruction, written);
                        live.ExceptWith(written);

                        foreach (var read in ReadLocals(instruction))
                            live.Add(read);
                    }

                    return block with { Operations = operations.ToImmutable() };
                }).ToImmutableArray();

        if (!changed)
        {
            return this;
        }

        var result = this with { Blocks = blocks };
        result.Validate();
        return result;
    }

    private static IEnumerable<int> ReadLocals(StackInstruction instruction) =>
        instruction.Kind is StackInstructionKind.Local_Get or
            StackInstructionKind.Local_Get_Skip_Head_Const or StackInstructionKind.Local_Int_Add_Const
        ?
        instruction.LocalIndices
        :
        [];

    private static void AddWrittenLocals(StackInstruction instruction, HashSet<int> written)
    {
        if (instruction.Kind is StackInstructionKind.Local_Set or
            StackInstructionKind.Local_Set_Literal or StackInstructionKind.Local_Int_Add_Const)
        {
            foreach (var slot in instruction.LocalIndices)
                written.Add(slot);
        }
    }
}
