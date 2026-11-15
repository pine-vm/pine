using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

namespace Pine.Core.Runtime.IntermediateVM;

public sealed partial record PineControlFlowGraph
{
    /// <summary>
    /// Removes writes to locals that cannot be read before their next write or the end of the frame.
    /// Stack values and operations producing them remain unchanged.
    /// </summary>
    public PineControlFlowGraph EliminateDeadLocalStores()
        => EliminateDeadLocalStores(null);

    private PineControlFlowGraph EliminateDeadLocalStores(OptimizationContext? context)
    {
        var liveOut = context?.GetLiveness(this) ?? AnalyzeLocalLiveness(null);
        return RewriteDeadLocalStores(liveOut, context);
    }

    private LocalSet[] AnalyzeLocalLiveness(OptimizationContext? context)
    {
        var uses = new LocalSet[Blocks.Length];
        var definitions = new LocalSet[Blocks.Length];
        var liveIn = new LocalSet[Blocks.Length];
        var liveOut = new LocalSet[Blocks.Length];

        var summary = context?.GetSummary(this);

        foreach (var block in Blocks)
        {
            if (summary is not null)
            {
                uses[block.Id.Value] = summary.Blocks[block.Id.Value].Uses;
                definitions[block.Id.Value] = summary.Blocks[block.Id.Value].Written;
                liveIn[block.Id.Value] = uses[block.Id.Value].Clone();
                continue;
            }

            var readBeforeWrite = new LocalSet();
            var written = new LocalSet();

            foreach (var operation in block.Operations)
            {
                var instruction = operation.Instruction;

                foreach (var read in ReadLocals(instruction))
                {
                    if (!written.Contains(read))
                        readBeforeWrite.Add(read);
                }

                AddWrittenLocals(instruction, ref written);
            }

            uses[block.Id.Value] = readBeforeWrite;
            definitions[block.Id.Value] = written;
            liveIn[block.Id.Value] = readBeforeWrite.Clone();
        }

        var predecessors = context?.GetPredecessors(this) ?? BuildPredecessors();
        var pending = new Queue<PineBlockId>(Blocks.Select(block => block.Id));
        var queued = new bool[Blocks.Length];
        System.Array.Fill(queued, true);
        var incoming = new LocalSet();

        while (pending.TryDequeue(out var blockId))
        {
            queued[blockId.Value] = false;
            var index = blockId.Value;
            ref var successorLive = ref liveOut[index];
            successorLive.Clear();

            foreach (var (target, _) in Successors(Blocks[index].Terminator))
            {
                successorLive.UnionWith(liveIn[target.Value]);
            }

            incoming.CopyFrom(successorLive);
            incoming.ExceptWith(definitions[index]);
            incoming.UnionWith(uses[index]);

            if (liveIn[index].SetEquals(incoming))
            {
                continue;
            }

            liveIn[index].CopyFrom(incoming);

            if (predecessors.TryGetValue(blockId, out var incomingBlocks))
            {
                foreach (var predecessor in incomingBlocks)
                {
                    if (!queued[predecessor.Value])
                    {
                        queued[predecessor.Value] = true;
                        pending.Enqueue(predecessor);
                    }
                }
            }
        }

        return liveOut;
    }

    private PineControlFlowGraph RewriteDeadLocalStores(LocalSet[] liveOut, OptimizationContext? context)
    {
        var changed = false;

        var blocks =
            RewriteBlocks(
                block =>
                {
                    var live = liveOut[block.Id.Value].Clone();
                    var operations = new OperationBuilder(block.Operations, includeAll: true);

                    for (var index = operations.Count - 1; index >= 0; index--)
                    {
                        var instruction = operations[index].Instruction;

                        if (instruction.Kind is
                            StackInstructionKind.Local_Set or StackInstructionKind.Local_Set_Literal &&
                            !live.ContainsAny(instruction.LocalIndices))
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

                        foreach (var local in WrittenLocals(instruction))
                            live.Remove(local);

                        foreach (var read in ReadLocals(instruction))
                            live.Add(read);
                    }

                    return operations.Build(block);
                });

        if (!changed)
        {
            return this;
        }

        var result = this with { Blocks = blocks };
        result.Validate(context);
        return result;
    }

    private static ImmutableArray<int> ReadLocals(StackInstruction instruction) =>
        instruction.Kind is StackInstructionKind.Local_Get or
            StackInstructionKind.Local_Get_Skip_Head_Const or StackInstructionKind.Local_Int_Add_Const
        ?
        instruction.LocalIndices
        :
        [];

    private static void AddWrittenLocals(StackInstruction instruction, HashSet<int> written)
    {
        foreach (var slot in WrittenLocals(instruction))
            written.Add(slot);
    }

    private static void AddWrittenLocals(StackInstruction instruction, ref LocalSet written)
    {
        foreach (var slot in WrittenLocals(instruction))
            written.Add(slot);
    }

    private static ImmutableArray<int> WrittenLocals(StackInstruction instruction) =>
        instruction.Kind is StackInstructionKind.Local_Set or
            StackInstructionKind.Local_Set_Literal or StackInstructionKind.Local_Int_Add_Const
        ?
        instruction.LocalIndices
        :
        [];
}
