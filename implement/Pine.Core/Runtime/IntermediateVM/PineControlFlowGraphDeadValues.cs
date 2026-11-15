using System.Collections.Generic;

namespace Pine.Core.Runtime.IntermediateVM;

public sealed partial record PineControlFlowGraph
{
    /// <summary>
    /// Cancels discarded stack results against computations that have no observable effect.
    /// A discarded computation's operands become discarded in turn, allowing whole chains
    /// to disappear while retaining any inputs that still need to execute.
    /// </summary>
    public PineControlFlowGraph EliminateDiscardedStackValues(int parameterCount)
        => EliminateDiscardedStackValues(parameterCount, null);

    private PineControlFlowGraph EliminateDiscardedStackValues(int parameterCount, OptimizationContext? context)
    {
        Validate(context);

        var initializedAtEntry = DefinitelyInitializedLocals(parameterCount, context);

        var blocks =
            RewriteBlocks(
                block =>
                {
                    var initialized = initializedAtEntry[block.Id.Value]?.Clone() ?? default;
                    var operations = new OperationBuilder(block.Operations);

                    foreach (var operation in block.Operations)
                    {
                        var instruction = operation.Instruction;

                        if (instruction.Kind is StackInstructionKind.Pop && instruction.PopCount is > 0)
                        {
                            var discarded = operation.Inputs;
                            var removedProducer = false;

                            while (!discarded.IsEmpty &&
                                operations.Count > 0 &&
                                operations[^1] is { Results.Length: 1 } producer &&
                                producer.Results[0] == discarded[^1] &&
                                IsDiscardable(producer.Instruction, initialized))
                            {
                                operations.RemoveAt(operations.Count - 1);
                                removedProducer = true;
                                discarded = [.. discarded[..^1], .. producer.Inputs];
                            }

                            if (!removedProducer && discarded.Length == instruction.PopCount)
                            {
                                operations.Add(operation);
                            }
                            else if (!discarded.IsEmpty)
                            {
                                operations.Add(
                                    operation with
                                    {
                                        Instruction = StackInstruction.PopMultiple(discarded.Length),
                                        Inputs = discarded
                                    });
                            }

                            continue;
                        }

                        operations.Add(operation);
                        AddWrittenLocals(instruction, ref initialized);
                    }

                    return operations.Build(block);
                });

        var result = blocks == Blocks ? this : this with { Blocks = blocks };
        result.Validate(context);
        return result;
    }

    private static bool IsDiscardable(StackInstruction instruction, in LocalSet initialized) =>
        instruction.Kind switch
        {
            StackInstructionKind.Head_Generic or
            StackInstructionKind.Length =>
            true,

            StackInstructionKind.Push_Literal =>
            instruction.Literal is not null,

            StackInstructionKind.Skip_Head_Const =>
            instruction.SkipCount is not null,

            StackInstructionKind.Length_Equal_Const =>
            instruction.IntegerLiteral is not null,

            StackInstructionKind.Build_List =>
            instruction.TakeCount is >= 0,

            StackInstructionKind.Build_List_With_Prefix =>
            instruction.TakeCount is >= 0 && instruction.Literal?.ListItemsOrNull() is not null,

            // A missing local raises an error in the VM, so an uninitialized read cannot disappear.
            StackInstructionKind.Local_Get =>
            initialized.ContainsAll(instruction.LocalIndices),

            StackInstructionKind.Local_Get_Skip_Head_Const =>
            initialized.ContainsAll(instruction.LocalIndices) &&
            instruction.SkipCount is not null &&
            !instruction.LocalIndices.IsDefaultOrEmpty,

            _ =>
            false
        };

    private LocalSet?[] DefinitelyInitializedLocals(int parameterCount, OptimizationContext? context = null) =>
        context?.GetInitializedLocals(this, parameterCount) ?? ComputeDefinitelyInitializedLocals(parameterCount);

    private LocalSet?[] ComputeDefinitelyInitializedLocals(int parameterCount, GraphSummary? summary = null)
    {
        System.ArgumentOutOfRangeException.ThrowIfNegative(parameterCount);
        var atEntry = new LocalSet?[Blocks.Length];
        var parameters = new LocalSet();

        for (var local = 0; local < parameterCount; local++)
            parameters.Add(local);

        atEntry[Entry.Value] = parameters;

        var pending = new Queue<PineBlockId>();
        var queued = new bool[Blocks.Length];
        queued[Entry.Value] = true;
        pending.Enqueue(Entry);
        var initialized = new LocalSet();

        while (pending.TryDequeue(out var id))
        {
            queued[id.Value] = false;
            initialized.CopyFrom(atEntry[id.Value]!.Value);

            if (summary is not null)
                initialized.UnionWith(summary.Blocks[id.Value].Written);

            else
            {
                foreach (var operation in Blocks[id.Value].Operations)
                    AddWrittenLocals(operation.Instruction, ref initialized);
            }

            foreach (var (target, _) in Successors(Blocks[id.Value].Terminator))
            {
                if (target == Entry)
                {
                    continue;
                }

                var previous = atEntry[target.Value];

                if (previous is { } incoming)
                {
                    if (!incoming.IntersectWith(initialized))
                        continue;

                    atEntry[target.Value] = incoming;
                }
                else
                    atEntry[target.Value] = initialized.Clone();

                if (!queued[target.Value])
                {
                    queued[target.Value] = true;
                    pending.Enqueue(target);
                }
            }
        }

        return atEntry;
    }
}
