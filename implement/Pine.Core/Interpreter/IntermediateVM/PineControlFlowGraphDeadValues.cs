using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

namespace Pine.Core.Interpreter.IntermediateVM;

public sealed partial record PineControlFlowGraph
{
    /// <summary>
    /// Cancels discarded stack results against computations that have no observable effect.
    /// A discarded computation's operands become discarded in turn, allowing whole chains
    /// to disappear while retaining any inputs that still need to execute.
    /// </summary>
    public PineControlFlowGraph EliminateDiscardedStackValues(int parameterCount)
    {
        Validate();

        var initializedAtEntry = DefinitelyInitializedLocals(parameterCount);
        var blocks = Blocks.ToBuilder();

        foreach (var block in Blocks)
        {
            var initialized = new HashSet<int>(initializedAtEntry[block.Id.Value] ?? []);
            var operations = ImmutableArray.CreateBuilder<PineControlFlowOperation>();

            foreach (var operation in block.Operations)
            {
                var instruction = operation.Instruction;

                if (instruction.Kind is StackInstructionKind.Pop && instruction.PopCount is > 0)
                {
                    var discarded = operation.Inputs;

                    while (!discarded.IsEmpty &&
                        operations.Count > 0 &&
                        operations[^1] is { Results.Length: 1 } producer &&
                        producer.Results[0] == discarded[^1] &&
                        IsDiscardable(producer.Instruction, initialized))
                    {
                        operations.RemoveAt(operations.Count - 1);
                        discarded = [.. discarded[..^1], .. producer.Inputs];
                    }

                    if (!discarded.IsEmpty)
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
                AddWrittenLocals(instruction, initialized);
            }

            blocks[block.Id.Value] = block with { Operations = operations.ToImmutable() };
        }

        var result = this with { Blocks = blocks.ToImmutable() };
        result.Validate();
        return result;
    }

    private static bool IsDiscardable(StackInstruction instruction, HashSet<int> initialized) =>
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
            instruction.LocalIndices.All(initialized.Contains),

            StackInstructionKind.Local_Get_Skip_Head_Const =>
            instruction.LocalIndices.All(initialized.Contains) &&
            instruction.SkipCount is not null &&
            !instruction.LocalIndices.IsDefaultOrEmpty,

            _ =>
            false
        };

    private HashSet<int>?[] DefinitelyInitializedLocals(int parameterCount)
    {
        var atEntry = new HashSet<int>?[Blocks.Length];
        atEntry[Entry.Value] = [.. Enumerable.Range(0, parameterCount)];

        var pending = new Queue<PineBlockId>();
        var queued = new HashSet<PineBlockId> { Entry };
        pending.Enqueue(Entry);

        while (pending.TryDequeue(out var id))
        {
            queued.Remove(id);
            var initialized = new HashSet<int>(atEntry[id.Value]!);

            foreach (var operation in Blocks[id.Value].Operations)
            {
                AddWrittenLocals(operation.Instruction, initialized);
            }

            foreach (var (target, _) in Successors(Blocks[id.Value].Terminator))
            {
                if (target == Entry)
                {
                    continue;
                }

                var previous = atEntry[target.Value];

                var incoming =
                    previous is null
                    ?
                    new HashSet<int>(initialized)
                    :
                    [.. previous.Intersect(initialized)];

                if (previous is not null && previous.SetEquals(incoming))
                {
                    continue;
                }

                atEntry[target.Value] = incoming;

                if (queued.Add(target))
                {
                    pending.Enqueue(target);
                }
            }
        }

        return atEntry;
    }
}
