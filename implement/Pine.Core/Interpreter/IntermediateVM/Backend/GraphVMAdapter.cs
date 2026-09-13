using Pine.Core.CodeAnalysis;
using System;
using System.Collections.Immutable;
using System.Linq;

namespace Pine.Core.Interpreter.IntermediateVM.Backend;

/// <summary>Final numeric emission and fresh legacy payload conversion; never reconstructs a graph.</summary>
public static class GraphVMAdapter
{
    /// <summary>
    /// Uses graph-derived resources with the precomputed frame constructor. Every edge has its own
    /// nonempty stub (at least a jump), so even an empty self-cycle has a negative, nonzero backjump.
    /// Legacy quotas/cancellation remain active; loop counts still depend on layout until increment 9.
    /// </summary>
    public static StackFrameInstructions ToStackFrame(GraphFunction function)
    {
        return Emit();

        StackFrameInstructions Emit()
        {
            var offsets = ImmutableDictionary.CreateBuilder<LayoutLabel, int>();
            var position = 0;
            foreach (var fragment in function.Layout)
            {
                offsets.Add(fragment.Label, position);
                position = checked(position + fragment.Instructions.Count + TransferSize(fragment.Transfer));
            }

            var instructions = ImmutableList.CreateBuilder<StackInstruction>();
            foreach (var fragment in function.Layout)
            {
                instructions.AddRange(fragment.Instructions.Select(StraightLineVMAdapter.ToInstruction));
                switch (fragment.Transfer)
                {
                    case LayoutTransfer.Return ret:
                        instructions.Add(StackInstruction.Local_Get(ret.Local));
                        instructions.Add(new(StackInstructionKind.Return));
                        break;
                    case LayoutTransfer.Jump jump:
                        Jump(jump.Target);
                        break;
                    case LayoutTransfer.Branch branch:
                        instructions.Add(StackInstruction.Local_Get(branch.Local));
                        // The current VM pops this operand on BOTH arms; no extra Pop is needed.
                        instructions.Add(StackInstruction.Jump_If_Equal(
                            checked(offsets[branch.Equal] - instructions.Count),
                            StraightLineVMAdapter.ToPineValue(branch.Literal)));
                        Jump(branch.NotEqual);
                        break;
                    default:
                        throw new NotImplementedException(
                            "ToStackFrame does not handle layout transfer variant: " + fragment.Transfer.GetType().Name);
                }
            }
            return new(StaticFunctionInterface.FromPathsSorted([[]]), instructions.ToArray(),
                function.Resources.LocalsCount, function.Resources.MaxStackUsage);

            void Jump(LayoutLabel target)
            {
                var offset = checked(offsets[target] - instructions.Count);
                if (offset == 0)
                    throw new InvalidOperationException("Graph emission must not produce a zero-offset unconditional jump.");
                instructions.Add(StackInstruction.Jump_Unconditional(offset));
            }
        }
    }

    private static int TransferSize(LayoutTransfer transfer) =>
        transfer switch
        {
            LayoutTransfer.Return => 2,
            LayoutTransfer.Jump => 1,
            LayoutTransfer.Branch => 3,
            _ => throw new NotImplementedException(
                "TransferSize does not handle layout transfer variant: " + transfer.GetType().Name),
        };
}
