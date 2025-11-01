using Pine.Core.CodeAnalysis;
using System;
using System.Collections.Generic;
using System.Linq;

namespace Pine.PineVM;

public record StackFrameInstructions(
    StackFrameParameters Parameters,
    IReadOnlyList<StackInstruction> Instructions,
    PineValueClass? TrackEnvConstraint = null)
{
    public int LocalsCount { init; get; } =
        ComputeLocalsCount(Instructions, Parameters);

    public int MaxStackUsage { init; get; } =
        ComputeMaxStackUsage(Instructions);

    public static int ComputeMaxStackUsage(
        IReadOnlyList<StackInstruction> instructions)
    {
        IEnumerable<int> GetSuccessors(
            int instructionIndex)
        {
            var inst = instructions[instructionIndex];

            switch (inst.Kind)
            {
                case StackInstructionKind.Return:
                    yield break;

                case StackInstructionKind.Jump_Const:

                    yield return
                        instructionIndex +
                        (inst.JumpOffset ?? throw new InvalidOperationException(
                            $"Jump without offset at {instructionIndex}."));

                    break;

                case StackInstructionKind.Jump_If_True_Const:

                    // fall-through
                    yield return instructionIndex + 1;

                    yield return
                        instructionIndex + 1 +
                        (inst.JumpOffset ?? throw new InvalidOperationException(
                            $"Jump without offset at {instructionIndex}."));

                    break;

                default:
                    // ordinary instruction
                    yield return instructionIndex + 1;
                    break;
            }
        }

        var instructionsDetails =
            instructions.Select(StackInstruction.GetDetails).ToArray();

        var stackDepthIn = new int?[instructions.Count];     // stack height *before* executing i

        var worklist = new Stack<int>();

        stackDepthIn[0] = 0;
        worklist.Push(0);

        var maxDepth = 0;

        while (worklist.Count > 0)
        {
            var instructionIndex = worklist.Pop();

            var instructionDetails =
                instructionsDetails[instructionIndex];

            var inDepth =
                stackDepthIn[instructionIndex]!.Value;

            var delta =
                -instructionDetails.PopCount + instructionDetails.PushCount;

            var outDepth = inDepth + delta;

            if (outDepth < 0)
                throw new InvalidOperationException($"Stack under-flow at instruction {instructionIndex}.");

            maxDepth = Math.Max(maxDepth, outDepth);

            foreach (var successorIndex in GetSuccessors(instructionIndex))
            {
                if (successorIndex >= instructions.Count)
                    continue;

                if (stackDepthIn[successorIndex] is null)
                {
                    stackDepthIn[successorIndex] = outDepth;
                    worklist.Push(successorIndex);
                }
                else if (stackDepthIn[successorIndex] != outDepth)
                {
                    // Byte-code would be ill-formed; surface a clear error.
                    throw new InvalidOperationException(
                        $"Inconsistent stack depth at {successorIndex} " +
                        $"({stackDepthIn[successorIndex]} vs {outDepth}).");
                }
            }
        }

        return maxDepth;
    }

    public static int ComputeLocalsCount(
        IReadOnlyList<StackInstruction> instructions,
        StackFrameParameters parameters)
    {
        var aggregateMax = parameters.EnvPaths.Count;

        for (var i = 0; i < instructions.Count; i++)
        {
            var inst = instructions[i];

            if (inst.Kind is StackInstructionKind.Local_Get or StackInstructionKind.Local_Set)
            {
                if (inst.LocalIndex is null)
                {
                    throw new InvalidOperationException(
                        $"Local instruction without index at {i}.");
                }

                var instrMax = inst.LocalIndex.Value + 1;

                aggregateMax =
                    aggregateMax < instrMax
                    ? instrMax
                    : aggregateMax;
            }
        }

        return aggregateMax;
    }

    public virtual bool Equals(StackFrameInstructions? other)
    {
        if (ReferenceEquals(this, other))
            return true;

        if (other is not { } notNull)
            return false;

        if (!other.Parameters.Equals(Parameters))
            return false;

        return
            Instructions.Count == notNull.Instructions.Count &&
            Instructions.SequenceEqual(notNull.Instructions);
    }

    public override int GetHashCode()
    {
        var hashCode = new HashCode();

        hashCode.Add(Parameters);

        foreach (var item in Instructions)
        {
            hashCode.Add(item.GetHashCode());
        }

        return hashCode.ToHashCode();
    }
}

