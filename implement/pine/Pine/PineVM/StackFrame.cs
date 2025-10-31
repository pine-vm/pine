using Pine.Core;
using Pine.Core.Internal;
using System;

using static Pine.PineVM.PineVM;

namespace Pine.PineVM;


record StackFrame(
    PineValue? ExpressionValue,
    Expression Expression,
    StackFrameInstructions Instructions,
    PineValueInProcess EnvironmentValue,
    Memory<PineValueInProcess> StackValues,
    Memory<PineValueInProcess> LocalsValues,
    StackFrameProfilingBaseline ProfilingBaseline,
    ApplyStepwise? Specialization)
{
    public int InstructionPointer { get; set; } = 0;

    public int StackPointer { get; set; } = 0;

    public long LoopIterationCount { get; set; } = 0;

    public long InstructionCount { get; set; } = 0;

    public void ReturnFromChildFrame(PineValue frameReturnValue)
    {
        if (Specialization is not null)
        {
            Specialization.ReturningFromChildFrame(frameReturnValue);
            return;
        }

        PushInstructionResult(PineValueInProcess.Create(frameReturnValue));
    }

    public void PushInstructionResult(PineValueInProcess value)
    {
        if (value is null)
        {
            throw new InvalidOperationException(
                "PushInstructionResult called with null value");
        }

        StackValues.Span[StackPointer] = value;
        StackPointer++;
        ++InstructionPointer;
    }

    public void LocalSet(int localIndex, PineValueInProcess value)
    {
        if (value is null)
        {
            throw new InvalidOperationException(
                "LocalSet called with null value");
        }

        LocalsValues.Span[localIndex] = value;
    }

    public PineValueInProcess LocalGet(int localIndex)
    {
        var value = LocalsValues.Span[localIndex];

        if (value is null)
        {
            throw new InvalidOperationException(
                "LocalGet called with null value");
        }

        return value;
    }

    public PineValueInProcess PopTopmostFromStack()
    {
        if (StackPointer <= 0)
            throw new InvalidOperationException("ConsumeSingleFromStack called with empty stack");

        --StackPointer;
        return StackValues.Span[StackPointer];
    }

    public PineValueInProcess PeekTopmostFromStack()
    {
        if (StackPointer <= 0)
            throw new InvalidOperationException("PeekTopmostFromStack called with empty stack");

        return StackValues.Span[StackPointer - 1];
    }
}

record struct StackFrameProfilingBaseline(
    long BeginInstructionCount,
    long BeginParseAndEvalCount,
    long BeginStackFrameCount);
