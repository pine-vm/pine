using Pine.Core.Internal;
using System;

namespace Pine.Core.Interpreter.IntermediateVM;


public record StackFrame(
    PineValue? ExpressionValue,
    Expression Expression,
    StackFrameInstructions Instructions,
    StackFrameInput InputValues,
    Memory<PineValueInProcess?> StackValues,
    Memory<PineValueInProcess> LocalsValues,
    StackFrameProfilingBaseline ProfilingBaseline,
    ApplyStepwise? Specialization,
    string? CacheFileName)
{
    public int InstructionPointer { get; set; } = 0;

    public int StackPointer { get; set; } = 0;

    public long LoopIterationCount { get; set; } = 0;

    public long InstructionCount { get; set; } = 0;

    public void ReturnFromChildFrame(PineValueInProcess frameReturnValue)
    {
        if (Specialization is not null)
        {
            Specialization.ReturningFromChildFrame(frameReturnValue);
            return;
        }

        PushInstructionResult(frameReturnValue);
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

        var itemValue =
            StackValues.Span[StackPointer]
            ??
            throw new InvalidOperationException("Invalid program code: null reference on pop from stack");

        StackValues.Span[StackPointer] = null;

        return itemValue;
    }

    public PineValueInProcess PeekTopmostFromStack()
    {
        if (StackPointer <= 0)
            throw new InvalidOperationException("PeekTopmostFromStack called with empty stack");

        var itemValue =
            StackValues.Span[StackPointer - 1]
            ??
            throw new InvalidOperationException("Invalid program code: null reference on pop from stack");

        return itemValue;
    }
}

public record struct StackFrameProfilingBaseline(
    long BeginInstructionCount,
    long BeginParseAndEvalCount,
    long BeginStackFrameCount);


/*
 * TODO: Expand the stack frame instruction format so that we can model these specializations
 * as precompiled stack frames.
 * That means the stack frame (instruction) model needs to be able to loop (mutate counter in place) and to supply inputs.
 * */
public record ApplyStepwise
{
    public StepResult CurrentStep { private set; get; }

    public ApplyStepwise(StepResult.Continue start)
    {
        CurrentStep = start;
    }

    public void ReturningFromChildFrame(PineValueInProcess frameReturnValue)
    {
        if (CurrentStep is StepResult.Continue cont)
        {
            CurrentStep = cont.Callback(frameReturnValue);
        }
        else
        {
            throw new Exception("Returning on frame already completed earlier.");
        }
    }

    public abstract record StepResult
    {
        public sealed record Continue(
            Expression Expression,
            PineValueInProcess EnvironmentValue,
            Func<PineValueInProcess, StepResult> Callback)
            : StepResult;

        public sealed record Complete(PineValueInProcess PineValue)
            : StepResult;
    }
}
