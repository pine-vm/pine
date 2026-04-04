using Pine.Core.Internal;
using System;

namespace Pine.Core.Interpreter.IntermediateVM;


/// <summary>
/// Represents an active stack frame in the Pine intermediate VM.
/// Each frame holds the compiled instructions, input arguments, an evaluation stack for operand passing,
/// local variable storage, and profiling counters.
/// </summary>
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
    /// <summary>
    /// The index of the next instruction to execute within <see cref="StackFrameInstructions.Instructions"/>.
    /// </summary>
    public int InstructionPointer { get; set; } = 0;

    /// <summary>
    /// The index of the next free slot on the evaluation stack, also representing the current stack depth.
    /// </summary>
    public int StackPointer { get; set; } = 0;

    /// <summary>
    /// The number of backward jumps (loop iterations) executed in this frame.
    /// </summary>
    public long LoopIterationCount { get; set; } = 0;

    /// <summary>
    /// The total number of instructions executed in this frame.
    /// </summary>
    public long InstructionCount { get; set; } = 0;

    /// <summary>
    /// Handles the return value from a child stack frame.
    /// If this frame uses a stepwise <see cref="Specialization"/>, the return value is forwarded to
    /// the specialization's continuation callback; otherwise, it is pushed onto the evaluation stack.
    /// </summary>
    public void ReturnFromChildFrame(PineValueInProcess frameReturnValue)
    {
        if (Specialization is not null)
        {
            Specialization.ReturningFromChildFrame(frameReturnValue);
            return;
        }

        PushInstructionResult(frameReturnValue);
    }

    /// <summary>
    /// Pushes a value onto the evaluation stack and advances the instruction pointer to the next instruction.
    /// </summary>
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

    /// <summary>
    /// Stores a value in the local variable slot at the given index.
    /// </summary>
    public void LocalSet(int localIndex, PineValueInProcess value)
    {
        if (value is null)
        {
            throw new InvalidOperationException(
                "LocalSet called with null value");
        }

        LocalsValues.Span[localIndex] = value;
    }

    /// <summary>
    /// Retrieves the value from the local variable slot at the given index.
    /// </summary>
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

    /// <summary>
    /// Pops and returns the topmost value from the evaluation stack,
    /// clearing the slot and decrementing the stack pointer.
    /// </summary>
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

    /// <summary>
    /// Returns the topmost value from the evaluation stack without removing it.
    /// </summary>
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

/// <summary>
/// Records the profiling counter values at the time a stack frame is created,
/// used to compute per-frame deltas for instruction count, invocation count, and other metrics.
/// </summary>
public record struct StackFrameProfilingBaseline(
    long BeginInstructionCount,
    long BeginInvocationCount,
    long BeginParseAndEvalCount,
    long BeginStackFrameCount,
    long BeginBuildListCount);


/*
 * TODO: Expand the stack frame instruction format so that we can model these specializations
 * as precompiled stack frames.
 * That means the stack frame (instruction) model needs to be able to loop (mutate counter in place) and to supply inputs.
 * */

/// <summary>
/// Manages iterative continuation-based execution for precompiled specializations.
/// Each step either requests evaluation of a sub-expression (via <see cref="StepResult.Continue"/>)
/// or signals completion (via <see cref="StepResult.Complete"/>).
/// </summary>
public record ApplyStepwise
{
    /// <summary>
    /// The current step result, indicating whether to continue with another sub-expression evaluation
    /// or that the computation has completed.
    /// </summary>
    public StepResult CurrentStep { private set; get; }

    /// <summary>
    /// Initializes a new <see cref="ApplyStepwise"/> with the given initial continuation step.
    /// </summary>
    public ApplyStepwise(StepResult.Continue start)
    {
        CurrentStep = start;
    }

    /// <summary>
    /// Feeds the return value from a child stack frame into the current continuation callback,
    /// advancing to the next step.
    /// </summary>
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

    /// <summary>
    /// Represents the result of a single step in a stepwise computation.
    /// </summary>
    public abstract record StepResult
    {
        /// <summary>
        /// Indicates that the computation needs to evaluate the given expression in the given environment,
        /// and then pass the result to the callback to produce the next step.
        /// </summary>
        public sealed record Continue(
            Expression Expression,
            PineValueInProcess EnvironmentValue,
            Func<PineValueInProcess, StepResult> Callback)
            : StepResult;

        /// <summary>
        /// Indicates that the stepwise computation has finished and produced the given final value.
        /// </summary>
        public sealed record Complete(PineValueInProcess PineValue)
            : StepResult;
    }
}
