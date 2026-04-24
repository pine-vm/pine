namespace Pine.Core.Interpreter.IntermediateVM;

/// <summary>
/// Snapshot of the full execution state of a single stack frame at a moment in time.
/// Used by <see cref="PineVM.EvaluateExpressionOnCustomStack(Expression, PineValue, PineVM.EvaluationConfig)"/>
/// to detect in-frame infinite loops produced by backward jump instructions: if two
/// successive snapshots of the same frame are equal (and the frame's loop iteration
/// counter has advanced in between), the frame is provably in an infinite cycle.
/// </summary>
/// <param name="Frame">
/// The stack frame whose state was captured. Compared by reference identity so the
/// snapshot is only considered equal to another snapshot of the very same frame
/// instance.
/// </param>
/// <param name="InstructionPointer">
/// Value of <see cref="StackFrame.InstructionPointer"/> at the moment of capture.
/// </param>
/// <param name="StackPointer">
/// Value of <see cref="StackFrame.StackPointer"/> at the moment of capture.
/// </param>
/// <param name="InstructionCount">
/// Total number of VM instructions executed by the enclosing
/// <see cref="PineVM.EvaluateExpressionOnCustomStack(Expression, PineValue, PineVM.EvaluationConfig)"/>
/// evaluation up to the moment of capture. Used to compute the upper bound on the
/// cycle length when an in-frame loop is detected.
/// </param>
/// <param name="LoopIterationCount">
/// Value of <see cref="StackFrame.LoopIterationCount"/> at the moment of capture.
/// Used to verify that the frame actually performed at least one backward jump
/// between two equal snapshots before reporting an infinite loop.
/// </param>
/// <param name="StackValues">
/// Evaluated values currently held on the frame's evaluation stack
/// (only the live slots, i.e. indices 0..StackPointer-1).
/// </param>
/// <param name="LocalsValues">
/// Evaluated values currently held in the frame's local-variable slots.
/// </param>
internal sealed record InFrameSnapshot(
    StackFrame Frame,
    int InstructionPointer,
    int StackPointer,
    long InstructionCount,
    long LoopIterationCount,
    PineValue[] StackValues,
    PineValue[] LocalsValues);
