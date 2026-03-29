namespace Pine.Core.Interpreter.IntermediateVM;

/// <summary>
/// Delegate for observing each stack instruction executed by <see cref="PineVM"/>.
/// </summary>
/// <param name="executedStackInstruction">
/// A value-type snapshot describing the current instruction about to execute.
/// </param>
public delegate void ReportExecutedStackInstruction(
    in ExecutedStackInstruction executedStackInstruction);

/// <summary>
/// Information about one stack instruction executed by <see cref="PineVM"/>.
/// </summary>
/// <param name="InstructionIndex">Zero-based index in the overall runtime instruction sequence for the evaluation.</param>
/// <param name="StackFrameDepth">Number of active stack frames when this instruction was about to execute.</param>
/// <param name="InstructionPointer">Instruction pointer within the current stack frame.</param>
/// <param name="EvaluationStackDepth">Depth of the current frame's evaluation stack before executing the instruction.</param>
/// <param name="Instruction">The instruction being executed.</param>
/// <param name="FrameExpression">The expression associated with the current stack frame.</param>
/// <param name="FrameInput">The current stack frame input.</param>
public readonly record struct ExecutedStackInstruction(
    long InstructionIndex,
    int StackFrameDepth,
    int InstructionPointer,
    int EvaluationStackDepth,
    StackInstruction Instruction,
    Expression FrameExpression,
    StackFrameInput FrameInput);

/// <summary>
/// Delegate for observing each time a new stack frame is pushed by <see cref="PineVM"/>.
/// </summary>
/// <param name="enteredStackFrame">
/// A value-type snapshot describing the frame that was just entered.
/// </param>
public delegate void ReportEnteredStackFrame(
    in EnteredStackFrame enteredStackFrame);

/// <summary>
/// Information about a stack frame entered (pushed) by <see cref="PineVM"/>.
/// </summary>
/// <param name="FrameIndex">Zero-based index of this frame push in the overall evaluation (i.e., how many frames have been pushed so far, minus one).</param>
/// <param name="StackFrameDepth">Number of active stack frames after pushing this frame.</param>
/// <param name="Instructions">The compiled instructions for the entered frame.</param>
/// <param name="FrameExpression">The expression associated with the entered frame.</param>
/// <param name="FrameInput">The input arguments for the entered frame.</param>
public readonly record struct EnteredStackFrame(
    long FrameIndex,
    int StackFrameDepth,
    StackFrameInstructions Instructions,
    Expression FrameExpression,
    StackFrameInput FrameInput);
