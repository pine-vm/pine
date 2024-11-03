using Pine.Core;
using System;
using static Pine.PineVM.PineVM;

namespace Pine.Pine.PineVM;

public class InvalidIntermediateCodeException(
    string message,
    Exception? innerException,
    ExecutionErrorReport? errorReport)
    : Exception(message, innerException)
{
    public ExecutionErrorReport? ErrorReport => errorReport;
}

public record ExecutionErrorReport(
    Expression FrameExpression,
    PineValue EnvironmentValue,
    StackFrameInstructions Instructions,
    int FrameInstructionPointer);
