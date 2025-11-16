using System;

namespace Pine.Core.Interpreter.IntermediateVM;

public class InvalidIntermediateCodeException(
    string message,
    Exception? innerException,
    ExecutionErrorReport? errorReport)
    : Exception(message, innerException)
{
    public ExecutionErrorReport? ErrorReport => errorReport;
}
