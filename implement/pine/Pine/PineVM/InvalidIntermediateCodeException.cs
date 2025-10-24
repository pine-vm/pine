using System;

namespace Pine.PineVM;

public class InvalidIntermediateCodeException(
    string message,
    Exception? innerException,
    ExecutionErrorReport? errorReport)
    : Exception(message, innerException)
{
    public ExecutionErrorReport? ErrorReport => errorReport;
}
