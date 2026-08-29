using System;

namespace Pine.Core.Interpreter.IntermediateVM;

/// <summary>
/// The exception that PineVM throws when compiled intermediate instructions are malformed or inconsistent with the current execution state.
/// </summary>
public class InvalidIntermediateCodeException(
    string message,
    Exception? innerException,
    ExecutionErrorReport? errorReport)
    : Exception(message, innerException)
{
    /// <summary>
    /// Gets the captured execution context that explains where the invalid intermediate code was detected.
    /// </summary>
    public ExecutionErrorReport? ErrorReport => errorReport;
}
