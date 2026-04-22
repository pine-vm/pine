using System.Collections.Generic;

namespace Pine.Core.Interpreter.IntermediateVM;

/// <summary>
/// Rich error description returned from
/// <see cref="PineVM.EvaluateExpressionOnCustomStack(Expression, PineValue, PineVM.EvaluationConfig)"/>
/// when evaluation of Pine code fails at runtime.
///
/// In addition to the textual <see cref="Message"/>, this record captures two
/// pieces of context that are essential for debugging evaluation failures:
/// <list type="bullet">
/// <item>
///   <description>
///     <see cref="StackTrace"/>: the sequence of frame expressions that were
///     active when the error was produced, ordered from innermost to
///     outermost. The first element is the frame that produced the error;
///     subsequent elements are the callers up to the root evaluation.
///   </description>
/// </item>
/// <item>
///   <description>
///     <see cref="Counters"/>: the performance counters accumulated up to
///     the point the error was produced, allowing consumers to distinguish
///     between failures hit immediately at start-up and failures hit only
///     after a large amount of work has already happened.
///   </description>
/// </item>
/// </list>
/// </summary>
/// <param name="Message">Human-readable description of the failure.</param>
/// <param name="StackTrace">
/// Frame expressions of the stack at the moment the error was produced.
/// Ordered from innermost (the frame that failed) to outermost
/// (the root frame). May be empty if the error was produced before any
/// frames were pushed.
/// </param>
/// <param name="Counters">
/// Aggregate performance counters accumulated from the start of the
/// evaluation up to the failure.
/// </param>
public record EvaluationError(
    string Message,
    IReadOnlyList<Expression> StackTrace,
    PerformanceCounters Counters)
{
    /// <summary>
    /// Returns a human-readable representation of this error including
    /// the message, the number of stack frames captured, and the
    /// performance counters. Useful when the error is converted to a
    /// string by a consumer that does not yet know about the richer
    /// structure (for example, via string concatenation).
    /// </summary>
    public override string ToString()
    {
        var frameCount = StackTrace?.Count ?? 0;

        return
            Message +
            " - stack frames: " + frameCount +
            " - instructions: " + Counters.InstructionCount +
            " - invocations: " + Counters.InvocationCount +
            " - build lists: " + Counters.BuildListCount +
            " - loop iterations: " + Counters.LoopIterationCount;
    }
}
