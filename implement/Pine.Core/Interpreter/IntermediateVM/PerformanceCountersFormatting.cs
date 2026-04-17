using System.Collections.Generic;

namespace Pine.Core.Interpreter.IntermediateVM;

/// <summary>
/// Formats <see cref="PerformanceCounters"/> (and the counters contained in an
/// <see cref="EvaluationReport"/>) as a human-readable multi-line string suitable
/// for use in test assertions and diagnostic output.
/// </summary>
public static class PerformanceCountersFormatting
{
    /// <summary>
    /// Formats the <see cref="PerformanceCounters"/> of the given report as a
    /// multi-line string.
    /// </summary>
    public static string FormatCounts(EvaluationReport report) =>
        FormatCounts(report.Counters);

    /// <summary>
    /// Formats the given <see cref="PerformanceCounters"/> as a multi-line string,
    /// with one counter per line and integers rendered using
    /// <see cref="CommandLineInterface.FormatIntegerForDisplay(long)"/> for readability.
    /// </summary>
    public static string FormatCounts(PerformanceCounters counters) =>
        string.Join(
            "\n",
            EnumerateCountLines(counters));

    private static IEnumerable<string> EnumerateCountLines(PerformanceCounters counters)
    {
        yield return "InstructionCount: " + CommandLineInterface.FormatIntegerForDisplay(counters.InstructionCount);
        yield return "InvocationCount: " + CommandLineInterface.FormatIntegerForDisplay(counters.InvocationCount);
        yield return "BuildListCount: " + CommandLineInterface.FormatIntegerForDisplay(counters.BuildListCount);
        yield return "LoopIterationCount: " + CommandLineInterface.FormatIntegerForDisplay(counters.LoopIterationCount);
    }
}
