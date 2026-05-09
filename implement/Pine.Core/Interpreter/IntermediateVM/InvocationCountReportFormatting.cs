using System.Collections.Generic;

namespace Pine.Core.Interpreter.IntermediateVM;

/// <summary>
/// Formats <see cref="InvocationCountReport"/> as a human-readable multi-line
/// string suitable for use in test assertions and diagnostic output, mirroring
/// the conventions of <see cref="PerformanceCountersFormatting"/>.
/// </summary>
public static class InvocationCountReportFormatting
{
    /// <summary>
    /// Formats the given <see cref="InvocationCountReport"/> as a multi-line
    /// string, with one statistic per line and integers rendered using
    /// <see cref="CommandLineInterface.FormatIntegerForDisplay(long)"/> for
    /// readability.
    /// </summary>
    public static string FormatCounts(InvocationCountReport report) =>
        string.Join(
            "\n",
            EnumerateCountLines(report));

    private static IEnumerable<string> EnumerateCountLines(InvocationCountReport report)
    {
        yield return "CompiledExpressionCount: " + CommandLineInterface.FormatIntegerForDisplay(report.CompiledExpressionCount);
        yield return "InvocationCountTotal: " + CommandLineInterface.FormatIntegerForDisplay(report.InvocationCountTotal);
        yield return "InvocationCountAverage: " + CommandLineInterface.FormatIntegerForDisplay(report.InvocationCountAverage);
        yield return "InvocationCountPercentile10: " + CommandLineInterface.FormatIntegerForDisplay(report.InvocationCountPercentile10);
        yield return "InvocationCountMedian: " + CommandLineInterface.FormatIntegerForDisplay(report.InvocationCountMedian);
        yield return "InvocationCountPercentile90: " + CommandLineInterface.FormatIntegerForDisplay(report.InvocationCountPercentile90);
    }
}
