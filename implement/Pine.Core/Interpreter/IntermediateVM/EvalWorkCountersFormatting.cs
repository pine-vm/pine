using Pine.Core.CLI;
using System.Collections.Generic;

namespace Pine.Core.Interpreter.IntermediateVM;

/// <summary>
/// Formats <see cref="EvalWorkCounters"/> (and the counters contained in an
/// <see cref="EvaluationReport"/>) as a human-readable multi-line string suitable
/// for use in test assertions and diagnostic output.
/// </summary>
public static class EvalWorkCountersFormatting
{
    /// <summary>
    /// Formats the <see cref="EvalWorkCounters"/> of the given report as a
    /// multi-line string.
    /// </summary>
    public static string FormatCounts(EvaluationReport report) =>
        FormatCounts(report.Counters);

    /// <summary>
    /// Formats the given <see cref="EvalWorkCounters"/> as a multi-line string,
    /// with one counter per line and integers rendered using
    /// <see cref="CommandLineInterface.FormatIntegerForDisplay(long)"/> for readability.
    /// </summary>
    public static string FormatCounts(EvalWorkCounters counters) =>
        string.Join(
            "\n",
            EnumerateCountLines(counters));

    /// <summary>Alias for <see cref="FormatCounts(EvalWorkCounters)"/>, which includes every counter.</summary>
    public static string FormatAllCounts(EvalWorkCounters counters) => FormatCounts(counters);

    /// <summary>Formats phase totals and their disjoint work-origin breakdowns, including every counter.</summary>
    public static string FormatCountsByPhase(IReadOnlyDictionary<string, EvalWorkCountersByOrigin> phases)
    {
        var sections = new List<string>();

        foreach (var (phase, origins) in phases)
        {
            sections.Add("Phase: " + phase + "\n" + FormatCounts(origins.Total));

            foreach (var (origin, counters) in origins.Enumerate())
            {
                if (counters != default)
                    sections.Add("Phase: " + phase + "; work origin: " + origin + "\n" + FormatCounts(counters));
            }

            if (origins.CachedWorkCounters != default)
            {
                sections.Add(
                    "Phase: " + phase + "; CachedWorkCounters:\n" +
                    CachedWorkCountersFormatting.FormatCounts(origins.CachedWorkCounters));
            }
        }

        return string.Join("\n\n", sections);
    }

    private static IEnumerable<string> EnumerateCountLines(EvalWorkCounters counters)
    {
        yield return "InvocationCount: " + CommandLineInterface.FormatIntegerForDisplay(counters.InvocationCount);
        yield return "BuildListCount: " + CommandLineInterface.FormatIntegerForDisplay(counters.BuildListCount);
        yield return "BuildListItemCount: " + CommandLineInterface.FormatIntegerForDisplay(counters.BuildListItemCount);
        yield return "LoopIterationCount: " + CommandLineInterface.FormatIntegerForDisplay(counters.LoopIterationCount);
        yield return "InstructionCount: " + CommandLineInterface.FormatIntegerForDisplay(counters.InstructionCount);
        yield return "DeferredTemplateValueAllocationCount: " + CommandLineInterface.FormatIntegerForDisplay(counters.DeferredTemplateValueAllocationCount);
        yield return "TemplateDirectInvocationCount: " + CommandLineInterface.FormatIntegerForDisplay(counters.TemplateDirectInvocationCount);
        yield return "DeferredTemplateValueMaterializationCount: " + CommandLineInterface.FormatIntegerForDisplay(counters.DeferredTemplateValueMaterializationCount);
        yield return "TemplateConsolidatedEvalCount: " + CommandLineInterface.FormatIntegerForDisplay(counters.TemplateConsolidatedEvalCount);
        yield return "DirectInterpreterInvocationCount: " + CommandLineInterface.FormatIntegerForDisplay(counters.DirectInterpreterInvocationCount);
        yield return "DirectInterpreterExpressionCount: " + CommandLineInterface.FormatIntegerForDisplay(counters.DirectInterpreterExpressionCount);
        yield return "DirectInterpreterLiteralCount: " + CommandLineInterface.FormatIntegerForDisplay(counters.DirectInterpreterLiteralCount);
        yield return "DirectInterpreterListCount: " + CommandLineInterface.FormatIntegerForDisplay(counters.DirectInterpreterListCount);
        yield return "DirectInterpreterEvalCount: " + CommandLineInterface.FormatIntegerForDisplay(counters.DirectInterpreterEvalCount);
        yield return "DirectInterpreterBuiltinCount: " + CommandLineInterface.FormatIntegerForDisplay(counters.DirectInterpreterBuiltinCount);
        yield return "DirectInterpreterConditionalCount: " + CommandLineInterface.FormatIntegerForDisplay(counters.DirectInterpreterConditionalCount);
        yield return "DirectInterpreterEnvironmentCount: " + CommandLineInterface.FormatIntegerForDisplay(counters.DirectInterpreterEnvironmentCount);
    }
}
