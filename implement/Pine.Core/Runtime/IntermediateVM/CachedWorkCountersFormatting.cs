using Pine.Core.CLI;

namespace Pine.Core.Runtime.IntermediateVM;

/// <summary>Formats cache-state-dependent derivation work separately from evaluation work.</summary>
public static class CachedWorkCountersFormatting
{
    /// <summary>Includes every currently modeled cached-work counter.</summary>
    public static string FormatCounts(CachedWorkCounters counters) =>
        "ExpressionTemplatePlanParseCount: " +
        CommandLineInterface.FormatIntegerForDisplay(counters.ExpressionTemplatePlanParseCount) +
        "\nExpressionTemplatePlanParsing:\n" +
        EvalWorkCountersFormatting.FormatCounts(counters.ExpressionTemplatePlanParsing);
}
