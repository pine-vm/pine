using System.Collections.Generic;

namespace Pine.Core.Runtime.IntermediateVM;

/// <summary>
/// Work performed to derive cached metadata, rather than execute the evaluated program.
/// Attribution to one evaluation is cache-state dependent: workers sharing a cache can race
/// to perform a derivation. These counts are not deterministic per VM instance or request.
/// Only currently instrumented derivations are represented; this is not a count of all cache operations.
/// </summary>
/// <param name="ExpressionTemplatePlanParseCount">Template recognition attempts, including negative results.</param>
/// <param name="ExpressionTemplatePlanParsing">Direct-interpreter work used to recognize template plans.</param>
public readonly record struct CachedWorkCounters(
    long ExpressionTemplatePlanParseCount = 0,
    EvalWorkCounters ExpressionTemplatePlanParsing = default)
{
    /// <summary>Adds independently captured cache-derivation work.</summary>
    public static CachedWorkCounters Add(CachedWorkCounters a, CachedWorkCounters b) =>
        new(
            a.ExpressionTemplatePlanParseCount + b.ExpressionTemplatePlanParseCount,
            EvalWorkCounters.Add(a.ExpressionTemplatePlanParsing, b.ExpressionTemplatePlanParsing));

    /// <summary>Computes the work performed between two snapshots of the same accounting scope.</summary>
    public static CachedWorkCounters Subtract(CachedWorkCounters a, CachedWorkCounters b) =>
        new(
            a.ExpressionTemplatePlanParseCount - b.ExpressionTemplatePlanParseCount,
            EvalWorkCounters.Subtract(a.ExpressionTemplatePlanParsing, b.ExpressionTemplatePlanParsing));

    /// <summary>Sums snapshots from disjoint evaluations or cache accounting scopes.</summary>
    public static CachedWorkCounters Aggregate(IEnumerable<CachedWorkCounters> counters)
    {
        var total = default(CachedWorkCounters);

        foreach (var counter in counters)
            total = Add(total, counter);

        return total;
    }
}
