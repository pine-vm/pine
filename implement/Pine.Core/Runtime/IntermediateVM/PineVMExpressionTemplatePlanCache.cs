using Pine.Core.PineLanguage;
using System.Collections.Concurrent;

namespace Pine.Core.Runtime.IntermediateVM;

/// <summary>
/// Thread-safe structural template plans shared by short-lived VMs. A plan depends only on its
/// encoded template value; it retains structural expressions and recipes, never mutable VM state
/// or evaluation counters. The integrating application controls the cache lifetime.
/// </summary>
public sealed class PineVMExpressionTemplatePlanCache
{
    internal sealed record Resolution(ExpressionTemplatePlan? Plan);

    internal ConcurrentDictionary<PineValue, Resolution> Entries { get; } = [];

    /// <summary>Number of recognized or rejected template values.</summary>
    public int Count => Entries.Count;
}
