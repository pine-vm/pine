using System.Collections.Concurrent;

namespace Pine.Core.Interpreter.IntermediateVM;

/// <summary>
/// Thread-safe structural template plans shared by short-lived VMs. A plan depends only on its
/// encoded template value; it retains its parsing cache, never VM state or evaluation counters.
/// </summary>
public sealed class PineVMExpressionTemplatePlanCache
{
    internal sealed record Resolution(ExpressionTemplatePlan? Plan);

    internal ConcurrentDictionary<PineValue, Resolution> Entries { get; } = [];

    /// <summary>Number of recognized or rejected template values.</summary>
    public int Count => Entries.Count;
}
