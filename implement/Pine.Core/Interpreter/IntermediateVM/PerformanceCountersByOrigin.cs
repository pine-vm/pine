using System.Collections.Generic;

namespace Pine.Core.Interpreter.IntermediateVM;

/// <summary>Disjoint work totals; origin identifies the operation requesting the work, not its call-stack depth.</summary>
public readonly record struct PerformanceCountersByOrigin(
    PerformanceCounters VirtualMachine,
    PerformanceCounters ExpressionTemplatePlanParsing,
    PerformanceCounters DeferredTemplateValueMaterialization)
{
    /// <summary>Sum of all work origins.</summary>
    public PerformanceCounters Total =>
        PerformanceCounters.Add(
            VirtualMachine,
            PerformanceCounters.Add(ExpressionTemplatePlanParsing, DeferredTemplateValueMaterialization));

    /// <summary>Adds all counters independently for each origin.</summary>
    public static PerformanceCountersByOrigin Add(PerformanceCountersByOrigin a, PerformanceCountersByOrigin b) =>
        new(
            PerformanceCounters.Add(a.VirtualMachine, b.VirtualMachine),
            PerformanceCounters.Add(a.ExpressionTemplatePlanParsing, b.ExpressionTemplatePlanParsing),
            PerformanceCounters.Add(a.DeferredTemplateValueMaterialization, b.DeferredTemplateValueMaterialization));

    /// <summary>Computes the work performed between snapshots independently for each origin.</summary>
    public static PerformanceCountersByOrigin Subtract(PerformanceCountersByOrigin a, PerformanceCountersByOrigin b) =>
        new(
            PerformanceCounters.Subtract(a.VirtualMachine, b.VirtualMachine),
            PerformanceCounters.Subtract(a.ExpressionTemplatePlanParsing, b.ExpressionTemplatePlanParsing),
            PerformanceCounters.Subtract(a.DeferredTemplateValueMaterialization, b.DeferredTemplateValueMaterialization));

    /// <summary>Enumerates origins in stable display order.</summary>
    public IEnumerable<(string Origin, PerformanceCounters Counters)> Enumerate()
    {
        yield return (nameof(VirtualMachine), VirtualMachine);
        yield return (nameof(ExpressionTemplatePlanParsing), ExpressionTemplatePlanParsing);
        yield return (nameof(DeferredTemplateValueMaterialization), DeferredTemplateValueMaterialization);
    }
}
