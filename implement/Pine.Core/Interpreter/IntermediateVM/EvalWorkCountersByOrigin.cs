using System.Collections.Generic;

namespace Pine.Core.Interpreter.IntermediateVM;

/// <summary>
/// Evaluation work by origin, with cached metadata derivations tracked separately.
/// Origin identifies the operation requesting the work, not its call-stack depth.
/// </summary>
public readonly record struct EvalWorkCountersByOrigin(
    EvalWorkCounters VirtualMachine,
    CachedWorkCounters CachedWorkCounters,
    EvalWorkCounters DeferredTemplateValueMaterialization)
{
    /// <summary>Evaluation work only; excludes cache-derivation work.</summary>
    public EvalWorkCounters Total =>
        EvalWorkCounters.Add(
            VirtualMachine,
            DeferredTemplateValueMaterialization);

    /// <summary>Adds all counters independently for each origin.</summary>
    public static EvalWorkCountersByOrigin Add(EvalWorkCountersByOrigin a, EvalWorkCountersByOrigin b) =>
        new(
            EvalWorkCounters.Add(a.VirtualMachine, b.VirtualMachine),
            CachedWorkCounters.Add(a.CachedWorkCounters, b.CachedWorkCounters),
            EvalWorkCounters.Add(a.DeferredTemplateValueMaterialization, b.DeferredTemplateValueMaterialization));

    /// <summary>Computes the work performed between snapshots independently for each origin.</summary>
    public static EvalWorkCountersByOrigin Subtract(EvalWorkCountersByOrigin a, EvalWorkCountersByOrigin b) =>
        new(
            EvalWorkCounters.Subtract(a.VirtualMachine, b.VirtualMachine),
            CachedWorkCounters.Subtract(a.CachedWorkCounters, b.CachedWorkCounters),
            EvalWorkCounters.Subtract(a.DeferredTemplateValueMaterialization, b.DeferredTemplateValueMaterialization));

    /// <summary>Enumerates evaluation origins only, in stable display order.</summary>
    public IEnumerable<(string Origin, EvalWorkCounters Counters)> Enumerate()
    {
        yield return (nameof(VirtualMachine), VirtualMachine);
        yield return (nameof(DeferredTemplateValueMaterialization), DeferredTemplateValueMaterialization);
    }
}
