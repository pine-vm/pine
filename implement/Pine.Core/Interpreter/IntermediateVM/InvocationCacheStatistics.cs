using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Threading;

namespace Pine.Core.Interpreter.IntermediateVM;

/// <summary>
/// Cumulative completed cache operations for one expression, or totals across expressions.
/// Admission means a successful TryAdd at the observed boundary, not a unique shared entry.
/// Expression-index checks are separate from full-key lookups and never count as cache hits.
/// </summary>
public readonly record struct InvocationCacheExpressionStatistics(
    long ExpressionCheckCount,
    long SkippedLookupCount,
    long LookupCount,
    long HitCount,
    long AdmissionAttemptCount,
    long AdmissionCount)
{
    /// <summary>Full-key lookups that did not find an entry.</summary>
    public long MissCount => LookupCount - HitCount;

    /// <summary>TryAdd calls returning false, including duplicates and buffer-capacity refusals.</summary>
    public long RejectedAdmissionCount => AdmissionAttemptCount - AdmissionCount;

    /// <summary>Fraction of full-key lookups finding an entry; zero when there were no lookups.</summary>
    public double HitRate => LookupCount is 0 ? 0 : (double)HitCount / LookupCount;

    internal InvocationCacheExpressionStatistics Add(InvocationCacheExpressionStatistics other) =>
        new(
            ExpressionCheckCount + other.ExpressionCheckCount,
            SkippedLookupCount + other.SkippedLookupCount,
            LookupCount + other.LookupCount,
            HitCount + other.HitCount,
            AdmissionAttemptCount + other.AdmissionAttemptCount,
            AdmissionCount + other.AdmissionCount);
}

/// <summary>
/// Immutable point-in-time operation counters. Totals equal the sum of the per-expression counters.
/// No invocation inputs or return values are retained by the statistics collector.
/// </summary>
public sealed record InvocationCacheStatisticsSnapshot(
    InvocationCacheExpressionStatistics Totals,
    IReadOnlyDictionary<PineValue, InvocationCacheExpressionStatistics> PerExpression)
{
    /// <summary>
    /// Optional shared-storage diagnostics, sampled separately from the operation counters.
    /// Physical slots, promotions and retirements are not VM admissions or lookups.
    /// </summary>
    public InvocationCacheGenerationStatistics? SharedCacheGenerations { get; init; }
}

/// <summary>
/// Thread-safe, opt-in cache instrumentation. Observe the outermost VM-facing cache to count
/// local and shared hits once, without counting internal probes, promotions or buffer publication.
/// Snapshots are cumulative for this collector's lifetime and do not reset counters.
/// </summary>
public sealed class InvocationCacheStatistics
{
    private readonly Lock _lock = new();

    private readonly Dictionary<PineValue, InvocationCacheExpressionStatistics> _perExpression = [];

    /// <summary>
    /// Wraps a cache, preserving its results and sharing this collector's counters.
    /// Batch publication is forwarded without being counted as VM admission.
    /// </summary>
    public IInvocationCacheAccess Observe(IInvocationCacheAccess cache)
    {
        ArgumentNullException.ThrowIfNull(cache);
        return new ObservedCache(cache, this);
    }

    /// <summary>Copies counters consistently, including expressions with only misses or skipped lookups.</summary>
    public InvocationCacheStatisticsSnapshot GetSnapshot()
    {
        lock (_lock)
        {
            var totals = new InvocationCacheExpressionStatistics();

            foreach (var counters in _perExpression.Values)
                totals = totals.Add(counters);

            return new(totals, _perExpression.ToImmutableDictionary());
        }
    }

    private void Record(PineValue expression, InvocationCacheExpressionStatistics counters)
    {
        lock (_lock)
        {
            _perExpression.TryGetValue(expression, out var previous);
            _perExpression[expression] = previous.Add(counters);
        }
    }

    private sealed class ObservedCache(
        IInvocationCacheAccess cache,
        InvocationCacheStatistics statistics) : IInvocationCacheAccess
    {
        public bool ShouldAdmitExpression(PineValue expressionValue) =>
            cache.ShouldAdmitExpression(expressionValue);

        public bool MayContainExpression(PineValue expressionValue)
        {
            var result = cache.MayContainExpression(expressionValue);
            statistics.Record(expressionValue, new(1, result ? 0 : 1, 0, 0, 0, 0));
            return result;
        }

        public bool TryGet(EvalCacheEntryKey key, out PineValue value)
        {
            var result = cache.TryGet(key, out value);
            statistics.Record(key.ExprValue, new(0, 0, 1, result ? 1 : 0, 0, 0));
            return result;
        }

        public bool TryAdd(EvalCacheEntryKey key, PineValue value)
        {
            var result = cache.TryAdd(key, value);
            statistics.Record(key.ExprValue, new(0, 0, 0, 0, 1, result ? 1 : 0));
            return result;
        }

        public void Merge(IReadOnlyDictionary<EvalCacheEntryKey, PineValue> entries) =>
            cache.Merge(entries);
    }
}
