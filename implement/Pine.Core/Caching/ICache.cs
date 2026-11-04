using System;
using System.Diagnostics.CodeAnalysis;
using System.Threading;
using System.Threading.Tasks;

namespace Pine.Core.Caching;

/// <summary>
/// Storage for a pure transformation. Implementations may be local, synchronized, or layered.
/// A miss is not an error; storage failures must be reported, not disguised as hits.
/// Keys and published values must remain immutable.
/// </summary>
public interface ICache<TKey, TValue> where TKey : notnull
{
    /// <summary>Looks up an exact input. Implementations document their concurrency requirements.</summary>
    bool TryGetValue(TKey key, [MaybeNullWhen(false)] out TValue value);

    /// <summary>Publishes a complete result for an exact input.</summary>
    void Set(TKey key, TValue value);
}

/// <summary>Distinct retained entries used by one invocation, including newly published results.</summary>
public interface ICacheUsage
{
    /// <summary>Number of distinct entries accessed in this invocation.</summary>
    int EntryCount { get; }
}

/// <summary>A caller's cleanup decision. Higher ranks are retained before lower ranks.</summary>
public abstract record CacheRetention
{
    /// <summary>Removes the entry regardless of the budget.</summary>
    public sealed record Remove : CacheRetention;

    /// <summary>Retains the entry even when mandatory retention exceeds the budget.</summary>
    public sealed record Retain : CacheRetention;

    /// <summary>Competes for the remaining budget after mandatory retention.</summary>
    public sealed record Ranked(long Rank) : CacheRetention;
}

/// <summary>Optional lifetime control, independent of the cache's data model and storage interface.</summary>
public interface IMarkerCacheLifetime<TMarker>
{
    /// <summary>Current retained entry count.</summary>
    int Count { get; }

    /// <summary>
    /// Sets the marker for subsequent operations in this execution context and its async children.
    /// Returns independent usage accounting for this invocation.
    /// </summary>
    ICacheUsage SetMarker(TMarker marker);

    /// <summary>
    /// Trims asynchronously using each entry's last lookup/publication marker.
    /// Mandatory retention and concurrent accesses can leave more than the requested budget.
    /// </summary>
    Task TrimAsync(int budget, Func<TMarker, CacheRetention> retention, CancellationToken cancellationToken = default);
}
