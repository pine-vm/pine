using Pine.Core.PineLanguage;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading;
using System.Threading.Tasks;

namespace Pine.Core.Runtime.IntermediateVM;

/// <summary>
/// Count-based background maintenance policy for an invocation cache.
/// Entry counts trigger maintenance; they never limit admission or promotion.
/// </summary>
public sealed record InvocationCacheGenerationOptions
{
    /// <summary>Default experimental policy for long-lived language-service sessions.</summary>
    public static InvocationCacheGenerationOptions Default { get; } = new();

    /// <summary>Maximum number of dictionaries retained, including the newest generation.</summary>
    public int MaximumGenerationCount { get; }

    /// <summary>Newest-generation occupancy at which a completed execution requests rotation.</summary>
    public int RotationEntryCount { get; }

    /// <summary>Creates a validated count-based policy; both values must be positive.</summary>
    public InvocationCacheGenerationOptions(
        int maximumGenerationCount = 3,
        int rotationEntryCount = 5_000)
    {
        ArgumentOutOfRangeException.ThrowIfLessThan(maximumGenerationCount, 1);
        ArgumentOutOfRangeException.ThrowIfLessThan(rotationEntryCount, 1);

        MaximumGenerationCount = maximumGenerationCount;
        RotationEntryCount = rotationEntryCount;
    }
}

/// <summary>
/// A thread-safe invocation cache with unbounded admission. Hits promote into the newest dictionary;
/// retiring a generation also retires its expression index. All dictionary access and rotation
/// use the same lock. Argument materialization happens outside that lock.
/// </summary>
public sealed class GenerationalInvocationCache : IInvocationCacheAccess
{
    private sealed class Generation
    {
        internal readonly Dictionary<EvalCacheEntryKey, PineValue> Entries = [];

        internal readonly HashSet<PineValue> Expressions = [];
    }

    private readonly Lock _lock = new();

    private readonly List<Generation> _generations = [new()];

    private int _newestGenerationEntryCount;

    private long _physicalEntryCount;

    private TaskCompletionSource? _pendingMaintenance;

    private long _hits;

    private long _misses;

    private long _promotions;

    private long _rotations;

    private long _retiredEntries;

    /// <summary>The immutable policy used for automatic request-boundary rotation.</summary>
    public InvocationCacheGenerationOptions Options { get; }

    /// <summary>Latest published newest-generation count, read without acquiring the cache lock.</summary>
    public int NewestGenerationEntryCount => Volatile.Read(ref _newestGenerationEntryCount);

    /// <summary>
    /// Latest published physical entry count, including promoted copies. Concurrent count reads
    /// are approximate; use <see cref="GetStatistics"/> for a consistent diagnostic snapshot.
    /// </summary>
    public long PhysicalEntryCount => Volatile.Read(ref _physicalEntryCount);

    /// <summary>Creates an empty cache with one generation.</summary>
    public GenerationalInvocationCache(InvocationCacheGenerationOptions? options = null)
    {
        Options = options ?? InvocationCacheGenerationOptions.Default;
    }

    /// <inheritdoc/>
    public bool MayContainExpression(PineValue expressionValue)
    {
        lock (_lock)
        {
            foreach (var generation in _generations)
            {
                if (generation.Expressions.Contains(expressionValue))
                    return true;
            }

            return false;
        }
    }

    /// <inheritdoc/>
    public bool TryGet(EvalCacheEntryKey key, out PineValue value)
    {
        key.StackFrameInput.Materialize();

        lock (_lock)
        {
            foreach (var generation in _generations)
            {
                if (!generation.Entries.TryGetValue(key, out value!))
                    continue;

                ++_hits;
                Promote(key, value, generation);
                return true;
            }

            ++_misses;
            value = null!;
            return false;
        }
    }

    /// <inheritdoc/>
    public bool TryAdd(EvalCacheEntryKey key, PineValue value)
    {
        key.StackFrameInput.Materialize();

        lock (_lock)
        {
            return TryAddLocked(key, value);
        }
    }

    /// <inheritdoc/>
    public void Merge(IReadOnlyDictionary<EvalCacheEntryKey, PineValue> entries)
    {
        foreach (var entry in entries)
            entry.Key.StackFrameInput.Materialize();

        lock (_lock)
        {
            foreach (var entry in entries)
                TryAddLocked(entry.Key, entry.Value);
        }
    }

    /// <summary>
    /// Queues count-triggered maintenance on the thread pool, coalescing concurrent requests.
    /// The background worker rechecks occupancy so a stale request cannot rotate an empty generation.
    /// The returned task observes completion/failure; callers need not await it on the request path.
    /// </summary>
    public Task MaintainAsync()
    {
        var pending = Volatile.Read(ref _pendingMaintenance);

        if (pending is not null)
            return pending.Task;

        var completion = new TaskCompletionSource(TaskCreationOptions.RunContinuationsAsynchronously);
        pending = Interlocked.CompareExchange(ref _pendingMaintenance, completion, null);

        if (pending is not null)
            return pending.Task;

        _ =
            Task.Run(
                () =>
                {
                    try
                    {
                        lock (_lock)
                        {
                            try
                            {
                                if (_generations[0].Entries.Count >= Options.RotationEntryCount)
                                    AdvanceGenerationLocked(Options.MaximumGenerationCount);
                            }
                            finally
                            {
                                // Release the single-flight slot before writers can publish another batch.
                                Interlocked.CompareExchange(ref _pendingMaintenance, null, completion);
                            }
                        }

                        completion.SetResult();
                    }
                    catch (Exception exception)
                    {
                        completion.SetException(exception);
                    }
                    finally
                    {
                        Interlocked.CompareExchange(ref _pendingMaintenance, null, completion);
                    }
                });

        return completion.Task;
    }

    /// <summary>
    /// Explicitly queues generation rotation/truncation on the thread pool, regardless of occupancy.
    /// The positive limit cannot exceed the configured maximum. No cleanup executes on the caller.
    /// </summary>
    public Task AdvanceGenerationAsync(int maxGenerationCount)
    {
        ArgumentOutOfRangeException.ThrowIfLessThan(maxGenerationCount, 1);
        ArgumentOutOfRangeException.ThrowIfGreaterThan(maxGenerationCount, Options.MaximumGenerationCount);

        return
            Task.Run(
                () =>
                {
                    lock (_lock)
                    {
                        AdvanceGenerationLocked(maxGenerationCount);
                    }
                });
    }

    /// <summary>
    /// Computes a consistent snapshot, including an exact unique count using the key comparer.
    /// This is O(physical entries), allocates a temporary key set, and is not intended for hot paths.
    /// </summary>
    public InvocationCacheGenerationStatistics GetStatistics()
    {
        lock (_lock)
        {
            var counts = _generations.Select(generation => generation.Entries.Count).ToArray();
            var keys = new HashSet<EvalCacheEntryKey>();

            foreach (var generation in _generations)
                keys.UnionWith(generation.Entries.Keys);

            return
                new InvocationCacheGenerationStatistics(
                    counts,
                    counts.Sum(count => (long)count),
                    keys.Count,
                    _hits,
                    _misses,
                    _promotions,
                    _rotations,
                    _retiredEntries);
        }
    }

    private void Promote(EvalCacheEntryKey key, PineValue value, Generation source)
    {
        var newest = _generations[0];

        if (ReferenceEquals(source, newest))
            return;

        newest.Entries.Add(key, value);
        newest.Expressions.Add(key.ExprValue);
        PublishCountsAfterInsertion();
        ++_promotions;
    }

    private bool TryAddLocked(EvalCacheEntryKey key, PineValue value)
    {
        foreach (var generation in _generations)
        {
            if (!generation.Entries.TryGetValue(key, out var existing))
                continue;

            Promote(key, existing, generation);
            return false;
        }

        var newest = _generations[0];
        newest.Entries.Add(key, value);
        newest.Expressions.Add(key.ExprValue);
        PublishCountsAfterInsertion();
        return true;
    }

    private void PublishCountsAfterInsertion()
    {
        Volatile.Write(ref _newestGenerationEntryCount, _generations[0].Entries.Count);
        Volatile.Write(ref _physicalEntryCount, _physicalEntryCount + 1);
    }

    private void AdvanceGenerationLocked(int maxGenerationCount)
    {
        _generations.Insert(0, new Generation());

        while (_generations.Count > maxGenerationCount)
        {
            var retiredCount = _generations[^1].Entries.Count;
            _retiredEntries += retiredCount;
            Volatile.Write(ref _physicalEntryCount, _physicalEntryCount - retiredCount);
            _generations.RemoveAt(_generations.Count - 1);
        }

        Volatile.Write(ref _newestGenerationEntryCount, 0);
        ++_rotations;
    }
}

/// <summary>
/// Consistent diagnostic snapshot. Counts are newest-first; physical entries include promoted
/// duplicates, and retired entries count discarded slots rather than reclaimed bytes.
/// </summary>
/// <param name="EntriesPerGeneration">Entry counts from newest to oldest.</param>
/// <param name="PhysicalEntryCount">Total slots occupied across all generations.</param>
/// <param name="UniqueEntryCount">Distinct invocation keys across all generations.</param>
/// <param name="Hits">Successful lookups.</param>
/// <param name="Misses">Unsuccessful lookups.</param>
/// <param name="Promotions">Copies from an older generation to the newest.</param>
/// <param name="Rotations">Explicit or automatic generation advances.</param>
/// <param name="RetiredEntries">Physical entries in retired dictionaries, including duplicates.</param>
public sealed record InvocationCacheGenerationStatistics(
    IReadOnlyList<int> EntriesPerGeneration,
    long PhysicalEntryCount,
    int UniqueEntryCount,
    long Hits,
    long Misses,
    long Promotions,
    long Rotations,
    long RetiredEntries);
