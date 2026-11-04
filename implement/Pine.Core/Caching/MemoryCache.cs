using System;
using System.Collections.Generic;
using System.Diagnostics.CodeAnalysis;
using System.Linq;
using System.Threading;
using System.Threading.Tasks;

namespace Pine.Core.Caching;

/// <summary>
/// Thread-safe in-memory transformation storage with async-context invocation markers.
/// Cleanup invokes application policy outside the storage lock and never removes an entry
/// touched or replaced after its cleanup snapshot.
/// </summary>
public sealed class MemoryCache<TKey, TValue, TMarker>(
    TMarker initialMarker,
    IEqualityComparer<TKey>? comparer = null,
    Action<TKey, TValue>? onRemoved = null)
    : ICache<TKey, TValue>, IMarkerCacheLifetime<TMarker>
    where TKey : notnull
{
    private sealed class Usage(TMarker marker) : ICacheUsage
    {
        public TMarker Marker { get; } = marker;

        private readonly HashSet<long> _identities = [];

        public int EntryCount => Volatile.Read(ref _count);

        private int _count;

        public void Touch(long identity)
        {
            if (_identities.Add(identity))
                Volatile.Write(ref _count, _identities.Count);
        }
    }

    private sealed class Entry(TValue value, TMarker marker, long access, long identity)
    {
        public TValue Value { get; } = value;

        public long Identity { get; } = identity;

        public TMarker Marker = marker;

        public long Access = access;
    }

    private readonly record struct Snapshot(TKey Key, Entry Entry, TMarker Marker, long Access);

    private readonly Lock _gate = new();

    private readonly Dictionary<TKey, Entry> _entries = new(comparer);

    private readonly AsyncLocal<Usage?> _usage = new();

    private readonly SemaphoreSlim _trimGate = new(1, 1);

    private long _access;

    private long _identity;

    /// <inheritdoc/>
    public int Count
    {
        get { lock (_gate) return _entries.Count; }
    }

    /// <inheritdoc/>
    public ICacheUsage SetMarker(TMarker marker) => _usage.Value = new Usage(marker);

    /// <inheritdoc/>
    public bool TryGetValue(TKey key, [MaybeNullWhen(false)] out TValue value)
    {
        lock (_gate)
        {
            if (!_entries.TryGetValue(key, out var entry))
            {
                value = default;
                return false;
            }

            var usage = _usage.Value;
            usage?.Touch(entry.Identity);
            entry.Marker = usage is null ? initialMarker : usage.Marker;
            entry.Access = ++_access;
            value = entry.Value;
            return true;
        }
    }

    /// <inheritdoc/>
    public void Set(TKey key, TValue value)
    {
        lock (_gate)
        {
            var usage = _usage.Value;
            var identity = _entries.TryGetValue(key, out var previous) ? previous.Identity : ++_identity;
            usage?.Touch(identity);
            _entries[key] = new(value, usage is null ? initialMarker : usage.Marker, ++_access, identity);
        }
    }

    /// <summary>
    /// Releases all entries, notifying the optional secondary-index observer outside the storage lock.
    /// The observer is also called for entries removed by trimming, but not for replaced values.
    /// </summary>
    public void Clear()
    {
        KeyValuePair<TKey, Entry>[] removed;
        lock (_gate)
        {
            removed = onRemoved is null ? [] : [.. _entries];
            _entries.Clear();
        }

        foreach (var entry in removed)
            onRemoved!(entry.Key, entry.Value.Value);
    }

    /// <inheritdoc/>
    public Task TrimAsync(
        int budget,
        Func<TMarker, CacheRetention> retention,
        CancellationToken cancellationToken = default)
    {
        ArgumentOutOfRangeException.ThrowIfNegative(budget);
        ArgumentNullException.ThrowIfNull(retention);

        return
            Task.Run(
                async () =>
                {
                    await _trimGate.WaitAsync(cancellationToken).ConfigureAwait(false);

                    try
                    {
                        Snapshot[] snapshot;

                        lock (_gate)
                        {
                            snapshot =
                                [
                                    .. _entries.Select(entry =>
                                        new Snapshot(entry.Key, entry.Value, entry.Value.Marker, entry.Value.Access))
                                ];
                        }

                        var remove = new List<Snapshot>();
                        var ranked = new List<(Snapshot Entry, long Rank)>();
                        var retained = 0;

                        foreach (var entry in snapshot)
                        {
                            cancellationToken.ThrowIfCancellationRequested();
                            var decision = retention(entry.Marker);

                            switch (decision)
                            {
                                case CacheRetention.Remove:
                                    remove.Add(entry);
                                    break;

                                case CacheRetention.Retain:
                                    retained++;
                                    break;

                                case CacheRetention.Ranked rank:
                                    ranked.Add((entry, rank.Rank));
                                    break;

                                default:
                                    throw new NotImplementedException(
                                        nameof(TrimAsync) + " does not handle retention variant: " +
                                        decision.GetType().Name);
                            }
                        }

                        remove.AddRange(
                            ranked.OrderByDescending(item => item.Rank)
                            .ThenByDescending(item => item.Entry.Access)
                            .Skip(Math.Max(0, budget - retained)).Select(item => item.Entry));

                        foreach (var entry in remove)
                        {
                            cancellationToken.ThrowIfCancellationRequested();
                            var removed = false;

                            lock (_gate)
                            {
                                if (_entries.TryGetValue(entry.Key, out var current) &&
                                    ReferenceEquals(current, entry.Entry) && current.Access == entry.Access)
                                {
                                    removed = _entries.Remove(entry.Key);
                                }
                            }

                            if (removed)
                                onRemoved?.Invoke(entry.Key, entry.Entry.Value);
                        }
                    }
                    finally
                    {
                        _trimGate.Release();
                    }
                },
                cancellationToken);
    }
}
