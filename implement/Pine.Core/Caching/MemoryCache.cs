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
public sealed class MemoryCache<TKey, TValue, TMarker>(TMarker initialMarker, IEqualityComparer<TKey>? comparer = null)
    : ICache<TKey, TValue>, IMarkerCacheLifetime<TMarker>
    where TKey : notnull
{
    private sealed class Usage(TMarker marker, IEqualityComparer<TKey>? comparer) : ICacheUsage
    {
        public TMarker Marker { get; } = marker;
        public HashSet<TKey> Keys { get; } = new(comparer);
        public int EntryCount => Volatile.Read(ref _count);
        private int _count;

        public void Touch(TKey key)
        {
            if (Keys.Add(key))
                Volatile.Write(ref _count, Keys.Count);
        }
    }

    private sealed record Entry(TValue Value, TMarker Marker, long Access);

    private readonly Lock _gate = new();
    private readonly Dictionary<TKey, Entry> _entries = new(comparer);
    private readonly AsyncLocal<Usage?> _usage = new();
    private readonly SemaphoreSlim _trimGate = new(1, 1);
    private long _access;

    /// <inheritdoc/>
    public int Count
    {
        get { lock (_gate) return _entries.Count; }
    }

    /// <inheritdoc/>
    public ICacheUsage SetMarker(TMarker marker) => _usage.Value = new Usage(marker, comparer);

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
            usage?.Touch(key);
            _entries[key] = entry with { Marker = usage is null ? initialMarker : usage.Marker, Access = ++_access };
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
            usage?.Touch(key);
            _entries[key] = new(value, usage is null ? initialMarker : usage.Marker, ++_access);
        }
    }

    /// <inheritdoc/>
    public Task TrimAsync(int budget, Func<TMarker, CacheRetention> retention, CancellationToken cancellationToken = default)
    {
        ArgumentOutOfRangeException.ThrowIfNegative(budget);
        ArgumentNullException.ThrowIfNull(retention);

        return Task.Run(async () =>
        {
            await _trimGate.WaitAsync(cancellationToken).ConfigureAwait(false);

            try
            {
                KeyValuePair<TKey, Entry>[] snapshot;
                lock (_gate)
                    snapshot = [.. _entries];

                var remove = new List<KeyValuePair<TKey, Entry>>();
                var ranked = new List<(KeyValuePair<TKey, Entry> Entry, long Rank)>();
                var retained = 0;

                foreach (var entry in snapshot)
                {
                    cancellationToken.ThrowIfCancellationRequested();
                    var decision = retention(entry.Value.Marker);

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
                                nameof(TrimAsync) + " does not handle retention variant: " + decision.GetType().Name);
                    }
                }

                remove.AddRange(ranked.OrderByDescending(item => item.Rank)
                    .ThenByDescending(item => item.Entry.Value.Access)
                    .Skip(Math.Max(0, budget - retained)).Select(item => item.Entry));

                foreach (var entry in remove)
                {
                    cancellationToken.ThrowIfCancellationRequested();
                    lock (_gate)
                    {
                        if (_entries.TryGetValue(entry.Key, out var current) && ReferenceEquals(current, entry.Value))
                            _entries.Remove(entry.Key);
                    }
                }
            }
            finally
            {
                _trimGate.Release();
            }
        }, cancellationToken);
    }
}
