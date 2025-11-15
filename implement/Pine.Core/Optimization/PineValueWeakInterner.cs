using System;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.Threading;
using System.Threading.Tasks;

namespace Pine.Core.Optimization;

internal static class PineValueWeakInterner
{
    private readonly record struct ListBucketKey(
        int SlimHashCode,
        int NodesCountTruncated)
        : IEquatable<ListBucketKey>
    {
    }

    private readonly record struct BlobBucketKey(
        int SlimHashCode,
        int Length)
        : IEquatable<BlobBucketKey>
    {
    }

    private static readonly ConcurrentDictionary<ListBucketKey, Bucket<PineValue.ListValue>> s_listBuckets = new();
    private static readonly ConcurrentDictionary<BlobBucketKey, Bucket<PineValue.BlobValue>> s_blobBuckets = new();

    // Probabilistic sweeper configuration
    private const int RandomSweepDenominator = 100_000;      // ~0.01% chance per new bucket
    private const int RandomSweepSampleSize = 100_000;       // buckets to sample per sweep

    public static PineValue.ListValue GetOrAdd(PineValue.ListValue candidate)
    {
        var key =
            new ListBucketKey(
                SlimHashCode: candidate.GetHashCode(),
                NodesCountTruncated: (int)candidate.NodesCount);

        var bucket =
            s_listBuckets.GetOrAdd(
                key,
                valueFactory: _ => new Bucket<PineValue.ListValue>());

        if (bucket.TryFind(candidate, out var canonical))
            return canonical;

        var winner = bucket.Add(candidate);

        MaybeStartSweep(s_listBuckets);

        return winner;
    }

    public static PineValue.BlobValue GetOrAdd(PineValue.BlobValue candidate)
    {
        var key =
            new BlobBucketKey(
                SlimHashCode: candidate.GetHashCode(),
                Length: candidate.Bytes.Length);

        var bucket =
            s_blobBuckets.GetOrAdd(
                key,
                valueFactory: _ => new Bucket<PineValue.BlobValue>());

        if (bucket.TryFind(candidate, out var canonical))
            return canonical;

        var winner = bucket.Add(candidate);

        MaybeStartSweep(s_blobBuckets);

        return winner;
    }

    public static PineValue.ListValue? TryGetCanonical(PineValue.ListValue candidate)
    {
        var key =
            new ListBucketKey(
                SlimHashCode: candidate.GetHashCode(),
                NodesCountTruncated: (int)candidate.NodesCount);

        if (!s_listBuckets.TryGetValue(key, out var bucket))
        {
            return null;
        }

        return bucket.TryFind(candidate, out var canonical)
            ? canonical
            : null;
    }

    public static void Register(PineValue.ListValue canonical) => _ = GetOrAdd(canonical);

    public static PineValue.BlobValue? TryGetCanonical(PineValue.BlobValue candidate)
    {
        var key = new BlobBucketKey(candidate.GetHashCode(), candidate.Bytes.Length);

        if (!s_blobBuckets.TryGetValue(key, out var bucket))
        {
            return null;
        }

        return bucket.TryFind(candidate, out var canonical)
            ? canonical
            : null;
    }

    public static void Register(PineValue.BlobValue canonical) => _ = GetOrAdd(canonical);

    // --------------- Probabilistic sweeping ---------------

    private static void MaybeStartSweep<TKey, TVal>(
        ConcurrentDictionary<TKey, Bucket<TVal>> buckets)
        where TKey : notnull
        where TVal : class
    {
        if ((Environment.TickCount64 % 41) is 0)
        {
            if (Random.Shared.Next(RandomSweepDenominator / 41) is 0)
            {
                // Occasionally sweep a larger slice
                Task.Run(() =>
                {
                    SweepRandomSlice(buckets, RandomSweepSampleSize);
                });
            }
        }
    }

    // --------------- Helpers ---------------

    private static void SweepRandomSlice<TKey, TVal>(
        ConcurrentDictionary<TKey, Bucket<TVal>> buckets,
        int maxTake)
        where TKey : notnull
        where TVal : class
    {
        var count = buckets.Count;

        if (count is 0 || maxTake <= 0)
            return;

        void SweepOne(TKey key)
        {
            if (!buckets.TryGetValue(key, out var bucket))
                return;

            var nowEmpty = bucket.SweepAndIsEmpty();

            if (!nowEmpty)
                return;

            if (buckets.TryGetValue(key, out var current) &&
                ReferenceEquals(current, bucket))
            {
                buckets.TryRemove(key, out _);
            }
        }

        var offset =
            (int)(Environment.TickCount64 % count);

        var take = Math.Min(maxTake, count);

        var taken = 0;

        // First pass: skip "offset" items, then take
        var skipped = 0;
        foreach (var kvp in buckets)
        {
            if (skipped < offset)
            {
                skipped++;
                continue;
            }

            SweepOne(kvp.Key);
            taken++;

            if (taken >= take)
                break;
        }

        if (taken >= take)
            return;

        // Wrap-around: take remaining from the start
        foreach (var kvp in buckets)
        {
            SweepOne(kvp.Key);
            taken++;

            if (taken >= take)
                break;
        }
    }

    private sealed class Bucket<TVal>
        where TVal : class
    {
        private WeakReference<TVal>[] _entries = [];
        private readonly Lock _gate = new();

        public bool TryFind(TVal candidate, out TVal canonical)
        {
            var snapshot = Volatile.Read(ref _entries);

            for (var read = 0; read < snapshot.Length; read++)
            {
                var weak = snapshot[read];

                if (!weak.TryGetTarget(out var target))
                    continue;

                if (ReferenceEquals(target, candidate) || target.Equals(candidate))
                {
                    canonical = target;
                    return true;
                }
            }

            canonical = candidate;
            return false;
        }

        public TVal Add(TVal candidate)
        {
            lock (_gate)
            {
                if (TryFindUnlocked(candidate, out var canonical))
                {
                    return canonical;
                }

                var snapshot = _entries;
                var newEntries = new WeakReference<TVal>[snapshot.Length + 1];

                Array.Copy(snapshot, newEntries, snapshot.Length);

                newEntries[^1] = new WeakReference<TVal>(candidate);

                Volatile.Write(ref _entries, newEntries);

                return candidate;
            }
        }

        public bool SweepAndIsEmpty()
        {
            lock (_gate)
            {
                var snapshot = _entries;

                if (snapshot.Length is 0)
                    return true;

                var live = new List<WeakReference<TVal>>(snapshot.Length);
                var hadDead = false;

                for (var read = 0; read < snapshot.Length; read++)
                {
                    var entry = snapshot[read];

                    if (entry.TryGetTarget(out _))
                    {
                        live.Add(entry);
                        continue;
                    }

                    hadDead = true;
                }

                if (!hadDead)
                    return false;

                if (live.Count is 0)
                {
                    Volatile.Write(ref _entries, []);
                    return true;
                }

                Volatile.Write(ref _entries, [.. live]);
                return false;
            }
        }

        private bool TryFindUnlocked(TVal candidate, out TVal canonical)
        {
            var snapshot = _entries;

            for (var read = 0; read < snapshot.Length; read++)
            {
                var weak = snapshot[read];

                if (!weak.TryGetTarget(out var target))
                    continue;

                if (ReferenceEquals(target, candidate) || target.Equals(candidate))
                {
                    canonical = target;
                    return true;
                }
            }

            canonical = candidate;
            return false;
        }
    }
}
