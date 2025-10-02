using System;
using System.Collections.Concurrent;
using System.Collections.Generic;
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

    // Buckets map to a list of weak refs. We lock per-bucket on the list instance.
    private static readonly ConcurrentDictionary<ListBucketKey, List<WeakReference<PineValue.ListValue>>> s_listBuckets = new();
    private static readonly ConcurrentDictionary<BlobBucketKey, List<WeakReference<PineValue.BlobValue>>> s_blobBuckets = new();

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
                valueFactory: _ => new List<WeakReference<PineValue.ListValue>>(capacity: 1));

        lock (bucket)
        {
            for (var read = 0; read < bucket.Count; read++)
            {
                if (!bucket[read].TryGetTarget(out var target))
                    continue;

                if (ReferenceEquals(target, candidate) || target.Equals(candidate))
                {
                    return target;
                }
            }

            bucket.Add(new WeakReference<PineValue.ListValue>(candidate));
            MaybeStartSweep(s_listBuckets);

            return candidate;
        }
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
                valueFactory: _ => new List<WeakReference<PineValue.BlobValue>>(capacity: 2));

        lock (bucket)
        {
            for (var read = 0; read < bucket.Count; read++)
            {
                if (!bucket[read].TryGetTarget(out var target))
                    continue;

                if (ReferenceEquals(target, candidate) || target.Equals(candidate))
                {
                    return target;
                }
            }

            bucket.Add(new WeakReference<PineValue.BlobValue>(candidate));
            MaybeStartSweep(s_blobBuckets);

            return candidate;
        }
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

        lock (bucket)
        {
            for (var read = 0; read < bucket.Count; read++)
            {
                if (!bucket[read].TryGetTarget(out var target))
                    continue;

                if (ReferenceEquals(target, candidate) || target.Equals(candidate))
                {
                    return target;
                }
            }
        }

        return null;
    }

    public static void Register(PineValue.ListValue canonical) => _ = GetOrAdd(canonical);

    public static PineValue.BlobValue? TryGetCanonical(PineValue.BlobValue candidate)
    {
        var key = new BlobBucketKey(candidate.GetHashCode(), candidate.Bytes.Length);

        if (!s_blobBuckets.TryGetValue(key, out var bucket))
        {
            return null;
        }

        lock (bucket)
        {
            for (var read = 0; read < bucket.Count; read++)
            {
                if (!bucket[read].TryGetTarget(out var target))
                    continue;

                if (ReferenceEquals(target, candidate) || target.Equals(candidate))
                {
                    return target;
                }
            }
        }

        return null;
    }

    public static void Register(PineValue.BlobValue canonical) => _ = GetOrAdd(canonical);

    // --------------- Probabilistic sweeping ---------------

    private static void MaybeStartSweep<TKey, TVal>(
        ConcurrentDictionary<TKey, List<WeakReference<TVal>>> buckets)
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
        ConcurrentDictionary<TKey, List<WeakReference<TVal>>> buckets,
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

            lock (bucket)
            {
                var write = 0;

                for (var read = 0; read < bucket.Count; read++)
                {
                    if (bucket[read].TryGetTarget(out _))
                    {
                        if (write != read)
                        {
                            bucket[write] = bucket[read];
                        }

                        write++;
                    }
                }

                if (write is 0)
                {
                    if (buckets.TryGetValue(key, out var current) &&
                        ReferenceEquals(current, bucket))
                    {
                        buckets.TryRemove(key, out _);
                    }
                }
                else
                {
                    if (write < bucket.Count)
                        bucket.RemoveRange(write, bucket.Count - write);
                }
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
}
