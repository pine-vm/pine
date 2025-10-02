using System;
using System.Collections.Concurrent;
using System.Collections.Generic;

namespace Pine.Core;

internal static class PineValueWeakInterner
{
    // Note: Keys avoid holding references to subtrees.
    private readonly record struct ListBucketKey(
        int SlimHashCode,
        int ItemsLength,
        long NodesCount,
        long BlobsBytesCount)
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

    // Trimming thresholds per bucket to avoid unbounded growth in degenerate cases.
    private const int MaxBucketSize = 16;

    public static bool TryGetCanonical(PineValue.ListValue candidate, out PineValue.ListValue canonical)
    {
        var key =
            new ListBucketKey(
                SlimHashCode: candidate.GetHashCode(),
                ItemsLength: candidate.Items.Length,
                NodesCount: candidate.NodesCount,
                BlobsBytesCount: candidate.BlobsBytesCount);

        if (!s_listBuckets.TryGetValue(key, out var bucket))
        {
            canonical = candidate;

            return false;
        }

        lock (bucket)
        {
            // Compact while probing.
            var write = 0;

            for (var read = 0; read < bucket.Count; read++)
            {
                if (!bucket[read].TryGetTarget(out var target))
                    continue;

                // Keep alive entries compacted at front
                bucket[write++] = bucket[read];

                // Reference or structural equality confirms canonical.
                if (ReferenceEquals(target, candidate) || target.Equals(candidate))
                {
                    canonical = target;

                    // Truncate any dead tail spots if compaction happened
                    if (write < bucket.Count)
                    {
                        bucket.RemoveRange(write, bucket.Count - write);
                    }

                    return true;
                }
            }

            if (write < bucket.Count)
                bucket.RemoveRange(write, bucket.Count - write);
        }

        canonical = candidate;
        return false;
    }

    public static void Register(PineValue.ListValue canonical)
    {
        var key =
            new ListBucketKey(
                SlimHashCode: canonical.GetHashCode(),
                ItemsLength: canonical.Items.Length,
                NodesCount: canonical.NodesCount,
                BlobsBytesCount: canonical.BlobsBytesCount);

        var bucket = s_listBuckets.GetOrAdd(key, _ => []);

        lock (bucket)
        {
            // Avoid duplicates when called concurrently
            for (var i = 0; i < bucket.Count; i++)
            {
                if (bucket[i].TryGetTarget(out var existing) && ReferenceEquals(existing, canonical))
                    return;
            }

            bucket.Add(new WeakReference<PineValue.ListValue>(canonical));
            TrimBucket(bucket);
        }
    }

    public static bool TryGetCanonical(PineValue.BlobValue candidate, out PineValue.BlobValue canonical)
    {
        var key = new BlobBucketKey(candidate.GetHashCode(), candidate.Bytes.Length);

        if (!s_blobBuckets.TryGetValue(key, out var bucket))
        {
            canonical = candidate;
            return false;
        }

        lock (bucket)
        {
            var write = 0;

            for (var read = 0; read < bucket.Count; read++)
            {
                if (!bucket[read].TryGetTarget(out var target))
                    continue;

                bucket[write++] = bucket[read];

                if (ReferenceEquals(target, candidate) || target.Equals(candidate))
                {
                    canonical = target;

                    if (write < bucket.Count)
                    {
                        bucket.RemoveRange(write, bucket.Count - write);
                    }

                    return true;
                }
            }

            if (write < bucket.Count)
            {
                bucket.RemoveRange(write, bucket.Count - write);
            }
        }

        canonical = candidate;

        return false;
    }

    public static void Register(PineValue.BlobValue canonical)
    {
        var key = new BlobBucketKey(canonical.GetHashCode(), canonical.Bytes.Length);

        var bucket = s_blobBuckets.GetOrAdd(key, _ => []);

        lock (bucket)
        {
            for (var i = 0; i < bucket.Count; i++)
            {
                if (bucket[i].TryGetTarget(out var existing) && ReferenceEquals(existing, canonical))
                    return;
            }

            bucket.Add(new WeakReference<PineValue.BlobValue>(canonical));
            TrimBucket(bucket);
        }
    }

    private static void TrimBucket<T>(List<WeakReference<T>> bucket) where T : class
    {
        if (bucket.Count <= MaxBucketSize)
            return;

        var write = 0;

        for (var read = 0; read < bucket.Count; read++)
        {
            if (bucket[read].TryGetTarget(out _))
                bucket[write++] = bucket[read];
        }

        if (write < bucket.Count)
        {
            bucket.RemoveRange(write, bucket.Count - write);
        }

        // If still too large (many live items), keep it as-is — the cap is advisory.
    }
}
