using System;
using System.Collections.Concurrent;
using System.Collections.Generic;

namespace Pine.Core.Addressing;

/// <summary>
/// Provides a thread-safe cache for storing and retrieving hashes of <see cref="PineValue"/> instances.
/// </summary>
public class ConcurrentPineValueHashCache
{
    readonly ConcurrentDictionary<PineValue, ReadOnlyMemory<byte>> _valueHashCache = new();

    /// <summary>
    /// Gets the hash for the specified <paramref name="pineValue"/>, using the cache.
    /// </summary>
    /// <param name="pineValue">The value to hash.</param>
    /// <returns>The hash as a <see cref="ReadOnlyMemory{Byte}"/>.</returns>
    public ReadOnlyMemory<byte> GetHash(PineValue pineValue) =>
        GetHash(pineValue, shouldCache: _ => true);

    /// <summary>
    /// Gets the hash for the specified <paramref name="pineValue"/>, optionally caching the result based on <paramref name="shouldCache"/>.
    /// </summary>
    /// <param name="pineValue">The value to hash.</param>
    /// <param name="shouldCache">A function that determines whether to cache the hash for the given value.</param>
    /// <returns>The hash as a <see cref="ReadOnlyMemory{Byte}"/>.</returns>
    public ReadOnlyMemory<byte> GetHash(
        PineValue pineValue,
        Func<PineValue, bool> shouldCache)
    {
        if (_valueHashCache.TryGetValue(pineValue, out var cached))
        {
            return cached;
        }

        var freshlyComputed = new Dictionary<PineValue, ReadOnlyMemory<byte>>();

        ReadOnlyMemory<byte>? TryGetCached(PineValue value)
        {
            if (_valueHashCache.TryGetValue(value, out var fromGlobal))
            {
                return fromGlobal;
            }

            if (freshlyComputed.TryGetValue(value, out var fromLocal))
            {
                return fromLocal;
            }

            return null;
        }

        void RecordComputed(PineValue value, ReadOnlyMemory<byte> hash)
        {
            if (!shouldCache(value))
            {
                return;
            }

            freshlyComputed[value] = hash;
        }

        var hash =
            PineValueHashTree.ComputeHash(
                pineValue,
                delegateGetHashOfComponent: TryGetCached,
                reportComputedHash: RecordComputed);

        foreach (var kvp in freshlyComputed)
        {
            _valueHashCache.TryAdd(kvp.Key, kvp.Value);
        }

        return hash;
    }
}
