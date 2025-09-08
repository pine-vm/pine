using System;
using System.Collections.Concurrent;

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
        if (shouldCache(pineValue))
        {
            return
                _valueHashCache.GetOrAdd(
                    pineValue,
                    valueFactory: v => PineValueHashTree.ComputeHash(v, other => GetHash(other, shouldCache)));
        }
        else
        {
            return PineValueHashTree.ComputeHash(pineValue, other => GetHash(other, shouldCache));
        }
    }
}
