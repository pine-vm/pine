using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

namespace ElmTime;

public static class Cache
{
    public static void RemoveItemsToLimitRetainedSize<TKey, TValue, TOrderKey>(
        IDictionary<TKey, TValue> cache,
        Func<KeyValuePair<TKey, TValue>, long> computeItemSize,
        Func<KeyValuePair<TKey, TValue>, TOrderKey> computeItemRetentionPriority,
        long retainedSizeLimit)
    {
        var itemsOrderedByRetentionPrio =
            cache
            .OrderByDescending(computeItemRetentionPriority)
            .ToImmutableList();

        long aggregateSize = 0;

        foreach (var item in itemsOrderedByRetentionPrio)
        {
            var itemSize = computeItemSize(item);

            aggregateSize += itemSize;

            if (retainedSizeLimit < aggregateSize)
                cache.Remove(item.Key, out _);
        }
    }
}
