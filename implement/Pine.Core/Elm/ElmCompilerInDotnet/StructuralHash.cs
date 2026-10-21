using Pine.Core.Elm.ElmSyntax.SyntaxModel;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;

namespace Pine.Core.Elm.ElmCompilerInDotnet;

internal static class StructuralHash
{
    public static int GetOrCompute<T>(ref long cache, Type type, T value)
    {
        if (SyntaxHashCache.TryGet(ref cache, out var hash))
            return hash;

        return SyntaxHashCache.Store(ref cache, HashCode.Combine(type, value));
    }

    public static int GetOrCompute<T1, T2>(ref long cache, Type type, T1 first, T2 second)
    {
        if (SyntaxHashCache.TryGet(ref cache, out var hash))
            return hash;

        return SyntaxHashCache.Store(ref cache, HashCode.Combine(type, first, second));
    }

    public static int GetOrCompute<T1, T2, T3>(ref long cache, Type type, T1 first, T2 second, T3 third)
    {
        if (SyntaxHashCache.TryGet(ref cache, out var hash))
            return hash;

        return SyntaxHashCache.Store(ref cache, HashCode.Combine(type, first, second, third));
    }

    public static int GetOrCompute<T1, T2, T3, T4>(ref long cache, Type type, T1 first, T2 second, T3 third, T4 fourth)
    {
        if (SyntaxHashCache.TryGet(ref cache, out var hash))
            return hash;

        return SyntaxHashCache.Store(ref cache, HashCode.Combine(type, first, second, third, fourth));
    }

    public static int UnorderedMap<TKey, TValue>(ImmutableDictionary<TKey, TValue> entries) where TKey : notnull
    {
        var sum = 0;
        var xor = 0;

        foreach (var (key, value) in entries)
        {
            var hash = HashCode.Combine(entries.KeyComparer.GetHashCode(key), value);
            sum = unchecked(sum + hash);
            xor ^= hash;
        }

        return HashCode.Combine(entries.Count, sum, xor);
    }

    public static bool SameEntries<TKey, TValue>(
        ImmutableDictionary<TKey, TValue> first, ImmutableDictionary<TKey, TValue> second) where TKey : notnull
    {
        if (ReferenceEquals(first, second))
            return true;

        if (first.Count != second.Count || !first.KeyComparer.Equals(second.KeyComparer))
            return false;

        foreach (var (key, value) in first)
        {
            if (!second.TryGetValue(key, out var other) || !EqualityComparer<TValue>.Default.Equals(value, other))
                return false;
        }

        return true;
    }
}
