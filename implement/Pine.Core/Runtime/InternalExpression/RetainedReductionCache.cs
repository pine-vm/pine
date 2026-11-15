using Pine.Core.CodeAnalysis;
using System.Collections;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.Diagnostics.CodeAnalysis;
using System.Linq;

using Canonical = Pine.Core.PineLanguage.Expression;

namespace Pine.Core.Runtime.InternalExpression;

internal sealed class RetainedReductionCache : IDictionary<(Canonical, ReductionConfig), Canonical>
{
    internal ConcurrentDictionary<(Expression, ReductionConfig), Expression> Entries { get; } = [];

    private static (Expression, ReductionConfig) Key((Canonical, ReductionConfig) key) =>
        (ExpressionConversion.FromCanonicalCached(key.Item1), key.Item2);

    public Canonical this[(Canonical, ReductionConfig) key]
    {
        get => ExpressionConversion.ToCanonicalCached(Entries[Key(key)]);
        set => Entries[Key(key)] = ExpressionConversion.FromCanonicalCached(value);
    }

    public ICollection<(Canonical, ReductionConfig)> Keys =>
        [.. Entries.Keys.Select(key => (ExpressionConversion.ToCanonicalCached(key.Item1), key.Item2))];

    public ICollection<Canonical> Values => [.. Entries.Values.Select(ExpressionConversion.ToCanonicalCached)];

    public int Count => Entries.Count;

    public bool IsReadOnly => false;

    public void Add((Canonical, ReductionConfig) key, Canonical value) =>
        ((IDictionary<(Expression, ReductionConfig), Expression>)Entries).Add(
            Key(key),
            ExpressionConversion.FromCanonicalCached(value));

    public bool ContainsKey((Canonical, ReductionConfig) key) => Entries.ContainsKey(Key(key));

    public bool Remove((Canonical, ReductionConfig) key) => Entries.TryRemove(Key(key), out _);

    public bool TryGetValue((Canonical, ReductionConfig) key, [NotNullWhen(true)] out Canonical? value)
    {
        if (Entries.TryGetValue(Key(key), out var retained))
        {
            value = ExpressionConversion.ToCanonicalCached(retained);
            return true;
        }

        value = null;
        return false;
    }

    public void Add(KeyValuePair<(Canonical, ReductionConfig), Canonical> item) => Add(item.Key, item.Value);

    public void Clear() => Entries.Clear();

    public bool Contains(KeyValuePair<(Canonical, ReductionConfig), Canonical> item) =>
        ((ICollection<KeyValuePair<(Expression, ReductionConfig), Expression>>)Entries)
        .Contains(new(Key(item.Key), ExpressionConversion.FromCanonicalCached(item.Value)));

    public void CopyTo(KeyValuePair<(Canonical, ReductionConfig), Canonical>[] array, int arrayIndex)
    {
        Enumerate().ToArray().CopyTo(array, arrayIndex);
    }

    public bool Remove(KeyValuePair<(Canonical, ReductionConfig), Canonical> item) =>
        ((ICollection<KeyValuePair<(Expression, ReductionConfig), Expression>>)Entries)
        .Remove(new(Key(item.Key), ExpressionConversion.FromCanonicalCached(item.Value)));

    public IEnumerator<KeyValuePair<(Canonical, ReductionConfig), Canonical>> GetEnumerator() =>
        Enumerate().GetEnumerator();

    private IEnumerable<KeyValuePair<(Canonical, ReductionConfig), Canonical>> Enumerate() =>
        Entries.Select(
            item => new KeyValuePair<(Canonical, ReductionConfig), Canonical>(
                (ExpressionConversion.ToCanonicalCached(item.Key.Item1), item.Key.Item2),
                ExpressionConversion.ToCanonicalCached(item.Value)));

    IEnumerator IEnumerable.GetEnumerator() => GetEnumerator();
}
