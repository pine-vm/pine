using Pine.Core.CodeAnalysis;
using System.Collections;
using System.Collections.Generic;
using System.Diagnostics.CodeAnalysis;
using System.Linq;

using Canonical = Pine.Core.PineLanguage.Expression;

namespace Pine.Core.Runtime.InternalExpression;

internal sealed class CanonicalReductionCacheAdapter(
    IDictionary<(Canonical, ReductionConfig), Canonical> source)
    : IDictionary<(Expression, ReductionConfig), Expression>
{
    private static (Canonical, ReductionConfig) Key((Expression, ReductionConfig) key) =>
        (ExpressionConversion.ToCanonicalCached(key.Item1), key.Item2);

    public Expression this[(Expression, ReductionConfig) key]
    {
        get => ExpressionConversion.FromCanonicalCached(source[Key(key)]);
        set => source[Key(key)] = ExpressionConversion.ToCanonicalCached(value);
    }

    public ICollection<(Expression, ReductionConfig)> Keys =>
        [
        .. source.Keys.Select(key => (ExpressionConversion.FromCanonicalCached(key.Item1), key.Item2))
        ];

    public ICollection<Expression> Values => [.. source.Values.Select(ExpressionConversion.FromCanonicalCached)];

    public int Count => source.Count;

    public bool IsReadOnly => source.IsReadOnly;

    public void Add((Expression, ReductionConfig) key, Expression value) =>
        source.Add(Key(key), ExpressionConversion.ToCanonicalCached(value));

    public bool ContainsKey((Expression, ReductionConfig) key) => source.ContainsKey(Key(key));

    public bool Remove((Expression, ReductionConfig) key) => source.Remove(Key(key));

    public bool TryGetValue((Expression, ReductionConfig) key, [NotNullWhen(true)] out Expression? value)
    {
        if (source.TryGetValue(Key(key), out var canonical))
        {
            value = ExpressionConversion.FromCanonicalCached(canonical);
            return true;
        }

        value = null;
        return false;
    }

    public void Add(KeyValuePair<(Expression, ReductionConfig), Expression> item) => Add(item.Key, item.Value);

    public void Clear() => source.Clear();

    public bool Contains(KeyValuePair<(Expression, ReductionConfig), Expression> item) =>
        source.Contains(new(Key(item.Key), ExpressionConversion.ToCanonicalCached(item.Value)));

    public void CopyTo(KeyValuePair<(Expression, ReductionConfig), Expression>[] array, int arrayIndex)
    {
        Enumerate().ToArray().CopyTo(array, arrayIndex);
    }

    public bool Remove(KeyValuePair<(Expression, ReductionConfig), Expression> item) =>
        source.Remove(
            new KeyValuePair<(Canonical, ReductionConfig), Canonical>(
                Key(item.Key),
                ExpressionConversion.ToCanonicalCached(item.Value)));

    public IEnumerator<KeyValuePair<(Expression, ReductionConfig), Expression>> GetEnumerator() =>
        Enumerate().GetEnumerator();

    private IEnumerable<KeyValuePair<(Expression, ReductionConfig), Expression>> Enumerate() =>
        source.Select(
            item => new KeyValuePair<(Expression, ReductionConfig), Expression>(
                (ExpressionConversion.FromCanonicalCached(item.Key.Item1), item.Key.Item2),
                ExpressionConversion.FromCanonicalCached(item.Value)));

    IEnumerator IEnumerable.GetEnumerator() => GetEnumerator();
}
