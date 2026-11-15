using System.Collections.Frozen;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Diagnostics.CodeAnalysis;

using Canonical = Pine.Core.PineLanguage.Expression;

namespace Pine.Core.Runtime.InternalExpression;

internal sealed class ExpressionLookup<T>
{
    private readonly IReadOnlyDictionary<Canonical, T>? _canonical;

    private readonly FrozenDictionary<Expression, T>? _retained;

    private ExpressionLookup(IReadOnlyDictionary<Canonical, T> source)
    {
        // Snapshot only immutable tables with ordinary structural equality; all other tables remain live.
        if (source is FrozenDictionary<Canonical, T> frozen &&
            frozen.Comparer == EqualityComparer<Canonical>.Default ||
            source is ImmutableDictionary<Canonical, T> immutable &&
            immutable.KeyComparer == EqualityComparer<Canonical>.Default)
        {
            _retained =
                source.ToFrozenDictionary(
                    pair => ExpressionConversion.FromCanonicalCached(pair.Key),
                    pair => pair.Value);
        }
        else
            _canonical = source;
    }

    internal static ExpressionLookup<T>? Create(IReadOnlyDictionary<Canonical, T>? source) =>
        source is null ? null : new(source);

    internal bool ContainsKey(Expression expression) =>
        _retained?.ContainsKey(expression) ??
        _canonical!.ContainsKey(ExpressionConversion.ToCanonicalCached(expression));

    internal bool TryGetValue(Expression expression, [MaybeNullWhen(false)] out T value) =>
        _retained is { } retained
        ?
        retained.TryGetValue(expression, out value)
        :
        _canonical!.TryGetValue(ExpressionConversion.ToCanonicalCached(expression), out value);
}
