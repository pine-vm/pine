using Pine.Core.Functional;
using Pine.Core.PineLanguage;
using System;
using System.Collections.Concurrent;

namespace Pine.Core.Runtime.IntermediateVM;

/// <summary>
/// Thread-safe cache of expression identities and their demand-driven canonical views.
/// </summary>
public sealed class PineVMExpressionEncodingCache
{
    private readonly ConcurrentDictionary<InternalExpression.Expression, ExpressionIdentity> _entries = [];

    /// <summary>
    /// Gets the number of retained identities, including views not yet materialized.
    /// </summary>
    public int Count => _entries.Count;

    /// <summary>
    /// Gets or creates the standard encoding of an expression.
    /// </summary>
    public PineValue GetOrEncode(Expression expression)
        => GetOrEncode(InternalExpression.ExpressionConversion.FromCanonicalCached(expression));

    internal PineValue GetOrEncode(InternalExpression.Expression expression)
        => GetIdentity(expression).EncodedValue;

    /// <summary>Retains an identity without requesting canonical syntax, an encoded value, or a hash.</summary>
    public ExpressionIdentity GetIdentity(InternalExpression.Expression expression) =>
        _entries.GetOrAdd(expression, static expression => new(expression));

    /// <summary>
    /// Registers a standard compact encoding of the supplied expression. Legacy or mismatched
    /// encodings are rejected, without changing the public legacy expression codecs.
    /// </summary>
    public void RegisterParsedEncoding(
        Expression expression,
        PineValue encodedExpression)
    {
        var retained = InternalExpression.ExpressionConversion.FromCanonicalCached(expression);
        var parsed = InternalExpression.ExpressionValueEncoding.ParseExpressionFromValue(encodedExpression);

        if (parsed.IsErrOrNull() is { } error)
            throw new ArgumentException("Invalid compact expression encoding: " + error, nameof(encodedExpression));

        if (!retained.Equals(parsed.IsOkOrNull()))
        {
            throw new ArgumentException(
                "The encoding does not match the supplied expression.",
                nameof(encodedExpression));
        }

        RegisterParsedEncoding(retained, encodedExpression);
    }

    internal void RegisterParsedEncoding(
        InternalExpression.Expression expression,
        PineValue encodedExpression)
    {
        GetIdentity(expression).RegisterEncodedValue(encodedExpression);
    }
}
