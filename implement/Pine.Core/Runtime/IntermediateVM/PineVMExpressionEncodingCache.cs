using Pine.Core.PineLanguage;
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
    /// Registers an encoded value that was successfully parsed as the supplied expression.
    /// </summary>
    public void RegisterParsedEncoding(
        Expression expression,
        PineValue encodedExpression)
        =>
        RegisterParsedEncoding(
            InternalExpression.ExpressionConversion.FromCanonicalCached(expression),
            encodedExpression);

    internal void RegisterParsedEncoding(
        InternalExpression.Expression expression,
        PineValue encodedExpression)
    {
        if (_entries.TryGetValue(expression, out _))
            return;

        _entries.TryAdd(expression, new(expression, encodedExpression));
    }
}
