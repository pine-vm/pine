using Pine.Core.PineLanguage;
using System;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.Threading;

namespace Pine.Core.Runtime.IntermediateVM;

/// <summary>
/// Thread-safe cache of the encoded values used by PineVM for expressions.
/// </summary>
public sealed class PineVMExpressionEncodingCache
{
    private readonly ConcurrentDictionary<InternalExpression.Expression, Lazy<PineValue>> _entries = [];

    /// <summary>
    /// Gets the number of cached expressions.
    /// </summary>
    public int Count => _entries.Count;

    /// <summary>
    /// Gets or creates the standard encoding of an expression.
    /// </summary>
    public PineValue GetOrEncode(Expression expression)
        => GetOrEncode(InternalExpression.ExpressionConversion.FromCanonicalCached(expression));

    internal PineValue GetOrEncode(InternalExpression.Expression expression)
    {
        var lazyEncoding =
            _entries.GetOrAdd(
                expression,
                _ =>
                new Lazy<PineValue>(
                    () => InternalExpression.ExpressionValueEncoding.EncodeExpressionAsValue(expression),
                    LazyThreadSafetyMode.ExecutionAndPublication));

        return GetValue(expression, lazyEncoding);
    }

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

        _entries.TryAdd(
            expression,
            new Lazy<PineValue>(
                () => encodedExpression,
                LazyThreadSafetyMode.ExecutionAndPublication));
    }

    private PineValue GetValue(
        InternalExpression.Expression expression,
        Lazy<PineValue> lazyEncoding)
    {
        try
        {
            return lazyEncoding.Value;
        }
        catch
        {
            ((ICollection<KeyValuePair<InternalExpression.Expression, Lazy<PineValue>>>)_entries)
                .Remove(new(expression, lazyEncoding));

            throw;
        }
    }
}
