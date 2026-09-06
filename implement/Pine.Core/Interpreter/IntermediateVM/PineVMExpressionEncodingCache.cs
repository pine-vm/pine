using Pine.Core.CommonEncodings;
using System;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.Threading;

namespace Pine.Core.Interpreter.IntermediateVM;

/// <summary>
/// Thread-safe cache of the encoded values used by PineVM for expressions.
/// </summary>
public sealed class PineVMExpressionEncodingCache
{
    private readonly ConcurrentDictionary<Expression, Lazy<PineValue>> _entries = [];

    private readonly PineExpressionEncodingCache _standardEncodingCache = new();

    /// <summary>
    /// Gets the number of cached expressions.
    /// </summary>
    public int Count => _entries.Count;

    /// <summary>
    /// Gets or creates the standard encoding of an expression.
    /// </summary>
    public PineValue GetOrEncode(Expression expression)
    {
        var lazyEncoding =
            _entries.GetOrAdd(
                expression,
                _ =>
                new Lazy<PineValue>(
                    () => _standardEncodingCache.EncodeExpressionAsValue(expression),
                    LazyThreadSafetyMode.ExecutionAndPublication));

        return GetValue(expression, lazyEncoding);
    }

    /// <summary>
    /// Registers an encoded value that was successfully parsed as the supplied expression.
    /// </summary>
    public void RegisterParsedEncoding(
        Expression expression,
        PineValue encodedExpression)
    {
        _entries.TryAdd(
            expression,
            new Lazy<PineValue>(
                () => encodedExpression,
                LazyThreadSafetyMode.ExecutionAndPublication));
    }

    private PineValue GetValue(
        Expression expression,
        Lazy<PineValue> lazyEncoding)
    {
        try
        {
            return lazyEncoding.Value;
        }
        catch
        {
            ((ICollection<KeyValuePair<Expression, Lazy<PineValue>>>)_entries)
                .Remove(new(expression, lazyEncoding));

            throw;
        }
    }
}
