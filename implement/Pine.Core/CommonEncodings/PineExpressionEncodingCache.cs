using System.Collections.Concurrent;

namespace Pine.Core.CommonEncodings;

/// <summary>
/// Caches results from encoding Pine <see cref="Expression"/> instances as <see cref="PineValue"/>s
/// as done in <see cref="ExpressionEncoding.EncodeExpressionAsValue(Expression)"/>.
/// <para>
/// Analogous to <see cref="CodeAnalysis.PineVMParseCache"/> on the parsing side: callers thread
/// a single shared instance through repeated encoding calls to avoid the quadratic re-encoding
/// of large shared subtrees produced by the Elm compiler.
/// </para>
/// </summary>
public class PineExpressionEncodingCache
{
    private readonly ConcurrentDictionary<Expression, PineValue.ListValue> _encodeExprCache = [];

    /// <summary>
    /// Encodes the given <paramref name="expression"/> as a <see cref="PineValue.ListValue"/> using
    /// <see cref="ExpressionEncoding.EncodeExpressionAsValue(Expression, PineExpressionEncodingCache?)"/>,
    /// caching the result. Subsequent calls with the same <paramref name="expression"/> return the
    /// cached result without re-encoding.
    /// </summary>
    public PineValue.ListValue EncodeExpressionAsValue(Expression expression) =>
        _encodeExprCache.GetOrAdd(
            expression,
            valueFactory:
            expr => ExpressionEncoding.EncodeExpressionAsValueWithoutTopLevelCacheLookup(expr, EncodeExpressionAsValue));
}
