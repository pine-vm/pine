using Pine.Core.Addressing;
using Pine.Core.CodeAnalysis;
using Pine.Core.Encodings;
using Pine.Core.PineLanguage;
using System;
using System.Collections.Concurrent;

namespace Pine.Core.DotNet;

/// <summary>
/// Names public spread workers and canonical dispatch methods from their original expressions.
/// Callbacks run once per declaration, only after the graph is proven evaluation-closed.
/// Reuse a policy across compilations to retain its expression-encoding and hash memoization.
/// </summary>
public sealed record PineCSharpMethodNaming(
    Func<Expression, PineValueClass?, string> GetMethodName,
    Func<Expression, string> GetDispatchMethodName)
{
    /// <summary>
    /// Uses ten lowercase hexadecimal digits from each canonical hash. Null and empty constraints use
    /// "generic"; constrained workers use the established <see cref="PineValueClass.HashBase16"/>.
    /// The caller-owned hash-tree cache can also be shared with other compilations and tooling.
    /// </summary>
    public static PineCSharpMethodNaming Default(ConcurrentPineValueHashCache canonicalHashCache)
    {
        ArgumentNullException.ThrowIfNull(canonicalHashCache);
        var encodings = new ExpressionEncodingCache();
        var hashes = new ConcurrentDictionary<Expression, Lazy<string>>();

        string ExpressionHash(Expression expression) =>
            hashes.GetOrAdd(
                expression,
                expression => new(() => Convert.ToHexStringLower(
                    canonicalHashCache.GetHash(encodings.EncodeExpressionAsValue(expression)).Span)))
            .Value[..10];

        return
            new(
                (expression, constraint) => "Expr_" + ExpressionHash(expression) + "_constraint_" +
                    (constraint is null || constraint.ParsedItems.Count == 0 ? "generic" : constraint.HashBase16[..10]),
                expression => "Expr_" + ExpressionHash(expression) + "_dispatch");
    }
}
