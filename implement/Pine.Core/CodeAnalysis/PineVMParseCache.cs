using Pine.Core.Encodings;
using Pine.Core.Functional;
using Pine.Core.PineLanguage;
using Pine.Core.Runtime.IntermediateVM;
using System;
using System.Collections.Concurrent;
using System.Diagnostics;

namespace Pine.Core.CodeAnalysis;

/// <summary>
/// Caches results from parsing expressions as done in PineVM.
/// </summary>
public class PineVMParseCache
{
    internal ExpressionTemplateProofCache TemplateProofs { get; } = new();

    /// <summary>Distinct parameterized constructor shapes, including uncertain shapes.</summary>
    public int TemplateConstructorProofCount => TemplateProofs.ConstructorCount;

    /// <summary>Distinct exact fixed-code values with bounded recognition proofs.</summary>
    public int TemplateBoundedValueProofCount => TemplateProofs.BoundedValueCount;

    private readonly ConcurrentDictionary<
        PineValue,
        ExpressionEncoding2026.ParseExpressionResult>
        _parseExprCache = [];

    /// <summary>Number of cached expression values, including parsed subexpressions.</summary>
    public int Count => _parseExprCache.Count;

    /// <summary>
    /// Parses the given <paramref name="expressionValue"/> into an <see cref="Expression"/> using
    /// <see cref="ExpressionEncoding.ParseExpressionFromValue(PineValue)"/>, caching the result.
    /// </summary>
    /// <param name="expressionValue">The encoded Pine expression value to parse.</param>
    /// <returns>
    /// A <see cref="Result{TError, TOk}"/> where the <c>Ok</c> case contains the parsed <see cref="Expression"/>,
    /// and the <c>Err</c> case contains an error description if parsing failed. Subsequent calls with the
    /// same <paramref name="expressionValue"/> return the cached result without re-parsing.
    /// </returns>
    /// <remarks>
    /// Uses the iterative post-order traversal
    /// <see cref="ExpressionEncoding.ParseExpressionFromValueViaPostOrder"/>, backed directly by the
    /// cache dictionary, so that deeply nested encodings do not overflow the call stack while every
    /// distinct subexpression value is still parsed and cached at most once.
    /// </remarks>
    public Result<string, Expression> ParseExpression(PineValue expressionValue)
    {
        return ParseExpressionWithoutResultAllocation(expressionValue).ToPublicResult();
    }

    internal ExpressionEncoding2026.ParseExpressionResult ParseExpressionWithoutResultAllocation(
        PineValue expressionValue)
    {
        var profile = PineVMWorkProfile.Current;

        if (profile is null)
            return ExpressionEncoding.ParseExpressionFromValueViaPostOrder(expressionValue, _parseExprCache);

        var hit = _parseExprCache.ContainsKey(expressionValue);
        var count = hit ? 0 : _parseExprCache.Count;
        var bytes = GC.GetAllocatedBytesForCurrentThread();
        var ticks = Stopwatch.GetTimestamp();

        var parsed =
            ExpressionEncoding.ParseExpressionFromValueViaPostOrder(
                expressionValue,
                _parseExprCache);

        var elapsed = Stopwatch.GetTimestamp() - ticks;
        var allocated = GC.GetAllocatedBytesForCurrentThread() - bytes;

        profile.DirectTemplates.Parsed(
            parsed.Expression,
            hit,
            hit ? 0 : _parseExprCache.Count - count,
            allocated,
            elapsed);

        return parsed;
    }
}
