using Pine.Core.CommonEncodings;
using System.Collections.Concurrent;

namespace Pine.Core.CodeAnalysis;

/// <summary>
/// Caches results from parsing expressions as done in PineVM.
/// </summary>
public class PineVMParseCache
{
    private readonly ConcurrentDictionary<
        PineValue,
        ExpressionEncoding2026.ParseExpressionResult>
        _parseExprCache = [];

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
        return
            ExpressionEncoding.ParseExpressionFromValueViaPostOrder(
                expressionValue,
                _parseExprCache)
            .ToPublicResult();
    }
}

