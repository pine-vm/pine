using System.Collections.Concurrent;
using Pine.Core.CommonEncodings;

namespace Pine.Core.CodeAnalysis;

/// <summary>
/// Caches results from parsing expressions as done in PineVM.
/// </summary>
public class PineVMParseCache
{
    private readonly ConcurrentDictionary<PineValue, Result<string, Expression>> _parseExprCache = [];

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
    public Result<string, Expression> ParseExpression(PineValue expressionValue)
    {
        return
            _parseExprCache
            .GetOrAdd(
                expressionValue,
                valueFactory:
                value =>
                ExpressionEncoding.ParseExpressionFromValue(value, ParseExpression));
    }
}

