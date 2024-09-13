using System.Collections.Generic;
using Pine.Core;

namespace Pine.PineVM;

/// <summary>
/// Caches results from parsing expressions as done in PineVM.
/// </summary>
public class PineVMParseCache
{
    private readonly Dictionary<PineValue, Result<string, Expression>> parseExprCache = [];

    public Result<string, Expression> ParseExpression(PineValue expressionValue)
    {
        if (parseExprCache.TryGetValue(expressionValue, out var cachedResult))
        {
            return cachedResult;
        }

        var result = ExpressionEncoding.ParseExpressionFromValueDefault(expressionValue);

        parseExprCache[expressionValue] = result;

        return result;
    }
}

