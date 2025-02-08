using System.Collections.Concurrent;
using Pine.Core;

namespace Pine.PineVM;

/// <summary>
/// Caches results from parsing expressions as done in PineVM.
/// </summary>
public class PineVMParseCache
{
    private readonly ConcurrentDictionary<PineValue, Result<string, Expression>> parseExprCache = [];

    public Result<string, Expression> ParseExpression(PineValue expressionValue)
    {
        return
            parseExprCache
            .GetOrAdd(
                expressionValue,
                valueFactory:
                ExpressionEncoding.ParseExpressionFromValueDefault);
    }
}

