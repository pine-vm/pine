using System.Collections.Concurrent;
using System.Collections.Immutable;

namespace Pine.PineVM;

/// <summary>
/// A cache for function application results and other expressions of type <see cref="Expression.DecodeAndEvaluateExpression"/>
/// It uses the environment time source to decide which evaluation results are worth caching and therefore is not deterministic.
/// </summary>
public class PineVMCache
{
    public record FunctionApplicationCacheEntryKey(Expression.DecodeAndEvaluateExpression Expression, PineValue Environment);

    private readonly ConcurrentDictionary<FunctionApplicationCacheEntryKey, PineValue> functionApplicationCache = new();

    public long FunctionApplicationCacheSize => functionApplicationCache.Count;

    public long FunctionApplicationCacheLookupCount { private set; get; }

    public IImmutableDictionary<FunctionApplicationCacheEntryKey, PineValue> CopyFunctionApplicationCache() =>
        functionApplicationCache.ToImmutableDictionary();

    public PineVM.EvalExprDelegate BuildEvalExprDelegate(PineVM.EvalExprDelegate evalExprDelegate)
    {
        return new PineVM.EvalExprDelegate((expression, environment) =>
        {
            Result<string, PineValue> evalWithoutCache() => evalExprDelegate(expression, environment);

            if (expression is Expression.DecodeAndEvaluateExpression decodeAndEvalExpr)
            {
                ++FunctionApplicationCacheLookupCount;

                var cacheKey = new FunctionApplicationCacheEntryKey(Expression: decodeAndEvalExpr, Environment: environment);

                if (functionApplicationCache.TryGetValue(cacheKey, out var cachedResult))
                    return Result<string, PineValue>.ok(cachedResult);

                var evalStopwatch = System.Diagnostics.Stopwatch.StartNew();

                var evalResult = evalExprDelegate(expression, environment);

                if (4 <= evalStopwatch.ElapsedMilliseconds && evalResult is Result<string, PineValue>.Ok evalOk)
                {
                    functionApplicationCache[cacheKey] = evalOk.Value;
                }

                return evalResult;
            }

            return evalWithoutCache();
        });
    }
}

