using System.Collections.Concurrent;
using System.Collections.Immutable;

namespace Pine.PineVM;

/// <summary>
/// Caches results from parsing and evaluating expressions as done in PineVM.
/// 
/// For parsing of expressions, it always caches the result.
/// 
/// For expression evaluation cache, it only considers caching if the expression is of type <see cref="Expression.ParseAndEvalExpression"/>.
/// To decide whether to cache the result, it uses the environment time source to measure time spent on evaluation.
/// Therefore, the caching of evaluation results is not deterministic.
/// </summary>
public class PineVMCache
{
    public record FunctionApplicationCacheEntryKey(Expression.ParseAndEvalExpression Expression, PineValue Environment);

    private readonly ConcurrentDictionary<PineValue, Result<string, Expression>> parseExprCache = new();

    private readonly ConcurrentDictionary<FunctionApplicationCacheEntryKey, PineValue> functionApplicationCache = new();

    public long FunctionApplicationCacheSize => functionApplicationCache.Count;

    public long FunctionApplicationCacheLookupCount { private set; get; }

    public IImmutableDictionary<FunctionApplicationCacheEntryKey, PineValue> CopyFunctionApplicationCache() =>
        functionApplicationCache.ToImmutableDictionary();


    public PineVM.ParseExprDelegate BuildParseExprDelegate(PineVM.ParseExprDelegate evalExprDelegate)
    {
        return new PineVM.ParseExprDelegate(exprValue =>
        {
            return parseExprCache.GetOrAdd(
                key: exprValue,
                valueFactory: exprValue => evalExprDelegate(exprValue));
        });
    }

    public PineVM.EvalExprDelegate BuildEvalExprDelegate(PineVM.EvalExprDelegate evalExprDelegate)
    {
        return new PineVM.EvalExprDelegate((expression, environment) =>
        {
            if (expression is Expression.DelegatingExpression)
            {
                // Dictionary lookup is not implemented for DelegatingExpression.
                return evalExprDelegate(expression, environment);
            }

            if (expression is Expression.ParseAndEvalExpression parseAndEvalExpr)
            {
                ++FunctionApplicationCacheLookupCount;

                var cacheKey = new FunctionApplicationCacheEntryKey(Expression: parseAndEvalExpr, Environment: environment);

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

            return evalExprDelegate(expression, environment);
        });
    }
}

