using System.Collections.Concurrent;
using System.Collections.Generic;

namespace Pine.PineVM;

/// <summary>
/// Caches results from parsing and evaluating expressions as done in PineVM.
/// 
/// For parsing of expressions, it always caches the result.
/// 
/// For expression evaluation cache, it only considers caching if the expression is of type <see cref="Expression.ParseAndEval"/>.
/// To decide whether to cache the result, it uses the environment time source to measure time spent on evaluation.
/// Therefore, the caching of evaluation results is not deterministic.
/// </summary>
public class PineVMCache
{
    private readonly ConcurrentDictionary<PineValue, Result<string, Expression>> parseExprCache = new();

    public Dictionary<EvalCacheEntryKey, PineValue> EvalCache { init; get; } = [];

    public long FunctionApplicationCacheSize => EvalCache.Count;


    public ParseExprDelegate BuildParseExprDelegate(ParseExprDelegate evalExprDelegate)
    {
        return new ParseExprDelegate(exprValue =>
        {
            return parseExprCache.GetOrAdd(
                key: exprValue,
                valueFactory: exprValue => evalExprDelegate(exprValue));
        });
    }
}

