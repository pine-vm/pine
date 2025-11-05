using Pine.Core;
using Pine.Core.PineVM;

namespace Pine.PineVM;

/// <summary>
/// A Pine virtual machine (VM) implementation that automatically resets its evaluation cache when a specified
/// entry threshold is reached.
/// </summary>
public class PineVMResettingCache : IPineVM
{
    private readonly PineVM _pineVM;

    private readonly PineVMCache _pineVMCache;

    private readonly int _resetCacheEntriesThresold;

    private PineVMResettingCache(
        int resetCacheEntriesThresold)
    {
        _resetCacheEntriesThresold = resetCacheEntriesThresold;

        _pineVMCache = new PineVMCache();

        _pineVM =
            new PineVM(evalCache: _pineVMCache.EvalCache);
    }

    /// <summary>
    /// Create an instance for a given reset threshold.
    /// </summary>
    public static PineVMResettingCache Create(
        int resetCacheEntriesThresold)
    {
        return new PineVMResettingCache(
            resetCacheEntriesThresold: resetCacheEntriesThresold);
    }

    public Result<string, PineValue> EvaluateExpression(Expression expression, PineValue environment)
    {
        var result =
            _pineVM.EvaluateExpression(expression, environment);

        if (_pineVMCache.EvalCache.Count >= _resetCacheEntriesThresold)
        {
            _pineVMCache.EvalCache.Clear();
        }

        return result;
    }
}
