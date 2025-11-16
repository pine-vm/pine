using Pine.Core;
using Pine.Core.Interpreter.IntermediateVM;
using Pine.Core.PineVM;
using Pine.IntermediateVM;

namespace Pine.PineVM;

/// <summary>
/// A Pine virtual machine (VM) implementation that automatically resets its evaluation cache when a specified
/// entry threshold is reached.
/// </summary>
public class PineVMResettingCache : IPineVM
{
    private readonly Core.Interpreter.IntermediateVM.PineVM _pineVM;

    private readonly InvocationCache _pineVMCache;

    private readonly int? _resetCacheEntriesThresholdDefault;

    private PineVMResettingCache(
        int? resetCacheEntriesThresholdDefault)
    {
        _resetCacheEntriesThresholdDefault = resetCacheEntriesThresholdDefault;

        _pineVMCache = [];

        _pineVM =
            SetupVM.Create(evalCache: _pineVMCache);
    }

    /// <summary>
    /// Create an instance for a given reset threshold.
    /// </summary>
    public static PineVMResettingCache Create(
        int? resetCacheEntriesThresholdDefault)
    {
        return new PineVMResettingCache(
            resetCacheEntriesThresholdDefault: resetCacheEntriesThresholdDefault);
    }

    public Result<string, PineValue> EvaluateExpression(Expression expression, PineValue environment)
    {
        var result =
            _pineVM.EvaluateExpression(expression, environment);

        if (_resetCacheEntriesThresholdDefault is { } threshold)
        {
            ResetCacheIfCountExceedsThreshold(threshold);
        }

        return result;
    }

    public bool ResetCacheIfCountExceedsThreshold(
        int threshold)
    {
        if (_pineVMCache.Count >= threshold)
        {
            _pineVMCache.Clear();

            return true;
        }

        return false;
    }
}
