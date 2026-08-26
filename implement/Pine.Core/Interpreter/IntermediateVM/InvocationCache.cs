using Pine.Core.Addressing;
using Pine.Core.CommonEncodings;
using Pine.Core.IO;
using Pine.Core.PineVM;
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

namespace Pine.Core.Interpreter.IntermediateVM;

/// <summary>
/// Key for caching results from function application in the VM.
/// </summary>
public readonly record struct EvalCacheEntryKey(PineValue ExprValue, StackFrameInput StackFrameInput);

/// <summary>
/// Caches results from function application in the VM.
/// </summary>
public sealed class InvocationCache : Dictionary<EvalCacheEntryKey, PineValue>;

/// <summary>
/// Thread-safe invocation cache for sharing entries between VM instances.
/// </summary>
public sealed class ConcurrentInvocationCache : IInvocationCacheAccess
{
    private readonly System.Threading.Lock _lock = new();

    private readonly Dictionary<EvalCacheEntryKey, PineValue> _entries = [];

    private readonly HashSet<PineValue> _expressionsWithEntries = [];

    /// <summary>
    /// Gets the number of shared entries.
    /// </summary>
    public int Count
    {
        get
        {
            lock (_lock)
            {
                return _entries.Count;
            }
        }
    }

    /// <inheritdoc/>
    public bool MayContainExpression(PineValue expressionValue)
    {
        lock (_lock)
        {
            return _expressionsWithEntries.Contains(expressionValue);
        }
    }

    /// <inheritdoc/>
    public bool TryGet(
        EvalCacheEntryKey key,
        out PineValue value)
    {
        key.StackFrameInput.Materialize();

        lock (_lock)
        {
            return _entries.TryGetValue(key, out value!);
        }
    }

    /// <inheritdoc/>
    public bool TryAdd(
        EvalCacheEntryKey key,
        PineValue value)
    {
        key.StackFrameInput.Materialize();

        lock (_lock)
        {
            if (!_entries.TryAdd(key, value))
            {
                return false;
            }

            _expressionsWithEntries.Add(key.ExprValue);

            return true;
        }
    }
}

/// <summary>
/// Worker-owned cache that reads local entries before a shared cache and buffers all writes locally.
/// </summary>
public sealed class BufferedInvocationCacheAccess : IInvocationCacheAccess
{
    private readonly IInvocationCacheAccess _sharedCache;

    private readonly InvocationCache _localEntries = [];

    private readonly InvocationCacheAccessFromDictionary _localCache;

    /// <summary>
    /// Creates a worker-owned buffer over the supplied shared cache.
    /// </summary>
    public BufferedInvocationCacheAccess(IInvocationCacheAccess sharedCache)
    {
        ArgumentNullException.ThrowIfNull(sharedCache);

        _sharedCache = sharedCache;
        _localCache = new InvocationCacheAccessFromDictionary(_localEntries);
    }

    /// <summary>
    /// Gets the number of entries waiting to be merged.
    /// </summary>
    public int BufferedEntryCount => _localEntries.Count;

    /// <inheritdoc/>
    public bool MayContainExpression(PineValue expressionValue) =>
        _localCache.MayContainExpression(expressionValue) ||
        _sharedCache.MayContainExpression(expressionValue);

    /// <inheritdoc/>
    public bool TryGet(
        EvalCacheEntryKey key,
        out PineValue value)
    {
        key.StackFrameInput.Materialize();

        return
            _localCache.TryGet(key, out value) ||
            _sharedCache.TryGet(key, out value);
    }

    /// <inheritdoc/>
    public bool TryAdd(
        EvalCacheEntryKey key,
        PineValue value)
    {
        key.StackFrameInput.Materialize();

        return _localCache.TryAdd(key, value);
    }

    /// <summary>
    /// Publishes all buffered entries to the shared cache using first-writer-wins insertion,
    /// then clears the local buffer.
    /// </summary>
    public void MergeIntoShared()
    {
        foreach (var entry in _localEntries)
        {
            _sharedCache.TryAdd(entry.Key, entry.Value);
        }

        _localEntries.Clear();
    }
}

/// <summary>
/// Invocation-cache tier that applies persistent-cache configuration and delegates
/// in-memory access to another cache implementation.
/// </summary>
public sealed class PersistentInvocationCacheAccess : IInvocationCacheAccess
{
    private readonly IInvocationCacheAccess? _memoryCache;

    private readonly IFileStore _fileStore;

    private readonly OptimizationParametersSerial _optimizationParameters;

    private readonly StackFrameInputHash _stackFrameInputHash = new();

    /// <summary>
    /// Creates a persistent tier with an optional in-memory tier.
    /// </summary>
    public PersistentInvocationCacheAccess(
        IInvocationCacheAccess? memoryCache,
        IFileStore fileStore,
        OptimizationParametersSerial optimizationParameters)
    {
        ArgumentNullException.ThrowIfNull(fileStore);
        ArgumentNullException.ThrowIfNull(optimizationParameters);

        _memoryCache = memoryCache;
        _fileStore = fileStore;
        _optimizationParameters = optimizationParameters;
    }

    /// <inheritdoc/>
    public bool MayContainExpression(PineValue expressionValue) =>
        (_memoryCache?.MayContainExpression(expressionValue) ?? false) ||
        PersistentConfigurationForExpression(expressionValue) is not null;

    /// <inheritdoc/>
    public bool TryGet(
        EvalCacheEntryKey key,
        out PineValue value)
    {
        if (_memoryCache?.TryGet(key, out value) is true)
        {
            return true;
        }

        if (BuildPersistentFileName(key) is not { } fileName ||
            _fileStore.GetFileContent([fileName]) is not { } cachedContent)
        {
            value = null!;
            return false;
        }

        try
        {
            value = ValueEncodingFlatDeterministic.DecodeRoot(cachedContent);
        }
        catch (Exception ex)
        {
            throw new Exception(
                "Failed to decode cached value for cache file '" + fileName + "'.",
                ex);
        }

        _memoryCache?.TryAdd(key, value);

        return true;
    }

    /// <inheritdoc/>
    public bool TryAdd(
        EvalCacheEntryKey key,
        PineValue value)
    {
        var addedToMemory = _memoryCache?.TryAdd(key, value) ?? false;

        if (BuildPersistentFileName(key) is not { } fileName)
        {
            return addedToMemory;
        }

        using var stream = new MemoryStream();

        ValueEncodingFlatDeterministic.Encode(stream, value);

        _fileStore.SetFileContent(
            path: [fileName],
            fileContent: stream.ToArray());

        return _memoryCache is null || addedToMemory;
    }

    private OptimizationParametersSerial.ExpressionConfig? PersistentConfigurationForExpression(
        PineValue expressionValue)
    {
        var (expressionHashBytes, _) =
            PineValueHashFlat.ComputeHashForValue(expressionValue);

        var config = _optimizationParameters.ConfigForExpression(expressionHashBytes);

        return config?.PersistentCachePredicate is null ? null : config;
    }

    private string? BuildPersistentFileName(EvalCacheEntryKey key)
    {
        var (expressionHashBytes, _) =
            PineValueHashFlat.ComputeHashForValue(key.ExprValue);

        var config = _optimizationParameters.ConfigForExpression(expressionHashBytes);

        if (!(config?.PersistentCachePredicate?.SatisfiedBy(
            parameters: key.StackFrameInput.Parameters,
            arguments: key.StackFrameInput.EvaluatedArguments) ?? false))
        {
            return null;
        }

        var expressionHashBase16 =
            Convert.ToHexStringLower(expressionHashBytes.Span);

        var inputPersistentHashBytes =
            _stackFrameInputHash.ComposeHashBytes(key.StackFrameInput).HashBytes;

        var stackFrameInputPersistentHash =
            Convert.ToHexStringLower(inputPersistentHashBytes.Span);

        return
            expressionHashBase16[..16] + "_" +
            stackFrameInputPersistentHash[..16];
    }
}

/// <summary>
/// Provides the invocation-cache operations used by <see cref="PineVM"/>.
/// </summary>
public interface IInvocationCacheAccess
{
    /// <summary>
    /// Returns whether this cache may contain an entry for the given expression.
    /// </summary>
    bool MayContainExpression(PineValue expressionValue);

    /// <summary>
    /// Attempts to read an invocation result.
    /// </summary>
    bool TryGet(
        EvalCacheEntryKey key,
        out PineValue value);

    /// <summary>
    /// Attempts to add an invocation result without replacing an existing entry.
    /// </summary>
    bool TryAdd(
        EvalCacheEntryKey key,
        PineValue value);
}

/// <summary>
/// Adapts an <see cref="IDictionary{TKey,TValue}"/> to <see cref="IInvocationCacheAccess"/>.
/// </summary>
/// <remarks>
/// Creates an adapter over the supplied dictionary.
/// </remarks>
public sealed class InvocationCacheAccessFromDictionary(
    IDictionary<EvalCacheEntryKey, PineValue> dictionary) : IInvocationCacheAccess
{
    private readonly IDictionary<EvalCacheEntryKey, PineValue> _dictionary = dictionary;

    private readonly HashSet<PineValue> _expressionsWithEntries = [.. dictionary.Keys.Select(key => key.ExprValue)];

    private int _indexedEntryCount = dictionary.Count;

    /// <inheritdoc/>
    public bool MayContainExpression(PineValue expressionValue)
    {
        RefreshExpressionIndex();

        return
            _dictionary.Count is not 0 &&
            _expressionsWithEntries.Contains(expressionValue);
    }

    /// <inheritdoc/>
    public bool TryGet(
        EvalCacheEntryKey key,
        out PineValue value) =>
        _dictionary.TryGetValue(key, out value!);

    /// <inheritdoc/>
    public bool TryAdd(
        EvalCacheEntryKey key,
        PineValue value)
    {
        RefreshExpressionIndex();

        var added = _dictionary.TryAdd(key, value);

        _expressionsWithEntries.Add(key.ExprValue);
        _indexedEntryCount = _dictionary.Count;

        return added;
    }

    private void RefreshExpressionIndex()
    {
        if (_indexedEntryCount == _dictionary.Count)
        {
            return;
        }

        _expressionsWithEntries.Clear();

        foreach (var key in _dictionary.Keys)
        {
            _expressionsWithEntries.Add(key.ExprValue);
        }

        _indexedEntryCount = _dictionary.Count;
    }
}

/// <summary>
/// Numeric configuration for deciding which completed function applications are offered to an invocation cache.
/// </summary>
public sealed record InvocationCacheConfiguration
{
    /// <summary>
    /// Default values preserving the historic PineVM cache-admission formula.
    /// </summary>
    public static InvocationCacheConfiguration Default { get; } =
        new(
            frameCostThreshold: 700,
            stackFrameCost: 100,
            entrySpacingCostThreshold: 700,
            evalCost: 100);

    /// <summary>
    /// Minimum weighted cost of a completed frame. Admission uses a strict greater-than comparison.
    /// </summary>
    public int FrameCostThreshold { get; }

    /// <summary>
    /// Cost assigned to each stack frame created while evaluating the completed frame.
    /// </summary>
    public int StackFrameCost { get; }

    /// <summary>
    /// Minimum weighted work since the last successful insertion. Admission uses a strict greater-than comparison.
    /// </summary>
    public int EntrySpacingCostThreshold { get; }

    /// <summary>
    /// Cost assigned to each eval since the last successful insertion.
    /// </summary>
    public int EvalCost { get; }

    /// <summary>
    /// Creates a numeric invocation-cache configuration.
    /// </summary>
    public InvocationCacheConfiguration(
        int frameCostThreshold,
        int stackFrameCost,
        int entrySpacingCostThreshold,
        int evalCost)
    {
        ArgumentOutOfRangeException.ThrowIfNegative(frameCostThreshold);
        ArgumentOutOfRangeException.ThrowIfNegative(stackFrameCost);
        ArgumentOutOfRangeException.ThrowIfNegative(entrySpacingCostThreshold);
        ArgumentOutOfRangeException.ThrowIfNegative(evalCost);

        FrameCostThreshold = frameCostThreshold;
        StackFrameCost = stackFrameCost;
        EntrySpacingCostThreshold = entrySpacingCostThreshold;
        EvalCost = evalCost;
    }

    /// <summary>
    /// Returns whether the weighted costs pass both cache-admission thresholds.
    /// </summary>
    public bool ShouldOfferEntry(
        long frameInstructionCount,
        long frameStackFrameCount,
        long instructionCountSinceLastEntry,
        long evalCountSinceLastEntry) =>
        ExceedsThreshold(
            frameInstructionCount,
            frameStackFrameCount,
            StackFrameCost,
            FrameCostThreshold) &&
        ExceedsThreshold(
            instructionCountSinceLastEntry,
            evalCountSinceLastEntry,
            EvalCost,
            EntrySpacingCostThreshold);

    private static bool ExceedsThreshold(
        long unweightedCount,
        long weightedCount,
        int weight,
        int threshold) =>
        (Int128)unweightedCount + (Int128)weightedCount * weight > threshold;
}
