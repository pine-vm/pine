using Pine.Core.Addressing;
using Pine.Core.CodeAnalysis;
using Pine.Core.Encodings;
using Pine.Core.Functional;
using Pine.Core.Internal;
using Pine.Core.IO;
using Pine.Core.PineLanguage;
using Pine.Core.PineVM;
using System;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Threading;

namespace Pine.Core.Runtime.IntermediateVM;

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
    private readonly Lock _lock = new();

    private readonly Dictionary<EvalCacheEntryKey, PineValue> _entries = [];

    private readonly ConcurrentDictionary<PineValue, byte> _expressionsWithEntries = [];

    private long _expressionMembershipVersion;

    /// <inheritdoc/>
    public long? ExpressionMembershipVersion => Volatile.Read(ref _expressionMembershipVersion);

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
    public bool MayContainExpression(PineValue expressionValue) =>
        _expressionsWithEntries.ContainsKey(expressionValue);

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

            PublishExpression(key.ExprValue);

            return true;
        }
    }

    /// <inheritdoc/>
    public void Merge(IReadOnlyDictionary<EvalCacheEntryKey, PineValue> entries)
    {
        foreach (var entry in entries)
            entry.Key.StackFrameInput.Materialize();

        lock (_lock)
        {
            foreach (var entry in entries)
            {
                if (_entries.TryAdd(entry.Key, entry.Value))
                    PublishExpression(entry.Key.ExprValue);
            }
        }
    }

    private void PublishExpression(PineValue expressionValue)
    {
        // Publish the result first, then membership, then invalidate request-local negatives.
        if (_expressionsWithEntries.TryAdd(expressionValue, 0))
            Interlocked.Increment(ref _expressionMembershipVersion);
    }
}

/// <summary>
/// Worker-owned cache that reads local entries before a shared cache and buffers all writes locally.
/// </summary>
public sealed class BufferedInvocationCacheAccess : IInvocationCacheAccess
{
    private const int MaximumNegativeExpressionCount = 1024;

    private readonly IInvocationCacheAccess _sharedCache;

    private readonly InvocationCache _localEntries = [];

    private readonly InvocationCacheAccessFromDictionary _localCache;

    private readonly int _maxEntryCount;

    private HashSet<PineValue>? _negativeExpressions;

    private PineValue? _lastNegativeExpression;

    private long _negativeExpressionVersion;

    /// <summary>
    /// Creates a worker-owned buffer over the supplied shared cache.
    /// </summary>
    public BufferedInvocationCacheAccess(IInvocationCacheAccess sharedCache, int maxEntryCount = int.MaxValue)
    {
        ArgumentNullException.ThrowIfNull(sharedCache);
        ArgumentOutOfRangeException.ThrowIfLessThan(maxEntryCount, 1);

        _sharedCache = sharedCache;
        _maxEntryCount = maxEntryCount;
        _localCache = new InvocationCacheAccessFromDictionary(_localEntries);
    }

    /// <summary>
    /// Gets the number of entries waiting to be merged.
    /// </summary>
    public int BufferedEntryCount => _localEntries.Count;

    /// <inheritdoc/>
    public bool MayContainExpression(PineValue expressionValue)
    {
        if (_localCache.MayContainExpression(expressionValue))
            return true;

        if (_sharedCache.ExpressionMembershipVersion is not { } version)
            return _sharedCache.MayContainExpression(expressionValue);

        if (_negativeExpressionVersion != version)
        {
            _negativeExpressions?.Clear();
            _lastNegativeExpression = null;
            _negativeExpressionVersion = version;
        }

        if (_lastNegativeExpression is not null && ReferenceEquals(_lastNegativeExpression, expressionValue))
            return false;

        if (_negativeExpressions?.Contains(expressionValue) is true)
        {
            _lastNegativeExpression = expressionValue;
            return false;
        }

        var result = _sharedCache.MayContainExpression(expressionValue);

        // A miss spanning publication must not be associated with the newer version.
        if (!result &&
            _sharedCache.ExpressionMembershipVersion == version &&
            (_negativeExpressions?.Count ?? 0) < MaximumNegativeExpressionCount)
        {
            (_negativeExpressions ??= []).Add(expressionValue);
            _lastNegativeExpression = expressionValue;
        }

        return result;
    }

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

        return _localEntries.Count < _maxEntryCount && _localCache.TryAdd(key, value);
    }

    /// <summary>
    /// Publishes all buffered entries to the shared cache using first-writer-wins insertion,
    /// then clears the local buffer.
    /// </summary>
    public void MergeIntoShared()
    {
        try
        {
            if (_localEntries.Count > 0)
                _sharedCache.Merge(_localEntries);
        }
        finally
        {
            _localEntries.Clear();
            _localCache.RefreshExpressionIndex();
            _negativeExpressions = null;
            _lastNegativeExpression = null;
        }
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

        switch (PineTreeBinaryEncoding.DecodeRoot(cachedContent))
        {
            case Result<PineTreeBinaryEncoding.DecodeError, PineValue>.Ok decodeOk:
                value = decodeOk.Value;
                break;

            case Result<PineTreeBinaryEncoding.DecodeError, PineValue>.Err decodeErr:
                throw new Exception(
                    "Failed to decode cached value for cache file '" + fileName + "': " + decodeErr.Value);

            default:
                throw new NotImplementedException("Unexpected result type.");
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

        var persisted = Persist(key, value);

        return _memoryCache is null ? persisted : addedToMemory;
    }

    /// <inheritdoc/>
    public bool TryGetResult(EvalCacheEntryKey key, ReturnValueLayout layout, out InvocationResult result)
    {
        if (ReferenceEquals(layout, ReturnValueLayout.Canonical))
        {
            if (TryGet(key, out var canonical))
            {
                result = new(PineValueInProcess.Create(canonical));
                return true;
            }

            result = default;
            return false;
        }

        if (_memoryCache?.TryGetResult(key, layout, out result) is true)
            return true;

        if (BuildPersistentFileName(key) is not { } name ||
            _fileStore.GetFileContent([name + "_return1_" + layout.Hash]) is not { } content)
        {
            result = default;
            return false;
        }

        using var stream = new MemoryStream(content.ToArray(), writable: false);
        using var reader = new BinaryReader(stream);
        var slots = new PineValueInProcess?[layout.SlotCount];
        Array.Fill(slots, PineValueInProcess.EmptyList);

        void Read(ReturnValueLayout.Node node)
        {
            switch (node)
            {
                case ReturnValueLayout.Node.Fixed:
                    break;

                case ReturnValueLayout.Node.Component component:
                    slots[component.Index] = ReadComponent();

                    if (!component.Values.IsEmpty)
                        layout.Slots[component.Index].KeyIndex(slots[component.Index]!);

                    break;

                case ReturnValueLayout.Node.List list:
                    foreach (var item in list.Items)
                        Read(item);

                    break;

                case ReturnValueLayout.Node.Variant variant:
                    slots[variant.Index] = ReadComponent();
                    Read(variant.Cases[layout.Slots[variant.Index].KeyIndex(slots[variant.Index]!)]);
                    break;

                default:
                    throw new NotImplementedException("TryGetResult does not handle return node: " + node.GetType().Name);
            }
        }

        PineValueInProcess ReadComponent()
        {
            var size = reader.ReadInt32();

            if (size < 0 || size > stream.Length - stream.Position)
                throw new FormatException("Invalid persisted return component length.");

            var encoded = reader.ReadBytes(size);

            return
                PineValueInProcess.Create(
                    PineTreeBinaryEncoding.DecodeRoot(encoded)
                    .Extract(error => throw new FormatException("Invalid persisted return component: " + error)));
        }

        Read(layout.Recipe);

        if (stream.Position != stream.Length)
            throw new FormatException("Unexpected bytes following persisted return components.");

        result = new(layout, slots);
        return true;
    }

    /// <inheritdoc/>
    public bool TryAddResult(EvalCacheEntryKey key, in InvocationResult result)
    {
        if (ReferenceEquals(result.Layout, ReturnValueLayout.Canonical))
            return TryAdd(key, result.Materialize().Evaluate());

        if (BuildPersistentFileName(key) is not { } name)
            return _memoryCache?.TryAddResult(key, result) ?? false;

        using var stream = new MemoryStream();
        using var writer = new BinaryWriter(stream);
        var borrowed = result;

        void WriteComponent(int index)
        {
            var start = stream.Position;
            writer.Write(0);
            PineTreeBinaryEncoding.Encode(stream, borrowed.Component(index).Evaluate());
            var end = stream.Position;
            stream.Position = start;
            writer.Write(checked((int)(end - start - sizeof(int))));
            stream.Position = end;
        }

        void Write(ReturnValueLayout.Node node)
        {
            switch (node)
            {
                case ReturnValueLayout.Node.Fixed:
                    break;

                case ReturnValueLayout.Node.Component component:
                    WriteComponent(component.Index);
                    break;

                case ReturnValueLayout.Node.List list:
                    foreach (var item in list.Items)
                        Write(item);

                    break;

                case ReturnValueLayout.Node.Variant variant:
                    WriteComponent(variant.Index);
                    Write(variant.Cases[borrowed.Layout.Slots[variant.Index].KeyIndex(borrowed.Component(variant.Index))]);
                    break;

                default:
                    throw new NotImplementedException("TryAddResult does not handle return node: " + node.GetType().Name);
            }
        }

        Write(result.Layout.Recipe);
        _fileStore.SetFileContent([name + "_return1_" + result.Layout.Hash], stream.ToArray());
        return true;
    }

    /// <inheritdoc/>
    public void Merge(IReadOnlyDictionary<EvalCacheEntryKey, PineValue> entries)
    {
        _memoryCache?.Merge(entries);

        foreach (var entry in entries)
            Persist(entry.Key, entry.Value);
    }

    private bool Persist(EvalCacheEntryKey key, PineValue value)
    {
        if (BuildPersistentFileName(key) is not { } fileName)
            return false;

        using var stream = new MemoryStream();

        PineTreeBinaryEncoding.Encode(stream, value);

        _fileStore.SetFileContent(
            path: [fileName],
            fileContent: stream.ToArray());

        return true;
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

        if (config?.PersistentCachePredicate is not { } predicate)
            return null;

        var parameters = key.StackFrameInput.Parameters;
        var arguments = key.StackFrameInput.EvaluatedArguments;

        if (key.StackFrameInput.EnvironmentConstraint is not null && predicate.Factors.Count > 0)
        {
            var knownInput = key.StackFrameInput.ToValueClass();

            parameters =
                StaticFunctionInterface.FromPathsSorted([.. predicate.Factors.Select(factor => factor.ParamPath)]);

            var predicateArguments = new PineValue[parameters.ParamsPaths.Count];

            for (var index = 0; index < predicateArguments.Length; index++)
            {
                // Removed inputs cannot always be reconstructed for size-based cache policies.
                if (knownInput.TryGetValue(parameters.ParamsPaths[index]) is not { } value)
                    return null;

                predicateArguments[index] = value;
            }

            arguments = predicateArguments;
        }

        if (!predicate.SatisfiedBy(parameters, arguments))
            return null;

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
    /// <summary>Reads a result using the selected return layout when supported.</summary>
    bool TryGetResult(EvalCacheEntryKey key, ReturnValueLayout layout, out InvocationResult result)
    {
        if (TryGet(key, out var value))
        {
            result = new(PineValueInProcess.Create(value));
            return true;
        }

        result = default;
        return false;
    }

    /// <summary>
    /// Consumes a borrowed return-slot view synchronously. Legacy caches request its canonical value.
    /// </summary>
    bool TryAddResult(EvalCacheEntryKey key, in InvocationResult result) =>
        TryAdd(key, result.Materialize().Evaluate());

    /// <summary>
    /// Optional publication version for caching negative expression-membership answers.
    /// Every change that can make a previous negative positive must advance this version
    /// after membership is visible and before publication returns, including partial publication
    /// before an exception. Two equal version reads spanning a miss permit retaining that miss.
    /// Null means readers must probe directly; adapters with independently mutable membership
    /// sources must not simply forward another cache's version.
    /// </summary>
    long? ExpressionMembershipVersion => null;

    /// <summary>
    /// Checks expression admission before the VM materializes the cache key or return value.
    /// Numeric work thresholds still apply after this check. Existing caches admit all expressions.
    /// </summary>
    bool ShouldAdmitExpression(PineValue expressionValue) => true;

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

    /// <summary>
    /// Publishes a stable request-owned batch using first-writer-wins insertion.
    /// Shared dictionary implementations override this to take their lock once for the whole batch.
    /// The caller must not modify the batch until this method returns.
    /// </summary>
    void Merge(IReadOnlyDictionary<EvalCacheEntryKey, PineValue> entries)
    {
        foreach (var entry in entries)
            TryAdd(entry.Key, entry.Value);
    }
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

    internal void RefreshExpressionIndex()
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
            frameCostThreshold: 1_000,
            stackFrameCost: 100,
            entrySpacingCostThreshold: 1_000,
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
