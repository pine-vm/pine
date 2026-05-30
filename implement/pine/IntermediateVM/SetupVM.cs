using Pine.Core;
using Pine.Core.CodeAnalysis;
using Pine.Core.Internal;
using Pine.Core.Interpreter.IntermediateVM;
using Pine.Core.IO;
using Pine.Core.PineVM;
using Pine.PineVM;
using System;
using System.Collections.Generic;
using System.IO;

namespace Pine.IntermediateVM;

public static class SetupVM
{
    private static readonly IFileStore s_cacheFileStoreDefault =
        new FileStoreFromSystemIOFile(Path.Combine(Filesystem.CacheDirectory, "eval"));

    public static IReadOnlyDictionary<PineValue, Func<PineValue, PineValue?>>? PrecompiledLeavesDefault;

    /// <summary>
    /// Aggregate of the precompiled-leaf dictionaries available to this VM:
    /// the per-area entries exposed by
    /// <see cref="Core.IntermediateVM.SetupVM.DefaultPrecompiledLeaves"/>
    /// (kernel-module leaves from the <c>Pine.Core</c> project), plus the
    /// bundle of hot-path .NET short-circuits from
    /// <see cref="Core.Bundle.BundledPineToDotnet"/> (overridable via
    /// <see cref="PrecompiledLeavesDefault"/>).
    /// This is the dictionary used by <see cref="Create"/> when no explicit
    /// <c>precompiledLeaves</c> argument is supplied, so consumers benefit from
    /// every precompiled leaf available in the project rather than only the
    /// per-area leaves.
    /// </summary>
    public static IReadOnlyDictionary<PineValue, Func<PineValue, PineValue?>> DefaultPrecompiledLeaves =>
        BuildAggregatePrecompiledLeaves();

    private static readonly Lazy<IReadOnlyDictionary<PineValue, Func<PineValue, PineValue?>>?> s_embeddedBundledLeaves =
        new(() => Core.Bundle.BundledPineToDotnet.LoadBundledTask.Result?.BuildDictionary());

    private static IReadOnlyDictionary<PineValue, Func<PineValue, PineValue?>>? s_aggregateCacheValue;
    private static IReadOnlyDictionary<PineValue, Func<PineValue, PineValue?>>? s_aggregateCacheBundleSource;

    private static IReadOnlyDictionary<PineValue, Func<PineValue, PineValue?>> BuildAggregatePrecompiledLeaves()
    {
        var coreLeaves = Core.IntermediateVM.SetupVM.DefaultPrecompiledLeaves;

        // Prefer an explicitly configured bundle, otherwise fall back to the
        // bundle embedded in the Pine.Core assembly.
        var bundledLeaves = PrecompiledLeavesDefault ?? s_embeddedBundledLeaves.Value;

        if (bundledLeaves is null || bundledLeaves.Count is 0)
        {
            return coreLeaves;
        }

        if (s_aggregateCacheValue is null ||
            !ReferenceEquals(s_aggregateCacheBundleSource, bundledLeaves))
        {
            var merged =
                new Dictionary<PineValue, Func<PineValue, PineValue?>>(
                    bundledLeaves.Count + coreLeaves.Count);

            foreach (var entry in bundledLeaves)
            {
                merged[entry.Key] = entry.Value;
            }

            // The per-area leaves from Pine.Core take precedence for shared keys.
            foreach (var entry in coreLeaves)
            {
                merged[entry.Key] = entry.Value;
            }

            s_aggregateCacheValue = merged;
            s_aggregateCacheBundleSource = bundledLeaves;
        }

        return s_aggregateCacheValue;
    }

    public static Core.Interpreter.IntermediateVM.PineVM Create(
        IDictionary<EvalCacheEntryKey, PineValue>? evalCache = null,
        Core.Interpreter.IntermediateVM.PineVM.EvaluationConfig? evaluationConfigDefault = null,
        Action<EvaluationReport>? reportFunctionApplication = null,
        IReadOnlyDictionary<Expression, IReadOnlyList<PineValueClass>>? compilationEnvClasses = null,
        bool disableReductionInCompilation = false,
        bool disablePrecompiled = false,
        bool enableTailRecursionOptimization = false,
        PineVMParseCache? parseCache = null,
        IReadOnlyDictionary<PineValue, Func<PineValue, PineValue?>>? precompiledLeaves = null,
        Action<PineValue, PineValue>? reportEnterPrecompiledLeaf = null,
        Action<PineValue, PineValue, PineValue?>? reportExitPrecompiledLeaf = null,
        OptimizationParametersSerial? optimizationParametersSerial = null,
        IFileStore? cacheFileStore = null)
    {
        Func<Expression, PineValueInProcess, PineVMParseCache, Func<PrecompiledResult>?>? selectPrecompiled =
            disablePrecompiled
            ?
            null
            :
            Precompiled.SelectPrecompiled;

        Func<Expression, bool> skipInlineForExpression =
            disablePrecompiled
            ?
            _ => false
            :
            Precompiled.HasPrecompiledForExpression;

        precompiledLeaves ??= DefaultPrecompiledLeaves;

        cacheFileStore ??= s_cacheFileStoreDefault;

        return
            Core.Interpreter.IntermediateVM.PineVM.CreateCustom(
                evalCache: evalCache,
                evaluationConfigDefault: evaluationConfigDefault,
                reportFunctionApplication: reportFunctionApplication,
                compilationEnvClasses: compilationEnvClasses,
                disableReductionInCompilation: disableReductionInCompilation,
                selectPrecompiled: selectPrecompiled,
                skipInlineForExpression: skipInlineForExpression,
                enableTailRecursionOptimization: enableTailRecursionOptimization,
                parseCache: parseCache,
                precompiledLeaves: precompiledLeaves,
                reportEnterPrecompiledLeaf: reportEnterPrecompiledLeaf,
                reportExitPrecompiledLeaf: reportExitPrecompiledLeaf,
                optimizationParametersSerial: optimizationParametersSerial,
                cacheFileStore: cacheFileStore);
    }
}
