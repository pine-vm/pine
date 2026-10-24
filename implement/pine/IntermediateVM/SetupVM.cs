using Pine.Core.CodeAnalysis;
using Pine.Core.Internal;
using Pine.Core.IO;
using Pine.Core.PineLanguage;
using Pine.Core.PineVM;
using Pine.Core.Runtime;
using Pine.Core.Runtime.IntermediateVM;
using System;
using System.Collections.Generic;
using System.IO;

namespace Pine.IntermediateVM;

public static class SetupVM
{
    private static readonly IFileStore s_cacheFileStoreDefault =
        new FileStoreFromSystemIOFile(Path.Combine(Filesystem.CacheDirectory, "eval"));

    public static IReadOnlyDictionary<PineValue, PrecompiledLeaf>? PrecompiledLeavesDefault;

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
    public static IReadOnlyDictionary<PineValue, PrecompiledLeaf> DefaultPrecompiledLeaves =>
        BuildAggregatePrecompiledLeaves();

    private static readonly Lazy<IReadOnlyDictionary<PineValue, PrecompiledLeaf>?> s_embeddedBundledLeaves =
        new(() => Core.Bundle.BundledPineToDotnet.LoadBundledTask.Result?.BuildDictionary());

    private static IReadOnlyDictionary<PineValue, PrecompiledLeaf>? s_aggregateCacheValue;

    private static IReadOnlyDictionary<PineValue, PrecompiledLeaf>? s_aggregateCacheBundleSource;

    private static IReadOnlyDictionary<PineValue, PrecompiledLeaf> BuildAggregatePrecompiledLeaves()
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
                new Dictionary<PineValue, PrecompiledLeaf>(
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

    /// <summary>
    /// Creates a VM with independently configurable caches. An omitted template-plan cache is created per VM.
    /// The template-plan cache can be shared without sharing mutable evaluation state.
    /// </summary>
    public static Core.Runtime.IntermediateVM.PineVM Create(
        IDictionary<EvalCacheEntryKey, PineValue>? evalCache = null,
        Core.Runtime.IntermediateVM.PineVM.EvaluationConfig? evaluationConfigDefault = null,
        Action<EvaluationReport>? reportFunctionApplication = null,
        IReadOnlyDictionary<Expression, IReadOnlyList<PineValueClass>>? compilationEnvClasses = null,
        bool disableReductionInCompilation = false,
        bool enableTailRecursionOptimization = true,
        PineVMParseCache? parseCache = null,
        IReadOnlyDictionary<PineValue, PrecompiledLeaf>? precompiledLeaves = null,
        Action<PineValue, PineValueInProcess>? reportEnterPrecompiledLeaf = null,
        Action<PineValue, PineValueInProcess, PineValueInProcess?>? reportExitPrecompiledLeaf = null,
        OptimizationParametersSerial? optimizationParametersSerial = null,
        IFileStore? cacheFileStore = null,
        IInvocationCacheAccess? invocationCache = null,
        InvocationCacheConfiguration? invocationCacheConfiguration = null,
        TryGetExpressionCompilation? tryGetExpressionCompilation = null,
        GetOrAddExpressionCompilation? getOrAddExpressionCompilation = null,
        PineVMExpressionEncodingCache? expressionEncodingCache = null,
        IDictionary<(Expression, ReductionConfig), Expression>? reducedExpressionCache = null,
        PineVMExpressionTemplatePlanCache? expressionTemplatePlanCache = null) =>
        CreateWithTemplatePlanCache(
            expressionTemplatePlanCache ?? new PineVMExpressionTemplatePlanCache(),
            evaluationConfigDefault,
            evalCache,
            reportFunctionApplication,
            compilationEnvClasses,
            disableReductionInCompilation,
            enableTailRecursionOptimization,
            parseCache,
            precompiledLeaves,
            reportEnterPrecompiledLeaf,
            reportExitPrecompiledLeaf,
            optimizationParametersSerial,
            cacheFileStore,
            invocationCache,
            invocationCacheConfiguration,
            tryGetExpressionCompilation,
            getOrAddExpressionCompilation,
            expressionEncodingCache,
            reducedExpressionCache);

    public static Core.Runtime.IntermediateVM.PineVM CreateWithTemplatePlanCache(
        PineVMExpressionTemplatePlanCache expressionTemplatePlanCache,
        Core.Runtime.IntermediateVM.PineVM.EvaluationConfig? evaluationConfigDefault,
        IDictionary<EvalCacheEntryKey, PineValue>? evalCache = null,
        Action<EvaluationReport>? reportFunctionApplication = null,
        IReadOnlyDictionary<Expression, IReadOnlyList<PineValueClass>>? compilationEnvClasses = null,
        bool disableReductionInCompilation = false,
        bool enableTailRecursionOptimization = true,
        PineVMParseCache? parseCache = null,
        IReadOnlyDictionary<PineValue, PrecompiledLeaf>? precompiledLeaves = null,
        Action<PineValue, PineValueInProcess>? reportEnterPrecompiledLeaf = null,
        Action<PineValue, PineValueInProcess, PineValueInProcess?>? reportExitPrecompiledLeaf = null,
        OptimizationParametersSerial? optimizationParametersSerial = null,
        IFileStore? cacheFileStore = null,
        IInvocationCacheAccess? invocationCache = null,
        InvocationCacheConfiguration? invocationCacheConfiguration = null,
        TryGetExpressionCompilation? tryGetExpressionCompilation = null,
        GetOrAddExpressionCompilation? getOrAddExpressionCompilation = null,
        PineVMExpressionEncodingCache? expressionEncodingCache = null,
        IDictionary<(Expression, ReductionConfig), Expression>? reducedExpressionCache = null)
    {
        precompiledLeaves ??= DefaultPrecompiledLeaves;

        cacheFileStore ??= s_cacheFileStoreDefault;

        return
            Core.Runtime.IntermediateVM.PineVM.CreateCustom(
                expressionTemplatePlanCache: expressionTemplatePlanCache,
                evalCache: evalCache,
                evaluationConfigDefault: evaluationConfigDefault,
                reportFunctionApplication: reportFunctionApplication,
                compilationEnvClasses: compilationEnvClasses,
                disableReductionInCompilation: disableReductionInCompilation,
                selectPrecompiled: null,
                skipInlineForExpression: _ => false,
                enableTailRecursionOptimization: enableTailRecursionOptimization,
                parseCache: parseCache,
                precompiledLeaves: precompiledLeaves,
                reportEnterPrecompiledLeaf: reportEnterPrecompiledLeaf,
                reportExitPrecompiledLeaf: reportExitPrecompiledLeaf,
                optimizationParametersSerial: optimizationParametersSerial,
                cacheFileStore: cacheFileStore,
                invocationCache: invocationCache,
                invocationCacheConfiguration: invocationCacheConfiguration,
                tryGetExpressionCompilation: tryGetExpressionCompilation,
                getOrAddExpressionCompilation: getOrAddExpressionCompilation,
                expressionEncodingCache: expressionEncodingCache,
                reducedExpressionCache: reducedExpressionCache);
    }
}
