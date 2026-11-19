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

    /// <summary>
    /// Handwritten in-process leaves used by <see cref="Create"/> when no explicit
    /// <c>precompiledLeaves</c> argument is supplied.
    /// </summary>
    public static IReadOnlyDictionary<PineValue, PrecompiledLeaf> DefaultPrecompiledLeaves =>
        Core.IntermediateVM.SetupVM.DefaultPrecompiledLeaves;

    /// <summary>
    /// Creates a VM with independently configurable caches. An omitted template-plan cache is created per VM.
    /// The template-plan cache can be shared without sharing mutable evaluation state.
    /// </summary>
    public static Core.Runtime.IntermediateVM.PineVM Create(
        IDictionary<EvalCacheEntryKey, PineValue>? evalCache = null,
        Core.Runtime.IntermediateVM.PineVM.EvaluationConfig? evaluationConfigDefault = null,
        Action<EvaluationReport>? reportFunctionApplication = null,
        IReadOnlyDictionary<Core.Runtime.InternalExpression.Expression, IReadOnlyList<PineValueClass>>? compilationEnvClasses = null,
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
        IDictionary<(Core.Runtime.InternalExpression.Expression, ReductionConfig), Core.Runtime.InternalExpression.Expression>? reducedExpressionCache = null,
        PineVMExpressionTemplatePlanCache? expressionTemplatePlanCache = null,
        ConcurrentExpressionCompilationCache? expressionCompilationCache = null) =>
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
            reducedExpressionCache,
            expressionCompilationCache);

    public static Core.Runtime.IntermediateVM.PineVM CreateWithTemplatePlanCache(
        PineVMExpressionTemplatePlanCache expressionTemplatePlanCache,
        Core.Runtime.IntermediateVM.PineVM.EvaluationConfig? evaluationConfigDefault,
        IDictionary<EvalCacheEntryKey, PineValue>? evalCache = null,
        Action<EvaluationReport>? reportFunctionApplication = null,
        IReadOnlyDictionary<Core.Runtime.InternalExpression.Expression, IReadOnlyList<PineValueClass>>? compilationEnvClasses = null,
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
        IDictionary<(Core.Runtime.InternalExpression.Expression, ReductionConfig), Core.Runtime.InternalExpression.Expression>? reducedExpressionCache = null,
        ConcurrentExpressionCompilationCache? expressionCompilationCache = null)
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
                skipInlineForExpression: null,
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
                expressionCompilationCache: expressionCompilationCache,
                expressionEncodingCache: expressionEncodingCache,
                reducedExpressionCache: reducedExpressionCache);
    }
}
