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

        precompiledLeaves ??=
            PrecompiledLeavesDefault
            ??
            Core.Bundle.BundledPineToDotnet.LoadBundledTask.Result?.BuildDictionary();

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
