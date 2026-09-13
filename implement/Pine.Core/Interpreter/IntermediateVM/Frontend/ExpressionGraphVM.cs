using Pine.Core.Interpreter.IntermediateVM.Backend;
using System;

namespace Pine.Core.Interpreter.IntermediateVM.Frontend;

/// <summary>
/// Opt-in runtime boundary for the graph frontend. The ordinary VM factory still selects the legacy
/// compiler. Every dynamically parsed callee uses this same frontend, never instruction-first IR.
/// </summary>
public static class ExpressionGraphVM
{
    /// <summary>
    /// Creates a VM using canonical graph calls. Preparation rewrites default off while the
    /// graph-first pipeline is opt-in; callers may explicitly select the legacy preparation policy.
    /// Graph optimization also defaults off; pass optimizerOptions to opt in. Runtime caches and
    /// interop frames belong to the VM, not to the pure compiler memo.
    /// The enabled policy includes guarded source-identity loops, scalar replacement across
    /// continuations and backedges, and selection of existing allocation-free kernel instructions.
    /// </summary>
    public static PineVM Create(
        PreparationOptions? preparationOptions = null,
        PineVM.EvaluationConfig? evaluationConfig = null,
        GraphOptimizerOptions? optimizerOptions = null)
    {
        var options = preparationOptions ?? new(DisableReduction: true);
        return PineVM.CreateCustom(
            evalCache: null, evaluationConfigDefault: evaluationConfig, reportFunctionApplication: null,
            compilationEnvClasses: null, disableReductionInCompilation: true, selectPrecompiled: null,
            skipInlineForExpression: _ => true, enableTailRecursionOptimization: false, parseCache: null,
            precompiledLeaves: null, reportEnterPrecompiledLeaf: null, reportExitPrecompiledLeaf: null,
            optimizationParametersSerial: null, cacheFileStore: null,
            disableDirectContinueForSimpleEval: true, disableDirectEvalForSimpleTemplate: true,
            compileExpression: expression => Compile(expression, options, optimizerOptions ?? new(Enabled: false)));
    }

    private static ExpressionCompilation Compile(
        Expression expression, PreparationOptions options, GraphOptimizerOptions optimizerOptions)
    {
        var optimized = ExpressionGraphOptimizer.Compile(
            CompilationRequest.Capture(expression, options), optimizerOptions, CompilerMemo.Empty)
            .Extract(errors => throw new InvalidOperationException(string.Join(", ", errors)));
        var function = GraphCompiler.Compile(optimized.Graph, fuseScalarBuiltins: optimizerOptions.Enabled && optimizerOptions.ScalarReplacement)
            .Extract(error => throw new InvalidOperationException(error.ToString()));
        return new(GraphVMAdapter.ToStackFrame(function), []);
    }
}
