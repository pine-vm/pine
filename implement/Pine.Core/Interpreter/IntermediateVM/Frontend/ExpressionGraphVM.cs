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
    /// Runtime caches and interop frames belong to the VM, not to the pure compiler memo.
    /// </summary>
    public static PineVM Create(
        PreparationOptions? preparationOptions = null,
        PineVM.EvaluationConfig? evaluationConfig = null)
    {
        var options = preparationOptions ?? new(DisableReduction: true);
        return PineVM.CreateCustom(
            evalCache: null, evaluationConfigDefault: evaluationConfig, reportFunctionApplication: null,
            compilationEnvClasses: null, disableReductionInCompilation: true, selectPrecompiled: null,
            skipInlineForExpression: _ => true, enableTailRecursionOptimization: false, parseCache: null,
            precompiledLeaves: null, reportEnterPrecompiledLeaf: null, reportExitPrecompiledLeaf: null,
            optimizationParametersSerial: null, cacheFileStore: null,
            disableDirectContinueForSimpleEval: true, disableDirectEvalForSimpleTemplate: true,
            compileExpression: expression => Compile(expression, options));
    }

    private static ExpressionCompilation Compile(Expression expression, PreparationOptions options)
    {
        var preparation = FunctionPreparation.PrepareFunction(
            CompilationRequest.Capture(expression, options), CompilerMemo.Empty);
        var graph = ExpressionGraphCompiler.CompileExpressionToGraph(preparation.Function, preparation.Memo)
            .Graph.Extract(errors => throw new InvalidOperationException(string.Join(", ", errors)));
        var function = GraphCompiler.Compile(graph)
            .Extract(error => throw new InvalidOperationException(error.ToString()));
        return new(GraphVMAdapter.ToStackFrame(function), []);
    }
}
