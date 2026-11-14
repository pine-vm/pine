using Pine.Core.Runtime.IntermediateVM;

namespace Pine.Elm019;

/// <summary>
/// Caches shared across compatible ECMAScript compilations without sharing mutable VM execution state.
/// </summary>
public sealed class ElmMakeCompilationCache(IInvocationCacheAccess? invocationCache = null)
{
    public IInvocationCacheAccess InvocationCache { get; } = invocationCache ?? new ConcurrentInvocationCache();

    public PineVMSharedCaches VmCaches { get; } = new();

    internal Core.Runtime.IntermediateVM.PineVM CreateVM(
        Core.Runtime.IntermediateVM.PineVM.EvaluationConfig evaluationConfig) =>
        IntermediateVM.SetupVM.Create(
            invocationCache: InvocationCache,
            evaluationConfigDefault: evaluationConfig,
            parseCache: VmCaches.ParsedExpressions,
            tryGetExpressionCompilation: VmCaches.ExpressionCompilations.TryGet,
            getOrAddExpressionCompilation: VmCaches.ExpressionCompilations.GetOrAdd,
            expressionEncodingCache: VmCaches.EncodedExpressions,
            reducedExpressionCache: VmCaches.ReducedExpressions,
            expressionTemplatePlanCache: VmCaches.ExpressionTemplatePlans);
}
