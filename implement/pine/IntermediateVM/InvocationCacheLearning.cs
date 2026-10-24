using Pine.Core.PineLanguage;
using Pine.Core.Runtime;
using Pine.Core.Runtime.IntermediateVM;
using System;
using System.Collections.Generic;
using System.Threading;
using System.Threading.Tasks;

namespace Pine.IntermediateVM;

internal static class InvocationCacheLearning
{
    internal static ContinuousInvocationCacheLearning Create(
        Core.Runtime.IntermediateVM.PineVM.EvaluationConfig? evaluationConfig = null,
        IReadOnlyDictionary<PineValue, PrecompiledLeaf>? precompiledLeaves = null,
        Action<string>? logDelegate = null,
        ContinuousInvocationCacheLearningOptions? options = null,
        TimeSpan? learningTimeout = null)
    {
        var timeout = learningTimeout ?? TimeSpan.FromMinutes(2);

        if (timeout <= TimeSpan.Zero || timeout.TotalMilliseconds > uint.MaxValue - 1)
            throw new ArgumentOutOfRangeException(nameof(learningTimeout));

        var sharedCaches = new PineVMSharedCaches();

        var learner =
            new InvocationCachePolicyLearner(
                invocationCache =>
                SetupVM.Create(
                    evaluationConfigDefault: evaluationConfig,
                    invocationCache: invocationCache,
                    parseCache: sharedCaches.ParsedExpressions,
                    tryGetExpressionCompilation: sharedCaches.ExpressionCompilations.TryGet,
                    getOrAddExpressionCompilation: sharedCaches.ExpressionCompilations.GetOrAdd,
                    expressionEncodingCache: sharedCaches.EncodedExpressions,
                    reducedExpressionCache: sharedCaches.ReducedExpressions,
                    expressionTemplatePlanCache: sharedCaches.ExpressionTemplatePlans,
                    precompiledLeaves: precompiledLeaves,
                    invocationCacheConfiguration: new(0, 0, 0, 0)),
                options:
                new()
                {
                    MaximumCandidateEvaluations = 4,
                    EvaluationConfig =
                    evaluationConfig ?? Core.Runtime.IntermediateVM.PineVM.EvaluationConfig.Default
                });

        return
            new(
                LearnAsync,
                log: logDelegate,
                options: options ?? new(SampleCapacity: 8, PendingCapacity: 16));

        Task<InvocationCacheLearningState> LearnAsync(
            InvocationCacheLearningState state,
            IReadOnlyList<InvocationCacheLearningRequest> requests,
            CancellationToken cancellationToken)
        {
            using var deadline = CancellationTokenSource.CreateLinkedTokenSource(cancellationToken);
            deadline.CancelAfter(timeout);

            try
            {
                return Task.FromResult(learner.Learn(state, requests, deadline.Token));
            }
            catch (OperationCanceledException exception)
                when (!cancellationToken.IsCancellationRequested && deadline.IsCancellationRequested)
            {
                throw new TimeoutException("Invocation-cache learning exceeded its deadline.", exception);
            }
        }
    }
}
