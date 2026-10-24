using Pine.Core.Functional;
using Pine.Core.PineLanguage;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Diagnostics;
using System.Linq;
using System.Threading;

namespace Pine.Core.Runtime.IntermediateVM;

/// <summary>An independent root evaluation used as one element of an ordered training sequence.</summary>
public sealed record InvocationCacheLearningRequest(Expression Expression, PineValue Environment);

/// <summary>
/// State carried between learning runs. Candidate discovery and the search cursor survive runs;
/// measurements are always recomputed for the current input sequence, never reused on a new workload.
/// </summary>
public sealed record InvocationCacheLearningState(
    InvocationCachePolicy Policy,
    ImmutableArray<PineValue> KnownExpressions,
    int NextCandidateIndex = 0,
    long RunCount = 0)
{
    /// <summary>Starts learning without any admitted expressions or previous observations.</summary>
    public static InvocationCacheLearningState Initial { get; } = new(InvocationCachePolicy.Empty, []);

    /// <summary>Evidence for the latest run, separate from the executable policy's value identity.</summary>
    public InvocationCacheLearningRun? LastRun { get; init; }
}

/// <summary>Comparable measurements for one complete ordered replay with initially empty invocation storage.</summary>
public sealed record InvocationCachePolicyEvaluation(
    decimal EstimatedCost,
    EvalWorkCounters Work,
    InvocationCacheStatisticsSnapshot CacheStatistics,
    int RetainedEntryCount,
    ImmutableArray<PineValue> Results);

/// <summary>Evidence for reviewing an incremental search step and policy churn.</summary>
public sealed record InvocationCacheLearningRun(
    decimal BaselineCost,
    decimal SelectedCost,
    int RequestCount,
    int CandidateCount,
    int EvaluatedCandidateCount,
    int AddedExpressionCount,
    int RemovedExpressionCount,
    TimeSpan Elapsed);

/// <summary>
/// Bounded coordinate-search options. Costs are deterministic estimates, not elapsed time or bytes.
/// Cache operation and retained-entry penalties discourage low-value caching and shadowed entries.
/// </summary>
public sealed record InvocationCacheLearningOptions
{
    /// <summary>Maximum add/remove neighbors replayed in one learning run.</summary>
    public int MaximumCandidateEvaluations { get; init; } = 16;

    /// <summary>Maximum discovered candidates retained between runs. Base policy expressions are never discarded.</summary>
    public int MaximumKnownExpressions { get; init; } = 4096;

    /// <summary>Weight of each invocation in addition to executed instructions.</summary>
    public decimal InvocationCost { get; init; } = 100;

    /// <summary>Weight of a full-key cache lookup.</summary>
    public decimal LookupCost { get; init; } = 8;

    /// <summary>Weight of each attempted admission.</summary>
    public decimal AdmissionCost { get; init; } = 16;

    /// <summary>Weight of each retained entry after the sequence.</summary>
    public decimal RetainedEntryCost { get; init; } = 32;

    /// <summary>Weight of deferred template materializations, including cache-induced materialization.</summary>
    public decimal MaterializationCost { get; init; } = 100;

    /// <summary>Per-root evaluation limits and root-result materialization.</summary>
    public PineVM.EvaluationConfig EvaluationConfig { get; init; } =
        PineVM.EvaluationConfig.Default with { MaterializeResult = true };
}

/// <summary>
/// Reusable deterministic replay learner. Every alternative starts with empty invocation storage and
/// replays the same ordered requests, allowing parent cache hits to shadow descendant admissions.
/// A run greedily toggles a bounded number of expressions and accepts only lower estimated cost
/// (or equal cost with fewer expressions). This estimates a local optimum, not a global guarantee.
/// </summary>
public sealed class InvocationCachePolicyLearner
{
    private readonly Func<IInvocationCacheAccess, PineVM> _createVM;

    /// <summary>The validated search and scoring options.</summary>
    public InvocationCacheLearningOptions Options { get; }

    /// <summary>
    /// Supplies VMs configured as in the target workload, using the given invocation cache.
    /// A fresh VM is created per root request. Configure numeric admission thresholds consistently
    /// with production; zero thresholds let the expression policy be the only admission filter.
    /// The factory may share metadata caches, but must not share invocation storage between replays.
    /// </summary>
    public InvocationCachePolicyLearner(
        Func<IInvocationCacheAccess, PineVM> createVM,
        InvocationCacheLearningOptions? options = null)
    {
        ArgumentNullException.ThrowIfNull(createVM);
        _createVM = createVM;
        Options = options ?? new();
        ArgumentOutOfRangeException.ThrowIfLessThan(Options.MaximumCandidateEvaluations, 1);
        ArgumentOutOfRangeException.ThrowIfLessThan(Options.MaximumKnownExpressions, 1);
        ArgumentOutOfRangeException.ThrowIfNegative(Options.InvocationCost);
        ArgumentOutOfRangeException.ThrowIfNegative(Options.LookupCost);
        ArgumentOutOfRangeException.ThrowIfNegative(Options.AdmissionCost);
        ArgumentOutOfRangeException.ThrowIfNegative(Options.RetainedEntryCost);
        ArgumentOutOfRangeException.ThrowIfNegative(Options.MaterializationCost);
        ArgumentNullException.ThrowIfNull(Options.EvaluationConfig);
    }

    /// <summary>
    /// Performs one bounded learning run from a previous policy. The original policy instance is
    /// returned when no better neighbor is found. Cancellation or evaluation failure publishes no state.
    /// </summary>
    public InvocationCacheLearningState Learn(
        InvocationCacheLearningState previous,
        IReadOnlyList<InvocationCacheLearningRequest> requests,
        CancellationToken cancellationToken = default)
    {
        ArgumentNullException.ThrowIfNull(previous);
        ArgumentNullException.ThrowIfNull(previous.Policy);
        ArgumentNullException.ThrowIfNull(requests);
        cancellationToken.ThrowIfCancellationRequested();
        var clock = Stopwatch.StartNew();
        var discovered = DiscoverExpressions(requests, cancellationToken);

        var candidates =
            previous.Policy.Expressions
            .Union(previous.KnownExpressions.IsDefault ? [] : previous.KnownExpressions)
            .Union(discovered)
            .OrderBy(InvocationCachePolicy.ExpressionId, StringComparer.Ordinal)
            .ToImmutableArray();

        // Always preserve removal candidates, even if a caller's base exceeds the discovery limit.
        var known =
            candidates.Where(previous.Policy.Expressions.Contains)
            .Concat(
                candidates.Where(expression => !previous.Policy.Expressions.Contains(expression))
                .Take(Math.Max(0, Options.MaximumKnownExpressions - previous.Policy.Expressions.Count)))
            .OrderBy(InvocationCachePolicy.ExpressionId, StringComparer.Ordinal)
            .ToImmutableArray();

        var policy = previous.Policy;
        var baseline = Evaluate(policy, requests, cancellationToken);
        var selected = baseline;
        var count = Math.Min(Options.MaximumCandidateEvaluations, known.Length);
        var start = known.Length is 0 ? 0 : (int)((uint)previous.NextCandidateIndex % (uint)known.Length);

        // Discovery can insert candidates before the cursor, so resume by expression identity.
        if (!previous.KnownExpressions.IsDefaultOrEmpty && known.Length > 0)
        {
            var previousIndex = (int)((uint)previous.NextCandidateIndex % (uint)previous.KnownExpressions.Length);
            var resumedIndex = known.IndexOf(previous.KnownExpressions[previousIndex]);

            if (resumedIndex >= 0)
                start = resumedIndex;
        }

        for (var index = 0; index < count; ++index)
        {
            cancellationToken.ThrowIfCancellationRequested();
            var expression = known[(start + index) % known.Length];

            var nextPolicy =
                new InvocationCachePolicy(
                    policy.Expressions.Contains(expression)
                    ?
                    policy.Expressions.Remove(expression)
                    :
                    policy.Expressions.Add(expression));

            var evaluation = Evaluate(nextPolicy, requests, cancellationToken);

            if (!evaluation.Results.SequenceEqual(baseline.Results))
                throw new InvalidOperationException("Invocation-cache policy changed evaluation results.");

            if (evaluation.EstimatedCost < selected.EstimatedCost ||
                evaluation.EstimatedCost == selected.EstimatedCost &&
                nextPolicy.Expressions.Count < policy.Expressions.Count)
            {
                policy = nextPolicy;
                selected = evaluation;
            }
        }

        cancellationToken.ThrowIfCancellationRequested();

        return
            new(
                policy,
                known,
                known.Length is 0 ? 0 : (start + count) % known.Length,
                previous.RunCount + 1)
            {
                LastRun =
                new(
                    baseline.EstimatedCost,
                    selected.EstimatedCost,
                    requests.Count,
                    known.Length,
                    count,
                    policy.Expressions.Except(previous.Policy.Expressions).Count(),
                    previous.Policy.Expressions.Except(policy.Expressions).Count(),
                    clock.Elapsed)
            };
    }

    /// <summary>Discovers eligible frame expressions with admission disabled, so no cache hit hides descendants.</summary>
    public ImmutableArray<PineValue> DiscoverExpressions(
        IReadOnlyList<InvocationCacheLearningRequest> requests,
        CancellationToken cancellationToken = default)
    {
        ArgumentNullException.ThrowIfNull(requests);
        var expressions = new HashSet<PineValue>();
        Replay(requests, new DiscoveryCache(expressions, Options.MaximumKnownExpressions), cancellationToken);
        return [.. expressions.OrderBy(InvocationCachePolicy.ExpressionId, StringComparer.Ordinal)];
    }

    /// <summary>
    /// Measures one policy independently. Cache entries are shared across the ordered requests
    /// within this replay only. Results and counters are available for automated comparisons.
    /// </summary>
    public InvocationCachePolicyEvaluation Evaluate(
        InvocationCachePolicy policy,
        IReadOnlyList<InvocationCacheLearningRequest> requests,
        CancellationToken cancellationToken = default)
    {
        ArgumentNullException.ThrowIfNull(policy);
        ArgumentNullException.ThrowIfNull(requests);
        var entries = new InvocationCache();
        var statistics = new InvocationCacheStatistics();
        var cache = statistics.Observe(policy.Apply(new InvocationCacheAccessFromDictionary(entries)));
        var (work, results) = Replay(requests, cache, cancellationToken);
        var snapshot = statistics.GetSnapshot();
        return new(EstimateCost(work, snapshot.Totals, entries.Count), work, snapshot, entries.Count, results);
    }

    /// <summary>Scores replay measurements using this learner's public cost model.</summary>
    public decimal EstimateCost(
        EvalWorkCounters work,
        InvocationCacheExpressionStatistics cache,
        int retainedEntries) =>
        work.InstructionCount +
        work.InvocationCount * Options.InvocationCost +
        work.DirectInterpreterExpressionCount +
        work.BuildListItemCount +
        work.DeferredTemplateValueMaterializationCount * Options.MaterializationCost +
        cache.LookupCount * Options.LookupCost +
        cache.AdmissionAttemptCount * Options.AdmissionCost +
        retainedEntries * Options.RetainedEntryCost;

    private (EvalWorkCounters Work, ImmutableArray<PineValue> Results) Replay(
        IReadOnlyList<InvocationCacheLearningRequest> requests,
        IInvocationCacheAccess cache,
        CancellationToken cancellationToken)
    {
        var work = default(EvalWorkCounters);
        var results = ImmutableArray.CreateBuilder<PineValue>(requests.Count);

        foreach (var request in requests)
        {
            cancellationToken.ThrowIfCancellationRequested();
            ArgumentNullException.ThrowIfNull(request);

            var report =
                _createVM(cache).EvaluateExpressionOnCustomStack(
                    request.Expression,
                    request.Environment,
                    Options.EvaluationConfig with { MaterializeResult = true },
                    cancellationToken);

            if (report.IsOkOrNull() is not { } success)
            {
                cancellationToken.ThrowIfCancellationRequested();
                throw new InvalidOperationException("Training evaluation failed: " + report);
            }

            work = EvalWorkCounters.Add(work, success.Counters);
            results.Add(success.ReturnValue.Evaluate());
        }

        return (work, results.MoveToImmutable());
    }

    private sealed class DiscoveryCache(HashSet<PineValue> expressions, int limit) : IInvocationCacheAccess
    {
        private void Observe(PineValue expression)
        {
            if (expressions.Count < limit)
                expressions.Add(expression);
        }

        public bool ShouldAdmitExpression(PineValue expressionValue)
        {
            Observe(expressionValue);
            return false;
        }

        public bool MayContainExpression(PineValue expressionValue)
        {
            Observe(expressionValue);
            return false;
        }

        public bool TryGet(EvalCacheEntryKey key, out PineValue value)
        {
            value = null!;
            return false;
        }

        public bool TryAdd(EvalCacheEntryKey key, PineValue value) => false;
    }
}
