using AwesomeAssertions;
using Pine.Core.CommonEncodings;
using Pine.Core.Interpreter.IntermediateVM;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Threading;
using Xunit;

namespace Pine.Core.Tests.Interpreter.IntermediateVM;

public class InvocationCachePolicyLearnerTests
{
    [Fact]
    public void Cache_policy_has_set_value_equality_and_hash_identity()
    {
        var first = PineValue.Blob([1, 2]);
        var second = PineValue.List([PineValue.Blob([3])]);
        var policy = new InvocationCachePolicy([first, second, first]);
        var reordered = new InvocationCachePolicy([second, PineValue.Blob([1, 2])]);

        policy.Equals(reordered).Should().BeTrue();
        (policy == reordered).Should().BeTrue();
        policy.GetHashCode().Should().Be(reordered.GetHashCode());
        policy.Id.Should().Be(reordered.Id);
        policy.Expressions.Should().HaveCount(2);

        new Dictionary<InvocationCachePolicy, string> { [policy] = "found" }[reordered]
            .Should().Be("found");

        (policy == InvocationCachePolicy.Empty).Should().BeFalse();
        (policy == null).Should().BeFalse();
        new InvocationCachePolicy([]).Should().Be(InvocationCachePolicy.Empty);
        var source = new List<PineValue> { first };
        var copied = new InvocationCachePolicy(source);
        source.Add(second);
        copied.Expressions.Should().ContainSingle().Which.Should().Be(first);
    }

    [Fact]
    public void Cache_policy_controls_both_lookup_and_admission_without_clearing_storage()
    {
        var shared = new ConcurrentInvocationCache();
        var key = GenerationalInvocationCacheTests.BuildKey(1);
        var admitted = new InvocationCachePolicy([key.ExprValue]).Apply(shared);
        var denied = InvocationCachePolicy.Empty.Apply(shared);

        denied.ShouldAdmitExpression(key.ExprValue).Should().BeFalse();
        denied.TryAdd(key, PineValue.EmptyList).Should().BeFalse();
        admitted.ShouldAdmitExpression(key.ExprValue).Should().BeTrue();
        admitted.TryAdd(key, PineValue.EmptyList).Should().BeTrue();
        denied.MayContainExpression(key.ExprValue).Should().BeFalse();
        denied.TryGet(key, out _).Should().BeFalse();
        admitted.TryGet(key, out _).Should().BeTrue();
        shared.Count.Should().Be(1);

        new InvocationCachePolicy([key.ExprValue]).Apply(shared)
            .TryGet(key, out _).Should().BeTrue();
    }

    [Fact]
    public void Policy_switch_is_pinned_per_execution_and_preserves_existing_cache_entries()
    {
        var shared = new ConcurrentInvocationCache();
        var key = GenerationalInvocationCacheTests.BuildKey(1);
        var enabled = new InvocationCachePolicy([key.ExprValue]);
        var policy = enabled;

        var context =
            new InvocationCacheExecutionContext<IInvocationCacheAccess>(
                shared,
                cache => cache,
                getPolicy: () => policy);

        context.Run(
            cache =>
            {
                cache.TryAdd(key, PineValue.EmptyList).Should().BeTrue();
                policy = InvocationCachePolicy.Empty;
                return cache.TryGet(key, out _);
            }).Should().BeTrue();

        context.Run(cache => cache.TryGet(key, out _)).Should().BeFalse();
        shared.Count.Should().Be(1);
        policy = enabled;
        context.Run(cache => cache.TryGet(key, out _)).Should().BeTrue();
    }

    [Fact]
    public void VM_checks_expression_admission_before_offering_a_cache_entry()
    {
        var cache = new NoAdmissionCache();
        var target = Expression.BuiltinInst("reverse", Expression.EnvironmentInstance);
        var report = CreateVM(cache).EvaluateExpression(Invoke(target), PineValue.Blob([1, 2, 3]));
        report.IsOkOrNull().Should().Be(PineValue.Blob([3, 2, 1]));
        cache.AdmissionChecks.Should().BeGreaterThan(0);
    }

    [Fact]
    public void Learner_adds_reused_expressions_and_returns_same_policy_at_a_local_optimum()
    {
        var learner = new InvocationCachePolicyLearner(CreateVM);
        var target = Expression.EnvironmentInstance;

        for (var index = 0; index < 64; ++index)
            target = Expression.BuiltinInst("reverse", target);

        var encoded = ExpressionEncoding.EncodeExpressionAsValue(target);
        var root = Expression.ListInst([Invoke(target)]);

        var requests =
            Enumerable.Repeat(new InvocationCacheLearningRequest(root, PineValue.Blob([1, 2, 3])), 4)
            .ToArray();

        learner.DiscoverExpressions(requests).Should().Contain(encoded);
        learner.Evaluate(new([encoded]), requests).CacheStatistics.Totals.HitCount.Should().Be(3);
        var learned = learner.Learn(InvocationCacheLearningState.Initial, requests);
        learned.Policy.Expressions.Should().Contain(encoded);
        learned.LastRun!.SelectedCost.Should().BeLessThan(learned.LastRun.BaselineCost);
        learned.LastRun.AddedExpressionCount.Should().BeGreaterThan(0);
        learned.RunCount.Should().Be(1);

        var next = learner.Learn(learned, requests);
        next.Policy.Should().BeSameAs(learned.Policy);
        next.LastRun!.AddedExpressionCount.Should().Be(0);
        next.LastRun.RemovedExpressionCount.Should().Be(0);
        next.RunCount.Should().Be(2);

        var uncached = learner.Evaluate(InvocationCachePolicy.Empty, requests);
        var cached = learner.Evaluate(learned.Policy, requests);
        cached.Results.Should().Equal(uncached.Results);
        cached.CacheStatistics.Totals.HitCount.Should().Be(3);
        cached.RetainedEntryCount.Should().BeGreaterThan(0);
        cached.EstimatedCost.Should().BeLessThan(uncached.EstimatedCost);
        learner.Evaluate(learned.Policy, requests).EstimatedCost.Should().Be(cached.EstimatedCost);
    }

    [Fact]
    public void Learner_removes_admitted_expressions_that_do_not_reuse()
    {
        var learner = new InvocationCachePolicyLearner(CreateVM);
        var target = Expression.BuiltinInst("reverse", Expression.EnvironmentInstance);
        var encoded = ExpressionEncoding.EncodeExpressionAsValue(target);
        var policy = new InvocationCachePolicy([encoded]);

        var requests =
            new[]
            {
                new InvocationCacheLearningRequest(Invoke(target), PineValue.Blob([1, 2, 3])),
                new InvocationCacheLearningRequest(Invoke(target), PineValue.Blob([4, 5, 6]))
            };

        var learned = learner.Learn(new(policy, [encoded]), requests);
        learned.Policy.Expressions.Should().NotContain(encoded);
        learned.LastRun!.RemovedExpressionCount.Should().BeGreaterThan(0);
        learned.LastRun.SelectedCost.Should().BeLessThan(learned.LastRun.BaselineCost);
    }

    [Fact]
    public void Learner_does_not_admit_cheap_expressions_just_because_they_repeat()
    {
        var learner = new InvocationCachePolicyLearner(CreateVM);
        var target = Expression.BuiltinInst("reverse", Expression.EnvironmentInstance);

        var requests =
            Enumerable.Repeat(
                new InvocationCacheLearningRequest(Expression.ListInst([Invoke(target)]), PineValue.Blob([1, 2, 3])),
                4).ToArray();

        var learned = learner.Learn(InvocationCacheLearningState.Initial, requests);
        learned.Policy.Should().BeSameAs(InvocationCachePolicy.Empty);
    }

    [Fact]
    public void Learner_can_remove_a_child_shadowed_by_a_cached_parent()
    {
        var learner = new InvocationCachePolicyLearner(CreateVM);
        var child = Expression.BuiltinInst("reverse", Expression.EnvironmentInstance);
        var parent = Expression.ListInst([Invoke(child), Expression.EnvironmentInstance]);
        var childValue = ExpressionEncoding.EncodeExpressionAsValue(child);
        var parentValue = ExpressionEncoding.EncodeExpressionAsValue(parent);
        var policy = new InvocationCachePolicy([parentValue, childValue]);

        var requests =
            Enumerable.Repeat(
                new InvocationCacheLearningRequest(Expression.ListInst([Invoke(parent)]), PineValue.Blob([1, 2, 3])),
                4).ToArray();

        var learned = learner.Learn(new(policy, [parentValue, childValue]), requests);

        learned.Policy.Expressions.Should().Contain(parentValue);
        learned.Policy.Expressions.Should().NotContain(childValue);
        learned.LastRun!.SelectedCost.Should().BeLessThan(learned.LastRun.BaselineCost);

        learner.Evaluate(learned.Policy, requests).Results
            .Should().Equal(learner.Evaluate(policy, requests).Results);
    }

    [Fact]
    public void Learner_retains_search_cursor_and_progresses_across_bounded_runs()
    {
        var learner =
            new InvocationCachePolicyLearner(
                CreateVM,
                new() { MaximumCandidateEvaluations = 1 });

        var unused =
            Enumerable.Range(0, 3)
            .Select(index => PineValue.Blob([(byte)index]))
            .OrderBy(InvocationCachePolicy.ExpressionId, StringComparer.Ordinal)
            .ToImmutableArray();

        var state = new InvocationCacheLearningState(new(unused), unused);
        var requests = Array.Empty<InvocationCacheLearningRequest>();

        var next = learner.Learn(state, requests);
        next.Policy.Expressions.Should().HaveCount(2);
        next.NextCandidateIndex.Should().Be(1);
        next.LastRun!.EvaluatedCandidateCount.Should().Be(1);
        next.KnownExpressions.Should().HaveCount(3);
        var last = learner.Learn(learner.Learn(next, requests), requests);
        last.Policy.Should().Be(InvocationCachePolicy.Empty);
    }

    [Fact]
    public void Learner_resumes_same_candidate_when_new_candidates_sort_before_the_cursor()
    {
        var learner =
            new InvocationCachePolicyLearner(
                CreateVM,
                new() { MaximumCandidateEvaluations = 1 });

        var candidates =
            Enumerable.Range(0, 3)
            .Select(index => PineValue.Blob([(byte)index]))
            .OrderBy(InvocationCachePolicy.ExpressionId, StringComparer.Ordinal)
            .ToImmutableArray();

        var state =
            new InvocationCacheLearningState(
                new(candidates),
                [candidates[1], candidates[2]],
                NextCandidateIndex: 1);

        var next = learner.Learn(state, []);

        next.Policy.Expressions.Should().NotContain(candidates[2]);
        next.Policy.Expressions.Should().Contain(candidates[0]);
        next.Policy.Expressions.Should().Contain(candidates[1]);
        next.NextCandidateIndex.Should().Be(0);
    }

    [Fact]
    public void Learner_cancellation_and_invalid_evaluation_do_not_mutate_previous_state()
    {
        var learner = new InvocationCachePolicyLearner(CreateVM);
        var previous = InvocationCacheLearningState.Initial;
        using var cancellation = new CancellationTokenSource();
        cancellation.Cancel();
        var cancelled = () => learner.Learn(previous, [], cancellation.Token);
        cancelled.Should().Throw<OperationCanceledException>();

        var invalid =
            () => learner.Learn(
                previous,
                [
                    new(
                        new Expression.Eval(Expression.LitralInst(PineValue.Blob([1])), Expression.EnvironmentInstance),
                        PineValue.EmptyList)
                ]);

        invalid.Should().Throw<InvalidOperationException>();
        previous.Policy.Should().BeSameAs(InvocationCachePolicy.Empty);
        previous.RunCount.Should().Be(0);
    }

    internal static Expression Invoke(Expression target) =>
        new Expression.Eval(
            Expression.LitralInst(ExpressionEncoding.EncodeExpressionAsValue(target)),
            Expression.EnvironmentInstance);

    internal static Core.Interpreter.IntermediateVM.PineVM CreateVM(IInvocationCacheAccess cache) =>
        Core.Interpreter.IntermediateVM.PineVM.CreateCustom(
            evalCache: null,
            evaluationConfigDefault: null,
            reportFunctionApplication: null,
            compilationEnvClasses: null,
            disableReductionInCompilation: true,
            selectPrecompiled: null,
            skipInlineForExpression: _ => true,
            enableTailRecursionOptimization: false,
            parseCache: null,
            precompiledLeaves: null,
            reportEnterPrecompiledLeaf: null,
            reportExitPrecompiledLeaf: null,
            optimizationParametersSerial: null,
            cacheFileStore: null,
            invocationCache: cache,
            invocationCacheConfiguration: new(0, 0, 0, 0),
            disableDirectInvocation: true,
            disableDirectContinueForSimpleEval: true,
            disableDirectEvalForSimpleTemplate: true);

    private sealed class NoAdmissionCache : IInvocationCacheAccess
    {
        public int AdmissionChecks { get; private set; }

        public bool ShouldAdmitExpression(PineValue expressionValue)
        {
            ++AdmissionChecks;
            return false;
        }

        public bool MayContainExpression(PineValue expressionValue) => false;

        public bool TryGet(EvalCacheEntryKey key, out PineValue value) => throw new NotSupportedException();

        public bool TryAdd(EvalCacheEntryKey key, PineValue value) =>
            throw new InvalidOperationException("VM must not offer entries rejected by expression policy.");
    }
}
