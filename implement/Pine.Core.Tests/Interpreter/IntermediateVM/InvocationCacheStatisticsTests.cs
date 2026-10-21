using AwesomeAssertions;
using Pine.Core.Interpreter.IntermediateVM;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;
using Xunit;

namespace Pine.Core.Tests.Interpreter.IntermediateVM;

public class InvocationCacheStatisticsTests
{
    [Fact]
    public void Invocation_cache_statistics_preserve_batch_publication_without_counting_admissions()
    {
        var statistics = new InvocationCacheStatistics();
        var shared = new BatchOnlyCache();
        var cache = statistics.Observe(shared);

        var entries =
            new InvocationCache
            {
                [GenerationalInvocationCacheTests.BuildKey(1)] = PineValue.EmptyList
            };

        cache.Merge(entries);
        shared.Published.Should().BeSameAs(entries);
        statistics.GetSnapshot().PerExpression.Should().BeEmpty();
        statistics.GetSnapshot().Totals.AdmissionAttemptCount.Should().Be(0);
    }

    [Fact]
    public void Invocation_cache_statistics_separate_expressions_and_preserve_snapshots()
    {
        var statistics = new InvocationCacheStatistics();
        var cache = statistics.Observe(new ConcurrentInvocationCache());
        var first = GenerationalInvocationCacheTests.BuildKey(1);
        var second = GenerationalInvocationCacheTests.BuildKey(2);
        var sameExpression = first with { StackFrameInput = second.StackFrameInput };

        statistics.GetSnapshot().PerExpression.Should().BeEmpty();
        statistics.GetSnapshot().Totals.HitRate.Should().Be(0);
        cache.MayContainExpression(first.ExprValue).Should().BeFalse();
        cache.TryGet(first, out _).Should().BeFalse();
        cache.TryAdd(first, PineValue.EmptyList).Should().BeTrue();
        cache.TryAdd(first, PineValue.Blob([5])).Should().BeFalse();
        cache.MayContainExpression(first.ExprValue).Should().BeTrue();
        cache.TryGet(first, out var found).Should().BeTrue();
        found.Should().Be(PineValue.EmptyList);
        cache.TryGet(sameExpression, out _).Should().BeFalse();
        cache.MayContainExpression(second.ExprValue).Should().BeFalse();

        var snapshot = statistics.GetSnapshot();
        snapshot.PerExpression[first.ExprValue].Should().Be(new InvocationCacheExpressionStatistics(2, 1, 3, 1, 2, 1));
        snapshot.PerExpression[second.ExprValue].Should().Be(new InvocationCacheExpressionStatistics(1, 1, 0, 0, 0, 0));
        snapshot.Totals.Should().Be(new InvocationCacheExpressionStatistics(3, 2, 3, 1, 2, 1));
        snapshot.Totals.MissCount.Should().Be(2);
        snapshot.Totals.RejectedAdmissionCount.Should().Be(1);
        snapshot.Totals.HitRate.Should().Be(1.0 / 3);

        cache.TryAdd(second, PineValue.EmptyList).Should().BeTrue();
        snapshot.PerExpression[second.ExprValue].AdmissionCount.Should().Be(0);
        statistics.GetSnapshot().Totals.AdmissionCount.Should().Be(2);
    }

    [Fact]
    public async Task Invocation_cache_statistics_count_local_and_shared_hits_once_and_ignore_publication()
    {
        var statistics = new InvocationCacheStatistics();
        var shared = new GenerationalInvocationCache();

        var context =
            new InvocationCacheExecutionContext<IInvocationCacheAccess>(
                shared,
                cache => cache,
                maxBufferedEntries: 1,
                statistics: statistics);

        var first = GenerationalInvocationCacheTests.BuildKey(1);
        var second = GenerationalInvocationCacheTests.BuildKey(2);

        context.Run(
            cache =>
            {
                cache.TryAdd(first, PineValue.EmptyList).Should().BeTrue();
                cache.TryAdd(second, PineValue.EmptyList).Should().BeFalse();
                return cache.TryGet(first, out _);
            }).Should().BeTrue();

        await shared.AdvanceGenerationAsync(2);

        (await context.RunAsync(
            async cache =>
            {
                await Task.Yield();
                return cache.TryGet(first, out _);
            })).Should().BeTrue();

        var snapshot = statistics.GetSnapshot();
        snapshot.Totals.Should().Be(new InvocationCacheExpressionStatistics(0, 0, 2, 2, 2, 1));
        shared.GetStatistics().Promotions.Should().Be(1);
        shared.GetStatistics().Hits.Should().Be(1);
        await shared.AdvanceGenerationAsync(1);
        context.Run(cache => cache.TryGet(first, out _)).Should().BeFalse();
        statistics.GetSnapshot().Totals.AdmissionCount.Should().Be(1);
        statistics.GetSnapshot().Totals.MissCount.Should().Be(1);
    }

    [Fact]
    public void Invocation_cache_statistics_include_failed_execution_and_factory_operations()
    {
        var statistics = new InvocationCacheStatistics();
        var shared = new ConcurrentInvocationCache();
        var key = GenerationalInvocationCacheTests.BuildKey(1);

        var context =
            new InvocationCacheExecutionContext<int>(
                shared,
                cache =>
                {
                    cache.TryAdd(key, PineValue.EmptyList);
                    throw new OperationCanceledException();
                },
                statistics: statistics);

        var run = () => context.Run(_ => 0);
        run.Should().Throw<OperationCanceledException>();
        statistics.GetSnapshot().Totals.AdmissionCount.Should().Be(1);
        shared.TryGet(key, out _).Should().BeTrue();
    }

    [Fact]
    public async Task Invocation_cache_statistics_are_consistent_during_concurrent_workers()
    {
        var statistics = new InvocationCacheStatistics();
        var shared = new ConcurrentInvocationCache();

        var context =
            new InvocationCacheExecutionContext<IInvocationCacheAccess>(
                shared,
                cache => cache,
                statistics: statistics);

        var key = GenerationalInvocationCacheTests.BuildKey(1);
        shared.TryAdd(key, PineValue.EmptyList);

        await Task.WhenAll(
            Enumerable.Range(0, 8).Select(
                workerIndex => Task.Run(
                    () => context.Run(
                        cache =>
                        {
                            for (var index = 0; index < 100; ++index)
                            {
                                cache.MayContainExpression(key.ExprValue).Should().BeTrue();
                                cache.TryGet(key, out _).Should().BeTrue();
                                var current = statistics.GetSnapshot();
                                current.Totals.Should().Be(current.PerExpression[key.ExprValue]);
                                current.Totals.MissCount.Should().Be(0);
                            }

                            return 0;
                        }))));

        statistics.GetSnapshot().Totals.Should().Be(new InvocationCacheExpressionStatistics(800, 0, 800, 800, 0, 0));
    }

    private sealed class BatchOnlyCache : IInvocationCacheAccess
    {
        public IReadOnlyDictionary<EvalCacheEntryKey, PineValue>? Published { get; private set; }

        public bool MayContainExpression(PineValue expressionValue) => throw new NotSupportedException();

        public bool TryGet(EvalCacheEntryKey key, out PineValue value) => throw new NotSupportedException();

        public bool TryAdd(EvalCacheEntryKey key, PineValue value) => throw new NotSupportedException();

        public void Merge(IReadOnlyDictionary<EvalCacheEntryKey, PineValue> entries) => Published = entries;
    }
}
