using AwesomeAssertions;
using Pine.Core.CodeGen.Pine;
using Pine.Core.Encodings;
using Pine.Core.PineLanguage;
using Pine.Core.Runtime.IntermediateVM;
using Pine.Elm.LanguageServerAdapters;
using Pine.IntermediateVM;
using System;
using System.Collections.Concurrent;
using System.Diagnostics;
using System.Globalization;
using System.Linq;
using System.Threading.Tasks;
using Xunit;

namespace Pine.IntegrationTests;

public class InvocationCacheLearningTests
{
    private static readonly TimeSpan s_timeout = TimeSpan.FromSeconds(30);

    [Fact(Timeout = 120_000)]
    public async Task Production_factory_switches_policy_and_reuses_retained_production_entries()
    {
        var body =
            Expression.ListInst(
                [
                    .. Enumerable.Range(0, 64).Select(
                        index =>
                        Expression.BuiltinInst(
                            "int_add",
                            Expression.ListInst(
                                [
                                Expression.EnvironmentInstance,
                                Expression.LitralInst(IntegerEncoding.EncodeSignedInteger(index))
                                ])))
                ]);

        var encodedBody = ExpressionEncoding.EncodeExpressionAsValue(body);

        var root =
            Expression.ListInst(
                [
                new Expression.Eval(
                    ExpressionBuilder.BuildExpressionForPathInExpression([0], Expression.EnvironmentInstance),
                    ExpressionBuilder.BuildExpressionForPathInExpression([1], Expression.EnvironmentInstance))
                ]);

        var environment = PineValue.List([encodedBody, IntegerEncoding.EncodeSignedInteger(42)]);

        var expected =
            PineValue.List(
                [
                PineValue.List(
                    [
                        .. Enumerable.Range(0, 64).Select(
                            index =>
                            IntegerEncoding.EncodeSignedInteger(42 + index))
                    ])
                ]);

        var policySwitch = new TaskCompletionSource<string>(TaskCreationOptions.RunContinuationsAsynchronously);

        await using var controller =
            LanguageServerComposition.CreateCacheLearning(
                logDelegate:
                message =>
                {
                    if (message.StartsWith("Invocation-cache policy switch:", StringComparison.Ordinal))
                        policySwitch.TrySetResult(message);
                },
                options: new(MinimumInterval: TimeSpan.FromMilliseconds(100), SampleCapacity: 8, PendingCapacity: 16));

        var storage = new ConcurrentInvocationCache();
        var statistics = new InvocationCacheStatistics();
        var sharedCaches = new PineVMSharedCaches();

        CreateVM(storage).EvaluateExpression(root, environment)
            .Extract(error => throw new InvalidOperationException(error))
            .Should().Be(expected);

        storage.MayContainExpression(encodedBody).Should().BeTrue();
        var retainedCount = storage.Count;
        retainedCount.Should().BeGreaterThan(0);

        for (var index = 0; index < 8; ++index)
        {
            controller.Wrap(CreateVM(statistics.Observe(controller.CurrentPolicy.Apply(storage))))
                .EvaluateExpression(root, environment)
                .Extract(error => throw new InvalidOperationException(error))
                .Should().Be(expected);
        }

        var message = await policySwitch.Task.WaitAsync(s_timeout);
        message.Should().Contain("baseline-cost=").And.Contain("selected-cost=");
        message.Should().Contain("evidence-added=1").And.Contain("evidence-removed=0");
        Cost("selected-cost").Should().BeLessThan(Cost("baseline-cost"));
        controller.CurrentPolicy.Expressions.Should().Contain(encodedBody);
        controller.GetSnapshot().AcceptedCount.Should().Be(8);
        controller.State.LastRun!.EvaluatedCandidateCount.Should().BeLessThanOrEqualTo(4);
        storage.Count.Should().Be(retainedCount);
        var hitsBeforeReuse = statistics.GetSnapshot().Totals.HitCount;

        for (var index = 0; index < 2; ++index)
        {
            CreateVM(statistics.Observe(controller.CurrentPolicy.Apply(storage)))
                .EvaluateExpression(root, environment)
                .Extract(error => throw new InvalidOperationException(error))
                .Should().Be(expected);
        }

        statistics.GetSnapshot().Totals.HitCount.Should().BeGreaterThan(hitsBeforeReuse);
        storage.Count.Should().Be(retainedCount);

        decimal Cost(string name) =>
            decimal.Parse(
                message.Split(", ").Single(field => field.StartsWith(name + "=", StringComparison.Ordinal))[(name.Length + 1)..],
                CultureInfo.InvariantCulture);

        Core.Runtime.IntermediateVM.PineVM CreateVM(IInvocationCacheAccess cache) =>
            SetupVM.Create(
                invocationCache: cache,
                parseCache: sharedCaches.ParsedExpressions,
                tryGetExpressionCompilation: sharedCaches.ExpressionCompilations.TryGet,
                getOrAddExpressionCompilation: sharedCaches.ExpressionCompilations.GetOrAdd,
                expressionEncodingCache: sharedCaches.EncodedExpressions,
                reducedExpressionCache: sharedCaches.ReducedExpressions,
                expressionTemplatePlanCache: sharedCaches.ExpressionTemplatePlans,
                invocationCacheConfiguration: new(0, 0, 0, 0));
    }

    [Fact(Timeout = 120_000)]
    public async Task Shutdown_cancels_infinite_replay_without_reporting_a_timeout()
    {
        var log = new ConcurrentQueue<string>();

        await using var controller =
            LanguageServerComposition.CreateCacheLearning(
                log.Enqueue,
                options: new(MinimumInterval: TimeSpan.FromMilliseconds(10), SampleCapacity: 2));

        controller.Record(InfiniteRequest()).Should().BeTrue();
        controller.Record(InfiniteRequest()).Should().BeTrue();
        await WaitUntil(() => controller.GetSnapshot().IsLearning);
        await Task.Delay(TimeSpan.FromMilliseconds(100));
        controller.GetSnapshot().IsLearning.Should().BeTrue();

        await controller.DisposeAsync().AsTask().WaitAsync(s_timeout);

        var snapshot = controller.GetSnapshot();
        snapshot.IsDisposed.Should().BeTrue();
        snapshot.IsLearning.Should().BeFalse();
        snapshot.State.Should().BeSameAs(InvocationCacheLearningState.Initial);
        log.Should().NotContain(message => message.Contains("failed", StringComparison.Ordinal));
        controller.Record(InfiniteRequest()).Should().BeFalse();
    }

    [Fact(Timeout = 120_000)]
    public async Task Deadline_stops_infinite_replay_and_later_valid_samples_recover()
    {
        var failed = new TaskCompletionSource<string>(TaskCreationOptions.RunContinuationsAsynchronously);

        await using var controller =
            LanguageServerComposition.CreateCacheLearning(
                message =>
                {
                    if (message.StartsWith("Invocation-cache learning failed:", StringComparison.Ordinal))
                        failed.TrySetResult(message);
                },
                options: new(MinimumInterval: TimeSpan.FromMilliseconds(10), SampleCapacity: 2),
                learningTimeout: TimeSpan.FromMilliseconds(200));

        controller.Record(InfiniteRequest()).Should().BeTrue();
        controller.Record(InfiniteRequest()).Should().BeTrue();

        var failure = await failed.Task.WaitAsync(s_timeout);
        failure.Should().Contain("exception=TimeoutException");
        controller.State.Should().BeSameAs(InvocationCacheLearningState.Initial);

        RecordValidPair(controller);
        await WaitUntil(() => controller.State.RunCount == 1);
        controller.CurrentPolicy.Should().BeSameAs(InvocationCachePolicy.Empty);
        controller.State.LastRun!.RequestCount.Should().Be(2);
    }

    [Fact(Timeout = 120_000)]
    public async Task Training_failure_keeps_published_state_and_later_valid_samples_recover()
    {
        var failed = new TaskCompletionSource<string>(TaskCreationOptions.RunContinuationsAsynchronously);

        await using var controller =
            LanguageServerComposition.CreateCacheLearning(
                message =>
                {
                    if (message.StartsWith("Invocation-cache learning failed:", StringComparison.Ordinal))
                        failed.TrySetResult(message);
                },
                options: new(MinimumInterval: TimeSpan.FromMilliseconds(10), SampleCapacity: 2));

        RecordValidPair(controller);
        await WaitUntil(() => controller.State.RunCount == 1);
        var previous = controller.State;

        var invalid =
            new InvocationCacheLearningRequest(
                new Expression.Eval(Expression.LitralInst(PineValue.Blob([1])), Expression.EnvironmentInstance),
                PineValue.EmptyList);

        controller.Record(invalid).Should().BeTrue();
        controller.Record(invalid).Should().BeTrue();

        var failure = await failed.Task.WaitAsync(s_timeout);
        failure.Should().Contain("exception=InvalidOperationException");
        controller.State.Should().BeSameAs(previous);
        controller.CurrentPolicy.Should().BeSameAs(previous.Policy);

        RecordValidPair(controller);
        await WaitUntil(() => controller.State.RunCount == previous.RunCount + 1);
        controller.State.LastRun!.RequestCount.Should().Be(2);
    }

    private static void RecordValidPair(ContinuousInvocationCacheLearning controller)
    {
        var request = new InvocationCacheLearningRequest(Expression.EnvironmentInstance, PineValue.EmptyList);
        controller.Record(request).Should().BeTrue();
        controller.Record(request).Should().BeTrue();
    }

    private static InvocationCacheLearningRequest InfiniteRequest()
    {
        var expression = new Expression.Eval(Expression.EnvironmentInstance, Expression.EnvironmentInstance);
        return new(expression, ExpressionEncoding.EncodeExpressionAsValue(expression));
    }

    private static async Task WaitUntil(Func<bool> predicate)
    {
        var clock = Stopwatch.StartNew();

        while (!predicate())
        {
            if (clock.Elapsed >= s_timeout)
                throw new TimeoutException("Timed out waiting for invocation-cache learning.");

            await Task.Delay(10);
        }
    }
}
