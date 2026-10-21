using AwesomeAssertions;
using Pine.Core.Addressing;
using Pine.Core.Interpreter.IntermediateVM;
using Pine.Core.PineVM;
using System;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Diagnostics;
using System.Linq;
using System.Threading;
using System.Threading.Tasks;
using Xunit;

namespace Pine.Core.Tests.Interpreter.IntermediateVM;

public class ContinuousInvocationCacheLearningTests
{
    private static readonly TimeSpan s_timeout = TimeSpan.FromSeconds(10);

    private static readonly TimeSpan s_interval = TimeSpan.FromMilliseconds(10);

    [Fact]
    public async Task Continuous_learning_records_only_successful_evaluations()
    {
        await using var controller = CreateIdleController();
        var success = Result<string, PineValue>.ok(PineValue.EmptyList);
        var failure = Result<string, PineValue>.err("failure");
        var expression = Expression.EnvironmentInstance;
        var environment = Value(1);
        var vm = controller.Wrap(new PlainVM((_, _) => success));

        vm.EvaluateExpression(expression, environment).Should().BeSameAs(success);
        vm.EvaluateExpression(expression, environment, CancellationToken.None).Should().BeSameAs(success);

        controller.Wrap(new PlainVM((_, _) => failure)).EvaluateExpression(expression, environment)
            .Should().BeSameAs(failure);

        Action fail =
            () => controller.Wrap(new PlainVM((_, _) => throw new InvalidOperationException()))
            .EvaluateExpression(expression, environment);

        fail.Should().Throw<InvalidOperationException>();

        var snapshot = controller.GetSnapshot();
        snapshot.AcceptedCount.Should().Be(2);
        snapshot.PendingCount.Should().Be(2);
        snapshot.RunCount.Should().Be(0);
        controller.CurrentPolicy.Should().Be(InvocationCachePolicy.Empty);
    }

    [Fact]
    public async Task Continuous_learning_preserves_cancellation_and_does_not_record_it()
    {
        await using var controller = CreateIdleController();
        using var cancellation = new CancellationTokenSource();
        CancellationToken? forwarded = null;

        var vm =
            controller.Wrap(
                new CancellableVM(
                    (_, _, token) =>
                    {
                        forwarded = token;
                        cancellation.Cancel();
                        token.ThrowIfCancellationRequested();
                        return Result<string, PineValue>.ok(PineValue.EmptyList);
                    }));

        Action evaluate =
            () => vm.EvaluateExpression(Expression.EnvironmentInstance, PineValue.EmptyList, cancellation.Token);

        evaluate.Should().Throw<OperationCanceledException>();
        forwarded.Should().Be(cancellation.Token);
        controller.GetSnapshot().AcceptedCount.Should().Be(0);

        var called = false;

        var plain =
            controller.Wrap(
                new PlainVM(
                    (_, _) =>
                    {
                        called = true;
                        return Result<string, PineValue>.ok(PineValue.EmptyList);
                    }));

        Action evaluatePlain =
            () => plain.EvaluateExpression(Expression.EnvironmentInstance, PineValue.EmptyList, cancellation.Token);

        evaluatePlain.Should().Throw<OperationCanceledException>();
        called.Should().BeFalse();
    }

    [Fact]
    public async Task Continuous_learning_checks_cancellation_after_noncancellable_evaluation()
    {
        await using var controller = CreateIdleController();
        using var cancellation = new CancellationTokenSource();

        var vm =
            controller.Wrap(
                new PlainVM(
                    (_, _) =>
                    {
                        cancellation.Cancel();
                        return Result<string, PineValue>.ok(PineValue.EmptyList);
                    }));

        Action evaluate =
            () => vm.EvaluateExpression(Expression.EnvironmentInstance, PineValue.EmptyList, cancellation.Token);

        evaluate.Should().Throw<OperationCanceledException>();
        controller.GetSnapshot().AcceptedCount.Should().Be(0);
    }

    [Fact]
    public async Task Continuous_learning_bounds_pending_requests_without_waiting()
    {
        await using var controller = CreateIdleController();

        for (var index = 0; index < 1000; ++index)
            controller.Record(Request(index % 256)).Should().Be(index < 32);

        var snapshot = controller.GetSnapshot();
        snapshot.PendingCount.Should().Be(32);
        snapshot.AcceptedCount.Should().Be(32);
        snapshot.DroppedCount.Should().Be(968);
        snapshot.SampleCount.Should().Be(0);
    }

    [Fact]
    public async Task Continuous_learning_retains_only_the_most_recent_sixteen_samples()
    {
        var entered = Signal();

        var release =
            new TaskCompletionSource<InvocationCacheLearningState>(TaskCreationOptions.RunContinuationsAsynchronously);

        var secondSample =
            new TaskCompletionSource<IReadOnlyList<InvocationCacheLearningRequest>>(
                TaskCreationOptions.RunContinuationsAsynchronously);

        var runs = 0;

        await using var controller =
            new ContinuousInvocationCacheLearning(
                async (state, requests, token) =>
                {
                    if (Interlocked.Increment(ref runs) == 1)
                    {
                        entered.TrySetResult();
                        return await release.Task.WaitAsync(token);
                    }

                    secondSample.TrySetResult(requests);
                    return state;
                },
                options: new(MinimumInterval: s_interval));

        RecordPair(controller);
        await entered.Task.WaitAsync(s_timeout);

        for (var index = 0; index < 32; ++index)
            controller.Record(Request(index)).Should().BeTrue();

        controller.Record(Request(99)).Should().BeFalse();
        release.SetResult(InvocationCacheLearningState.Initial);

        var sample = await secondSample.Task.WaitAsync(s_timeout);
        sample.Count.Should().Be(16);
        sample.Select(request => request.Environment).Should().Equal(Enumerable.Range(16, 16).Select(Value));
        controller.GetSnapshot().SampleCount.Should().Be(16);
        controller.GetSnapshot().DroppedCount.Should().Be(1);
    }

    [Fact]
    public async Task Continuous_learning_requires_two_samples()
    {
        var entered = Signal();

        await using var controller =
            new ContinuousInvocationCacheLearning(
                (state, _, _) =>
                {
                    entered.TrySetResult();
                    return Task.FromResult(state);
                },
                options: new(MinimumInterval: s_interval));

        controller.Record(Request(1));
        await WaitUntil(() => controller.GetSnapshot().SampleCount == 1);
        entered.Task.IsCompleted.Should().BeFalse();
        controller.Record(Request(2));
        await entered.Task.WaitAsync(s_timeout);
    }

    [Fact]
    public async Task Continuous_learning_publishes_atomically_and_logs_complete_policy_differences()
    {
        var entered = Signal();

        var release =
            new TaskCompletionSource<InvocationCacheLearningState>(TaskCreationOptions.RunContinuationsAsynchronously);

        var log = new ConcurrentQueue<string>();
        var before = new InvocationCachePolicy(ImmutableHashSet.Create(Value(1)));
        var after = new InvocationCachePolicy(ImmutableHashSet.Create(Value(2)));
        var runs = 0;

        await using var controller =
            new ContinuousInvocationCacheLearning(
                async (state, _, token) =>
                {
                    if (Interlocked.Increment(ref runs) == 1)
                    {
                        entered.TrySetResult();
                        return await release.Task.WaitAsync(token);
                    }

                    return state;
                },
                log.Enqueue,
                new(MinimumInterval: s_interval),
                InvocationCacheLearningState.Initial with { Policy = before });

        RecordPair(controller);
        await entered.Task.WaitAsync(s_timeout);
        controller.CurrentPolicy.Should().BeSameAs(before);

        release.SetResult(
            InvocationCacheLearningState.Initial with
            {
                Policy = after,
                RunCount = 1,
                LastRun = new(120, 80, 2, 4, 3, 1, 1, TimeSpan.FromMilliseconds(25))
            });

        await WaitUntil(() => SwitchLogs(log).Length == 1);
        controller.CurrentPolicy.Should().BeSameAs(after);
        var entry = SwitchLogs(log).Should().ContainSingle().Which;
        entry.Should().Contain("sequence=1");
        entry.Should().Contain("run=1");
        entry.Should().Contain("old=" + before.Id);
        entry.Should().Contain("new=" + after.Id);
        entry.Should().Contain("added=[" + Hash(Value(2)) + "]");
        entry.Should().Contain("removed=[" + Hash(Value(1)) + "]");
        entry.Should().Contain("old-expressions=1");
        entry.Should().Contain("new-expressions=1");
        entry.Should().Contain("accepted=2");
        entry.Should().Contain("dropped=0");
        entry.Should().Contain("elapsed-ms=");
        entry.Should().Contain("since-last-switch-ms=none");
        entry.Should().Contain("baseline-cost=120");
        entry.Should().Contain("selected-cost=80");
        entry.Should().Contain("requests=2");
        entry.Should().Contain("candidates=4");
        entry.Should().Contain("evaluated-candidates=3");
    }

    [Fact]
    public async Task Continuous_learning_equal_policies_do_not_switch_or_loop_when_search_is_finished()
    {
        var policy = new InvocationCachePolicy(ImmutableHashSet.Create(Value(1), Value(2)));
        var completed = Signal();
        var log = new ConcurrentQueue<string>();

        await using var controller =
            new ContinuousInvocationCacheLearning(
                (state, _, _) =>
                {
                    completed.TrySetResult();

                    return
                        Task.FromResult(
                            state with
                            {
                                Policy = new InvocationCachePolicy(ImmutableHashSet.Create(Value(2), Value(1))),
                                RunCount = state.RunCount + 1
                            });
                },
                log.Enqueue,
                new(MinimumInterval: s_interval),
                InvocationCacheLearningState.Initial with { Policy = policy });

        RecordPair(controller);
        await completed.Task.WaitAsync(s_timeout);
        await WaitUntil(() => controller.State.RunCount == 1);
        await Task.Delay(s_interval * 5);

        controller.GetSnapshot().RunCount.Should().Be(1);
        controller.GetSnapshot().SwitchCount.Should().Be(0);
        SwitchLogs(log).Should().BeEmpty();
    }

    [Fact]
    public async Task Continuous_learning_continues_unfinished_search_and_policy_changes_sequentially()
    {
        var finished = Signal();
        var policy = new InvocationCachePolicy(ImmutableHashSet.Create(Value(1)));
        var times = new ConcurrentQueue<long>();
        var active = 0;
        var maximumActive = 0;
        var runs = 0;

        await using var controller =
            new ContinuousInvocationCacheLearning(
                async (state, _, token) =>
                {
                    var currentActive = Interlocked.Increment(ref active);
                    maximumActive = Math.Max(maximumActive, currentActive);
                    times.Enqueue(Stopwatch.GetTimestamp());
                    await Task.Delay(s_interval * 2, token);
                    Interlocked.Decrement(ref active);
                    var run = Interlocked.Increment(ref runs);

                    if (run == 1)
                        return state with { KnownExpressions = [Value(1), Value(2)], NextCandidateIndex = 1 };

                    if (run == 2)
                        return state with { Policy = policy, NextCandidateIndex = 2 };

                    finished.TrySetResult();
                    return state;
                },
                options: new(MinimumInterval: s_interval));

        RecordPair(controller);
        await finished.Task.WaitAsync(s_timeout);
        await WaitUntil(() => !controller.GetSnapshot().IsLearning);
        await Task.Delay(s_interval * 4);

        runs.Should().Be(3);
        maximumActive.Should().Be(1);
        controller.GetSnapshot().SwitchCount.Should().Be(1);
        var starts = times.ToArray();

        foreach (var pair in starts.Zip(starts.Skip(1)))
            Stopwatch.GetElapsedTime(pair.First, pair.Second).Should().BeGreaterThanOrEqualTo(s_interval);
    }

    [Fact]
    public async Task Continuous_learning_failures_are_observed_private_and_do_not_break_future_evaluations()
    {
        var failed = Signal();
        var recovered = Signal();
        var log = new ConcurrentQueue<string>();
        var runs = 0;

        await using var controller =
            new ContinuousInvocationCacheLearning(
                (state, _, _) =>
                {
                    if (Interlocked.Increment(ref runs) == 1)
                        throw new InvalidOperationException("private evaluation input");

                    recovered.TrySetResult();
                    return Task.FromResult(state);
                },
                message =>
                {
                    log.Enqueue(message);

                    if (message.Contains("learning failed:", StringComparison.Ordinal))
                        failed.TrySetResult();
                },
                new(MinimumInterval: s_interval));

        RecordPair(controller);
        await failed.Task.WaitAsync(s_timeout);

        var error =
            log.Where(message => message.Contains("learning failed:", StringComparison.Ordinal))
            .Should().ContainSingle().Which;

        error.Should().Contain("InvalidOperationException");
        error.Should().NotContain("private evaluation input");
        controller.CurrentPolicy.Should().Be(InvocationCachePolicy.Empty);

        controller.Wrap(new PlainVM((_, environment) => Result<string, PineValue>.ok(environment)))
            .EvaluateExpression(Expression.EnvironmentInstance, Value(3));

        await recovered.Task.WaitAsync(s_timeout);
        controller.GetSnapshot().RunCount.Should().Be(2);
    }

    [Fact]
    public async Task Continuous_learning_finishes_one_evidenced_sweep_even_when_the_cursor_wraps()
    {
        var finished = Signal();
        var runs = 0;

        await using var controller =
            new ContinuousInvocationCacheLearning(
                (state, _, _) =>
                {
                    var run = Interlocked.Increment(ref runs);

                    if (run == 2)
                        finished.TrySetResult();

                    return
                        Task.FromResult(
                            state with
                            {
                                KnownExpressions = [Value(1), Value(2), Value(3)],
                                NextCandidateIndex = (state.NextCandidateIndex + 2) % 3,
                                LastRun = new(100, 100, 2, 3, 2, 0, 0, TimeSpan.Zero)
                            });
                },
                options: new(MinimumInterval: s_interval));

        RecordPair(controller);
        await finished.Task.WaitAsync(s_timeout);
        await WaitUntil(() => !controller.GetSnapshot().IsLearning);
        await Task.Delay(s_interval * 5);

        runs.Should().Be(2);
        controller.GetSnapshot().SwitchCount.Should().Be(0);
    }

    [Fact]
    public async Task Continuous_learning_logs_every_switch_with_cumulative_churn()
    {
        var first = new InvocationCachePolicy(ImmutableHashSet.Create(Value(1)));
        var second = new InvocationCachePolicy(ImmutableHashSet.Create(Value(2)));
        var log = new ConcurrentQueue<string>();
        var runs = 0;

        await using var controller =
            new ContinuousInvocationCacheLearning(
                (state, _, _) =>
                Task.FromResult(
                    state with
                    {
                        Policy = Interlocked.Increment(ref runs) == 1 ? first : second
                    }),
                log.Enqueue,
                new(MinimumInterval: s_interval));

        RecordPair(controller);
        await WaitUntil(() => SwitchLogs(log).Length == 2);
        var entries = SwitchLogs(log);
        entries.Length.Should().Be(2);
        entries[1].Should().Contain("sequence=2");
        entries[1].Should().Contain("switches=2");
        entries[1].Should().Contain("old=" + first.Id);
        entries[1].Should().Contain("new=" + second.Id);
        entries[1].Should().NotContain("since-last-switch-ms=none");
    }

    [Fact]
    public async Task Continuous_learning_disposal_cancels_and_waits_without_post_disposal_publication()
    {
        var entered = Signal();
        var cancelled = Signal();

        var release =
            new TaskCompletionSource<InvocationCacheLearningState>(TaskCreationOptions.RunContinuationsAsynchronously);

        var log = new ConcurrentQueue<string>();
        var resourceDisposals = 0;

        var controller =
            new ContinuousInvocationCacheLearning(
                async (_, _, token) =>
                {
                    using var registration = token.Register(() => cancelled.TrySetResult());
                    entered.TrySetResult();
                    return await release.Task;
                },
                log.Enqueue,
                new(MinimumInterval: s_interval),
                learningResource: new DisposableAction(() => Interlocked.Increment(ref resourceDisposals)));

        try
        {
            RecordPair(controller);
            await entered.Task.WaitAsync(s_timeout);
            var disposal = controller.DisposeAsync().AsTask();
            await cancelled.Task.WaitAsync(s_timeout);
            disposal.IsCompleted.Should().BeFalse();
            controller.Record(Request(3)).Should().BeFalse();
            Volatile.Read(ref resourceDisposals).Should().Be(1);

            release.SetResult(
                InvocationCacheLearningState.Initial with
                {
                    Policy = new InvocationCachePolicy(ImmutableHashSet.Create(Value(1)))
                });

            await disposal.WaitAsync(s_timeout);
            await controller.DisposeAsync();
            controller.Dispose();

            controller.CurrentPolicy.Should().Be(InvocationCachePolicy.Empty);
            controller.GetSnapshot().SwitchCount.Should().Be(0);
            controller.GetSnapshot().IsDisposed.Should().BeTrue();
            controller.GetSnapshot().PendingCount.Should().Be(0);
            resourceDisposals.Should().Be(1);
            SwitchLogs(log).Should().BeEmpty();
        }
        finally
        {
            release.TrySetResult(InvocationCacheLearningState.Initial);
            await controller.DisposeAsync();
        }
    }

    [Fact]
    public async Task Continuous_learning_synchronous_disposal_cancels_in_flight_learning()
    {
        var entered = Signal();

        var controller =
            new ContinuousInvocationCacheLearning(
                async (state, _, token) =>
                {
                    entered.TrySetResult();
                    await Task.Delay(Timeout.InfiniteTimeSpan, token);
                    return state;
                },
                options: new(MinimumInterval: s_interval));

        RecordPair(controller);
        await entered.Task.WaitAsync(s_timeout);
        controller.Dispose();
        await controller.DisposeAsync().AsTask().WaitAsync(s_timeout);
        controller.GetSnapshot().IsDisposed.Should().BeTrue();
        controller.GetSnapshot().SwitchCount.Should().Be(0);
    }

    [Fact]
    public async Task Continuous_learning_throwing_logger_does_not_stop_the_pump()
    {
        var policy = new InvocationCachePolicy(ImmutableHashSet.Create(Value(1)));
        var secondRun = Signal();
        var runs = 0;

        await using var controller =
            new ContinuousInvocationCacheLearning(
                (state, _, _) =>
                {
                    if (Interlocked.Increment(ref runs) > 1)
                        secondRun.TrySetResult();

                    return Task.FromResult(state with { Policy = policy });
                },
                _ => throw new InvalidOperationException("logger"),
                new(MinimumInterval: s_interval));

        RecordPair(controller);
        await secondRun.Task.WaitAsync(s_timeout);
        controller.CurrentPolicy.Should().Be(policy);
        controller.GetSnapshot().SwitchCount.Should().Be(1);
    }

    [Fact]
    public void Continuous_learning_validates_bounds_before_starting_a_pump()
    {
        foreach (var options in new ContinuousInvocationCacheLearningOptions[]
        {
            new(MinimumInterval: TimeSpan.Zero),
            new(MinimumInterval: TimeSpan.FromTicks(1)),
            new(SampleCapacity: 1),
            new(SampleCapacity: 17),
            new(PendingCapacity: 0),
            new(PendingCapacity: 33)
        })
        {
            Action construct =
                () => new ContinuousInvocationCacheLearning(
                    (state, _, _) => Task.FromResult(state),
                    options: options);

            construct.Should().Throw<ArgumentOutOfRangeException>();
        }
    }

    [Fact]
    public void Continuous_learning_disposes_owned_resource_if_initialization_fails()
    {
        var disposals = 0;

        Action construct =
            () => new ContinuousInvocationCacheLearning(
                (state, _, _) => Task.FromResult(state),
                options: new(SampleCapacity: 1),
                learningResource: new DisposableAction(() => ++disposals));

        construct.Should().Throw<ArgumentOutOfRangeException>();
        disposals.Should().Be(1);
    }

    [Fact]
    public async Task Continuous_learning_observes_resource_disposal_failures_once_without_private_logs()
    {
        var disposals = 0;
        var log = new ConcurrentQueue<string>();

        var controller =
            new ContinuousInvocationCacheLearning(
                (state, _, _) => Task.FromResult(state),
                log.Enqueue,
                learningResource:
                new DisposableAction(
                    () =>
                    {
                        Interlocked.Increment(ref disposals);
                        throw new InvalidOperationException("private transport details");
                    }));

        controller.Dispose();
        await controller.DisposeAsync();
        controller.Dispose();

        disposals.Should().Be(1);

        var error =
            log.Where(message => message.Contains("resource disposal failed:", StringComparison.Ordinal))
            .Should().ContainSingle().Which;

        error.Should().Contain("resource disposal failed");
        error.Should().Contain("InvalidOperationException");
        error.Should().NotContain("private transport details");
    }

    [Fact]
    public async Task Continuous_learning_concurrent_async_disposal_waits_for_owned_resource_cleanup()
    {
        var entered = Signal();
        var release = Signal();
        var disposals = 0;

        var controller =
            new ContinuousInvocationCacheLearning(
                (state, _, _) => Task.FromResult(state),
                learningResource:
                new DisposableAction(
                    () =>
                    {
                        Interlocked.Increment(ref disposals);
                        entered.TrySetResult();
                        release.Task.GetAwaiter().GetResult();
                    }));

        var synchronousDisposal = Task.Run(controller.Dispose);

        try
        {
            await entered.Task.WaitAsync(s_timeout);
            var asynchronousDisposal = controller.DisposeAsync().AsTask();
            asynchronousDisposal.IsCompleted.Should().BeFalse();
            release.TrySetResult();
            await Task.WhenAll(synchronousDisposal, asynchronousDisposal).WaitAsync(s_timeout);
            disposals.Should().Be(1);
        }
        finally
        {
            release.TrySetResult();
            await controller.DisposeAsync();
        }
    }

    [Fact]
    public async Task Continuous_learning_diagnostics_do_not_hold_the_publication_lock()
    {
        var entered = Signal();
        var release = Signal();
        var policy = new InvocationCachePolicy(ImmutableHashSet.Create(Value(1)));

        var controller =
            new ContinuousInvocationCacheLearning(
                (state, _, _) => Task.FromResult(state with { Policy = policy }),
                message =>
                {
                    if (!message.StartsWith("Invocation-cache policy switch:", StringComparison.Ordinal))
                        return;

                    entered.TrySetResult();
                    release.Task.GetAwaiter().GetResult();
                },
                new(MinimumInterval: s_interval));

        try
        {
            RecordPair(controller);
            await entered.Task.WaitAsync(s_timeout);
            var snapshot = await Task.Run(controller.GetSnapshot).WaitAsync(s_timeout);
            controller.CurrentPolicy.Should().BeSameAs(policy);
            controller.State.Policy.Should().BeSameAs(policy);
            snapshot.SwitchCount.Should().Be(1);
            await Task.Run(controller.Dispose).WaitAsync(s_timeout);
            release.TrySetResult();
            await controller.DisposeAsync().AsTask().WaitAsync(s_timeout);
        }
        finally
        {
            release.TrySetResult();
            await controller.DisposeAsync();
        }
    }

    [Fact]
    public async Task Continuous_learning_logs_initial_empty_policy_and_default_configuration_once()
    {
        var configured = new TaskCompletionSource<string>(TaskCreationOptions.RunContinuationsAsynchronously);
        var log = new ConcurrentQueue<string>();

        await using var controller =
            new ContinuousInvocationCacheLearning(
                (state, _, _) => Task.FromResult(state),
                message =>
                {
                    log.Enqueue(message);
                    configured.TrySetResult(message);
                });

        var entry = await configured.Task.WaitAsync(s_timeout);
        entry.Should().StartWith("Invocation-cache learning configured:");
        entry.Should().Contain("policy=" + InvocationCachePolicy.Empty.Id);
        entry.Should().Contain("expressions=0");
        entry.Should().Contain("interval-ms=30000.000");
        entry.Should().Contain("sample-capacity=16");
        entry.Should().Contain("pending-capacity=32");
        controller.CurrentPolicy.Should().BeSameAs(InvocationCachePolicy.Empty);
        controller.GetSnapshot().RunCount.Should().Be(0);
        log.Should().ContainSingle();
    }

    private static ContinuousInvocationCacheLearning CreateIdleController() =>
        new(
            (state, _, _) => Task.FromResult(state),
            options: new(MinimumInterval: TimeSpan.FromDays(1)));

    private static TaskCompletionSource Signal() =>
        new(TaskCreationOptions.RunContinuationsAsynchronously);

    private static PineValue Value(int value) =>
        PineValue.Blob([(byte)value]);

    private static InvocationCacheLearningRequest Request(int value) =>
        new(Expression.EnvironmentInstance, Value(value));

    private static void RecordPair(ContinuousInvocationCacheLearning controller)
    {
        controller.Record(Request(1)).Should().BeTrue();
        controller.Record(Request(2)).Should().BeTrue();
    }

    private static string Hash(PineValue value) =>
        Convert.ToHexStringLower(PineValueHashFlat.ComputeHashForValue(value).hashBytes.Span);

    private static string[] SwitchLogs(IEnumerable<string> messages) =>
        [.. messages.Where(message => message.StartsWith("Invocation-cache policy switch:", StringComparison.Ordinal))];

    private static async Task WaitUntil(Func<bool> predicate)
    {
        var started = Stopwatch.GetTimestamp();

        while (!predicate())
        {
            Stopwatch.GetElapsedTime(started).Should().BeLessThan(s_timeout, "the controller should make progress");
            await Task.Delay(1);
        }
    }

    private sealed class PlainVM(
        Func<Expression, PineValue, Result<string, PineValue>> evaluate) : IPineVM
    {
        public Result<string, PineValue> EvaluateExpression(Expression expression, PineValue environment) =>
            evaluate(expression, environment);
    }

    private sealed class DisposableAction(Action dispose) : IDisposable
    {
        public void Dispose() => dispose();
    }

    private sealed class CancellableVM(
        Func<Expression, PineValue, CancellationToken, Result<string, PineValue>> evaluate) : ICancellablePineVM
    {
        public Result<string, PineValue> EvaluateExpression(Expression expression, PineValue environment) =>
            evaluate(expression, environment, CancellationToken.None);

        public Result<string, PineValue> EvaluateExpression(
            Expression expression,
            PineValue environment,
            CancellationToken cancellationToken) =>
            evaluate(expression, environment, cancellationToken);
    }
}
