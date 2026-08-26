using AwesomeAssertions;
using Pine.Core.Concurrent;
using System;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.Linq;
using System.Threading;
using System.Threading.Tasks;
using Xunit;

namespace Pine.Core.Tests.Concurrent;

public class RevisionedOperationSchedulerTests
{
    [Fact]
    public async Task Mutations_replay_and_commit_in_submission_order()
    {
        var attempts = new ConcurrentDictionary<int, int>();

        await using var scheduler =
            new RevisionedOperationScheduler<string, int, string, object>(
                initialState: string.Empty,
                maxConcurrencyCount: 2,
                createWorker: _ => new object(),
                execute:
                async (_, operation, state, cancellationToken) =>
                {
                    attempts.AddOrUpdate(operation, 1, (_, count) => count + 1);
                    await Task.Delay(operation is 1 ? 30 : 1, cancellationToken);
                    var nextState = state + operation;
                    return new RevisionedOperationAttempt<string, string>(
                        nextState,
                        nextState,
                        CanCompleteSpeculatively: false);
                },
                statesEqual: StringComparer.Ordinal.Equals);

        var first = scheduler.SubmitAsync(1);
        var second = scheduler.SubmitAsync(2);

        (await first).Result.Should().Be("1");
        (await second).Result.Should().Be("12");
        scheduler.GetStateSnapshot().Should().Be(("12", 2));
        attempts.Should().Contain(new KeyValuePair<int, int>(1, 2));
        attempts.Should().Contain(new KeyValuePair<int, int>(2, 2));
    }

    [Fact]
    public async Task Current_read_only_result_completes_without_replay()
    {
        var attemptCount = 0;

        await using var scheduler =
            new RevisionedOperationScheduler<string, string, string, object>(
                initialState: "state",
                maxConcurrencyCount: 1,
                createWorker: _ => new object(),
                execute:
                (_, operation, state, _) =>
                {
                    Interlocked.Increment(ref attemptCount);
                    return
                        ValueTask.FromResult(
                            new RevisionedOperationAttempt<string, string>(
                                state,
                                operation + state,
                                CanCompleteSpeculatively: true));
                },
                statesEqual: StringComparer.Ordinal.Equals);

        var result = await scheduler.SubmitAsync("read:");

        result.Should().Be(new RevisionedOperationResult<string>("read:state", 0, false));
        attemptCount.Should().Be(1);
    }

    [Fact]
    public async Task Stale_read_only_result_replays_against_latest_state()
    {
        await using var scheduler =
            new RevisionedOperationScheduler<int, bool, int, object>(
                initialState: 0,
                maxConcurrencyCount: 2,
                createWorker: _ => new object(),
                execute:
                async (_, mutates, state, cancellationToken) =>
                {
                    await Task.Delay(mutates ? 20 : 1, cancellationToken);

                    return
                        mutates
                        ?
                        new RevisionedOperationAttempt<int, int>(
                            state + 1,
                            state + 1,
                            CanCompleteSpeculatively: false)
                        :
                        new RevisionedOperationAttempt<int, int>(
                            state,
                            state,
                            CanCompleteSpeculatively: true);
                },
                statesEqual: (left, right) => left == right);

        var mutation = scheduler.SubmitAsync(true);
        var read = scheduler.SubmitAsync(false);

        (await mutation).Result.Should().Be(1);

        var readResult = await read;

        readResult.Should().Be(new RevisionedOperationResult<int>(1, 1, true));
    }

    [Fact]
    public async Task Worker_leases_are_bounded_and_finalized_after_every_attempt()
    {
        var activeWorkers = 0;
        var maximumActiveWorkers = 0;
        var finalizedAttempts = 0;

        await using var scheduler =
            new RevisionedOperationScheduler<int, int, int, object>(
                initialState: 0,
                maxConcurrencyCount: 2,
                createWorker: _ => new object(),
                execute:
                async (_, operation, state, cancellationToken) =>
                {
                    var active = Interlocked.Increment(ref activeWorkers);
                    UpdateMaximum(ref maximumActiveWorkers, active);

                    try
                    {
                        await Task.Delay(10, cancellationToken);
                        return new RevisionedOperationAttempt<int, int>(
                            state,
                            operation,
                            CanCompleteSpeculatively: true);
                    }
                    finally
                    {
                        Interlocked.Decrement(ref activeWorkers);
                    }
                },
                statesEqual: (left, right) => left == right,
                finalizeAttempt:
                _ =>
                {
                    Interlocked.Increment(ref finalizedAttempts);
                    return ValueTask.CompletedTask;
                });

        var operations =
            Enumerable.Range(0, 8)
            .Select(operation => scheduler.SubmitAsync(operation))
            .ToArray();

        await Task.WhenAll(operations);

        maximumActiveWorkers.Should().Be(2);
        finalizedAttempts.Should().Be(8);
    }

    [Fact]
    public async Task Failed_attempt_is_finalized_and_does_not_block_later_operations()
    {
        var finalizedAttempts = 0;

        await using var scheduler =
            new RevisionedOperationScheduler<int, bool, int, object>(
                initialState: 0,
                maxConcurrencyCount: 1,
                createWorker: _ => new object(),
                execute:
                (_, fails, state, _) =>
                fails
                ?
                ValueTask.FromException<RevisionedOperationAttempt<int, int>>(
                    new InvalidOperationException("failure"))
                :
                ValueTask.FromResult(
                    new RevisionedOperationAttempt<int, int>(
                        state,
                        state,
                        CanCompleteSpeculatively: true)),
                statesEqual: (left, right) => left == right,
                finalizeAttempt:
                _ =>
                {
                    Interlocked.Increment(ref finalizedAttempts);
                    return ValueTask.CompletedTask;
                });

        var failed = scheduler.SubmitAsync(true);
        var succeeded = scheduler.SubmitAsync(false);

        Func<Task> awaitFailed = async () => await failed;

        await awaitFailed.Should().ThrowAsync<InvalidOperationException>();
        await succeeded;

        finalizedAttempts.Should().Be(2);
    }

    [Fact]
    public async Task Failed_attempt_does_not_break_ordering_chain()
    {
        var releaseFirst = new TaskCompletionSource(TaskCreationOptions.RunContinuationsAsynchronously);

        await using var scheduler =
            new RevisionedOperationScheduler<int, int, int, object>(
                initialState: 0,
                maxConcurrencyCount: 3,
                createWorker: _ => new object(),
                execute:
                async (_, operation, state, _) =>
                {
                    if (operation is 1)
                    {
                        await releaseFirst.Task;
                    }

                    if (operation is 2)
                    {
                        throw new InvalidOperationException("failure");
                    }

                    return new RevisionedOperationAttempt<int, int>(
                        state + operation,
                        state + operation,
                        CanCompleteSpeculatively: false);
                },
                statesEqual: (left, right) => left == right);

        var first = scheduler.SubmitAsync(1);
        var failed = scheduler.SubmitAsync(2);
        var third = scheduler.SubmitAsync(3);

        await Task.Delay(20);

        failed.IsCompleted.Should().BeFalse();
        third.IsCompleted.Should().BeFalse();

        releaseFirst.SetResult();

        (await first).Result.Should().Be(1);

        Func<Task> awaitFailed = async () => await failed;

        await awaitFailed.Should().ThrowAsync<InvalidOperationException>();
        (await third).Result.Should().Be(4);
    }

    private static void UpdateMaximum(ref int maximum, int candidate)
    {
        var current = Volatile.Read(ref maximum);

        while (candidate > current)
        {
            var observed = Interlocked.CompareExchange(ref maximum, candidate, current);

            if (observed == current)
            {
                return;
            }

            current = observed;
        }
    }
}
