using AwesomeAssertions;
using Pine.Core.Interpreter.IntermediateVM;
using System;
using System.Threading;
using System.Threading.Tasks;
using Xunit;

namespace Pine.Core.Tests.Interpreter.IntermediateVM;

public class InvocationCacheExecutionContextTests
{
    [Fact]
    public void Multiple_evals_are_private_until_request_completion_and_each_request_gets_a_fresh_evaluator()
    {
        var shared = new ConcurrentInvocationCache();
        var creations = 0;
        var completions = 0;
        var key = GenerationalInvocationCacheTests.BuildKey(1);
        var value = PineValue.Blob([10]);

        var context =
            new InvocationCacheExecutionContext<IInvocationCacheAccess>(
                shared,
                buffer =>
                {
                    ++creations;
                    return buffer;
                },
                onCompleted:
                () =>
                {
                    shared.TryGet(key, out _).Should().BeTrue();
                    ++completions;
                });

        IInvocationCacheAccess? first = null;

        context.Run(
            buffer =>
            {
                first = buffer;
                buffer.TryAdd(key, value).Should().BeTrue();
                shared.Count.Should().Be(0);
                buffer.TryGet(key, out var found).Should().BeTrue();
                found.Should().Be(value);
                return 42;
            }).Should().Be(42);

        context.Run(
            buffer =>
            {
                buffer.Should().NotBeSameAs(first);
                buffer.TryGet(key, out var found).Should().BeTrue();
                found.Should().Be(value);
                return 0;
            });

        creations.Should().Be(2);
        completions.Should().Be(2);
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void Exceptions_and_cancellation_publish_completed_entries_and_run_policy(bool cancel)
    {
        var shared = new ConcurrentInvocationCache();
        var completed = 0;
        var key = GenerationalInvocationCacheTests.BuildKey(1);

        var context =
            new InvocationCacheExecutionContext<IInvocationCacheAccess>(
                shared,
                buffer => buffer,
                onCompleted: () => ++completed);

        var run =
            () => context.Run<int>(
                buffer =>
                {
                    buffer.TryAdd(key, PineValue.EmptyList);
                    throw cancel ? new OperationCanceledException() : new InvalidOperationException("failure");
                });

        run.Should().Throw<Exception>();
        shared.TryGet(key, out _).Should().BeTrue();
        completed.Should().Be(1);
    }

    [Fact]
    public void Factory_failure_also_finalizes_its_buffer()
    {
        var shared = new ConcurrentInvocationCache();
        var key = GenerationalInvocationCacheTests.BuildKey(1);
        var completed = false;

        var context =
            new InvocationCacheExecutionContext<IInvocationCacheAccess>(
                shared,
                buffer =>
                {
                    buffer.TryAdd(key, PineValue.EmptyList);
                    throw new InvalidOperationException("factory");
                },
                onCompleted: () => completed = true);

        var run = () => context.Run(_ => 1);
        run.Should().Throw<InvalidOperationException>().WithMessage("factory");
        shared.TryGet(key, out _).Should().BeTrue();
        completed.Should().BeTrue();
    }

    [Fact]
    public async Task Async_execution_merges_only_after_await_and_keeps_concurrent_buffers_isolated()
    {
        var shared = new ConcurrentInvocationCache();
        var context = new InvocationCacheExecutionContext<IInvocationCacheAccess>(shared, buffer => buffer);
        var started = new TaskCompletionSource(TaskCreationOptions.RunContinuationsAsynchronously);
        var release = new TaskCompletionSource(TaskCreationOptions.RunContinuationsAsynchronously);
        var key = GenerationalInvocationCacheTests.BuildKey(1);

        var first =
            context.RunAsync(
                async buffer =>
                {
                    buffer.TryAdd(key, PineValue.EmptyList);
                    started.SetResult();
                    await release.Task;
                    return 7;
                });

        await started.Task;
        shared.Count.Should().Be(0);
        context.Run(buffer => buffer.TryGet(key, out _)).Should().BeFalse();
        release.SetResult();
        (await first).Should().Be(7);
        shared.TryGet(key, out _).Should().BeTrue();
    }

    [Fact]
    public async Task Async_cancellation_preserves_token_and_merges_before_completion()
    {
        var shared = new ConcurrentInvocationCache();
        var key = GenerationalInvocationCacheTests.BuildKey(1);
        using var cancellation = new CancellationTokenSource();
        var context = new InvocationCacheExecutionContext<IInvocationCacheAccess>(shared, buffer => buffer);

        var run =
            async () => await context.RunAsync(
                async buffer =>
                {
                    buffer.TryAdd(key, PineValue.EmptyList);
                    await Task.Yield();
                    cancellation.Cancel();
                    cancellation.Token.ThrowIfCancellationRequested();
                    return 0;
                });

        var exception = await run.Should().ThrowAsync<OperationCanceledException>();
        exception.Which.CancellationToken.Should().Be(cancellation.Token);
        shared.TryGet(key, out _).Should().BeTrue();
    }

    [Fact]
    public async Task Optional_buffer_limit_is_independent_of_shared_admission_after_rotation()
    {
        var shared = new GenerationalInvocationCache(new(2, 2));

        var context =
            new InvocationCacheExecutionContext<IInvocationCacheAccess>(
                shared,
                buffer => buffer,
                maxBufferedEntries: 2);

        var key = GenerationalInvocationCacheTests.BuildKey(1);

        await context.RunAsync(
            async buffer =>
            {
                buffer.TryAdd(key, PineValue.EmptyList).Should().BeTrue();
                buffer.TryAdd(GenerationalInvocationCacheTests.BuildKey(2), PineValue.EmptyList).Should().BeTrue();
                buffer.TryAdd(GenerationalInvocationCacheTests.BuildKey(3), PineValue.EmptyList).Should().BeFalse();
                await shared.AdvanceGenerationAsync(1);
                shared.TryGet(key, out _).Should().BeFalse();
                return 0;
            });

        shared.GetStatistics().EntriesPerGeneration.Should().Equal(2);
        shared.TryGet(key, out _).Should().BeTrue();
    }

    [Fact]
    public void Default_buffer_and_shared_merge_keep_all_entries_above_the_rotation_threshold()
    {
        var shared = new GenerationalInvocationCache(new(2, 1));
        var context = new InvocationCacheExecutionContext<IInvocationCacheAccess>(shared, buffer => buffer);

        context.Run(
            buffer =>
            {
                for (var index = 0; index < 20_000; ++index)
                {

                    buffer.TryAdd(GenerationalInvocationCacheTests.BuildKey(index), PineValue.EmptyList)
                        .Should().BeTrue();
                }

                shared.PhysicalEntryCount.Should().Be(0);
                return 0;
            });

        shared.PhysicalEntryCount.Should().Be(20_000);
        shared.NewestGenerationEntryCount.Should().Be(20_000);
        shared.GetStatistics().Rotations.Should().Be(0);
    }
}
