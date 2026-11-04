using AwesomeAssertions;
using Pine.Core.Caching;
using System;
using System.Collections.Generic;
using System.Runtime.CompilerServices;
using System.Threading;
using System.Threading.Tasks;
using Xunit;

namespace Pine.Core.Tests.Caching;

public class MemoryCacheTests
{
    [Fact]
    public void Clearing_releases_keys_and_values_even_with_a_retained_invocation_usage()
    {
        var cache = new MemoryCache<object, object, long>(0);
        var usage = cache.SetMarker(1);
        var (key, value) = PublishAndClear(cache);
        GC.Collect();
        GC.WaitForPendingFinalizers();
        GC.Collect();
        key.IsAlive.Should().BeFalse();
        value.IsAlive.Should().BeFalse();
        usage.EntryCount.Should().Be(1);
        GC.KeepAlive(cache);
    }

    [MethodImpl(MethodImplOptions.NoInlining)]
    private static (WeakReference Key, WeakReference Value) PublishAndClear(MemoryCache<object, object, long> cache)
    {
        var key = new object();
        var value = new object();
        cache.Set(key, value);
        cache.Clear();
        return (new(key), new(value));
    }

    [Fact]
    public async Task Removal_observers_update_secondary_indexes_outside_the_storage_lock()
    {
        MemoryCache<string, int, long>? cache = null;
        var removed = new List<(string, int)>();
        cache = new(0, onRemoved: (key, value) =>
        {
            Task.Run(() => cache!.Count).Wait(TimeSpan.FromSeconds(10)).Should().BeTrue();
            removed.Add((key, value));
        });
        cache.Set("first", 1);
        await cache.TrimAsync(0, _ => new CacheRetention.Remove());
        cache.Set("second", 2);
        cache.Clear();
        removed.Should().Equal([("first", 1), ("second", 2)]);
    }

    [Fact]
    public async Task Lookups_update_last_marker_and_count_distinct_entries()
    {
        var cache = new MemoryCache<string, int, long>(0);
        var first = cache.SetMarker(1);
        cache.Set("a", 1);
        cache.Set("b", 2);
        cache.TryGetValue("a", out _).Should().BeTrue();
        cache.TryGetValue("missing", out _).Should().BeFalse();
        first.EntryCount.Should().Be(2);
        var second = cache.SetMarker(2);
        cache.TryGetValue("a", out _).Should().BeTrue();
        second.EntryCount.Should().Be(1);
        first.EntryCount.Should().Be(2);

        await cache.TrimAsync(1, marker => new CacheRetention.Ranked(marker));
        cache.TryGetValue("a", out var value).Should().BeTrue();
        value.Should().Be(1);
        cache.TryGetValue("b", out _).Should().BeFalse();
    }

    [Fact]
    public async Task Mandatory_retention_consumes_budget_and_mandatory_removal_ignores_rank()
    {
        var cache = new MemoryCache<int, int, int>(0);

        for (var marker = 0; marker < 6; marker++)
        {
            cache.SetMarker(marker);
            cache.Set(marker, marker);
        }

        await cache.TrimAsync(
            2,
            marker => marker switch
            {
                0 => new CacheRetention.Retain(),
                5 => new CacheRetention.Remove(),

                _ =>
                new CacheRetention.Ranked(marker)
            });

        cache.Count.Should().Be(2);
        cache.TryGetValue(0, out _).Should().BeTrue();
        cache.TryGetValue(4, out _).Should().BeTrue();
        cache.TryGetValue(5, out _).Should().BeFalse();
        await cache.TrimAsync(0, _ => new CacheRetention.Retain());
        cache.Count.Should().Be(2);
        await cache.TrimAsync(0, _ => new CacheRetention.Remove());
        cache.Count.Should().Be(0);
    }

    [Fact]
    public async Task Concurrent_invocations_inherit_independent_markers_across_await()
    {
        var cache = new MemoryCache<string, int, long>(0);
        cache.SetMarker(100);
        var ready = new TaskCompletionSource(TaskCreationOptions.RunContinuationsAsynchronously);
        var start = new TaskCompletionSource(TaskCreationOptions.RunContinuationsAsynchronously);

        var first =
            Task.Run(
                async () =>
                {
                    var usage = cache.SetMarker(1);
                    ready.SetResult();
                    await start.Task;
                    cache.Set("first", 1);
                    usage.EntryCount.Should().Be(1);
                });

        await ready.Task;

        var second =
            Task.Run(
                async () =>
                {
                    cache.SetMarker(2);
                    await Task.Yield();
                    cache.Set("second", 2);
                    start.SetResult();
                });

        await Task.WhenAll(first, second);

        await cache.TrimAsync(
            2,
            marker => marker == 1
            ?
            new CacheRetention.Remove()
            :
            new CacheRetention.Retain());

        cache.TryGetValue("first", out _).Should().BeFalse();
        cache.Set("parent", 3);
        var markers = new HashSet<long>();

        await cache.TrimAsync(
            10,
            marker =>
            {
                markers.Add(marker);
                return new CacheRetention.Retain();
            });

        markers.Should().BeEquivalentTo([2L, 100L]);
        cache.TryGetValue("second", out _).Should().BeTrue();
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public async Task Cleanup_does_not_evict_entries_touched_or_replaced_after_snapshot(bool replace)
    {
        var cache = new MemoryCache<string, int, long>(0);
        cache.Set("a", 1);
        using var entered = new ManualResetEventSlim();
        using var release = new ManualResetEventSlim();

        var cleanup =
            cache.TrimAsync(
                0,
                _ =>
                {
                    entered.Set();

                    if (!release.Wait(TimeSpan.FromSeconds(10)))
                        throw new TimeoutException("Test did not release cleanup.");

                    return new CacheRetention.Remove();
                });

        try
        {
            entered.Wait(TimeSpan.FromSeconds(10)).Should().BeTrue();
            cache.SetMarker(2);

            if (replace)
                cache.Set("a", 2);

            else
                cache.TryGetValue("a", out _).Should().BeTrue();

            cache.Set("new", 3);
        }
        finally
        {
            release.Set();
        }

        await cleanup;
        cache.Count.Should().Be(2);
        cache.TryGetValue("a", out var value).Should().BeTrue();
        value.Should().Be(replace ? 2 : 1);
    }

    [Fact]
    public async Task Cancellation_and_policy_failure_are_reported_and_allow_retry()
    {
        var cache = new MemoryCache<int, int, int>(0);
        cache.Set(1, 1);
        using var cancelled = new CancellationTokenSource();
        cancelled.Cancel();
        Func<Task> cancel = () => cache.TrimAsync(0, _ => new CacheRetention.Remove(), cancelled.Token);
        await cancel.Should().ThrowAsync<OperationCanceledException>();
        Func<Task> fail = () => cache.TrimAsync(0, _ => throw new InvalidOperationException("policy failed"));
        await fail.Should().ThrowAsync<InvalidOperationException>();
        cache.Count.Should().Be(1);
        await cache.TrimAsync(0, _ => new CacheRetention.Remove());
        cache.Count.Should().Be(0);
        Action invalid = () => cache.TrimAsync(-1, _ => new CacheRetention.Remove());
        invalid.Should().Throw<ArgumentOutOfRangeException>();
    }
}
