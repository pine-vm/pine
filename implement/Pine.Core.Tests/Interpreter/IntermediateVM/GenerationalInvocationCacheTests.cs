using AwesomeAssertions;
using Pine.Core.Interpreter.IntermediateVM;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Reflection;
using System.Runtime.CompilerServices;
using System.Threading;
using System.Threading.Tasks;
using Xunit;

namespace Pine.Core.Tests.Interpreter.IntermediateVM;

public class GenerationalInvocationCacheTests
{
    [Fact]
    public async Task Hits_promote_hot_entries_while_cold_entries_and_expression_indexes_expire()
    {
        var cache = CreateCache();
        var hot = BuildKey(1);
        var cold = BuildKey(2);
        var value = PineValue.Blob([10]);
        cache.TryAdd(hot, value).Should().BeTrue();
        cache.TryAdd(cold, value).Should().BeTrue();

        await cache.AdvanceGenerationAsync(2);
        cache.TryGet(hot, out var found).Should().BeTrue();
        found.Should().Be(value);
        var promoted = cache.GetStatistics();
        promoted.EntriesPerGeneration.Should().Equal(1, 2);
        promoted.PhysicalEntryCount.Should().Be(3);
        promoted.UniqueEntryCount.Should().Be(2);
        promoted.Promotions.Should().Be(1);

        await cache.AdvanceGenerationAsync(2);
        cache.MayContainExpression(cold.ExprValue).Should().BeFalse();
        cache.TryGet(cold, out _).Should().BeFalse();
        cache.TryGet(hot, out found).Should().BeTrue();
        found.Should().Be(value);
        cache.GetStatistics().UniqueEntryCount.Should().Be(1);
        cache.GetStatistics().RetiredEntries.Should().Be(2);
    }

    [Fact]
    public async Task Duplicate_publication_promotes_the_original_value_not_the_losing_value()
    {
        var cache = CreateCache();
        var key = BuildKey(1);
        var original = PineValue.Blob([10]);
        cache.TryAdd(key, original).Should().BeTrue();
        await cache.AdvanceGenerationAsync(2);

        cache.TryAdd(BuildKey(1), PineValue.Blob([20])).Should().BeFalse();
        await cache.AdvanceGenerationAsync(2);
        cache.TryGet(key, out var found).Should().BeTrue();
        found.Should().Be(original);
    }

    [Fact]
    public async Task Admission_merge_and_promotion_are_unbounded_until_background_maintenance()
    {
        var cache = CreateCache(threshold: 1);
        var old = BuildKey(1);
        var newest = BuildKey(2);
        cache.TryAdd(old, PineValue.Blob([1])).Should().BeTrue();
        await cache.AdvanceGenerationAsync(2);
        cache.TryAdd(newest, PineValue.Blob([2])).Should().BeTrue();
        cache.TryAdd(BuildKey(3), PineValue.EmptyList).Should().BeTrue();

        var entries =
            Enumerable.Range(4, 20_000)
            .ToDictionary(BuildKey, _ => (PineValue)PineValue.EmptyList);

        cache.Merge(entries);
        cache.TryGet(old, out var value).Should().BeTrue();
        value.Should().Be(PineValue.Blob([1]));

        var stats = cache.GetStatistics();
        stats.EntriesPerGeneration.Should().Equal(20_003, 1);
        stats.Promotions.Should().Be(1);
        cache.NewestGenerationEntryCount.Should().Be(20_003);
        cache.PhysicalEntryCount.Should().Be(20_004);
        stats.Rotations.Should().Be(1, "insertion and merging must not run maintenance");
        await cache.MaintainAsync();
        cache.TryGet(old, out _).Should().BeTrue();
        cache.TryGet(newest, out _).Should().BeTrue();
    }

    [Fact]
    public async Task Maintenance_is_nonblocking_coalesced_and_rechecks_occupancy()
    {
        var cache = CreateCache(threshold: 2);
        cache.TryAdd(BuildKey(1), PineValue.EmptyList);
        cache.TryAdd(BuildKey(2), PineValue.EmptyList);
        cache.GetStatistics().Rotations.Should().Be(0);

        var gate =
            (Lock)typeof(GenerationalInvocationCache)
            .GetField("_lock", BindingFlags.Instance | BindingFlags.NonPublic)!.GetValue(cache)!;

        Task maintenance;

        lock (gate)
        {
            maintenance = cache.MaintainAsync();
            maintenance.IsCompleted.Should().BeFalse("maintenance cannot run inline on the requesting thread");
            cache.MaintainAsync().Should().BeSameAs(maintenance);
            cache.GetStatistics().Rotations.Should().Be(0);
            cache.NewestGenerationEntryCount.Should().Be(2);
        }

        await maintenance.WaitAsync(TimeSpan.FromSeconds(30));
        cache.GetStatistics().EntriesPerGeneration.Should().Equal(0, 2);
        cache.GetStatistics().Rotations.Should().Be(1);
        cache.PhysicalEntryCount.Should().Be(2);
        cache.NewestGenerationEntryCount.Should().Be(0);

        await cache.MaintainAsync();
        cache.GetStatistics().Rotations.Should().Be(1, "stale maintenance must not retire another generation");

        for (var iteration = 0; iteration < 10; ++iteration)
        {
            cache.TryAdd(BuildKey(3), PineValue.EmptyList);
            cache.TryAdd(BuildKey(4), PineValue.EmptyList);
            await cache.MaintainAsync();
            cache.NewestGenerationEntryCount.Should().Be(0);
        }

        cache.GetStatistics().Rotations.Should().Be(11);
    }

    [Theory]
    [InlineData(0, 1)]
    [InlineData(-1, 1)]
    [InlineData(1, 0)]
    [InlineData(1, -1)]
    public void Invalid_options_are_rejected(int count, int threshold)
    {
        var construct = () => new InvocationCacheGenerationOptions(count, threshold);
        construct.Should().Throw<ArgumentOutOfRangeException>();
    }

    [Fact]
    public async Task Explicit_rotation_validates_limits_and_supports_one_generation()
    {
        var cache = CreateCache();
        var key = BuildKey(1);
        cache.TryAdd(key, PineValue.EmptyList);
        Action zero = () => cache.AdvanceGenerationAsync(0);
        Action negative = () => cache.AdvanceGenerationAsync(-1);
        Action tooLarge = () => cache.AdvanceGenerationAsync(3);
        zero.Should().Throw<ArgumentOutOfRangeException>();
        negative.Should().Throw<ArgumentOutOfRangeException>();
        tooLarge.Should().Throw<ArgumentOutOfRangeException>();
        cache.GetStatistics().UniqueEntryCount.Should().Be(1);
        await cache.AdvanceGenerationAsync(1);
        cache.TryGet(key, out _).Should().BeFalse();
        cache.MayContainExpression(key.ExprValue).Should().BeFalse();
        cache.GetStatistics().EntriesPerGeneration.Should().Equal(0);
    }

    [Fact]
    public async Task Parallel_lookup_batch_publication_and_maintenance_preserve_values()
    {
        var cache = CreateCache(threshold: 16);
        var maintenance = new System.Collections.Concurrent.ConcurrentBag<Task>();

        Parallel.For(
            0,
            5_000,
            index =>
            {
                var key = BuildKey(index % 50);
                var value = PineValue.Blob(BitConverter.GetBytes(index % 50));
                cache.TryAdd(key, value);

                if (cache.TryGet(key, out var found))
                    found.Should().Be(value);

                if (index % 7 == 0)
                    maintenance.Add(cache.MaintainAsync());

                if (index % 31 == 0)
                    cache.Merge(new Dictionary<EvalCacheEntryKey, PineValue> { [key] = value });
            });

        await Task.WhenAll(maintenance);
        var stats = cache.GetStatistics();
        stats.EntriesPerGeneration.Count.Should().BeLessThanOrEqualTo(2);
        stats.EntriesPerGeneration.Should().OnlyContain(count => count <= 50);
        stats.PhysicalEntryCount.Should().BeLessThanOrEqualTo(100);
        cache.PhysicalEntryCount.Should().Be(stats.PhysicalEntryCount);
        cache.NewestGenerationEntryCount.Should().Be(stats.EntriesPerGeneration[0]);
        stats.UniqueEntryCount.Should().BeLessThanOrEqualTo((int)stats.PhysicalEntryCount);
        stats.Hits.Should().BeGreaterThan(0);
    }

    [Fact]
    public async Task Retired_dictionaries_do_not_keep_key_or_value_graphs_alive()
    {
        var cache = CreateCache();
        var references = PopulateAndRetire(cache);
        await cache.AdvanceGenerationAsync(1);

        GC.Collect();
        GC.WaitForPendingFinalizers();
        GC.Collect();

        references.Input.IsAlive.Should().BeFalse();
        references.Value.IsAlive.Should().BeFalse();
        GC.KeepAlive(cache);
    }

    [MethodImpl(MethodImplOptions.NoInlining)]
    private static (WeakReference Input, WeakReference Value) PopulateAndRetire(GenerationalInvocationCache cache)
    {
        var key = BuildKey(123_456);
        var value = PineValue.Blob(new byte[100_000]);
        cache.TryAdd(key, value);
        var references = (new WeakReference(key.StackFrameInput), new WeakReference(value));
        return references;
    }

    private static GenerationalInvocationCache CreateCache(
        int threshold = 10) =>
        new(new InvocationCacheGenerationOptions(2, threshold));

    internal static EvalCacheEntryKey BuildKey(int value) =>
        new(
            PineValue.Blob(BitConverter.GetBytes(value)),
            StackFrameInput.GenericFromEnvironmentValue(PineValue.Blob(BitConverter.GetBytes(value))));
}
