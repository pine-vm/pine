using AwesomeAssertions;
using Pine.Core.Addressing;
using Pine.Core.Interpreter.IntermediateVM;
using Pine.Core.IO;
using Pine.Core.PineVM;
using System;
using System.Collections.Generic;
using System.Threading.Tasks;
using Xunit;

namespace Pine.Core.Tests.Interpreter.IntermediateVM;

public class InvocationCacheTests
{
    [Fact]
    public void Default_admission_configuration_preserves_strict_weighted_thresholds()
    {
        var configuration = InvocationCacheConfiguration.Default;

        configuration.ShouldOfferEntry(
            frameInstructionCount: 900,
            frameStackFrameCount: 1,
            instructionCountSinceLastEntry: 900,
            evalCountSinceLastEntry: 1)
            .Should().BeFalse();

        configuration.ShouldOfferEntry(
            frameInstructionCount: 901,
            frameStackFrameCount: 1,
            instructionCountSinceLastEntry: 901,
            evalCountSinceLastEntry: 1)
            .Should().BeTrue();
    }

    [Fact]
    public void Admission_configuration_applies_independent_weights_and_thresholds()
    {
        var configuration =
            new InvocationCacheConfiguration(
                frameCostThreshold: 10,
                stackFrameCost: 3,
                entrySpacingCostThreshold: 20,
                evalCost: 5);

        configuration.ShouldOfferEntry(
            frameInstructionCount: 5,
            frameStackFrameCount: 2,
            instructionCountSinceLastEntry: 11,
            evalCountSinceLastEntry: 2)
            .Should().BeTrue();

        configuration.ShouldOfferEntry(
            frameInstructionCount: 4,
            frameStackFrameCount: 2,
            instructionCountSinceLastEntry: 11,
            evalCountSinceLastEntry: 2)
            .Should().BeFalse();

        configuration.ShouldOfferEntry(
            frameInstructionCount: 5,
            frameStackFrameCount: 2,
            instructionCountSinceLastEntry: 10,
            evalCountSinceLastEntry: 2)
            .Should().BeFalse();
    }

    [Fact]
    public void Admission_configuration_handles_large_counters_without_overflow()
    {
        InvocationCacheConfiguration.Default.ShouldOfferEntry(
            frameInstructionCount: long.MaxValue,
            frameStackFrameCount: long.MaxValue,
            instructionCountSinceLastEntry: long.MaxValue,
            evalCountSinceLastEntry: long.MaxValue)
            .Should().BeTrue();
    }

    [Theory]
    [InlineData(-1, 0, 0, 0)]
    [InlineData(0, -1, 0, 0)]
    [InlineData(0, 0, -1, 0)]
    [InlineData(0, 0, 0, -1)]
    public void Admission_configuration_rejects_negative_values(
        int frameCostThreshold,
        int stackFrameCost,
        int entrySpacingCostThreshold,
        int evalCost)
    {
        var act =
            () =>
            new InvocationCacheConfiguration(
                frameCostThreshold,
                stackFrameCost,
                entrySpacingCostThreshold,
                evalCost);

        act.Should().Throw<ArgumentOutOfRangeException>();
    }

    [Fact]
    public void Dictionary_adapter_observes_external_additions_and_preserves_first_value()
    {
        var dictionary = new InvocationCache();
        var access = new InvocationCacheAccessFromDictionary(dictionary);
        var expression = PineValue.Blob([1]);

        var key =
            new EvalCacheEntryKey(
                expression,
                StackFrameInput.GenericFromEnvironmentValue(PineValue.Blob([2])));

        access.MayContainExpression(expression).Should().BeFalse();

        dictionary.Add(key, PineValue.Blob([3]));

        access.MayContainExpression(expression).Should().BeTrue();
        access.TryGet(key, out var value).Should().BeTrue();
        value.Should().Be(PineValue.Blob([3]));

        access.TryAdd(key, PineValue.Blob([4])).Should().BeFalse();
        dictionary[key].Should().Be(PineValue.Blob([3]));
    }

    [Fact]
    public void Buffered_cache_keeps_writes_local_until_merge()
    {
        var shared = new ConcurrentInvocationCache();
        var worker = new BufferedInvocationCacheAccess(shared);
        var key = BuildKey(1);
        var value = PineValue.Blob([2]);

        worker.TryAdd(key, value).Should().BeTrue();

        worker.TryGet(key, out var localValue).Should().BeTrue();
        localValue.Should().Be(value);
        shared.TryGet(key, out _).Should().BeFalse();

        worker.MergeIntoShared();

        worker.BufferedEntryCount.Should().Be(0);
        shared.TryGet(key, out var sharedValue).Should().BeTrue();
        sharedValue.Should().Be(value);
    }

    [Fact]
    public void Buffered_cache_reads_through_to_shared_entries()
    {
        var shared = new ConcurrentInvocationCache();
        var worker = new BufferedInvocationCacheAccess(shared);
        var key = BuildKey(1);
        var value = PineValue.Blob([2]);

        shared.TryAdd(key, value).Should().BeTrue();

        worker.MayContainExpression(key.ExprValue).Should().BeTrue();
        worker.TryGet(key, out var result).Should().BeTrue();
        result.Should().Be(value);
        worker.BufferedEntryCount.Should().Be(0);
    }

    [Fact]
    public async Task Completed_buffer_drops_its_expression_index_when_shared_entries_expire()
    {
        var shared = new GenerationalInvocationCache();
        var worker = new BufferedInvocationCacheAccess(shared, maxEntryCount: 1);
        var key = BuildKey(1);
        worker.TryAdd(key, PineValue.EmptyList).Should().BeTrue();
        worker.TryAdd(BuildKey(2), PineValue.EmptyList).Should().BeFalse();
        worker.MergeIntoShared();
        await shared.AdvanceGenerationAsync(1);
        worker.BufferedEntryCount.Should().Be(0);
        worker.MayContainExpression(key.ExprValue).Should().BeFalse();
    }

    [Fact]
    public void Buffer_publishes_one_batch_instead_of_individual_shared_insertions()
    {
        var shared = new BatchOnlyCache();
        var buffer = new BufferedInvocationCacheAccess(shared);
        buffer.TryAdd(BuildKey(1), PineValue.EmptyList);
        buffer.TryAdd(BuildKey(2), PineValue.EmptyList);
        buffer.MergeIntoShared();
        shared.MergeCount.Should().Be(1);
        shared.EntryCount.Should().Be(2);
        buffer.BufferedEntryCount.Should().Be(0);
        buffer.MergeIntoShared();
        shared.MergeCount.Should().Be(1, "an empty buffer must not acquire a shared-cache lock");
    }

    [Fact]
    public void Persistent_tier_forwards_the_whole_batch_to_the_memory_cache()
    {
        var memory = new BatchOnlyCache();
        var files = new FileStoreFromConcurrentDictionary();
        var key = BuildKey(1);

        var persistent =
            new PersistentInvocationCacheAccess(
                memory,
                files,
                PersistentOptimizationForExpression(key.ExprValue));

        var worker = new BufferedInvocationCacheAccess(persistent);
        worker.TryAdd(key, PineValue.Blob([3]));
        worker.TryAdd(BuildKey(2), PineValue.Blob([4]));

        worker.MergeIntoShared();

        memory.MergeCount.Should().Be(1);
        memory.EntryCount.Should().Be(2);
        files.ListFilesInDirectory([]).Should().HaveCount(2);
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void Concurrent_batch_merge_preserves_all_entries_and_first_writer(bool generational)
    {
        IInvocationCacheAccess cache =
            generational
            ?
            new GenerationalInvocationCache(new(2, 1))
            :
            new ConcurrentInvocationCache();

        var key = BuildKey(1);
        var first = PineValue.Blob([3]);
        cache.TryAdd(key, first);

        Parallel.For(
            0,
            10,
            index =>
            {
                cache.Merge(
                    new Dictionary<EvalCacheEntryKey, PineValue>
                    {
                        [key] = PineValue.Blob([4]),
                        [BuildKey(index + 2)] = PineValue.EmptyList
                    });
            });

        cache.TryGet(key, out var value).Should().BeTrue();
        value.Should().Be(first);

        for (var index = 2; index < 12; ++index)
            cache.TryGet(BuildKey(index), out _).Should().BeTrue();
    }

    private sealed class BatchOnlyCache : IInvocationCacheAccess
    {
        public int MergeCount { get; private set; }

        public int EntryCount { get; private set; }

        public bool MayContainExpression(PineValue expressionValue) => false;

        public bool TryGet(EvalCacheEntryKey key, out PineValue value)
        {
            value = null!;
            return false;
        }

        public bool TryAdd(EvalCacheEntryKey key, PineValue value) =>
            throw new InvalidOperationException("The buffer must use batch merge.");

        public void Merge(IReadOnlyDictionary<EvalCacheEntryKey, PineValue> entries)
        {
            ++MergeCount;
            EntryCount += entries.Count;
        }
    }

    [Fact]
    public void Merging_overlapping_buffers_is_idempotent_and_first_writer_wins()
    {
        var shared = new ConcurrentInvocationCache();
        var firstWorker = new BufferedInvocationCacheAccess(shared);
        var secondWorker = new BufferedInvocationCacheAccess(shared);
        var key = BuildKey(1);
        var firstValue = PineValue.Blob([2]);

        firstWorker.TryAdd(key, firstValue).Should().BeTrue();
        secondWorker.TryAdd(key, PineValue.Blob([3])).Should().BeTrue();

        firstWorker.MergeIntoShared();
        secondWorker.MergeIntoShared();

        shared.Count.Should().Be(1);
        shared.TryGet(key, out var result).Should().BeTrue();
        result.Should().Be(firstValue);

        firstWorker.MergeIntoShared();
        shared.Count.Should().Be(1);
    }

    [Fact]
    public void Concurrent_cache_supports_parallel_entry_publication()
    {
        var shared = new ConcurrentInvocationCache();

        Parallel.For(
            fromInclusive: 0,
            toExclusive: 1_000,
            index =>
            {
                var key = BuildKey(index);
                shared.TryAdd(key, PineValue.Blob(BitConverter.GetBytes(index))).Should().BeTrue();
            });

        shared.Count.Should().Be(1_000);

        Parallel.For(
            fromInclusive: 0,
            toExclusive: 1_000,
            index =>
            {
                shared.TryGet(BuildKey(index), out var value).Should().BeTrue();
                value.Should().Be(PineValue.Blob(BitConverter.GetBytes(index)));
            });
    }

    [Fact]
    public void Persistent_cache_stores_reads_and_promotes_eligible_entries()
    {
        var fileStore = new FileStoreFromConcurrentDictionary();
        var firstMemory = new InvocationCacheAccessFromDictionary(new InvocationCache());
        var key = BuildKey(1);
        var value = PineValue.Blob([2]);
        var optimization = PersistentOptimizationForExpression(key.ExprValue);
        var first = new PersistentInvocationCacheAccess(firstMemory, fileStore, optimization);

        first.TryAdd(key, value).Should().BeTrue();
        fileStore.ListFilesInDirectory([]).Should().ContainSingle();

        var secondMemory = new InvocationCacheAccessFromDictionary(new InvocationCache());
        var second = new PersistentInvocationCacheAccess(secondMemory, fileStore, optimization);

        second.MayContainExpression(key.ExprValue).Should().BeTrue();
        second.TryGet(key, out var result).Should().BeTrue();
        result.Should().Be(value);
        secondMemory.TryGet(key, out var promoted).Should().BeTrue();
        promoted.Should().Be(value);
    }

    [Fact]
    public void Persistent_cache_ignores_entries_without_matching_configuration()
    {
        var fileStore = new FileStoreFromConcurrentDictionary();

        var persistent =
            new PersistentInvocationCacheAccess(
                memoryCache: null,
                fileStore,
                OptimizationParametersSerial.Empty);

        var key = BuildKey(1);

        persistent.MayContainExpression(key.ExprValue).Should().BeFalse();
        persistent.TryAdd(key, PineValue.Blob([2])).Should().BeFalse();
        fileStore.ListFilesInDirectory([]).Should().BeEmpty();
    }

    private static EvalCacheEntryKey BuildKey(int argument) =>
        new(
            PineValue.Blob([1]),
            StackFrameInput.GenericFromEnvironmentValue(
                PineValue.Blob(BitConverter.GetBytes(argument))));

    private static OptimizationParametersSerial PersistentOptimizationForExpression(
        PineValue expressionValue)
    {
        var (hashBytes, _) = PineValueHashFlat.ComputeHashForValue(expressionValue);

        return
            new OptimizationParametersSerial(
                [
                    new OptimizationParametersSerial.ExpressionEntry(
                        Convert.ToHexStringLower(hashBytes.Span),
                        new OptimizationParametersSerial.ExpressionConfig(
                            PersistentCachePredicate:
                            OptimizationParametersSerial.InputPredicate.Unconditional,
                            ParallelThreadPredicate: null))
                ]);
    }
}
