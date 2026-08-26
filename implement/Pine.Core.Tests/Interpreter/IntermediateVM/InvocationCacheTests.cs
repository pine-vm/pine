using AwesomeAssertions;
using Pine.Core.Addressing;
using Pine.Core.Interpreter.IntermediateVM;
using Pine.Core.IO;
using Pine.Core.PineVM;
using System;
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
            frameInstructionCount: 600,
            frameStackFrameCount: 1,
            instructionCountSinceLastEntry: 600,
            evalCountSinceLastEntry: 1)
            .Should().BeFalse();

        configuration.ShouldOfferEntry(
            frameInstructionCount: 601,
            frameStackFrameCount: 1,
            instructionCountSinceLastEntry: 601,
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
