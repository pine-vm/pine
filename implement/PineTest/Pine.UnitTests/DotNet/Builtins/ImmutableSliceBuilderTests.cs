using AwesomeAssertions;
using Pine.Core;
using Pine.Core.DotNet.Builtins;
using Pine.Core.PopularEncodings;
using System.Linq;
using Xunit;

namespace Pine.UnitTests.DotNet.Builtins;

public class ImmutableSliceBuilderTests
{
    private static PineValue KernelSkip(int skipCount, PineValue source) =>
        KernelSkip(
            skipCount: IntegerEncoding.EncodeSignedInteger(skipCount),
            source);

    private static PineValue KernelSkip(PineValue skipCount, PineValue source)
        => KernelFunction.skip(PineValue.List([skipCount, source]));

    private static PineValue KernelTake(int takeCount, PineValue source) =>
        KernelTake(
            takeCount: IntegerEncoding.EncodeSignedInteger(takeCount),
            source);

    private static PineValue KernelTake(PineValue takeCount, PineValue source)
        => KernelFunction.take(PineValue.List([takeCount, source]));

    [Fact]
    public void Create_initializes_with_full_range()
    {
        var original =
            PineValue.List(
                [
                PineValue.Blob([1]),
                PineValue.Blob([2]),
                PineValue.Blob([3])
                ]);

        var builder = ImmutableSliceBuilder.Create(original);

        builder.SkipCount.Should().Be(0);
        builder.TakeCount.Should().BeNull();
        builder.FinalValue.Should().BeNull();
        builder.Original.Should().Be(original);
    }

    [Fact]
    public void Evaluate_with_no_operations_returns_original()
    {
        var original =
            PineValue.List(
                [
                PineValue.Blob([1]),
                PineValue.Blob([2]),
                PineValue.Blob([3])
                ]);

        var builder = ImmutableSliceBuilder.Create(original);
        var result = builder.Evaluate();

        result.Should().Be(original);
    }

    [Fact]
    public void Skip_with_int_increases_skip_count()
    {
        var original =
            PineValue.List(
                [
                PineValue.Blob([1]),
                PineValue.Blob([2]),
                PineValue.Blob([3])
                ]);

        var builder =
            ImmutableSliceBuilder.Create(original)
            .Skip(1);

        builder.SkipCount.Should().Be(1);

        var result = builder.Evaluate();

        var expected = KernelSkip(1, original);

        result.Should().Be(expected);
    }

    [Fact]
    public void Skip_multiple_times_accumulates_skip_count()
    {
        var original =
            PineValue.List(
                [
                PineValue.Blob([1]),
                PineValue.Blob([2]),
                PineValue.Blob([3]),
                PineValue.Blob([4]),
                PineValue.Blob([5])
                ]);

        var builder =
            ImmutableSliceBuilder.Create(original)
            .Skip(1)
            .Skip(2);

        builder.SkipCount.Should().Be(3);

        var result = builder.Evaluate();

        var expected = KernelSkip(3, original);

        result.Should().Be(expected);
    }

    [Fact]
    public void Take_with_int_sets_take_count()
    {
        var original =
            PineValue.List(
                [
                PineValue.Blob([1]),
                PineValue.Blob([2]),
                PineValue.Blob([3]),
                PineValue.Blob([4]),
                PineValue.Blob([5])
                ]);

        var builder =
            ImmutableSliceBuilder.Create(original)
            .Take(3);

        builder.TakeCount.Should().Be(3);

        var result = builder.Evaluate();

        var expected = KernelTake(takeCount: 3, original);

        result.Should().Be(expected);
    }

    [Fact]
    public void Take_multiple_times_uses_minimum()
    {
        var original =
            PineValue.List(
                [
                PineValue.Blob([1]),
                PineValue.Blob([2]),
                PineValue.Blob([3]),
                PineValue.Blob([4]),
                PineValue.Blob([5])
                ]);

        var builder =
            ImmutableSliceBuilder.Create(original)
            .Take(4)
            .Take(2);

        builder.TakeCount.Should().Be(2);

        var result = builder.Evaluate();

        var expected = KernelTake(takeCount: 2, original);

        result.Should().Be(expected);
    }

    [Fact]
    public void Take_with_larger_count_keeps_smaller()
    {
        var original =
            PineValue.List(
                [
                PineValue.Blob([1]),
                PineValue.Blob([2]),
                PineValue.Blob([3])
                ]);

        var builder =
            ImmutableSliceBuilder.Create(original)
            .Take(2)
            .Take(5);

        builder.TakeCount.Should().Be(2);

        var result = builder.Evaluate();

        var expected = KernelTake(takeCount: 2, original);

        result.Should().Be(expected);
    }

    [Fact]
    public void Skip_after_take_lowers_effective_take()
    {
        var original =
            PineValue.List(
                [
                PineValue.Blob([1]),
                PineValue.Blob([2]),
                PineValue.Blob([3]),
                PineValue.Blob([4])
                ]);

        var builder =
            ImmutableSliceBuilder.Create(original)
            .Take(3)
            .Skip(1);

        builder.SkipCount.Should().Be(1);
        builder.TakeCount.Should().Be(2);

        var result = builder.Evaluate();

        var afterTake = KernelTake(takeCount: 3, original);
        var expected = KernelSkip(skipCount: 1, afterTake);

        result.Should().Be(expected);
        builder.GetLength().Should().Be(2);
    }

    [Fact]
    public void Skip_after_take_exceeding_available_returns_empty()
    {
        var original =
            PineValue.List(
                [
                PineValue.Blob([1]),
                PineValue.Blob([2]),
                PineValue.Blob([3])
                ]);

        var builder =
            ImmutableSliceBuilder.Create(original)
            .Take(2)
            .Skip(5);

        builder.SkipCount.Should().Be(5);
        builder.TakeCount.Should().Be(0);

        var result = builder.Evaluate();

        var afterTake = KernelTake(takeCount: 2, original);
        var expected = KernelSkip(skipCount: 5, afterTake);

        result.Should().Be(expected);
        builder.GetLength().Should().Be(0);
        builder.IsEmptyList().Should().BeTrue();
    }

    [Fact]
    public void Combined_skip_and_take_produces_correct_slice()
    {
        var original =
            PineValue.List(
                [
                PineValue.Blob([1]),
                PineValue.Blob([2]),
                PineValue.Blob([3]),
                PineValue.Blob([4]),
                PineValue.Blob([5])
                ]);

        var builder =
            ImmutableSliceBuilder.Create(original)
            .Skip(1)
            .Take(3);

        var result = builder.Evaluate();

        // First skip, then take

        var afterSkip = KernelSkip(skipCount: 1, original);

        var expected = KernelTake(takeCount: 3, afterSkip);

        result.Should().Be(expected);
    }

    [Fact]
    public void Skip_beyond_list_length_returns_empty()
    {
        var original =
            PineValue.List(
                [
                PineValue.Blob([1]),
                PineValue.Blob([2])
                ]);

        var builder =
            ImmutableSliceBuilder.Create(original)
            .Skip(10);

        var result = builder.Evaluate();

        var expected = KernelSkip(skipCount: 10, original);

        result.Should().Be(expected);
    }

    [Fact]
    public void Take_zero_returns_empty()
    {
        var original =
            PineValue.List(
                [
                PineValue.Blob([1]),
                PineValue.Blob([2]),
                PineValue.Blob([3])
                ]);

        var builder =
            ImmutableSliceBuilder.Create(original)
            .Take(0);

        var result = builder.Evaluate();

        var expected = KernelTake(0, original);

        result.Should().Be(expected);
    }

    [Fact]
    public void Skip_with_PineValue_parses_integer()
    {
        var original =
            PineValue.List(
                [
                PineValue.Blob([1]),
                PineValue.Blob([2]),
                PineValue.Blob([3]),
                PineValue.Blob([4])
                ]);

        var skipValue = IntegerEncoding.EncodeSignedInteger(2);
        var builder = ImmutableSliceBuilder.Create(original).Skip(skipValue);

        var result = builder.Evaluate();
        var expected = KernelSkip(skipValue, original);

        result.Should().Be(expected);
    }

    [Fact]
    public void Skip_with_negative_PineValue_treats_as_zero()
    {
        var original =
            PineValue.List(
                [
                PineValue.Blob([1]),
                PineValue.Blob([2]),
                PineValue.Blob([3])
                ]);

        var skipValue = IntegerEncoding.EncodeSignedInteger(-1);
        var builder = ImmutableSliceBuilder.Create(original).Skip(skipValue);

        builder.SkipCount.Should().Be(0);
        var result = builder.Evaluate();

        result.Should().Be(original);
    }

    [Fact]
    public void Skip_with_invalid_PineValue_returns_empty_list()
    {
        var original =
            PineValue.List(
                [
                PineValue.Blob([1]),
                PineValue.Blob([2]),
                PineValue.Blob([3])
                ]);

        var skipValue = PineValue.List([PineValue.Blob([1])]); // Invalid for integer
        var builder = ImmutableSliceBuilder.Create(original).Skip(skipValue);

        builder.FinalValue.Should().Be(PineValue.EmptyList);
        var result = builder.Evaluate();

        result.Should().Be(PineValue.EmptyList);
    }

    [Fact]
    public void Take_with_PineValue_parses_integer()
    {
        var original = PineValue.List([
            PineValue.Blob([1]),
            PineValue.Blob([2]),
            PineValue.Blob([3]),
            PineValue.Blob([4]),
            PineValue.Blob([5])
        ]);

        var takeValue = IntegerEncoding.EncodeSignedInteger(3);
        var builder = ImmutableSliceBuilder.Create(original).Take(takeValue);

        var result = builder.Evaluate();
        var expected = KernelTake(takeValue, original);

        result.Should().Be(expected);
    }

    [Fact]
    public void Take_with_negative_PineValue_treats_as_zero()
    {
        var original = PineValue.List([
            PineValue.Blob([1]),
            PineValue.Blob([2]),
            PineValue.Blob([3])
        ]);

        var takeValue = IntegerEncoding.EncodeSignedInteger(-1);
        var builder = ImmutableSliceBuilder.Create(original).Take(takeValue);

        builder.TakeCount.Should().Be(0);
        var result = builder.Evaluate();

        var expected = KernelTake(0, original);
        result.Should().Be(expected);
    }

    [Fact]
    public void Take_with_invalid_PineValue_returns_empty_list()
    {
        var original =
            PineValue.List(
                [
                PineValue.Blob([1]),
                PineValue.Blob([2]),
                PineValue.Blob([3])
                ]);

        var takeValue = PineValue.List([PineValue.Blob([1])]); // Invalid for integer

        var builder =
            ImmutableSliceBuilder.Create(original)
            .Take(takeValue);

        builder.FinalValue.Should().Be(PineValue.EmptyList);

        var result = builder.Evaluate();

        result.Should().Be(PineValue.EmptyList);
    }

    [Fact]
    public void GetLength_returns_correct_length_for_list()
    {
        var original =
            PineValue.List(
                [
                PineValue.Blob([1]),
                PineValue.Blob([2]),
                PineValue.Blob([3]),
                PineValue.Blob([4]),
                PineValue.Blob([5])
                ]);

        var builder =
            ImmutableSliceBuilder.Create(original)
            .Skip(1)
            .Take(3);

        builder.GetLength().Should().Be(3);
    }

    [Fact]
    public void GetLength_for_empty_slice_returns_zero()
    {
        var original =
            PineValue.List(
                [
                PineValue.Blob([1]),
                PineValue.Blob([2])
                ]);

        var builder =
            ImmutableSliceBuilder.Create(original)
            .Skip(5);

        builder.GetLength().Should().Be(0);
    }

    [Fact]
    public void GetLength_with_blob_value()
    {
        var original = PineValue.Blob([1, 2, 3, 4, 5]);

        var builder =
            ImmutableSliceBuilder.Create(original)
            .Skip(1)
            .Take(3);

        builder.GetLength().Should().Be(3);
    }

    [Fact]
    public void IsEmptyList_returns_true_for_empty_slice()
    {
        var original =
            PineValue.List(
                [
                PineValue.Blob([1]),
                PineValue.Blob([2])
                ]);

        var builder =
            ImmutableSliceBuilder.Create(original)
            .Skip(10);

        builder.IsEmptyList().Should().BeTrue();
    }

    [Fact]
    public void IsEmptyList_returns_false_for_non_empty_slice()
    {
        var original =
            PineValue.List(
                [
                PineValue.Blob([1]),
                PineValue.Blob([2]),
                PineValue.Blob([3])
                ]);

        var builder =
            ImmutableSliceBuilder.Create(original)
            .Skip(1);

        builder.IsEmptyList().Should().BeFalse();
    }

    [Fact]
    public void IsEmptyList_with_take_zero()
    {
        var original =
            PineValue.List(
                [
                PineValue.Blob([1]),
                PineValue.Blob([2])
                ]);

        var builder =
            ImmutableSliceBuilder.Create(original)
            .Take(0);

        builder.IsEmptyList().Should().BeTrue();
    }

    [Fact]
    public void IsEmptyList_returns_false_for_blob_values()
    {
        var original = PineValue.Blob([1, 2, 3]);

        var builder =
            ImmutableSliceBuilder.Create(original)
            .Skip(10);

        // IsEmptyList returns false for blobs even when empty
        builder.IsEmptyList().Should().BeFalse();
    }

    [Fact]
    public void GetHead_returns_first_element_of_slice()
    {
        var original =
            PineValue.List(
                [
                PineValue.Blob([1]),
                PineValue.Blob([2]),
                PineValue.Blob([3]),
                PineValue.Blob([4])
                ]);

        var builder =
            ImmutableSliceBuilder.Create(original)
            .Skip(1);

        var head = builder.GetHead();

        head.Should().Be(PineValue.Blob([2]));
    }

    [Fact]
    public void GetHead_with_take_operation()
    {
        var original =
            PineValue.List(
                [
                PineValue.Blob([1]),
                PineValue.Blob([2]),
                PineValue.Blob([3])
                ]);

        var builder =
            ImmutableSliceBuilder.Create(original)
            .Skip(0)
            .Take(2);

        var head = builder.GetHead();

        head.Should().Be(PineValue.Blob([1]));
    }

    [Fact]
    public void GetHead_on_empty_slice_returns_empty_list()
    {
        var original =
            PineValue.List(
                [
                PineValue.Blob([1])
                ]);

        var builder =
            ImmutableSliceBuilder.Create(original)
            .Skip(10);

        var head = builder.GetHead();

        head.Should().Be(PineValue.EmptyList);
    }

    [Fact]
    public void GetHead_with_blob_value()
    {
        var original = PineValue.Blob([1, 2, 3, 4]);

        var builder =
            ImmutableSliceBuilder.Create(original)
            .Skip(1);

        var head = builder.GetHead();

        head.Should().Be(PineValue.Blob([2]));
    }

    [Fact]
    public void Slice_with_blob_value_returns_blob()
    {
        var original = PineValue.Blob([1, 2, 3, 4, 5]);

        var builder =
            ImmutableSliceBuilder.Create(original)
            .Skip(1)
            .Take(3);

        var result = builder.Evaluate();

        // First skip, then take
        var afterSkip = KernelSkip(skipCount: 1, original);

        var expected = KernelTake(takeCount: 3, afterSkip);

        result.Should().Be(expected);
    }

    [Fact]
    public void Slice_entire_blob_returns_original()
    {
        var original = PineValue.Blob([1, 2, 3]);

        var builder = ImmutableSliceBuilder.Create(original);
        var result = builder.Evaluate();

        result.Should().Be(original);
    }

    [Fact]
    public void Slice_empty_list_returns_empty_list()
    {
        var original = PineValue.EmptyList;

        var builder = ImmutableSliceBuilder.Create(original)
            .Skip(0)
            .Take(5);

        var result = builder.Evaluate();

        var afterSkip = KernelSkip(0, original);
        var expected = KernelTake(5, afterSkip);

        result.Should().Be(expected);
    }

    [Fact]
    public void Slice_empty_blob_returns_empty_blob()
    {
        var original = PineValue.EmptyBlob;

        var builder = ImmutableSliceBuilder.Create(original)
            .Skip(0)
            .Take(5);

        var result = builder.Evaluate();

        var afterSkip = KernelSkip(0, original);
        var expected = KernelTake(5, afterSkip);

        result.Should().Be(expected);
    }

    [Fact]
    public void Builder_is_immutable_original_unchanged_after_skip()
    {
        var original_value = PineValue.List([
            PineValue.Blob([1]),
            PineValue.Blob([2]),
            PineValue.Blob([3])
        ]);

        var original = ImmutableSliceBuilder.Create(original_value);
        var modified = original.Skip(1);

        original.SkipCount.Should().Be(0);
        modified.SkipCount.Should().Be(1);

        var originalResult = original.Evaluate();
        var modifiedResult = modified.Evaluate();

        originalResult.Should().Be(original_value);

        var expectedModified = KernelSkip(skipCount: 1, original_value);

        modifiedResult.Should().Be(expectedModified);
    }

    [Fact]
    public void Builder_is_immutable_original_unchanged_after_take()
    {
        var original_value =
            PineValue.List(
                [
                PineValue.Blob([1]),
                PineValue.Blob([2]),
                PineValue.Blob([3])
                ]);

        var original = ImmutableSliceBuilder.Create(original_value);
        var modified = original.Take(2);

        original.TakeCount.Should().BeNull();
        modified.TakeCount.Should().Be(2);

        var originalResult = original.Evaluate();
        var modifiedResult = modified.Evaluate();

        originalResult.Should().Be(original_value);

        var expectedModified = KernelTake(takeCount: 2, original_value);
        modifiedResult.Should().Be(expectedModified);
    }

    [Fact]
    public void Complex_chaining_with_multiple_operations()
    {
        var original =
            PineValue.List(
                [.. Enumerable.Range(1, 20).Select(i => PineValue.Blob([(byte)i]))
                ]);

        var builder =
            ImmutableSliceBuilder.Create(original)
            .Skip(2)
            .Take(15)
            .Skip(3)
            .Take(8);

        // Skip 2, then skip 3 more = skip 5
        // Take 15, then take 8 = take 8
        builder.SkipCount.Should().Be(5);
        builder.TakeCount.Should().Be(8);

        var result = builder.Evaluate();

        // Chain the operations as they would be with KernelFunction

        var afterSkip = KernelSkip(skipCount: 5, original);

        var expected = KernelTake(takeCount: 8, afterSkip);

        result.Should().Be(expected);
    }

    [Fact]
    public void Skip_exact_list_length_returns_empty()
    {
        var original =
            PineValue.List(
                [
                PineValue.Blob([1]),
                PineValue.Blob([2]),
                PineValue.Blob([3])
                ]);

        var builder =
            ImmutableSliceBuilder.Create(original)
            .Skip(3);

        var result = builder.Evaluate();

        var expected = KernelSkip(skipCount: 3, original);

        result.Should().Be(expected);
    }

    [Fact]
    public void Take_more_than_available_returns_all_remaining()
    {
        var original =
            PineValue.List(
                [
                PineValue.Blob([1]),
                PineValue.Blob([2]),
                PineValue.Blob([3])
                ]);

        var builder =
            ImmutableSliceBuilder.Create(original)
            .Skip(1)
            .Take(100);

        var result = builder.Evaluate();

        var afterSkip = KernelSkip(skipCount: 1, original);

        var expected = KernelTake(takeCount: 100, afterSkip);

        result.Should().Be(expected);
    }

    [Fact]
    public void GetLength_with_take_larger_than_remaining()
    {
        var original =
            PineValue.List(
                [
                PineValue.Blob([1]),
                PineValue.Blob([2]),
                PineValue.Blob([3]),
                PineValue.Blob([4]),
                PineValue.Blob([5])
                ]);

        var builder =
            ImmutableSliceBuilder.Create(original)
            .Skip(3)
            .Take(100);

        builder.GetLength().Should().Be(2);
    }

    [Fact]
    public void Large_list_slicing_operations()
    {
        var items =
            Enumerable.Range(0, 1000)
            .Select(i => PineValue.Blob([(byte)(i % 256)]))
            .ToArray();

        var original = PineValue.List(items);

        var builder =
            ImmutableSliceBuilder.Create(original)
            .Skip(100)
            .Take(500);

        builder.GetLength().Should().Be(500);

        var result = builder.Evaluate();

        var afterSkip = KernelSkip(skipCount: 100, original);

        var expected = KernelTake(takeCount: 500, afterSkip);

        result.Should().Be(expected);
    }

    [Fact]
    public void FinalValue_caching_when_set_to_empty()
    {
        var original =
            PineValue.List(
                [
                PineValue.Blob([1]),
                PineValue.Blob([2])
                ]);

        var skipValue = PineValue.List([PineValue.Blob([1])]); // Invalid

        var builder =
            ImmutableSliceBuilder.Create(original)
            .Skip(skipValue);

        builder.FinalValue.Should().Be(PineValue.EmptyList);
        builder.GetLength().Should().Be(0);
        builder.IsEmptyList().Should().BeTrue();
    }

    [Fact]
    public void GetHead_uses_cached_final_value()
    {
        var original =
            PineValue.List(
                [
                PineValue.Blob([1]),
                PineValue.Blob([2])
                ]);

        var takeValue = PineValue.List([PineValue.Blob([1])]); // Invalid

        var builder =
            ImmutableSliceBuilder.Create(original)
            .Take(takeValue);

        var head = builder.GetHead();

        var expected = KernelFunction.head(PineValue.EmptyList);
        head.Should().Be(expected);
    }

    [Fact]
    public void Skip_and_take_with_blob_producing_single_byte()
    {
        var original = PineValue.Blob([10, 20, 30, 40, 50]);

        var builder =
            ImmutableSliceBuilder.Create(original)
            .Skip(2)
            .Take(1);

        var result = builder.Evaluate();

        var afterSkip = KernelSkip(2, original);
        var expected = KernelTake(1, afterSkip);

        result.Should().Be(expected);
    }

    [Fact]
    public void Skip_and_take_with_blob_producing_empty()
    {
        var original = PineValue.Blob([10, 20, 30]);

        var builder =
            ImmutableSliceBuilder.Create(original)
            .Skip(10)
            .Take(5);

        var result = builder.Evaluate();

        var afterSkip = KernelSkip(10, original);
        var expected = KernelTake(5, afterSkip);

        result.Should().Be(expected);
    }

    [Fact]
    public void Negative_skip_count_in_builder_handled_correctly()
    {
        var original =
            PineValue.List(
                [
                PineValue.Blob([1]),
                PineValue.Blob([2]),
                PineValue.Blob([3])
                ]);

        // Create a builder with negative skip (shouldn't happen in practice but testing robustness)
        var builder =
            new ImmutableSliceBuilder(
                Original: original,
                SkipCount: -5,
                TakeCount: 2,
                FinalValue: null);

        // GetLength should treat negative as 0
        builder.GetLength().Should().Be(2);

        var result = builder.Evaluate();

        // Negative skip is treated as 0, so just take 2

        var expected = KernelTake(takeCount: 2, original);

        result.Should().Be(expected);
    }
}
