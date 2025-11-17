using AwesomeAssertions;
using Pine.Core.CommonEncodings;
using Pine.Core.DotNet.Builtins;
using System.Linq;
using Xunit;

namespace Pine.Core.Tests.DotNet.Builtins;

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

    [Fact]
    public void GetElementAt_returns_element_at_index()
    {
        var original =
            PineValue.List(
                [
                PineValue.Blob([10]),
                PineValue.Blob([20]),
                PineValue.Blob([30]),
                PineValue.Blob([40])
                ]);

        var builder = ImmutableSliceBuilder.Create(original);

        builder.GetElementAt(0).Should().Be(PineValue.Blob([10]));
        builder.GetElementAt(1).Should().Be(PineValue.Blob([20]));
        builder.GetElementAt(2).Should().Be(PineValue.Blob([30]));
        builder.GetElementAt(3).Should().Be(PineValue.Blob([40]));
    }

    [Fact]
    public void GetElementAt_with_skip_operation()
    {
        var original =
            PineValue.List(
                [
                PineValue.Blob([10]),
                PineValue.Blob([20]),
                PineValue.Blob([30]),
                PineValue.Blob([40]),
                PineValue.Blob([50])
                ]);

        var builder =
            ImmutableSliceBuilder.Create(original)
            .Skip(2);

        // After skip(2), the slice is [30, 40, 50]
        builder.GetElementAt(0).Should().Be(PineValue.Blob([30]));
        builder.GetElementAt(1).Should().Be(PineValue.Blob([40]));
        builder.GetElementAt(2).Should().Be(PineValue.Blob([50]));
    }

    [Fact]
    public void GetElementAt_with_take_operation()
    {
        var original =
            PineValue.List(
                [
                PineValue.Blob([10]),
                PineValue.Blob([20]),
                PineValue.Blob([30]),
                PineValue.Blob([40]),
                PineValue.Blob([50])
                ]);

        var builder =
            ImmutableSliceBuilder.Create(original)
            .Take(3);

        // After take(3), the slice is [10, 20, 30]
        builder.GetElementAt(0).Should().Be(PineValue.Blob([10]));
        builder.GetElementAt(1).Should().Be(PineValue.Blob([20]));
        builder.GetElementAt(2).Should().Be(PineValue.Blob([30]));
    }

    [Fact]
    public void GetElementAt_with_skip_and_take()
    {
        var original =
            PineValue.List(
                [
                PineValue.Blob([10]),
                PineValue.Blob([20]),
                PineValue.Blob([30]),
                PineValue.Blob([40]),
                PineValue.Blob([50]),
                PineValue.Blob([60])
                ]);

        var builder =
            ImmutableSliceBuilder.Create(original)
            .Skip(1)
            .Take(3);

        // After skip(1).take(3), the slice is [20, 30, 40]
        builder.GetElementAt(0).Should().Be(PineValue.Blob([20]));
        builder.GetElementAt(1).Should().Be(PineValue.Blob([30]));
        builder.GetElementAt(2).Should().Be(PineValue.Blob([40]));
    }

    [Fact]
    public void GetElementAt_out_of_bounds_returns_empty_list()
    {
        var original =
            PineValue.List(
                [
                PineValue.Blob([10]),
                PineValue.Blob([20]),
                PineValue.Blob([30])
                ]);

        var builder = ImmutableSliceBuilder.Create(original);

        builder.GetElementAt(10).Should().Be(PineValue.EmptyList);
        builder.GetElementAt(100).Should().Be(PineValue.EmptyList);
    }

    [Fact]
    public void GetElementAt_negative_index()
    {
        var original =
            PineValue.List(
                [
                PineValue.Blob([10]),
                PineValue.Blob([20]),
                PineValue.Blob([30])
                ]);

        var builder = ImmutableSliceBuilder.Create(original);

        builder.GetElementAt(-1).Should().Be(PineValue.Blob([10]));
        builder.GetElementAt(-10).Should().Be(PineValue.Blob([10]));
    }

    [Fact]
    public void GetElementAt_on_empty_slice_returns_empty_list()
    {
        var original =
            PineValue.List(
                [
                PineValue.Blob([10]),
                PineValue.Blob([20])
                ]);

        var builder =
            ImmutableSliceBuilder.Create(original)
            .Skip(10);

        builder.GetElementAt(0).Should().Be(PineValue.EmptyList);
        builder.GetElementAt(1).Should().Be(PineValue.EmptyList);
    }

    [Fact]
    public void GetElementAt_with_blob_value()
    {
        var original = PineValue.Blob([10, 20, 30, 40, 50]);

        var builder = ImmutableSliceBuilder.Create(original);

        builder.GetElementAt(0).Should().Be(PineValue.Blob([10]));
        builder.GetElementAt(1).Should().Be(PineValue.Blob([20]));
        builder.GetElementAt(2).Should().Be(PineValue.Blob([30]));
        builder.GetElementAt(3).Should().Be(PineValue.Blob([40]));
        builder.GetElementAt(4).Should().Be(PineValue.Blob([50]));
    }

    [Fact]
    public void GetElementAt_with_blob_value_and_skip()
    {
        var original = PineValue.Blob([10, 20, 30, 40, 50]);

        var builder =
            ImmutableSliceBuilder.Create(original)
            .Skip(2);

        // After skip(2), the slice is [30, 40, 50]
        builder.GetElementAt(0).Should().Be(PineValue.Blob([30]));
        builder.GetElementAt(1).Should().Be(PineValue.Blob([40]));
        builder.GetElementAt(2).Should().Be(PineValue.Blob([50]));
    }

    [Fact]
    public void GetElementAt_blob_out_of_bounds_returns_empty_blob()
    {
        var original = PineValue.Blob([10, 20, 30]);

        var builder = ImmutableSliceBuilder.Create(original);

        builder.GetElementAt(10).Should().Be(PineValue.EmptyBlob);
    }

    [Fact]
    public void GetElementAt_blob_negative_index()
    {
        var original = PineValue.Blob([10, 20, 30]);

        var builder = ImmutableSliceBuilder.Create(original);

        builder.GetElementAt(-1).Should().Be(PineValue.BlobSingleByte(10));
    }

    [Fact]
    public void GetElementAt_uses_cached_final_value()
    {
        var original =
            PineValue.List(
                [
                PineValue.Blob([10]),
                PineValue.Blob([20]),
                PineValue.Blob([30])
                ]);

        var takeValue = PineValue.List([PineValue.Blob([1])]); // Invalid

        var builder =
            ImmutableSliceBuilder.Create(original)
            .Take(takeValue);

        // FinalValue is cached as EmptyList due to invalid take value
        var elementAt0 = builder.GetElementAt(0);
        var elementAt1 = builder.GetElementAt(1);

        elementAt0.Should().Be(PineValue.EmptyList);
        elementAt1.Should().Be(PineValue.EmptyList);
    }

    [Fact]
    public void GetElementAt_is_equivalent_to_GetHead_for_index_zero()
    {
        var original =
            PineValue.List(
                [
                PineValue.Blob([10]),
                PineValue.Blob([20]),
                PineValue.Blob([30])
                ]);

        var builder =
            ImmutableSliceBuilder.Create(original)
            .Skip(1);

        var head = builder.GetHead();
        var elementAt0 = builder.GetElementAt(0);

        elementAt0.Should().Be(head);

        VerifyConsistencyOfDerivedProperties(builder);
    }

    [Fact]
    public void GetElementAt_with_multiple_skip_operations()
    {
        var original =
            PineValue.List(
                [.. Enumerable.Range(0, 20).Select(i => PineValue.Blob([(byte)i]))
                ]);

        var builder =
            ImmutableSliceBuilder.Create(original)
            .Skip(3)
            .Skip(2)
            .Skip(1);

        // Total skip is 6, so slice starts at element 6
        builder.GetElementAt(0).Should().Be(PineValue.Blob([6]));
        builder.GetElementAt(1).Should().Be(PineValue.Blob([7]));
        builder.GetElementAt(2).Should().Be(PineValue.Blob([8]));

        VerifyConsistencyOfDerivedProperties(builder);
    }

    [Fact]
    public void GetElementAt_respects_take_limit()
    {
        var original =
            PineValue.List(
                [
                PineValue.Blob([10]),
                PineValue.Blob([20]),
                PineValue.Blob([30]),
                PineValue.Blob([40]),
                PineValue.Blob([50])
                ]);

        var builder =
            ImmutableSliceBuilder.Create(original)
            .Skip(1)
            .Take(2);

        // After skip(1).take(2), the slice is [20, 30]
        builder.GetElementAt(0).Should().Be(PineValue.Blob([20]));
        builder.GetElementAt(1).Should().Be(PineValue.Blob([30]));

        // Index 2 is beyond the take limit
        builder.GetElementAt(2).Should().Be(PineValue.EmptyList);

        VerifyConsistencyOfDerivedProperties(builder);
    }

    [Fact]
    public void GetElementAt_with_complex_chaining()
    {
        var original =
            PineValue.List(
                [.. Enumerable.Range(1, 30).Select(i => PineValue.Blob([(byte)i]))
                ]);

        var builder =
            ImmutableSliceBuilder.Create(original)
            .Skip(5)
            .Take(20)
            .Skip(3)
            .Take(10);

        // Skip 5, then skip 3 more = skip 8
        // Take 20, then take 10 = take 10
        // So the slice is elements [8..17] (0-indexed), which are values 9..18

        builder.GetElementAt(0).Should().Be(PineValue.Blob([9]));
        builder.GetElementAt(1).Should().Be(PineValue.Blob([10]));
        builder.GetElementAt(9).Should().Be(PineValue.Blob([18]));
        builder.GetElementAt(10).Should().Be(PineValue.EmptyList);

        VerifyConsistencyOfDerivedProperties(builder);
    }

    [Fact]
    public void IsList_returns_true_for_list_original()
    {
        var original =
            PineValue.List(
                [
                PineValue.Blob([1]),
                PineValue.Blob([2]),
                PineValue.Blob([3])
                ]);

        var builder = ImmutableSliceBuilder.Create(original);

        VerifyConsistencyOfDerivedProperties(builder);
    }

    [Fact]
    public void IsList_returns_false_for_blob_original()
    {
        var original = PineValue.Blob([1, 2, 3]);

        var builder = ImmutableSliceBuilder.Create(original);

        VerifyConsistencyOfDerivedProperties(builder);
    }

    [Fact]
    public void IsList_returns_true_for_list_after_skip_and_take()
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
            .Take(1);

        VerifyConsistencyOfDerivedProperties(builder);
    }

    [Fact]
    public void IsList_respects_final_value_when_cached()
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

        // Skip(10) on a 2-element list should result in empty list (final value cached)
        VerifyConsistencyOfDerivedProperties(builder);
    }

    [Fact]
    public void IsBlob_returns_true_for_blob_original()
    {
        var original = PineValue.Blob([1, 2, 3]);

        var builder = ImmutableSliceBuilder.Create(original);

        VerifyConsistencyOfDerivedProperties(builder);
    }

    [Fact]
    public void IsBlob_returns_false_for_list_original()
    {
        var original =
            PineValue.List(
                [
                PineValue.Blob([1]),
                PineValue.Blob([2]),
                PineValue.Blob([3])
                ]);

        var builder = ImmutableSliceBuilder.Create(original);

        VerifyConsistencyOfDerivedProperties(builder);
    }

    [Fact]
    public void IsBlob_returns_true_for_blob_after_skip_and_take()
    {
        var original = PineValue.Blob([1, 2, 3, 4, 5]);

        var builder =
            ImmutableSliceBuilder.Create(original)
            .Skip(1)
            .Take(3);

        VerifyConsistencyOfDerivedProperties(builder);
    }

    [Fact]
    public void IsBlob_respects_final_value_when_cached()
    {
        var original = PineValue.Blob([1, 2, 3]);

        var builder =
            ImmutableSliceBuilder.Create(original)
            .Skip(10);

        VerifyConsistencyOfDerivedProperties(builder);
    }

    [Fact]
    public void IsList_and_IsBlob_are_mutually_exclusive()
    {
        var listOriginal = PineValue.List([PineValue.Blob([1])]);
        var blobOriginal = PineValue.Blob([1, 2, 3]);

        var listBuilder = ImmutableSliceBuilder.Create(listOriginal);
        var blobBuilder = ImmutableSliceBuilder.Create(blobOriginal);

        VerifyConsistencyOfDerivedProperties(listBuilder);
        VerifyConsistencyOfDerivedProperties(blobBuilder);
    }

    [Fact]
    public void TakeLast_with_int_takes_from_end()
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
            .TakeLast(2);

        var result = builder.Evaluate();

        // TakeLast(2) should give us [4, 5]
        var expected =
            PineValue.List(
                [
                PineValue.Blob([4]),
                PineValue.Blob([5])
                ]);

        result.Should().Be(expected);
        builder.GetLength().Should().Be(2);
    }

    [Fact]
    public void TakeLast_more_than_available_returns_all()
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
            .TakeLast(10);

        var result = builder.Evaluate();

        result.Should().Be(original);
    }

    [Fact]
    public void TakeLast_zero_returns_empty()
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
            .TakeLast(0);

        var result = builder.Evaluate();

        result.Should().Be(PineValue.EmptyList);
        builder.GetLength().Should().Be(0);
    }

    [Fact]
    public void TakeLast_with_negative_count_treats_as_zero()
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
            .TakeLast(-5);

        var result = builder.Evaluate();

        result.Should().Be(PineValue.EmptyList);
    }

    [Fact]
    public void TakeLast_after_skip_takes_from_end_of_remaining()
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
            .TakeLast(2);

        var result = builder.Evaluate();

        // After skip(1), we have [2, 3, 4, 5]
        // TakeLast(2) should give us [4, 5]
        var expected =
            PineValue.List(
                [
                PineValue.Blob([4]),
                PineValue.Blob([5])
                ]);

        result.Should().Be(expected);
        builder.GetLength().Should().Be(2);
    }

    [Fact]
    public void TakeLast_after_take_takes_from_end_of_limited_range()
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
            .TakeLast(2);

        var result = builder.Evaluate();

        // After take(4), we have [1, 2, 3, 4]
        // TakeLast(2) should give us [3, 4]
        var expected =
            PineValue.List(
                [
                PineValue.Blob([3]),
                PineValue.Blob([4])
                ]);

        result.Should().Be(expected);
        builder.GetLength().Should().Be(2);
    }

    [Fact]
    public void TakeLast_with_blob_value()
    {
        var original = PineValue.Blob([1, 2, 3, 4, 5]);

        var builder =
            ImmutableSliceBuilder.Create(original)
            .TakeLast(3);

        var result = builder.Evaluate();

        // TakeLast(3) should give us [3, 4, 5]
        var expected = PineValue.Blob([3, 4, 5]);

        result.Should().Be(expected);
        builder.GetLength().Should().Be(3);
    }

    [Fact]
    public void TakeLast_with_PineValue_parses_integer()
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

        var takeValue = IntegerEncoding.EncodeSignedInteger(3);
        var builder =
            ImmutableSliceBuilder.Create(original)
            .TakeLast(takeValue);

        var result = builder.Evaluate();

        // TakeLast(3) should give us [3, 4, 5]
        var expected =
            PineValue.List(
                [
                PineValue.Blob([3]),
                PineValue.Blob([4]),
                PineValue.Blob([5])
                ]);

        result.Should().Be(expected);
    }

    [Fact]
    public void TakeLast_with_negative_PineValue_treats_as_zero()
    {
        var original =
            PineValue.List(
                [
                PineValue.Blob([1]),
                PineValue.Blob([2]),
                PineValue.Blob([3])
                ]);

        var takeValue = IntegerEncoding.EncodeSignedInteger(-1);
        var builder =
            ImmutableSliceBuilder.Create(original)
            .TakeLast(takeValue);

        var result = builder.Evaluate();

        result.Should().Be(PineValue.EmptyList);
    }

    [Fact]
    public void TakeLast_with_invalid_PineValue_returns_empty_list()
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
            .TakeLast(takeValue);

        builder.FinalValue.Should().Be(PineValue.EmptyList);

        var result = builder.Evaluate();

        result.Should().Be(PineValue.EmptyList);
    }

    [Fact]
    public void TakeLast_complex_chaining()
    {
        var original =
            PineValue.List(
                [.. Enumerable.Range(1, 20).Select(i => PineValue.Blob([(byte)i]))
                ]);

        var builder =
            ImmutableSliceBuilder.Create(original)
            .Skip(5)
            .Take(10)
            .TakeLast(3);

        // Skip 5: [6..20]
        // Take 10: [6..15]
        // TakeLast 3: [13, 14, 15]

        var result = builder.Evaluate();

        var expected =
            PineValue.List(
                [
                PineValue.Blob([13]),
                PineValue.Blob([14]),
                PineValue.Blob([15])
                ]);

        result.Should().Be(expected);
        builder.GetLength().Should().Be(3);
    }

    [Fact]
    public void TakeLast_on_empty_list_returns_empty()
    {
        var original = PineValue.EmptyList;

        var builder =
            ImmutableSliceBuilder.Create(original)
            .TakeLast(5);

        var result = builder.Evaluate();

        result.Should().Be(PineValue.EmptyList);
    }

    [Fact]
    public void TakeLast_on_empty_blob_returns_empty_blob()
    {
        var original = PineValue.EmptyBlob;

        var builder =
            ImmutableSliceBuilder.Create(original)
            .TakeLast(5);

        var result = builder.Evaluate();

        result.Should().Be(PineValue.EmptyBlob);
    }

    [Fact]
    public void TakeLast_exact_length_returns_all()
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
            .TakeLast(3);

        var result = builder.Evaluate();

        result.Should().Be(original);
    }

    [Fact]
    public void TakeLast_with_blob_zero_returns_empty_blob()
    {
        var original = PineValue.Blob([1, 2, 3]);

        var builder =
            ImmutableSliceBuilder.Create(original)
            .TakeLast(0);

        var result = builder.Evaluate();

        result.Should().Be(PineValue.EmptyBlob);
    }

    [Fact]
    public void TakeLast_is_immutable()
    {
        var original_value =
            PineValue.List(
                [
                PineValue.Blob([1]),
                PineValue.Blob([2]),
                PineValue.Blob([3]),
                PineValue.Blob([4])
                ]);

        var original = ImmutableSliceBuilder.Create(original_value);
        var modified = original.TakeLast(2);

        // Original should be unchanged
        var originalResult = original.Evaluate();
        originalResult.Should().Be(original_value);

        // Modified should have the last 2 elements
        var modifiedResult = modified.Evaluate();
        var expectedModified =
            PineValue.List(
                [
                PineValue.Blob([3]),
                PineValue.Blob([4])
                ]);

        modifiedResult.Should().Be(expectedModified);
    }

    [Fact]
    public void TakeLast_multiple_times_uses_most_restrictive()
    {
        var original =
            PineValue.List(
                [.. Enumerable.Range(1, 10).Select(i => PineValue.Blob([(byte)i]))
                ]);

        var builder =
            ImmutableSliceBuilder.Create(original)
            .TakeLast(7)
            .TakeLast(3);

        var result = builder.Evaluate();

        // First TakeLast(7): [4..10]
        // Second TakeLast(3) on that: [8, 9, 10]

        var expected =
            PineValue.List(
                [
                PineValue.Blob([8]),
                PineValue.Blob([9]),
                PineValue.Blob([10])
                ]);

        result.Should().Be(expected);
        builder.GetLength().Should().Be(3);
    }

    [Fact]
    public void TakeLast_with_large_list()
    {
        var items =
            Enumerable.Range(0, 1000)
            .Select(i => PineValue.Blob([(byte)(i % 256)]))
            .ToArray();

        var original = PineValue.List(items);

        var builder =
            ImmutableSliceBuilder.Create(original)
            .TakeLast(100);

        builder.GetLength().Should().Be(100);

        var result = builder.Evaluate();

        // Should have the last 100 elements
        var expected = PineValue.List(items[^100..]);

        result.Should().Be(expected);
    }

    [Fact]
    public void TakeLast_combined_with_skip_and_take()
    {
        var original =
            PineValue.List(
                [.. Enumerable.Range(1, 15).Select(i => PineValue.Blob([(byte)i]))
                ]);

        var builder =
            ImmutableSliceBuilder.Create(original)
            .Skip(2)
            .Take(10)
            .TakeLast(5);

        // Skip 2: [3..15]
        // Take 10: [3..12]
        // TakeLast 5: [8, 9, 10, 11, 12]

        var result = builder.Evaluate();

        var expected =
            PineValue.List(
                [
                PineValue.Blob([8]),
                PineValue.Blob([9]),
                PineValue.Blob([10]),
                PineValue.Blob([11]),
                PineValue.Blob([12])
                ]);

        result.Should().Be(expected);
        builder.GetLength().Should().Be(5);
    }

    static void VerifyConsistencyOfDerivedProperties(ImmutableSliceBuilder builder)
    {
        var isList = builder.IsList();
        var isBlob = builder.IsBlob();

        var length = builder.GetLength();

        // An ImmutableSliceBuilder should be either a list or a blob, but not both or neither
        (isList ^ isBlob).Should().BeTrue("An ImmutableSliceBuilder must be either a list or a blob.");

        var evaluated = builder.Evaluate();

        switch (evaluated)
        {
            case PineValue.ListValue:
                isList.Should().BeTrue("Evaluated value is a list, so IsList should be true.");
                isBlob.Should().BeFalse("Evaluated value is a list, so IsBlob should be false.");
                length.Should().Be(((PineValue.ListValue)evaluated).Items.Length, "Length should match the number of items in the list.");
                break;

            case PineValue.BlobValue:
                isBlob.Should().BeTrue("Evaluated value is a blob, so IsBlob should be true.");
                isList.Should().BeFalse("Evaluated value is a blob, so IsList should be false.");
                length.Should().Be(((PineValue.BlobValue)evaluated).Bytes.Length, "Length should match the number of bytes in the blob.");
                break;

            default:
                throw new System.NotImplementedException(
                    "Unexpected PineValue type from Evaluate: " + evaluated.GetType().FullName);
        }
    }
}
