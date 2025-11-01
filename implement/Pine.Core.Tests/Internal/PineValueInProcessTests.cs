using AwesomeAssertions;
using Pine.Core.Internal;
using Pine.Core.PopularEncodings;
using System.Collections.Generic;
using System.Linq;
using System.Numerics;
using Xunit;

namespace Pine.Core.Tests.Internal;

public class PineValueInProcessTests
{
    [Fact]
    public void Create_initializes_with_evaluated_value()
    {
        var value = PineValue.List([PineValue.Blob([1, 2, 3])]);
        var inProcess = PineValueInProcess.Create(value);

        var result = inProcess.Evaluate();

        result.Should().Be(value);
    }

    [Fact]
    public void CreateList_initializes_without_immediate_evaluation()
    {
        var items = new List<PineValueInProcess>
        {
            PineValueInProcess.Create(PineValue.Blob([1])),
            PineValueInProcess.Create(PineValue.Blob([2])),
            PineValueInProcess.Create(PineValue.Blob([3]))
        };

        var inProcess = PineValueInProcess.CreateList(items);

        var result = inProcess.Evaluate();

        var expected = PineValue.List([PineValue.Blob([1]), PineValue.Blob([2]), PineValue.Blob([3])]);

        result.Should().Be(expected);
    }

    [Fact]
    public void CreateList_with_empty_list()
    {
        var items = new List<PineValueInProcess>();
        var inProcess = PineValueInProcess.CreateList(items);

        var result = inProcess.Evaluate();

        result.Should().Be(PineValue.EmptyList);
    }

    [Fact]
    public void Evaluate_caches_result()
    {
        var items = new List<PineValueInProcess> { PineValueInProcess.Create(PineValue.Blob([1])) };
        var inProcess = PineValueInProcess.CreateList(items);

        var result1 = inProcess.Evaluate();
        var result2 = inProcess.Evaluate();

        // Same reference indicates caching
        ReferenceEquals(result1, result2).Should().BeTrue();
    }

    [Fact]
    public void AsInteger_parses_integer_from_blob()
    {
        var integerValue = IntegerEncoding.EncodeSignedInteger(42);
        var inProcess = PineValueInProcess.Create(integerValue);

        var result = inProcess.AsInteger();

        result.Should().Be(new BigInteger(42));
    }

    [Fact]
    public void AsInteger_returns_null_for_non_integer_values()
    {
        var listValue = PineValue.List([PineValue.Blob([1])]);
        var inProcess = PineValueInProcess.Create(listValue);

        var result = inProcess.AsInteger();

        result.Should().BeNull();
    }

    [Fact]
    public void AsInteger_caches_parsed_value()
    {
        var integerValue = IntegerEncoding.EncodeSignedInteger(100);
        var inProcess = PineValueInProcess.Create(integerValue);

        var result1 = inProcess.AsInteger();
        var result2 = inProcess.AsInteger();

        result1.Should().Be(result2);
    }

    [Fact]
    public void IsList_returns_true_for_list_value()
    {
        var listValue = PineValue.List([PineValue.Blob([1])]);
        var inProcess = PineValueInProcess.Create(listValue);

        VerifyConsistencyOfDerivedProperties(inProcess);
    }

    [Fact]
    public void IsList_returns_false_for_blob_value()
    {
        var blobValue = PineValue.Blob([1, 2, 3]);
        var inProcess = PineValueInProcess.Create(blobValue);

        VerifyConsistencyOfDerivedProperties(inProcess);
    }

    [Fact]
    public void IsList_returns_true_for_CreateList()
    {
        var items = new List<PineValueInProcess> { PineValueInProcess.Create(PineValue.Blob([1])) };
        var inProcess = PineValueInProcess.CreateList(items);

        VerifyConsistencyOfDerivedProperties(inProcess);
    }

    [Fact]
    public void IsBlob_returns_true_for_blob_value()
    {
        var blobValue = PineValue.Blob([1, 2, 3]);
        var inProcess = PineValueInProcess.Create(blobValue);

        VerifyConsistencyOfDerivedProperties(inProcess);
    }

    [Fact]
    public void IsBlob_returns_false_for_list_value()
    {
        var listValue = PineValue.List([PineValue.Blob([1])]);
        var inProcess = PineValueInProcess.Create(listValue);

        VerifyConsistencyOfDerivedProperties(inProcess);
    }

    [Fact]
    public void IsBlob_returns_false_for_CreateList()
    {
        var items = new List<PineValueInProcess> { PineValueInProcess.Create(PineValue.Blob([1])) };
        var inProcess = PineValueInProcess.CreateList(items);

        VerifyConsistencyOfDerivedProperties(inProcess);
    }

    [Fact]
    public void IsList_and_IsBlob_are_mutually_exclusive()
    {
        var listValue = PineValue.List([PineValue.Blob([1])]);
        var blobValue = PineValue.Blob([1, 2, 3]);

        var listInProcess = PineValueInProcess.Create(listValue);
        var blobInProcess = PineValueInProcess.Create(blobValue);

        VerifyConsistencyOfDerivedProperties(listInProcess);
        VerifyConsistencyOfDerivedProperties(blobInProcess);
    }

    [Fact]
    public void GetLength_returns_correct_length_for_list()
    {
        var items = new List<PineValueInProcess>
        {
            PineValueInProcess.Create(PineValue.Blob([1])),
            PineValueInProcess.Create(PineValue.Blob([2])),
            PineValueInProcess.Create(PineValue.Blob([3]))
        };
        var inProcess = PineValueInProcess.CreateList(items);

        var length = inProcess.GetLength();

        length.Should().Be(3);
        VerifyConsistencyOfDerivedProperties(inProcess);
    }

    [Fact]
    public void GetLength_returns_correct_length_for_blob()
    {
        var blobValue = PineValue.Blob([1, 2, 3, 4, 5]);
        var inProcess = PineValueInProcess.Create(blobValue);

        var length = inProcess.GetLength();

        length.Should().Be(5);
        VerifyConsistencyOfDerivedProperties(inProcess);
    }

    [Fact]
    public void GetLength_returns_zero_for_empty_list()
    {
        var inProcess = PineValueInProcess.CreateList([]);

        var length = inProcess.GetLength();

        length.Should().Be(0);
    }

    [Fact]
    public void GetLength_returns_zero_for_empty_blob()
    {
        var inProcess = PineValueInProcess.Create(PineValue.EmptyBlob);

        var length = inProcess.GetLength();

        length.Should().Be(0);
    }

    [Fact]
    public void Skip_with_zero_returns_same_value()
    {
        var original = PineValueInProcess.Create(
            PineValue.List([PineValue.Blob([1]), PineValue.Blob([2])]));

        var result = PineValueInProcess.Skip(0, original);

        // Should return the same reference for zero skip
        ReferenceEquals(result, original).Should().BeTrue();
    }

    [Fact]
    public void Skip_with_negative_count_returns_same_value()
    {
        var original = PineValueInProcess.Create(
            PineValue.List([PineValue.Blob([1]), PineValue.Blob([2])]));

        var result = PineValueInProcess.Skip(-5, original);

        ReferenceEquals(result, original).Should().BeTrue();
    }

    [Fact]
    public void Skip_on_list_produces_correct_result()
    {
        var originalValue = PineValue.List([
            PineValue.Blob([1]),
            PineValue.Blob([2]),
            PineValue.Blob([3]),
            PineValue.Blob([4])
        ]);
        var original = PineValueInProcess.Create(originalValue);

        var result = PineValueInProcess.Skip(2, original);
        var evaluated = result.Evaluate();

        var expected = KernelFunction.skip(
            PineValue.List([IntegerEncoding.EncodeSignedInteger(2), originalValue]));

        evaluated.Should().Be(expected);
        VerifyConsistencyOfDerivedProperties(result);
    }

    [Fact]
    public void Skip_on_blob_produces_correct_result()
    {
        var originalValue = PineValue.Blob([10, 20, 30, 40, 50]);
        var original = PineValueInProcess.Create(originalValue);

        var result = PineValueInProcess.Skip(2, original);
        var evaluated = result.Evaluate();

        var expected = KernelFunction.skip(
            PineValue.List([IntegerEncoding.EncodeSignedInteger(2), originalValue]));

        evaluated.Should().Be(expected);
        VerifyConsistencyOfDerivedProperties(result);
    }

    [Fact]
    public void Skip_chaining_accumulates_correctly()
    {
        var originalValue = PineValue.List([
            PineValue.Blob([1]),
            PineValue.Blob([2]),
            PineValue.Blob([3]),
            PineValue.Blob([4]),
            PineValue.Blob([5])
        ]);
        var original = PineValueInProcess.Create(originalValue);

        var result = PineValueInProcess.Skip(1, original);
        result = PineValueInProcess.Skip(2, result);
        var evaluated = result.Evaluate();

        var expected = KernelFunction.skip(
            PineValue.List([IntegerEncoding.EncodeSignedInteger(3), originalValue]));

        evaluated.Should().Be(expected);
        VerifyConsistencyOfDerivedProperties(result);
    }

    [Fact]
    public void Take_on_list_produces_correct_result()
    {
        var originalValue = PineValue.List([
            PineValue.Blob([1]),
            PineValue.Blob([2]),
            PineValue.Blob([3]),
            PineValue.Blob([4])
        ]);
        var original = PineValueInProcess.Create(originalValue);

        var result = PineValueInProcess.Take(2, original);
        var evaluated = result.Evaluate();

        var expected = KernelFunction.take(
            PineValue.List([IntegerEncoding.EncodeSignedInteger(2), originalValue]));

        evaluated.Should().Be(expected);
        VerifyConsistencyOfDerivedProperties(result);
    }

    [Fact]
    public void Take_on_blob_produces_correct_result()
    {
        var originalValue = PineValue.Blob([10, 20, 30, 40, 50]);
        var original = PineValueInProcess.Create(originalValue);

        var result = PineValueInProcess.Take(3, original);
        var evaluated = result.Evaluate();

        var expected = KernelFunction.take(
            PineValue.List([IntegerEncoding.EncodeSignedInteger(3), originalValue]));

        evaluated.Should().Be(expected);
        VerifyConsistencyOfDerivedProperties(result);
    }

    [Fact]
    public void Take_with_zero_produces_empty()
    {
        var originalValue = PineValue.List([PineValue.Blob([1]), PineValue.Blob([2])]);
        var original = PineValueInProcess.Create(originalValue);

        var result = PineValueInProcess.Take(0, original);
        var evaluated = result.Evaluate();

        var expected = KernelFunction.take(
            PineValue.List([IntegerEncoding.EncodeSignedInteger(0), originalValue]));

        evaluated.Should().Be(expected);
    }

    [Fact]
    public void Combined_skip_and_take_produces_correct_slice()
    {
        var originalValue = PineValue.List([
            PineValue.Blob([1]),
            PineValue.Blob([2]),
            PineValue.Blob([3]),
            PineValue.Blob([4]),
            PineValue.Blob([5])
        ]);
        var original = PineValueInProcess.Create(originalValue);

        var result = PineValueInProcess.Skip(1, original);
        result = PineValueInProcess.Take(3, result);
        var evaluated = result.Evaluate();

        var afterSkip = KernelFunction.skip(
            PineValue.List([IntegerEncoding.EncodeSignedInteger(1), originalValue]));
        var expected = KernelFunction.take(
            PineValue.List([IntegerEncoding.EncodeSignedInteger(3), afterSkip]));

        evaluated.Should().Be(expected);
        VerifyConsistencyOfDerivedProperties(result);
    }

    [Fact]
    public void ConcatBinary_with_two_evaluated_values()
    {
        var left = PineValueInProcess.Create(PineValue.Blob([1, 2, 3]));
        var right = PineValueInProcess.Create(PineValue.Blob([4, 5]));

        var result = PineValueInProcess.ConcatBinary(left, right);
        var evaluated = result.Evaluate();

        var expected = KernelFunction.concat(
            PineValue.List([PineValue.Blob([1, 2, 3]), PineValue.Blob([4, 5])]));

        evaluated.Should().Be(expected);
        VerifyConsistencyOfDerivedProperties(result);
    }

    [Fact]
    public void ConcatBinary_with_lists()
    {
        var leftValue = PineValue.List([PineValue.Blob([1])]);
        var rightValue = PineValue.List([PineValue.Blob([2])]);
        var left = PineValueInProcess.Create(leftValue);
        var right = PineValueInProcess.Create(rightValue);

        var result = PineValueInProcess.ConcatBinary(left, right);
        var evaluated = result.Evaluate();

        var expected = KernelFunction.concat(PineValue.List([leftValue, rightValue]));

        evaluated.Should().Be(expected);
        VerifyConsistencyOfDerivedProperties(result);
    }

    [Fact]
    public void ConcatBinary_chaining_multiple_operations()
    {
        var value1 = PineValueInProcess.Create(PineValue.Blob([1, 2]));
        var value2 = PineValueInProcess.Create(PineValue.Blob([3, 4]));
        var value3 = PineValueInProcess.Create(PineValue.Blob([5, 6]));

        var result = PineValueInProcess.ConcatBinary(value1, value2);
        result = PineValueInProcess.ConcatBinary(result, value3);
        var evaluated = result.Evaluate();

        var expected = KernelFunction.concat(
            PineValue.List([
                PineValue.Blob([1, 2]),
                PineValue.Blob([3, 4]),
                PineValue.Blob([5, 6])
            ]));

        evaluated.Should().Be(expected);
        VerifyConsistencyOfDerivedProperties(result);
    }

    [Fact]
    public void ConcatBinary_with_empty_blobs()
    {
        var left = PineValueInProcess.Create(PineValue.EmptyBlob);
        var right = PineValueInProcess.Create(PineValue.Blob([1, 2]));

        var result = PineValueInProcess.ConcatBinary(left, right);
        var evaluated = result.Evaluate();

        var expected = KernelFunction.concat(
            PineValue.List([PineValue.EmptyBlob, PineValue.Blob([1, 2])]));

        evaluated.Should().Be(expected);
    }

    [Fact]
    public void Equal_with_same_reference_returns_true()
    {
        var value = PineValueInProcess.Create(PineValue.Blob([1, 2, 3]));

        var result = PineValueInProcess.AreEqual(value, value);

        result.Should().BeTrue();
    }

    [Fact]
    public void Equal_with_same_evaluated_value_returns_true()
    {
        var pineValue = PineValue.Blob([1, 2, 3]);
        var value1 = PineValueInProcess.Create(pineValue);
        var value2 = PineValueInProcess.Create(pineValue);

        var result = PineValueInProcess.AreEqual(value1, value2);

        result.Should().BeTrue();
    }

    [Fact]
    public void Equal_with_different_values_returns_false()
    {
        var value1 = PineValueInProcess.Create(PineValue.Blob([1, 2, 3]));
        var value2 = PineValueInProcess.Create(PineValue.Blob([4, 5, 6]));

        var result = PineValueInProcess.AreEqual(value1, value2);

        result.Should().BeFalse();
    }

    [Fact]
    public void Equal_with_list_and_blob_returns_false()
    {
        var listValue =
            PineValueInProcess.Create(
                PineValue.List([PineValue.Blob([1])]));

        var blobValue = PineValueInProcess.Create(PineValue.Blob([1]));

        var result = PineValueInProcess.AreEqual(listValue, blobValue);

        result.Should().BeFalse();
    }

    [Fact]
    public void Equal_with_same_list_items()
    {
        var items = new List<PineValueInProcess>
        {
            PineValueInProcess.Create(PineValue.Blob([1])),
            PineValueInProcess.Create(PineValue.Blob([2]))
        };

        var value1 = PineValueInProcess.CreateList(items);
        var value2 = PineValueInProcess.CreateList(items);

        var result = PineValueInProcess.AreEqual(value1, value2);

        result.Should().BeTrue();
    }

    [Fact]
    public void Equal_with_different_list_items()
    {
        var items1 = new List<PineValueInProcess> { PineValueInProcess.Create(PineValue.Blob([1])) };
        var items2 = new List<PineValueInProcess> { PineValueInProcess.Create(PineValue.Blob([2])) };

        var value1 = PineValueInProcess.CreateList(items1);
        var value2 = PineValueInProcess.CreateList(items2);

        var result = PineValueInProcess.AreEqual(value1, value2);

        result.Should().BeFalse();
    }

    [Fact]
    public void Equal_with_unevaluated_and_evaluated_equivalent_values()
    {
        var items = new List<PineValueInProcess>
        {
            PineValueInProcess.Create(PineValue.Blob([1])),
            PineValueInProcess.Create(PineValue.Blob([2]))
        };

        var unevaluated = PineValueInProcess.CreateList(items);
        var evaluated = PineValueInProcess.Create(PineValue.List([PineValue.Blob([1]), PineValue.Blob([2])]));

        var result = PineValueInProcess.AreEqual(unevaluated, evaluated);

        result.Should().BeTrue();
    }

    [Fact]
    public void Complex_combination_of_operations_produces_correct_result()
    {
        // Create a complex scenario: skip, take, concat
        var originalValue =
            PineValue.List(
                [
                PineValue.Blob([1]),
                PineValue.Blob([2]),
                PineValue.Blob([3]),
                PineValue.Blob([4]),
                PineValue.Blob([5])
                ]);

        var original = PineValueInProcess.Create(originalValue);

        var skipped = PineValueInProcess.Skip(1, original);
        var taken = PineValueInProcess.Take(3, skipped);

        var additionalValue =
            PineValueInProcess.Create(
                PineValue.List([PineValue.Blob([6])]));

        var concatenated =
            PineValueInProcess.ConcatBinary(taken, additionalValue);

        var evaluated = concatenated.Evaluate();

        var afterSkip =
            KernelFunction.skip(
                PineValue.List([IntegerEncoding.EncodeSignedInteger(1), originalValue]));

        var afterTake =
            KernelFunction.take(
                PineValue.List([IntegerEncoding.EncodeSignedInteger(3), afterSkip]));

        var expected =
            KernelFunction.concat(
                PineValue.List([afterTake, PineValue.List([PineValue.Blob([6])])]));

        evaluated.Should().Be(expected);

        VerifyConsistencyOfDerivedProperties(concatenated);
    }

    [Fact]
    public void Large_list_operations()
    {
        var items =
            Enumerable.Range(0, 1000)
            .Select(i => PineValueInProcess.Create(PineValue.Blob([(byte)(i % 256)])))
            .ToList();

        var inProcess = PineValueInProcess.CreateList(items);

        var skipped = PineValueInProcess.Skip(100, inProcess);
        var taken = PineValueInProcess.Take(500, skipped);

        var length = taken.GetLength();
        length.Should().Be(500);

        VerifyConsistencyOfDerivedProperties(taken);

        var evaluated = taken.Evaluate();
        var originalValue = PineValue.List(
            Enumerable.Range(0, 1000).Select(i => PineValue.Blob([(byte)(i % 256)])).ToArray());

        var afterSkip =
            KernelFunction.skip(
                PineValue.List([IntegerEncoding.EncodeSignedInteger(100), originalValue]));

        var expected =
            KernelFunction.take(
                PineValue.List([IntegerEncoding.EncodeSignedInteger(500), afterSkip]));

        evaluated.Should().Be(expected);
    }

    [Fact]
    public void Integer_value_is_blob()
    {
        var integerValue = IntegerEncoding.EncodeSignedInteger(42);
        var inProcess = PineValueInProcess.Create(integerValue);

        VerifyConsistencyOfDerivedProperties(inProcess);
    }

    [Fact]
    public void Empty_list_has_zero_length()
    {
        var inProcess = PineValueInProcess.Create(PineValue.EmptyList);

        var length = inProcess.GetLength();

        length.Should().Be(0);

        VerifyConsistencyOfDerivedProperties(inProcess);
    }

    [Fact]
    public void Skip_beyond_length_produces_empty()
    {
        var originalValue = PineValue.List([PineValue.Blob([1]), PineValue.Blob([2])]);
        var original = PineValueInProcess.Create(originalValue);

        var result = PineValueInProcess.Skip(10, original);
        var evaluated = result.Evaluate();

        var expected = KernelFunction.skip(
            PineValue.List([IntegerEncoding.EncodeSignedInteger(10), originalValue]));

        evaluated.Should().Be(expected);
    }

    [Fact]
    public void Take_more_than_available_returns_all()
    {
        var originalValue =
            PineValue.List(
                [
                PineValue.Blob([1]),
                PineValue.Blob([2]),
                PineValue.Blob([3])
                ]);

        var original = PineValueInProcess.Create(originalValue);

        var result = PineValueInProcess.Take(100, original);
        var evaluated = result.Evaluate();

        var expected = KernelFunction.take(
            PineValue.List([IntegerEncoding.EncodeSignedInteger(100), originalValue]));

        evaluated.Should().Be(expected);
    }

    [Fact]
    public void Nested_list_structure()
    {
        var nestedValue =
            PineValue.List(
                [
                PineValue.List([PineValue.Blob([1])]),
                PineValue.List([PineValue.Blob([2])]),
                PineValue.List([PineValue.Blob([3])])
                ]);

        var inProcess = PineValueInProcess.Create(nestedValue);

        VerifyConsistencyOfDerivedProperties(inProcess);

        var length = inProcess.GetLength();
        length.Should().Be(3);
    }

    [Fact]
    public void ConcatBinary_preserves_type_information()
    {
        // Concat of two blobs should result in a blob
        var blob1 = PineValueInProcess.Create(PineValue.Blob([1, 2]));
        var blob2 = PineValueInProcess.Create(PineValue.Blob([3, 4]));

        var result = PineValueInProcess.ConcatBinary(blob1, blob2);

        VerifyConsistencyOfDerivedProperties(result);

        // Concat with at least one list should result in a list
        var list1 = PineValueInProcess.Create(PineValue.List([PineValue.Blob([1])]));
        var blob3 = PineValueInProcess.Create(PineValue.Blob([2]));

        var result2 = PineValueInProcess.ConcatBinary(list1, blob3);

        VerifyConsistencyOfDerivedProperties(result2);
    }

    [Fact]
    public void GetLength_matches_evaluated_length()
    {
        var testCases = new[]
        {
            PineValue.EmptyList,
            PineValue.EmptyBlob,
            PineValue.List([PineValue.Blob([1])]),
            PineValue.List([PineValue.Blob([1]), PineValue.Blob([2]), PineValue.Blob([3])]),
            PineValue.Blob([1, 2, 3, 4, 5]),
            IntegerEncoding.EncodeSignedInteger(0),
            IntegerEncoding.EncodeSignedInteger(255),
            IntegerEncoding.EncodeSignedInteger(0x1_00),
            IntegerEncoding.EncodeSignedInteger(0x1_00_00),

            IntegerEncoding.EncodeSignedInteger(-0x100),
            IntegerEncoding.EncodeSignedInteger(-0x1_00_00),
            IntegerEncoding.EncodeSignedInteger(-100),
        };

        foreach (var testCase in testCases)
        {
            var inProcess = PineValueInProcess.Create(testCase);

            VerifyConsistencyOfDerivedProperties(inProcess);

            var length = inProcess.GetLength();
            var evaluated = inProcess.Evaluate();
            var expectedLength = KernelFunctionSpecialized.length_as_int(evaluated);

            length.Should().Be(expectedLength);
        }
    }

    [Fact]
    public void Skip_and_take_with_CreateList()
    {
        var items = new List<PineValueInProcess>
        {
            PineValueInProcess.Create(PineValue.Blob([1])),
            PineValueInProcess.Create(PineValue.Blob([2])),
            PineValueInProcess.Create(PineValue.Blob([3])),
            PineValueInProcess.Create(PineValue.Blob([4])),
            PineValueInProcess.Create(PineValue.Blob([5]))
        };

        var inProcess = PineValueInProcess.CreateList(items);

        var result = PineValueInProcess.Skip(1, inProcess);

        result = PineValueInProcess.Take(3, result);

        VerifyConsistencyOfDerivedProperties(result);

        var evaluated = result.Evaluate();
        var originalValue = PineValue.List([
            PineValue.Blob([1]),
            PineValue.Blob([2]),
            PineValue.Blob([3]),
            PineValue.Blob([4]),
            PineValue.Blob([5])
        ]);

        var afterSkip = KernelFunction.skip(
            PineValue.List([IntegerEncoding.EncodeSignedInteger(1), originalValue]));

        var expected = KernelFunction.take(
            PineValue.List([IntegerEncoding.EncodeSignedInteger(3), afterSkip]));

        evaluated.Should().Be(expected);
    }

    [Fact]
    public void Multiple_concat_operations_maintain_consistency()
    {
        var value1 = PineValueInProcess.Create(PineValue.Blob([1]));
        var value2 = PineValueInProcess.Create(PineValue.Blob([2]));
        var value3 = PineValueInProcess.Create(PineValue.Blob([3]));
        var value4 = PineValueInProcess.Create(PineValue.Blob([4]));

        var concat1 = PineValueInProcess.ConcatBinary(value1, value2);
        var concat2 = PineValueInProcess.ConcatBinary(value3, value4);
        var finalConcat = PineValueInProcess.ConcatBinary(concat1, concat2);

        VerifyConsistencyOfDerivedProperties(finalConcat);

        var evaluated = finalConcat.Evaluate();

        var expected = KernelFunction.concat(
            PineValue.List([
                PineValue.Blob([1]),
                PineValue.Blob([2]),
                PineValue.Blob([3]),
                PineValue.Blob([4])
            ]));

        evaluated.Should().Be(expected);
    }

    [Fact]
    public void AsInteger_works_without_evaluation()
    {
        var items = new List<PineValueInProcess> { PineValueInProcess.Create(IntegerEncoding.EncodeSignedInteger(42)) };
        var inProcess = PineValueInProcess.CreateList(items);

        // This should not be an integer
        var result = inProcess.AsInteger();

        result.Should().BeNull();
    }

    [Fact]
    public void Equal_handles_different_internal_representations()
    {
        // Two ways to create the same value
        var items = new List<PineValueInProcess>
        {
            PineValueInProcess.Create(PineValue.Blob([1])),
            PineValueInProcess.Create(PineValue.Blob([2]))
        };

        var fromList = PineValueInProcess.CreateList(items);
        var fromEvaluated = PineValueInProcess.Create(PineValue.List([PineValue.Blob([1]), PineValue.Blob([2])]));

        var result = PineValueInProcess.AreEqual(fromList, fromEvaluated);

        result.Should().BeTrue();
    }

    [Fact]
    public void Skip_on_slice_builder_reuses_builder()
    {
        var originalValue = PineValue.List([
            PineValue.Blob([1]),
            PineValue.Blob([2]),
            PineValue.Blob([3])
        ]);
        var original = PineValueInProcess.Create(originalValue);

        var skipped1 = PineValueInProcess.Skip(1, original);
        var skipped2 = PineValueInProcess.Skip(1, skipped1);

        VerifyConsistencyOfDerivedProperties(skipped2);

        var evaluated = skipped2.Evaluate();

        var expected =
            KernelFunction.skip(
                PineValue.List([IntegerEncoding.EncodeSignedInteger(2), originalValue]));

        evaluated.Should().Be(expected);
    }

    [Fact]
    public void Take_on_slice_builder_reuses_builder()
    {
        var originalValue =
            PineValue.List(
                [
                PineValue.Blob([1]),
                PineValue.Blob([2]),
                PineValue.Blob([3]),
                PineValue.Blob([4])
                ]);

        var original = PineValueInProcess.Create(originalValue);

        var taken1 = PineValueInProcess.Take(3, original);
        var taken2 = PineValueInProcess.Take(2, taken1);

        VerifyConsistencyOfDerivedProperties(taken2);

        var evaluated = taken2.Evaluate();

        var afterTake1 = KernelFunction.take(
            PineValue.List([IntegerEncoding.EncodeSignedInteger(3), originalValue]));

        var expected = KernelFunction.take(
            PineValue.List([IntegerEncoding.EncodeSignedInteger(2), afterTake1]));

        evaluated.Should().Be(expected);
    }

    [Fact]
    public void GetElementAt_on_list_returns_correct_element()
    {
        var originalValue = PineValue.List([
            PineValue.Blob([10]),
            PineValue.Blob([20]),
            PineValue.Blob([30]),
            PineValue.Blob([40]),
            PineValue.Blob([50])
        ]);
        var inProcess = PineValueInProcess.Create(originalValue);

        var element0 = inProcess.GetElementAt(0);
        var element2 = inProcess.GetElementAt(2);
        var element4 = inProcess.GetElementAt(4);

        element0.Evaluate().Should().Be(PineValue.Blob([10]));
        element2.Evaluate().Should().Be(PineValue.Blob([30]));
        element4.Evaluate().Should().Be(PineValue.Blob([50]));
    }

    [Fact]
    public void GetElementAt_on_blob_returns_single_byte()
    {
        var originalValue = PineValue.Blob([10, 20, 30, 40, 50]);
        var inProcess = PineValueInProcess.Create(originalValue);

        var element0 = inProcess.GetElementAt(0);
        var element2 = inProcess.GetElementAt(2);
        var element4 = inProcess.GetElementAt(4);

        element0.Evaluate().Should().Be(PineValue.Blob([10]));
        element2.Evaluate().Should().Be(PineValue.Blob([30]));
        element4.Evaluate().Should().Be(PineValue.Blob([50]));
    }

    [Fact]
    public void GetElementAt_with_skip_operation()
    {
        var originalValue =
            PineValue.List(
                [
                PineValue.Blob([10]),
                PineValue.Blob([20]),
                PineValue.Blob([30]),
                PineValue.Blob([40]),
                PineValue.Blob([50])
                ]);

        var inProcess = PineValueInProcess.Create(originalValue);
        var skipped = PineValueInProcess.Skip(2, inProcess);

        // After skip(2), the slice is [30, 40, 50]
        var element0 = skipped.GetElementAt(0);
        var element1 = skipped.GetElementAt(1);
        var element2 = skipped.GetElementAt(2);

        element0.Evaluate().Should().Be(PineValue.Blob([30]));
        element1.Evaluate().Should().Be(PineValue.Blob([40]));
        element2.Evaluate().Should().Be(PineValue.Blob([50]));
    }

    [Fact]
    public void GetElementAt_with_take_operation()
    {
        var originalValue =
            PineValue.List(
                [
                PineValue.Blob([10]),
                PineValue.Blob([20]),
                PineValue.Blob([30]),
                PineValue.Blob([40]),
                PineValue.Blob([50])
                ]);

        var inProcess = PineValueInProcess.Create(originalValue);
        var taken = PineValueInProcess.Take(3, inProcess);

        // After take(3), the slice is [10, 20, 30]
        var element0 = taken.GetElementAt(0);
        var element1 = taken.GetElementAt(1);
        var element2 = taken.GetElementAt(2);

        element0.Evaluate().Should().Be(PineValue.Blob([10]));
        element1.Evaluate().Should().Be(PineValue.Blob([20]));
        element2.Evaluate().Should().Be(PineValue.Blob([30]));
    }

    [Fact]
    public void GetElementAt_with_skip_and_take()
    {
        var originalValue =
            PineValue.List(
                [
                PineValue.Blob([10]),
                PineValue.Blob([20]),
                PineValue.Blob([30]),
                PineValue.Blob([40]),
                PineValue.Blob([50]),
                PineValue.Blob([60])
                ]);

        var inProcess = PineValueInProcess.Create(originalValue);
        var skipped = PineValueInProcess.Skip(1, inProcess);
        var taken = PineValueInProcess.Take(3, skipped);

        // After skip(1).take(3), the slice is [20, 30, 40]
        var element0 = taken.GetElementAt(0);
        var element1 = taken.GetElementAt(1);
        var element2 = taken.GetElementAt(2);

        element0.Evaluate().Should().Be(PineValue.Blob([20]));
        element1.Evaluate().Should().Be(PineValue.Blob([30]));
        element2.Evaluate().Should().Be(PineValue.Blob([40]));
    }

    [Fact]
    public void GetElementAt_with_multiple_chained_skips()
    {
        var originalValue =
            PineValue.List(
                [.. Enumerable.Range(0, 20).Select(i => PineValue.Blob([(byte)i]))
                ]);

        var inProcess = PineValueInProcess.Create(originalValue);
        var skipped = PineValueInProcess.Skip(3, inProcess);
        skipped = PineValueInProcess.Skip(2, skipped);
        skipped = PineValueInProcess.Skip(1, skipped);

        // Total skip is 6, so slice starts at element 6
        var element0 = skipped.GetElementAt(0);
        var element1 = skipped.GetElementAt(1);
        var element2 = skipped.GetElementAt(2);

        element0.Evaluate().Should().Be(PineValue.Blob([6]));
        element1.Evaluate().Should().Be(PineValue.Blob([7]));
        element2.Evaluate().Should().Be(PineValue.Blob([8]));
    }

    [Fact]
    public void GetElementAt_out_of_bounds_returns_empty()
    {
        var originalValue =
            PineValue.List(
                [
                PineValue.Blob([10]),
                PineValue.Blob([20])
                ]);

        var inProcess = PineValueInProcess.Create(originalValue);

        var element10 = inProcess.GetElementAt(10);

        PineValueInProcess.AreEqual(element10, PineValue.EmptyList).Should().BeTrue();
    }

    [Fact]
    public void GetElementAt_out_of_bounds_on_blob_returns_empty_blob()
    {
        var originalValue = PineValue.Blob([10, 20]);
        var inProcess = PineValueInProcess.Create(originalValue);

        var element10 = inProcess.GetElementAt(10);

        element10.Evaluate().Should().Be(PineValue.EmptyBlob);
    }

    [Fact]
    public void GetElementAt_with_negative_index_treats_as_zero()
    {
        var originalValue = PineValue.List([
            PineValue.Blob([10]),
            PineValue.Blob([20]),
            PineValue.Blob([30])
        ]);
        var inProcess = PineValueInProcess.Create(originalValue);

        var element = inProcess.GetElementAt(-5);

        element.Evaluate().Should().Be(PineValue.Blob([10]));
    }

    [Fact]
    public void AreEqual_with_PineValue_same_blob_returns_true()
    {
        var pineValue = PineValue.Blob([1, 2, 3]);
        var inProcess = PineValueInProcess.Create(pineValue);

        var result = PineValueInProcess.AreEqual(inProcess, pineValue);

        result.Should().BeTrue();
    }

    [Fact]
    public void AreEqual_with_PineValue_same_list_returns_true()
    {
        var pineValue =
            PineValue.List([PineValue.Blob([1]), PineValue.Blob([2])]);

        var inProcess =
            PineValueInProcess.Create(pineValue);

        var result = PineValueInProcess.AreEqual(inProcess, pineValue);

        result.Should().BeTrue();
    }

    [Fact]
    public void AreEqual_with_PineValue_different_blobs_returns_false()
    {
        var inProcess =
            PineValueInProcess.Create(
                PineValue.Blob([1, 2, 3]));

        var pineValue = PineValue.Blob([4, 5, 6]);

        var result = PineValueInProcess.AreEqual(inProcess, pineValue);

        result.Should().BeFalse();
    }

    [Fact]
    public void AreEqual_with_PineValue_list_vs_blob_returns_false()
    {
        var inProcess =
            PineValueInProcess.Create(
                PineValue.List([PineValue.Blob([1])]));

        var pineValue = PineValue.Blob([1]);

        var result = PineValueInProcess.AreEqual(inProcess, pineValue);

        result.Should().BeFalse();
    }

    [Fact]
    public void AreEqual_with_PineValue_unevaluated_list_matches()
    {
        var items = new List<PineValueInProcess>
        {
            PineValueInProcess.Create(PineValue.Blob([1])),
            PineValueInProcess.Create(PineValue.Blob([2]))
        };
        var inProcess = PineValueInProcess.CreateList(items);
        var pineValue = PineValue.List([PineValue.Blob([1]), PineValue.Blob([2])]);

        var result = PineValueInProcess.AreEqual(inProcess, pineValue);

        result.Should().BeTrue();
    }

    [Fact]
    public void AreEqual_with_PineValue_unevaluated_list_different_items()
    {
        var items1 = new List<PineValueInProcess> { PineValueInProcess.Create(PineValue.Blob([1])) };
        var inProcess = PineValueInProcess.CreateList(items1);
        var pineValue = PineValue.List([PineValue.Blob([2])]);

        var result = PineValueInProcess.AreEqual(inProcess, pineValue);

        result.Should().BeFalse();
    }

    [Fact]
    public void AreEqual_with_PineValue_unevaluated_list_different_length()
    {
        var items1 = new List<PineValueInProcess>
        {
            PineValueInProcess.Create(PineValue.Blob([1])),
            PineValueInProcess.Create(PineValue.Blob([2]))
        };

        var inProcess = PineValueInProcess.CreateList(items1);
        var pineValue = PineValue.List([PineValue.Blob([1])]);

        var result = PineValueInProcess.AreEqual(inProcess, pineValue);

        result.Should().BeFalse();
    }

    [Fact]
    public void AreEqual_with_PineValue_empty_values()
    {
        var emptyListInProcess = PineValueInProcess.Create(PineValue.EmptyList);
        var emptyBlobInProcess = PineValueInProcess.Create(PineValue.EmptyBlob);

        var result1 = PineValueInProcess.AreEqual(emptyListInProcess, PineValue.EmptyList);
        var result2 = PineValueInProcess.AreEqual(emptyBlobInProcess, PineValue.EmptyBlob);
        var result3 = PineValueInProcess.AreEqual(emptyListInProcess, PineValue.EmptyBlob);

        result1.Should().BeTrue();
        result2.Should().BeTrue();
        result3.Should().BeFalse();
    }

    [Fact]
    public void Equals_infix_operator_throws_exception()
    {
        // To avoid accidental use of (expensive) equality checks, make noise on invocation of default 'Equals'

        var instanceA = PineValueInProcess.Create(PineValue.EmptyList);

        var instanceB = PineValueInProcess.Create(PineValue.EmptyList);

        System.Func<bool> action = () => instanceA == instanceB;

        action.Should().Throw<System.InvalidOperationException>();
    }

    [Fact]
    public void Nested_list_construction_without_evaluating_parent()
    {
        // Test that we can build nested lists without computing hash codes for intermediate values
        var innerItem1 = PineValueInProcess.Create(PineValue.Blob([1, 2, 3]));
        var innerItem2 = PineValueInProcess.Create(PineValue.Blob([4, 5, 6]));

        var innerList = PineValueInProcess.CreateList([innerItem1, innerItem2]);

        // Create a list containing the inner list without evaluating it
        var outerList = PineValueInProcess.CreateList([innerList]);

        // Verify that the inner list has not been evaluated yet
        innerList.EvaluatedOrNull.Should().BeNull("Inner list should not be evaluated yet");

        // Now evaluate the outer list
        var evaluated = outerList.Evaluate();

        // Verify the structure is correct
        var expected =
            PineValue.List(
                [
                PineValue.List(
                    [
                    PineValue.Blob([1, 2, 3]),
                    PineValue.Blob([4, 5, 6])
                    ])
                ]);

        evaluated.Should().Be(expected);
    }

    [Fact]
    public void Deeply_nested_list_construction()
    {
        // Test multiple levels of nesting
        var leaf1 = PineValueInProcess.Create(PineValue.Blob([1]));
        var leaf2 = PineValueInProcess.Create(PineValue.Blob([2]));
        var leaf3 = PineValueInProcess.Create(PineValue.Blob([3]));

        var level1a = PineValueInProcess.CreateList([leaf1, leaf2]);
        var level1b = PineValueInProcess.CreateList([leaf3]);

        var level2 = PineValueInProcess.CreateList([level1a, level1b]);

        var level3 = PineValueInProcess.CreateList([level2]);

        // None of the intermediate lists should be evaluated
        level1a.EvaluatedOrNull.Should().BeNull();
        level1b.EvaluatedOrNull.Should().BeNull();
        level2.EvaluatedOrNull.Should().BeNull();

        // Evaluate only the top-level list
        var evaluated = level3.Evaluate();

        var expected =
            PineValue.List(
                [
                PineValue.List(
                    [
                    PineValue.List([PineValue.Blob([1]), PineValue.Blob([2])]),
                    PineValue.List([PineValue.Blob([3])])
                    ])
                ]);

        evaluated.Should().Be(expected);
    }

    [Fact]
    public void Nested_list_with_operations()
    {
        // Test that operations like Skip/Take work with nested lists
        var innerItem1 = PineValueInProcess.Create(PineValue.Blob([1]));
        var innerItem2 = PineValueInProcess.Create(PineValue.Blob([2]));
        var innerItem3 = PineValueInProcess.Create(PineValue.Blob([3]));

        var innerList = PineValueInProcess.CreateList([innerItem1, innerItem2, innerItem3]);

        var outerList = PineValueInProcess.CreateList([innerList]);

        // Get the first element (which is the inner list)
        var firstElement = outerList.GetElementAt(0);

        // The first element should be the evaluated inner list
        var expectedInner =
            PineValue.List(
                [
                PineValue.Blob([1]),
                PineValue.Blob([2]),
                PineValue.Blob([3])
                ]);

        firstElement.Evaluate().Should().Be(expectedInner);
    }

    [Fact]
    public void GetElementAt_returns_PineValueInProcess_without_forcing_evaluation()
    {
        // Test that GetElementAt returns PineValueInProcess without evaluating the element
        var innerItem1 = PineValueInProcess.Create(PineValue.Blob([1, 2, 3]));
        var innerItem2 = PineValueInProcess.Create(PineValue.Blob([4, 5, 6]));

        var list = PineValueInProcess.CreateList([innerItem1, innerItem2]);

        // Get the first element - should not trigger evaluation
        var firstElement = list.GetElementAt(0);

        // Verify the element hasn't been evaluated yet
        firstElement.EvaluatedOrNull.Should().NotBeNull("Item was already evaluated when created");

        // But when we do evaluate it, we get the right value
        firstElement.Evaluate().Should().Be(PineValue.Blob([1, 2, 3]));
    }

    [Fact]
    public void GetElementAt_on_nested_lists_defers_evaluation()
    {
        // Test that getting an element from a nested list doesn't force evaluation of the inner list
        var innerItem1 = PineValueInProcess.Create(PineValue.Blob([1]));
        var innerItem2 = PineValueInProcess.Create(PineValue.Blob([2]));

        var innerList = PineValueInProcess.CreateList([innerItem1, innerItem2]);

        var outerList = PineValueInProcess.CreateList([innerList]);

        // Get the first element (the inner list) - should not trigger evaluation of the inner list
        var retrievedInnerList = outerList.GetElementAt(0);

        // The inner list should not have been evaluated yet
        retrievedInnerList.EvaluatedOrNull.Should().BeNull("Inner list should not be evaluated until needed");

        // Verify the structure is correct when we do evaluate
        var evaluated = retrievedInnerList.Evaluate();
        var expected = PineValue.List([PineValue.Blob([1]), PineValue.Blob([2])]);
        evaluated.Should().Be(expected);
    }

    [Fact]
    public void StartsWithConstAtOffsetVar_blob_matches_at_offset_zero()
    {
        var value = PineValueInProcess.Create(PineValue.Blob([1, 2, 3, 4, 5]));
        var prefix = PineValue.Blob([1, 2, 3]);

        value.StartsWithConstAtOffsetVar(0, prefix).Should().BeTrue();
    }

    [Fact]
    public void StartsWithConstAtOffsetVar_blob_matches_at_offset_two()
    {
        var value = PineValueInProcess.Create(PineValue.Blob([1, 2, 3, 4, 5]));
        var prefix = PineValue.Blob([3, 4]);

        value.StartsWithConstAtOffsetVar(2, prefix).Should().BeTrue();
    }

    [Fact]
    public void StartsWithConstAtOffsetVar_blob_does_not_match()
    {
        var value = PineValueInProcess.Create(PineValue.Blob([1, 2, 3, 4, 5]));
        var prefix = PineValue.Blob([2, 3, 4]);

        value.StartsWithConstAtOffsetVar(0, prefix).Should().BeFalse();
    }

    [Fact]
    public void StartsWithConstAtOffsetVar_blob_offset_beyond_length()
    {
        var value = PineValueInProcess.Create(PineValue.Blob([1, 2, 3]));
        var prefix = PineValue.Blob([4, 5]);

        value.StartsWithConstAtOffsetVar(10, prefix).Should().BeFalse();
    }

    [Fact]
    public void StartsWithConstAtOffsetVar_blob_prefix_longer_than_remaining()
    {
        var value = PineValueInProcess.Create(PineValue.Blob([1, 2, 3, 4, 5]));
        var prefix = PineValue.Blob([4, 5, 6]);

        value.StartsWithConstAtOffsetVar(3, prefix).Should().BeFalse();
    }

    [Fact]
    public void StartsWithConstAtOffsetVar_list_matches_at_offset_zero()
    {
        var value =
            PineValueInProcess.Create(
                PineValue.List(
                    [
                    PineValue.Blob([1]),
                    PineValue.Blob([2]),
                    PineValue.Blob([3]),
                    PineValue.Blob([4])
                    ]));

        var prefix = PineValue.List([PineValue.Blob([1]), PineValue.Blob([2])]);

        value.StartsWithConstAtOffsetVar(0, prefix).Should().BeTrue();
    }

    [Fact]
    public void StartsWithConstAtOffsetVar_list_matches_at_offset_one()
    {
        var value =
            PineValueInProcess.Create(
                PineValue.List(
                    [
                    PineValue.Blob([1]),
                    PineValue.Blob([2]),
                    PineValue.Blob([3]),
                    PineValue.Blob([4])
                    ]));

        var prefix = PineValue.List([PineValue.Blob([2]), PineValue.Blob([3])]);

        value.StartsWithConstAtOffsetVar(1, prefix).Should().BeTrue();
    }

    [Fact]
    public void StartsWithConstAtOffsetVar_list_does_not_match()
    {
        var value =
            PineValueInProcess.Create(
                PineValue.List(
                    [
                    PineValue.Blob([1]),
                    PineValue.Blob([2]),
                    PineValue.Blob([3])
                    ]));

        var prefix = PineValue.List([PineValue.Blob([2]), PineValue.Blob([4])]);

        value.StartsWithConstAtOffsetVar(1, prefix).Should().BeFalse();
    }

    [Fact]
    public void StartsWithConstAtOffsetVar_list_empty_prefix_always_matches()
    {
        var value =
            PineValueInProcess.Create(
                PineValue.List(
                    [
                    PineValue.Blob([1]),
                    PineValue.Blob([2])
                    ]));

        var prefix = PineValue.EmptyList;

        value.StartsWithConstAtOffsetVar(0, prefix).Should().BeTrue();
        value.StartsWithConstAtOffsetVar(5, prefix).Should().BeFalse();
    }

    [Fact]
    public void StartsWithConstAtOffsetVar_list_offset_beyond_length()
    {
        var value =
            PineValueInProcess.Create(
                PineValue.List([PineValue.Blob([1]), PineValue.Blob([2])]));

        var prefix = PineValue.List([PineValue.Blob([3])]);

        value.StartsWithConstAtOffsetVar(10, prefix).Should().BeFalse();
    }

    [Fact]
    public void StartsWithConstAtOffsetVar_type_mismatch_blob_vs_list()
    {
        var value = PineValueInProcess.Create(PineValue.Blob([1, 2, 3]));
        var prefix = PineValue.List([PineValue.Blob([1])]);

        value.StartsWithConstAtOffsetVar(0, prefix).Should().BeFalse();
    }

    [Fact]
    public void StartsWithConstAtOffsetVar_type_mismatch_list_vs_blob()
    {
        var value =
            PineValueInProcess.Create(
                PineValue.List(
                    [
                    PineValue.Blob([1]),
                    PineValue.Blob([2])
                    ]));

        var prefix = PineValue.Blob([1, 2]);

        value.StartsWithConstAtOffsetVar(0, prefix).Should().BeFalse();
    }

    [Fact]
    public void StartsWithConstAtOffsetVar_with_skip_operation()
    {
        var originalValue = PineValue.Blob([1, 2, 3, 4, 5, 6, 7, 8]);
        var value = PineValueInProcess.Create(originalValue);
        var skipped = PineValueInProcess.Skip(2, value);

        // After skip(2), the blob is [3, 4, 5, 6, 7, 8]
        // Check if it starts with [5, 6] at offset 2
        var prefix = PineValue.Blob([5, 6]);

        skipped.StartsWithConstAtOffsetVar(2, prefix).Should().BeTrue();
    }

    [Fact]
    public void StartsWithConstAtOffsetVar_with_take_operation()
    {
        var originalValue = PineValue.Blob([1, 2, 3, 4, 5, 6, 7, 8]);
        var value = PineValueInProcess.Create(originalValue);
        var taken = PineValueInProcess.Take(5, value);

        // After take(5), the blob is [1, 2, 3, 4, 5]
        // Check if it starts with [3, 4] at offset 2
        var prefix = PineValue.Blob([3, 4]);

        taken.StartsWithConstAtOffsetVar(2, prefix).Should().BeTrue();
    }

    [Fact]
    public void StartsWithConstAtOffsetVar_with_skip_and_take()
    {
        var originalValue =
            PineValue.List(
                [
                PineValue.Blob([1]),
                PineValue.Blob([2]),
                PineValue.Blob([3]),
                PineValue.Blob([4]),
                PineValue.Blob([5]),
                PineValue.Blob([6])
                ]);

        var value = PineValueInProcess.Create(originalValue);
        var skipped = PineValueInProcess.Skip(1, value);
        var taken = PineValueInProcess.Take(4, skipped);

        // After skip(1).take(4), the list is [2, 3, 4, 5]
        // Check if it starts with [4, 5] at offset 2
        var prefix = PineValue.List([PineValue.Blob([4]), PineValue.Blob([5])]);

        taken.StartsWithConstAtOffsetVar(2, prefix).Should().BeTrue();
    }

    [Fact]
    public void StartsWithConstAtOffsetVar_with_unevaluated_list()
    {
        var items = new List<PineValueInProcess>
        {
            PineValueInProcess.Create(PineValue.Blob([10])),
            PineValueInProcess.Create(PineValue.Blob([20])),
            PineValueInProcess.Create(PineValue.Blob([30])),
            PineValueInProcess.Create(PineValue.Blob([40]))
        };

        var value = PineValueInProcess.CreateList(items);

        // Check if it starts with [20, 30] at offset 1
        var prefix = PineValue.List([PineValue.Blob([20]), PineValue.Blob([30])]);

        value.StartsWithConstAtOffsetVar(1, prefix).Should().BeTrue();
    }

    [Fact]
    public void StartsWithConstAtOffsetVar_blob_exact_match_at_end()
    {
        var value = PineValueInProcess.Create(PineValue.Blob([1, 2, 3, 4, 5]));
        var prefix = PineValue.Blob([4, 5]);

        value.StartsWithConstAtOffsetVar(3, prefix).Should().BeTrue();
    }

    [Fact]
    public void StartsWithConstAtOffsetVar_list_exact_match_at_end()
    {
        var value =
            PineValueInProcess.Create(
                PineValue.List(
                    [
                    PineValue.Blob([1]),
                    PineValue.Blob([2]),
                    PineValue.Blob([3])
                    ]));

        var prefix = PineValue.List([PineValue.Blob([3])]);

        value.StartsWithConstAtOffsetVar(2, prefix).Should().BeTrue();
    }

    [Fact]
    public void StartsWithConstAtOffsetVar_single_byte_match()
    {
        var value = PineValueInProcess.Create(PineValue.Blob([10, 20, 30, 40]));
        var prefix = PineValue.Blob([30]);

        value.StartsWithConstAtOffsetVar(2, prefix).Should().BeTrue();
    }

    [Fact]
    public void StartsWithConstAtOffsetVar_single_element_list_match()
    {
        var value =
            PineValueInProcess.Create(
                PineValue.List(
                    [
                    PineValue.Blob([10]),
                    PineValue.Blob([20]),
                    PineValue.Blob([30])
                    ]));

        var prefix = PineValue.List([PineValue.Blob([20])]);

        value.StartsWithConstAtOffsetVar(1, prefix).Should().BeTrue();
    }

    private static void VerifyConsistencyOfDerivedProperties(PineValueInProcess inProcess)
    {
        var isList = inProcess.IsList();
        var isBlob = inProcess.IsBlob();
        var length = inProcess.GetLength();

        var evaluated = inProcess.Evaluate();

        // IsList and IsBlob should be mutually exclusive
        (isList ^ isBlob).Should().BeTrue(
            "A PineValueInProcess must be either a list or a blob.");

        // Verify the type checks match the evaluated result
        switch (evaluated)
        {
            case PineValue.ListValue listValue:
                isList.Should().BeTrue(
                    "Evaluated value is a list, so IsList should be true.");
                isBlob.Should().BeFalse(
                    "Evaluated value is a list, so IsBlob should be false.");
                length.Should().Be(listValue.Items.Length,
                    "Length should match the number of items in the list.");
                break;

            case PineValue.BlobValue blobValue:
                isBlob.Should().BeTrue(
                    "Evaluated value is a blob, so IsBlob should be true.");
                isList.Should().BeFalse(
                    "Evaluated value is a blob, so IsList should be false.");
                length.Should().Be(blobValue.Bytes.Length,
                    "Length should match the number of bytes in the blob.");
                break;

            default:
                throw new System.NotImplementedException(
                    "Unexpected PineValue type: " + evaluated.GetType().FullName);
        }

        // Verify that GetLength matches the kernel function's length
        var expectedLength = KernelFunctionSpecialized.length_as_int(evaluated);

        length.Should().Be(expectedLength,
            "GetLength should match KernelFunctionSpecialized.length_as_int");
    }
}
