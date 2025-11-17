using Pine.Core.CodeAnalysis;
using Pine.Core.CommonEncodings;
using System.Collections.Generic;
using AwesomeAssertions;
using Xunit;

namespace Pine.Core.Tests.CodeAnalysis;

public class PineValueClassExtensionsTests
{
    [Fact]
    public void CreateMinimalValue_with_empty_constraints_returns_empty_blob()
    {
        var pvClass = PineValueClass.Empty;

        var result = pvClass.CreateMinimalValue();

        result.Should().Be(PineValue.EmptyBlob);
    }

    [Fact]
    public void CreateMinimalValue_with_root_constraint_returns_that_value()
    {
        var expectedValue = PineValue.Blob([1, 2, 3]);
        var pvClass = PineValueClass.CreateEquals(expectedValue);

        var result = pvClass.CreateMinimalValue();

        result.Should().Be(expectedValue);
    }

    [Fact]
    public void CreateMinimalValue_with_single_path_constraint_creates_minimal_list()
    {
        // Constraint: path [0] must be a specific blob
        var constraintValue = PineValue.Blob([42]);
        var pvClass = PineValueClass.Create([
            new KeyValuePair<IReadOnlyList<int>, PineValue>([0], constraintValue)
        ]);

        var result = pvClass.CreateMinimalValue();

        // Should be a list with one element
        result.Should().BeOfType<PineValue.ListValue>();
        var listValue = (PineValue.ListValue)result;
        listValue.Items.Length.Should().Be(1);
        listValue.Items.Span[0].Should().Be(constraintValue);
    }

    [Fact]
    public void CreateMinimalValue_with_multiple_path_constraints_at_same_depth()
    {
        // Constraints: path [0] and path [2] have specific values
        var value0 = PineValue.Blob([10]);
        var value2 = PineValue.Blob([20]);
        var pvClass = PineValueClass.Create([
            new KeyValuePair<IReadOnlyList<int>, PineValue>([0], value0),
            new KeyValuePair<IReadOnlyList<int>, PineValue>([2], value2)
        ]);

        var result = pvClass.CreateMinimalValue();

        // Should be a list with 3 elements [value0, EmptyList, value2]
        result.Should().BeOfType<PineValue.ListValue>();
        var listValue = (PineValue.ListValue)result;
        listValue.Items.Length.Should().Be(3);
        listValue.Items.Span[0].Should().Be(value0);
        listValue.Items.Span[1].Should().Be(PineValue.EmptyList);
        listValue.Items.Span[2].Should().Be(value2);
    }

    [Fact]
    public void CreateMinimalValue_with_nested_path_constraint()
    {
        // Constraint: path [0, 1] must be a specific blob
        var constraintValue = PineValue.Blob([99]);
        var pvClass = PineValueClass.Create([
            new KeyValuePair<IReadOnlyList<int>, PineValue>([0, 1], constraintValue)
        ]);

        var result = pvClass.CreateMinimalValue();

        // Should be a list with one element, which is itself a list with 2 elements
        result.Should().BeOfType<PineValue.ListValue>();
        var outerList = (PineValue.ListValue)result;
        outerList.Items.Length.Should().Be(1);

        var innerList = outerList.Items.Span[0];
        innerList.Should().BeOfType<PineValue.ListValue>();
        var innerListValue = (PineValue.ListValue)innerList;
        innerListValue.Items.Length.Should().Be(2);
        innerListValue.Items.Span[0].Should().Be(PineValue.EmptyList);
        innerListValue.Items.Span[1].Should().Be(constraintValue);
    }

    [Fact]
    public void CreateMinimalValue_with_multiple_nested_constraints()
    {
        // Constraints: 
        // - path [0, 0] = blob(1)
        // - path [0, 2] = blob(2)
        // - path [1] = blob(3)
        var value00 = PineValue.Blob([1]);
        var value02 = PineValue.Blob([2]);
        var value1 = PineValue.Blob([3]);

        var pvClass = PineValueClass.Create([
            new KeyValuePair<IReadOnlyList<int>, PineValue>([0, 0], value00),
            new KeyValuePair<IReadOnlyList<int>, PineValue>([0, 2], value02),
            new KeyValuePair<IReadOnlyList<int>, PineValue>([1], value1)
        ]);

        var result = pvClass.CreateMinimalValue();

        result.Should().BeOfType<PineValue.ListValue>();
        var outerList = (PineValue.ListValue)result;
        outerList.Items.Length.Should().Be(2);

        // Check first element is a list with 3 items
        var firstElement = outerList.Items.Span[0];
        firstElement.Should().BeOfType<PineValue.ListValue>();
        var firstElementList = (PineValue.ListValue)firstElement;
        firstElementList.Items.Length.Should().Be(3);
        firstElementList.Items.Span[0].Should().Be(value00);
        firstElementList.Items.Span[1].Should().Be(PineValue.EmptyList);
        firstElementList.Items.Span[2].Should().Be(value02);

        // Check second element
        outerList.Items.Span[1].Should().Be(value1);
    }

    [Fact]
    public void CreateMinimalValue_with_deep_nesting()
    {
        // Constraint: path [0, 1, 2] must be a specific blob
        var constraintValue = PineValue.Blob([77]);
        var pvClass = PineValueClass.Create([
            new KeyValuePair<IReadOnlyList<int>, PineValue>([0, 1, 2], constraintValue)
        ]);

        var result = pvClass.CreateMinimalValue();

        // Navigate through the structure
        result.Should().BeOfType<PineValue.ListValue>();
        var level0 = (PineValue.ListValue)result;
        level0.Items.Length.Should().Be(1);

        var level1 = level0.Items.Span[0];
        level1.Should().BeOfType<PineValue.ListValue>();
        var level1List = (PineValue.ListValue)level1;
        level1List.Items.Length.Should().Be(2);

        var level2 = level1List.Items.Span[1];
        level2.Should().BeOfType<PineValue.ListValue>();
        var level2List = (PineValue.ListValue)level2;
        level2List.Items.Length.Should().Be(3);
        level2List.Items.Span[2].Should().Be(constraintValue);
    }

    [Fact]
    public void CreateMinimalValue_satisfies_the_original_constraint()
    {
        // Create a constraint with multiple paths
        var value0 = PineValue.Blob([10]);
        var value12 = PineValue.Blob([20]);
        var value2 = PineValue.List([
            PineValue.Blob([30]),
            PineValue.Blob([40])
        ]);

        var pvClass = PineValueClass.Create([
            new KeyValuePair<IReadOnlyList<int>, PineValue>([0], value0),
            new KeyValuePair<IReadOnlyList<int>, PineValue>([1, 2], value12),
            new KeyValuePair<IReadOnlyList<int>, PineValue>([2], value2)
        ]);

        var result = pvClass.CreateMinimalValue();

        // The created value should satisfy the constraint
        pvClass.SatisfiedByValue(result).Should().BeTrue();
    }

    [Fact]
    public void CreateMinimalValue_with_list_as_constraint()
    {
        // Constraint: path [0] is a list containing specific items
        var constraintValue = PineValue.List([
            PineValue.Blob([1]),
            PineValue.Blob([2]),
            PineValue.Blob([3])
        ]);

        var pvClass = PineValueClass.Create([
            new KeyValuePair<IReadOnlyList<int>, PineValue>([0], constraintValue)
        ]);

        var result = pvClass.CreateMinimalValue();

        result.Should().BeOfType<PineValue.ListValue>();
        var listValue = (PineValue.ListValue)result;
        listValue.Items.Length.Should().Be(1);
        listValue.Items.Span[0].Should().Be(constraintValue);

        pvClass.SatisfiedByValue(result).Should().BeTrue();
    }

    [Fact]
    public void CreateMinimalValue_handles_sparse_indices()
    {
        // Constraint: paths [0], [5], [10] have values
        var value0 = PineValue.Blob([0]);
        var value5 = PineValue.Blob([5]);
        var value10 = PineValue.Blob([10]);

        var pvClass = PineValueClass.Create([
            new KeyValuePair<IReadOnlyList<int>, PineValue>([0], value0),
            new KeyValuePair<IReadOnlyList<int>, PineValue>([5], value5),
            new KeyValuePair<IReadOnlyList<int>, PineValue>([10], value10)
        ]);

        var result = pvClass.CreateMinimalValue();

        result.Should().BeOfType<PineValue.ListValue>();
        var listValue = (PineValue.ListValue)result;
        listValue.Items.Length.Should().Be(11); // indices 0-10
        listValue.Items.Span[0].Should().Be(value0);
        listValue.Items.Span[5].Should().Be(value5);
        listValue.Items.Span[10].Should().Be(value10);

        // Other indices should be EmptyList
        var maxIndex = listValue.Items.Length - 1;

        for (var i = 1; i < maxIndex; i++)
        {
            if (i is not 5)
            {
                listValue.Items.Span[i].Should().Be(PineValue.EmptyList);
            }
        }

        pvClass.SatisfiedByValue(result).Should().BeTrue();
    }

    [Fact]
    public void CreateMinimalValue_with_complex_nested_structure()
    {
        // Create a more complex constraint that exercises multiple features
        var pvClass = PineValueClass.Create([
            new KeyValuePair<IReadOnlyList<int>, PineValue>(
                [0, 0],
                StringEncoding.BlobValueFromString("hello")),
            new KeyValuePair<IReadOnlyList<int>, PineValue>(
                [0, 1],
                IntegerEncoding.EncodeSignedInteger(42)),
            new KeyValuePair<IReadOnlyList<int>, PineValue>(
                [1],
                PineValue.List([
                    PineValue.Blob([1, 2, 3]),
                    PineValue.EmptyBlob
                ])),
            new KeyValuePair<IReadOnlyList<int>, PineValue>(
                [2, 0, 1],
                PineValue.Blob([99]))
        ]);

        var result = pvClass.CreateMinimalValue();

        // Verify the structure and that it satisfies the constraint
        pvClass.SatisfiedByValue(result).Should().BeTrue();

        result.Should().BeOfType<PineValue.ListValue>();
        var resultList = (PineValue.ListValue)result;
        resultList.Items.Length.Should().Be(3);
    }

    [Fact]
    public void CreateMinimalValue_result_has_minimal_nodes_count()
    {
        // For a simple constraint, the result should not have extra nodes
        var pvClass = PineValueClass.Create([
            new KeyValuePair<IReadOnlyList<int>, PineValue>([0], PineValue.Blob([1]))
        ]);

        var result = pvClass.CreateMinimalValue();

        result.Should().BeOfType<PineValue.ListValue>();
        var listValue = (PineValue.ListValue)result;

        // Should only have 1 item (the minimum required)
        listValue.Items.Length.Should().Be(1);
    }
}
