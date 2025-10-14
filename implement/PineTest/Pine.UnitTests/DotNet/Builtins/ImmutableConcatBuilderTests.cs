using AwesomeAssertions;
using Pine.Core;
using Pine.Core.DotNet.Builtins;
using System.Collections.Generic;
using System.Linq;
using Xunit;

namespace Pine.UnitTests.DotNet.Builtins;

public class ImmutableConcatBuilderTests
{
    [Fact]
    public void Create_with_empty_list_produces_empty_result()
    {
        var items = System.Array.Empty<PineValue>();
        var builder = ImmutableConcatBuilder.Create(items);

        var result = builder.Evaluate();
        var expected = KernelFunction.concat(PineValue.List(items));

        result.Should().Be(expected);
        builder.AggregateItemsCount.Should().Be(0);
    }

    [Fact]
    public void Create_with_single_item_returns_that_item()
    {
        var item = PineValue.Blob([1, 2, 3]);
        var items = new[] { item };
        var builder = ImmutableConcatBuilder.Create(items);

        var result = builder.Evaluate();
        var expected = KernelFunction.concat(PineValue.List(items));

        result.Should().Be(expected);
        builder.AggregateItemsCount.Should().Be(1);
    }

    [Fact]
    public void Create_with_multiple_items_concatenates_them()
    {
        var list1 = PineValue.List([PineValue.Blob([1])]);
        var list2 = PineValue.List([PineValue.Blob([2])]);
        var list3 = PineValue.List([PineValue.Blob([3])]);

        PineValue[] items = [list1, list2, list3];
        var builder = ImmutableConcatBuilder.Create(items);

        var result = builder.Evaluate();
        var expected = KernelFunction.concat(PineValue.List(items));

        result.Should().Be(expected);
        builder.AggregateItemsCount.Should().Be(3);
    }

    [Fact]
    public void AppendItem_adds_single_item_to_end()
    {
        var list1 = PineValue.List([PineValue.Blob([1])]);
        var list2 = PineValue.List([PineValue.Blob([2])]);

        var builder = ImmutableConcatBuilder.Create([list1])
            .AppendItem(list2);

        var result = builder.Evaluate();
        var expected = KernelFunction.concat(PineValue.List([list1, list2]));

        result.Should().Be(expected);
        builder.AggregateItemsCount.Should().Be(2);
    }

    [Fact]
    public void AppendItems_adds_multiple_items_to_end()
    {
        var list1 = PineValue.List([PineValue.Blob([1])]);
        var list2 = PineValue.List([PineValue.Blob([2])]);
        var list3 = PineValue.List([PineValue.Blob([3])]);

        var builder = ImmutableConcatBuilder.Create([list1])
            .AppendItems([list2, list3]);

        var result = builder.Evaluate();
        var expected = KernelFunction.concat(PineValue.List([list1, list2, list3]));

        result.Should().Be(expected);
        builder.AggregateItemsCount.Should().Be(3);
    }

    [Fact]
    public void AppendItems_with_empty_list_does_not_change_result()
    {
        var list1 = PineValue.List([PineValue.Blob([1])]);

        var builder = ImmutableConcatBuilder.Create([list1])
            .AppendItems([]);

        var result = builder.Evaluate();
        var expected = KernelFunction.concat(PineValue.List([list1]));

        result.Should().Be(expected);
        builder.AggregateItemsCount.Should().Be(1);
    }

    [Fact]
    public void PrependItem_adds_single_item_to_beginning()
    {
        var list1 = PineValue.List([PineValue.Blob([2])]);
        var list2 = PineValue.List([PineValue.Blob([1])]);

        var builder = ImmutableConcatBuilder.Create([list1])
            .PrependItem(list2);

        var result = builder.Evaluate();
        var expected = KernelFunction.concat(PineValue.List([list2, list1]));

        result.Should().Be(expected);
        builder.AggregateItemsCount.Should().Be(2);
    }

    [Fact]
    public void PrependItems_adds_multiple_items_to_beginning()
    {
        var list1 = PineValue.List([PineValue.Blob([3])]);
        var list2 = PineValue.List([PineValue.Blob([1])]);
        var list3 = PineValue.List([PineValue.Blob([2])]);

        var builder = ImmutableConcatBuilder.Create([list1])
            .PrependItems([list2, list3]);

        var result = builder.Evaluate();
        var expected = KernelFunction.concat(PineValue.List([list2, list3, list1]));

        result.Should().Be(expected);
        builder.AggregateItemsCount.Should().Be(3);
    }

    [Fact]
    public void PrependItems_with_empty_list_does_not_change_result()
    {
        var list1 = PineValue.List([PineValue.Blob([1])]);

        var builder = ImmutableConcatBuilder.Create([list1])
            .PrependItems([]);

        var result = builder.Evaluate();
        var expected = KernelFunction.concat(PineValue.List([list1]));

        result.Should().Be(expected);
        builder.AggregateItemsCount.Should().Be(1);
    }

    [Fact]
    public void Chaining_multiple_appends_maintains_order()
    {
        var list1 = PineValue.List([PineValue.Blob([1])]);
        var list2 = PineValue.List([PineValue.Blob([2])]);
        var list3 = PineValue.List([PineValue.Blob([3])]);
        var list4 = PineValue.List([PineValue.Blob([4])]);

        var builder = ImmutableConcatBuilder.Create([list1])
            .AppendItem(list2)
            .AppendItem(list3)
            .AppendItem(list4);

        var result = builder.Evaluate();
        var expected = KernelFunction.concat(PineValue.List([list1, list2, list3, list4]));

        result.Should().Be(expected);
        builder.AggregateItemsCount.Should().Be(4);
    }

    [Fact]
    public void Chaining_multiple_prepends_maintains_order()
    {
        var list1 = PineValue.List([PineValue.Blob([4])]);
        var list2 = PineValue.List([PineValue.Blob([3])]);
        var list3 = PineValue.List([PineValue.Blob([2])]);
        var list4 = PineValue.List([PineValue.Blob([1])]);

        var builder = ImmutableConcatBuilder.Create([list1])
            .PrependItem(list2)
            .PrependItem(list3)
            .PrependItem(list4);

        var result = builder.Evaluate();
        var expected = KernelFunction.concat(PineValue.List([list4, list3, list2, list1]));

        result.Should().Be(expected);
        builder.AggregateItemsCount.Should().Be(4);
    }

    [Fact]
    public void Mixed_prepend_and_append_operations_maintain_correct_order()
    {
        var list1 = PineValue.List([PineValue.Blob([3])]);
        var list2 = PineValue.List([PineValue.Blob([2])]);
        var list3 = PineValue.List([PineValue.Blob([4])]);
        var list4 = PineValue.List([PineValue.Blob([1])]);
        var list5 = PineValue.List([PineValue.Blob([5])]);

        var builder = ImmutableConcatBuilder.Create([list1])
            .PrependItem(list2)
            .AppendItem(list3)
            .PrependItem(list4)
            .AppendItem(list5);

        var result = builder.Evaluate();
        var expected = KernelFunction.concat(PineValue.List([list4, list2, list1, list3, list5]));

        result.Should().Be(expected);
        builder.AggregateItemsCount.Should().Be(5);
    }

    [Fact]
    public void Builder_is_immutable_original_unchanged_after_append()
    {
        var list1 = PineValue.List([PineValue.Blob([1])]);
        var list2 = PineValue.List([PineValue.Blob([2])]);

        var original = ImmutableConcatBuilder.Create([list1]);
        var modified = original.AppendItem(list2);

        var originalResult = original.Evaluate();
        var modifiedResult = modified.Evaluate();

        var originalExpected = KernelFunction.concat(PineValue.List([list1]));
        var modifiedExpected = KernelFunction.concat(PineValue.List([list1, list2]));

        originalResult.Should().Be(originalExpected);
        modifiedResult.Should().Be(modifiedExpected);
        original.AggregateItemsCount.Should().Be(1);
        modified.AggregateItemsCount.Should().Be(2);
    }

    [Fact]
    public void Builder_is_immutable_original_unchanged_after_prepend()
    {
        var list1 = PineValue.List([PineValue.Blob([2])]);
        var list2 = PineValue.List([PineValue.Blob([1])]);

        var original = ImmutableConcatBuilder.Create([list1]);
        var modified = original.PrependItem(list2);

        var originalResult = original.Evaluate();
        var modifiedResult = modified.Evaluate();

        var originalExpected = KernelFunction.concat(PineValue.List([list1]));
        var modifiedExpected = KernelFunction.concat(PineValue.List([list2, list1]));

        originalResult.Should().Be(originalExpected);
        modifiedResult.Should().Be(modifiedExpected);
        original.AggregateItemsCount.Should().Be(1);
        modified.AggregateItemsCount.Should().Be(2);
    }

    [Fact]
    public void Large_collection_can_be_built_efficiently()
    {
        var builder = ImmutableConcatBuilder.Create([]);
        var items = new List<PineValue>();

        for (var i = 0; i < 1000; i++)
        {
            var item = PineValue.List([PineValue.Blob([(byte)(i % 256)])]);
            builder = builder.AppendItem(item);
            items.Add(item);
        }

        var result = builder.Evaluate();
        var expected = KernelFunction.concat(PineValue.List([.. items]));

        result.Should().Be(expected);
        builder.AggregateItemsCount.Should().Be(1000);
    }

    [Fact]
    public void Deep_nesting_of_nodes_evaluates_correctly()
    {
        var items = new List<PineValue>();
        var builder = ImmutableConcatBuilder.Create([PineValue.List([PineValue.Blob([1])])]);
        items.Add(PineValue.List([PineValue.Blob([1])]));

        // Create deep nesting by repeatedly wrapping
        for (var i = 2; i <= 10; i++)
        {
            var item = PineValue.List([PineValue.Blob([(byte)i])]);
            builder = builder.AppendItem(item);
            items.Add(item);
        }

        var result = builder.Evaluate();
        var expected = KernelFunction.concat(PineValue.List([.. items]));

        result.Should().Be(expected);
        builder.AggregateItemsCount.Should().Be(10);
    }

    [Fact]
    public void Leaf_node_with_multiple_items_evaluates_correctly()
    {
        PineValue[] items = [.. Enumerable.Range(1, 5)
            .Select(i => PineValue.List([PineValue.Blob([(byte)i])]))];

        var leaf = new ImmutableConcatBuilder.Leaf(items);

        var result = leaf.Evaluate();
        var expected = KernelFunction.concat(PineValue.List(items));

        result.Should().Be(expected);
        leaf.AggregateItemsCount.Should().Be(5);
    }

    [Fact]
    public void Node_with_single_child_evaluates_correctly()
    {
        var item = PineValue.List([PineValue.Blob([1])]);
        var leaf = new ImmutableConcatBuilder.Leaf([item]);
        var node = new ImmutableConcatBuilder.Node([leaf]);

        var result = node.Evaluate();
        PineValue[] items = [item];
        var expected = KernelFunction.concat(PineValue.List(items));

        result.Should().Be(expected);
        node.AggregateItemsCount.Should().Be(1);
    }

    [Fact]
    public void Node_with_multiple_children_evaluates_correctly()
    {
        PineValue[] items =
        [
            PineValue.List([PineValue.Blob([1])]),
            PineValue.List([PineValue.Blob([2])]),
            PineValue.List([PineValue.Blob([3])]),
            PineValue.List([PineValue.Blob([4])])
        ];

        var leaf1 = new ImmutableConcatBuilder.Leaf([items[0], items[1]]);
        var leaf2 = new ImmutableConcatBuilder.Leaf([items[2], items[3]]);
        var node = new ImmutableConcatBuilder.Node([leaf1, leaf2]);

        var result = node.Evaluate();
        var expected = KernelFunction.concat(PineValue.List(items));

        result.Should().Be(expected);
        node.AggregateItemsCount.Should().Be(4);
    }

    [Fact]
    public void Node_with_nested_nodes_evaluates_correctly()
    {
        PineValue[] items =
        [
            PineValue.List([PineValue.Blob([1])]),
            PineValue.List([PineValue.Blob([2])]),
            PineValue.List([PineValue.Blob([3])])
        ];

        var leaf1 = new ImmutableConcatBuilder.Leaf([items[0]]);
        var leaf2 = new ImmutableConcatBuilder.Leaf([items[1]]);
        var node1 = new ImmutableConcatBuilder.Node([leaf1, leaf2]);

        var leaf3 = new ImmutableConcatBuilder.Leaf([items[2]]);
        var node2 = new ImmutableConcatBuilder.Node([node1, leaf3]);

        var result = node2.Evaluate();
        var expected = KernelFunction.concat(PineValue.List(items));

        result.Should().Be(expected);
        node2.AggregateItemsCount.Should().Be(3);
    }

    [Fact]
    public void Empty_leaf_evaluates_to_empty_list()
    {
        var leaf = new ImmutableConcatBuilder.Leaf([]);

        var result = leaf.Evaluate();
        var expected = KernelFunction.concat(PineValue.EmptyList);

        result.Should().Be(expected);
        leaf.AggregateItemsCount.Should().Be(0);
    }

    [Fact]
    public void Combining_lists_as_pine_values()
    {
        var list1 = PineValue.List([PineValue.Blob([1]), PineValue.Blob([2])]);
        var list2 = PineValue.List([PineValue.Blob([3]), PineValue.Blob([4])]);

        var builder = ImmutableConcatBuilder.Create([list1, list2]);

        var result = builder.Evaluate();
        var expected = KernelFunction.concat(PineValue.List([list1, list2]));

        result.Should().Be(expected);
        builder.AggregateItemsCount.Should().Be(2);
    }

    [Fact]
    public void AppendItems_and_PrependItems_with_large_collections()
    {
        PineValue[] initial = [.. Enumerable.Range(1, 10)
            .Select(i => PineValue.List([PineValue.Blob([(byte)i])]))];

        PineValue[] toAppend = [.. Enumerable.Range(11, 10)
            .Select(i => PineValue.List([PineValue.Blob([(byte)i])]))];

        PineValue[] toPrepend = [.. Enumerable.Range(21, 10)
            .Select(i => PineValue.List([PineValue.Blob([(byte)i])]))];

        var builder = ImmutableConcatBuilder.Create(initial)
            .AppendItems(toAppend)
            .PrependItems(toPrepend);

        var result = builder.Evaluate();

        // Build the expected list by concatenating all items in the correct order
        PineValue[] allItems = [.. toPrepend.Concat(initial).Concat(toAppend)];
        var expected = KernelFunction.concat(PineValue.List(allItems));

        result.Should().Be(expected);
        builder.AggregateItemsCount.Should().Be(30);
    }

    [Fact]
    public void Concat_of_blobs_produces_single_blob()
    {
        var blob1 = PineValue.Blob([1, 2, 3]);
        var blob2 = PineValue.Blob([4, 5]);
        var blob3 = PineValue.Blob([6, 7, 8, 9]);

        var items = new[] { blob1, blob2, blob3 };
        var builder = ImmutableConcatBuilder.Create(items);

        var result = builder.Evaluate();
        var expected = KernelFunction.concat(PineValue.List(items));

        result.Should().Be(expected);
        builder.AggregateItemsCount.Should().Be(3);
    }

    [Fact]
    public void Concat_of_single_blob_returns_that_blob()
    {
        var blob = PineValue.Blob([1, 2, 3, 4, 5]);

        var items = new[] { blob };
        var builder = ImmutableConcatBuilder.Create(items);

        var result = builder.Evaluate();
        var expected = KernelFunction.concat(PineValue.List(items));

        result.Should().Be(expected);
        builder.AggregateItemsCount.Should().Be(1);
    }

    [Fact]
    public void EvaluateReverse_with_single_item_returns_that_item()
    {
        var item = PineValue.List([PineValue.Blob([1])]);
        var builder = ImmutableConcatBuilder.Create([item]);

        var result = builder.EvaluateReverse();
        var expected = KernelFunction.concat(PineValue.List([item]));

        result.Should().Be(expected);
    }

    [Fact]
    public void EvaluateReverse_with_multiple_items_reverses_order()
    {
        var list1 = PineValue.List([PineValue.Blob([1])]);
        var list2 = PineValue.List([PineValue.Blob([2])]);
        var list3 = PineValue.List([PineValue.Blob([3])]);

        var builder =
            ImmutableConcatBuilder.Create([list1, list2, list3]);

        var result = builder.EvaluateReverse();

        var expected = KernelFunction.concat(PineValue.List([list3, list2, list1]));

        result.Should().Be(expected);
    }

    [Fact]
    public void EvaluateReverse_with_append_reverses_order()
    {
        var list1 = PineValue.List([PineValue.Blob([1])]);
        var list2 = PineValue.List([PineValue.Blob([2])]);
        var list3 = PineValue.List([PineValue.Blob([3])]);

        var builder =
            ImmutableConcatBuilder.Create([list1])
            .AppendItem(list2)
            .AppendItem(list3);

        var result = builder.EvaluateReverse();

        var expected = KernelFunction.concat(PineValue.List([list3, list2, list1]));

        result.Should().Be(expected);
    }

    [Fact]
    public void EvaluateReverse_with_prepend_reverses_order()
    {
        var list1 = PineValue.List([PineValue.Blob([3])]);
        var list2 = PineValue.List([PineValue.Blob([2])]);
        var list3 = PineValue.List([PineValue.Blob([1])]);

        var builder =
            ImmutableConcatBuilder.Create([list1])
            .PrependItem(list2)
            .PrependItem(list3);

        var result = builder.EvaluateReverse();
        var expected = KernelFunction.concat(PineValue.List([list1, list2, list3]));

        result.Should().Be(expected);
    }

    [Fact]
    public void EvaluateReverse_with_mixed_operations_reverses_order()
    {
        var list1 = PineValue.List([PineValue.Blob([2])]);
        var list2 = PineValue.List([PineValue.Blob([1])]);
        var list3 = PineValue.List([PineValue.Blob([3])]);

        var builder =
            ImmutableConcatBuilder.Create([list1])
            .PrependItem(list2)
            .AppendItem(list3);

        var result = builder.EvaluateReverse();
        var expected = KernelFunction.concat(PineValue.List([list3, list1, list2]));

        result.Should().Be(expected);
    }

    [Fact]
    public void EvaluateReverse_with_empty_list_returns_empty()
    {
        var builder = ImmutableConcatBuilder.Create([]);

        var result = builder.EvaluateReverse();
        var expected = KernelFunction.concat(PineValue.EmptyList);

        result.Should().Be(expected);
    }

    [Fact]
    public void EvaluateReverse_with_nested_nodes_reverses_correctly()
    {
        var list1 = PineValue.List([PineValue.Blob([1])]);
        var list2 = PineValue.List([PineValue.Blob([2])]);
        var list3 = PineValue.List([PineValue.Blob([3])]);
        var list4 = PineValue.List([PineValue.Blob([4])]);

        var builder =
            ImmutableConcatBuilder.Create([list1, list2])
            .AppendItems([list3, list4]);

        var result = builder.EvaluateReverse();
        var expected = KernelFunction.concat(PineValue.List([list4, list3, list2, list1]));

        result.Should().Be(expected);
    }

    [Fact]
    public void EvaluateReverse_matches_reverse_of_evaluate()
    {
        var list1 = PineValue.List([PineValue.Blob([1])]);
        var list2 = PineValue.List([PineValue.Blob([2])]);
        var list3 = PineValue.List([PineValue.Blob([3])]);

        var builder =
            ImmutableConcatBuilder.Create([list1])
            .AppendItem(list2)
            .PrependItem(list3);

        var evaluateReverse = builder.EvaluateReverse();
        var evaluate = builder.Evaluate();
        var reverseOfEvaluate = KernelFunction.reverse(evaluate);

        evaluateReverse.Should().Be(reverseOfEvaluate);
    }

    [Fact]
    public void IsList_returns_true_when_concatenating_lists()
    {
        var list1 = PineValue.List([PineValue.Blob([1])]);
        var list2 = PineValue.List([PineValue.Blob([2])]);

        var builder = ImmutableConcatBuilder.Create([list1, list2]);

        VerifyConsistencyOfDerivedProperties(builder);
    }

    [Fact]
    public void IsList_returns_true_when_any_item_is_list()
    {
        var blob1 = PineValue.Blob([1, 2, 3]);
        var list1 = PineValue.List([PineValue.Blob([4])]);
        var blob2 = PineValue.Blob([5, 6]);

        var builder = ImmutableConcatBuilder.Create([blob1, list1, blob2]);

        VerifyConsistencyOfDerivedProperties(builder);
    }

    [Fact]
    public void IsList_returns_false_when_all_items_are_blobs()
    {
        var blob1 = PineValue.Blob([1, 2, 3]);
        var blob2 = PineValue.Blob([4, 5]);
        var blob3 = PineValue.Blob([6, 7, 8, 9]);

        var builder = ImmutableConcatBuilder.Create([blob1, blob2, blob3]);

        VerifyConsistencyOfDerivedProperties(builder);
    }

    [Fact]
    public void IsList_returns_false_for_empty_builder()
    {
        var builder = ImmutableConcatBuilder.Create([]);

        VerifyConsistencyOfDerivedProperties(builder);
    }

    [Fact]
    public void IsBlob_returns_true_when_concatenating_blobs()
    {
        var blob1 = PineValue.Blob([1, 2, 3]);
        var blob2 = PineValue.Blob([4, 5]);
        var blob3 = PineValue.Blob([6, 7, 8, 9]);

        var builder = ImmutableConcatBuilder.Create([blob1, blob2, blob3]);

        VerifyConsistencyOfDerivedProperties(builder);
    }

    [Fact]
    public void IsBlob_returns_false_when_any_item_is_list()
    {
        var blob1 = PineValue.Blob([1, 2, 3]);
        var list1 = PineValue.List([PineValue.Blob([4])]);
        var blob2 = PineValue.Blob([5, 6]);

        var builder = ImmutableConcatBuilder.Create([blob1, list1, blob2]);

        VerifyConsistencyOfDerivedProperties(builder);
    }

    [Fact]
    public void IsBlob_returns_true_for_empty_builder()
    {
        var builder = ImmutableConcatBuilder.Create([]);

        VerifyConsistencyOfDerivedProperties(builder);
    }

    [Fact]
    public void IsList_and_IsBlob_work_with_nested_nodes()
    {
        var blob1 = PineValue.Blob([1, 2, 3]);
        var blob2 = PineValue.Blob([4, 5]);

        var builder = ImmutableConcatBuilder.Create([blob1])
            .AppendItem(blob2);

        // Both items are blobs
        VerifyConsistencyOfDerivedProperties(builder);

        var list1 = PineValue.List([PineValue.Blob([6])]);
        var builderWithList = builder.AppendItem(list1);

        // Now one item is a list
        VerifyConsistencyOfDerivedProperties(builderWithList);
    }

    [Fact]
    public void IsList_returns_true_for_nested_nodes_with_list()
    {
        var list1 = PineValue.List([PineValue.Blob([1])]);
        var list2 = PineValue.List([PineValue.Blob([2])]);
        var list3 = PineValue.List([PineValue.Blob([3])]);

        var builder = ImmutableConcatBuilder.Create([list1])
            .AppendItem(list2)
            .PrependItem(list3);

        VerifyConsistencyOfDerivedProperties(builder);
    }

    [Fact]
    public void IsBlob_returns_true_for_nested_nodes_with_blobs()
    {
        var blob1 = PineValue.Blob([1]);
        var blob2 = PineValue.Blob([2]);
        var blob3 = PineValue.Blob([3]);

        var builder = ImmutableConcatBuilder.Create([blob1])
            .AppendItem(blob2)
            .PrependItem(blob3);

        VerifyConsistencyOfDerivedProperties(builder);
    }

    private static void VerifyConsistencyOfDerivedProperties(ImmutableConcatBuilder builder)
    {
        var isList = builder.IsList();
        var isBlob = builder.IsBlob();
        var evaluated = builder.Evaluate();

        // Verify IsList and IsBlob are mutually exclusive
        (isList != isBlob).Should().BeTrue();

        // Verify the type checks match the evaluated result
        (evaluated is PineValue.ListValue).Should().Be(isList);
        (evaluated is PineValue.BlobValue).Should().Be(isBlob);
    }
}
