using AwesomeAssertions;
using Xunit;

namespace Pine.Core.Tests;

public class PineValueTests
{
    [Fact]
    public void Blob_reused_instance_lookup_allocates_no_objects()
    {
        System.ReadOnlyMemory<byte> bytes =
            new byte[]
            {
                0,
                0,
                0,
                (byte)'P',
                0,
                0,
                0,
                (byte)'i',
                0,
                0,
                0,
                (byte)'n',
                0,
                0,
                0,
                (byte)'e'
            };

        var expected = PineValue.Blob(bytes);

        PineValue.ReusedBlobInstances.Contains(expected).Should().BeTrue();

        for (var i = 0; i < 10; ++i)
            _ = PineValue.Blob(bytes);

        var allocatedBefore = System.GC.GetAllocatedBytesForCurrentThread();

        PineValue.BlobValue? actual = null;

        for (var i = 0; i < 100; ++i)
            actual = PineValue.Blob(bytes);

        var allocatedBytes =
            System.GC.GetAllocatedBytesForCurrentThread() - allocatedBefore;

        ReferenceEquals(actual, expected).Should().BeTrue();
        allocatedBytes.Should().Be(0);
    }

    [Fact]
    public void Blob_unreused_instance_lookup_allocates_only_returned_instance()
    {
        System.ReadOnlyMemory<byte> bytes = new byte[] { 17, 31, 47, 63, 79 };

        _ = new PineValue.BlobValue(bytes);
        _ = PineValue.Blob(bytes);

        const int Iterations = 100;

        var directAllocatedBefore = System.GC.GetAllocatedBytesForCurrentThread();

        PineValue.BlobValue? direct = null;

        for (var i = 0; i < Iterations; ++i)
            direct = new PineValue.BlobValue(bytes);

        var directAllocatedBytes =
            System.GC.GetAllocatedBytesForCurrentThread() - directAllocatedBefore;

        var factoryAllocatedBefore = System.GC.GetAllocatedBytesForCurrentThread();

        PineValue.BlobValue? fromFactory = null;

        for (var i = 0; i < Iterations; ++i)
            fromFactory = PineValue.Blob(bytes);

        var factoryAllocatedBytes =
            System.GC.GetAllocatedBytesForCurrentThread() - factoryAllocatedBefore;

        System.GC.KeepAlive(direct);
        System.GC.KeepAlive(fromFactory);

        factoryAllocatedBytes.Should().Be(directAllocatedBytes);
        ReferenceEquals(fromFactory, direct).Should().BeFalse();
        fromFactory.Should().Be(direct);
    }

    [Fact]
    public void Pine_list_value_content_counts()
    {
        var testCases =
            new[]
            {
                new
                {
                    testName = "empty list",

                    listValue =
                    PineValue.List([]),

                    expectedNodeCount = 0,
                    expectedByteCount = 0
                },

                new
                {
                    testName = "list with one empty list",

                    listValue =
                    PineValue.List([PineValue.List([])]),

                    expectedNodeCount = 1,
                    expectedByteCount = 0
                },

                new
                {
                    testName = "list with one list containing empty list",

                    listValue =
                    PineValue.List([PineValue.List([PineValue.List([])])]),

                    expectedNodeCount = 2,
                    expectedByteCount = 0
                },

                new
                {
                    testName = "list (blob(1))",

                    listValue =
                    PineValue.List([PineValue.Blob([1])]),

                    expectedNodeCount = 1,
                    expectedByteCount = 1
                },

                new
                {
                    testName = "list (blob(1),blob(3))",

                    listValue =
                    PineValue.List([PineValue.Blob([123]),PineValue.Blob([1,2,3])]),

                    expectedNodeCount = 2,
                    expectedByteCount = 4
                },
            };

        foreach (var testCase in testCases)
        {
            testCase.listValue.NodesCount.Should().Be(
                testCase.expectedNodeCount,
                testCase.testName + " - node count");

            testCase.listValue.BlobsBytesCount.Should().Be(
                testCase.expectedByteCount,
                testCase.testName + " - byte count");
        }
    }

    [Fact]
    public void Pine_list_value_max_depth()
    {
        var testCases =
            new[]
            {
                new
                {
                    testName = "empty list has depth 1",
                    listValue = PineValue.List([]),
                    expectedMaxDepth = 1L
                },

                new
                {
                    testName = "list with only blobs has depth 1",
                    listValue = PineValue.List([PineValue.Blob([1]), PineValue.Blob([2, 3])]),
                    expectedMaxDepth = 1L
                },

                new
                {
                    testName = "list with one empty list has depth 2",
                    listValue = PineValue.List([PineValue.List([])]),
                    expectedMaxDepth = 2L
                },

                new
                {
                    testName = "list with one list containing empty list has depth 3",
                    listValue = PineValue.List([PineValue.List([PineValue.List([])])]),
                    expectedMaxDepth = 3L
                },

                new
                {
                    testName = "list with nested lists of different depths takes maximum",
                    listValue =
                    PineValue.List(
                        [
                        PineValue.List([]), // depth 2
                        PineValue.Blob([1]), // depth 1
                        PineValue.List([PineValue.List([])]), // depth 3
                        PineValue.List([PineValue.List([PineValue.List([])])]) // depth 4
                        ]),
                    expectedMaxDepth = 4L
                },

                new
                {
                    testName = "deeply nested list chain",
                    listValue =
                    PineValue.List(
                        [
                        PineValue.List(
                            [
                            PineValue.List(
                                [
                                PineValue.List(
                                    [
                                    PineValue.List(
                                        [
                                        PineValue.Blob([1])
                                        ])
                                    ])
                                ])
                            ])
                        ]),
                    expectedMaxDepth = 5L
                },

                new
                {
                    testName = "list with multiple items including nested lists",
                    listValue =
                    PineValue.List(
                        [
                        PineValue.Blob([1]),
                        PineValue.List([PineValue.Blob([2])]),
                        PineValue.Blob([3]),
                        PineValue.List([PineValue.List([PineValue.Blob([4])])])
                        ]),
                    expectedMaxDepth = 3L
                },
            };

        foreach (var testCase in testCases)
        {
            testCase.listValue.MaxDepth.Should().Be(
                testCase.expectedMaxDepth,
                testCase.testName);
        }
    }

    [Fact]
    public void List_value_equality_shallow_recursive_path_allocates_no_objects()
    {
        const int Width = 100;
        const int Iterations = 100;

        static PineValue.ListValue BuildValue()
        {
            var items = new PineValue[Width];

            for (var i = 0; i < items.Length; ++i)
                items[i] = BuildNestedList(PineValue.ListValue.RecursiveEqualityDepthThreshold - 1, (byte)i);

            return new PineValue.ListValue(items);
        }

        var valueA = BuildValue();
        var valueB = BuildValue();

        valueA.MaxDepth.Should().Be(PineValue.ListValue.RecursiveEqualityDepthThreshold);
        ReferenceEquals(valueA, valueB).Should().BeFalse();

        for (var i = 0; i < 10; ++i)
            valueA.Equals(valueB).Should().BeTrue();

        var allocatedBefore = System.GC.GetAllocatedBytesForCurrentThread();

        var equal = false;

        for (var i = 0; i < Iterations; ++i)
            equal = valueA.Equals(valueB);

        var allocatedBytes =
            System.GC.GetAllocatedBytesForCurrentThread() - allocatedBefore;

        equal.Should().BeTrue();
        allocatedBytes.Should().Be(0);
    }

    [Fact]
    public void List_value_equality_first_deep_level_recurses_into_shallow_child_without_allocating()
    {
        const int Iterations = 100;

        var valueA = BuildNestedList(PineValue.ListValue.RecursiveEqualityDepthThreshold + 1, 1);
        var valueB = BuildNestedList(PineValue.ListValue.RecursiveEqualityDepthThreshold + 1, 1);

        valueA.MaxDepth.Should().Be(PineValue.ListValue.RecursiveEqualityDepthThreshold + 1);

        for (var i = 0; i < 10; ++i)
            valueA.Equals(valueB).Should().BeTrue();

        var allocatedBefore = System.GC.GetAllocatedBytesForCurrentThread();

        var equal = false;

        for (var i = 0; i < Iterations; ++i)
            equal = valueA.Equals(valueB);

        var allocatedBytes =
            System.GC.GetAllocatedBytesForCurrentThread() - allocatedBefore;

        equal.Should().BeTrue();
        allocatedBytes.Should().Be(0);
    }

    [Fact]
    public void List_value_equality_deep_path_does_not_grow_stack_for_shallow_siblings()
    {
        const int Iterations = 100;

        static PineValue.ListValue BuildValue(int shallowSiblingCount)
        {
            var items = new PineValue[shallowSiblingCount + 1];

            for (var i = 0; i < shallowSiblingCount; ++i)
                items[i] = new PineValue.ListValue(new[] { PineValue.Blob([(byte)i]) });

            items[^1] = BuildNestedList(PineValue.ListValue.RecursiveEqualityDepthThreshold + 1, 1);

            return new PineValue.ListValue(items);
        }

        var narrowA = BuildValue(shallowSiblingCount: 1);
        var narrowB = BuildValue(shallowSiblingCount: 1);
        var wideA = BuildValue(shallowSiblingCount: 100);
        var wideB = BuildValue(shallowSiblingCount: 100);

        narrowA.MaxDepth.Should().Be(PineValue.ListValue.RecursiveEqualityDepthThreshold + 2);
        wideA.MaxDepth.Should().Be(PineValue.ListValue.RecursiveEqualityDepthThreshold + 2);

        for (var i = 0; i < 10; ++i)
        {
            narrowA.Equals(narrowB).Should().BeTrue();
            wideA.Equals(wideB).Should().BeTrue();
        }

        var narrowAllocatedBefore = System.GC.GetAllocatedBytesForCurrentThread();

        var narrowEqual = false;

        for (var i = 0; i < Iterations; ++i)
            narrowEqual = narrowA.Equals(narrowB);

        var narrowAllocatedBytes =
            System.GC.GetAllocatedBytesForCurrentThread() - narrowAllocatedBefore;

        var wideAllocatedBefore = System.GC.GetAllocatedBytesForCurrentThread();

        var wideEqual = false;

        for (var i = 0; i < Iterations; ++i)
            wideEqual = wideA.Equals(wideB);

        var wideAllocatedBytes =
            System.GC.GetAllocatedBytesForCurrentThread() - wideAllocatedBefore;

        narrowEqual.Should().BeTrue();
        wideEqual.Should().BeTrue();
        narrowAllocatedBytes.Should().BeGreaterThan(0);
        wideAllocatedBytes.Should().Be(narrowAllocatedBytes);
    }

    [Fact]
    public void List_value_equality_does_not_stack_overflow_on_deeply_nested_values()
    {
        // Comparing two structurally-equal, reference-distinct deeply nested list values previously
        // recursed once per nesting level (via ListValue equality), overflowing the call stack.
        // Equality now descends iteratively.
        //
        // The internal ListValue constructor is used directly to bypass the interning done by the
        // PineValue.List factory, so that the two values do not share references at any level.

        const int Depth = 100_000;

        static PineValue BuildDeeplyNested(byte innerLeaf)
        {
            var sibling = PineValue.Blob([4]);

            var nested = PineValue.Blob([innerLeaf, 2, 3]);

            for (var i = 0; i < Depth; ++i)
                nested = new PineValue.ListValue(new[] { sibling, nested });

            return nested;
        }

        var valueA = BuildDeeplyNested(1);
        var valueB = BuildDeeplyNested(1);

        // Distinct instances that are structurally equal.
        ReferenceEquals(valueA, valueB).Should().BeFalse();

        valueA.Equals(valueB).Should().BeTrue();
        valueA.GetHashCode().Should().Be(valueB.GetHashCode());

        // A difference at the innermost leaf must still be detected.
        var different = BuildDeeplyNested(9);

        valueA.Equals(different).Should().BeFalse();
    }

    private static PineValue.ListValue BuildNestedList(int depth, byte innerLeaf)
    {
        PineValue nested = PineValue.Blob([innerLeaf]);

        for (var i = 0; i < depth; ++i)
            nested = new PineValue.ListValue(new[] { nested });

        return (PineValue.ListValue)nested;
    }
}
