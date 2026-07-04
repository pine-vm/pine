using AwesomeAssertions;
using Xunit;

namespace Pine.Core.Tests;

public class PineValueTests
{
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
}
