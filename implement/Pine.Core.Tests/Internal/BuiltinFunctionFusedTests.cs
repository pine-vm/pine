using AwesomeAssertions;
using Pine.Core.Internal;
using System.Collections.Generic;
using Xunit;

namespace Pine.Core.Tests.Internal;

public class BuiltinFunctionFusedTests
{
    [Fact]
    public void SkipAndHead_on_list_matches_unfused_behavior()
    {
        var argument =
            PineValue.List(
                [
                PineValue.Blob([10]),
                PineValue.Blob([20]),
                PineValue.Blob([30])
                ]);

        foreach (var skipCount in new[] { -1, 0, 1, 2, 3, 10, int.MaxValue })
        {
            var expected =
                BuiltinFunction.head(
                    BuiltinFunctionFused.SkipAndTake(
                        takeCount: 1,
                        skipCount,
                        argument));

            BuiltinFunctionFused.SkipAndHead(skipCount, argument)
                .Should()
                .BeSameAs(expected);
        }
    }

    [Fact]
    public void SkipAndHead_on_blob_matches_unfused_behavior()
    {
        var argument = PineValue.Blob([10, 20, 30]);

        foreach (var skipCount in new[] { -1, 0, 1, 2, 3, 10, int.MaxValue })
        {
            var expected =
                BuiltinFunction.head(
                    BuiltinFunctionFused.SkipAndTake(
                        takeCount: 1,
                        skipCount,
                        argument));

            BuiltinFunctionFused.SkipAndHead(skipCount, argument)
                .Should()
                .BeSameAs(expected);
        }
    }

    [Fact]
    public void ConcatAndReverse_SymmetricBehavior()
    {
        IReadOnlyList<PineValue[]> testCases =
            [
            [],

            [PineValue.EmptyList],

            [PineValue.Blob([1, 2, 3])],

            [
            PineValue.Blob([10, 20]),
            PineValue.Blob([30, 40])
            ],

            [
            PineValue.List(
                [
                PineValue.Blob([1]),
                PineValue.Blob([2])
                ]),
            PineValue.List(
                [
                PineValue.Blob([3]),
                PineValue.Blob([4])
                ])
            ],

            [
            PineValue.Blob([10, 20]),
            PineValue.List(
                [
                PineValue.Blob([30]),
                PineValue.Blob([40])
                ]),
            PineValue.Blob([50, 60])
            ]
            ];

        for (var i = 0; i < testCases.Count; i++)
        {
            var inputValues = testCases[i];

            var referenceResult =
                BuiltinFunction.reverse(
                    BuiltinFunction.concat(
                        PineValue.List(inputValues)));

            var testResult = BuiltinFunctionFused.ConcatAndReverse(inputValues);

            testResult.Should().Be(referenceResult);
        }
    }
}
