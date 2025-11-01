using AwesomeAssertions;
using Pine.Core.Internal;
using System.Collections.Generic;
using Xunit;

namespace Pine.Core.Tests.Internal;

public class KernelFunctionFusedTests
{
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
                KernelFunction.reverse(
                    KernelFunction.concat(
                        PineValue.List(inputValues)));

            var testResult = KernelFunctionFused.ConcatAndReverse(inputValues);

            testResult.Should().Be(referenceResult);
        }
    }
}
