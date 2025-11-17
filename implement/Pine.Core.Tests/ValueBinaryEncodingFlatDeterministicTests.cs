using AwesomeAssertions;
using Pine.Core.CommonEncodings;
using System.Collections.Generic;
using System.Linq;
using Xunit;

namespace Pine.Core.Tests;

public class ValueBinaryEncodingFlatDeterministicTests
{
    [Fact]
    public void Reuses_components()
    {
        var largeComponent =
            StringEncoding.ValueFromString(
                "building a value of size large enough so that non-duplicate encoding would become obvious");

        var compositionAlfa =
            PineValue.List(
                PineValue.List(
                    IntegerEncoding.EncodeSignedInteger(71),
                    largeComponent
                    ),
                PineValue.List(
                    IntegerEncoding.EncodeSignedInteger(91)
                    )
                );

        var compositionBeta =
            PineValue.List(
                PineValue.List(
                    IntegerEncoding.EncodeSignedInteger(71),
                    largeComponent
                    ),
                PineValue.List(
                    IntegerEncoding.EncodeSignedInteger(91),
                    largeComponent
                    ),
                largeComponent
                );

        using var compositionAlfaEncodedBytes = new System.IO.MemoryStream();

        ValueEncodingFlatDeterministic.Encode(compositionAlfaEncodedBytes, compositionAlfa);

        compositionAlfaEncodedBytes.Seek(
            offset: 0,
            System.IO.SeekOrigin.Begin);

        var reproducedAlfa =
            ValueEncodingFlatDeterministic.DecodeRoot(compositionAlfaEncodedBytes.ToArray());

        using var compositionBetaEncodedBytes = new System.IO.MemoryStream();

        ValueEncodingFlatDeterministic.Encode(compositionBetaEncodedBytes, compositionBeta);

        compositionBetaEncodedBytes.Seek(
            offset: 0,
            System.IO.SeekOrigin.Begin);

        var reproducedBeta =
            ValueEncodingFlatDeterministic.DecodeRoot(compositionBetaEncodedBytes.ToArray());

        reproducedAlfa.Should().Be(compositionAlfa);

        reproducedBeta.Should().Be(compositionBeta);

        compositionBetaEncodedBytes.Length.Should().BeLessThan(compositionAlfaEncodedBytes.Length * 2);
    }

    [Fact]
    public void Roundtrips()
    {
        IReadOnlyList<PineValue> testCases =
            [
            PineValue.EmptyBlob,
            PineValue.EmptyList,

            PineValue.List(PineValue.EmptyList),

            PineValue.List(PineValue.EmptyBlob),

            PineValue.List(
                PineValue.EmptyList,
                PineValue.EmptyList),

            PineValue.List(
                PineValue.EmptyList,
                PineValue.EmptyBlob),

            IntegerEncoding.EncodeSignedInteger(71),
            IntegerEncoding.EncodeSignedInteger(4171),

            PineValue.List(
                IntegerEncoding.EncodeSignedInteger(71),
                IntegerEncoding.EncodeSignedInteger(4171),
                IntegerEncoding.EncodeSignedInteger(134171),
                IntegerEncoding.EncodeSignedInteger(43134171),
                IntegerEncoding.EncodeSignedInteger(8143134171)),

            PineValue.List(
                IntegerEncoding.EncodeSignedInteger(71),
                IntegerEncoding.EncodeSignedInteger(131),
                IntegerEncoding.EncodeSignedInteger(71)),

            PineValue.List(
                IntegerEncoding.EncodeSignedInteger(47),
                IntegerEncoding.EncodeSignedInteger(71),
                IntegerEncoding.EncodeSignedInteger(131),
                IntegerEncoding.EncodeSignedInteger(71),
                IntegerEncoding.EncodeSignedInteger(47)),

            PineValue.List(
                IntegerEncoding.EncodeSignedInteger(47),
                PineValue.List(
                    IntegerEncoding.EncodeSignedInteger(71)),
                IntegerEncoding.EncodeSignedInteger(19),
                PineValue.List(
                    IntegerEncoding.EncodeSignedInteger(71))),

            PineValue.List(
                IntegerEncoding.EncodeSignedInteger(47),
                PineValue.List(
                    IntegerEncoding.EncodeSignedInteger(43)),
                PineValue.List(
                    IntegerEncoding.EncodeSignedInteger(91)),
                IntegerEncoding.EncodeSignedInteger(21),
                PineValue.List(
                    IntegerEncoding.EncodeSignedInteger(91)),
                PineValue.List(
                    IntegerEncoding.EncodeSignedInteger(43))),

            Pine.PineVM.PopularExpression.BuildPopularValueDictionary().Values
            .OfType<PineValue.ListValue>()
            .OrderByDescending(l => l.NodesCount)
            .First()
            ];

        for (var i = 0; i < testCases.Count; i++)
        {
            var testCase = testCases[i];

            try
            {
                using var encodedStream = new System.IO.MemoryStream();

                ValueEncodingFlatDeterministic.Encode(encodedStream, testCase);

                var encodedFlat = encodedStream.ToArray();

                var decoded =
                    ValueEncodingFlatDeterministic.DecodeRoot(encodedFlat);

                decoded.Should().Be(testCase);
            }
            catch (System.Exception ex)
            {
                throw new System.Exception(
                    "Failed for test case [" + i + "] (" + testCase + ")",
                    innerException: ex);
            }
        }
    }
}
