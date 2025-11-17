using AwesomeAssertions;
using Pine.Core.CommonEncodings;
using System.Collections.Generic;
using System.Linq;
using Xunit;

namespace Pine.Core.Tests;

public class ValueBinaryEncodingClassicTests
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

        ValueBinaryEncodingClassic.Encode(compositionAlfaEncodedBytes, compositionAlfa);

        compositionAlfaEncodedBytes.Seek(
            offset: 0,
            System.IO.SeekOrigin.Begin);

        var reproducedAlfa =
            ValueBinaryEncodingClassic.DecodeRoot(compositionAlfaEncodedBytes.ToArray());

        using var compositionBetaEncodedBytes = new System.IO.MemoryStream();

        ValueBinaryEncodingClassic.Encode(compositionBetaEncodedBytes, compositionBeta);

        compositionBetaEncodedBytes.Seek(
            offset: 0,
            System.IO.SeekOrigin.Begin);

        var reproducedBeta =
            ValueBinaryEncodingClassic.DecodeRoot(compositionBetaEncodedBytes.ToArray());

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

                ValueBinaryEncodingClassic.Encode(encodedStream, testCase);

                var encodedFlat = encodedStream.ToArray();

                var decoded =
                    ValueBinaryEncodingClassic.DecodeRoot(encodedFlat);

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

    [Fact]
    public void New_32bit_encoding_is_smaller_than_64bit()
    {
        var testValue =
            PineValue.List(
                IntegerEncoding.EncodeSignedInteger(71),
                IntegerEncoding.EncodeSignedInteger(4171),
                IntegerEncoding.EncodeSignedInteger(134171),
                IntegerEncoding.EncodeSignedInteger(43134171),
                IntegerEncoding.EncodeSignedInteger(8143134171));

        // Encode with new 32-bit format
        using var encoded32Stream = new System.IO.MemoryStream();
        ValueBinaryEncodingClassic.Encode(encoded32Stream, testValue);
        var encoded32 = encoded32Stream.ToArray();

        // Encode with old 64-bit format by calling Encode64 directly via reflection
        var encode64Method = typeof(ValueBinaryEncodingClassic)
            .GetMethod("Encode64", System.Reflection.BindingFlags.NonPublic | System.Reflection.BindingFlags.Static);

        using var encoded64Stream = new System.IO.MemoryStream();

        void Write64(System.ReadOnlySpan<byte> bytes)
        {
            encoded64Stream.Write(bytes);
        }

        encode64Method!.Invoke(null, [testValue, (System.Action<System.ReadOnlySpan<byte>>)Write64, 0L, null]);
        var encoded64 = encoded64Stream.ToArray();

        // Verify that 32-bit encoding is smaller
        encoded32.Length.Should().BeLessThan(encoded64.Length);

        // Verify both decode correctly
        var decoded32 = ValueBinaryEncodingClassic.DecodeRoot(encoded32);
        decoded32.Should().Be(testValue);

        var decoded64 = ValueBinaryEncodingClassic.DecodeRoot(encoded64);
        decoded64.Should().Be(testValue);
    }

    [Fact]
    public void Backward_compatibility_reads_64bit_format()
    {
        // Create test values
        IReadOnlyList<PineValue> testCases =
            [
            PineValue.EmptyBlob,
            PineValue.EmptyList,
            IntegerEncoding.EncodeSignedInteger(71),
            PineValue.List(
                IntegerEncoding.EncodeSignedInteger(71),
                IntegerEncoding.EncodeSignedInteger(131)),
            ];

        foreach (var testCase in testCases)
        {
            // Encode with old 64-bit format using reflection
            var encode64Method =
                typeof(ValueBinaryEncodingClassic)
                .GetMethod("Encode64", System.Reflection.BindingFlags.NonPublic | System.Reflection.BindingFlags.Static);

            encode64Method.Should().NotBeNull();

            using var encoded64Stream = new System.IO.MemoryStream();

            void Write64(System.ReadOnlySpan<byte> bytes)
            {
                encoded64Stream.Write(bytes);
            }

            encode64Method!.Invoke(null, [testCase, (System.Action<System.ReadOnlySpan<byte>>)Write64, 0L, null]);

            var encoded64 = encoded64Stream.ToArray();

            // Verify that DecodeRoot can read the 64-bit format
            var decoded = ValueBinaryEncodingClassic.DecodeRoot(encoded64);
            decoded.Should().Be(testCase);
        }
    }
}
