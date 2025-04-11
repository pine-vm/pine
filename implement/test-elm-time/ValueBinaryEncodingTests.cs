using Microsoft.VisualStudio.TestTools.UnitTesting;
using Pine.Core;
using System.Collections.Generic;
using System.Linq;

namespace TestElmTime;

[TestClass]
public class ValueBinaryEncodingTests
{
    [TestMethod]
    public void Reuses_components()
    {
        var largeComponent =
            PineValueAsString.ValueFromString(
                "building a value of size large enough so that non-duplicate encoding would become obvious");

        var compositionAlfa =
            PineValue.List(
                PineValue.List(
                    PineValueAsInteger.ValueFromSignedInteger(71),
                    largeComponent
                    ),
                PineValue.List(
                    PineValueAsInteger.ValueFromSignedInteger(91)
                    )
                );

        var compositionBeta =
            PineValue.List(
                PineValue.List(
                    PineValueAsInteger.ValueFromSignedInteger(71),
                    largeComponent
                    ),
                PineValue.List(
                    PineValueAsInteger.ValueFromSignedInteger(91),
                    largeComponent
                    ),
                largeComponent
                );

        using var compositionAlfaEncodedBytes = new System.IO.MemoryStream();

        PineValueBinaryEncoding.Encode(compositionAlfaEncodedBytes, compositionAlfa);

        compositionAlfaEncodedBytes.Seek(
            offset: 0,
            System.IO.SeekOrigin.Begin);

        var reproducedAlfa =
            PineValueBinaryEncoding.DecodeRoot(compositionAlfaEncodedBytes.ToArray());

        using var compositionBetaEncodedBytes = new System.IO.MemoryStream();

        PineValueBinaryEncoding.Encode(compositionBetaEncodedBytes, compositionBeta);

        compositionBetaEncodedBytes.Seek(
            offset: 0,
            System.IO.SeekOrigin.Begin);

        var reproducedBeta =
            PineValueBinaryEncoding.DecodeRoot(compositionBetaEncodedBytes.ToArray());

        Assert.AreEqual(
            expected: compositionAlfa,
            actual: reproducedAlfa);

        Assert.AreEqual(
            expected: compositionBeta,
            actual: reproducedBeta);

        Assert.IsTrue(compositionBetaEncodedBytes.Length < compositionAlfaEncodedBytes.Length * 2);
    }

    [TestMethod]
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

            PineValueAsInteger.ValueFromSignedInteger(71),
            PineValueAsInteger.ValueFromSignedInteger(4171),

            PineValue.List(
                PineValueAsInteger.ValueFromSignedInteger(71),
                PineValueAsInteger.ValueFromSignedInteger(4171),
                PineValueAsInteger.ValueFromSignedInteger(134171),
                PineValueAsInteger.ValueFromSignedInteger(43134171),
                PineValueAsInteger.ValueFromSignedInteger(8143134171)),

            PineValue.List(
                PineValueAsInteger.ValueFromSignedInteger(71),
                PineValueAsInteger.ValueFromSignedInteger(131),
                PineValueAsInteger.ValueFromSignedInteger(71)),

            PineValue.List(
                PineValueAsInteger.ValueFromSignedInteger(47),
                PineValueAsInteger.ValueFromSignedInteger(71),
                PineValueAsInteger.ValueFromSignedInteger(131),
                PineValueAsInteger.ValueFromSignedInteger(71),
                PineValueAsInteger.ValueFromSignedInteger(47)),

            PineValue.List(
                PineValueAsInteger.ValueFromSignedInteger(47),
                PineValue.List(
                    PineValueAsInteger.ValueFromSignedInteger(71)),
                PineValueAsInteger.ValueFromSignedInteger(19),
                PineValue.List(
                    PineValueAsInteger.ValueFromSignedInteger(71))),

            PineValue.List(
                PineValueAsInteger.ValueFromSignedInteger(47),
                PineValue.List(
                    PineValueAsInteger.ValueFromSignedInteger(43)),
                PineValue.List(
                    PineValueAsInteger.ValueFromSignedInteger(91)),
                PineValueAsInteger.ValueFromSignedInteger(21),
                PineValue.List(
                    PineValueAsInteger.ValueFromSignedInteger(91)),
                PineValue.List(
                    PineValueAsInteger.ValueFromSignedInteger(43))),

            Pine.PineVM.PopularExpression.BuildPopularValueDictionary().Values
            .OfType<PineValue.ListValue>()
            .OrderByDescending(l => l.NodesCount)
            .First()
            ];

        for (int i = 0; i < testCases.Count; i++)
        {
            PineValue testCase = testCases[i];

            try
            {
                using var encodedStream = new System.IO.MemoryStream();

                PineValueBinaryEncoding.Encode(encodedStream, testCase);

                var encodedFlat = encodedStream.ToArray();

                var decoded =
                    PineValueBinaryEncoding.DecodeRoot(encodedFlat);

                Assert.AreEqual(
                    expected: testCase,
                    actual: decoded);
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
