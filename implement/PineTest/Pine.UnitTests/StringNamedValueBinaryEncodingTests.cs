using AwesomeAssertions;
using Pine.Core;
using Pine.Core.PopularEncodings;
using System.Collections.Generic;
using System.Collections.Immutable;
using Xunit;

namespace Pine.UnitTests;

public class StringNamedValueBinaryEncodingTests
{
    [Fact]
    public void Roundtrips()
    {
        IReadOnlyList<IReadOnlyDictionary<string, PineValue>> testCases =
            [
                ImmutableDictionary<string, PineValue>.Empty
                .SetItem("a", PineValue.EmptyBlob),

                ImmutableDictionary<string, PineValue>.Empty
                .SetItem(
                    "alfa",
                    PineValue.List(
                        [
                        StringEncoding.ValueFromString("tag-a"),
                        StringEncoding.ValueFromString("Sed ut perspiciatis unde omnis iste natus error sit voluptatem accusantium doloremque laudantium, totam rem aperiam, eaque ipsa quae ab illo inventore veritatis et quasi architecto beatae vitae dicta sunt explicabo."),
                        ]))
                .SetItem(
                    "beta",
                    PineValue.List(
                        [
                        StringEncoding.ValueFromString("tag-b"),
                        StringEncoding.ValueFromString("Sed ut perspiciatis unde omnis iste natus error sit voluptatem accusantium doloremque laudantium, totam rem aperiam, eaque ipsa quae ab illo inventore veritatis et quasi architecto beatae vitae dicta sunt explicabo."),
                        ]))
            ];

        for (var i = 0; i < testCases.Count; i++)
        {
            var testCase = testCases[i];

            try
            {
                using var encodedStream = new System.IO.MemoryStream();

                StringNamedPineValueBinaryEncoding.Encode(
                    encodedStream,
                    testCase);

                var encodedFlat = encodedStream.ToArray();

                var decoded =
                    StringNamedPineValueBinaryEncoding.Decode(encodedFlat).decls;

                decoded.Should().BeEquivalentTo(testCase);
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
