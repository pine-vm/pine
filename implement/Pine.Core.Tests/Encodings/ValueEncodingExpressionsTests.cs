using AwesomeAssertions;
using Pine.Core.Addressing;
using Pine.Core.CodeAnalysis;
using Pine.Core.Encodings;
using Pine.Core.PineLanguage;
using Pine.Core.Runtime;
using System;
using System.Linq;
using Xunit;

namespace Pine.Core.Tests.Encodings;

public class ValueEncodingExpressionsTests
{
    [Fact]
    public void Blob_padding_expression_hash_matches_the_program()
    {
        var actual =
            Convert.ToHexStringLower(PineValueHashTree.ComputeHash(ValueEncodingExpressions.BlobPadBytesTo4).Span);

        actual.Should().Be(
            Convert.ToHexStringLower(ValueEncodingExpressions.BlobPadBytesTo4Hash),
            "the static external reference must match the encoded program hash {0}",
            actual);
    }

    [Fact]
    public void Blob_padding_preserves_all_byte_values_and_prepends_three_zero_bytes_per_byte()
    {
        var cache = new PineVMParseCache();
        var interpreter = DirectInterpreter.WithoutEvalCaching(cache);

        var expression =
            cache.ParseExpression(ValueEncodingExpressions.BlobPadBytesTo4)
            .Extract(error => throw new Exception(error));

        foreach (var length in new[] { 0, 1, 2, 3, 7, 15, 16, 17, 255, 256, 257, 513 })
        {
            var input = Enumerable.Range(0, length).Select(index => (byte)(index % 256)).ToArray();
            var expected = new byte[length * 4];

            for (var index = 0; index < input.Length; ++index)
                expected[index * 4 + 3] = input[index];

            interpreter.EvaluateExpressionDefault(expression, PineValue.List(PineValue.Blob(input)))
                .Should().Be(PineValue.Blob(expected));
        }
    }
}
