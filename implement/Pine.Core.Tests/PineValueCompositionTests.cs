using AwesomeAssertions;
using Pine.Core.Addressing;
using Pine.Core.CommonEncodings;
using Pine.Core.Files;
using System;
using System.Collections.Immutable;
using System.Linq;
using Xunit;

namespace Pine.Core.Tests;

public class PineValueCompositionTests
{
    [Fact]
    public void Hash_composition()
    {
        var testCases =
            new[]
            {
                new
                {
                    input = PineValue.Blob([0,1,2]),
                    expectedHashBase16 =
                    Convert.ToHexStringLower(
                        System.Security.Cryptography.SHA256.HashData(
                            "blob 3\0"u8.ToArray().Concat(new byte[]{0,1,2}).ToArray()))
                },
            };

        foreach (var testCase in testCases)
        {
            var hash = PineValueHashTree.ComputeHash(testCase.input);

            Convert.ToHexStringLower(hash.Span).Should().BeEquivalentTo(
                testCase.expectedHashBase16,
                options => options.IgnoringCase());
        }
    }

    [Fact]
    public void String_value_roundtrips()
    {
        var testCases =
            new[]
            {
                "",
                "Hello World!",
                "Using some non-ASCII chars: 🌀 𝅘𝅥𝅰𝆁  𝅘𝅥𝅰 𝆁◌ 𝅘 𝅥 𝅰 𝆁◌ 😃 ⚠️☢️✔️",
            };

        foreach (var testCase in testCases)
        {
            var asPineValue =
                StringEncoding.ValueFromString(testCase);

            var toStringResult =
                StringEncoding.StringFromValue(asPineValue)
                .Extract(error => throw new Exception(error));

            toStringResult.Should().Be(testCase);
        }
    }

    [Fact]
    public void String_blob_encoding_writes_utf32_big_endian()
    {
        var encoded = StringEncoding.BlobValueFromString("A✔😃");

        encoded.Bytes.ToArray().Should().Equal(
            0x00,
            0x00,
            0x00,
            0x41,
            0x00,
            0x00,
            0x27,
            0x14,
            0x00,
            0x01,
            0xF6,
            0x03);
    }

    [Fact]
    public void String_blob_encoding_allocates_only_result_array_and_blob()
    {
        const string Input = "allocation-check-😃-2026";
        const int Iterations = 100;

        var encodedBytesCount = StringEncoding.ToCodePoints(Input).Count * 4;

        _ = StringEncoding.BlobValueFromString(Input);

        var baselineBytes = new byte[encodedBytesCount];
        baselineBytes[0] = 17;
        _ = PineValue.Blob(baselineBytes);

        var baselineAllocatedBefore = GC.GetAllocatedBytesForCurrentThread();

        PineValue? baseline = null;

        for (var i = 0; i < Iterations; ++i)
        {
            baselineBytes = new byte[encodedBytesCount];
            baselineBytes[0] = 17;
            baseline = PineValue.Blob(baselineBytes);
        }

        var baselineAllocatedBytes =
            GC.GetAllocatedBytesForCurrentThread() - baselineAllocatedBefore;

        var encodingAllocatedBefore = GC.GetAllocatedBytesForCurrentThread();

        PineValue.BlobValue? encoded = null;

        for (var i = 0; i < Iterations; ++i)
            encoded = StringEncoding.BlobValueFromString(Input);

        var encodingAllocatedBytes =
            GC.GetAllocatedBytesForCurrentThread() - encodingAllocatedBefore;

        GC.KeepAlive(baseline);
        GC.KeepAlive(encoded);

        encodingAllocatedBytes.Should().Be(baselineAllocatedBytes);
    }

    [Fact]
    public void String_blob_encoding_rejects_unpaired_surrogate()
    {
        var act = () => StringEncoding.BlobValueFromString("\ud800");

        act.Should().Throw<ArgumentException>();
    }

    [Fact]
    public void Signed_Integer_value_roundtrips()
    {
        var testCases =
            new[]
            {
                0,
                -1,
                1,
                -1234,
                2345,
                123456789
            };

        foreach (var testCase in testCases)
        {
            var asPineValue =
                IntegerEncoding.EncodeSignedInteger(testCase);

            var toIntegerResult =
                IntegerEncoding.ParseSignedIntegerStrict(asPineValue)
                .Extract(error => throw new Exception(error));

            toIntegerResult.Should().Be(testCase);
        }
    }

    [Fact]
    public void Unsigned_Integer_value_roundtrips()
    {
        var testCases =
            new[]
            {
                0,
                1,
                1234,
                2345,
                123456789
            };

        foreach (var testCase in testCases)
        {
            var asPineValue =
                IntegerEncoding.EncodeUnsignedInteger(testCase)
                .Extract(error => throw new Exception(error));

            var toIntegerResult =
                IntegerEncoding.ParseUnsignedInteger(asPineValue)
                .Extract(error => throw new Exception(error));

            toIntegerResult.Should().Be(testCase);
        }

        IntegerEncoding.EncodeUnsignedInteger(-1).Should().BeOfType<Result<string, PineValue>.Err>();
    }

    [Fact]
    public void Tree_with_string_path_sorting()
    {
        var testCases =
            new[]
            {
                new
                {
                    input =
                    FileTree.NonSortedDirectory(
                        treeContent:
                        [
                        ("ba-", FileTree.File(new byte[]{ 0 })),
                        ("ba", FileTree.File(new byte[] { 1 })),
                        ("bb", FileTree.File(new byte[] { 2 })),
                        ("a", FileTree.File(new byte[] { 3 })),
                        ("test😃", FileTree.File(new byte[] { 4 })),
                        ("testa", FileTree.File(new byte[] { 5 })),
                        ("tesz", FileTree.File(new byte[] { 6 })),
                        ("", FileTree.File(new byte[] { 7 })),
                        ("🌿", FileTree.File(new byte[] { 8 })),
                        ("🌲", FileTree.File(new byte[] { 9 })),
                        ("c",
                        FileTree.NonSortedDirectory(
                            treeContent:
                            ImmutableList.Create(
                                ("gamma", FileTree.File(new byte[] { 10 })),
                                ("alpha", FileTree.File(new byte[] { 11 }))))),
                        ("bA", FileTree.File(new byte[] { 12 }))
                        ]),
                    expected =
                    FileTree.NonSortedDirectory(
                        treeContent:
                        [
                        ("", FileTree.File(new byte[] { 7 })),
                        ("a", FileTree.File(new byte[] { 3 })),
                        ("bA", FileTree.File(new byte[] { 12 })),
                        ("ba", FileTree.File(new byte[] { 1 })),
                        ("ba-", FileTree.File(new byte[] { 0 })),
                        ("bb", FileTree.File(new byte[] { 2 })),
                        ("c",
                        FileTree.NonSortedDirectory(
                            treeContent:
                            [
                            ("alpha", FileTree.File(new byte[] { 11 })),
                            ("gamma", FileTree.File(new byte[] { 10 }))
                            ])),
                        ("testa", FileTree.File(new byte[] { 5 })),
                        ("test😃", FileTree.File(new byte[] { 4 })),
                        ("tesz", FileTree.File(new byte[] { 6 })),
                        ("🌲", FileTree.File(new byte[] { 9 })),
                        ("🌿", FileTree.File(new byte[] { 8 }))
                        ]),
                }
            };

        foreach (var testCase in testCases)
        {
            var sortedTree = FileTree.Sort(testCase.input);

            sortedTree.Should().Be(testCase.expected);
        }
    }
}
