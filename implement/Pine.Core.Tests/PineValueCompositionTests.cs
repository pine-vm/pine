using AwesomeAssertions;
using Pine.Core.Addressing;
using Pine.Core.PopularEncodings;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using Xunit;

namespace Pine.Core.Tests;

public class PineValueCompositionTests
{
    [Fact]
    public void Composition_from_tree_with_string_path()
    {
        var testCases = new[]
        {
            new
            {
                input = BlobTreeWithStringPath.Blob(new byte[]{0,1,2}),
                expectedOutput = PineValue.Blob([0,1,2])
            },
            new
            {
                input = BlobTreeWithStringPath.SortedTree(
                    [
                        (name: "ABC ä 😀",
                        component: BlobTreeWithStringPath.Blob(new byte[]{0,1,2,3}) ),
                    ]),
                expectedOutput = (PineValue)PineValue.List(
                    [
                        PineValue.List(
                            PineValue.Blob([0,0,0,65,0,0,0,66,0,0,0,67,0,0,0,32,0,0,0,228,0,0,0,32,0,1,246,0]),
                            PineValue.Blob([0,1,2,3])
                        )
                    ])
                },
            };

        foreach (var testCase in testCases)
        {
            var asComposition = PineValueComposition.FromTreeWithStringPath(testCase.input);

            asComposition.Should().Be(testCase.expectedOutput);
        }
    }

    [Fact]
    public void Parse_as_tree_with_string_path()
    {
        var testCases = new[]
        {
            new
            {
                input =
                PineValue.Blob([0,1,2]),

                expectedOutput = Result<IReadOnlyList<(int index, string name)>, BlobTreeWithStringPath>.ok(
                    BlobTreeWithStringPath.Blob(new byte[]{0,1,2}))
            },
            new
            {
                input =
                (PineValue)PineValue.List(
                    PineValue.List(
                        PineValue.Blob([0,0,0,68,0,0,0,69,0,0,0,70,0,0,0,32,0,1,243,50]),
                        PineValue.Blob([0,1,2,3]))),
                expectedOutput = Result<IReadOnlyList<(int index, string name)>, BlobTreeWithStringPath>.ok(
                    BlobTreeWithStringPath.SortedTree(
                        treeContent:
                        [
                            (name: "DEF 🌲",
                            component: BlobTreeWithStringPath.Blob(new byte[]{0,1,2,3}) ),
                        ])
                )
            },
        };

        foreach (var testCase in testCases)
        {
            var parseResult = PineValueComposition.ParseAsTreeWithStringPath(testCase.input);

            parseResult.Should().Be(testCase.expectedOutput);
        }
    }

    [Fact]
    public void Composition_from_file_tree()
    {
        var testCases = new[]
        {
            new
            {
                input = new []
                {
                    (filePath: "a", fileContent: (ReadOnlyMemory<byte>)new byte[]{0,1,2}),
                    (filePath: "b/c", fileContent: new byte[]{3,4,5,6}),
                    (filePath: "b/d", fileContent: new byte[]{7,8}),
                },
                expectedOutput = PineValue.List(
                    items:
                    [
                        PineValue.List(
                            items:
                            [
                                PineValue.Blob([0,0,0,97]),
                                PineValue.Blob([0,1,2]),
                            ]),
                        PineValue.List(
                            PineValue.Blob([0,0,0,98]),
                            PineValue.List(
                                PineValue.List(
                                    PineValue.Blob([0,0,0,99]),
                                    PineValue.Blob([3,4,5,6])),

                                PineValue.List(
                                    PineValue.Blob([0,0,0,100]),
                                    PineValue.Blob([7,8]))
                                )
                        ),
                    ])
            },
        };

        foreach (var testCase in testCases)
        {
            var asComposition = PineValueComposition.FromTreeWithStringPath(
                PineValueComposition.SortedTreeFromSetOfBlobsWithCommonFilePath(testCase.input));

            asComposition.Should().Be(testCase.expectedOutput);
        }
    }

    [Fact]
    public void Hash_composition()
    {
        var testCases = new[]
        {
            new
            {
                input = PineValue.Blob([0,1,2]),
                expectedHashBase16 =
                Convert.ToHexStringLower(
                    System.Security.Cryptography.SHA256.HashData("blob 3\0"u8.ToArray().Concat(new byte[]{0,1,2}).ToArray()))
            },
        };

        foreach (var testCase in testCases)
        {
            var hash = PineValueHashTree.ComputeHash(testCase.input);

            Convert.ToHexStringLower(hash.Span).Should().BeEquivalentTo(testCase.expectedHashBase16, options => options.IgnoringCase());
        }
    }

    [Fact]
    public void String_value_roundtrips()
    {
        var testCases = new[]
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
    public void Signed_Integer_value_roundtrips()
    {
        var testCases = new[]
        {
            0,-1,1,-1234,2345,123456789
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
        var testCases = new[]
        {
            0,1,1234,2345,123456789
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
        var testCases = new[]
        {
            new
            {
                input = BlobTreeWithStringPath.NonSortedTree(
                    treeContent:
                    [
                        ("ba-", BlobTreeWithStringPath.Blob(new byte[]{ 0 })),
                        ("ba", BlobTreeWithStringPath.Blob(new byte[] { 1 })),
                        ("bb", BlobTreeWithStringPath.Blob(new byte[] { 2 })),
                        ("a", BlobTreeWithStringPath.Blob(new byte[] { 3 })),
                        ("test😃", BlobTreeWithStringPath.Blob(new byte[] { 4 })),
                        ("testa", BlobTreeWithStringPath.Blob(new byte[] { 5 })),
                        ("tesz", BlobTreeWithStringPath.Blob(new byte[] { 6 })),
                        ("", BlobTreeWithStringPath.Blob(new byte[] { 7 })),
                        ("🌿", BlobTreeWithStringPath.Blob(new byte[] { 8 })),
                        ("🌲", BlobTreeWithStringPath.Blob(new byte[] { 9 })),
                        ("c", BlobTreeWithStringPath.NonSortedTree(
                            treeContent:
                            ImmutableList.Create(
                                ("gamma", BlobTreeWithStringPath.Blob(new byte[] { 10 })),
                                ("alpha", BlobTreeWithStringPath.Blob(new byte[] { 11 }))
                                )
                        )),
                        ("bA", BlobTreeWithStringPath.Blob(new byte[] { 12 }))
                        ]
                ),
                expected = BlobTreeWithStringPath.NonSortedTree(
                    treeContent:
                    [
                        ("", BlobTreeWithStringPath.Blob(new byte[] { 7 })),
                        ("a", BlobTreeWithStringPath.Blob(new byte[] { 3 })),
                        ("bA", BlobTreeWithStringPath.Blob(new byte[] { 12 })),
                        ("ba", BlobTreeWithStringPath.Blob(new byte[] { 1 })),
                        ("ba-", BlobTreeWithStringPath.Blob(new byte[] { 0 })),
                        ("bb", BlobTreeWithStringPath.Blob(new byte[] { 2 })),
                        ("c", BlobTreeWithStringPath.NonSortedTree(
                            treeContent:
                            [
                                ("alpha", BlobTreeWithStringPath.Blob(new byte[] { 11 })),
                                ("gamma", BlobTreeWithStringPath.Blob(new byte[] { 10 }))
                                ]
                        )),
                        ("testa", BlobTreeWithStringPath.Blob(new byte[] { 5 })),
                        ("test😃", BlobTreeWithStringPath.Blob(new byte[] { 4 })),
                        ("tesz", BlobTreeWithStringPath.Blob(new byte[] { 6 })),
                        ("🌲", BlobTreeWithStringPath.Blob(new byte[] { 9 })),
                        ("🌿", BlobTreeWithStringPath.Blob(new byte[] { 8 }))
                        ]
                ),
            }
        };

        foreach (var testCase in testCases)
        {
            var sortedTree = PineValueComposition.SortedTreeFromTree(testCase.input);

            sortedTree.Should().Be(testCase.expected);
        }
    }
}
