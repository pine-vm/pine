using FluentAssertions;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using Pine.Core;
using Pine.Core.PopularEncodings;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

namespace TestElmTime;

[TestClass]
public class PineValueCompositionTests
{
    [TestMethod]
    public void Composition_from_tree_with_string_path()
    {
        var testCases = new[]
        {
            new
            {
                input = TreeNodeWithStringPath.Blob(new byte[]{0,1,2}),
                expectedOutput = PineValue.Blob([0,1,2])
            },
            new
            {
                input = TreeNodeWithStringPath.SortedTree(
                    [
                        (name: "ABC ä 😀",
                        component: TreeNodeWithStringPath.Blob(new byte[]{0,1,2,3}) ),
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

    [TestMethod]
    public void Parse_as_tree_with_string_path()
    {
        var testCases = new[]
        {
            new
            {
                input =
                PineValue.Blob([0,1,2]),

                expectedOutput = Result<IReadOnlyList<(int index, string name)>, TreeNodeWithStringPath>.ok(
                    TreeNodeWithStringPath.Blob(new byte[]{0,1,2}))
            },
            new
            {
                input =
                (PineValue)PineValue.List(
                    PineValue.List(
                        PineValue.Blob([0,0,0,68,0,0,0,69,0,0,0,70,0,0,0,32,0,1,243,50]),
                        PineValue.Blob([0,1,2,3]))),
                expectedOutput = Result<IReadOnlyList<(int index, string name)>, TreeNodeWithStringPath>.ok(
                    TreeNodeWithStringPath.SortedTree(
                        treeContent:
                        [
                            (name: "DEF 🌲",
                            component: TreeNodeWithStringPath.Blob(new byte[]{0,1,2,3}) ),
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

    [TestMethod]
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
                    elements:
                    [
                        PineValue.List(
                            elements:
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

    [TestMethod]
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

    [TestMethod]
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

    [TestMethod]
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

    [TestMethod]
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

    [TestMethod]
    public void Tree_with_string_path_sorting()
    {
        var testCases = new[]
        {
            new
            {
                input = TreeNodeWithStringPath.NonSortedTree(
                    treeContent:
                    [
                        ("ba-", TreeNodeWithStringPath.Blob(new byte[]{ 0 })),
                        ("ba", TreeNodeWithStringPath.Blob(new byte[] { 1 })),
                        ("bb", TreeNodeWithStringPath.Blob(new byte[] { 2 })),
                        ("a", TreeNodeWithStringPath.Blob(new byte[] { 3 })),
                        ("test😃", TreeNodeWithStringPath.Blob(new byte[] { 4 })),
                        ("testa", TreeNodeWithStringPath.Blob(new byte[] { 5 })),
                        ("tesz", TreeNodeWithStringPath.Blob(new byte[] { 6 })),
                        ("", TreeNodeWithStringPath.Blob(new byte[] { 7 })),
                        ("🌿", TreeNodeWithStringPath.Blob(new byte[] { 8 })),
                        ("🌲", TreeNodeWithStringPath.Blob(new byte[] { 9 })),
                        ("c", TreeNodeWithStringPath.NonSortedTree(
                            treeContent:
                            ImmutableList.Create(
                                ("gamma", TreeNodeWithStringPath.Blob(new byte[] { 10 })),
                                ("alpha", TreeNodeWithStringPath.Blob(new byte[] { 11 }))
                                )
                        )),
                        ("bA", TreeNodeWithStringPath.Blob(new byte[] { 12 }))
                        ]
                ),
                expected = TreeNodeWithStringPath.NonSortedTree(
                    treeContent:
                    [
                        ("", TreeNodeWithStringPath.Blob(new byte[] { 7 })),
                        ("a", TreeNodeWithStringPath.Blob(new byte[] { 3 })),
                        ("bA", TreeNodeWithStringPath.Blob(new byte[] { 12 })),
                        ("ba", TreeNodeWithStringPath.Blob(new byte[] { 1 })),
                        ("ba-", TreeNodeWithStringPath.Blob(new byte[] { 0 })),
                        ("bb", TreeNodeWithStringPath.Blob(new byte[] { 2 })),
                        ("c", TreeNodeWithStringPath.NonSortedTree(
                            treeContent:
                            [
                                ("alpha", TreeNodeWithStringPath.Blob(new byte[] { 11 })),
                                ("gamma", TreeNodeWithStringPath.Blob(new byte[] { 10 }))
                                ]
                        )),
                        ("testa", TreeNodeWithStringPath.Blob(new byte[] { 5 })),
                        ("test😃", TreeNodeWithStringPath.Blob(new byte[] { 4 })),
                        ("tesz", TreeNodeWithStringPath.Blob(new byte[] { 6 })),
                        ("🌲", TreeNodeWithStringPath.Blob(new byte[] { 9 })),
                        ("🌿", TreeNodeWithStringPath.Blob(new byte[] { 8 }))
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
