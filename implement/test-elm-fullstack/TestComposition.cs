using System;
using System.Collections.Immutable;
using System.Linq;
using System.Text;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using Pine;

namespace test_elm_fullstack;

[TestClass]
public class TestComposition
{
    [TestMethod]
    public void Composition_from_tree_with_string_path()
    {
        var testCases = new[]
        {
            new
            {
                input = TreeNodeWithStringPath.Blob(new byte[]{0,1,2}),
                expectedOutput = PineValue.Blob(new byte[]{0,1,2})
            },
            new
            {
                input = TreeNodeWithStringPath.SortedTree(
                    new []
                    {
                        (name: "ABC ä 😀",
                        component: TreeNodeWithStringPath.Blob(new byte[]{0,1,2,3}) ),
                    }.ToImmutableList()),
                expectedOutput = PineValue.List(
                    new[]
                    {
                        PineValue.List(
                            new[]
                            {
                                PineValue.List(
                                    new[]
                                    {
                                        PineValue.Blob(new byte[]{ 65 }),
                                        PineValue.Blob(new byte[]{ 66 }),
                                        PineValue.Blob(new byte[]{ 67 }),
                                        PineValue.Blob(new byte[]{ 32 }),
                                        PineValue.Blob(new byte[]{ 228 }),
                                        PineValue.Blob(new byte[]{ 32 }),
                                        PineValue.Blob(new byte[]{ 1,246,0 }),
                                    }.ToImmutableList()
                                ),
                                PineValue.Blob(new byte[]{0,1,2,3 } )
                            }.ToImmutableList()
                        )
                    }.ToImmutableList())
                },
            };

        foreach (var testCase in testCases)
        {
            var asComposition = Composition.FromTreeWithStringPath(testCase.input);

            Assert.AreEqual(testCase.expectedOutput, asComposition);
        }
    }

    [TestMethod]
    public void Parse_as_tree_with_string_path()
    {
        var testCases = new[]
        {
            new
            {
                input = PineValue.Blob(new byte[]{0,1,2}),
                expectedOutput = Result<IImmutableList<(int index, string name)>, TreeNodeWithStringPath>.ok(
                    TreeNodeWithStringPath.Blob(new byte[]{0,1,2}))
            },
            new
            {
                input = PineValue.List(
                    elements:
                    new[]
                    {
                        PineValue.List(
                            elements:
                            new[]
                            {
                                PineValue.List(
                                    elements:
                                    new []
                                    {
                                        PineValue.Blob(new byte[] { 68 }),
                                        PineValue.Blob(new byte[] { 69 }),
                                        PineValue.Blob(new byte[] { 70 }),
                                        PineValue.Blob(new byte[] { 32 }),
                                        PineValue.Blob(new byte[]{ 1,243,50 }),
                                    }.ToImmutableList()),
                                PineValue.Blob(new byte[]{0,1,2,3}),
                            }.ToImmutableList())
                    }.ToImmutableList()),
                expectedOutput = Result<IImmutableList<(int index, string name)>, TreeNodeWithStringPath>.ok(
                    TreeNodeWithStringPath.SortedTree(
                        treeContent:
                        new []
                        {
                            (name: "DEF 🌲",
                            component: TreeNodeWithStringPath.Blob(new byte[]{0,1,2,3}) ),
                        }.ToImmutableList())
                )
            },
        };

        foreach (var testCase in testCases)
        {
            var parseResult = Composition.ParseAsTreeWithStringPath(testCase.input);

            Assert.AreEqual(testCase.expectedOutput, parseResult);
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
                    new []
                    {
                        PineValue.List(
                            elements:
                            new []
                            {
                                PineValue.List(
                                    elements:
                                    new []
                                    {
                                        PineValue.Blob(new byte[] { 97 }),
                                    }.ToImmutableList()),
                                PineValue.Blob(new byte[]{0,1,2}),
                            }.ToImmutableList()),
                        PineValue.List(
                            elements:
                            new []
                            {
                                PineValue.List(
                                    elements:
                                    new []
                                    {
                                        PineValue.Blob(new byte[] { 98 }),
                                    }.ToImmutableList()
                                ),
                                PineValue.List(
                                    elements:
                                    new []
                                    {
                                        PineValue.List(
                                            elements:
                                            new []
                                            {
                                                PineValue.List(
                                                    elements:
                                                    new []
                                                    {
                                                        PineValue.Blob(new byte[] { 99 }),
                                                    }.ToImmutableList()
                                                ),
                                                PineValue.Blob(new byte[]{3,4,5,6}),
                                            }.ToImmutableList()
                                        ),
                                        PineValue.List(
                                            elements:
                                            new []
                                            {
                                                PineValue.List(
                                                    elements:
                                                    new []
                                                    {
                                                        PineValue.Blob(new byte[] { 100 }),
                                                    }.ToImmutableList()
                                                ),
                                                PineValue.Blob(new byte[]{7,8}),
                                            }.ToImmutableList()
                                        ),
                                    }.ToImmutableList()
                                )
                            }.ToImmutableList()
                        ),
                    }.ToImmutableList())
            },
        };

        foreach (var testCase in testCases)
        {
            var asComposition = Composition.FromTreeWithStringPath(
                Composition.SortedTreeFromSetOfBlobsWithCommonFilePath(testCase.input));

            Assert.AreEqual(testCase.expectedOutput, asComposition);
        }
    }

    [TestMethod]
    public void Hash_composition()
    {
        var testCases = new[]
        {
            new
            {
                input = PineValue.Blob(new byte[]{0,1,2}),
                expectedHashBase16 = CommonConversion.StringBase16(
                    CommonConversion.HashSHA256(Encoding.ASCII.GetBytes("blob 3\0").Concat(new byte[]{0,1,2}).ToArray()))
            },
        };

        foreach (var testCase in testCases)
        {
            var hash = Composition.GetHash(testCase.input);

            Assert.AreEqual(testCase.expectedHashBase16, CommonConversion.StringBase16(hash), ignoreCase: true);
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
                Composition.ComponentFromString(testCase);

            var toStringResult =
                Composition.StringFromComponent(asPineValue)
                .Extract(error => throw new Exception(error));

            Assert.AreEqual(testCase, toStringResult);
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
                Composition.ComponentFromSignedInteger(testCase);

            var toIntegerResult =
                Composition.SignedIntegerFromComponent(asPineValue)
                .Extract(error => throw new Exception(error));

            Assert.AreEqual(testCase, toIntegerResult);
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
                Composition.ComponentFromUnsignedInteger(testCase)
                .Extract(error => throw new Exception(error));

            var toIntegerResult =
                Composition.UnsignedIntegerFromComponent(asPineValue)
                .Extract(error => throw new Exception(error));

            Assert.AreEqual(testCase, toIntegerResult);
        }

        Assert.IsTrue(Composition.ComponentFromUnsignedInteger(-1) is Result<string, PineValue>.Err);
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
                    ImmutableList.Create(
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
                        )
                ),
                expected = TreeNodeWithStringPath.NonSortedTree(
                    treeContent:
                    ImmutableList.Create(
                        ("", TreeNodeWithStringPath.Blob(new byte[] { 7 })),
                        ("a", TreeNodeWithStringPath.Blob(new byte[] { 3 })),
                        ("bA", TreeNodeWithStringPath.Blob(new byte[] { 12 })),
                        ("ba", TreeNodeWithStringPath.Blob(new byte[] { 1 })),
                        ("ba-", TreeNodeWithStringPath.Blob(new byte[] { 0 })),
                        ("bb", TreeNodeWithStringPath.Blob(new byte[] { 2 })),
                        ("c", TreeNodeWithStringPath.NonSortedTree(
                            treeContent:
                            ImmutableList.Create(
                                ("alpha", TreeNodeWithStringPath.Blob(new byte[] { 11 })),
                                ("gamma", TreeNodeWithStringPath.Blob(new byte[] { 10 }))
                                )
                        )),
                        ("testa", TreeNodeWithStringPath.Blob(new byte[] { 5 })),
                        ("test😃", TreeNodeWithStringPath.Blob(new byte[] { 4 })),
                        ("tesz", TreeNodeWithStringPath.Blob(new byte[] { 6 })),
                        ("🌲", TreeNodeWithStringPath.Blob(new byte[] { 9 })),
                        ("🌿", TreeNodeWithStringPath.Blob(new byte[] { 8 }))
                        )
                ),
            }
        };

        foreach (var testCase in testCases)
        {
            var sortedTree = Composition.SortedTreeFromTree(testCase.input);

            Assert.AreEqual(expected: testCase.expected, sortedTree);
        }
    }
}
