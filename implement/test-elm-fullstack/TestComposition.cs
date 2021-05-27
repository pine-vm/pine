using System.Collections.Immutable;
using System.Linq;
using System.Text;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using Pine;

namespace test_elm_fullstack
{
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
                    input = Composition.TreeWithStringPath.Blob(new byte[]{0,1,2}),
                    expectedOutput = Composition.Component.Blob(new byte[]{0,1,2})
                },
                new
                {
                    input = Composition.TreeWithStringPath.Tree(
                        new []
                        {
                            (name: "ABC ä 😀",
                            component: Composition.TreeWithStringPath.Blob(new byte[]{0,1,2,3}) ),
                        }.ToImmutableList()),
                    expectedOutput = Composition.Component.List(
                        new[]
                        {
                            Composition.Component.List(
                                new[]
                                {
                                    Composition.Component.List(
                                        new[]
                                        {
                                            Composition.Component.Blob(ImmutableList.Create<byte>(65)),
                                            Composition.Component.Blob(ImmutableList.Create<byte>(66)),
                                            Composition.Component.Blob(ImmutableList.Create<byte>(67)),
                                            Composition.Component.Blob(ImmutableList.Create<byte>(32)),
                                            Composition.Component.Blob(ImmutableList.Create<byte>(228)),
                                            Composition.Component.Blob(ImmutableList.Create<byte>(32)),
                                            Composition.Component.Blob(ImmutableList.Create<byte>(1,246,0)),
                                        }.ToImmutableList()
                                    ),
                                    Composition.Component.Blob(new byte[]{0,1,2,3 } )
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
                    input = Composition.Component.Blob(new byte[]{0,1,2}),
                    expectedOutput = new Composition.ParseAsTreeWithStringPathResult
                    {
                        Ok = Composition.TreeWithStringPath.Blob(new byte[]{0,1,2})
                    }
                },
                new
                {
                    input = Composition.Component.List(
                        listContent:
                        new[]
                        {
                            Composition.Component.List(
                                listContent:
                                new[]
                                {
                                    Composition.Component.List(
                                        listContent:
                                        new []
                                        {
                                            Composition.Component.Blob(ImmutableList.Create<byte>(68)),
                                            Composition.Component.Blob(ImmutableList.Create<byte>(69)),
                                            Composition.Component.Blob(ImmutableList.Create<byte>(70)),
                                            Composition.Component.Blob(ImmutableList.Create<byte>(32)),
                                            Composition.Component.Blob(ImmutableList.Create<byte>(1,243,50)),
                                        }.ToImmutableList()),
                                    Composition.Component.Blob(new byte[]{0,1,2,3}),
                                }.ToImmutableList())
                        }.ToImmutableList()),
                    expectedOutput = new Composition.ParseAsTreeWithStringPathResult
                    {
                        Ok = Composition.TreeWithStringPath.Tree(
                            treeContent:
                            new []
                            {
                                (name: "DEF 🌲",
                                component: Composition.TreeWithStringPath.Blob(new byte[]{0,1,2,3}) ),
                            }.ToImmutableList())
                    }
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
                        (filePath: "a", fileContent: new byte[]{0,1,2}),
                        (filePath: "b/c", fileContent: new byte[]{3,4,5,6}),
                        (filePath: "b/d", fileContent: new byte[]{7,8}),
                    },
                    expectedOutput = Composition.Component.List(
                        listContent:
                        new []
                        {
                            Composition.Component.List(
                                listContent:
                                new []
                                {
                                    Composition.Component.List(
                                        listContent:
                                        new []
                                        {
                                            Composition.Component.Blob(ImmutableList.Create<byte>(97)),
                                        }.ToImmutableList()),
                                    Composition.Component.Blob(new byte[]{0,1,2}),
                                }.ToImmutableList()),
                            Composition.Component.List(
                                listContent:
                                new []
                                {
                                    Composition.Component.List(
                                        listContent:
                                        new []
                                        {
                                            Composition.Component.Blob(ImmutableList.Create<byte>(98)),
                                        }.ToImmutableList()
                                    ),
                                    Composition.Component.List(
                                        listContent:
                                        new []
                                        {
                                            Composition.Component.List(
                                                listContent:
                                                new []
                                                {
                                                    Composition.Component.List(
                                                        listContent:
                                                        new []
                                                        {
                                                            Composition.Component.Blob(ImmutableList.Create<byte>(99)),
                                                        }.ToImmutableList()
                                                    ),
                                                    Composition.Component.Blob(new byte[]{3,4,5,6}),
                                                }.ToImmutableList()
                                            ),
                                            Composition.Component.List(
                                                listContent:
                                                new []
                                                {
                                                    Composition.Component.List(
                                                        listContent:
                                                        new []
                                                        {
                                                            Composition.Component.Blob(ImmutableList.Create<byte>(100)),
                                                        }.ToImmutableList()
                                                    ),
                                                    Composition.Component.Blob(new byte[]{7,8}),
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
                    input = Composition.Component.Blob(new byte[]{0,1,2}),
                    expectedHashBase16 = CommonConversion.StringBase16FromByteArray(
                        CommonConversion.HashSHA256(Encoding.ASCII.GetBytes("blob 3\0").Concat(new byte[]{0,1,2}).ToArray()))
                },
            };

            foreach (var testCase in testCases)
            {
                var hash = Composition.GetHash(testCase.input);

                Assert.AreEqual(testCase.expectedHashBase16, CommonConversion.StringBase16FromByteArray(hash), ignoreCase: true);
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
                    Composition.StringFromComponent(asPineValue);

                Assert.IsNull(toStringResult.Err);

                Assert.AreEqual(testCase, toStringResult.Ok);
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
                    Composition.SignedIntegerFromComponent(asPineValue);

                Assert.IsNull(toIntegerResult.Err);

                Assert.AreEqual(testCase, toIntegerResult.Ok);
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
                    Composition.ComponentFromUnsignedInteger(testCase);

                Assert.IsNull(asPineValue.Err);

                var toIntegerResult =
                    Composition.UnsignedIntegerFromComponent(asPineValue.Ok);

                Assert.IsNull(toIntegerResult.Err);

                Assert.AreEqual(testCase, toIntegerResult.Ok);
            }

            Assert.IsNotNull(Composition.ComponentFromUnsignedInteger(-1).Err);
        }

        [TestMethod]
        public void Tree_with_string_path_sorting()
        {
            var testCases = new[]
            {
                new
                {
                    input = Composition.TreeWithStringPath.Tree(
                        treeContent:
                        ImmutableList.Create(
                            ("ba-", Composition.TreeWithStringPath.Blob(ImmutableList.Create<byte>(0))),
                            ("ba", Composition.TreeWithStringPath.Blob(ImmutableList.Create<byte>(1))),
                            ("bb", Composition.TreeWithStringPath.Blob(ImmutableList.Create<byte>(2))),
                            ("a", Composition.TreeWithStringPath.Blob(ImmutableList.Create<byte>(3))),
                            ("test😃", Composition.TreeWithStringPath.Blob(ImmutableList.Create<byte>(4))),
                            ("testa", Composition.TreeWithStringPath.Blob(ImmutableList.Create<byte>(5))),
                            ("tesz", Composition.TreeWithStringPath.Blob(ImmutableList.Create<byte>(6))),
                            ("", Composition.TreeWithStringPath.Blob(ImmutableList.Create<byte>(7))),
                            ("🌿", Composition.TreeWithStringPath.Blob(ImmutableList.Create<byte>(8))),
                            ("🌲", Composition.TreeWithStringPath.Blob(ImmutableList.Create<byte>(9))),
                            ("c", Composition.TreeWithStringPath.Tree(
                                treeContent:
                                ImmutableList.Create(
                                    ("gamma", Composition.TreeWithStringPath.Blob(ImmutableList.Create<byte>(10))),
                                    ("alpha", Composition.TreeWithStringPath.Blob(ImmutableList.Create<byte>(11)))
                                    )
                            )),
                            ("bA", Composition.TreeWithStringPath.Blob(ImmutableList.Create<byte>(12)))
                            )
                    ),
                    expected = Composition.TreeWithStringPath.Tree(
                        treeContent:
                        ImmutableList.Create(
                            ("", Composition.TreeWithStringPath.Blob(ImmutableList.Create<byte>(7))),
                            ("a", Composition.TreeWithStringPath.Blob(ImmutableList.Create<byte>(3))),
                            ("bA", Composition.TreeWithStringPath.Blob(ImmutableList.Create<byte>(12))),
                            ("ba", Composition.TreeWithStringPath.Blob(ImmutableList.Create<byte>(1))),
                            ("ba-", Composition.TreeWithStringPath.Blob(ImmutableList.Create<byte>(0))),
                            ("bb", Composition.TreeWithStringPath.Blob(ImmutableList.Create<byte>(2))),
                            ("c", Composition.TreeWithStringPath.Tree(
                                treeContent:
                                ImmutableList.Create(
                                    ("alpha", Composition.TreeWithStringPath.Blob(ImmutableList.Create<byte>(11))),
                                    ("gamma", Composition.TreeWithStringPath.Blob(ImmutableList.Create<byte>(10)))
                                    )
                            )),
                            ("testa", Composition.TreeWithStringPath.Blob(ImmutableList.Create<byte>(5))),
                            ("test😃", Composition.TreeWithStringPath.Blob(ImmutableList.Create<byte>(4))),
                            ("tesz", Composition.TreeWithStringPath.Blob(ImmutableList.Create<byte>(6))),
                            ("🌲", Composition.TreeWithStringPath.Blob(ImmutableList.Create<byte>(9))),
                            ("🌿", Composition.TreeWithStringPath.Blob(ImmutableList.Create<byte>(8)))
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
}
