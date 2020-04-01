using System.Collections.Immutable;
using System.Linq;
using System.Text;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace Kalmit.PersistentProcess.Test
{
    [TestClass]
    public class TestComposition
    {
        [TestMethod]
        public void Composition_from_tree()
        {
            var testCases = new[]
            {
                new
                {
                    input = new Composition.TreeComponent{ BlobContent = new byte[]{0,1,2}.ToImmutableList()},
                    expectedOutput = new Composition.Component
                    {
                        BlobContent = new byte[]{0,1,2}.ToImmutableList()
                    }
                },
                new
                {
                    input = new Composition.TreeComponent
                    {
                        TreeContent = new []
                        {
                            (name: (IImmutableList<byte>)new byte[]{60,61,62}.ToImmutableList(),
                            component: new Composition.TreeComponent{ BlobContent = new byte[]{0,1,2,3}.ToImmutableList()} ),
                        }.ToImmutableList()
                    },
                    expectedOutput = new Composition.Component
                    {
                        ListContent = new[]
                        {
                            new Composition.Component
                            {
                                ListContent = new[]
                                {
                                    new Composition.Component{ BlobContent = new byte[]{60,61,62}.ToImmutableList() },
                                    new Composition.Component{ BlobContent = new byte[]{0,1,2,3}.ToImmutableList() }
                                }.ToImmutableList()
                            }
                        }.ToImmutableList()
                    },
                },
            };

            foreach (var testCase in testCases)
            {
                var asComposition = Composition.FromTree(testCase.input);

                Assert.AreEqual(testCase.expectedOutput, asComposition);
            }
        }

        [TestMethod]
        public void Parse_as_tree()
        {
            var testCases = new[]
            {
                new
                {
                    input = new Composition.Component
                    {
                        BlobContent = new byte[]{0,1,2}.ToImmutableList()
                    },
                    expectedOutput = new Composition.ParseAsTreeResult
                    {
                        ok = new Composition.TreeComponent{ BlobContent = new byte[]{0,1,2}.ToImmutableList()}
                    }
                },
                new
                {
                    input = new Composition.Component
                    {
                        ListContent = new[]
                        {
                            new Composition.Component
                            {
                                ListContent = new[]
                                {
                                    new Composition.Component{ BlobContent = new byte[]{60,61,62}.ToImmutableList() },
                                    new Composition.Component{ BlobContent = new byte[]{0,1,2,3}.ToImmutableList() }
                                }.ToImmutableList()
                            }
                        }.ToImmutableList()
                    },
                    expectedOutput = new Composition.ParseAsTreeResult
                    {
                        ok = new Composition.TreeComponent
                        {
                            TreeContent = new []
                            {
                                (name: (IImmutableList<byte>)new byte[]{60,61,62}.ToImmutableList(),
                                component: new Composition.TreeComponent{ BlobContent = new byte[]{0,1,2,3}.ToImmutableList()} ),
                            }.ToImmutableList()
                        }
                    }
                },
            };

            foreach (var testCase in testCases)
            {
                var parseResult = Composition.ParseAsTree(testCase.input);

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
                    expectedOutput = new Composition.Component
                    {
                        ListContent = new []
                        {
                            new Composition.Component
                            {
                                ListContent = new []
                                {
                                    new Composition.Component{ BlobContent = new byte[]{97}.ToImmutableList()},
                                    new Composition.Component{ BlobContent = new byte[]{0,1,2}.ToImmutableList()},
                                }.ToImmutableList()
                            },
                            new Composition.Component
                            {
                                ListContent = new []
                                {
                                    new Composition.Component{ BlobContent = new byte[]{98}.ToImmutableList()},
                                    new Composition.Component
                                    {
                                        ListContent = new []
                                        {
                                            new Composition.Component
                                            {
                                                ListContent = new []
                                                {
                                                    new Composition.Component{ BlobContent = Encoding.UTF8.GetBytes("c").ToImmutableList()},
                                                    new Composition.Component{ BlobContent = new byte[]{3,4,5,6}.ToImmutableList()},
                                                }.ToImmutableList()
                                            },
                                            new Composition.Component
                                            {
                                                ListContent = new []
                                                {
                                                    new Composition.Component{ BlobContent = Encoding.UTF8.GetBytes("d").ToImmutableList()},
                                                    new Composition.Component{ BlobContent = new byte[]{7,8}.ToImmutableList()},
                                                }.ToImmutableList()
                                            },
                                        }.ToImmutableList()
                                    }
                                }.ToImmutableList()
                            },
                        }.ToImmutableList()
                    }
                },
            };

            foreach (var testCase in testCases)
            {
                var asComposition = Composition.FromTree(
                    Composition.TreeFromSetOfBlobsWithCommonFilePath(testCase.input));

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
                    input = new Composition.Component
                    {
                        BlobContent = new byte[]{0,1,2}.ToImmutableList()
                    },
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
    }
}
