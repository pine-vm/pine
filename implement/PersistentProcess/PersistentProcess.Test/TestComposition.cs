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
                    input = new Composition.TreeComponent{ Literal = new byte[]{0,1,2}.ToImmutableList()},
                    expectedOutput = new Composition.Component
                    {
                        Literal = new byte[]{0,1,2}.ToImmutableList()
                    }
                },
                new
                {
                    input = new Composition.TreeComponent
                    {
                        Composition = new []
                        {
                            (name: (IImmutableList<byte>)new byte[]{60,61,62}.ToImmutableList(),
                            component: new Composition.TreeComponent{ Literal = new byte[]{0,1,2,3}.ToImmutableList()} ),
                        }.ToImmutableList()
                    },
                    expectedOutput = new Composition.Component
                    {
                        Composition = new[]
                        {
                            new Composition.Component
                            {
                                Composition = new[]
                                {
                                    new Composition.Component{ Literal = new byte[]{60,61,62}.ToImmutableList() },
                                    new Composition.Component{ Literal = new byte[]{0,1,2,3}.ToImmutableList() }
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
                        Literal = new byte[]{0,1,2}.ToImmutableList()
                    },
                    expectedOutput = new Composition.ParseAsTreeResult
                    {
                        ok = new Composition.TreeComponent{ Literal = new byte[]{0,1,2}.ToImmutableList()}
                    }
                },
                new
                {
                    input = new Composition.Component
                    {
                        Composition = new[]
                        {
                            new Composition.Component
                            {
                                Composition = new[]
                                {
                                    new Composition.Component{ Literal = new byte[]{60,61,62}.ToImmutableList() },
                                    new Composition.Component{ Literal = new byte[]{0,1,2,3}.ToImmutableList() }
                                }.ToImmutableList()
                            }
                        }.ToImmutableList()
                    },
                    expectedOutput = new Composition.ParseAsTreeResult
                    {
                        ok = new Composition.TreeComponent
                        {
                            Composition = new []
                            {
                                (name: (IImmutableList<byte>)new byte[]{60,61,62}.ToImmutableList(),
                                component: new Composition.TreeComponent{ Literal = new byte[]{0,1,2,3}.ToImmutableList()} ),
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
        public void Hash_composition()
        {
            var testCases = new[]
            {
                new
                {
                    input = new Composition.Component
                    {
                        Literal = new byte[]{0,1,2}.ToImmutableList()
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
