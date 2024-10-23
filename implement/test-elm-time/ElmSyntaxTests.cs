using ElmTime.ElmSyntax;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using Pine.Core;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

namespace TestElmTime;

[TestClass]
public class ElmSyntaxTests
{
    [TestMethod]
    public void Parse_Elm_module_text_imports()
    {
        var testCases = new[]
        {
            new
            {
                moduleText = @"module TestModule exposing (..)",
                expectedImports = System.Array.Empty<IReadOnlyList<string>>()
            },
            new
            {
                moduleText = """
                module TestModule exposing (..)

                import Basics exposing (Bool, Int)
                import Dict
                import List exposing ((::))
                import Maybe exposing (Maybe(..))
                """,
                expectedImports = new IReadOnlyList<string>[]
                {
                    ["Basics"],
                    ["Dict"],
                    ["List"],
                    ["Maybe"]
                }
            },
        };

        foreach (var testCase in testCases)
        {
            var actualImports =
                ElmModule.ParseModuleImportedModulesNames(testCase.moduleText)
                .ToImmutableHashSet(EnumerableExtension.EqualityComparer<IReadOnlyList<string>>());

            Assert.IsTrue(
                actualImports.SequenceEqual(
                    testCase.expectedImports,
                    EnumerableExtension.EqualityComparer<IReadOnlyList<string>>()));
        }
    }

    [TestMethod]
    public void Enumerate_module_lines()
    {
        var testCases = new[]
            {
                new
                {
                    moduleText =
                    "",

                    expectedLines =
                    (IReadOnlyList<string>)[""]
                },

                new
                {
                    moduleText =
                    "focaccia\narancino",

                    expectedLines =
                    (IReadOnlyList<string>)["focaccia","arancino"]
                },

                new
                {
                    moduleText =
                    "focaccia\narancino\n",

                    expectedLines =
                    (IReadOnlyList<string>)["focaccia","arancino",""]
                },

                new
                {
                    moduleText =
                    "focaccia\rarancino",

                    expectedLines =
                    (IReadOnlyList<string>)["focaccia","arancino"]
                },

                new
                {
                    moduleText =
                    "focaccia\r\narancino",

                    expectedLines =
                    (IReadOnlyList<string>)["focaccia","arancino"]
                },

                new
                {
                    moduleText =
                    "focaccia\n\rarancino",

                    expectedLines =
                    (IReadOnlyList<string>)["focaccia","arancino"]
                },

                new
                {
                    moduleText =
                    "focaccia\n\narancino",

                    expectedLines =
                    (IReadOnlyList<string>)["focaccia","","arancino"]
                },

                new
                {
                    moduleText =
                    "focaccia\n\r\narancino",

                    expectedLines =
                    (IReadOnlyList<string>)["focaccia","","arancino"]
                },

                new
                {
                    moduleText =
                    "focaccia\r\n\rarancino",

                    expectedLines =
                    (IReadOnlyList<string>)["focaccia","","arancino"]
                },
            };

        foreach (var testCase in testCases)
        {
            IReadOnlyList<string> lines =
                [.. ElmModule.ModuleLines(moduleText: testCase.moduleText)];

            try
            {
                Assert.AreEqual(
                    testCase.expectedLines.Count,
                    lines.Count,
                    "Number of lines");

                Assert.IsTrue(
                    lines.SequenceEqual(testCase.expectedLines),
                    message: "Lines contents");
            }
            catch (System.Exception ex)
            {
                throw new System.Exception(
                    "Failed for test case:\n" + testCase.moduleText,
                    ex);
            }
        }
    }
}
