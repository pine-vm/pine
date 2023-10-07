using Microsoft.VisualStudio.TestTools.UnitTesting;
using Pine;
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
                ElmTime.ElmSyntax.ElmModule.ParseModuleImportedModulesNames(testCase.moduleText)
                .ToImmutableHashSet(EnumerableExtension.EqualityComparer<IReadOnlyList<string>>());

            Assert.IsTrue(
                actualImports.SequenceEqual(
                    testCase.expectedImports,
                    EnumerableExtension.EqualityComparer<IReadOnlyList<string>>()));
        }
    }
}
