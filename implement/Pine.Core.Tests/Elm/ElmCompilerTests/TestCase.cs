using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using System.Collections.Frozen;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Pine.Core.Tests.Elm.ElmCompilerTests;

public record TestCase(
    IReadOnlyList<string> ElmModulesTexts)
{
    public static TestCase DefaultAppWithoutPackages(
        IReadOnlyList<string> elmModulesTexts) =>
        new(ElmModulesTexts: elmModulesTexts);

    public BlobTreeWithStringPath AsFileTree() =>
        FileTreeFromElmModulesWithoutPackages(ElmModulesTexts);


    public static readonly string ElmJsonFileDefault =
        """
        {
            "type": "application",
            "source-directories": [
                "src"
            ],
            "elm-version": "0.19.1",
            "dependencies": {
                "direct": {
                    "elm/core": "1.0.5"
                },
                "indirect": {
                }
            },
            "test-dependencies": {
                "direct": {
                },
                "indirect": {
                }
            }
        }
        """;

    public static BlobTreeWithStringPath FileTreeFromElmModulesWithoutPackages(
        IReadOnlyList<string> elmModulesTexts)
    {
        var elmModulesByNameDict =
            elmModulesTexts
            .ToFrozenDictionary(
                moduleText =>
                Core.Elm.ElmSyntax.ElmModule.ParseModuleName(moduleText)
                .Extract(err => throw new System.Exception("Failed parsing module name from module text: " + err)),
                moduleText => moduleText,
                comparer: EnumerableExtensions.EqualityComparer<IReadOnlyList<string>>());

        var appCodeTree =
            BlobTreeWithStringPath.EmptyTree
            .SetNodeAtPathSorted(
                ["elm.json"],
                BlobTreeWithStringPath.Blob(Encoding.UTF8.GetBytes(ElmJsonFileDefault)));

        foreach (var (modulePath, moduleText) in elmModulesByNameDict)
        {
            var moduleName = modulePath[modulePath.Count - 1];

            IReadOnlyList<string> moduleFilePath =
                ["src", .. modulePath.SkipLast(1), moduleName + ".elm"];

            appCodeTree =
                appCodeTree.SetNodeAtPathSorted(
                    moduleFilePath,
                    BlobTreeWithStringPath.Blob(Encoding.UTF8.GetBytes(moduleText)));
        }

        return appCodeTree;
    }

    public static StaticProgram ParseAsStaticMonomorphicProgramAndCrashOnAnyFailure(
        ElmInteractiveEnvironment.ParsedInteractiveEnvironment parsedEnvironment,
        System.Func<DeclQualifiedName, bool> includeDeclaration,
        PineVMParseCache parseCache)
    {
        var (staticProgram, declsFailed) =
            Core.CodeAnalysis.CodeAnalysis.ParseAsStaticMonomorphicProgram(
                parsedEnvironment,
                includeDeclaration: includeDeclaration,
                parseCache: parseCache)
            .Extract(err => throw new System.Exception("Failed parsing as static program: " + err));

        foreach (var decl in declsFailed)
        {
            throw new System.Exception("Failed to parse declaration " + decl.Key.FullName + ": " + decl.Value);
        }

        staticProgram.Should().NotBeNull();

        return staticProgram;
    }
}
