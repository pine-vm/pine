using ElmTime.ElmInteractive;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using Pine;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Text;

namespace TestElmTime;

[TestClass]
public class CompileElmCompilerTests
{
    static IReadOnlyList<string> CompilerPackageSources =>
        [
            "https://github.com/elm-community/maybe-extra/tree/2a9e2c6143dcee04180b265c840b6052b0a053c2/",
            "https://github.com/elm-community/list-extra/tree/5a083cf0400260537adef75f96fbd48bfcedc7c0/",
            "https://github.com/Viir/result-extra/tree/e3b2e4358ac701d66e75ccbfdc4256513dc70694",
            "https://github.com/Viir/elm-bigint/tree/d452b489c5795f8deed19658a7b8f7bf5ef1e9a4/"
        ];

    [Ignore("Productive side not ready yet")]
    [TestMethod]
    public void Test_compile_elm_compiler()
    {
        var compilerProgram = IInteractiveSession.CompileElmProgramCodeFilesDefault.Value;

        using var compileJavaScriptEngine =
            ElmInteractive.PrepareJavaScriptEngineToEvaluateElm(
                compileElmProgramCodeFiles: compilerProgram,
                InteractiveSessionJavaScript.JavaScriptEngineFlavor.V8);

        var compilerPackageSourcesTrees =
            CompilerPackageSources
            .Select(LoadFromGitHubOrGitLab.LoadFromUrl)
            .ListCombine()
            .Extract(err => throw new System.Exception(err));

        var compilerPackageSourcesFiles =
            compilerPackageSourcesTrees
            .SelectMany(tree => tree.tree.EnumerateBlobsTransitive())
            .Where(blobAtPath =>
            blobAtPath.path.First() == "src" && blobAtPath.path.Last().ToLower().EndsWith(".elm"));

        var compilerAppCodeSourceFiles =
            compilerProgram.EnumerateBlobsTransitive()
            .Where(blobAtPath => blobAtPath.path.Last().ToLower().EndsWith(".elm"))
            .ToImmutableArray();

        var elmCoreLibraryModulesTexts =
            ElmInteractive.GetDefaultElmCoreModulesTexts(compileJavaScriptEngine);

        var elmModulesTexts = elmCoreLibraryModulesTexts;

        var compilerProgramOnlyElmJson =
            TreeNodeWithStringPath.FilterNodes(
                compilerProgram,
                nodePath => nodePath.SequenceEqual(["elm.json"]));

        var allAvailableElmFiles =
            compilerAppCodeSourceFiles
            .Concat(compilerPackageSourcesFiles)
            .Select(blobAtPath => (blobAtPath, moduleText: Encoding.UTF8.GetString(blobAtPath.blobContent.Span)))
            .ToImmutableArray();

        var rootElmFile =
            allAvailableElmFiles
            .First(c => c.blobAtPath.path.SequenceEqual(["src", "ElmInteractive.elm"]));

        var elmModulesTextsForInteractiveCompiler =
            ElmTime.ElmSyntax.ElmModule.ModulesTextOrderedForCompilationByDependencies(
                rootModulesTexts: [rootElmFile.moduleText],
                availableModulesTexts: [.. allAvailableElmFiles.Select(f => f.moduleText)]);

        var elmModulesForInteractiveCompiler =
            elmModulesTextsForInteractiveCompiler
            .Select(moduleText => allAvailableElmFiles.First(c => c.moduleText == moduleText).blobAtPath)
            .ToImmutableArray();

        var compilerWithPackagesTree =
            elmModulesForInteractiveCompiler
            .Aggregate(
                seed: compilerProgramOnlyElmJson,
                func: (aggregate, elmModule) =>
                aggregate.SetNodeAtPathSorted(elmModule.path, TreeNodeWithStringPath.Blob(elmModule.blobContent)));

        using var compilerInteractiveSession = new InteractiveSessionPine(
            compileElmProgramCodeFiles: compilerProgram,
            appCodeTree: compilerWithPackagesTree,
            caching: true,
            autoPGO: null);

        Assert.AreEqual(
            "4",
            compilerInteractiveSession.Submit("1 + 3")
            .Extract(err => throw new System.Exception(err))
            .ToString());
    }
}
