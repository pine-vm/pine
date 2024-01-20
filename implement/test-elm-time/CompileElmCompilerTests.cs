using ElmTime.ElmInteractive;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using Pine;
using Pine.PineVM;
using System;
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
            "https://github.com/Viir/elm-bigint/tree/d452b489c5795f8deed19658a7b8f7bf5ef1e9a4/",

            /*
             * Remove usages of Json.Decode and Json.Encode to speed up bootstrapping of the Elm compiler.
             * */
            "https://github.com/Viir/elm-syntax/tree/f7d9be0a1f346b22dfaa7b55679659874c72714b/"
        ];

    [TestMethod]
    public void Test_call_Basics_modBy()
    {
        using var pgoShare = new DynamicPGOShare();

        var compilerProgram = IInteractiveSession.CompileElmProgramCodeFilesDefault.Value;

        using var interactiveSession =
            new InteractiveSessionPine(
                compilerProgram,
                appCodeTree: null,
                caching: true,
                autoPGO: pgoShare);

        // Force integration of the 'Basics' module.
        var testSubmissionResult = interactiveSession.Submit(" Basics.modBy  13 17 ");

        var testSubmissionResponse =
            testSubmissionResult.Extract(err => throw new Exception(err));

        Assert.AreEqual("4", testSubmissionResponse.interactiveResponse.displayText);

        var interactiveEnvironmentValue = interactiveSession.CurrentEnvironmentValue();

        var pineVM = new PineVM();

        var modByFunction =
            ElmInteractiveEnvironment.ParseFunctionFromElmModule(
                interactiveEnvironment: interactiveEnvironmentValue,
                moduleName: "Basics",
                declarationName: "modBy",
                pineVM: pineVM)
            .Extract(err => throw new Exception(err));

        var modByApplicationResult =
            ElmInteractiveEnvironment.ApplyFunction(
                pineVM,
                modByFunction,
                arguments:
                [PineValueAsInteger.ValueFromSignedInteger(13),
                    PineValueAsInteger.ValueFromSignedInteger(17)]);

        Assert.AreEqual(
            PineValueAsInteger.ValueFromSignedInteger(4),
            modByApplicationResult.Extract(err => throw new Exception(err)));

        modByApplicationResult =
            ElmInteractiveEnvironment.ApplyFunction(
                pineVM,
                modByFunction,
                arguments:
                [PineValueAsInteger.ValueFromSignedInteger(41),
                    PineValueAsInteger.ValueFromSignedInteger(47)]);

        Assert.AreEqual(
            PineValueAsInteger.ValueFromSignedInteger(6),
            modByApplicationResult.Extract(err => throw new Exception(err)));
    }


    [TestMethod]
    public void Test_call_String_split()
    {
        using var pgoShare = new DynamicPGOShare();

        var compilerProgram = IInteractiveSession.CompileElmProgramCodeFilesDefault.Value;

        using var interactiveSession =
            new InteractiveSessionPine(
                compilerProgram,
                appCodeTree: null,
                caching: true,
                autoPGO: pgoShare);

        // Force integration of the 'String' module.
        var testSubmissionResult = interactiveSession.Submit(""" String.isEmpty  "test" """);

        var testSubmissionResponse =
            testSubmissionResult.Extract(err => throw new Exception(err));

        Assert.AreEqual("False", testSubmissionResponse.interactiveResponse.displayText);

        var interactiveEnvironmentValue = interactiveSession.CurrentEnvironmentValue();

        var pineVM = new PineVM();

        var stringSplitFunction =
            ElmInteractiveEnvironment.ParseFunctionFromElmModule(
                interactiveEnvironment: interactiveEnvironmentValue,
                moduleName: "String",
                declarationName: "split",
                pineVM: pineVM)
            .Extract(err => throw new Exception(err));

        var stringSplitApplicationResult =
            ElmInteractiveEnvironment.ApplyFunction(
                pineVM,
                stringSplitFunction,
                arguments:
                [ElmValue.ElmValueAsPineValue(new ElmValue.ElmString(",")),
                    ElmValue.ElmValueAsPineValue(new ElmValue.ElmString("pizza,risotto,focaccia"))]);

        var stringSplitResultElmValue =
            ElmValue.PineValueAsElmValue(stringSplitApplicationResult.Extract(err => throw new Exception(err)))
            .Extract(err => throw new Exception(err));

        Assert.AreEqual(
            new ElmValue.ElmList(
                [new ElmValue.ElmString("pizza"),
                    new ElmValue.ElmString("risotto"),
                    new ElmValue.ElmString("focaccia")]),
            stringSplitResultElmValue);
    }

    [Ignore("Productive side not ready yet")]
    [TestMethod]
    public void Test_compile_elm_compiler()
    {
        using var pgoShare = new DynamicPGOShare();

        var compilerProgram = IInteractiveSession.CompileElmProgramCodeFilesDefault.Value;

        using var compileJavaScriptEngine =
            ElmInteractive.PrepareJavaScriptEngineToEvaluateElm(
                compileElmProgramCodeFiles: compilerProgram,
                InteractiveSessionJavaScript.JavaScriptEngineFlavor.V8);

        var compilerPackageSourcesTrees =
            CompilerPackageSources
            .Select(LoadFromGitHubOrGitLab.LoadFromUrl)
            .ListCombine()
            .Extract(err => throw new Exception(err));

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
            autoPGO: pgoShare);

        Assert.AreEqual(
            "4",
            compilerInteractiveSession.Submit("1 + 3")
            .Extract(err => throw new Exception(err))
            .ToString());
    }
}
