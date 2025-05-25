using FluentAssertions;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using Pine.Core;
using Pine.Core.Elm;
using Pine.UnitTests;

namespace TestElmTime;

[TestClass]
public class PineExecutableBundleTests
{
    [TestMethod]
    public void Bundles_default_elm_compiler()
    {
        var compilerSourceFiles =
            Pine.Elm.ElmCompiler.CompilerSourceFilesDefault.Value;

        var combinedSourceFiles =
            Pine.Elm.ElmCompiler.ElmCompilerFileTreeFromBundledFileTree(compilerSourceFiles);

        var elmCompilerFromBundleValue =
            BundledElmEnvironments.BundledElmEnvironmentFromFileTree(combinedSourceFiles);

        elmCompilerFromBundleValue.Should().NotBeNull();

        var elmCompilerFromBundle =
            Pine.Elm.ElmCompiler.ElmCompilerFromEnvValue(elmCompilerFromBundleValue)
            .Extract(err => throw new System.Exception("Failed parsing compiler from bundled value: " + err));

        var freshEnvironment =
            Pine.Elm.ElmCompiler.CompileInteractiveEnvironment(
                combinedSourceFiles,
                rootFilePaths: Pine.Elm.ElmCompiler.DefaultCompilerTreeRootModuleFilePaths,
                skipLowering: true,
                skipFilteringForSourceDirs: true,
                overrideElmCompiler: elmCompilerFromBundle)
            .Extract(err => throw new System.Exception(err));

        var elmCompiler =
            Pine.Elm.ElmCompiler.BuildCompilerFromSourceFiles(compilerSourceFiles)
            .Extract(err => throw new System.Exception(err));

        System.Console.WriteLine(
            string.Join(
                "\n",
                [
                    ..CompileElmCompilerTests.CompareCompiledEnvironmentsAndAssertEqual(
                        expectedEnv: freshEnvironment,
                        actualEnv: elmCompilerFromBundleValue)
                ]));

        freshEnvironment.Should().Be(elmCompilerFromBundleValue);

        elmCompiler.CompilerEnvironment.Should().Be(elmCompilerFromBundleValue);
    }

    [TestMethod]
    public void Bundled_environment_contains_Elm_syntax_parser()
    {
        var elmCompilerFromBundle =
            BundledElmEnvironments.BundledElmCompilerCompiledEnvValue();

        elmCompilerFromBundle.Should().NotBeNull(
            "Elm compiler environment not found in bundled environments");

        var elmCompiler =
            Pine.Elm.ElmCompiler.ElmCompilerFromEnvValue(elmCompilerFromBundle)
            .Extract(err => throw new System.Exception(err));

        var elmModuleText =
            """
            module Namespace.Beta exposing (..)

            type alias MaybeInt =
                Maybe Int
            
            """;

        var pineVMCache = new Pine.PineVM.PineVMCache();

        var pineVM =
            new Pine.PineVM.PineVM(evalCache: pineVMCache.EvalCache);

        var parseResult =
            elmCompiler.ParseElmModuleText(elmModuleText, pineVM);

        var parsedElmValue =
            ElmValueEncoding.PineValueAsElmValue(
                parseResult.Extract(err => throw new System.Exception(err)),
                null,
                null)
            .Extract(err => throw new System.Exception(err));

        var parsedElmModuleSyntaxAsString =
            ElmValue.RenderAsElmExpression(parsedElmValue).expressionString;

        parsedElmModuleSyntaxAsString.Should().Be(
            "{ comments = [], declarations = [Node { end = { column = 14, row = 4 }, start = { column = 1, row = 3 } } (AliasDeclaration { documentation = Nothing, generics = [], name = Node { end = { column = 20, row = 3 }, start = { column = 12, row = 3 } } \"MaybeInt\", typeAnnotation = Node { end = { column = 14, row = 4 }, start = { column = 5, row = 4 } } (Typed (Node { end = { column = 10, row = 4 }, start = { column = 5, row = 4 } } ([],\"Maybe\")) [Node { end = { column = 14, row = 4 }, start = { column = 11, row = 4 } } (Typed (Node { end = { column = 14, row = 4 }, start = { column = 11, row = 4 } } ([],\"Int\")) [])]) })], imports = [], moduleDefinition = Node { end = { column = 36, row = 1 }, start = { column = 1, row = 1 } } (NormalModule { exposingList = Node { end = { column = 36, row = 1 }, start = { column = 23, row = 1 } } (All { end = { column = 35, row = 1 }, start = { column = 33, row = 1 } }), moduleName = Node { end = { column = 22, row = 1 }, start = { column = 8, row = 1 } } [\"Namespace\",\"Beta\"] }) }");
    }

    [TestMethod]
    public void Embedded_precompiled_pine_value_lists()
    {
        var elmCompilerValue =
            BundledElmCompilerValue()
            ?? throw new System.Exception("Elm compiler value not found in bundled environments");

        var fromFreshBuild =
            ReusedInstances.BuildPineListValueReusedInstances(
                ReusedInstances.ExpressionsSource(),
                additionalRoots: [elmCompilerValue]);

        var fromFreshBuildListValues =
            ReusedInstances.BuildListValuesFromBundledListValues(
                fromFreshBuild.PineValueLists);

        var file =
            ReusedInstances.BuildPrecompiledDictFile(fromFreshBuild);

        var parsedFile =
            ReusedInstances.LoadFromPrebuiltJson(file);

        ReusedInstancesTests.AssertPineValueListDictsAreEquivalent(
            parsedFile.PineValueLists,
            fromFreshBuild.PineValueLists);

        ReusedInstancesTests.AssertPineValueListDictsAreEquivalent(
            ReusedInstances.Instance.ListValues,
            fromFreshBuildListValues);
    }

    [TestMethod]
    public void Bundled_Elm_kernel_module_sources_equal()
    {
        /*
         * Verify that the bundled Elm kernel module sources are equal to the module texts modeled
         * as declarations in the Elm modules "ElmInteractiveCoreModules.elm" and "ElmInteractiveKernelModules.elm"
         * Having the source text duplicated in these declarations makes it easier to integrate when using tools like
         * `elm-test-rs` to run tests of the Elm compiler.
         * */

        // TODO
    }


    public static PineValue? BundledElmCompilerValue()
    {
        var compilerSourceFiles =
            Pine.Elm.ElmCompiler.CompilerSourceFilesDefault.Value;

        var combinedSourceFiles =
            Pine.Elm.ElmCompiler.ElmCompilerFileTreeFromBundledFileTree(compilerSourceFiles);

        var elmCompilerFromBundleValue =
            BundledElmEnvironments.BundledElmEnvironmentFromFileTree(combinedSourceFiles);

        return elmCompilerFromBundleValue;
    }
}
