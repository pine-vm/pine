using Microsoft.VisualStudio.TestTools.UnitTesting;

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

        var freshEnvironment =
            Pine.Elm.ElmCompiler.CompileInteractiveEnvironment(combinedSourceFiles)
            .Extract(err => throw new System.Exception(err));

        var elmCompiler =
            Pine.Elm.ElmCompiler.BuildElmCompiler(compilerSourceFiles)
            .Extract(err => throw new System.Exception(err));

        var elmCompilerFromBundle =
            Pine.Core.Elm.BundledElmEnvironments.BundledElmEnvironmentFromFileTree(combinedSourceFiles);

        Assert.IsNotNull(elmCompilerFromBundle);

        Assert.AreEqual(
            freshEnvironment,
            elmCompilerFromBundle);

        Assert.AreEqual(
            elmCompiler.CompilerEnvironment,
            elmCompilerFromBundle);
    }
}