using Pine.Core.CodeAnalysis;
using Pine.Core.Elm.ElmCompilerInDotnet;
using System.Collections.Generic;
using System.Linq;

namespace Pine.IntegrationTests.CodeAnalysis;

public class CodeAnalysisTestHelper
{
    public static (StaticProgram<DeclQualifiedName> staticProgram, IReadOnlyDictionary<DeclQualifiedName, StaticProgramFunctionMetadata> functionMetadata) ParseAsStaticMonomorphicProgramAndCrashOnAnyFailure(
        ElmInteractiveEnvironment.ParsedInteractiveEnvironment parsedEnvironment,
        System.Func<DeclQualifiedName, bool> includeDeclaration,
        PineVMParseCache parseCache)
    {
        return
            Core.Tests.Elm.ElmCompilerTests.TestCase.ParseAsStaticMonomorphicProgramAndCrashOnAnyFailure(
                parsedEnvironment,
                includeDeclaration,
                parseCache);
    }

    public static (ElmInteractiveEnvironment.ParsedInteractiveEnvironment parsedEnv, StaticProgram<DeclQualifiedName> staticProgram, IReadOnlyDictionary<DeclQualifiedName, StaticProgramFunctionMetadata> functionMetadata)
        StaticProgramFromElmModules(
        IReadOnlyList<string> elmModulesTexts,
        System.Func<DeclQualifiedName, bool> includeDeclaration,
        PineVMParseCache parseCache)
    {
        var appCodeTree =
            Core.Tests.Elm.ElmCompilerTests.TestCase.FileTreeFromElmModulesWithoutPackages(elmModulesTexts);

        var rootFilePaths =
            appCodeTree.EnumerateFilesTransitive()
            .Where(b => b.path[^1].EndsWith(".elm"))
            .Select(b => b.path)
            .ToList();

        var compiledEnv =
            Core.Tests.Elm.ElmCompilerInDotnet.ElmCompilerTestHelper.CompileInteractiveEnvironmentFromFiles(
                appCodeTree,
                rootFilePaths: rootFilePaths,
                new ElmSyntaxOptimizationConfig.SyntaxOptimizationDisabled())
            .Extract(err => throw new System.Exception(err));

        var parsedEnv =
            ElmInteractiveEnvironment.ParseInteractiveEnvironment(compiledEnv.compiledEnvValue)
            .Extract(err => throw new System.Exception("Failed parsing interactive environment: " + err));

        var (staticProgram, functionMetadata) =
            ParseAsStaticMonomorphicProgramAndCrashOnAnyFailure(
                parsedEnv,
                includeDeclaration: includeDeclaration,
                parseCache);

        return (parsedEnv, staticProgram, functionMetadata);
    }

    public static (ElmInteractiveEnvironment.ParsedInteractiveEnvironment parsedEnv, StaticProgram<DeclQualifiedName> staticProgram, IReadOnlyDictionary<DeclQualifiedName, StaticProgramFunctionMetadata> functionMetadata)
        StaticProgramFromElmKernelModules(
        IReadOnlyList<string> rootModuleFileNames,
        System.Func<DeclQualifiedName, bool> includeDeclaration,
        PineVMParseCache parseCache)
    {
        var appCodeTree =
            Core.Elm.ElmInElm.BundledFiles.ElmKernelModulesDefault.Value;

        var rootFilePaths =
            appCodeTree.EnumerateFilesTransitive()
            .Where(b => rootModuleFileNames.Any(n => b.path[^1].Equals(n, System.StringComparison.OrdinalIgnoreCase)))
            .Select(b => (IReadOnlyList<string>)b.path)
            .ToList();

        var compiledEnv =
            Core.Tests.Elm.ElmCompilerInDotnet.ElmCompilerTestHelper.CompileInteractiveEnvironmentFromFiles(
                appCodeTree,
                rootFilePaths: rootFilePaths,
                new ElmSyntaxOptimizationConfig.SyntaxOptimizationDisabled())
            .Extract(err => throw new System.Exception(err));

        var parsedEnv =
            ElmInteractiveEnvironment.ParseInteractiveEnvironment(compiledEnv.compiledEnvValue)
            .Extract(err => throw new System.Exception("Failed parsing interactive environment: " + err));

        var (staticProgram, functionMetadata) =
            ParseAsStaticMonomorphicProgramAndCrashOnAnyFailure(
                parsedEnv,
                includeDeclaration: includeDeclaration,
                parseCache);

        return (parsedEnv, staticProgram, functionMetadata);
    }
}
