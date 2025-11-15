using Pine.Core.CodeAnalysis;
using Pine.Core.Elm.ElmCompilerInDotnet;
using Pine.Core.Tests.Elm.ElmCompilerTests;
using System.Collections.Generic;
using System.Linq;

namespace Pine.Core.Tests.Elm.ElmCompilerInDotnet;

public class ElmCompilerTestHelper
{
    public static (ElmInteractiveEnvironment.ParsedInteractiveEnvironment parsedEnv, StaticProgram staticProgram)
        StaticProgramFromElmModules(
        IReadOnlyList<string> elmModulesTexts,
        System.Func<DeclQualifiedName, bool> includeDeclaration,
        PineVMParseCache parseCache)
    {
        var testCase =
            TestCase.DefaultAppWithoutPackages(elmModulesTexts);

        return
            StaticProgramFromTestCase(
                testCase,
                includeDeclaration,
                parseCache);
    }

    public static (ElmInteractiveEnvironment.ParsedInteractiveEnvironment parsedEnv, StaticProgram staticProgram)
        StaticProgramFromTestCase(
        TestCase testCase,
        System.Func<DeclQualifiedName, bool> includeDeclaration,
        PineVMParseCache parseCache)
    {
        var appCodeTree = testCase.AsFileTree();

        var rootFilePaths =
            appCodeTree.EnumerateFilesTransitive()
            .Where(b => b.path[^1].EndsWith(".elm"))
            .Select(b => b.path)
            .ToList();

        var compiledEnv =
            ElmCompiler.CompileInteractiveEnvironment(
                appCodeTree,
                rootFilePaths: rootFilePaths)
            .Extract(err => throw new System.Exception(err));

        var parsedEnv =
            ElmInteractiveEnvironment.ParseInteractiveEnvironment(compiledEnv)
            .Extract(err => throw new System.Exception("Failed parsing interactive environment: " + err));

        var staticProgram =
            TestCase.ParseAsStaticMonomorphicProgramAndCrashOnAnyFailure(
                parsedEnv,
                includeDeclaration: includeDeclaration,
                parseCache);

        return (parsedEnv, staticProgram);
    }
}
