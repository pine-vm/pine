using AwesomeAssertions;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Pine.Core;
using Pine.Core.CodeAnalysis;
using Pine.Core.CommonEncodings;
using Pine.Core.DotNet;
using Pine.Core.Internal;
using Pine.Core.PineVM;
using Pine.Elm;
using System.Collections.Generic;
using System.Linq;

namespace Pine.IntegrationTests.CodeAnalysis;

public class CodeAnalysisTestHelper
{
    static public readonly DeclarationSyntaxContext DeclarationSyntaxContext = BuildDeclarationSyntaxContext();

    static DeclarationSyntaxContext BuildDeclarationSyntaxContext()
    {
        var usingDirectivesTypes = new[]
        {
            typeof(KernelFunction),
            typeof(KernelFunctionSpecialized),
            typeof(KernelFunctionFused),
            typeof(PineKernelValues),
            typeof(IReadOnlyDictionary<,>),
            typeof(IntegerEncoding),
            typeof(StringEncoding),
            typeof(ParseExpressionException),
            typeof(Core.DotNet.Builtins.ImmutableConcatBuilder),
            typeof(Core.DotNet.Builtins.ImmutableSliceBuilder),
            typeof(Core.DotNet.Builtins.MutatingConcatBuilder),
        };

        IReadOnlyList<UsingDirectiveSyntax> usingDirectives =
            [..usingDirectivesTypes
            .Select(t => t.Namespace)
            .WhereNotNull()
            .Distinct()
            .Order()
            .Select(ns => SyntaxFactory.UsingDirective(SyntaxFactory.ParseName(ns)))
            ];

        return
            new DeclarationSyntaxContext(usingDirectives);
    }

    public static StaticProgram ParseAsStaticMonomorphicProgramAndCrashOnAnyFailure(
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

    public static (ElmInteractiveEnvironment.ParsedInteractiveEnvironment parsedEnv, StaticProgram staticProgram)
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
            ElmCompiler.CompileInteractiveEnvironment(
                appCodeTree,
                rootFilePaths: rootFilePaths,
                skipLowering: true,
                skipFilteringForSourceDirs: false)
            .Extract(err => throw new System.Exception(err));

        var parsedEnv =
            ElmInteractiveEnvironment.ParseInteractiveEnvironment(compiledEnv)
            .Extract(err => throw new System.Exception("Failed parsing interactive environment: " + err));

        var staticProgram =
            ParseAsStaticMonomorphicProgramAndCrashOnAnyFailure(
                parsedEnv,
                includeDeclaration: includeDeclaration,
                parseCache);

        return (parsedEnv, staticProgram);
    }
}
