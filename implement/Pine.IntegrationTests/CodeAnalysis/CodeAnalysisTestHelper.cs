using AwesomeAssertions;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Pine.Core;
using Pine.Core.CodeAnalysis;
using Pine.Core.DotNet;
using Pine.Core.Internal;
using Pine.Core.PopularEncodings;
using Pine.Elm;
using System.Collections.Frozen;
using System.Collections.Generic;
using System.Linq;
using System.Text;

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
            typeof(Core.PineVM.PineKernelValues),
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

    public static (ElmInteractiveEnvironment.ParsedInteractiveEnvironment parsedEnv, StaticProgram staticProgram)
        StaticProgramFromElmModules(
        IReadOnlyList<string> elmModulesTexts,
        System.Func<DeclQualifiedName, bool> includeDeclaration,
        PineVMParseCache parseCache)
    {
        var elmModulesByNameDict =
            elmModulesTexts
            .ToFrozenDictionary(
                moduleText =>
                ElmTime.ElmSyntax.ElmModule.ParseModuleName(moduleText)
                .Extract(err => throw new System.Exception("Failed parsing module name from module text: " + err)),
                moduleText => moduleText,
                comparer: EnumerableExtension.EqualityComparer<IReadOnlyList<string>>());

        var elmJsonFile =
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

        var appCodeTree =
            BlobTreeWithStringPath.EmptyTree
            .SetNodeAtPathSorted(
                ["elm.json"],
                BlobTreeWithStringPath.Blob(Encoding.UTF8.GetBytes(elmJsonFile)));

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

        var rootFilePaths =
            appCodeTree.EnumerateBlobsTransitive()
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
