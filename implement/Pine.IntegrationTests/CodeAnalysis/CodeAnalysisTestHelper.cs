using AwesomeAssertions;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Pine.Core;
using Pine.Core.CodeAnalysis;
using Pine.Core.DotNet;
using Pine.Core.Internal;
using Pine.Core.PopularEncodings;
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
}
