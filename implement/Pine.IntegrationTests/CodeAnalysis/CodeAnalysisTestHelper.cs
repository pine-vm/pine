using AwesomeAssertions;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Pine.CompilePineToDotNet;
using Pine.Core;
using Pine.Core.CodeAnalysis;
using Pine.Core.Internal;
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
        };

        IReadOnlyList<UsingDirectiveSyntax> usingDirectives =
            [..usingDirectivesTypes
            .Select(t => t.Namespace)
            .WhereNotNull()
            .Distinct()
            .Select(ns => SyntaxFactory.UsingDirective(SyntaxFactory.IdentifierName(ns)))
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
            PineVM.CodeAnalysis.ParseAsStaticMonomorphicProgram(
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
