using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using System;
using System.Collections.Generic;

namespace Pine.PineVM;

/// <summary>
/// This partial class supports the integration of source generators.
/// Source generators provide a way to implement the code generation we use to optimize the execution of Elm programs.
/// To learn about source generators, see https://learn.microsoft.com/en-us/dotnet/csharp/roslyn-sdk/source-generators-overview
/// </summary>
public partial class PineVMConfiguration
{
    public static IReadOnlyDictionary<PineValue, Func<PineVM.EvalExprDelegate, PineValue, Result<string, PineValue>>>?
        DecodeExpressionOverrides = null;

    static PineVMConfiguration()
    {
        LinkGeneratedOptimizations();
    }

    static partial void LinkGeneratedOptimizations();

    public static Result<string, string> GenerateCSharpFile(
        PineCompileToDotNet.SyntaxContainerConfig syntaxContainerConfig,
        PineCompileToDotNet.CompileCSharpClassResult compileCSharpClassResult)
    {
        var configurationClassDeclaration =
            SyntaxFactory.ClassDeclaration(nameof(PineVMConfiguration))
                .WithModifiers(
                    SyntaxFactory.TokenList(
                        SyntaxFactory.Token(SyntaxKind.PublicKeyword),
                        SyntaxFactory.Token(SyntaxKind.PartialKeyword)))
                .WithMembers(
                    SyntaxFactory.SingletonList<MemberDeclarationSyntax>(
                        SyntaxFactory.MethodDeclaration(
                                SyntaxFactory.PredefinedType(
                                    SyntaxFactory.Token(SyntaxKind.VoidKeyword)),
                                SyntaxFactory.Identifier(nameof(LinkGeneratedOptimizations)))
                            .WithModifiers(
                                SyntaxFactory.TokenList(SyntaxFactory.Token(
                                    SyntaxKind.StaticKeyword),
                                    SyntaxFactory.Token(SyntaxKind.PartialKeyword)))
                            .WithBody(
                                SyntaxFactory.Block(
                                    SyntaxFactory.SingletonList<StatementSyntax>(
                                        SyntaxFactory.ExpressionStatement(
                                            SyntaxFactory.AssignmentExpression(
                                                SyntaxKind.SimpleAssignmentExpression,
                                                SyntaxFactory.IdentifierName(nameof(DecodeExpressionOverrides)),
                                                SyntaxFactory.InvocationExpression(
                                                    SyntaxFactory.MemberAccessExpression(
                                                        SyntaxKind.SimpleMemberAccessExpression,
                                                        SyntaxFactory.IdentifierName(syntaxContainerConfig
                                                            .containerTypeName),
                                                        SyntaxFactory.IdentifierName(syntaxContainerConfig
                                                            .dictionaryMemberName))))))))));

        var configurationClassDeclarationInNamespace =
            SyntaxFactory.NamespaceDeclaration(
                    SyntaxFactory.QualifiedName(
                        SyntaxFactory.IdentifierName(nameof(Pine)),
                        SyntaxFactory.IdentifierName("PineVM")))
                .WithMembers(SyntaxFactory.SingletonList<MemberDeclarationSyntax>(configurationClassDeclaration));

        var compilationUnitSyntax =
            SyntaxFactory.CompilationUnit()
                .WithUsings(new SyntaxList<UsingDirectiveSyntax>(compileCSharpClassResult.UsingDirectives))
                .WithMembers(
                    SyntaxFactory.List(
                        new MemberDeclarationSyntax[]
                        {
                            configurationClassDeclarationInNamespace,
                            compileCSharpClassResult.ClassDeclarationSyntax
                        }))
                .NormalizeWhitespace(eol: "\n");

        var syntaxTree = CSharpSyntaxTree.Create(compilationUnitSyntax);

        return Result<string, string>.ok(syntaxTree.ToString());
    }
}