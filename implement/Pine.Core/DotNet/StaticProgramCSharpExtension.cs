using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using System;
using System.Collections.Frozen;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Pine.Core.DotNet;

public static class StaticProgramCSharpExtension
{
    public static IReadOnlyDictionary<IReadOnlyList<string>, ReadOnlyMemory<byte>> BuildCSharpProjectFiles(
        this StaticProgramCSharp staticProgram,
        IReadOnlyList<string> namespacePrefix)
    {
        static IReadOnlyList<string> FilePathFromClassDeclaration(
            ClassDeclarationSyntax classDeclarationSyntax,
            IReadOnlyList<string> namespacePrefix)
        {
            var namespacesFromSyntax =
                classDeclarationSyntax
                .Ancestors()
                .OfType<NamespaceDeclarationSyntax>()
                .Select(ns => ns.Name.ToString())
                .Reverse()
                .ToList();

            return
                [
                    ..namespacePrefix,
                    ..namespacesFromSyntax,
                    classDeclarationSyntax.Identifier.Text + ".cs"
                ];
        }

        IReadOnlyList<(IReadOnlyList<string> namespaces, ClassDeclarationSyntax declSyntax)> classDeclarations =
        [
            ([],staticProgram.CommonValueClass),
            ..staticProgram.ModulesClasses.Select(mc => (mc.Key.Namespaces, mc.Value.ClassDeclarationSyntax)),
            ([], staticProgram.GlobalAnonymousClass.ClassDeclarationSyntax),
            ([], staticProgram.DispatcherClass),
            ];

        return
            classDeclarations
            .ToFrozenDictionary(
                cd => FilePathFromClassDeclaration(cd.declSyntax, [.. namespacePrefix, .. cd.namespaces]),
                cd =>
                (ReadOnlyMemory<byte>)
                    Encoding.UTF8.GetBytes(
                        BuildCompilationUnitSyntax(
                            cd.declSyntax,
                            staticProgram.DeclarationSyntaxContext,
                            [.. namespacePrefix, .. cd.namespaces]).ToFullString())
                    .AsMemory(),
                comparer:
                EnumerableExtensions.EqualityComparer<IReadOnlyList<string>>());
    }

    public static CompilationUnitSyntax BuildCompilationUnitSyntax(
        ClassDeclarationSyntax classDeclaration,
        DeclarationSyntaxContext declarationSyntaxContext,
        IReadOnlyList<string> namespacePrefix)
    {
        IReadOnlyList<string> aggregateNamepace =
            [.. namespacePrefix
            ,..declarationSyntaxContext.CurrentNamespace is { } currentNs ? currentNs.Split('.'):[]
            ];

        MemberDeclarationSyntax
            compilationUnitMember =
            aggregateNamepace.Count is 0
            ?
            classDeclaration
            :
            SyntaxFactory.FileScopedNamespaceDeclaration(
                SyntaxFactory.ParseName(string.Join('.', aggregateNamepace)))
            .WithMembers([classDeclaration]);

        var compilationUnitSyntax =
            SyntaxFactory.CompilationUnit()
            .WithUsings(
                [.. declarationSyntaxContext.UsingDirectives
                .OrderBy(ud => ud.ToFullString())
                ])
            .WithMembers([compilationUnitMember]);

        var normalized = compilationUnitSyntax.NormalizeWhitespace(eol: "\n");
        var tree = CSharpSyntaxTree.ParseText(normalized.ToFullString());
        var formattedTree = FormatCSharpFile.FormatSyntaxTree(tree);

        return (CompilationUnitSyntax)formattedTree.GetRoot();
    }
}
