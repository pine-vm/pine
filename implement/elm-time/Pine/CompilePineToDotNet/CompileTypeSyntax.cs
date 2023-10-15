using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

namespace Pine.CompilePineToDotNet;

public static class CompileTypeSyntax
{
    public static TypeSyntax TypeSyntaxFromType(
        Type type,
        IReadOnlyCollection<UsingDirectiveSyntax> usings)
    {
        var namespaceSegments =
            ShortestRelativeNamespace(type.Namespace ?? "", usings)
            .Concat(NamespaceSegmentsResultingFromDeclaringTypes(type))
            .ToImmutableArray();

        SimpleNameSyntax nameInNamespace = SyntaxFactory.IdentifierName(type.Name);

        if (type.IsGenericType)
        {
            var genericTypeDefinition = type.GetGenericTypeDefinition();

            /*
             * type.GetGenericTypeDefinition().Name returns names like "Result`2"
             * */
            var genericTypeName = genericTypeDefinition.Name.Split('`').First();

            nameInNamespace =
                SyntaxFactory.GenericName(
                    SyntaxFactory.Identifier(genericTypeName),
                    SyntaxFactory.TypeArgumentList(
                        SyntaxFactory.SeparatedList(
                            type.GetGenericArguments()
                            .Select(ga => TypeSyntaxFromType(ga, usings)))));
        }

        return
            NameSyntaxFromQualifiedName(
                namespaceSegments: namespaceSegments,
                nameInNamespace: nameInNamespace);
    }

    public static IEnumerable<string> NamespaceSegmentsResultingFromDeclaringTypes(Type type) =>
        type.DeclaringType switch
        {
            null =>
            [],

            var declaringType =>
            NamespaceSegmentsResultingFromDeclaringTypes(declaringType)
            .Append(declaringType.Name)
        };

    public static IReadOnlyList<string> ShortestRelativeNamespace(
        string fullNamespace,
        IReadOnlyCollection<UsingDirectiveSyntax> usings)
    {
        static IReadOnlyList<string> SegmentsFromName(string name) =>
            name.Split('.', StringSplitOptions.TrimEntries | StringSplitOptions.RemoveEmptyEntries);

        var namespaceSegments =
            SegmentsFromName(fullNamespace);

        var usingsNames =
            usings
            .SelectWhereNotNull(usingDirective => usingDirective.Name?.ToFullString()?.Trim())
            .Select(SegmentsFromName);

        if (usingsNames.Any(usingName => usingName.SequenceEqual(namespaceSegments)))
            return [];

        return namespaceSegments;
    }

    public static NameSyntax NameSyntaxFromQualifiedName(
        IReadOnlyList<string> namespaceSegments,
        SimpleNameSyntax nameInNamespace) =>
        namespaceSegments switch
        {
        [] =>
        nameInNamespace,

        [var firstSegment, ..] =>
            SyntaxFactory.QualifiedName(
                namespaceSegments
                .Skip(1)
                .Select(SyntaxFactory.IdentifierName)
                .Aggregate(
                    seed: (NameSyntax)SyntaxFactory.IdentifierName(firstSegment),
                    func: SyntaxFactory.QualifiedName),
            nameInNamespace)
        };
}
