using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Pine.Core;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

namespace Pine.CompilePineToDotNet;

public static class CompileTypeSyntax
{
    public static TypeSyntax TypeSyntaxFromType(
        Type type,
        IReadOnlyCollection<UsingDirectiveSyntax> usings) =>
        TypeSyntaxFromType(type, new DeclarationSyntaxContext(usings));

    public static TypeSyntax TypeSyntaxFromType(
        Type type,
        DeclarationSyntaxContext context)
    {
        // Check if the type can be represented using a using alias

        if (TryGetAliasForType(type, context) is { } aliasTypeName)
        {
            SimpleNameSyntax aliasSyntax = SyntaxFactory.IdentifierName(aliasTypeName);

            if (type.IsGenericType)
            {
                var genericTypeName = aliasTypeName;

                aliasSyntax =
                    SyntaxFactory.GenericName(
                        SyntaxFactory.Identifier(genericTypeName),
                        SyntaxFactory.TypeArgumentList(
                            SyntaxFactory.SeparatedList(
                                type.GetGenericArguments()
                                .Select(ga => TypeSyntaxFromType(ga, context)))));
            }

            return aliasSyntax;
        }

        var namespaceSegments =
            ShortestRelativeNamespace(type.Namespace ?? "", context)
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
                            .Select(ga => TypeSyntaxFromType(ga, context)))));
        }

        return
            NameSyntaxFromQualifiedName(
                namespaceSegments: namespaceSegments,
                nameInNamespace: nameInNamespace);
    }

    private static string? TryGetAliasForType(Type type, DeclarationSyntaxContext context)
    {
        // Get the namespace and name for comparison
        var namespacePart = type.Namespace ?? "";
        var typeName = type.Name;

        // Handle generic types by getting the generic type definition name without the arity
        if (type.IsGenericType)
        {
            var genericTypeDefinition = type.GetGenericTypeDefinition();
            namespacePart = genericTypeDefinition.Namespace ?? "";
            typeName = genericTypeDefinition.Name.Split('`').First();
        }

        // Construct the full name for comparison
        var fullTypeName = string.IsNullOrEmpty(namespacePart) ? typeName : $"{namespacePart}.{typeName}";

        foreach (var usingDirective in context.UsingDirectives)
        {
            // Check if this is an alias using directive (has an Alias property)
            if (usingDirective.Alias is not null && usingDirective.Name is not null)
            {
                var aliasTarget = usingDirective.Name.ToFullString().Trim();
                if (aliasTarget == fullTypeName)
                {
                    return usingDirective.Alias.Name.Identifier.ValueText;
                }
            }
        }

        return null;
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
        IReadOnlyCollection<UsingDirectiveSyntax> usings) =>
        ShortestRelativeNamespace(fullNamespace, new DeclarationSyntaxContext(usings));

    public static IReadOnlyList<string> ShortestRelativeNamespace(
        string fullNamespace,
        DeclarationSyntaxContext context)
    {
        static IReadOnlyList<string> SegmentsFromName(string name) =>
            name.Split('.', StringSplitOptions.TrimEntries | StringSplitOptions.RemoveEmptyEntries);

        var namespaceSegments =
            SegmentsFromName(fullNamespace);

        var usingsNames =
            context.UsingDirectives
            .SelectWhereNotNull(usingDirective => usingDirective.Name?.ToFullString()?.Trim())
            .Select(SegmentsFromName);

        // Check if the namespace is covered by using directives
        if (usingsNames.Any(usingName => usingName.SequenceEqual(namespaceSegments)))
            return [];

        // Check if the current namespace allows shortening the path
        if (context.CurrentNamespace is not null)
        {
            var currentNamespaceSegments = SegmentsFromName(context.CurrentNamespace);

            // If the target namespace exactly matches the current namespace, we can omit it entirely
            if (currentNamespaceSegments.SequenceEqual(namespaceSegments))
            {
                return [];
            }

            // If the target namespace starts with the current namespace, we can shorten it
            if (namespaceSegments.Count > currentNamespaceSegments.Count &&
                currentNamespaceSegments.SequenceEqual(namespaceSegments.Take(currentNamespaceSegments.Count)))
            {
                return namespaceSegments.Skip(currentNamespaceSegments.Count).ToArray();
            }
        }

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
