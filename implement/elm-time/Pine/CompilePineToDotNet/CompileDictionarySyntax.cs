using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using System.Collections.Generic;
using System.Linq;

namespace Pine.CompilePineToDotNet;

public static class CompileDictionarySyntax
{
    public static ExpressionSyntax EmptyImmutableDictionaryExpressionSyntax(
        TypeSyntax keyTypeSyntax,
        TypeSyntax valueTypeSyntax)
    {
        return
        SyntaxFactory.MemberAccessExpression(
            SyntaxKind.SimpleMemberAccessExpression,
            SyntaxFactory.GenericName(
                SyntaxFactory.Identifier("ImmutableDictionary"))
            .WithTypeArgumentList(
                SyntaxFactory.TypeArgumentList(
                    SyntaxFactory.SeparatedList<TypeSyntax>(
                        new SyntaxNodeOrToken[]
                        {
                            keyTypeSyntax,
                            SyntaxFactory.Token(SyntaxKind.CommaToken),
                            valueTypeSyntax
                        }))),
            SyntaxFactory.IdentifierName("Empty"));
    }

    public static ExpressionSyntax ImmutableDictionaryExpressionSyntax<KeyT, ValueT>(
        TypeSyntax keyTypeSyntax,
        TypeSyntax valueTypeSyntax,
        IReadOnlyList<(KeyT key, ValueT value)> dictionaryEntries)
        where KeyT : ExpressionSyntax
        where ValueT : ExpressionSyntax
        =>
        ImmutableDictionaryExpressionSyntax(
            keyTypeSyntax: keyTypeSyntax,
            valueTypeSyntax: valueTypeSyntax,
            dictionaryEntries:
            [.. dictionaryEntries
            .Select(entry => new KeyValuePair<ExpressionSyntax, ExpressionSyntax>(entry.key, entry.value))]);

    public static ExpressionSyntax ImmutableDictionaryExpressionSyntax(
        TypeSyntax keyTypeSyntax,
        TypeSyntax valueTypeSyntax,
        IReadOnlyList<KeyValuePair<ExpressionSyntax, ExpressionSyntax>> dictionaryEntries)
    {
        return
            dictionaryEntries
            .Aggregate(
                seed:
                EmptyImmutableDictionaryExpressionSyntax(
                    keyTypeSyntax: keyTypeSyntax,
                    valueTypeSyntax: valueTypeSyntax),
                func: (dictionaryExpression, dictionaryEntry) =>
                    SyntaxFactory.InvocationExpression(
                        SyntaxFactory.MemberAccessExpression(
                            SyntaxKind.SimpleMemberAccessExpression,
                            dictionaryExpression,
                            SyntaxFactory.IdentifierName("SetItem")))
                    .WithArgumentList(
                        SyntaxFactory.ArgumentList(
                            SyntaxFactory.SeparatedList<ArgumentSyntax>(
                                new SyntaxNodeOrToken[]
                                {
                                    SyntaxFactory.Argument(dictionaryEntry.Key),
                                    SyntaxFactory.Token(SyntaxKind.CommaToken),
                                    SyntaxFactory.Argument(dictionaryEntry.Value)
                                }))));
    }
}
