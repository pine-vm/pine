namespace Pine.Core.Elm.ElmCompilerInDotnet;

using Pine.Core.CodeAnalysis;
using System.Collections.Immutable;

using Abstract = ElmSyntax.ElmSyntaxAbstract;
using Stil4mElmSyntax7 = ElmSyntax.Stil4mElmSyntax7;

internal static class ElmSyntaxAbstractConversion
{
    public static Abstract.File FromFile(Stil4mElmSyntax7.File file) =>
        Abstract.ConvertFromConcrete.FromFile(Stil4mElmSyntax7.ToFullSyntaxModel.Convert(file));

    public static Stil4mElmSyntax7.File ToFile(Abstract.File file) =>
        Stil4mElmSyntax7.FromFullSyntaxModel.Convert(Abstract.ConvertToConcrete.FromFile(file));

    public static Abstract.Expression FromExpression(Stil4mElmSyntax7.Expression expression) =>
        Abstract.ConvertFromConcrete.FromExpression(Stil4mElmSyntax7.ToFullSyntaxModel.Convert(expression));

    public static Abstract.Pattern FromPattern(Stil4mElmSyntax7.Pattern pattern) =>
        Abstract.ConvertFromConcrete.FromPattern(Stil4mElmSyntax7.ToFullSyntaxModel.Convert(pattern));

    public static Abstract.Declaration FromDeclaration(Stil4mElmSyntax7.Declaration declaration) =>
        Abstract.ConvertFromConcrete.FromDeclaration(Stil4mElmSyntax7.ToFullSyntaxModel.Convert(declaration));

    public static Stil4mElmSyntax7.Declaration ToDeclaration(Abstract.Declaration declaration) =>
        Stil4mElmSyntax7.FromFullSyntaxModel.Convert(Abstract.ConvertToConcrete.ToDeclaration(declaration));

    public static ImmutableDictionary<DeclQualifiedName, Abstract.Declaration> FromDeclarationDictionary(
        ImmutableDictionary<DeclQualifiedName, Stil4mElmSyntax7.Declaration> declarations) =>
        declarations.ToImmutableDictionary(
            item => item.Key,
            item => FromDeclaration(item.Value));

    public static ImmutableDictionary<DeclQualifiedName, Stil4mElmSyntax7.Declaration> ToDeclarationDictionary(
        ImmutableDictionary<DeclQualifiedName, Abstract.Declaration> declarations) =>
        declarations.ToImmutableDictionary(
            item => item.Key,
            item => ToDeclaration(item.Value));
}
