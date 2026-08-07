namespace Pine.Core.Elm.ElmCompilerInDotnet;

using Pine.Core.CodeAnalysis;
using System.Collections.Immutable;

using Abstract = ElmSyntax.ElmSyntaxAbstract;
using Concrete = ElmSyntax.Stil4mElmSyntax7;

internal static class ElmSyntaxAbstractConversion
{
    public static Abstract.File FromFile(Concrete.File file) =>
        Abstract.ConvertFromConcrete.FromFile(Concrete.ToFullSyntaxModel.Convert(file));

    public static Concrete.File ToFile(Abstract.File file) =>
        Concrete.FromFullSyntaxModel.Convert(Abstract.ConvertToConcrete.FromFile(file));

    public static Abstract.Expression FromExpression(Concrete.Expression expression) =>
        Abstract.ConvertFromConcrete.FromExpression(Concrete.ToFullSyntaxModel.Convert(expression));

    public static Abstract.Pattern FromPattern(Concrete.Pattern pattern) =>
        Abstract.ConvertFromConcrete.FromPattern(Concrete.ToFullSyntaxModel.Convert(pattern));

    public static Abstract.Declaration FromDeclaration(Concrete.Declaration declaration) =>
        Abstract.ConvertFromConcrete.FromDeclaration(Concrete.ToFullSyntaxModel.Convert(declaration));

    public static Concrete.Declaration ToDeclaration(Abstract.Declaration declaration) =>
        Concrete.FromFullSyntaxModel.Convert(Abstract.ConvertToConcrete.ToDeclaration(declaration));

    public static ImmutableDictionary<DeclQualifiedName, Abstract.Declaration> FromDeclarationDictionary(
        ImmutableDictionary<DeclQualifiedName, Concrete.Declaration> declarations) =>
        declarations.ToImmutableDictionary(
            item => item.Key,
            item => FromDeclaration(item.Value));

    public static ImmutableDictionary<DeclQualifiedName, Concrete.Declaration> ToDeclarationDictionary(
        ImmutableDictionary<DeclQualifiedName, Abstract.Declaration> declarations) =>
        declarations.ToImmutableDictionary(
            item => item.Key,
            item => ToDeclaration(item.Value));
}
