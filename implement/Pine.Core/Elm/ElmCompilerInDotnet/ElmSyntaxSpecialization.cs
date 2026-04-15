using Pine.Core.CodeAnalysis;
using System.Collections.Immutable;

using SyntaxTypes = Pine.Core.Elm.ElmSyntax.Stil4mElmSyntax7;

namespace Pine.Core.Elm.ElmCompilerInDotnet;

public static class ElmSyntaxSpecialization
{
    public static Result<string, ImmutableDictionary<DeclQualifiedName, SyntaxTypes.Declaration>> Apply(
        ImmutableDictionary<DeclQualifiedName, SyntaxTypes.Declaration> declarations,
        Inlining.Config config) =>
        Inlining.RunSpecializationStage(declarations, config);
}
