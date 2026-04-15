using Pine.Core.CodeAnalysis;
using System.Collections.Immutable;

using SyntaxTypes = Pine.Core.Elm.ElmSyntax.Stil4mElmSyntax7;

namespace Pine.Core.Elm.ElmCompilerInDotnet;

public static class ElmSyntaxInlining
{
    public static Result<string, ImmutableDictionary<DeclQualifiedName, SyntaxTypes.Declaration>> Apply(
        ImmutableDictionary<DeclQualifiedName, SyntaxTypes.Declaration> declarations,
        Inlining.Config config) =>
        Inlining.RunInliningStage(declarations, config);
}
