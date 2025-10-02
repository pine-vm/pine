using Microsoft.CodeAnalysis.CSharp.Syntax;
using System.Collections.Generic;

namespace Pine.Core.DotNet;

public record DeclarationSyntaxContext(
    IReadOnlyCollection<UsingDirectiveSyntax> UsingDirectives,
    string? CurrentNamespace = null)
{
    public static readonly DeclarationSyntaxContext None =
        new([], null);
}
