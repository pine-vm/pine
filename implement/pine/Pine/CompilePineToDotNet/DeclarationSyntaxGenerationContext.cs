using Microsoft.CodeAnalysis.CSharp.Syntax;
using System.Collections.Generic;

namespace Pine.CompilePineToDotNet
{
    public record DeclarationSyntaxGenerationContext(
        IReadOnlyCollection<UsingDirectiveSyntax> UsingDirectives,
        string? CurrentNamespace = null);
}
