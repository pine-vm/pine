using Microsoft.CodeAnalysis.CSharp.Syntax;
using System.Collections.Generic;

namespace Pine.Core.DotNet;

/// <summary>
/// Ambient Roslyn context used during C# emission.
/// Carries using directives and an optional current namespace, to inform how to emit a (type) reference for a given canonical name.
/// </summary>
/// <param name="UsingDirectives">The set of using directives (including alias usings) to attach to a compilation unit.</param>
/// <param name="CurrentNamespace">Optional namespace the generated members should live in. Null means emit at the global namespace.</param>
public record DeclarationSyntaxContext(
    IReadOnlyCollection<UsingDirectiveSyntax> UsingDirectives,
    string? CurrentNamespace = null)
{
    /// <summary>
    /// An empty context: no using directives and no namespace.
    /// </summary>
    public static readonly DeclarationSyntaxContext None =
        new([], null);
}
