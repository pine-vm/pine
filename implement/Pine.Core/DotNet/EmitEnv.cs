using Microsoft.CodeAnalysis.CSharp.Syntax;
using Pine.Core.CodeAnalysis;
using System.Collections.Generic;
using System.Collections.Immutable;

namespace Pine.Core.DotNet;


using LocalDeclarations = ImmutableDictionary<StaticExpression<DeclQualifiedName>, (string identifier, LocalType ltype)>;


/// <summary>
/// Per-function emission environment used by the C# backend when lowering static expressions.
/// Carries available function/value declarations, the Roslyn syntax context, and hooks to
/// map environment-path parameters and to override emission for specific subexpressions.
/// </summary>
/// <param name="AvailableFunctions">
/// Map from declaration name to its <see cref="StaticFunctionInterface"/>. Used to render call sites and
/// reconstruct ordered argument expressions.
/// </param>
/// <param name="AvailableValueDeclarations">
/// Map from literal <see cref="PineValue"/> to a reusable declaration name, allowing the emitter to reference
/// a shared field instead of re-encoding a literal value.
/// </param>
/// <param name="DeclarationSyntaxContext">Ambient Roslyn syntax context (usings, namespace) for generated code.</param>
/// <param name="SelfFunctionInterface">
/// Maps a path inside the environment (e.g., <c>[0,1]</c>) to an <see cref="ExpressionSyntax"/> that references the
/// corresponding parameter/local in the current function (e.g., <c>param_0_1</c> or a builder local).
/// </param>
/// <param name="GeneralOverride">
/// Optional hook to replace emission for specific nodes with specialized C# (e.g., builder fast-paths).
/// Returning <c>null</c> leaves default emission in place.
/// </param>
public sealed record FunctionEmitEnv(
       IReadOnlyDictionary<DeclQualifiedName, StaticFunctionInterface> AvailableFunctions,
       IReadOnlyDictionary<PineValue, DeclQualifiedName> AvailableValueDeclarations,
       DeclarationSyntaxContext DeclarationSyntaxContext,
       System.Func<IReadOnlyList<int>, ExpressionSyntax?> SelfFunctionInterface,
       System.Func<StaticExpression<DeclQualifiedName>, CompiledCSharpExpression?> GeneralOverride);

/// <summary>
/// Per-expression emission environment derived from a <see cref="FunctionEmitEnv"/>,
/// extended with the set of already declared locals used for common-subexpression elimination
/// and specialized builder locals.
/// </summary>
/// <param name="FunctionEnv">The enclosing per-function emission environment.</param>
/// <param name="AlreadyDeclared">
/// Map from static subexpressions to the backing local identifier and its kind. Enables reusing computed
/// values (or evaluating builder locals) during emission to avoid duplication.
/// </param>
public readonly record struct ExpressionEmitEnv(
    FunctionEmitEnv FunctionEnv,
    LocalDeclarations AlreadyDeclared)
{
    /// <summary>
    /// Returns a new <see cref="ExpressionEmitEnv"/> with <paramref name="toAdd"/> merged into <see cref="AlreadyDeclared"/>.
    /// </summary>
    /// <param name="toAdd">Newly introduced local declarations to add to the environment.</param>
    public ExpressionEmitEnv AddAlreadyDeclared(
        LocalDeclarations toAdd) =>
        this with
        {
            AlreadyDeclared = AlreadyDeclared.SetItems(toAdd)
        };
}


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
