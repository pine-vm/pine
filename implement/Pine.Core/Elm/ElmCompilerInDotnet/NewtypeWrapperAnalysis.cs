using Pine.Core.CodeAnalysis;
using System.Collections.Immutable;

using ModuleName = System.Collections.Generic.IReadOnlyList<string>;
using SyntaxTypes = Pine.Core.Elm.ElmSyntax.Stil4mElmSyntax7;

namespace Pine.Core.Elm.ElmCompilerInDotnet;

/// <summary>
/// Pure analysis primitives supporting the second step of solution
/// <em>D2 — Whole-program monomorphization of function-typed parameters</em>:
/// stripping the newtype-shaped wrapper from the root of every function
/// whose body wraps an inner expression with a 1-variant 1-argument
/// constructor (e.g. <c>type Parser a = Parser (State -&gt; PStep a)</c>).
/// <para>
/// This module intentionally contains <em>only</em> pure analysis
/// helpers — no rewriting, no pipeline integration. The actual rewriter
/// (Step B) and the integration with
/// <see cref="ElmSyntaxOptimization.SpecializeAndInlineDeclarations"/> (Step C) build on these
/// primitives in subsequent commits.
/// </para>
/// <para>
/// See <c>explore/internal-analysis/2026-05-10-compiled-expressions-diff-for-parse-file-scenarios.md</c>
/// section "D2 — Step 2 plan: stripping the newtype-wrapper from function-return roots"
/// for the design rationale.
/// </para>
/// </summary>
internal static class NewtypeWrapperAnalysis
{
    /// <summary>
    /// Information about a custom type that is "newtype-shaped" — exactly
    /// one constructor, exactly one constructor argument. The single
    /// inner type can be any type annotation (function arrow, record,
    /// concrete reference, …); the stripping rewrite is uniform either
    /// way, but the optimization pay-off is largest when the inner type
    /// is a function arrow.
    /// </summary>
    /// <param name="TypeName">Qualified name of the type itself (module + type name).</param>
    /// <param name="ConstructorName">Qualified name of the (sole) data constructor.</param>
    /// <param name="InnerType">
    /// Type annotation of the constructor's single field — what the
    /// stripped sibling's body has as its return type.
    /// </param>
    public sealed record NewtypeShapeInfo(
        DeclQualifiedName TypeName,
        DeclQualifiedName ConstructorName,
        SyntaxTypes.TypeAnnotation InnerType);

    /// <summary>
    /// Returns true iff the supplied <paramref name="typeStruct"/> is
    /// "newtype-shaped" — exactly one constructor with exactly one
    /// field. When true, <paramref name="constructorName"/> is set to
    /// the constructor's bare name (no module qualification) and
    /// <paramref name="innerType"/> to its single field's type
    /// annotation.
    /// </summary>
    public static bool IsNewtypeShapedTypeStruct(
        SyntaxTypes.TypeStruct typeStruct,
        out string? constructorName,
        out SyntaxTypes.TypeAnnotation? innerType)
    {
        if (typeStruct.Constructors.Count is not 1)
        {
            constructorName = null;
            innerType = null;
            return false;
        }

        var ctorNode = typeStruct.Constructors[0];
        var ctor = ctorNode.Value;

        if (ctor.Arguments.Count is not 1)
        {
            constructorName = null;
            innerType = null;
            return false;
        }

        constructorName = ctor.Name.Value;
        innerType = ctor.Arguments[0].Value;
        return true;
    }

    /// <summary>
    /// Scans every <see cref="SyntaxTypes.Declaration.CustomTypeDeclaration"/>
    /// in <paramref name="declarations"/> and builds a registry mapping
    /// <em>both</em> the type's qualified name and the constructor's
    /// qualified name to the same <see cref="NewtypeShapeInfo"/>. Both
    /// directions are populated because Step B's call-site rewriter
    /// needs constructor-name lookups while Step A's body-shape
    /// matcher works with type-name lookups.
    /// </summary>
    public static ImmutableDictionary<DeclQualifiedName, NewtypeShapeInfo>
        BuildNewtypeRegistry(
        ImmutableDictionary<DeclQualifiedName, SyntaxTypes.Declaration> declarations)
    {
        var builder = ImmutableDictionary.CreateBuilder<DeclQualifiedName, NewtypeShapeInfo>();

        foreach (var (declName, decl) in declarations)
        {
            if (decl is not SyntaxTypes.Declaration.CustomTypeDeclaration ctd)
                continue;

            if (!IsNewtypeShapedTypeStruct(
                    ctd.TypeDeclaration,
                    out var ctorBareName,
                    out var inner))
            {
                continue;
            }

            // Use the type's own module path for the constructor (Elm
            // constructors live in the same module as their type).
            var typeName =
                new DeclQualifiedName(
                    declName.Namespaces,
                    ctd.TypeDeclaration.Name.Value);

            var ctorName =
                new DeclQualifiedName(
                    declName.Namespaces,
                    ctorBareName!);

            var info = new NewtypeShapeInfo(typeName, ctorName, inner!);

            // Index under both the type and constructor names for
            // O(1) lookup from either direction.
            builder[typeName] = info;
            builder[ctorName] = info;
        }

        return builder.ToImmutable();
    }

    /// <summary>
    /// <see cref="OptimizedElmSyntaxDeclarations"/>-flavoured overload of
    /// <see cref="BuildNewtypeRegistry(ImmutableDictionary{DeclQualifiedName, SyntaxTypes.Declaration})"/>.
    /// </summary>
    public static ImmutableDictionary<DeclQualifiedName, NewtypeShapeInfo>
        BuildNewtypeRegistry(
        OptimizedElmSyntaxDeclarations declarations) =>
        BuildNewtypeRegistry(declarations.RenderAsFlatDictionary());

    /// <summary>
    /// Result of matching a function body against the wrapper-return
    /// shape. Records the qualified name of the matched constructor
    /// plus the inner expression that the stripped sibling decl would
    /// use as its body.
    /// </summary>
    /// <param name="ConstructorName">
    /// Qualified name of the wrapper constructor that the original
    /// body applies as its outermost form.
    /// </param>
    /// <param name="StrippedInnerExpression">
    /// The expression that the stripped sibling would have as its
    /// body — i.e. the body with the outermost
    /// <c>Application[FunctionOrValue(WrapperCtor), …]</c> peeled off.
    /// </param>
    public sealed record WrapperReturnMatch(
        DeclQualifiedName ConstructorName,
        SyntaxTypes.Expression StrippedInnerExpression);

    /// <summary>
    /// Pure body-shape matcher. Returns a non-null
    /// <see cref="WrapperReturnMatch"/> iff <paramref name="body"/>
    /// (after peeling outer
    /// <see cref="SyntaxTypes.Expression.ParenthesizedExpression"/>
    /// layers) is exactly an <see cref="SyntaxTypes.Expression.Application"/>
    /// whose head is a <see cref="SyntaxTypes.Expression.FunctionOrValue"/>
    /// referring to one of the registered newtype constructors and
    /// whose argument list has exactly one element.
    /// <para>
    /// The matcher does NOT recurse into <c>Let</c> / <c>CaseExpression</c>
    /// arms — those re-threading variants are handled by Step B
    /// (sibling generation) where they need to produce a different
    /// stripped body shape per arm.
    /// </para>
    /// </summary>
    public static WrapperReturnMatch? TryMatchWrapperReturnBody(
        SyntaxTypes.Expression body,
        ImmutableDictionary<DeclQualifiedName, NewtypeShapeInfo> registry,
        ModuleName currentModuleName)
    {
        var peeled = SyntaxTypes.SyntaxAnalysis.UnwrapParenthesized(body);

        if (peeled is not SyntaxTypes.Expression.Application app)
            return null;

        if (app.Arguments.Count is not 2)
            return null;

        var headPeeled = SyntaxTypes.SyntaxAnalysis.UnwrapParenthesized(app.Arguments[0].Value);

        if (headPeeled is not SyntaxTypes.Expression.FunctionOrValue head)
            return null;

        // Resolve qualified name. An unqualified reference inside the
        // declaring module is allowed.
        var qualifiedName = ElmSyntaxTransformations.ResolveReference(head, currentModuleName);

        if (!registry.TryGetValue(qualifiedName, out var info))
            return null;

        // The matched entry MUST be the constructor (not the type) —
        // both share the registry but only one matches the bare name.
        if (!info.ConstructorName.Equals(qualifiedName))
            return null;

        var innerExpr = SyntaxTypes.SyntaxAnalysis.UnwrapParenthesized(app.Arguments[1].Value);

        return new WrapperReturnMatch(info.ConstructorName, innerExpr);
    }
}
