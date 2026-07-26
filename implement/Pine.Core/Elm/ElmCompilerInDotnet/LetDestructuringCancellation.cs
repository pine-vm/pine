using Pine.Core.CodeAnalysis;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;

using SyntaxTypes = Pine.Core.Elm.ElmSyntax.ElmSyntaxAbstract;

namespace Pine.Core.Elm.ElmCompilerInDotnet;

/// <summary>
/// Implementation of the "Locally Cancellable Let Destructuring" rewrite
/// described in <c>explore/internal-analysis/2026-05-20-elm-syntax-case-block-consolidation.md</c>
/// §3.
///
/// <para>
/// After some of the syntax transformations in the optimization pipeline,
/// the generated Elm syntax often contains let-destructure shapes where
/// the outermost constructor on the right-hand side matches the
/// outermost constructor of the destructure pattern. For example:
/// </para>
///
/// <code>
/// let
///     (Maybe.Just (ParserFast.Parser parse)) =
///         Maybe.Just Elm.Parser.Layout.fromSingleLineCommentNode
/// in
/// body
/// </code>
///
/// <para>
/// The outermost <c>Maybe.Just</c> tag pairs with itself and can be
/// cancelled with only local information, yielding the equivalent:
/// </para>
///
/// <code>
/// let
///     (ParserFast.Parser parse) =
///         Elm.Parser.Layout.fromSingleLineCommentNode
/// in
/// body
/// </code>
///
/// <para>
/// The peephole only fires when:
/// </para>
/// <list type="bullet">
///   <item>The destructure pattern is a
///   <see cref="SyntaxTypes.Pattern.NamedPattern"/> with no
///   <see cref="SyntaxTypes.Pattern.AsPattern"/> alias at the level
///   that would be removed (otherwise the alias binding would silently
///   disappear).</item>
///   <item>The RHS expression deconstructs as a constructor
///   application via
///   <see cref="ElmSyntaxAbstractTransformations.TryDeconstructConstructorApplication(SyntaxTypes.Expression)"/>.</item>
///   <item>The constructor names are equivalent
///   (<see cref="ElmSyntaxAbstractTransformations.AreEquivalentConstructorNames(SyntaxTypes.QualifiedNameRef, DeclQualifiedName)"/>)
///   and the argument arities match.</item>
/// </list>
///
/// <para>
/// On a successful match, the single <see cref="SyntaxTypes.LetDeclaration.LetDestructuring"/>
/// declaration is replaced by N sibling
/// <see cref="SyntaxTypes.LetDeclaration.LetDestructuring"/>
/// declarations — one per constructor argument — preserving any names
/// bound by the inner sub-patterns. Zero-arity constructors (e.g.
/// <c>Nothing = Nothing</c>) elide the binding entirely. The
/// transformation is bottom-up so cascading cancellations are handled in
/// a single pass.
/// </para>
/// </summary>
internal static class LetDestructuringCancellation
{
    /// <summary>
    /// <see cref="OptimizedElmSyntaxDeclarations"/>-flavoured overload of
    /// <see cref="RewriteDeclarationDictionary(ImmutableDictionary{DeclQualifiedName, SyntaxTypes.Declaration})"/>.
    /// </summary>
    public static OptimizedElmSyntaxDeclarations RewriteDeclarationDictionary(
        OptimizedElmSyntaxDeclarations declarations) =>
        OptimizedElmSyntaxDeclarations.FromFlatDictionary(
            RewriteDeclarationDictionary(declarations.RenderAsFlatDictionary()));

    /// <summary>
    /// Walks every function declaration's body bottom-up and applies the
    /// <see cref="TryCancelLocal"/> peephole at every
    /// <see cref="SyntaxTypes.Expression.LetExpression"/> site.
    /// Non-function declarations pass through unchanged.
    /// </summary>
    public static ImmutableDictionary<DeclQualifiedName, SyntaxTypes.Declaration> RewriteDeclarationDictionary(
        ImmutableDictionary<DeclQualifiedName, SyntaxTypes.Declaration> declarations)
    {
        var builder = ImmutableDictionary.CreateBuilder<DeclQualifiedName, SyntaxTypes.Declaration>();

        foreach (var (declName, decl) in declarations)
        {
            builder[declName] = RewriteDeclaration(decl);
        }

        return builder.ToImmutable();
    }

    private static SyntaxTypes.Declaration RewriteDeclaration(
        SyntaxTypes.Declaration decl)
    {
        switch (decl)
        {
            case SyntaxTypes.Declaration.FunctionDeclaration funcDecl:
                {
                    var impl = funcDecl.Function.Declaration;

                    var newBody = RewriteExpression(impl.Expression);

                    if (ReferenceEquals(newBody, impl.Expression))
                        return decl;

                    var newImpl = impl with { Expression = newBody };

                    var newFunc = funcDecl.Function with { Declaration = newImpl };

                    return new SyntaxTypes.Declaration.FunctionDeclaration(newFunc);
                }

            case SyntaxTypes.Declaration.ChoiceTypeDeclaration:
            case SyntaxTypes.Declaration.AliasDeclaration:
            case SyntaxTypes.Declaration.PortDeclaration:
            case SyntaxTypes.Declaration.InfixDeclaration:
                return decl;

            default:
                throw new NotImplementedException(
                    "LetDestructuringCancellation.RewriteDeclaration does not handle declaration variant: " +
                    decl.GetType().Name);
        }
    }

    /// <summary>
    /// Bottom-up rewrite over an expression tree. Recurses into all
    /// children first via
    /// <see cref="ElmSyntaxAbstractTransformations.MapChildExpressions"/>, then
    /// applies <see cref="TryCancelLocal"/> at the current node. If the
    /// local rewrite fires, the result is re-rewritten to allow
    /// cascading cancellations (e.g. when peeling exposes another
    /// tag-on-tag match).
    /// </summary>
    public static SyntaxTypes.Expression RewriteExpression(
        SyntaxTypes.Expression expr)
    {
        var anyChildChanged = false;

        SyntaxTypes.Expression RecurseChild(SyntaxTypes.Expression child)
        {
            var rewrittenChild = RewriteExpression(child);

            if (ReferenceEquals(rewrittenChild, child))
                return child;

            anyChildChanged = true;
            return rewrittenChild;
        }

        var withChildrenRewritten = ElmSyntaxAbstractTransformations.MapChildExpressions(expr, RecurseChild);

        var afterChildRecursion = anyChildChanged ? withChildrenRewritten : expr;

        var local = TryCancelLocal(afterChildRecursion);

        if (local is not null)
        {
            return RewriteExpression(local);
        }

        return afterChildRecursion;
    }

    /// <summary>
    /// Pure local peephole: returns a non-null rewritten
    /// <see cref="SyntaxTypes.Expression.LetExpression"/> iff at least
    /// one of its <see cref="SyntaxTypes.LetDeclaration.LetDestructuring"/>
    /// declarations matches the cancellation shape. Does NOT recurse
    /// into children — call <see cref="RewriteExpression"/> for the
    /// full bottom-up walk.
    /// </summary>
    public static SyntaxTypes.Expression? TryCancelLocal(
        SyntaxTypes.Expression expr)
    {
        if (expr is not SyntaxTypes.Expression.LetExpression letExpr)
            return null;

        var originalDecls = letExpr.Declarations;

        var newDecls = new List<SyntaxTypes.LetDeclaration>(capacity: originalDecls.Count);

        var anyChanged = false;

        for (var i = 0; i < originalDecls.Count; i++)
        {
            var declaration = originalDecls[i];

            if (declaration is not SyntaxTypes.LetDeclaration.LetDestructuring letDestr)
            {
                newDecls.Add(declaration);
                continue;
            }

            var cancelled = TryCancelLetDestructuring(letDestr);

            if (cancelled is null)
            {
                newDecls.Add(declaration);
                continue;
            }

            anyChanged = true;
            newDecls.AddRange(cancelled);
        }

        if (!anyChanged)
            return null;

        // If every destructure cancelled to a zero-arity elision, the
        // let block may now be empty — drop it and return the body
        // directly. Otherwise re-emit the LetExpression with the
        // rewritten declaration list.
        if (newDecls.Count is 0)
            return letExpr.Expression;

        return new SyntaxTypes.Expression.LetExpression(newDecls, letExpr.Expression);
    }

    /// <summary>
    /// Attempts to cancel a single
    /// <see cref="SyntaxTypes.LetDeclaration.LetDestructuring"/>.
    /// Returns the replacement declarations (possibly empty for a
    /// zero-arity elision, possibly N declarations for N-arg
    /// cancellation) on success; <see langword="null"/> when no
    /// cancellation applies.
    /// </summary>
    private static IReadOnlyList<SyntaxTypes.LetDeclaration>? TryCancelLetDestructuring(
        SyntaxTypes.LetDeclaration.LetDestructuring letDestr)
    {
        // An AsPattern at this level would bind an alias name that we
        // cannot drop, so we refuse to cancel in that case (the alias
        // would silently disappear).
        var topPattern = letDestr.Pattern;

        if (topPattern is SyntaxTypes.Pattern.AsPattern)
            return null;

        if (topPattern is not SyntaxTypes.Pattern.NamedPattern namedPattern)
            return null;

        var ctorApp =
            ElmSyntaxAbstractTransformations.TryDeconstructConstructorApplication(letDestr.Expression);

        if (ctorApp is null)
            return null;

        if (!ElmSyntaxAbstractTransformations.AreEquivalentConstructorNames(namedPattern.Name, ctorApp.ConstructorName))
            return null;

        if (namedPattern.Arguments.Count != ctorApp.FieldExpressions.Count)
            return null;

        // Cancellation applies. Emit N sibling LetDestructuring
        // declarations — one per constructor argument. Skip
        // AllPattern arguments (which bind no names).
        var result = new List<SyntaxTypes.LetDeclaration>(capacity: namedPattern.Arguments.Count);

        for (var i = 0; i < namedPattern.Arguments.Count; i++)
        {
            var argPattern = namedPattern.Arguments[i];
            var argExpr = ctorApp.FieldExpressions[i];

            // A wildcard pattern binds nothing — elide the let entirely.
            if (argPattern is SyntaxTypes.Pattern.AllPattern)
                continue;

            result.Add(
                new SyntaxTypes.LetDeclaration.LetDestructuring(
                    Pattern: argPattern,
                    Expression: argExpr));
        }

        return result;
    }
}
