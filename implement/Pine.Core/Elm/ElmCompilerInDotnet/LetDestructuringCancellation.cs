using Pine.Core.CodeAnalysis;
using Pine.Core.Elm.ElmSyntax.SyntaxModel;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;

using SyntaxTypes = Pine.Core.Elm.ElmSyntax.Stil4mElmSyntax7;

// Alias to avoid ambiguity with System.Range.
using Range = Pine.Core.Elm.ElmSyntax.SyntaxModel.Range;

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
///   <item>The destructure pattern peels to a
///   <see cref="SyntaxTypes.Pattern.NamedPattern"/> with no
///   <see cref="SyntaxTypes.Pattern.AsPattern"/> aliases at the level
///   that would be removed (otherwise the alias binding would silently
///   disappear).</item>
///   <item>The RHS expression deconstructs as a constructor
///   application via
///   <see cref="ElmSyntaxTransformations.TryDeconstructConstructorApplication(SyntaxTypes.Expression)"/>.</item>
///   <item>The constructor names are equivalent
///   (<see cref="ElmSyntaxTransformations.AreEquivalentConstructorNames(SyntaxTypes.QualifiedNameRef, SyntaxTypes.QualifiedNameRef)"/>)
///   and the argument arities match.</item>
/// </list>
///
/// <para>
/// On a successful match, the single <see cref="SyntaxTypes.Expression.LetDeclaration.LetDestructuring"/>
/// declaration is replaced by N sibling
/// <see cref="SyntaxTypes.Expression.LetDeclaration.LetDestructuring"/>
/// declarations — one per constructor argument — preserving any names
/// bound by the inner sub-patterns. Zero-arity constructors (e.g.
/// <c>Nothing = Nothing</c>) elide the binding entirely. The
/// transformation is bottom-up so cascading cancellations are handled in
/// a single pass.
/// </para>
/// </summary>
internal static class LetDestructuringCancellation
{
    private static readonly Location s_zeroLoc = new(Row: 0, Column: 0);

    private static readonly Range s_zeroRange = new(Start: s_zeroLoc, End: s_zeroLoc);

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
                    var impl = funcDecl.Function.Declaration.Value;

                    var newBody = RewriteExpression(impl.Expression.Value);

                    if (ReferenceEquals(newBody, impl.Expression.Value))
                        return decl;

                    var newImpl =
                        impl with
                        {
                            Expression = new Node<SyntaxTypes.Expression>(impl.Expression.Range, newBody),
                        };

                    var newFunc =
                        funcDecl.Function with
                        {
                            Declaration =
                            new Node<SyntaxTypes.FunctionImplementation>(
                                funcDecl.Function.Declaration.Range,
                                newImpl),
                        };

                    return new SyntaxTypes.Declaration.FunctionDeclaration(newFunc);
                }

            case SyntaxTypes.Declaration.CustomTypeDeclaration:
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
    /// <see cref="ElmSyntaxTransformations.MapChildExpressions"/>, then
    /// applies <see cref="TryCancelLocal"/> at the current node. If the
    /// local rewrite fires, the result is re-rewritten to allow
    /// cascading cancellations (e.g. when peeling exposes another
    /// tag-on-tag match).
    /// </summary>
    public static SyntaxTypes.Expression RewriteExpression(
        SyntaxTypes.Expression expr)
    {
        var anyChildChanged = false;

        Node<SyntaxTypes.Expression> RecurseNode(Node<SyntaxTypes.Expression> child)
        {
            var rewrittenChild = RewriteExpression(child.Value);

            if (ReferenceEquals(rewrittenChild, child.Value))
                return child;

            anyChildChanged = true;
            return new Node<SyntaxTypes.Expression>(child.Range, rewrittenChild);
        }

        var withChildrenRewritten = ElmSyntaxTransformations.MapChildExpressions(expr, RecurseNode);

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
    /// one of its <see cref="SyntaxTypes.Expression.LetDeclaration.LetDestructuring"/>
    /// declarations matches the cancellation shape. Does NOT recurse
    /// into children — call <see cref="RewriteExpression"/> for the
    /// full bottom-up walk.
    /// </summary>
    public static SyntaxTypes.Expression? TryCancelLocal(
        SyntaxTypes.Expression expr)
    {
        if (expr is not SyntaxTypes.Expression.LetExpression letExpr)
            return null;

        var originalDecls = letExpr.Value.Declarations;

        var newDecls = new List<Node<SyntaxTypes.Expression.LetDeclaration>>(capacity: originalDecls.Count);

        var anyChanged = false;

        for (var i = 0; i < originalDecls.Count; i++)
        {
            var declNode = originalDecls[i];

            if (declNode.Value is not SyntaxTypes.Expression.LetDeclaration.LetDestructuring letDestr)
            {
                newDecls.Add(declNode);
                continue;
            }

            var cancelled = TryCancelLetDestructuring(letDestr);

            if (cancelled is null)
            {
                newDecls.Add(declNode);
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
            return letExpr.Value.Expression.Value;

        return
            new SyntaxTypes.Expression.LetExpression(
                new SyntaxTypes.Expression.LetBlock(
                    Declarations: newDecls,
                    Expression: letExpr.Value.Expression));
    }

    /// <summary>
    /// Attempts to cancel a single
    /// <see cref="SyntaxTypes.Expression.LetDeclaration.LetDestructuring"/>.
    /// Returns the replacement declarations (possibly empty for a
    /// zero-arity elision, possibly N declarations for N-arg
    /// cancellation) on success; <see langword="null"/> when no
    /// cancellation applies.
    /// </summary>
    private static IReadOnlyList<Node<SyntaxTypes.Expression.LetDeclaration>>? TryCancelLetDestructuring(
        SyntaxTypes.Expression.LetDeclaration.LetDestructuring letDestr)
    {
        // The pattern at the LetDestructuring node may be wrapped in
        // ParenthesizedPattern; an AsPattern at this level would bind
        // an alias name that we cannot drop, so we refuse to cancel in
        // that case (the alias would silently disappear).
        var topPattern = SyntaxTypes.SyntaxAnalysis.UnwrapParenthesized(letDestr.Pattern.Value);

        if (topPattern is SyntaxTypes.Pattern.AsPattern)
            return null;

        if (topPattern is not SyntaxTypes.Pattern.NamedPattern namedPattern)
            return null;

        var ctorApp =
            ElmSyntaxTransformations.TryDeconstructConstructorApplication(letDestr.Expression.Value);

        if (ctorApp is null)
            return null;

        if (!ElmSyntaxTransformations.AreEquivalentConstructorNames(namedPattern.Name, ctorApp.ConstructorName))
            return null;

        if (namedPattern.Arguments.Count != ctorApp.FieldExpressions.Count)
            return null;

        // Cancellation applies. Emit N sibling LetDestructuring
        // declarations — one per constructor argument. Skip
        // AllPattern arguments (which bind no names).
        var result = new List<Node<SyntaxTypes.Expression.LetDeclaration>>(capacity: namedPattern.Arguments.Count);

        for (var i = 0; i < namedPattern.Arguments.Count; i++)
        {
            var argPattern = namedPattern.Arguments[i];
            var argExpr = ctorApp.FieldExpressions[i];

            var peeledArgPattern = SyntaxTypes.SyntaxAnalysis.UnwrapParenthesized(argPattern.Value);

            // A wildcard pattern binds nothing — elide the let entirely.
            if (peeledArgPattern is SyntaxTypes.Pattern.AllPattern)
                continue;

            var newLetDestr =
                new SyntaxTypes.Expression.LetDeclaration.LetDestructuring(
                    Pattern: argPattern,
                    Expression: argExpr);

            // Use the constructor-argument expression's range as the
            // surrounding LetDeclaration range. Using s_zeroRange here
            // confuses the snapshot formatter's SpansMultipleRows
            // heuristic (which compares row numbers across child
            // nodes), producing spurious multi-line layouts for what
            // is structurally a one-line decl. The argument expression
            // already carries an accurate parsed range.
            result.Add(
                new Node<SyntaxTypes.Expression.LetDeclaration>(argExpr.Range, newLetDestr));
        }

        return result;
    }
}
