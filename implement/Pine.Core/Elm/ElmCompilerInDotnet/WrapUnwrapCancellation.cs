using Pine.Core.CodeAnalysis;
using Pine.Core.Elm.ElmSyntax.SyntaxModel;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;

using ModuleName = System.Collections.Generic.IReadOnlyList<string>;
using SyntaxTypes = Pine.Core.Elm.ElmSyntax.Stil4mElmSyntax7;

// Alias to avoid ambiguity with System.Range
using Range = Pine.Core.Elm.ElmSyntax.SyntaxModel.Range;

namespace Pine.Core.Elm.ElmCompilerInDotnet;

/// <summary>
/// Implementation of solution
/// <em>S1 — Cancel <c>Parser</c>-newtype wrap/unwrap at fully-saturated
/// call sites</em>. Pure peephole rewrite that walks every expression
/// bottom-up and cancels adjacent wrap/unwrap pairs of newtype-shaped
/// (single-ctor, single-arg) constructors.
/// <para>
/// Shapes handled:
/// </para>
/// <list type="bullet">
///   <item><description>
///     <b>Shape A — let-destructure of a literal wrap.</b>
///     <c>let (Wrap p) = Wrap inner in body[p]</c> →
///     <c>let p = inner in body[p]</c>.
///     Multi-decl let-blocks have only the matching destructurings
///     rewritten; other bindings are preserved.
///   </description></item>
///   <item><description>
///     <b>Shape A' — let-destructure of a sibling call.</b>
///     <c>let (Wrap p) = f args in body[p]</c> where <c>f</c> has a
///     <see cref="WrapperReturnStripping.WrapperStripPlan"/> with
///     <see cref="SiblingResultTransform.WrapWithConstructor"/> over
///     the same <c>Wrap</c> →
///     <c>let p = f__stripped args in body[p]</c>.
///     Driven by the
///     <see cref="GeneratedSiblingDecl"/> metadata model.
///   </description></item>
///   <item><description>
///     <b>Shape B — case-of single arm on a literal wrap.</b>
///     <c>case Wrap inner of Wrap p -&gt; body[p]</c> →
///     <c>let p = inner in body[p]</c>.
///   </description></item>
///   <item><description>
///     <b>Shape B' — case-of single arm on a sibling call.</b>
///     <c>case f args of Wrap p -&gt; body[p]</c> →
///     <c>let p = f__stripped args in body[p]</c>.
///   </description></item>
/// </list>
/// <para>
/// Shape D (function-arg destructure pattern) is deliberately
/// out-of-scope here; see
/// <c>explore/internal-analysis/2026-05-15-s1-cancel-parser-newtype-wrap-unwrap.md</c>
/// section 11 (follow-up items).
/// </para>
/// <para>
/// The pass is idempotent: a second invocation against an already-rewritten
/// dictionary returns it unchanged. Bottom-up structural recursion plus a
/// fixed-point inner loop handles cascading cancellations (e.g. a
/// cancellation that exposes another cancellable pair).
/// </para>
/// </summary>
internal static class WrapUnwrapCancellation
{
    private static readonly Location s_zeroLoc = new(Row: 0, Column: 0);

    private static readonly Range s_zeroRange = new(Start: s_zeroLoc, End: s_zeroLoc);

    /// <summary>
    /// Top-level orchestrator (literal-cancellation only): walks every
    /// declaration and applies Shapes A + B (literal-wrap cancellation
    /// against the constructor pattern). Safe to invoke stand-alone —
    /// emits no references to siblings that may not exist.
    /// <para>
    /// To also enable Shapes A' / B' (sibling-aware cancellation that
    /// rewrites <c>let (Wrap p) = f args in body</c> to
    /// <c>let p = f__stripped args in body</c>), use the overload
    /// <see cref="RewriteDeclarationDictionary(ImmutableDictionary{DeclQualifiedName, SyntaxTypes.Declaration}, ImmutableDictionary{DeclQualifiedName, GeneratedSiblingDecl})"/>
    /// and supply a sibling registry whose
    /// <see cref="GeneratedSiblingDecl.SiblingDeclName"/>s are
    /// guaranteed to resolve in the output dictionary.
    /// </para>
    /// </summary>
    public static ImmutableDictionary<DeclQualifiedName, SyntaxTypes.Declaration>
        RewriteDeclarationDictionary(
        ImmutableDictionary<DeclQualifiedName, SyntaxTypes.Declaration> declarations) =>
        RewriteDeclarationDictionary(
            declarations,
            siblingsByOriginal: []);

    /// <summary>
    /// Top-level orchestrator with explicit sibling-decl registry —
    /// enables Shapes A' / B' (sibling-aware cancellation) in addition
    /// to the always-on Shapes A / B (literal-wrap cancellation).
    /// <para>
    /// The caller is responsible for ensuring each
    /// <see cref="GeneratedSiblingDecl.SiblingDeclName"/> in
    /// <paramref name="siblingsByOriginal"/> resolves to a declaration
    /// that is reachable from the entry points after this pass — either
    /// because it already exists in <paramref name="declarations"/> or
    /// because a downstream pass will emit it.
    /// </para>
    /// </summary>
    /// <summary>
    /// <see cref="OptimizedElmSyntaxDeclarations"/>-flavoured overload of
    /// <see cref="RewriteDeclarationDictionary(ImmutableDictionary{DeclQualifiedName, SyntaxTypes.Declaration}, ImmutableDictionary{DeclQualifiedName, GeneratedSiblingDecl})"/>.
    /// </summary>
    public static OptimizedElmSyntaxDeclarations
        RewriteDeclarationDictionary(
        OptimizedElmSyntaxDeclarations declarations,
        ImmutableDictionary<DeclQualifiedName, GeneratedSiblingDecl> siblingsByOriginal) =>
        OptimizedElmSyntaxDeclarations.FromFlatDictionary(
            RewriteDeclarationDictionary(declarations.RenderAsFlatDictionary(), siblingsByOriginal));

    /// <summary>
    /// <see cref="OptimizedElmSyntaxDeclarations"/>-flavoured overload of
    /// <see cref="RewriteDeclarationDictionary(ImmutableDictionary{DeclQualifiedName, SyntaxTypes.Declaration})"/>
    /// — literal-cancellation only (no sibling-aware shapes). Callers do
    /// not need to flatten and re-lift the structured declaration model.
    /// </summary>
    public static OptimizedElmSyntaxDeclarations
        RewriteDeclarationDictionary(
        OptimizedElmSyntaxDeclarations declarations) =>
        RewriteDeclarationDictionary(
            declarations,
            siblingsByOriginal: []);

    public static ImmutableDictionary<DeclQualifiedName, SyntaxTypes.Declaration>
        RewriteDeclarationDictionary(
        ImmutableDictionary<DeclQualifiedName, SyntaxTypes.Declaration> declarations,
        ImmutableDictionary<DeclQualifiedName, GeneratedSiblingDecl> siblingsByOriginal)
    {
        var registry = NewtypeWrapperAnalysis.BuildNewtypeRegistry(declarations);

        if (registry.IsEmpty)
            return declarations;

        var builder = ImmutableDictionary.CreateBuilder<DeclQualifiedName, SyntaxTypes.Declaration>();

        foreach (var (declName, decl) in declarations)
        {
            builder[declName] =
                RewriteDeclaration(decl, registry, siblingsByOriginal, declName.Namespaces);
        }

        return builder.ToImmutable();
    }

    /// <summary>
    /// Rewrites the expression body of a single declaration.
    /// Non-function declarations (custom types, aliases, ports, infixes)
    /// pass through unchanged.
    /// </summary>
    private static SyntaxTypes.Declaration RewriteDeclaration(
        SyntaxTypes.Declaration decl,
        ImmutableDictionary<DeclQualifiedName, NewtypeWrapperAnalysis.NewtypeShapeInfo> registry,
        ImmutableDictionary<DeclQualifiedName, GeneratedSiblingDecl> siblingsByOriginal,
        ModuleName currentModuleName)
    {
        switch (decl)
        {
            case SyntaxTypes.Declaration.FunctionDeclaration funcDecl:
                {
                    var impl = funcDecl.Function.Declaration.Value;

                    var newBody =
                        RewriteExpression(impl.Expression.Value, registry, siblingsByOriginal, currentModuleName);

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
                    "WrapUnwrapCancellation.RewriteDeclaration does not handle declaration variant: " +
                    decl.GetType().Name);
        }
    }

    /// <summary>
    /// Bottom-up rewrite over an expression tree. Recurses into all
    /// children first via <see cref="ElmSyntaxTransformations.MapChildExpressions"/>,
    /// then applies a local peephole rewrite to the (possibly child-modified)
    /// expression. If the local rewrite fires, the result is re-rewritten
    /// to allow cascading cancellations (e.g. removing one wrap exposes
    /// an adjacent unwrap).
    /// </summary>
    public static SyntaxTypes.Expression RewriteExpression(
        SyntaxTypes.Expression expr,
        ImmutableDictionary<DeclQualifiedName, NewtypeWrapperAnalysis.NewtypeShapeInfo> registry,
        ImmutableDictionary<DeclQualifiedName, GeneratedSiblingDecl> siblingsByOriginal,
        ModuleName currentModuleName)
    {
        // Track whether any nested rewrite changed the tree. We only
        // hand back a freshly-built expression when something actually
        // changed — otherwise return the original `expr` reference
        // unchanged. This keeps the per-declaration reference-equality
        // short-circuit in <see cref="RewriteDeclaration"/> effective
        // and matches the convention used by similar passes (notably
        // <see cref="Inlining"/> helpers that walk every body).
        var anyChildChanged = false;

        Node<SyntaxTypes.Expression> RecurseNode(Node<SyntaxTypes.Expression> child)
        {
            var rewrittenChild =
                RewriteExpression(child.Value, registry, siblingsByOriginal, currentModuleName);

            if (ReferenceEquals(rewrittenChild, child.Value))
                return child;

            anyChildChanged = true;
            return new Node<SyntaxTypes.Expression>(child.Range, rewrittenChild);
        }

        var withChildrenRewritten = ElmSyntaxTransformations.MapChildExpressions(expr, RecurseNode);

        // Use the original `expr` reference when no child changed —
        // <see cref="ElmSyntaxTransformations.MapChildExpressions"/>
        // always allocates a fresh composite node even when the recursive
        // walk produced the same children.
        var afterChildRecursion = anyChildChanged ? withChildrenRewritten : expr;

        var local =
            TryCancelLocalWrapUnwrap(afterChildRecursion, registry, siblingsByOriginal, currentModuleName);

        if (local is not null)
        {
            // Re-run on the rewritten form to allow chained cancellations
            // (the rewrite may have introduced a new shape that itself
            // matches another peephole).
            return RewriteExpression(local, registry, siblingsByOriginal, currentModuleName);
        }

        return afterChildRecursion;
    }

    /// <summary>
    /// Pure local peephole: returns a non-null rewritten expression iff
    /// <paramref name="expr"/> is one of Shapes A / A' / B / B' above.
    /// Returns null otherwise. Does NOT recurse into children — call
    /// <see cref="RewriteExpression"/> for the full bottom-up walk.
    /// </summary>
    public static SyntaxTypes.Expression? TryCancelLocalWrapUnwrap(
        SyntaxTypes.Expression expr,
        ImmutableDictionary<DeclQualifiedName, NewtypeWrapperAnalysis.NewtypeShapeInfo> registry,
        ImmutableDictionary<DeclQualifiedName, GeneratedSiblingDecl> siblingsByOriginal,
        ModuleName currentModuleName)
    {
        return expr switch
        {
            SyntaxTypes.Expression.LetExpression letExpr =>
            TryCancelInLet(letExpr, registry, siblingsByOriginal, currentModuleName),

            SyntaxTypes.Expression.CaseExpression caseExpr =>
            TryCancelCaseOf(caseExpr, registry, siblingsByOriginal, currentModuleName),

            _ =>
            null,
        };
    }

    // ------------------------------------------------------------------
    // Shape A / A' — let-destructure adjacent to a wrap (literal or via
    // a sibling call's WrapWithConstructor result transform).
    // ------------------------------------------------------------------

    private static SyntaxTypes.Expression? TryCancelInLet(
        SyntaxTypes.Expression.LetExpression letExpr,
        ImmutableDictionary<DeclQualifiedName, NewtypeWrapperAnalysis.NewtypeShapeInfo> registry,
        ImmutableDictionary<DeclQualifiedName, GeneratedSiblingDecl> siblingsByOriginal,
        ModuleName currentModuleName)
    {
        var letBlock = letExpr.Value;

        var newDeclarations =
            new List<Node<SyntaxTypes.Expression.LetDeclaration>>(letBlock.Declarations.Count);

        var cancelledIndices = new HashSet<int>();

        var anyChanged = false;

        for (var i = 0; i < letBlock.Declarations.Count; i++)
        {
            var declNode = letBlock.Declarations[i];

            if (declNode.Value is SyntaxTypes.Expression.LetDeclaration.LetDestructuring letDestr &&
                TryCancelLetDestructuring(letDestr, registry, siblingsByOriginal, currentModuleName) is { } cancelled)
            {
                anyChanged = true;

                newDeclarations.Add(
                    new Node<SyntaxTypes.Expression.LetDeclaration>(declNode.Range, cancelled));

                cancelledIndices.Add(i);
            }
            else
            {
                newDeclarations.Add(declNode);
            }
        }

        if (!anyChanged)
            return null;

        // After the per-declaration cancellation pass, try to substitute
        // any cancelled LetDestructuring(VarPattern, RHS) bindings directly
        // into the let body (and into any remaining bindings' RHSes), eliding
        // them entirely. This matters for downstream analyses (notably the
        // OptimizationOpportunityFinder's HigherOrderParameter detector,
        // which flags let-bound names whose RHS is a function value such
        // as a qualified function reference exposed by cancellation).
        // <para>
        // Only bindings we just cancelled are considered for substitution —
        // pre-existing untouched bindings are preserved as-is to avoid
        // unrelated rewrites in callers that observe the let-block shape.
        // </para>
        // <para>
        // For each candidate we check duplication safety the same way
        // <see cref="TrySubstituteSingleVarBinding"/> does (refCount &lt;= 1)
        // unless the RHS is a qualified function reference, which is
        // always safe to substitute even if duplicated (a name reference
        // duplicates no runtime work and does not affect tail-call
        // detection).
        // </para>
        var substitutions =
            new Dictionary<string, Node<SyntaxTypes.Expression>>(StringComparer.Ordinal);

        var remainingAfterSubst =
            new List<Node<SyntaxTypes.Expression.LetDeclaration>>(newDeclarations.Count);

        for (var i = 0; i < newDeclarations.Count; i++)
        {
            var declNode = newDeclarations[i];

            if (cancelledIndices.Contains(i) &&
                declNode.Value is SyntaxTypes.Expression.LetDeclaration.LetDestructuring destr &&
                SyntaxTypes.SyntaxAnalysis.UnwrapParenthesized(destr.Pattern.Value)
                    is SyntaxTypes.Pattern.VarPattern varPat &&
                IsSafeToSubstituteEverywhere(destr.Expression, varPat.Name, letBlock, newDeclarations))
            {
                substitutions[varPat.Name] = destr.Expression;
            }
            else
            {
                remainingAfterSubst.Add(declNode);
            }
        }

        if (substitutions.Count > 0)
        {
            var bodyAfterSubst =
                ElmSyntaxTransformations.SubstituteInExpression(letBlock.Expression, substitutions);

            for (var i = 0; i < remainingAfterSubst.Count; i++)
            {
                remainingAfterSubst[i] =
                    ElmSyntaxTransformations.SubstituteInLetDeclaration(remainingAfterSubst[i], substitutions);
            }

            if (remainingAfterSubst.Count is 0)
                return bodyAfterSubst.Value;

            return
                new SyntaxTypes.Expression.LetExpression(
                    new SyntaxTypes.Expression.LetBlock(
                        Declarations: remainingAfterSubst,
                        Expression: bodyAfterSubst));
        }

        // If after rewriting the let-block has exactly one declaration
        // and it is a `VarPattern p = expr` form (the post-cancellation
        // shape), try to substitute `p` directly into the body and
        // elide the let-binding entirely. See
        // <see cref="TrySubstituteSingleVarBinding"/> for the rationale.
        if (newDeclarations.Count is 1 &&
            newDeclarations[0].Value is SyntaxTypes.Expression.LetDeclaration.LetDestructuring singleDestr &&
            TrySubstituteSingleVarBinding(singleDestr.Pattern, singleDestr.Expression, letBlock.Expression) is { } substituted)
        {
            return substituted.Value;
        }

        return
            new SyntaxTypes.Expression.LetExpression(
                new SyntaxTypes.Expression.LetBlock(
                    Declarations: newDeclarations,
                    Expression: letBlock.Expression));
    }

    /// <summary>
    /// Returns true iff substituting the binding <c>varName = rhs</c>
    /// at every occurrence in the let block (its body and the RHSes of
    /// the other declarations) is observably equivalent — either
    /// because <paramref name="rhs"/> is a qualified function reference
    /// (which duplicates no runtime work and does not affect tail-call
    /// detection) or because the unshadowed reference count of
    /// <paramref name="varName"/> across the let body and all other
    /// declarations' RHSes is at most one.
    /// </summary>
    private static bool IsSafeToSubstituteEverywhere(
        Node<SyntaxTypes.Expression> rhs,
        string varName,
        SyntaxTypes.Expression.LetBlock letBlock,
        IReadOnlyList<Node<SyntaxTypes.Expression.LetDeclaration>> currentDeclarations)
    {
        // A qualified function reference is always safe to duplicate.
        var rhsExpr = rhs.Value;

        while (rhsExpr is SyntaxTypes.Expression.ParenthesizedExpression paren)
        {
            rhsExpr = paren.Expression.Value;
        }

        if (rhsExpr is SyntaxTypes.Expression.FunctionOrValue fov && fov.ModuleName.Count > 0)
            return true;

        var totalRefs =
            ElmSyntaxTransformations.CountUnshadowedLocalVariableReferences(letBlock.Expression.Value, varName);

        if (totalRefs > 1)
            return false;

        foreach (var declNode in currentDeclarations)
        {
            // Don't count references inside the binding itself (it shadows
            // nothing relevant).
            if (declNode.Value is SyntaxTypes.Expression.LetDeclaration.LetDestructuring d &&
                SyntaxTypes.SyntaxAnalysis.UnwrapParenthesized(d.Pattern.Value)
                    is SyntaxTypes.Pattern.VarPattern vp && vp.Name == varName)
            {
                continue;
            }

            var rhsToCount =
                declNode.Value switch
                {
                    SyntaxTypes.Expression.LetDeclaration.LetDestructuring d2 => d2.Expression,
                    SyntaxTypes.Expression.LetDeclaration.LetFunction lf => lf.Function.Declaration.Value.Expression,

                    _ =>
                    null,
                };

            if (rhsToCount is not null)
            {
                totalRefs +=
                    ElmSyntaxTransformations.CountUnshadowedLocalVariableReferences(rhsToCount.Value, varName);

                if (totalRefs > 1)
                    return false;
            }
        }

        return true;
    }

    /// <summary>
    /// Tries to rewrite a single <c>let (Wrap p) = ...</c> destructuring
    /// into <c>let p = ...</c> by cancelling the wrap on the RHS against
    /// the constructor pattern on the LHS. Returns the new destructuring
    /// on success, null on no-match.
    /// </summary>
    private static SyntaxTypes.Expression.LetDeclaration.LetDestructuring? TryCancelLetDestructuring(
        SyntaxTypes.Expression.LetDeclaration.LetDestructuring letDestr,
        ImmutableDictionary<DeclQualifiedName, NewtypeWrapperAnalysis.NewtypeShapeInfo> registry,
        ImmutableDictionary<DeclQualifiedName, GeneratedSiblingDecl> siblingsByOriginal,
        ModuleName currentModuleName)
    {
        // Pattern must be NamedPattern(Wrap, [innerPattern]) for a
        // registered newtype-shaped constructor.
        var pattern = SyntaxTypes.SyntaxAnalysis.UnwrapParenthesized(letDestr.Pattern.Value);

        if (pattern is not SyntaxTypes.Pattern.NamedPattern namedPat)
            return null;

        if (namedPat.Arguments.Count is not 1)
            return null;

        var ctorQName = ElmSyntaxTransformations.ResolveReference(namedPat.Name, currentModuleName);

        if (!registry.TryGetValue(ctorQName, out var ctorInfo) || !ctorInfo.ConstructorName.Equals(ctorQName))
            return null;

        // Try to obtain a wrap-rooted form for the RHS — either a
        // literal `Wrap inner` (Shape A) or a sibling-aware
        // `Sibling args` rewrite of a `f args` call (Shape A').
        var wrapped =
            TryGetWrappedInnerExpression(
                letDestr.Expression,
                ctorQName,
                siblingsByOriginal,
                currentModuleName);

        if (wrapped is null)
            return null;

        return
            new SyntaxTypes.Expression.LetDeclaration.LetDestructuring(
                Pattern: namedPat.Arguments[0],
                Expression: wrapped);
    }

    // ------------------------------------------------------------------
    // Shape B / B' — case-of single arm on a wrap (literal or via
    // a sibling call's WrapWithConstructor result transform).
    // ------------------------------------------------------------------

    private static SyntaxTypes.Expression? TryCancelCaseOf(
        SyntaxTypes.Expression.CaseExpression caseExpr,
        ImmutableDictionary<DeclQualifiedName, NewtypeWrapperAnalysis.NewtypeShapeInfo> registry,
        ImmutableDictionary<DeclQualifiedName, GeneratedSiblingDecl> siblingsByOriginal,
        ModuleName currentModuleName)
    {
        var caseBlock = caseExpr.CaseBlock;

        if (caseBlock.Cases.Count is not 1)
            return null;

        var onlyCase = caseBlock.Cases[0];

        var pattern = SyntaxTypes.SyntaxAnalysis.UnwrapParenthesized(onlyCase.Pattern.Value);

        if (pattern is not SyntaxTypes.Pattern.NamedPattern namedPat)
            return null;

        if (namedPat.Arguments.Count is not 1)
            return null;

        var ctorQName = ElmSyntaxTransformations.ResolveReference(namedPat.Name, currentModuleName);

        if (!registry.TryGetValue(ctorQName, out var ctorInfo) || !ctorInfo.ConstructorName.Equals(ctorQName))
            return null;

        var wrapped =
            TryGetWrappedInnerExpression(
                caseBlock.Expression,
                ctorQName,
                siblingsByOriginal,
                currentModuleName);

        if (wrapped is null)
            return null;

        var innerPatternNode = namedPat.Arguments[0];

        // Fast path: when the inner pattern is a single VarPattern AND
        // the case-arm body references that name at most once, we can
        // substitute directly and elide the let-binding entirely. This
        // matters for downstream analyses (notably the
        // OptimizationOpportunityFinder's HigherOrderParameter detector,
        // which still flags let-bound names whose RHS is a function
        // value) — the let-binding form would survive as a finding even
        // though the wrap/unwrap pair is gone.
        if (TrySubstituteSingleVarBinding(innerPatternNode, wrapped, onlyCase.Expression) is { } substituted)
            return substituted.Value;

        // Fallback: emit `let innerPattern = wrapped in branchBody`.
        var letDestr =
            new SyntaxTypes.Expression.LetDeclaration.LetDestructuring(
                Pattern: innerPatternNode,
                Expression: wrapped);

        return
            new SyntaxTypes.Expression.LetExpression(
                new SyntaxTypes.Expression.LetBlock(
                    Declarations:
                    [
                    new Node<SyntaxTypes.Expression.LetDeclaration>(s_zeroRange, letDestr),
                    ],
                    Expression: onlyCase.Expression));
    }

    /// <summary>
    /// If <paramref name="patternNode"/> is a <see cref="SyntaxTypes.Pattern.VarPattern"/>
    /// and <paramref name="bodyExpr"/> references that variable at most
    /// once (counting only unshadowed occurrences), returns
    /// <paramref name="bodyExpr"/> with that variable substituted by
    /// <paramref name="rhsNode"/> directly. Returns null otherwise.
    /// <para>
    /// The single-reference duplication-safety guard mirrors the rule
    /// used by <see cref="ElmSyntaxOptimization"/>'s let-collapse helpers (see repo
    /// memory <em>inliner duplication guard</em>): substituting an
    /// expression that may contain a function application into multiple
    /// use sites would duplicate runtime work and can break the tail-call
    /// detector. Zero references is also safe — the binding becomes
    /// dead and gets discarded along with its RHS.
    /// </para>
    /// </summary>
    private static Node<SyntaxTypes.Expression>? TrySubstituteSingleVarBinding(
        Node<SyntaxTypes.Pattern> patternNode,
        Node<SyntaxTypes.Expression> rhsNode,
        Node<SyntaxTypes.Expression> bodyExpr)
    {
        var pattern = SyntaxTypes.SyntaxAnalysis.UnwrapParenthesized(patternNode.Value);

        if (pattern is not SyntaxTypes.Pattern.VarPattern varPat)
            return null;

        var refCount =
            ElmSyntaxTransformations.CountUnshadowedLocalVariableReferences(bodyExpr.Value, varPat.Name);

        if (refCount > 1)
            return null;

        if (refCount is 0)
            return bodyExpr;

        var substitutions =
            new Dictionary<string, Node<SyntaxTypes.Expression>>(StringComparer.Ordinal)
            {
                [varPat.Name] = rhsNode,
            };

        return ElmSyntaxTransformations.SubstituteInExpression(bodyExpr, substitutions);
    }

    // ------------------------------------------------------------------
    // Shared: wrap-detection that handles both literal-constructor
    // and sibling-decl forms uniformly.
    // ------------------------------------------------------------------

    /// <summary>
    /// Returns a non-null replacement node for <paramref name="rhsExpr"/>
    /// iff its outer form is a wrap on <paramref name="expectedCtor"/>
    /// — either as a literal <c>Wrap inner</c> application
    /// (Shape A / B) or as a fully-saturated call to a function whose
    /// <see cref="GeneratedSiblingDecl.ResultTransform"/> is
    /// <see cref="SiblingResultTransform.WrapWithConstructor"/> over
    /// the same constructor (Shape A' / B'). The returned node is the
    /// inner expression with the wrap removed: <c>inner</c> in the
    /// literal case, <c>Sibling args</c> in the sibling-aware case.
    /// </summary>
    private static Node<SyntaxTypes.Expression>? TryGetWrappedInnerExpression(
        Node<SyntaxTypes.Expression> rhsExpr,
        DeclQualifiedName expectedCtor,
        ImmutableDictionary<DeclQualifiedName, GeneratedSiblingDecl> siblingsByOriginal,
        ModuleName currentModuleName)
    {
        // 0. Peel a `let bindings in body` wrapping the wrap. If the
        //    inner body recursively matches as a wrap, return the same
        //    let with its body swapped to the stripped inner expression.
        //    The let-bindings remain in scope around the new body, so any
        //    names they introduce stay resolvable. See Fix A of
        //    explore/internal-analysis/2026-05-17-wrapper-then-intermediate-failing-test-analysis.md.
        var rhsValuePeeled = SyntaxTypes.SyntaxAnalysis.UnwrapParenthesized(rhsExpr.Value);

        if (rhsValuePeeled is SyntaxTypes.Expression.LetExpression outerLet)
        {
            var innerStripped =
                TryGetWrappedInnerExpression(
                    outerLet.Value.Expression,
                    expectedCtor,
                    siblingsByOriginal,
                    currentModuleName);

            if (innerStripped is not null)
            {
                var rebuiltLet =
                    new SyntaxTypes.Expression.LetExpression(
                        new SyntaxTypes.Expression.LetBlock(
                            Declarations: outerLet.Value.Declarations,
                            Expression: innerStripped));

                return new Node<SyntaxTypes.Expression>(rhsExpr.Range, rebuiltLet);
            }
        }

        // 1. Literal-constructor form: `Wrap inner` (or just `Wrap`
        //    treated as a 0-arg ctor — newtypes are 1-arg so ignore that).
        var ctorApp = ElmSyntaxTransformations.TryDeconstructConstructorApplication(rhsExpr);

        if (ctorApp is not null && ctorApp.FieldExpressions.Count is 1)
        {
            var rhsCtorQName = ElmSyntaxTransformations.ResolveReference(ctorApp.ConstructorName, currentModuleName);

            if (rhsCtorQName.Equals(expectedCtor))
                return ctorApp.FieldExpressions[0];
        }

        // 2. Sibling-aware form: `f args` where `f`'s strip plan tells
        //    us the result is `Wrap (f__stripped args)`. We rewrite
        //    this call to point at the stripped sibling directly,
        //    discarding the would-be wrap.
        var rhsPeeled = SyntaxTypes.SyntaxAnalysis.UnwrapParenthesized(rhsExpr.Value);

        if (rhsPeeled is SyntaxTypes.Expression.Application app && app.Arguments.Count >= 2)
        {
            var headPeeled = SyntaxTypes.SyntaxAnalysis.UnwrapParenthesized(app.Arguments[0].Value);

            if (headPeeled is SyntaxTypes.Expression.FunctionOrValue head)
            {
                var calleeQName = ElmSyntaxTransformations.ResolveReference(head, currentModuleName);

                if (siblingsByOriginal.TryGetValue(calleeQName, out var siblingDecl))
                {
                    var argCount = app.Arguments.Count - 1;

                    if (argCount == siblingDecl.OriginalArity &&
                        siblingDecl.ResultTransform is SiblingResultTransform.WrapWithConstructor wrap &&
                        wrap.Constructor.Equals(expectedCtor) &&
                        AllParameterOriginsAreIdentity(siblingDecl))
                    {
                        // Build: Sibling args
                        var newHead =
                            new SyntaxTypes.Expression.FunctionOrValue(
                                siblingDecl.SiblingDeclName.Namespaces,
                                siblingDecl.SiblingDeclName.DeclName);

                        var newArgs =
                            new List<Node<SyntaxTypes.Expression>>(app.Arguments.Count)
                            {
                                new(s_zeroRange, newHead),
                            };

                        for (var i = 1; i < app.Arguments.Count; i++)
                            newArgs.Add(app.Arguments[i]);

                        var siblingApp = new SyntaxTypes.Expression.Application(newArgs);

                        return new Node<SyntaxTypes.Expression>(rhsExpr.Range, siblingApp);
                    }
                }
            }
        }

        return null;
    }

    /// <summary>
    /// True iff every <see cref="SiblingParameterOrigin"/> in
    /// <paramref name="sibling"/> is
    /// <see cref="SiblingParameterOrigin.Identity"/> at the matching
    /// position. Sibling-aware cancellation only fires for identity
    /// mappings today; <see cref="SiblingParameterOrigin.InnerOfWrapper"/>
    /// is reserved for the future Shape D rewrite that needs
    /// per-argument deconstruction.
    /// </summary>
    private static bool AllParameterOriginsAreIdentity(GeneratedSiblingDecl sibling)
    {
        if (sibling.SiblingArity != sibling.OriginalArity)
            return false;

        for (var i = 0; i < sibling.ParameterOrigins.Count; i++)
        {
            if (sibling.ParameterOrigins[i] is not SiblingParameterOrigin.Identity id)
                return false;

            if (id.SiblingIndex != i || id.OriginalIndex != i)
                return false;
        }

        return true;
    }

    // ------------------------------------------------------------------
    // Reference resolution: callers use
    // ElmSyntaxTransformations.ResolveReference directly (overloads
    // exist for both Expression.FunctionOrValue and QualifiedNameRef).
    // ------------------------------------------------------------------
}
