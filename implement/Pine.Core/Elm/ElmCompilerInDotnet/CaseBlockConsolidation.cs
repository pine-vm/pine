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
/// Implementation of the case-block consolidation rewrites described in
/// <c>explore/internal-analysis/2026-05-20-elm-syntax-case-block-consolidation.md</c>.
///
/// <para>
/// Two local peephole rewrites are implemented:
/// </para>
/// <list type="number">
/// <item>
/// <b>Conditional duplication of case arm by leaf shape</b>
/// (<see cref="TryConsolidateBranchedCase"/>): when the scrutinee of a
/// <see cref="SyntaxTypes.Expression.CaseExpression"/> is an
/// <see cref="SyntaxTypes.Expression.IfBlock"/> whose every leaf produces
/// an expression with a statically-known outermost constructor tag, and
/// each tag maps uniquely to one arm of the case, the case is folded
/// down into the if-chain: each leaf is replaced by the matching arm's
/// body, with the arm's pattern destructured as a <c>let</c> binding
/// against the leaf's constructor arguments.
/// </item>
/// <item>
/// <b>Deduct tag constraint via alias pattern</b>
/// (<see cref="TryConsolidateNestedCaseOfCase"/>): when an outer case's
/// scrutinee is itself a case expression (possibly wrapped in a
/// <see cref="SyntaxTypes.Expression.LetExpression"/>), and every inner-arm
/// body has a statically-inferable tag (either directly or via an
/// <see cref="SyntaxTypes.Pattern.AsPattern"/> on the inner arm pattern),
/// and the inner-tag→outer-arm mapping is one-to-one (no duplication
/// required), the two cases are collapsed into the inner case with each
/// arm's body rewritten to its matching outer arm's body.
/// </item>
/// </list>
///
/// <para>
/// Both rewrites are equivalence-preserving local transformations. They
/// only fire when their preconditions are met and otherwise return the
/// input unchanged.
/// </para>
/// </summary>
internal static class CaseBlockConsolidation
{
    private static readonly Location s_zeroLoc = new(Row: 0, Column: 0);

    private static readonly Range s_zeroRange = new(Start: s_zeroLoc, End: s_zeroLoc);

    /// <summary>
    /// Cost model for the case-block consolidation pass. The rewrite
    /// in <see cref="TryConsolidateBranchedCase"/> can duplicate an arm
    /// body across multiple if-chain leaves, so we gate the rewrite by
    /// a node-count budget.
    /// </summary>
    public record CaseConsolidationConfig(
        int MaxArmBodyNodeCount,
        int MaxTotalDuplicatedNodes)
    {
        /// <summary>
        /// Conservative default thresholds:
        /// <list type="bullet">
        /// <item><c>MaxArmBodyNodeCount = 16</c></item>
        /// <item><c>MaxTotalDuplicatedNodes = 32</c></item>
        /// </list>
        /// </summary>
        public static readonly CaseConsolidationConfig Default =
            new(
                MaxArmBodyNodeCount: 16,
                MaxTotalDuplicatedNodes: 32);
    }

    /// <summary>
    /// <see cref="OptimizedElmSyntaxDeclarations"/>-flavoured overload of
    /// <see cref="RewriteDeclarationDictionary(ImmutableDictionary{DeclQualifiedName, SyntaxTypes.Declaration}, CaseConsolidationConfig)"/>.
    /// </summary>
    public static OptimizedElmSyntaxDeclarations RewriteDeclarationDictionary(
        OptimizedElmSyntaxDeclarations declarations,
        CaseConsolidationConfig config) =>
        OptimizedElmSyntaxDeclarations.FromFlatDictionary(
            RewriteDeclarationDictionary(declarations.RenderAsFlatDictionary(), config));

    /// <summary>
    /// Walks every function declaration's body bottom-up and applies the
    /// two local case-block-consolidation peepholes
    /// (<see cref="TryConsolidateBranchedCase"/> and
    /// <see cref="TryConsolidateNestedCaseOfCase"/>). Non-function
    /// declarations pass through unchanged. The traversal is bottom-up
    /// and re-tries the local rewrites on each freshly rewritten
    /// expression so cascading rewrites (e.g. one consolidation that
    /// exposes a second) are handled in a single pass.
    /// </summary>
    public static ImmutableDictionary<DeclQualifiedName, SyntaxTypes.Declaration> RewriteDeclarationDictionary(
        ImmutableDictionary<DeclQualifiedName, SyntaxTypes.Declaration> declarations,
        CaseConsolidationConfig config)
    {
        var builder = ImmutableDictionary.CreateBuilder<DeclQualifiedName, SyntaxTypes.Declaration>();

        foreach (var (declName, decl) in declarations)
        {
            builder[declName] = RewriteDeclaration(decl, config);
        }

        return builder.ToImmutable();
    }

    private static SyntaxTypes.Declaration RewriteDeclaration(
        SyntaxTypes.Declaration decl,
        CaseConsolidationConfig config)
    {
        switch (decl)
        {
            case SyntaxTypes.Declaration.FunctionDeclaration funcDecl:
                {
                    var impl = funcDecl.Function.Declaration.Value;

                    var newBody = RewriteExpression(impl.Expression.Value, config);

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
                    "CaseBlockConsolidation.RewriteDeclaration does not handle declaration variant: " +
                    decl.GetType().Name);
        }
    }

    /// <summary>
    /// Bottom-up rewrite over an expression tree. Recurses into all
    /// children first via
    /// <see cref="ElmSyntaxTransformations.MapChildExpressions"/>, then
    /// applies the two local peephole rewrites. If the local rewrite
    /// fires, the result is re-rewritten to allow cascading
    /// consolidations.
    /// </summary>
    public static SyntaxTypes.Expression RewriteExpression(
        SyntaxTypes.Expression expr,
        CaseConsolidationConfig config)
    {
        var anyChildChanged = false;

        Node<SyntaxTypes.Expression> RecurseNode(Node<SyntaxTypes.Expression> child)
        {
            var rewrittenChild = RewriteExpression(child.Value, config);

            if (ReferenceEquals(rewrittenChild, child.Value))
                return child;

            anyChildChanged = true;
            return new Node<SyntaxTypes.Expression>(child.Range, rewrittenChild);
        }

        var withChildrenRewritten = ElmSyntaxTransformations.MapChildExpressions(expr, RecurseNode);

        var afterChildRecursion = anyChildChanged ? withChildrenRewritten : expr;

        var local =
            TryConsolidateLocal(afterChildRecursion, config);

        if (local is not null)
        {
            return RewriteExpression(local, config);
        }

        return afterChildRecursion;
    }

    /// <summary>
    /// Pure local peephole: returns a non-null rewritten expression iff
    /// <paramref name="expr"/> matches one of the consolidation shapes.
    /// Does NOT recurse into children — call
    /// <see cref="RewriteExpression"/> for the full bottom-up walk.
    /// </summary>
    public static SyntaxTypes.Expression? TryConsolidateLocal(
        SyntaxTypes.Expression expr,
        CaseConsolidationConfig config)
    {
        if (expr is not SyntaxTypes.Expression.CaseExpression caseExpr)
            return null;

        // The two rewrites are mutually exclusive based on the scrutinee
        // shape after peeling transparent wrappers:
        // - if-rooted scrutinee → TryConsolidateBranchedCase
        // - case-rooted scrutinee → TryConsolidateNestedCaseOfCase
        var consolidated = TryConsolidateBranchedCase(caseExpr, config);

        if (consolidated is not null)
            return consolidated;

        return TryConsolidateNestedCaseOfCase(caseExpr);
    }

    // ------------------------------------------------------------------
    // Rewrite 1 — Conditional duplication of case arm by leaf shape.
    // ------------------------------------------------------------------

    /// <summary>
    /// If <paramref name="caseExpr"/>'s scrutinee is, after peeling
    /// <see cref="SyntaxTypes.Expression.ParenthesizedExpression"/> /
    /// <see cref="SyntaxTypes.Expression.LetExpression"/>, an
    /// <see cref="SyntaxTypes.Expression.IfBlock"/> whose every leaf is
    /// tagged by a statically-known constructor, AND each predicted
    /// leaf tag maps uniquely to exactly one arm of the case (with no
    /// pre-empting wildcard / variable arm), AND the duplication cost
    /// budget in <paramref name="config"/> is respected, returns the
    /// consolidated expression (an <see cref="SyntaxTypes.Expression.IfBlock"/>
    /// rooted at the same root with each leaf substituted by the
    /// matching arm body — possibly wrapped in a <c>let</c>
    /// destructure binding the arm pattern's variables against the
    /// constructor's argument expressions).
    ///
    /// <para>
    /// Returns <see langword="null"/> if any precondition fails.
    /// </para>
    /// </summary>
    public static SyntaxTypes.Expression? TryConsolidateBranchedCase(
        SyntaxTypes.Expression.CaseExpression caseExpr,
        CaseConsolidationConfig config)
    {
        var scrutinee = caseExpr.CaseBlock.Expression.Value;

        // Identify and split the root: any LetExpression / Paren wrappers
        // around the IfBlock will be re-emitted as outer wrappers around
        // the consolidated expression.
        var (peeledScrutinee, rewrap) = PeelLetAndParenWrappers(scrutinee);

        if (peeledScrutinee is not SyntaxTypes.Expression.IfBlock)
            return null;

        var leaves =
            SyntaxTypes.SyntaxAnalysis
            .EnumerateConstructorTaggedLeavesOfIfChain(peeledScrutinee);

        if (leaves is null || leaves.Count is 0)
            return null;

        // For each leaf, find the unique arm of the outer case that
        // selects its predicted tag. Reject the rewrite if any leaf has
        // a preceding wildcard / variable arm, or if no arm matches.
        var armForLeaf = new SyntaxTypes.Case[leaves.Count];

        for (var i = 0; i < leaves.Count; i++)
        {
            var match = TryFindUniqueArmForTag(caseExpr.CaseBlock.Cases, leaves[i].PredictedTag);

            if (match is null)
                return null;

            armForLeaf[i] = match;
        }

        // Cost budget check.
        var totalDuplicated = 0;

        foreach (var arm in armForLeaf)
        {
            var armNodes =
                SyntaxTypes.SyntaxAnalysis.CountExpressionNodes(
                    arm.Expression.Value,
                    max: config.MaxArmBodyNodeCount + 1);

            if (armNodes > config.MaxArmBodyNodeCount)
                return null;

            totalDuplicated += armNodes;

            if (totalDuplicated > config.MaxTotalDuplicatedNodes)
                return null;
        }

        // Build the replacement: walk the if-chain and substitute each
        // leaf with the corresponding (rebound) arm body.

        // Shadowing guard: if any selected arm's pattern binds a name
        // that already appears free in its corresponding leaf
        // expression, substituting the arm body under
        // `let <armPattern> = <leafExpr> in <armBody>` (see
        // <see cref="SubstituteLeafIntoArmBody"/>) would shadow that
        // outer-scope name and trip the pipeline's post-stage
        // <c>CheckForNamingErrors</c> validation. Abort the rewrite.
        for (var i = 0; i < leaves.Count; i++)
        {
            var armPatternBindings =
                SyntaxTypes.SyntaxAnalysis.CollectNamesBoundByPattern(armForLeaf[i].Pattern.Value);

            if (armPatternBindings.Count is 0)
                continue;

            var leafFreeVars =
                SyntaxTypes.SyntaxAnalysis.CollectRemainingFreeVariables(leaves[i].LeafExpression);

            if (armPatternBindings.Overlaps(leafFreeVars))
                return null;
        }

        var leafIndex = 0;
        var replacement = RebuildIfChainWithSubstitutedLeaves(peeledScrutinee, leaves, armForLeaf, ref leafIndex);

        if (leafIndex != leaves.Count)
        {
            // Defensive: the leaf order from the enumerator must match
            // the order in which the rebuilder visits leaves. If it does
            // not, abort the rewrite rather than emitting a wrong tree.
            return null;
        }

        return rewrap(replacement);
    }

    private static SyntaxTypes.Expression RebuildIfChainWithSubstitutedLeaves(
        SyntaxTypes.Expression expression,
        IReadOnlyList<(SyntaxTypes.Expression LeafExpression, SyntaxTypes.QualifiedNameRef PredictedTag)> leaves,
        SyntaxTypes.Case[] armForLeaf,
        ref int leafIndex)
    {
        // Peel parens at each chain position. Parens around an IfBlock
        // are transparent.
        var current = expression;

        if (current is SyntaxTypes.Expression.ParenthesizedExpression paren)
        {
            var rewrittenInner =
                RebuildIfChainWithSubstitutedLeaves(paren.Expression.Value, leaves, armForLeaf, ref leafIndex);

            return
                new SyntaxTypes.Expression.ParenthesizedExpression(
                    new Node<SyntaxTypes.Expression>(paren.Expression.Range, rewrittenInner));
        }

        if (current is SyntaxTypes.Expression.IfBlock ifBlock)
        {
            var newThen =
                RebuildIfChainWithSubstitutedLeaves(ifBlock.ThenBlock.Value, leaves, armForLeaf, ref leafIndex);

            var newElse =
                RebuildIfChainWithSubstitutedLeaves(ifBlock.ElseBlock.Value, leaves, armForLeaf, ref leafIndex);

            return
                new SyntaxTypes.Expression.IfBlock(
                    Condition: ifBlock.Condition,
                    ThenBlock: new Node<SyntaxTypes.Expression>(ifBlock.ThenBlock.Range, newThen),
                    ElseBlock: new Node<SyntaxTypes.Expression>(ifBlock.ElseBlock.Range, newElse));
        }

        // Otherwise this is a leaf. The leaf's expression must match the
        // one the enumerator reported at this index (reference- or
        // value-equality on the expression node).
        if (leafIndex >= leaves.Count)
        {
            // Defensive: more leaves than the enumerator reported.
            // Signal via the index check at the call site by leaving
            // leafIndex past the end.
            leafIndex++;
            return current;
        }

        var leafEntry = leaves[leafIndex];
        var arm = armForLeaf[leafIndex];

        leafIndex++;

        return SubstituteLeafIntoArmBody(leafEntry.LeafExpression, arm);
    }

    /// <summary>
    /// Builds the expression that replaces an if-chain leaf at the
    /// matching outer-case arm: pattern-bind the arm's pattern against
    /// the leaf's constructor application arguments, then evaluate the
    /// arm body in that scope.
    /// </summary>
    private static SyntaxTypes.Expression SubstituteLeafIntoArmBody(
        SyntaxTypes.Expression leafExpression,
        SyntaxTypes.Case arm)
    {
        // Peel parens around the leaf to expose the constructor head
        // / argument list. The leaf retains its tag by construction (the
        // enumerator only reports leaves whose
        // TryPredictOutermostConstructorTag returns non-null).
        var peeledLeaf = UnwrapParenExpression(leafExpression);

        var armPattern = SyntaxTypes.SyntaxAnalysis.UnwrapParenthesized(arm.Pattern.Value);

        // If the arm pattern is a wildcard or var pattern, no
        // destructure is needed — substitute the var name (if any)
        // through the body.
        if (armPattern is SyntaxTypes.Pattern.AllPattern)
        {
            return arm.Expression.Value;
        }

        if (armPattern is SyntaxTypes.Pattern.VarPattern armVar)
        {
            // The entire leaf expression is bound to the var.
            var subs =
                new Dictionary<string, Node<SyntaxTypes.Expression>>(StringComparer.Ordinal)
                {
                    [armVar.Name] = new Node<SyntaxTypes.Expression>(s_zeroRange, leafExpression),
                };

            return ElmSyntaxTransformations.SubstituteInExpression(arm.Expression, subs).Value;
        }

        // Named constructor (possibly aliased): emit a let-destructure
        // binding the arm pattern against the constructor argument
        // expression(s) from the leaf.
        var namedArmPattern = ElmSyntaxTransformations.TryUnwrapToNamedPattern(arm.Pattern.Value);

        // Even with an alias, we need the alias name visible inside the
        // arm body — re-emit the original arm pattern as the
        // let-destructure pattern so all bound names (alias + nested
        // var patterns) are introduced together.
        var destructurePattern = arm.Pattern;

        Node<SyntaxTypes.Expression> rhsNode;

        if (namedArmPattern is null)
        {
            // Arm pattern is not a constructor — fall back to a single
            // VarPattern destructure of the leaf if possible, or emit
            // the leaf as the RHS unchanged.
            rhsNode = new Node<SyntaxTypes.Expression>(s_zeroRange, leafExpression);
        }
        else if (namedArmPattern.Arguments.Count is 0)
        {
            // Zero-arity constructor arm (e.g. `Maybe.Nothing -> body`).
            // The destructure has no variables to bind, so we can elide
            // it entirely and emit the arm body directly.
            return arm.Expression.Value;
        }
        else if (peeledLeaf is SyntaxTypes.Expression.Application app
            && app.Arguments.Count == namedArmPattern.Arguments.Count + 1)
        {
            // Constructor with arguments: rhsNode is the leaf
            // application as-is. The let-destructure pattern matches
            // it positionally.
            rhsNode = new Node<SyntaxTypes.Expression>(s_zeroRange, leafExpression);
        }
        else
        {
            // The leaf is a bare uppercase reference but the arm
            // expects arguments, or the arity does not match — give up
            // safely by binding the leaf to the destructure pattern as
            // a whole (Elm runtime will fail at pattern match, but the
            // shape is preserved).
            rhsNode = new Node<SyntaxTypes.Expression>(s_zeroRange, leafExpression);
        }

        var letDestr =
            new SyntaxTypes.Expression.LetDeclaration.LetDestructuring(
                Pattern: destructurePattern,
                Expression: rhsNode);

        return
            new SyntaxTypes.Expression.LetExpression(
                new SyntaxTypes.Expression.LetBlock(
                    Declarations:
                    [
                    new Node<SyntaxTypes.Expression.LetDeclaration>(s_zeroRange, letDestr),
                    ],
                    Expression: arm.Expression));
    }

    /// <summary>
    /// Finds the arm of <paramref name="arms"/> whose pattern's
    /// outermost named constructor matches <paramref name="tag"/>.
    /// Returns <see langword="null"/> if no arm matches, more than one
    /// arm matches, or a preceding wildcard / variable arm would
    /// short-circuit the match.
    /// </summary>
    private static SyntaxTypes.Case? TryFindUniqueArmForTag(
        IReadOnlyList<SyntaxTypes.Case> arms,
        SyntaxTypes.QualifiedNameRef tag)
    {
        SyntaxTypes.Case? match = null;

        for (var i = 0; i < arms.Count; i++)
        {
            var arm = arms[i];
            var peeled = SyntaxTypes.SyntaxAnalysis.UnwrapParenthesized(arm.Pattern.Value);

            // Look through AsPattern: the alias-bound name does not
            // change the matched tag.
            while (peeled is SyntaxTypes.Pattern.AsPattern asPat)
            {
                peeled = SyntaxTypes.SyntaxAnalysis.UnwrapParenthesized(asPat.Pattern.Value);
            }

            // A wildcard or var arm preceding our predicted-tag arm
            // would short-circuit the match — abort the rewrite.
            if (peeled is SyntaxTypes.Pattern.AllPattern || peeled is SyntaxTypes.Pattern.VarPattern)
            {
                if (match is not null)
                {
                    // We already found the matching arm; an arm AFTER
                    // it is irrelevant (Elm's first-match semantics
                    // means it cannot fire). But the early wildcard
                    // case is the one that breaks the rewrite — bail
                    // here only if we have NOT found a match yet.
                }
                else
                {
                    return null;
                }

                continue;
            }

            if (peeled is SyntaxTypes.Pattern.NamedPattern named && named.Name.Equals(tag))
            {
                if (match is not null)
                {
                    // Duplicate match — Elm's first-match semantics
                    // means the second one is dead code; pick the
                    // first match deterministically.
                    continue;
                }

                match = arm;
            }
        }

        return match;
    }

    // ------------------------------------------------------------------
    // Rewrite 2 — Deduct tag constraint via alias pattern (case-of-case).
    // ------------------------------------------------------------------

    /// <summary>
    /// If <paramref name="caseExpr"/>'s scrutinee is, after peeling
    /// <see cref="SyntaxTypes.Expression.ParenthesizedExpression"/> /
    /// <see cref="SyntaxTypes.Expression.LetExpression"/>, another
    /// <see cref="SyntaxTypes.Expression.CaseExpression"/>, AND every
    /// inner-arm body has a statically-inferable outermost tag (either
    /// directly or via an alias pattern on the inner arm pattern), AND
    /// every inner-arm tag maps uniquely to one outer-arm with no
    /// duplication required, returns the consolidated expression: the
    /// inner case with each arm body rewritten to the matching
    /// outer-arm body (bound through the inner arm body's tag-carrying
    /// expression).
    ///
    /// <para>
    /// Returns <see langword="null"/> if any precondition fails.
    /// </para>
    /// </summary>
    public static SyntaxTypes.Expression? TryConsolidateNestedCaseOfCase(
        SyntaxTypes.Expression.CaseExpression outerCase)
    {
        var outerScrutinee = outerCase.CaseBlock.Expression.Value;
        var (peeledScrutinee, rewrap) = PeelLetAndParenWrappers(outerScrutinee);

        if (peeledScrutinee is not SyntaxTypes.Expression.CaseExpression innerCase)
            return null;

        // For every inner arm, compute the predicted outermost tag.
        var innerArms = innerCase.CaseBlock.Cases;

        if (innerArms.Count is 0)
            return null;

        var tagPerInnerArm = new SyntaxTypes.QualifiedNameRef[innerArms.Count];

        for (var i = 0; i < innerArms.Count; i++)
        {
            var tag =
                SyntaxTypes.SyntaxAnalysis.TryPredictOutermostConstructorTagInArmBody(
                    innerArms[i].Pattern.Value,
                    innerArms[i].Expression.Value);

            if (tag is null)
                return null;

            tagPerInnerArm[i] = tag;
        }

        // Map each inner-arm tag to a unique outer arm. No outer arm
        // may be selected by two distinct inner tags (this rewrite
        // does not duplicate arm bodies).
        var outerArmForInnerArm = new SyntaxTypes.Case[innerArms.Count];
        var outerArmIndexUsed = new HashSet<int>();

        for (var i = 0; i < innerArms.Count; i++)
        {
            var (outerArm, outerIdx) =
                TryFindUniqueArmForTagWithIndex(outerCase.CaseBlock.Cases, tagPerInnerArm[i]);

            if (outerArm is null)
                return null;

            if (!outerArmIndexUsed.Add(outerIdx))
                return null;

            // Shadowing guard: if the outer-arm pattern would re-bind
            // any name already in scope from the inner-arm pattern,
            // emitting `let <outerPattern> = <innerArmBody> in
            // <outerArmBody>` (see <see cref="BuildConsolidatedArmBody"/>)
            // would produce a shadowing — caught by the pipeline's
            // post-stage <c>CheckForNamingErrors</c> validation. Abort
            // the rewrite rather than emit shadow-prone code.
            var innerBindings =
                SyntaxTypes.SyntaxAnalysis.CollectNamesBoundByPattern(innerArms[i].Pattern.Value);

            var outerBindings =
                SyntaxTypes.SyntaxAnalysis.CollectNamesBoundByPattern(outerArm.Pattern.Value);

            if (innerBindings.Overlaps(outerBindings))
                return null;

            outerArmForInnerArm[i] = outerArm;
        }

        // Build the consolidated case: each inner arm keeps its pattern
        // but its body becomes the outer-arm body, with the outer-arm
        // pattern matched against the inner-arm body expression.
        var newCases = new SyntaxTypes.Case[innerArms.Count];

        for (var i = 0; i < innerArms.Count; i++)
        {
            var innerArm = innerArms[i];
            var outerArm = outerArmForInnerArm[i];

            var rewrittenBody =
                BuildConsolidatedArmBody(
                    innerArmPattern: innerArm.Pattern.Value,
                    innerArmBody: innerArm.Expression,
                    outerArmPattern: outerArm.Pattern,
                    outerArmBody: outerArm.Expression);

            newCases[i] =
                new SyntaxTypes.Case(
                    Pattern: innerArm.Pattern,
                    Expression: rewrittenBody);
        }

        var consolidatedCase =
            new SyntaxTypes.Expression.CaseExpression(
                new SyntaxTypes.CaseBlock(
                    Expression: innerCase.CaseBlock.Expression,
                    Cases: newCases));

        return rewrap(consolidatedCase);
    }

    private static (SyntaxTypes.Case? Arm, int Index) TryFindUniqueArmForTagWithIndex(
        IReadOnlyList<SyntaxTypes.Case> arms,
        SyntaxTypes.QualifiedNameRef tag)
    {
        SyntaxTypes.Case? match = null;
        var matchIndex = -1;

        for (var i = 0; i < arms.Count; i++)
        {
            var arm = arms[i];
            var peeled = SyntaxTypes.SyntaxAnalysis.UnwrapParenthesized(arm.Pattern.Value);

            while (peeled is SyntaxTypes.Pattern.AsPattern asPat)
            {
                peeled = SyntaxTypes.SyntaxAnalysis.UnwrapParenthesized(asPat.Pattern.Value);
            }

            if ((peeled is SyntaxTypes.Pattern.AllPattern || peeled is SyntaxTypes.Pattern.VarPattern)
                && match is null)
            {
                return (null, -1);
            }

            if (peeled is SyntaxTypes.Pattern.NamedPattern named && named.Name.Equals(tag))
            {
                if (match is not null)
                    continue;

                match = arm;
                matchIndex = i;
            }
        }

        return (match, matchIndex);
    }

    private static Node<SyntaxTypes.Expression> BuildConsolidatedArmBody(
        SyntaxTypes.Pattern innerArmPattern,
        Node<SyntaxTypes.Expression> innerArmBody,
        Node<SyntaxTypes.Pattern> outerArmPattern,
        Node<SyntaxTypes.Expression> outerArmBody)
    {
        var outerPeeled = SyntaxTypes.SyntaxAnalysis.UnwrapParenthesized(outerArmPattern.Value);

        // Trivial cases for the outer arm pattern:
        // - wildcard `_` → no binding, return outer body unchanged.
        // - bare var `x`  → substitute `x` with the inner arm's body expression.
        if (outerPeeled is SyntaxTypes.Pattern.AllPattern)
        {
            return outerArmBody;
        }

        if (outerPeeled is SyntaxTypes.Pattern.VarPattern outerVar)
        {
            var subs =
                new Dictionary<string, Node<SyntaxTypes.Expression>>(StringComparer.Ordinal)
                {
                    [outerVar.Name] = innerArmBody,
                };

            return ElmSyntaxTransformations.SubstituteInExpression(outerArmBody, subs);
        }

        // Otherwise emit a let-destructure binding the outer arm
        // pattern against the inner arm body, and evaluate the outer
        // arm body in that scope.
        var letDestr =
            new SyntaxTypes.Expression.LetDeclaration.LetDestructuring(
                Pattern: outerArmPattern,
                Expression: innerArmBody);

        var letExpr =
            new SyntaxTypes.Expression.LetExpression(
                new SyntaxTypes.Expression.LetBlock(
                    Declarations:
                    [
                    new Node<SyntaxTypes.Expression.LetDeclaration>(s_zeroRange, letDestr),
                    ],
                    Expression: outerArmBody));

        return new Node<SyntaxTypes.Expression>(outerArmBody.Range, letExpr);
    }

    // ------------------------------------------------------------------
    // Shared helpers.
    // ------------------------------------------------------------------

    /// <summary>
    /// Peels <see cref="SyntaxTypes.Expression.ParenthesizedExpression"/>
    /// and <see cref="SyntaxTypes.Expression.LetExpression"/> wrappers
    /// off the given expression, returning the peeled inner expression
    /// together with a function that re-wraps a replacement inner
    /// expression in the same enclosing wrappers. The re-wrap function
    /// preserves the let-bindings so any names they introduce remain in
    /// scope around the new inner expression.
    /// </summary>
    private static (SyntaxTypes.Expression Peeled, Func<SyntaxTypes.Expression, SyntaxTypes.Expression> Rewrap)
        PeelLetAndParenWrappers(SyntaxTypes.Expression expression)
    {
        var stack = new List<Func<SyntaxTypes.Expression, SyntaxTypes.Expression>>();
        var current = expression;

        while (true)
        {
            switch (current)
            {
                case SyntaxTypes.Expression.ParenthesizedExpression paren:
                    {
                        var range = paren.Expression.Range;

                        stack.Add(
                            inner =>
                            new SyntaxTypes.Expression.ParenthesizedExpression(
                                new Node<SyntaxTypes.Expression>(range, inner)));

                        current = paren.Expression.Value;
                        continue;
                    }

                case SyntaxTypes.Expression.LetExpression letExpr:
                    {
                        var letBlock = letExpr.Value;
                        var bodyRange = letBlock.Expression.Range;

                        stack.Add(
                            inner =>
                            new SyntaxTypes.Expression.LetExpression(
                                new SyntaxTypes.Expression.LetBlock(
                                    Declarations: letBlock.Declarations,
                                    Expression: new Node<SyntaxTypes.Expression>(bodyRange, inner))));

                        current = letBlock.Expression.Value;
                        continue;
                    }
            }

            break;
        }

        SyntaxTypes.Expression Rewrap(SyntaxTypes.Expression inner)
        {
            var result = inner;

            for (var i = stack.Count - 1; i >= 0; i--)
            {
                result = stack[i](result);
            }

            return result;
        }

        return (current, Rewrap);
    }

    private static SyntaxTypes.Expression UnwrapParenExpression(SyntaxTypes.Expression expression)
    {
        while (expression is SyntaxTypes.Expression.ParenthesizedExpression paren)
        {
            expression = paren.Expression.Value;
        }

        return expression;
    }
}
