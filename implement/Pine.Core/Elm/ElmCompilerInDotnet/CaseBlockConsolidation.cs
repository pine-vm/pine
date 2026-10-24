using Pine.Core.CodeAnalysis;
using Pine.Core.Elm.Syntax.Abstract;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;

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
/// <see cref="Expression.CaseExpression"/> is an
/// <see cref="Expression.IfBlock"/> whose every leaf produces
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
/// <see cref="Expression.LetExpression"/>), and every inner-arm
/// body has a statically-inferable tag (either directly or via an
/// <see cref="Pattern.AsPattern"/> on the inner arm pattern),
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
    /// <see cref="RewriteDeclarationDictionary(ImmutableDictionary{DeclQualifiedName, Declaration}, CaseConsolidationConfig)"/>.
    /// </summary>
    public static OptimizedElmSyntaxDeclarations RewriteDeclarationDictionary(
        OptimizedElmSyntaxDeclarations declarations,
        CaseConsolidationConfig config) =>
        OptimizedElmSyntaxDeclarations.FromFlatDictionary(
            RewriteDeclarationDictionary(
                declarations.RenderAsFlatDictionary(),
                config));

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
    public static ImmutableDictionary<DeclQualifiedName, Declaration> RewriteDeclarationDictionary(
        ImmutableDictionary<DeclQualifiedName, Declaration> declarations,
        CaseConsolidationConfig config)
    {
        var builder = ImmutableDictionary.CreateBuilder<DeclQualifiedName, Declaration>();

        foreach (var (declName, decl) in declarations)
        {
            builder[declName] = RewriteDeclaration(decl, config);
        }

        return builder.ToImmutable();
    }

    private static Declaration RewriteDeclaration(
        Declaration decl,
        CaseConsolidationConfig config)
    {
        switch (decl)
        {
            case Declaration.FunctionDeclaration funcDecl:
                {
                    var impl = funcDecl.Function.Declaration;

                    var newBody = RewriteExpression(impl.Expression, config);

                    if (ReferenceEquals(newBody, impl.Expression))
                        return decl;

                    var newImpl =
                        new FunctionImplementation(
                            impl.Name,
                            impl.Arguments,
                            newBody);

                    var newFunc =
                        funcDecl.Function with
                        {
                            Declaration = newImpl,
                        };

                    return new Declaration.FunctionDeclaration(newFunc);
                }

            case Declaration.ChoiceTypeDeclaration:
            case Declaration.AliasDeclaration:
            case Declaration.PortDeclaration:
            case Declaration.InfixDeclaration:
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
    /// <see cref="ElmSyntaxAbstractTransformations.MapChildExpressions"/>, then
    /// applies the two local peephole rewrites. If the local rewrite
    /// fires, the result is re-rewritten to allow cascading
    /// consolidations.
    /// </summary>
    public static Expression RewriteExpression(
        Expression expr,
        CaseConsolidationConfig config)
    {
        var anyChildChanged = false;

        Expression Recurse(Expression child)
        {
            var rewrittenChild = RewriteExpression(child, config);

            if (ReferenceEquals(rewrittenChild, child))
                return child;

            anyChildChanged = true;
            return rewrittenChild;
        }

        var withChildrenRewritten = ElmSyntaxAbstractTransformations.MapChildExpressions(expr, Recurse);

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
    public static Expression? TryConsolidateLocal(
        Expression expr,
        CaseConsolidationConfig config)
    {
        if (expr is not Expression.CaseExpression caseExpr)
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
    /// <see cref="Expression.LetExpression"/>, an
    /// <see cref="Expression.IfBlock"/> whose every leaf is
    /// tagged by a statically-known constructor, AND each predicted
    /// leaf tag maps uniquely to exactly one arm of the case (with no
    /// pre-empting wildcard / variable arm), AND the duplication cost
    /// budget in <paramref name="config"/> is respected, returns the
    /// consolidated expression (an <see cref="Expression.IfBlock"/>
    /// rooted at the same root with each leaf substituted by the
    /// matching arm body — possibly wrapped in a <c>let</c>
    /// destructure binding the arm pattern's variables against the
    /// constructor's argument expressions).
    ///
    /// <para>
    /// Returns <see langword="null"/> if any precondition fails.
    /// </para>
    /// </summary>
    public static Expression? TryConsolidateBranchedCase(
        Expression.CaseExpression caseExpr,
        CaseConsolidationConfig config)
    {
        var scrutinee = caseExpr.Expression;

        // Identify and split the root: any LetExpression wrappers
        // around the IfBlock will be re-emitted as outer wrappers around
        // the consolidated expression.
        var (peeledScrutinee, rewrap) = PeelLetWrappers(scrutinee);

        if (peeledScrutinee is not Expression.IfBlock)
            return null;

        var leaves =
            EnumerateConstructorTaggedLeavesOfIfChain(peeledScrutinee);

        if (leaves is null || leaves.Count is 0)
            return null;

        // For each leaf, find the unique arm of the outer case that
        // selects its predicted tag. Reject the rewrite if any leaf has
        // a preceding wildcard / variable arm, or if no arm matches.
        var armForLeaf = new Case[leaves.Count];

        for (var i = 0; i < leaves.Count; i++)
        {
            var match = TryFindUniqueArmForTag(caseExpr.Cases, leaves[i].PredictedTag);

            if (match is null)
                return null;

            armForLeaf[i] = match;
        }

        // Cost budget check.
        var totalDuplicated = 0;

        foreach (var arm in armForLeaf)
        {
            var armNodes =
                CountExpressionNodes(
                    arm.Expression,
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
                SyntaxAnalysis.CollectNamesBoundByPattern(armForLeaf[i].Pattern);

            if (armPatternBindings.Count is 0)
                continue;

            var leafFreeVars =
                SyntaxAnalysis.CollectRemainingFreeVariables(leaves[i].LeafExpression);

            if (armPatternBindings.Overlaps(leafFreeVars))
                return null;

            // Also abort if the arm pattern would re-bind a name that is
            // already introduced by a binder inside the leaf expression
            // (e.g. a nested `let quotient = ...` inside the leaf and a
            // `Just (quotient, _)` arm pattern). Emitting
            // `let <armPattern> = <leafExpr> in <armBody>` in that case
            // produces two declarations of the same name in overlapping
            // scopes — a naming clash caught by post-stage validation.
            var leafBoundVars =
                CollectTopLevelLetBoundNames(leaves[i].LeafExpression);

            if (armPatternBindings.Overlaps(leafBoundVars))
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

    private static Expression RebuildIfChainWithSubstitutedLeaves(
        Expression expression,
        IReadOnlyList<(Expression LeafExpression, QualifiedNameRef PredictedTag)> leaves,
        Case[] armForLeaf,
        ref int leafIndex)
    {
        var current = expression;

        if (current is Expression.IfBlock ifBlock)
        {
            var newThen =
                RebuildIfChainWithSubstitutedLeaves(ifBlock.ThenBlock, leaves, armForLeaf, ref leafIndex);

            var newElse =
                RebuildIfChainWithSubstitutedLeaves(ifBlock.ElseBlock, leaves, armForLeaf, ref leafIndex);

            return
                new Expression.IfBlock(
                    Condition: ifBlock.Condition,
                    ThenBlock: newThen,
                    ElseBlock: newElse);
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
    private static Expression SubstituteLeafIntoArmBody(
        Expression leafExpression,
        Case arm)
    {
        // Named constructor (possibly aliased): emit a let-destructure
        // binding the arm pattern against the constructor argument
        // expression(s) from the leaf.
        var namedArmPattern =
            SyntaxAnalysis.PeelPatternAsBinder(arm.Pattern) as Pattern.NamedPattern;

        // Even with an alias, we need the alias name visible inside the
        // arm body — re-emit the original arm pattern as the
        // let-destructure pattern so all bound names (alias + nested
        // var patterns) are introduced together.
        if (namedArmPattern is { Arguments.Count: 0 } &&
            SyntaxAnalysis.CollectNamesBoundByPattern(arm.Pattern).Count is 0)
        {
            // Zero-arity constructor arm (e.g. `Maybe.Nothing -> body`).
            // Elide the destructure only when it introduces no names;
            // an alias still needs to be bound even without arguments.
            return arm.Expression;
        }

        var letDestr =
            new LetDeclaration.LetDestructuring(
                Pattern: arm.Pattern,
                Expression: leafExpression);

        return
            new Expression.LetExpression(
                Declarations: [letDestr],
                Expression: arm.Expression);
    }

    /// <summary>
    /// Finds the arm of <paramref name="arms"/> whose pattern's
    /// outermost named constructor matches <paramref name="tag"/>.
    /// Returns <see langword="null"/> if no arm matches, more than one
    /// arm matches, or a preceding wildcard / variable arm would
    /// short-circuit the match.
    /// </summary>
    private static Case? TryFindUniqueArmForTag(
        IReadOnlyList<Case> arms,
        QualifiedNameRef tag)
    {
        Case? match = null;

        for (var i = 0; i < arms.Count; i++)
        {
            var arm = arms[i];
            var peeled = SyntaxAnalysis.PeelPatternAsBinder(arm.Pattern);

            // Look through AsPattern: the alias-bound name does not
            // change the matched tag.
            // A wildcard or var arm preceding our predicted-tag arm
            // would short-circuit the match — abort the rewrite.
            if (peeled is Pattern.AllPattern || peeled is Pattern.VarPattern)
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

            if (peeled is Pattern.NamedPattern named && named.Name.Equals(tag))
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
    /// If <paramref name="outerCase"/>'s scrutinee is, after peeling
    /// <see cref="Expression.LetExpression"/>, another
    /// <see cref="Expression.CaseExpression"/>, AND every
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
    public static Expression? TryConsolidateNestedCaseOfCase(
        Expression.CaseExpression outerCase)
    {
        var outerScrutinee = outerCase.Expression;
        var (peeledScrutinee, rewrap) = PeelLetWrappers(outerScrutinee);

        if (peeledScrutinee is not Expression.CaseExpression innerCase)
            return null;

        // For every inner arm, compute the predicted outermost tag.
        var innerArms = innerCase.Cases;

        if (innerArms.Count is 0)
            return null;

        var tagPerInnerArm = new QualifiedNameRef[innerArms.Count];

        for (var i = 0; i < innerArms.Count; i++)
        {
            var tag =
                TryPredictOutermostConstructorTagInArmBody(
                    innerArms[i].Pattern,
                    innerArms[i].Expression);

            if (tag is null)
                return null;

            tagPerInnerArm[i] = tag;
        }

        // Map each inner-arm tag to a unique outer arm. No outer arm
        // may be selected by two distinct inner tags (this rewrite
        // does not duplicate arm bodies).
        var outerArmForInnerArm = new Case[innerArms.Count];
        var outerArmIndexUsed = new HashSet<int>();

        for (var i = 0; i < innerArms.Count; i++)
        {
            var (outerArm, outerIdx) =
                TryFindUniqueArmForTagWithIndex(outerCase.Cases, tagPerInnerArm[i]);

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
                SyntaxAnalysis.CollectNamesBoundByPattern(innerArms[i].Pattern);

            var outerBindings =
                SyntaxAnalysis.CollectNamesBoundByPattern(outerArm.Pattern);

            if (innerBindings.Overlaps(outerBindings))
                return null;

            // The outer-arm pattern is emitted as a let-destructure whose
            // RHS is the inner-arm body. If that body itself introduces a
            // binder of the same name (e.g. a nested `let`), the wrapping
            // would create a naming clash; abort.
            var innerBodyBound =
                CollectTopLevelLetBoundNames(innerArms[i].Expression);

            if (outerBindings.Overlaps(innerBodyBound))
                return null;

            outerArmForInnerArm[i] = outerArm;
        }

        // Build the consolidated case: each inner arm keeps its pattern
        // but its body becomes the outer-arm body, with the outer-arm
        // pattern matched against the inner-arm body expression.
        var newCases = new Case[innerArms.Count];

        for (var i = 0; i < innerArms.Count; i++)
        {
            var innerArm = innerArms[i];
            var outerArm = outerArmForInnerArm[i];

            var rewrittenBody =
                BuildConsolidatedArmBody(
                    innerArmBody: innerArm.Expression,
                    outerArmPattern: outerArm.Pattern,
                    outerArmBody: outerArm.Expression);

            newCases[i] =
                new Case(
                    Pattern: innerArm.Pattern,
                    Expression: rewrittenBody);
        }

        var consolidatedCase =
            new Expression.CaseExpression(
                Expression: innerCase.Expression,
                Cases: newCases);

        return rewrap(consolidatedCase);
    }

    private static (Case? Arm, int Index) TryFindUniqueArmForTagWithIndex(
        IReadOnlyList<Case> arms,
        QualifiedNameRef tag)
    {
        Case? match = null;
        var matchIndex = -1;

        for (var i = 0; i < arms.Count; i++)
        {
            var arm = arms[i];
            var peeled = SyntaxAnalysis.PeelPatternAsBinder(arm.Pattern);

            if ((peeled is Pattern.AllPattern || peeled is Pattern.VarPattern)
                && match is null)
            {
                return (null, -1);
            }

            if (peeled is Pattern.NamedPattern named && named.Name.Equals(tag))
            {
                if (match is not null)
                    continue;

                match = arm;
                matchIndex = i;
            }
        }

        return (match, matchIndex);
    }

    private static Expression BuildConsolidatedArmBody(
        Expression innerArmBody,
        Pattern outerArmPattern,
        Expression outerArmBody)
    {
        // The selected outer arm always has a named constructor pattern.
        // Emit a let-destructure binding the outer arm
        // pattern against the inner arm body, and evaluate the outer
        // arm body in that scope.
        var letDestr =
            new LetDeclaration.LetDestructuring(
                Pattern: outerArmPattern,
                Expression: innerArmBody);

        return
            new Expression.LetExpression(
                Declarations: [letDestr],
                Expression: outerArmBody);
    }

    // ------------------------------------------------------------------
    // Shared helpers.
    // ------------------------------------------------------------------

    private static int CountExpressionNodes(Expression expression, int max)
    {
        if (max <= 0)
            return 0;

        var pending = new Stack<Expression>();
        pending.Push(expression);

        // Preserve the existing budget convention: count descendants, not the root.
        var count = 0;

        while (pending.TryPop(out var current))
        {
            ElmSyntaxAbstractTransformations.MapChildExpressions(
                current,
                child =>
                {
                    pending.Push(child);
                    count++;
                    return child;
                });

            if (count >= max)
                return max;
        }

        return count;
    }

    private static ImmutableHashSet<string> CollectTopLevelLetBoundNames(Expression expression)
    {
        var names = ImmutableHashSet.CreateBuilder<string>();

        while (expression is Expression.LetExpression letExpr)
        {
            foreach (var declaration in letExpr.Declarations)
            {
                switch (declaration)
                {
                    case LetDeclaration.LetFunction function:
                        names.Add(function.Function.Declaration.Name);
                        break;

                    case LetDeclaration.LetDestructuring destructuring:
                        names.UnionWith(SyntaxAnalysis.CollectNamesBoundByPattern(destructuring.Pattern));
                        break;

                    default:
                        throw new NotImplementedException(
                            "CollectTopLevelLetBoundNames does not handle let declaration variant: " +
                            declaration.GetType().Name);
                }
            }

            expression = letExpr.Expression;
        }

        return names.ToImmutable();
    }

    private static QualifiedNameRef? TryPredictOutermostConstructorTag(
        Expression expression)
    {
        while (expression is Expression.LetExpression letExpr)
            expression = letExpr.Expression;

        var identifier =
            expression is Pine.Core.Elm.Syntax.Abstract.Expression.Application application
            ?
            application.Function as Pine.Core.Elm.Syntax.Abstract.Expression.Identifier
            :
            expression as Pine.Core.Elm.Syntax.Abstract.Expression.Identifier;

        if (identifier is not null &&
            identifier.QualifiedName.DeclName.Length > 0 &&
            char.IsUpper(identifier.QualifiedName.DeclName[0]))
        {
            return
                new QualifiedNameRef(
                    identifier.QualifiedName.Namespaces,
                    identifier.QualifiedName.DeclName);
        }

        if (expression is Expression.IfBlock ifBlock)
        {
            var thenTag = TryPredictOutermostConstructorTag(ifBlock.ThenBlock);
            var elseTag = TryPredictOutermostConstructorTag(ifBlock.ElseBlock);

            return thenTag is not null && thenTag.Equals(elseTag) ? thenTag : null;
        }

        return null;
    }

    private static IReadOnlyList<(Expression LeafExpression, QualifiedNameRef PredictedTag)>?
        EnumerateConstructorTaggedLeavesOfIfChain(Expression expression)
    {
        var leaves = new List<(Expression, QualifiedNameRef)>();

        bool Collect(Expression current)
        {
            if (current is Expression.IfBlock ifBlock)
                return Collect(ifBlock.ThenBlock) && Collect(ifBlock.ElseBlock);

            var tag = TryPredictOutermostConstructorTag(current);

            if (tag is null)
                return false;

            leaves.Add((current, tag));
            return true;
        }

        return Collect(expression) ? leaves : null;
    }

    private static QualifiedNameRef? TryPredictOutermostConstructorTagInArmBody(
        Pattern armPattern,
        Expression armBody)
    {
        if (TryPredictOutermostConstructorTag(armBody) is { } directTag)
            return directTag;

        if (armBody is Expression.Identifier identifier &&
            identifier.QualifiedName.Namespaces.Count is 0 &&
            armPattern is Pattern.AsPattern alias &&
            string.Equals(alias.Name, identifier.QualifiedName.DeclName, StringComparison.Ordinal) &&
            SyntaxAnalysis.PeelPatternAsBinder(alias.Pattern) is Pattern.NamedPattern named)
        {
            return named.Name;
        }

        return null;
    }

    /// <summary>
    /// Peels <see cref="Expression.LetExpression"/> wrappers
    /// off the given expression, returning the peeled inner expression
    /// together with a function that re-wraps a replacement inner
    /// expression in the same enclosing wrappers. The re-wrap function
    /// preserves the let-bindings so any names they introduce remain in
    /// scope around the new inner expression.
    /// </summary>
    private static (Expression Peeled, Func<Expression, Expression> Rewrap)
        PeelLetWrappers(Expression expression)
    {
        var stack = new List<Func<Expression, Expression>>();
        var current = expression;

        while (current is Expression.LetExpression letExpr)
        {
            stack.Add(
                inner =>
                new Expression.LetExpression(
                    Declarations: letExpr.Declarations,
                    Expression: inner));

            current = letExpr.Expression;
        }

        Expression Rewrap(Expression inner)
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

}
