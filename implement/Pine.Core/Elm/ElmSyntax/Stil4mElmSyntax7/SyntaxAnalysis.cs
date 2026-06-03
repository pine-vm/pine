using Pine.Core.Elm.ElmSyntax.SyntaxModel;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

namespace Pine.Core.Elm.ElmSyntax.Stil4mElmSyntax7;

/// <summary>
/// Pure data-flow analyses over Elm expression syntax.
///
/// <para>
/// These analyses answer questions of the form
/// "which (free) names contribute to the value computed by this expression?".
/// They operate purely on the Elm syntax — no type information, no whole-program
/// view of declarations, no module resolution.
/// </para>
///
/// <para>
/// The primary entry points are
/// <see cref="CollectRemainingFreeVariables(Expression)"/> — the set of names referenced inside the
/// expression but not bound by any binder strictly within it — and
/// <see cref="ComputeNamesFlowingIntoApplicationFunctions"/> — the set of names that
/// influence the value of <em>any</em> expression that appears in the function
/// position of an application inside the given expression. The latter expands the
/// data flow through enclosing <c>let</c>-bindings: if an application head names a
/// let-bound local, the analysis recursively follows that binding's right-hand
/// side and includes its free variables in the result.
/// </para>
///
/// <para>
/// "Free" means: when the analysis is run on a function body in isolation, the
/// function's own parameters are <em>not</em> bound in that context, so any
/// reference to a parameter name flows into the result. This is the behaviour
/// callers like the higher-order-parameter detector rely on.
/// </para>
/// </summary>
public static class SyntaxAnalysis
{
    /// <summary>
    /// Collects every name that the given <paramref name="pattern"/> introduces
    /// into scope. Includes:
    /// <list type="bullet">
    /// <item><see cref="Pattern.VarPattern"/> directly.</item>
    /// <item>Names from nested patterns inside
    /// <see cref="Pattern.NamedPattern"/> (constructor arguments),
    /// <see cref="Pattern.TuplePattern"/>,
    /// <see cref="Pattern.ListPattern"/> and
    /// <see cref="Pattern.UnConsPattern"/>.</item>
    /// <item>Field names of a <see cref="Pattern.RecordPattern"/>.</item>
    /// <item>The aliased name of an <see cref="Pattern.AsPattern"/>
    /// (in addition to the names of its inner pattern).</item>
    /// <item>Recurses through <see cref="Pattern.ParenthesizedPattern"/>.</item>
    /// </list>
    /// Literal / unit / wildcard patterns introduce nothing.
    /// </summary>
    public static ImmutableHashSet<string> CollectNamesBoundByPattern(
        Pattern pattern)
    {
        var names = new HashSet<string>();
        CollectNamesBoundByPatternInto(pattern, names);
        return [.. names];
    }

    /// <summary>
    /// Convenience union of every name bound by every pattern in
    /// <paramref name="patterns"/> — useful for parameter lists (function arguments,
    /// lambda arguments).
    /// </summary>
    public static ImmutableHashSet<string> CollectNamesBoundByPatterns(
        IReadOnlyList<Node<Pattern>> patterns)
    {
        var names = new HashSet<string>();

        foreach (var patternNode in patterns)
            CollectNamesBoundByPatternInto(patternNode.Value, names);

        return [.. names];
    }

    /// <summary>
    /// Accumulator-form pattern-name walker shared by
    /// <see cref="CollectNamesBoundByPattern"/>,
    /// <see cref="CollectNamesBoundByPatterns"/> and the
    /// <c>HashSet</c>-based mirrors retained on
    /// <c>ElmSyntaxTransformations</c> for legacy mutable-collection call sites.
    /// </summary>
    internal static void CollectNamesBoundByPatternInto(
        Pattern pattern,
        HashSet<string> names)
    {
        switch (pattern)
        {
            case Pattern.VarPattern varPattern:
                names.Add(varPattern.Name);
                break;

            case Pattern.TuplePattern tuplePattern:
                foreach (var elem in tuplePattern.Elements)
                    CollectNamesBoundByPatternInto(elem.Value, names);

                break;

            case Pattern.RecordPattern recordPattern:
                foreach (var field in recordPattern.Fields)
                    names.Add(field.Value);

                break;

            case Pattern.UnConsPattern unconsPattern:
                CollectNamesBoundByPatternInto(unconsPattern.Head.Value, names);
                CollectNamesBoundByPatternInto(unconsPattern.Tail.Value, names);
                break;

            case Pattern.ListPattern listPattern:
                foreach (var elem in listPattern.Elements)
                    CollectNamesBoundByPatternInto(elem.Value, names);

                break;

            case Pattern.NamedPattern namedPattern:
                foreach (var arg in namedPattern.Arguments)
                    CollectNamesBoundByPatternInto(arg.Value, names);

                break;

            case Pattern.AsPattern asPattern:
                names.Add(asPattern.Name.Value);
                CollectNamesBoundByPatternInto(asPattern.Pattern.Value, names);
                break;

            case Pattern.ParenthesizedPattern parenPattern:
                CollectNamesBoundByPatternInto(parenPattern.Pattern.Value, names);
                break;

                // Other pattern variants (literal / unit / wildcard / ...) introduce nothing.
        }
    }

    /// <summary>
    /// Computes the set of free variable names referenced inside
    /// <paramref name="expression"/>. A "free variable" is an unqualified
    /// <see cref="Expression.FunctionOrValue"/> (one with an empty
    /// module name) whose name is <em>not</em> introduced by any binder strictly
    /// within <paramref name="expression"/> (lambda parameters, let-function
    /// parameters, let-function names, let-destructure patterns, case branch
    /// patterns).
    ///
    /// <para>
    /// Names referenced with a non-empty module qualifier are never free in the
    /// expression-local sense — they always resolve to a top-level declaration —
    /// and are therefore excluded from the result.
    /// </para>
    ///
    /// <para>
    /// When the analysis is applied to a function body in isolation, the
    /// function's parameters are unbound in this context and any reference to
    /// them is therefore reported as a free variable.
    /// </para>
    /// </summary>
    /// <summary>
    /// Computes the set of free variable names referenced inside
    /// <paramref name="expression"/>. A "free variable" is an unqualified
    /// <see cref="Expression.FunctionOrValue"/> (one with an empty
    /// module name) whose name is <em>not</em> introduced by any binder strictly
    /// within <paramref name="expression"/> (lambda parameters, let-function
    /// parameters, let-function names, let-destructure patterns, case branch
    /// patterns).
    ///
    /// <para>
    /// Names referenced with a non-empty module qualifier are never free in the
    /// expression-local sense — they always resolve to a top-level declaration —
    /// and are therefore excluded from the result.
    /// </para>
    ///
    /// <para>
    /// When the analysis is applied to a function body in isolation, the
    /// function's parameters are unbound in this context and any reference to
    /// them is therefore reported as a free variable.
    /// </para>
    ///
    /// <para>
    /// The implementation is directly recursive and returns an
    /// <see cref="ImmutableHashSet{T}"/> at every level — no accumulator
    /// parameters are threaded through. At each binding site (lambda,
    /// case arm, let-block), names introduced locally are removed from the
    /// child sub-expression's result before it is returned to the parent.
    /// Outer scopes therefore do not need to be communicated downward.
    /// </para>
    ///
    /// <para>
    /// The walker treats every unqualified <see cref="Expression.FunctionOrValue"/>
    /// reference as contributing to its sub-expression's free-variable set,
    /// regardless of whether an enclosing binder happens to introduce the
    /// same name. As a consequence, a name referenced on an outer expression
    /// that an inner pattern <em>also</em> binds (a "rebinding/refreeing"
    /// shape that lowering passes can produce) is reported correctly as free
    /// at the outer site, even though the inner reference is bound — see
    /// the regression tests in <c>SyntaxAnalysisTests</c>.
    /// </para>
    /// </summary>
    public static ImmutableHashSet<string> CollectRemainingFreeVariables(
        Expression expression)
    {
        switch (expression)
        {
            case Expression.FunctionOrValue funcOrValue when funcOrValue.ModuleName.Count is 0:
                return [funcOrValue.Name];

            // Qualified FunctionOrValue references resolve to a module-level name and
            // contribute no free local variables.
            case Expression.FunctionOrValue:
                return [];

            case Expression.LambdaExpression lambdaExpr:
                {
                    var bound =
                        CollectNamesBoundByPatterns(lambdaExpr.Lambda.Arguments);

                    return
                        CollectRemainingFreeVariables(lambdaExpr.Lambda.Expression.Value)
                        .Except(bound);
                }

            case Expression.CaseExpression caseExpr:
                {
                    var result =
                        CollectRemainingFreeVariables(caseExpr.CaseBlock.Expression.Value);

                    foreach (var caseItem in caseExpr.CaseBlock.Cases)
                    {
                        var armBound =
                            CollectNamesBoundByPattern(caseItem.Pattern.Value);

                        result =
                            result.Union(
                                CollectRemainingFreeVariables(caseItem.Expression.Value)
                                .Except(armBound));
                    }

                    return result;
                }

            case Expression.LetExpression letExpr:
                {
                    // Let bindings are mutually recursive: every name introduced by the
                    // block is in scope for every RHS and for the body. Collect them all
                    // first, then take the union of each child's remaining-free set and
                    // subtract the let-bound names at the end.
                    var letBoundBuilder = ImmutableHashSet.CreateBuilder<string>();

                    foreach (var decl in letExpr.Value.Declarations)
                    {
                        switch (decl.Value)
                        {
                            case Expression.LetDeclaration.LetFunction letFunc:
                                letBoundBuilder.Add(
                                    letFunc.Function.Declaration.Value.Name.Value);

                                break;

                            case Expression.LetDeclaration.LetDestructuring letDestr:
                                foreach (var name in CollectNamesBoundByPattern(letDestr.Pattern.Value))
                                    letBoundBuilder.Add(name);

                                break;
                        }
                    }

                    var letBound = letBoundBuilder.ToImmutable();

                    var result = ImmutableHashSet<string>.Empty;

                    foreach (var decl in letExpr.Value.Declarations)
                    {
                        switch (decl.Value)
                        {
                            case Expression.LetDeclaration.LetFunction letFunc:
                                {
                                    var paramBound =
                                        CollectNamesBoundByPatterns(
                                            letFunc.Function.Declaration.Value.Arguments);

                                    result =
                                        result.Union(
                                            CollectRemainingFreeVariables(
                                                letFunc.Function.Declaration.Value.Expression.Value)
                                            .Except(paramBound));

                                    break;
                                }

                            case Expression.LetDeclaration.LetDestructuring letDestr:
                                result =
                                    result.Union(
                                        CollectRemainingFreeVariables(letDestr.Expression.Value));

                                break;
                        }
                    }

                    result =
                        result.Union(
                            CollectRemainingFreeVariables(letExpr.Value.Expression.Value));

                    return result.Except(letBound);
                }

            case Expression.RecordUpdateExpression recordUpdate:
                {
                    // The record name on the LHS is itself a value reference.
                    var result = ImmutableHashSet.Create(recordUpdate.RecordName.Value);

                    foreach (var field in recordUpdate.Fields)
                        result =
                            result.Union(
                                CollectRemainingFreeVariables(field.Value.valueExpr.Value));

                    return result;
                }

            // Variants with no binding semantics: recurse into every immediate
            // child expression via the shared ForEachChildExpression walker.
            // Each variant is listed explicitly so the throwing default below
            // never fires for valid expression values.
            case Expression.UnitExpr:
            case Expression.Literal:
            case Expression.CharLiteral:
            case Expression.Integer:
            case Expression.Hex:
            case Expression.Floatable:
            case Expression.Negation:
            case Expression.ListExpr:
            case Expression.IfBlock:
            case Expression.PrefixOperator:
            case Expression.ParenthesizedExpression:
            case Expression.Application:
            case Expression.OperatorApplication:
            case Expression.TupledExpression:
            case Expression.RecordExpr:
            case Expression.RecordAccess:
            case Expression.RecordAccessFunction:
            case Expression.GLSLExpression:
                {
                    var result = ImmutableHashSet<string>.Empty;

                    ForEachChildExpression(
                        expression,
                        child =>
                        {
                            result = result.Union(CollectRemainingFreeVariables(child));
                        });

                    return result;
                }

            default:
                throw new NotImplementedException(
                    "CollectRemainingFreeVariables does not handle expression variant: " +
                    expression.GetType().Name);
        }
    }

    /// <summary>
    /// Computes the free variables in a case arm by determining which variables used in the expression are not bound by
    /// the pattern.
    /// </summary>
    /// <param name="caseArm">The case arm to analyze.</param>
    /// <returns>A set of variable names that are free (not bound by the pattern) in the case arm's expression.</returns>
    public static ImmutableHashSet<string> CollectRemainingFreeVariables(
        Case caseArm)
    {
        var expressionFreeVariables =
            CollectRemainingFreeVariables(caseArm.Expression.Value);

        var patternBoundNames =
            CollectNamesBoundByPattern(caseArm.Pattern.Value);

        return
            [.. expressionFreeVariables.Where(name => !patternBoundNames.Contains(name))];
    }


    /// <summary>
    /// Computes the set of names that flow into the function-position expression
    /// of <em>any</em> <see cref="Expression.Application"/> appearing
    /// inside <paramref name="expression"/>.
    ///
    /// <para>
    /// The "function position" of an Application is its first argument
    /// (<c>Application.Arguments[0]</c>) — the expression that is being invoked.
    /// A name <c>n</c> "flows into" the function position if it appears free in
    /// that head expression, <em>or</em> if the head expression references a
    /// local let-bound name whose right-hand side (transitively, through further
    /// let-bindings) has <c>n</c> as a free variable.
    /// </para>
    ///
    /// <para>
    /// Names introduced by lambdas, by case-branch patterns, by let-destructuring
    /// patterns, and by let-function names / parameters are <em>not</em> reported
    /// as flowing in: they are bound inside <paramref name="expression"/>.
    /// References to function parameters, by contrast, <em>are</em> reported,
    /// because parameters are unbound at the top of a function body.
    /// </para>
    /// </summary>
    public static ImmutableHashSet<string> ComputeNamesFlowingIntoApplicationFunctions(
        Expression expression)
    {
        var result = ImmutableHashSet<string>.Empty.ToBuilder();

        AddNamesFlowingIntoAppFunctions(
            expression,
            letRhsByName: [],
            bound: [],
            result: result);

        return result.ToImmutable();
    }

    private static void AddNamesFlowingIntoAppFunctions(
        Expression expression,
        ImmutableDictionary<string, Expression> letRhsByName,
        ImmutableHashSet<string> bound,
        ImmutableHashSet<string>.Builder result)
    {
        switch (expression)
        {
            case Expression.Application app:
                if (app.Arguments.Count > 0)
                {
                    AddFlowingNamesOf(
                        app.Arguments[0].Value,
                        letRhsByName,
                        bound,
                        result,
                        []);
                }
                // Recurse into every argument (including head) so that nested
                // applications are also discovered.
                foreach (var arg in app.Arguments)
                    AddNamesFlowingIntoAppFunctions(arg.Value, letRhsByName, bound, result);

                break;

            case Expression.ParenthesizedExpression paren:
                AddNamesFlowingIntoAppFunctions(paren.Expression.Value, letRhsByName, bound, result);
                break;

            case Expression.IfBlock ifBlock:
                AddNamesFlowingIntoAppFunctions(ifBlock.Condition.Value, letRhsByName, bound, result);
                AddNamesFlowingIntoAppFunctions(ifBlock.ThenBlock.Value, letRhsByName, bound, result);
                AddNamesFlowingIntoAppFunctions(ifBlock.ElseBlock.Value, letRhsByName, bound, result);
                break;

            case Expression.LambdaExpression lambda:
                {
                    var innerBound = bound;

                    foreach (var arg in lambda.Lambda.Arguments)
                        innerBound = innerBound.Union(CollectNamesBoundByPattern(arg.Value));
                    // Lambda parameters shadow any outer let-bindings of the same name.
                    var innerLetRhs = RemoveKeys(letRhsByName, innerBound);

                    AddNamesFlowingIntoAppFunctions(
                        lambda.Lambda.Expression.Value,
                        innerLetRhs,
                        innerBound,
                        result);

                    break;
                }

            case Expression.CaseExpression caseExpr:
                AddNamesFlowingIntoAppFunctions(
                    caseExpr.CaseBlock.Expression.Value,
                    letRhsByName,
                    bound,
                    result);

                foreach (var branch in caseExpr.CaseBlock.Cases)
                {
                    var branchBindings = CollectNamesBoundByPattern(branch.Pattern.Value);
                    var branchBound = bound.Union(branchBindings);
                    // Over-approximate data flow from the scrutinee into the
                    // branch body: every name bound by the branch pattern
                    // may carry information from the scrutinee, so map each
                    // bound name to the scrutinee expression in
                    // letRhsByName. This mirrors the LetDestructuring
                    // handling above and lets AddFlowingNamesOf resolve a
                    // later use of one of those names back to whatever the
                    // scrutinee referenced (e.g. a higher-order parameter
                    // of the enclosing function).
                    var branchLetRhsBuilder = letRhsByName.ToBuilder();

                    foreach (var n in branchBindings)
                        branchLetRhsBuilder[n] = caseExpr.CaseBlock.Expression.Value;

                    AddNamesFlowingIntoAppFunctions(
                        branch.Expression.Value,
                        branchLetRhsBuilder.ToImmutable(),
                        branchBound,
                        result);
                }

                break;

            case Expression.LetExpression letExpr:
                {
                    var declaredNames = ImmutableHashSet<string>.Empty.ToBuilder();
                    var letRhsBuilder = letRhsByName.ToBuilder();

                    foreach (var decl in letExpr.Value.Declarations)
                    {
                        switch (decl.Value)
                        {
                            case Expression.LetDeclaration.LetFunction lf:
                                {
                                    var name = lf.Function.Declaration.Value.Name.Value;
                                    declaredNames.Add(name);

                                    // For let-functions with parameters, model the binding's value as a
                                    // lambda over those parameters with the function body. This way,
                                    // calling the let-function from elsewhere flows through to the body.
                                    var paramArgs = lf.Function.Declaration.Value.Arguments;

                                    if (paramArgs.Count is 0)
                                    {
                                        letRhsBuilder[name] = lf.Function.Declaration.Value.Expression.Value;
                                    }
                                    else
                                    {
                                        // Wrap as a synthetic lambda so the flow analysis treats the
                                        // let-function name like any other value binding.
                                        var lambdaStruct =
                                            new LambdaStruct(
                                                Arguments: paramArgs,
                                                Expression: lf.Function.Declaration.Value.Expression);

                                        letRhsBuilder[name] =
                                            new Expression.LambdaExpression(lambdaStruct);
                                    }

                                    break;
                                }

                            case Expression.LetDeclaration.LetDestructuring ld:
                                {
                                    var bindings = CollectNamesBoundByPattern(ld.Pattern.Value);

                                    foreach (var n in bindings)
                                    {
                                        declaredNames.Add(n);
                                        // We don't know precisely how a destructured name relates to
                                        // the RHS, so over-approximate by mapping each bound name to
                                        // the full RHS. Flow into any of them treats the whole RHS as
                                        // contributing.
                                        letRhsBuilder[n] = ld.Expression.Value;
                                    }

                                    break;
                                }

                            default:
                                throw new NotImplementedException(
                                    "AddNamesFlowingIntoAppFunctions does not handle let declaration variant: " +
                                    decl.Value.GetType().Name);
                        }
                    }

                    var newBound = bound.Union(declaredNames.ToImmutable());
                    var newLetRhs = letRhsBuilder.ToImmutable();

                    // Recurse into RHS expressions and the let body, all with the
                    // expanded scope so mutual references inside the let are seen.
                    foreach (var decl in letExpr.Value.Declarations)
                    {
                        switch (decl.Value)
                        {
                            case Expression.LetDeclaration.LetFunction lf:
                                {
                                    var innerBound = newBound;

                                    foreach (var arg in lf.Function.Declaration.Value.Arguments)
                                        innerBound = innerBound.Union(CollectNamesBoundByPattern(arg.Value));

                                    var innerLetRhs = RemoveKeys(newLetRhs, innerBound);

                                    AddNamesFlowingIntoAppFunctions(
                                        lf.Function.Declaration.Value.Expression.Value,
                                        innerLetRhs,
                                        innerBound,
                                        result);

                                    break;
                                }

                            case Expression.LetDeclaration.LetDestructuring ld:
                                AddNamesFlowingIntoAppFunctions(
                                    ld.Expression.Value,
                                    newLetRhs,
                                    newBound,
                                    result);

                                break;
                        }
                    }

                    AddNamesFlowingIntoAppFunctions(
                        letExpr.Value.Expression.Value,
                        newLetRhs,
                        newBound,
                        result);

                    break;
                }

            case Expression.OperatorApplication opApp:
                AddNamesFlowingIntoAppFunctions(opApp.Left.Value, letRhsByName, bound, result);
                AddNamesFlowingIntoAppFunctions(opApp.Right.Value, letRhsByName, bound, result);
                break;

            case Expression.ListExpr listExpr:
                foreach (var e in listExpr.Elements)
                    AddNamesFlowingIntoAppFunctions(e.Value, letRhsByName, bound, result);

                break;

            case Expression.TupledExpression tupled:
                foreach (var e in tupled.Elements)
                    AddNamesFlowingIntoAppFunctions(e.Value, letRhsByName, bound, result);

                break;

            case Expression.RecordExpr recordExpr:
                foreach (var f in recordExpr.Fields)
                    AddNamesFlowingIntoAppFunctions(
                        f.Value.valueExpr.Value,
                        letRhsByName,
                        bound,
                        result);

                break;

            case Expression.RecordUpdateExpression recordUpdate:
                foreach (var f in recordUpdate.Fields)
                    AddNamesFlowingIntoAppFunctions(
                        f.Value.valueExpr.Value,
                        letRhsByName,
                        bound,
                        result);

                break;

            case Expression.RecordAccess recordAccess:
                AddNamesFlowingIntoAppFunctions(
                    recordAccess.Record.Value,
                    letRhsByName,
                    bound,
                    result);

                break;

            case Expression.Negation negation:
                AddNamesFlowingIntoAppFunctions(
                    negation.Expression.Value,
                    letRhsByName,
                    bound,
                    result);

                break;

            case Expression.UnitExpr:
            case Expression.Literal:
            case Expression.CharLiteral:
            case Expression.Integer:
            case Expression.Hex:
            case Expression.Floatable:
            case Expression.FunctionOrValue:
            case Expression.PrefixOperator:
            case Expression.RecordAccessFunction:
            case Expression.GLSLExpression:
                break;

            default:
                throw new NotImplementedException(
                    "AddNamesFlowingIntoAppFunctions does not handle expression variant: " +
                    expression.GetType().Name);
        }
    }

    /// <summary>
    /// Collects names that flow into the value of <paramref name="expression"/>,
    /// transitively expanding any free reference to a let-bound name through
    /// the recorded right-hand sides in <paramref name="letRhsByName"/>.
    ///
    /// Names that are still bound (in <paramref name="bound"/>) inside the
    /// enclosing scope of the original analysis target are <em>not</em> reported.
    /// The <paramref name="visited"/> set guards against infinite recursion for
    /// mutually recursive let-bindings.
    /// </summary>
    internal static void AddFlowingNamesOf(
        Expression expression,
        ImmutableDictionary<string, Expression> letRhsByName,
        ImmutableHashSet<string> bound,
        ImmutableHashSet<string>.Builder result,
        ImmutableHashSet<string> visited)
    {
        var freeVars = CollectRemainingFreeVariables(expression);

        foreach (var name in freeVars)
        {
            if (letRhsByName.TryGetValue(name, out var rhs))
            {
                if (visited.Contains(name))
                    continue;

                AddFlowingNamesOf(
                    rhs,
                    letRhsByName,
                    bound,
                    result,
                    visited.Add(name));
            }
            else if (!bound.Contains(name))
            {
                result.Add(name);
            }
        }
    }

    private static ImmutableDictionary<string, Expression> RemoveKeys(
        ImmutableDictionary<string, Expression> dict,
        IEnumerable<string> keys)
    {
        var builder = dict.ToBuilder();

        foreach (var k in keys)
            builder.Remove(k);

        return builder.ToImmutable();
    }

    /// <summary>
    /// Generalised walker that traverses <paramref name="expression"/> and
    /// invokes <paramref name="onApplication"/> at every
    /// <see cref="Expression.Application"/> node it encounters, threading the
    /// current let-binding right-hand-side map and the set of names bound in
    /// the enclosing scopes — the same information that
    /// <see cref="AddNamesFlowingIntoAppFunctions"/> threads — so the callback
    /// can run flow analyses such as <see cref="AddFlowingNamesOf"/> against
    /// arbitrary sub-expressions of the Application (e.g. argument positions,
    /// not just the head).
    /// <para>
    /// Visitor shape mirrors <see cref="AddNamesFlowingIntoAppFunctions"/>
    /// (lambdas / let-functions shadow outer let bindings, case branches
    /// extend <c>bound</c> by branch patterns, etc.) so callers get the
    /// exact same scoping semantics as the existing
    /// <see cref="ComputeNamesFlowingIntoApplicationFunctions"/>.
    /// </para>
    /// </summary>
    internal static void VisitApplications(
        Expression expression,
        ImmutableDictionary<string, Expression> letRhsByName,
        ImmutableHashSet<string> bound,
        Action<Expression.Application, ImmutableDictionary<string, Expression>, ImmutableHashSet<string>> onApplication)
    {
        switch (expression)
        {
            case Expression.Application app:
                onApplication(app, letRhsByName, bound);

                foreach (var arg in app.Arguments)
                    VisitApplications(arg.Value, letRhsByName, bound, onApplication);

                break;

            case Expression.ParenthesizedExpression paren:
                VisitApplications(paren.Expression.Value, letRhsByName, bound, onApplication);
                break;

            case Expression.IfBlock ifBlock:
                VisitApplications(ifBlock.Condition.Value, letRhsByName, bound, onApplication);
                VisitApplications(ifBlock.ThenBlock.Value, letRhsByName, bound, onApplication);
                VisitApplications(ifBlock.ElseBlock.Value, letRhsByName, bound, onApplication);
                break;

            case Expression.LambdaExpression lambda:
                {
                    var innerBound = bound;

                    foreach (var arg in lambda.Lambda.Arguments)
                        innerBound = innerBound.Union(CollectNamesBoundByPattern(arg.Value));

                    var innerLetRhs = RemoveKeys(letRhsByName, innerBound);

                    VisitApplications(
                        lambda.Lambda.Expression.Value,
                        innerLetRhs,
                        innerBound,
                        onApplication);

                    break;
                }

            case Expression.CaseExpression caseExpr:
                VisitApplications(
                    caseExpr.CaseBlock.Expression.Value,
                    letRhsByName,
                    bound,
                    onApplication);

                foreach (var branch in caseExpr.CaseBlock.Cases)
                {
                    var branchBindings = CollectNamesBoundByPattern(branch.Pattern.Value);
                    var branchBound = bound.Union(branchBindings);
                    // See AddNamesFlowingIntoAppFunctions: extend
                    // letRhsByName with branch-pattern bindings mapped to
                    // the scrutinee, so flow analyses run against the
                    // branch body can trace through case bindings.
                    var branchLetRhsBuilder = letRhsByName.ToBuilder();

                    foreach (var n in branchBindings)
                        branchLetRhsBuilder[n] = caseExpr.CaseBlock.Expression.Value;

                    VisitApplications(
                        branch.Expression.Value,
                        branchLetRhsBuilder.ToImmutable(),
                        branchBound,
                        onApplication);
                }

                break;

            case Expression.LetExpression letExpr:
                {
                    var declaredNames = ImmutableHashSet<string>.Empty.ToBuilder();
                    var letRhsBuilder = letRhsByName.ToBuilder();

                    foreach (var decl in letExpr.Value.Declarations)
                    {
                        switch (decl.Value)
                        {
                            case Expression.LetDeclaration.LetFunction lf:
                                {
                                    var name = lf.Function.Declaration.Value.Name.Value;
                                    declaredNames.Add(name);

                                    var paramArgs = lf.Function.Declaration.Value.Arguments;

                                    if (paramArgs.Count is 0)
                                    {
                                        letRhsBuilder[name] = lf.Function.Declaration.Value.Expression.Value;
                                    }
                                    else
                                    {
                                        var lambdaStruct =
                                            new LambdaStruct(
                                                Arguments: paramArgs,
                                                Expression: lf.Function.Declaration.Value.Expression);

                                        letRhsBuilder[name] =
                                            new Expression.LambdaExpression(lambdaStruct);
                                    }

                                    break;
                                }

                            case Expression.LetDeclaration.LetDestructuring ld:
                                {
                                    var bindings = CollectNamesBoundByPattern(ld.Pattern.Value);

                                    foreach (var n in bindings)
                                    {
                                        declaredNames.Add(n);
                                        letRhsBuilder[n] = ld.Expression.Value;
                                    }

                                    break;
                                }

                            default:
                                throw new NotImplementedException(
                                    "VisitApplications does not handle let declaration variant: " +
                                    decl.Value.GetType().Name);
                        }
                    }

                    var newBound = bound.Union(declaredNames.ToImmutable());
                    var newLetRhs = letRhsBuilder.ToImmutable();

                    foreach (var decl in letExpr.Value.Declarations)
                    {
                        switch (decl.Value)
                        {
                            case Expression.LetDeclaration.LetFunction lf:
                                {
                                    var innerBound = newBound;

                                    foreach (var arg in lf.Function.Declaration.Value.Arguments)
                                        innerBound = innerBound.Union(CollectNamesBoundByPattern(arg.Value));

                                    var innerLetRhs = RemoveKeys(newLetRhs, innerBound);

                                    VisitApplications(
                                        lf.Function.Declaration.Value.Expression.Value,
                                        innerLetRhs,
                                        innerBound,
                                        onApplication);

                                    break;
                                }

                            case Expression.LetDeclaration.LetDestructuring ld:
                                VisitApplications(
                                    ld.Expression.Value,
                                    newLetRhs,
                                    newBound,
                                    onApplication);

                                break;
                        }
                    }

                    VisitApplications(
                        letExpr.Value.Expression.Value,
                        newLetRhs,
                        newBound,
                        onApplication);

                    break;
                }

            case Expression.OperatorApplication opApp:
                VisitApplications(opApp.Left.Value, letRhsByName, bound, onApplication);
                VisitApplications(opApp.Right.Value, letRhsByName, bound, onApplication);
                break;

            case Expression.ListExpr listExpr:
                foreach (var e in listExpr.Elements)
                    VisitApplications(e.Value, letRhsByName, bound, onApplication);

                break;

            case Expression.TupledExpression tupled:
                foreach (var e in tupled.Elements)
                    VisitApplications(e.Value, letRhsByName, bound, onApplication);

                break;

            case Expression.RecordExpr recordExpr:
                foreach (var f in recordExpr.Fields)
                    VisitApplications(f.Value.valueExpr.Value, letRhsByName, bound, onApplication);

                break;

            case Expression.RecordUpdateExpression recordUpdate:
                foreach (var f in recordUpdate.Fields)
                    VisitApplications(f.Value.valueExpr.Value, letRhsByName, bound, onApplication);

                break;

            case Expression.RecordAccess recordAccess:
                VisitApplications(recordAccess.Record.Value, letRhsByName, bound, onApplication);
                break;

            case Expression.Negation negation:
                VisitApplications(negation.Expression.Value, letRhsByName, bound, onApplication);
                break;

            case Expression.UnitExpr:
            case Expression.Literal:
            case Expression.CharLiteral:
            case Expression.Integer:
            case Expression.Hex:
            case Expression.Floatable:
            case Expression.FunctionOrValue:
            case Expression.PrefixOperator:
            case Expression.RecordAccessFunction:
            case Expression.GLSLExpression:
                break;

            default:
                throw new NotImplementedException(
                    "VisitApplications does not handle expression variant: " +
                    expression.GetType().Name);
        }
    }

    /// <summary>
    /// Counts the number of expression nodes in the AST, up to a maximum.
    /// Uses an iterative worklist to avoid stack overflow.
    /// </summary>
    internal static int CountExpressionNodes(Expression body, int max = 3000)
    {
        return
            EnumerateDescendantsNodes(body)
            .Take(max)
            .Count();
    }

    /// <summary>
    /// Enumerates expression nodes in the AST, up to a maximum.
    /// Uses an iterative worklist to avoid stack overflow.
    /// </summary>
    internal static IEnumerable<Expression> EnumerateDescendantsNodes(
        Expression body)
    {
        var worklist = new Stack<Expression>();

        worklist.Push(body);

        while (worklist.Count > 0)
        {
            var current = worklist.Pop();

            var children = new List<Expression>();

            ForEachChildExpression(
                current,
                children.Add);

            foreach (var child in children)
            {
                yield return child;

                worklist.Push(child);
            }
        }
    }

    /// <summary>
    /// Enqueues all immediate child expressions of an expression node onto the given worklist.
    /// </summary>
    internal static void EnqueueChildExpressions(
        Expression expr,
        Stack<Expression> worklist) =>
        ForEachChildExpression(
            expr,
            worklist.Push);

    /// <summary>
    /// Invokes the given delegate for each direct child expression.
    /// </summary>
    internal static void ForEachChildExpression(
        Expression expr,
        Action<Expression> reportChild)
    {
        switch (expr)
        {
            case Expression.Application app:
                foreach (var arg in app.Arguments)
                    reportChild(arg.Value);

                break;

            case Expression.ParenthesizedExpression paren:
                reportChild(paren.Expression.Value);
                break;

            case Expression.IfBlock ifBlock:
                reportChild(ifBlock.Condition.Value);
                reportChild(ifBlock.ThenBlock.Value);
                reportChild(ifBlock.ElseBlock.Value);
                break;

            case Expression.CaseExpression caseExpr:
                reportChild(caseExpr.CaseBlock.Expression.Value);

                foreach (var c in caseExpr.CaseBlock.Cases)
                    reportChild(c.Expression.Value);

                break;

            case Expression.LetExpression letExpr:
                foreach (var decl in letExpr.Value.Declarations)
                {
                    switch (decl.Value)
                    {
                        case Expression.LetDeclaration.LetFunction lf:
                            reportChild(lf.Function.Declaration.Value.Expression.Value);
                            break;

                        case Expression.LetDeclaration.LetDestructuring ld:
                            reportChild(ld.Expression.Value);
                            break;

                        default:
                            throw new NotImplementedException(
                                "EnqueueChildExpressions does not handle let declaration variant: " +
                                decl.Value.GetType().Name);
                    }
                }

                reportChild(letExpr.Value.Expression.Value);
                break;

            case Expression.LambdaExpression lambda:
                reportChild(lambda.Lambda.Expression.Value);
                break;

            case Expression.ListExpr listExpr:
                foreach (var e in listExpr.Elements)
                    reportChild(e.Value);

                break;

            case Expression.TupledExpression tupled:
                foreach (var e in tupled.Elements)
                    reportChild(e.Value);

                break;

            case Expression.RecordExpr recordExpr:
                foreach (var f in recordExpr.Fields)
                    reportChild(f.Value.valueExpr.Value);

                break;

            case Expression.RecordUpdateExpression recordUpdate:
                foreach (var f in recordUpdate.Fields)
                    reportChild(f.Value.valueExpr.Value);

                break;

            case Expression.RecordAccess recordAccess:
                reportChild(recordAccess.Record.Value);
                break;

            case Expression.Negation negation:
                reportChild(negation.Expression.Value);
                break;

            case Expression.OperatorApplication opApp:
                reportChild(opApp.Left.Value);
                reportChild(opApp.Right.Value);
                break;

            // Leaf expression variants: no child expressions to enqueue.
            case Expression.UnitExpr:
            case Expression.Literal:
            case Expression.CharLiteral:
            case Expression.Integer:
            case Expression.Hex:
            case Expression.Floatable:
            case Expression.FunctionOrValue:
            case Expression.PrefixOperator:
            case Expression.RecordAccessFunction:
            case Expression.GLSLExpression:
                break;

            default:
                throw new NotImplementedException(
                    "EnqueueChildExpressions does not handle expression variant: " + expr.GetType().Name);
        }
    }

    // =========================================================================
    // Tag prediction helpers — used by the case-block-consolidation pass
    // (see explore/internal-analysis/2026-05-20-elm-syntax-case-block-consolidation.md).
    // These analyses determine, for a given expression, whether its outermost
    // value-level constructor (tag) is statically pinned to a specific
    // <see cref="QualifiedNameRef"/>.
    // =========================================================================

    /// <summary>
    /// Returns the statically-known outermost constructor tag of
    /// <paramref name="expression"/>, or <see langword="null"/> if the
    /// tag cannot be locally determined.
    ///
    /// <para>
    /// Peels <see cref="Expression.ParenthesizedExpression"/> and
    /// <see cref="Expression.LetExpression"/> wrappers (the let-bindings
    /// do not affect the value-level tag of the in-expression).
    /// Recognises:
    /// </para>
    /// <list type="bullet">
    /// <item>Bare <see cref="Expression.FunctionOrValue"/> whose name
    /// begins with an uppercase letter — i.e. a zero-arity constructor
    /// reference (e.g. <c>Maybe.Nothing</c>, <c>Basics.True</c>).</item>
    /// <item><see cref="Expression.Application"/> whose head is such an
    /// uppercase <see cref="Expression.FunctionOrValue"/> — i.e. a
    /// constructor applied to one or more arguments.</item>
    /// <item><see cref="Expression.IfBlock"/> only if both branches
    /// recursively predict the same tag (rare — the more useful entry
    /// point for if-chains is
    /// <see cref="EnumerateConstructorTaggedLeavesOfIfChain"/>).</item>
    /// </list>
    /// <para>
    /// Returns <see langword="null"/> for any other shape (lambda,
    /// record literal, case expression, function call to a lowercase
    /// name, <c>Pine_kernel</c>/<c>Pine_builtin</c> primitive, etc.).
    /// </para>
    /// </summary>
    public static QualifiedNameRef? TryPredictOutermostConstructorTag(
        Expression expression)
    {
        // Peel transparent wrappers that do not change the tag.
        while (true)
        {
            switch (expression)
            {
                case Expression.ParenthesizedExpression paren:
                    expression = paren.Expression.Value;
                    continue;

                case Expression.LetExpression letExpr:
                    expression = letExpr.Value.Expression.Value;
                    continue;
            }

            break;
        }

        switch (expression)
        {
            case Expression.FunctionOrValue fov
            when fov.Name.Length > 0 && char.IsUpper(fov.Name[0]):
                return new QualifiedNameRef(fov.ModuleName, fov.Name);

            case Expression.Application app
            when app.Arguments.Count > 0
                  && UnwrapParenthesized(app.Arguments[0].Value) is Expression.FunctionOrValue headFov
                  && headFov.Name.Length > 0
                  && char.IsUpper(headFov.Name[0]):

                return new QualifiedNameRef(headFov.ModuleName, headFov.Name);

            case Expression.IfBlock ifBlock:
                {
                    var thenTag = TryPredictOutermostConstructorTag(ifBlock.ThenBlock.Value);

                    if (thenTag is null)
                        return null;

                    var elseTag = TryPredictOutermostConstructorTag(ifBlock.ElseBlock.Value);

                    if (elseTag is null)
                        return null;

                    return thenTag.Equals(elseTag) ? thenTag : null;
                }
        }

        return null;
    }

    /// <summary>
    /// Enumerates the constructor-tagged leaves of an
    /// <see cref="Expression.IfBlock"/>-rooted expression. Each leaf is
    /// reported together with the expression that produces it (suitable
    /// for substitution at the leaf position) and the predicted tag.
    ///
    /// <para>
    /// Peels <see cref="Expression.ParenthesizedExpression"/> and
    /// <see cref="Expression.LetExpression"/> wrappers at the root
    /// before recognising the <see cref="Expression.IfBlock"/> shape.
    /// Within the chain, <c>else if</c> arms are walked recursively
    /// (an <c>else if</c> is just an <see cref="Expression.IfBlock"/>
    /// in the else-position).
    /// </para>
    ///
    /// <para>
    /// Returns <see langword="null"/> if the root is not an
    /// <see cref="Expression.IfBlock"/> after peeling, or if any leaf
    /// fails <see cref="TryPredictOutermostConstructorTag"/>. The
    /// returned list preserves the textual order of the leaves
    /// (then-branch, else-if then-branch, …, final else-branch).
    /// </para>
    /// </summary>
    public static IReadOnlyList<(Expression LeafExpression, QualifiedNameRef PredictedTag)>?
        EnumerateConstructorTaggedLeavesOfIfChain(Expression expression)
    {
        // Peel transparent wrappers at the root only — leaves carry
        // their own (possibly wrapped) expression unchanged.
        var root = expression;

        while (true)
        {
            switch (root)
            {
                case Expression.ParenthesizedExpression paren:
                    root = paren.Expression.Value;
                    continue;

                case Expression.LetExpression letExpr:
                    root = letExpr.Value.Expression.Value;
                    continue;
            }

            break;
        }

        if (root is not Expression.IfBlock)
            return null;

        var leaves = new List<(Expression, QualifiedNameRef)>();

        if (!TryCollectLeavesOfIfChain(root, leaves))
            return null;

        return leaves;
    }

    private static bool TryCollectLeavesOfIfChain(
        Expression expression,
        List<(Expression, QualifiedNameRef)> leaves)
    {
        // Peel parenthesised wrappers at every chain position. We do NOT
        // peel `LetExpression` here, because a `let … in e` at a leaf
        // position must remain as the leaf expression so the
        // consolidation pass can substitute it whole (its bindings may
        // be referenced inside the leaf's prediction target).
        var current = expression;

        while (current is Expression.ParenthesizedExpression paren)
        {
            current = paren.Expression.Value;
        }

        if (current is Expression.IfBlock ifBlock)
        {
            if (!TryCollectLeavesOfIfChain(ifBlock.ThenBlock.Value, leaves))
                return false;

            if (!TryCollectLeavesOfIfChain(ifBlock.ElseBlock.Value, leaves))
                return false;

            return true;
        }

        var tag = TryPredictOutermostConstructorTag(current);

        if (tag is null)
            return false;

        // Preserve the original expression (with any peeled parens
        // wrapping the predicted constructor) so callers receive a
        // shape they can substitute back into the tree verbatim.
        leaves.Add((expression, tag));

        return true;
    }

    /// <summary>
    /// Variant of <see cref="TryPredictOutermostConstructorTag"/> that
    /// also accepts the aliased-pattern case: when the arm body is a
    /// bare reference to a name introduced by an
    /// <see cref="Pattern.AsPattern"/> whose inner pattern carries a
    /// statically-known tag (via
    /// <c>ElmSyntaxTransformations.TryUnwrapToNamedPattern</c>), the
    /// aliased binding's tag is inferred from the surrounding match.
    ///
    /// <para>
    /// Two cases are recognised:
    /// </para>
    /// <list type="bullet">
    /// <item>
    /// (a) <paramref name="armBody"/> itself satisfies
    /// <see cref="TryPredictOutermostConstructorTag"/> — return that tag.
    /// </item>
    /// <item>
    /// (b) <paramref name="armBody"/> is, after peeling
    /// <see cref="Expression.ParenthesizedExpression"/>, a bare
    /// <see cref="Expression.FunctionOrValue"/> whose module name is
    /// empty and whose value name matches the alias introduced by
    /// <paramref name="armPattern"/> (after peeling
    /// <see cref="Pattern.ParenthesizedPattern"/>) — return the tag
    /// of the alias's inner pattern, taken from the alias's first
    /// reachable <see cref="Pattern.NamedPattern"/>.
    /// </item>
    /// </list>
    /// <para>
    /// Returns <see langword="null"/> for any other shape.
    /// </para>
    /// </summary>
    public static QualifiedNameRef? TryPredictOutermostConstructorTagInArmBody(
        Pattern armPattern,
        Expression armBody)
    {
        if (TryPredictOutermostConstructorTag(armBody) is { } directTag)
            return directTag;

        // Case (b): bare reference to an alias-bound name.
        var peeledBody = UnwrapParenthesized(armBody);

        if (peeledBody is not Expression.FunctionOrValue fov)
            return null;

        if (fov.ModuleName.Count > 0)
            return null;

        // Peel ParenthesizedPattern at the arm-pattern root, but do NOT
        // descend through AsPattern's inner pattern here — we are looking
        // for the alias name carried by the outermost AsPattern.
        var peeledPattern = armPattern;

        while (peeledPattern is Pattern.ParenthesizedPattern pp)
        {
            peeledPattern = pp.Pattern.Value;
        }

        if (peeledPattern is not Pattern.AsPattern asPattern)
            return null;

        if (!string.Equals(asPattern.Name.Value, fov.Name, StringComparison.Ordinal))
            return null;

        // Use the same alias-aware unwrap as the inlining pipeline so
        // ``((Wrap _) as alias)``-style nested aliases also resolve.
        var inner =
            ElmCompilerInDotnet.ElmSyntaxTransformations.TryUnwrapToNamedPattern(
                asPattern.Pattern.Value);

        if (inner is null)
            return null;

        return inner.Name;
    }

    /// <summary>
    /// Yields the "return leaves" reachable from <paramref name="expression"/>
    /// by walking through <see cref="Expression.LetExpression"/>
    /// bodies, the <see cref="Expression.IfBlock"/> then/else
    /// arms, and every <see cref="Expression.CaseExpression"/>
    /// branch body. Parenthesised wrappers are peeled. Stops at any other
    /// expression and yields it unchanged.
    /// </summary>
    public static IEnumerable<Expression> EnumerateReturnLeaves(
        Expression expression)
    {
        var stack = new Stack<Expression>();
        stack.Push(expression);

        while (stack.Count > 0)
        {
            var current = UnwrapParenthesized(stack.Pop());

            switch (current)
            {
                case Expression.LetExpression letExpr:
                    stack.Push(letExpr.Value.Expression.Value);
                    break;

                case Expression.IfBlock ifBlock:
                    stack.Push(ifBlock.ElseBlock.Value);
                    stack.Push(ifBlock.ThenBlock.Value);
                    break;

                case Expression.CaseExpression caseExpr:
                    if (caseExpr.CaseBlock.Cases.Count is 0)
                    {
                        // Yield as a non-leaf so callers can decide.
                        yield return current;
                        break;
                    }

                    foreach (var arm in caseExpr.CaseBlock.Cases.Reverse())
                        stack.Push(arm.Expression.Value);

                    break;

                default:
                    yield return current;
                    break;
            }
        }
    }

    /// <summary>
    /// Recursively removes parenthesized pattern wrappers from a pattern.
    /// </summary>
    public static Pattern UnwrapParenthesized(Pattern pattern)
    {
        return pattern switch
        {
            Pattern.ParenthesizedPattern pp => UnwrapParenthesized(pp.Pattern.Value),

            _ =>
            pattern
        };
    }

    /// <summary>
    /// Unwraps nested ParenthesizedExpression wrappers to get the inner expression.
    /// </summary>
    public static Expression UnwrapParenthesized(Expression expression)
    {
        while (expression is Expression.ParenthesizedExpression paren)
        {
            expression = paren.Expression.Value;
        }

        return expression;
    }
}
