using System;
using System.Collections.Generic;
using System.Collections.Immutable;

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
/// <see cref="ComputeFreeVariables"/> — the set of names referenced inside the
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
        var builder = ImmutableHashSet<string>.Empty.ToBuilder();
        AddNamesBoundByPattern(pattern, builder);
        return builder.ToImmutable();
    }

    private static void AddNamesBoundByPattern(
        Pattern pattern,
        ImmutableHashSet<string>.Builder builder)
    {
        switch (pattern)
        {
            case Pattern.VarPattern varPattern:
                builder.Add(varPattern.Name);
                break;

            case Pattern.ParenthesizedPattern paren:
                AddNamesBoundByPattern(paren.Pattern.Value, builder);
                break;

            case Pattern.AsPattern asPattern:
                builder.Add(asPattern.Name.Value);
                AddNamesBoundByPattern(asPattern.Pattern.Value, builder);
                break;

            case Pattern.NamedPattern namedPattern:
                foreach (var arg in namedPattern.Arguments)
                    AddNamesBoundByPattern(arg.Value, builder);

                break;

            case Pattern.TuplePattern tuplePattern:
                foreach (var element in tuplePattern.Elements)
                    AddNamesBoundByPattern(element.Value, builder);

                break;

            case Pattern.RecordPattern recordPattern:
                foreach (var field in recordPattern.Fields)
                    builder.Add(field.Value);

                break;

            case Pattern.UnConsPattern unCons:
                AddNamesBoundByPattern(unCons.Head.Value, builder);
                AddNamesBoundByPattern(unCons.Tail.Value, builder);
                break;

            case Pattern.ListPattern listPattern:
                foreach (var element in listPattern.Elements)
                    AddNamesBoundByPattern(element.Value, builder);

                break;

            // Patterns that introduce no names.
            case Pattern.AllPattern:
            case Pattern.UnitPattern:
            case Pattern.CharPattern:
            case Pattern.StringPattern:
            case Pattern.IntPattern:
            case Pattern.HexPattern:
            case Pattern.FloatPattern:
                break;

            default:
                throw new NotImplementedException(
                    "AddNamesBoundByPattern does not handle pattern variant: " +
                    pattern.GetType().Name);
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
    public static ImmutableHashSet<string> ComputeFreeVariables(
        Expression expression)
    {
        var builder = ImmutableHashSet<string>.Empty.ToBuilder();
        AddFreeVariables(expression, [], builder);
        return builder.ToImmutable();
    }

    private static void AddFreeVariables(
        Expression expression,
        ImmutableHashSet<string> bound,
        ImmutableHashSet<string>.Builder result)
    {
        switch (expression)
        {
            case Expression.FunctionOrValue funcOrValue:
                if (funcOrValue.ModuleName.Count is 0 && !bound.Contains(funcOrValue.Name))
                    result.Add(funcOrValue.Name);

                break;

            case Expression.Application app:
                foreach (var arg in app.Arguments)
                    AddFreeVariables(arg.Value, bound, result);

                break;

            case Expression.ParenthesizedExpression paren:
                AddFreeVariables(paren.Expression.Value, bound, result);
                break;

            case Expression.IfBlock ifBlock:
                AddFreeVariables(ifBlock.Condition.Value, bound, result);
                AddFreeVariables(ifBlock.ThenBlock.Value, bound, result);
                AddFreeVariables(ifBlock.ElseBlock.Value, bound, result);
                break;

            case Expression.LambdaExpression lambda:
                {
                    var newBound = bound;

                    foreach (var arg in lambda.Lambda.Arguments)
                        newBound = newBound.Union(CollectNamesBoundByPattern(arg.Value));

                    AddFreeVariables(lambda.Lambda.Expression.Value, newBound, result);
                    break;
                }

            case Expression.CaseExpression caseExpr:
                AddFreeVariables(caseExpr.CaseBlock.Expression.Value, bound, result);

                foreach (var branch in caseExpr.CaseBlock.Cases)
                {
                    var branchBound =
                        bound.Union(CollectNamesBoundByPattern(branch.Pattern.Value));

                    AddFreeVariables(branch.Expression.Value, branchBound, result);
                }

                break;

            case Expression.LetExpression letExpr:
                {
                    // All declared names are mutually in scope across the let block,
                    // including each declaration's RHS (Elm allows mutual recursion
                    // among let-function declarations).
                    var declared = bound;

                    foreach (var decl in letExpr.Value.Declarations)
                    {
                        switch (decl.Value)
                        {
                            case Expression.LetDeclaration.LetFunction lf:
                                declared = declared.Add(lf.Function.Declaration.Value.Name.Value);
                                break;

                            case Expression.LetDeclaration.LetDestructuring ld:
                                declared =
                                    declared.Union(CollectNamesBoundByPattern(ld.Pattern.Value));

                                break;

                            default:
                                throw new NotImplementedException(
                                    "AddFreeVariables does not handle let declaration variant: " +
                                    decl.Value.GetType().Name);
                        }
                    }

                    foreach (var decl in letExpr.Value.Declarations)
                    {
                        switch (decl.Value)
                        {
                            case Expression.LetDeclaration.LetFunction lf:
                                {
                                    var inner = declared;

                                    foreach (var arg in lf.Function.Declaration.Value.Arguments)
                                        inner = inner.Union(CollectNamesBoundByPattern(arg.Value));

                                    AddFreeVariables(
                                        lf.Function.Declaration.Value.Expression.Value,
                                        inner,
                                        result);

                                    break;
                                }

                            case Expression.LetDeclaration.LetDestructuring ld:
                                AddFreeVariables(ld.Expression.Value, declared, result);
                                break;

                            default:
                                throw new NotImplementedException(
                                    "AddFreeVariables does not handle let declaration variant: " +
                                    decl.Value.GetType().Name);
                        }
                    }

                    AddFreeVariables(letExpr.Value.Expression.Value, declared, result);
                    break;
                }

            case Expression.ListExpr listExpr:
                foreach (var e in listExpr.Elements)
                    AddFreeVariables(e.Value, bound, result);

                break;

            case Expression.TupledExpression tupled:
                foreach (var e in tupled.Elements)
                    AddFreeVariables(e.Value, bound, result);

                break;

            case Expression.RecordExpr recordExpr:
                foreach (var f in recordExpr.Fields)
                    AddFreeVariables(f.Value.valueExpr.Value, bound, result);

                break;

            case Expression.RecordUpdateExpression recordUpdate:
                if (!bound.Contains(recordUpdate.RecordName.Value))
                    result.Add(recordUpdate.RecordName.Value);

                foreach (var f in recordUpdate.Fields)
                    AddFreeVariables(f.Value.valueExpr.Value, bound, result);

                break;

            case Expression.RecordAccess recordAccess:
                AddFreeVariables(recordAccess.Record.Value, bound, result);
                break;

            case Expression.Negation negation:
                AddFreeVariables(negation.Expression.Value, bound, result);
                break;

            case Expression.OperatorApplication opApp:
                AddFreeVariables(opApp.Left.Value, bound, result);
                AddFreeVariables(opApp.Right.Value, bound, result);
                break;

            // Leaf variants that reference no names.
            case Expression.UnitExpr:
            case Expression.Literal:
            case Expression.CharLiteral:
            case Expression.Integer:
            case Expression.Hex:
            case Expression.Floatable:
            case Expression.PrefixOperator:
            case Expression.RecordAccessFunction:
            case Expression.GLSLExpression:
                break;

            default:
                throw new NotImplementedException(
                    "AddFreeVariables does not handle expression variant: " +
                    expression.GetType().Name);
        }
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
                    var branchLetRhs = RemoveKeys(letRhsByName, branchBindings);

                    AddNamesFlowingIntoAppFunctions(
                        branch.Expression.Value,
                        branchLetRhs,
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
    private static void AddFlowingNamesOf(
        Expression expression,
        ImmutableDictionary<string, Expression> letRhsByName,
        ImmutableHashSet<string> bound,
        ImmutableHashSet<string>.Builder result,
        ImmutableHashSet<string> visited)
    {
        var freeVars = ComputeFreeVariables(expression);

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
}
