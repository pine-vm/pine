using Pine.Core.CodeAnalysis;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

using SyntaxTypes = Pine.Core.Elm.ElmSyntax.ElmSyntaxAbstract;

namespace Pine.Core.Elm.ElmSyntax.ElmSyntaxAbstract;

/// <summary>
/// Analysis utilities for Elm syntax trees.
/// </summary>
public static class SyntaxAnalysis
{
    /// <summary>
    /// A constructor (tag) application deconstructed into its qualified constructor
    /// name and argument (field) expressions.
    /// </summary>
    public sealed record ConstructorApplication(
        DeclQualifiedName ConstructorName,
        IReadOnlyList<Expression> FieldExpressions);

    internal sealed record FlowBinding(
        Expression Expression,
        ImmutableHashSet<string> LocallyBoundNames);

    /// <summary>
    /// Attempts to view an expression as a constructor (tag) application: either a bare
    /// <see cref="Expression.Identifier"/> (zero-arity constructor) or an
    /// <see cref="Expression.Application"/> whose head function is a
    /// <see cref="Expression.Identifier"/>. Returns <see langword="null"/>
    /// for any other shape.
    /// </summary>
    public static ConstructorApplication? TryDeconstructConstructorApplication(
        Expression expr)
    {
        switch (expr)
        {
            case Expression.Identifier funcOrValue:
                return new ConstructorApplication(funcOrValue.QualifiedName, []);

            case Expression.Application app
            when app.Function is Expression.Identifier constructorRef:
                return new ConstructorApplication(constructorRef.QualifiedName, app.Arguments);

            default:
                return null;
        }
    }

    /// <summary>
    /// Determines whether a constructor reference in a pattern
    /// (<see cref="QualifiedNameRef"/>) is equivalent to a constructor name
    /// carried by a value expression (<see cref="DeclQualifiedName"/>). Two names are
    /// equivalent when the local declaration names match and either the pattern reference is
    /// unqualified or the module paths are identical.
    /// </summary>
    public static bool AreEquivalentConstructorNames(
        QualifiedNameRef left,
        DeclQualifiedName right) =>
        left.Name == right.DeclName &&
        (left.ModuleName.Count is 0 || left.ModuleName.SequenceEqual(right.Namespaces));

    /// <summary>
    /// Resolves a <see cref="Expression.Identifier"/> reference into a
    /// fully-qualified name. References without an explicit module qualifier (empty
    /// <see cref="DeclQualifiedName.Namespaces"/>) are interpreted as belonging to the
    /// declaring module <paramref name="currentModuleName"/>.
    /// </summary>
    public static DeclQualifiedName ResolveReference(
        Expression.Identifier reference,
        IReadOnlyList<string> currentModuleName) =>
        reference.QualifiedName.Namespaces.Count is 0
        ?
        DeclQualifiedName.Create(currentModuleName, reference.QualifiedName.DeclName)
        :
        reference.QualifiedName;

    // =====================================================================
    // Pure data-flow analyses over the abstract Expression / Pattern model.
    // Abstract-model counterparts of the corresponding members of the
    // concrete-model <see cref="Elm.ElmSyntax.Stil4mElmSyntax7.SyntaxAnalysis"/>.
    // As noted on the type doc comment, the abstract model has no
    // Node&lt;T&gt; wrappers to thread and no ParenthesizedExpression /
    // ParenthesizedPattern layers to peel, so every walker below is a
    // direct structural simplification of its concrete analogue.
    // =====================================================================

    /// <summary>
    /// Collects every name that the given <paramref name="pattern"/> introduces into scope.
    /// </summary>
    public static ImmutableHashSet<string> CollectNamesBoundByPattern(
        Pattern pattern)
    {
        var names = new HashSet<string>();
        CollectNamesBoundByPatternInto(pattern, names);
        return [.. names];
    }

    /// <summary>
    /// Convenience union of every name bound by every pattern in <paramref name="patterns"/> —
    /// useful for parameter lists (function arguments, lambda arguments).
    /// </summary>
    public static ImmutableHashSet<string> CollectNamesBoundByPatterns(
        IReadOnlyList<Pattern> patterns)
    {
        var names = new HashSet<string>();

        foreach (var pattern in patterns)
            CollectNamesBoundByPatternInto(pattern, names);

        return [.. names];
    }

    private static void CollectNamesBoundByPatternInto(
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
                    CollectNamesBoundByPatternInto(elem, names);

                break;

            case Pattern.RecordPattern recordPattern:
                foreach (var field in recordPattern.Fields)
                    names.Add(field.FieldName);

                break;

            case Pattern.UnConsPattern unconsPattern:
                CollectNamesBoundByPatternInto(unconsPattern.Head, names);
                CollectNamesBoundByPatternInto(unconsPattern.Tail, names);
                break;

            case Pattern.ListPattern listPattern:
                foreach (var elem in listPattern.Elements)
                    CollectNamesBoundByPatternInto(elem, names);

                break;

            case Pattern.NamedPattern namedPattern:
                foreach (var arg in namedPattern.Arguments)
                    CollectNamesBoundByPatternInto(arg, names);

                break;

            case Pattern.AsPattern asPattern:
                names.Add(asPattern.Name);
                CollectNamesBoundByPatternInto(asPattern.Pattern, names);
                break;

                // Other pattern variants (literal / unit / wildcard / ...) introduce nothing.
        }
    }

    /// <summary>
    /// Returns the bound name of a parameter pattern that is most useful for display.
    /// Recognises a bare <see cref="Pattern.VarPattern"/>, an
    /// <see cref="Pattern.AsPattern"/> (the <c>as</c>-name), and a
    /// <see cref="Pattern.NamedPattern"/> with a single argument (the inner var
    /// name; the destructuring shape <c>(Ctor inner)</c>). Returns <see langword="null"/> for
    /// any other pattern shape.
    /// </summary>
    public static string? TryGetParameterDisplayName(Pattern pattern)
    {
        while (true)
        {
            switch (pattern)
            {
                case Pattern.VarPattern vp:
                    return vp.Name;

                case Pattern.AsPattern ap:
                    return ap.Name;

                case Pattern.NamedPattern np when np.Arguments.Count is 1:
                    pattern = np.Arguments[0];
                    continue;

                default:
                    return null;
            }
        }
    }

    /// <summary>
    /// Peels nested <see cref="Pattern.AsPattern"/> wrappers off <paramref name="pattern"/>,
    /// returning the innermost pattern.
    /// </summary>
    public static Pattern PeelPatternAsBinder(Pattern pattern)
    {
        while (pattern is Pattern.AsPattern asPattern)
            pattern = asPattern.Pattern;

        return pattern;
    }

    /// <summary>
    /// Computes the set of free variable names referenced inside <paramref name="expression"/>.
    /// A "free variable" is an unqualified <see cref="Expression.Identifier"/> (one
    /// with an empty <see cref="DeclQualifiedName.Namespaces"/>) whose name is not introduced by
    /// any binder strictly within <paramref name="expression"/>.
    /// </summary>
    public static ImmutableHashSet<string> CollectRemainingFreeVariables(
        Expression expression)
    {
        switch (expression)
        {
            case Expression.Identifier identifier
            when identifier.QualifiedName.Namespaces.Count is 0:
                return [identifier.QualifiedName.DeclName];

            // Qualified Identifier references resolve to a module-level name and contribute
            // no free local variables.
            case SyntaxTypes.Expression.Identifier:
                return [];

            case Expression.LambdaExpression lambdaExpr:
                {
                    var bound = CollectNamesBoundByPatterns(lambdaExpr.Arguments);

                    return
                        CollectRemainingFreeVariables(lambdaExpr.Expression)
                        .Except(bound);
                }

            case Expression.CaseExpression caseExpr:
                {
                    var result = CollectRemainingFreeVariables(caseExpr.Expression);

                    foreach (var caseItem in caseExpr.Cases)
                    {
                        var armBound = CollectNamesBoundByPattern(caseItem.Pattern);

                        result =
                            result.Union(
                                CollectRemainingFreeVariables(caseItem.Expression)
                                .Except(armBound));
                    }

                    return result;
                }

            case Expression.LetExpression letExpr:
                {
                    // Let bindings are mutually recursive: every name introduced by the block
                    // is in scope for every RHS and for the body. Collect them all first, then
                    // take the union of each child's remaining-free set and subtract the
                    // let-bound names at the end.
                    var letBoundBuilder = ImmutableHashSet.CreateBuilder<string>();

                    foreach (var decl in letExpr.Declarations)
                    {
                        switch (decl)
                        {
                            case LetDeclaration.LetFunction letFunc:
                                letBoundBuilder.Add(letFunc.Function.Declaration.Name);
                                break;

                            case LetDeclaration.LetDestructuring letDestr:
                                foreach (var name in CollectNamesBoundByPattern(letDestr.Pattern))
                                    letBoundBuilder.Add(name);

                                break;
                        }
                    }

                    var letBound = letBoundBuilder.ToImmutable();

                    var result = ImmutableHashSet<string>.Empty;

                    foreach (var decl in letExpr.Declarations)
                    {
                        switch (decl)
                        {
                            case LetDeclaration.LetFunction letFunc:
                                {
                                    var paramBound =
                                        CollectNamesBoundByPatterns(letFunc.Function.Declaration.Arguments);

                                    result =
                                        result.Union(
                                            CollectRemainingFreeVariables(letFunc.Function.Declaration.Expression)
                                            .Except(paramBound));

                                    break;
                                }

                            case LetDeclaration.LetDestructuring letDestr:
                                result =
                                    result.Union(
                                        CollectRemainingFreeVariables(letDestr.Expression));

                                break;
                        }
                    }

                    result = result.Union(CollectRemainingFreeVariables(letExpr.Expression));

                    return result.Except(letBound);
                }

            case Expression.RecordUpdateExpression recordUpdate:
                {
                    // The record name on the LHS is itself a value reference.
                    var result = ImmutableHashSet.Create(recordUpdate.RecordName);

                    foreach (var field in recordUpdate.Fields)
                        result = result.Union(CollectRemainingFreeVariables(field.Value));

                    return result;
                }

            case Expression.Negation negation:
                return CollectRemainingFreeVariables(negation.Expression);

            case Expression.ListExpr listExpr:
                {
                    var result = ImmutableHashSet<string>.Empty;

                    foreach (var e in listExpr.Elements)
                        result = result.Union(CollectRemainingFreeVariables(e));

                    return result;
                }

            case Expression.IfBlock ifBlock:
                return
                    CollectRemainingFreeVariables(ifBlock.Condition)
                    .Union(CollectRemainingFreeVariables(ifBlock.ThenBlock))
                    .Union(CollectRemainingFreeVariables(ifBlock.ElseBlock));

            case Expression.Application app:
                {
                    var result = CollectRemainingFreeVariables(app.Function);

                    foreach (var arg in app.Arguments)
                        result = result.Union(CollectRemainingFreeVariables(arg));

                    return result;
                }

            case Expression.OperatorApplication opApp:
                return
                    CollectRemainingFreeVariables(opApp.Left)
                    .Union(CollectRemainingFreeVariables(opApp.Right));

            case Expression.TupledExpression tupled:
                {
                    var result = ImmutableHashSet<string>.Empty;

                    foreach (var e in tupled.Elements)
                        result = result.Union(CollectRemainingFreeVariables(e));

                    return result;
                }

            case Expression.RecordExpr recordExpr:
                {
                    var result = ImmutableHashSet<string>.Empty;

                    foreach (var f in recordExpr.Fields)
                        result = result.Union(CollectRemainingFreeVariables(f.Value));

                    return result;
                }

            case Expression.RecordAccess recordAccess:
                return CollectRemainingFreeVariables(recordAccess.Record);

            // Leaf variants with no nested Expression children and no binding semantics.
            case SyntaxTypes.Expression.UnitExpr:
            case SyntaxTypes.Expression.StringLiteral:
            case SyntaxTypes.Expression.CharLiteral:
            case SyntaxTypes.Expression.IntegerLiteral:
            case SyntaxTypes.Expression.FloatLiteral:
            case SyntaxTypes.Expression.PrefixOperator:
            case SyntaxTypes.Expression.RecordAccessFunction:
            case SyntaxTypes.Expression.GLSLExpression:
                return [];

            default:
                throw new NotImplementedException(
                    "CollectRemainingFreeVariables does not handle expression variant: " +
                    expression.GetType().Name);
        }
    }

    /// <summary>
    /// Computes the set of names that flow into the function-position expression
    /// (<see cref="Expression.Application.Function"/>) of <em>any</em>
    /// <see cref="Expression.Application"/> appearing inside
    /// <paramref name="expression"/>.
    /// </summary>
    public static ImmutableHashSet<string> ComputeNamesFlowingIntoApplicationFunctions(
        Expression expression)
    {
        var result = ImmutableHashSet<string>.Empty.ToBuilder();

        VisitApplications(
            expression,
            letRhsByName: [],
            bound: [],
            (application, letRhsByName, bound) =>
            {
                AddFlowingNamesOf(
                    application.Function,
                    letRhsByName,
                    bound,
                    result,
                    visited: []);
            });

        return result.ToImmutable();
    }

    /// <summary>
    /// Collects names that flow into the value of <paramref name="expression"/>, transitively
    /// expanding any free reference to a let-bound name through the recorded right-hand sides
    /// in <paramref name="letRhsByName"/>. Names that are still bound (in
    /// <paramref name="bound"/>) inside the enclosing scope of the original analysis target are
    /// <em>not</em> reported. The <paramref name="visited"/> set guards against infinite
    /// recursion for mutually recursive let-bindings.
    /// </summary>
    internal static void AddFlowingNamesOf(
        Expression expression,
        ImmutableDictionary<string, FlowBinding> letRhsByName,
        ImmutableHashSet<string> bound,
        ImmutableHashSet<string>.Builder result,
        ImmutableHashSet<string> visited) =>
        AddFlowingNamesOf(
            expression,
            locallyBoundNames: [],
            letRhsByName,
            bound,
            result,
            visited);

    private static void AddFlowingNamesOf(
        Expression expression,
        ImmutableHashSet<string> locallyBoundNames,
        ImmutableDictionary<string, FlowBinding> letRhsByName,
        ImmutableHashSet<string> bound,
        ImmutableHashSet<string>.Builder result,
        ImmutableHashSet<string> visited)
    {
        var freeVars = CollectRemainingFreeVariables(expression).Except(locallyBoundNames);

        foreach (var name in freeVars)
        {
            if (letRhsByName.TryGetValue(name, out var binding))
            {
                if (visited.Contains(name))
                    continue;

                AddFlowingNamesOf(
                    binding.Expression,
                    binding.LocallyBoundNames,
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

    private static ImmutableDictionary<string, FlowBinding> RemoveKeys(
        ImmutableDictionary<string, FlowBinding> dict,
        IEnumerable<string> keys)
    {
        var builder = dict.ToBuilder();

        foreach (var k in keys)
            builder.Remove(k);

        return builder.ToImmutable();
    }

    /// <summary>
    /// Generalised walker that traverses <paramref name="expression"/> and invokes
    /// <paramref name="onApplication"/> at every <see cref="Expression.Application"/>
    /// node it encounters, threading the current let-binding right-hand-side map and the set
    /// of names bound in the enclosing scopes.
    /// </summary>
    internal static void VisitApplications(
        Expression expression,
        ImmutableDictionary<string, FlowBinding> letRhsByName,
        ImmutableHashSet<string> bound,
        Action<Expression.Application, ImmutableDictionary<string, FlowBinding>, ImmutableHashSet<string>> onApplication)
    {
        switch (expression)
        {
            case Expression.Application app:
                onApplication(app, letRhsByName, bound);

                VisitApplications(app.Function, letRhsByName, bound, onApplication);

                foreach (var arg in app.Arguments)
                    VisitApplications(arg, letRhsByName, bound, onApplication);

                break;

            case Expression.IfBlock ifBlock:
                VisitApplications(ifBlock.Condition, letRhsByName, bound, onApplication);
                VisitApplications(ifBlock.ThenBlock, letRhsByName, bound, onApplication);
                VisitApplications(ifBlock.ElseBlock, letRhsByName, bound, onApplication);
                break;

            case Expression.LambdaExpression lambda:
                {
                    var innerBound = bound;

                    foreach (var arg in lambda.Arguments)
                        innerBound = innerBound.Union(CollectNamesBoundByPattern(arg));

                    var innerLetRhs = RemoveKeys(letRhsByName, innerBound);

                    VisitApplications(lambda.Expression, innerLetRhs, innerBound, onApplication);

                    break;
                }

            case Expression.CaseExpression caseExpr:
                VisitApplications(caseExpr.Expression, letRhsByName, bound, onApplication);

                foreach (var branch in caseExpr.Cases)
                {
                    var branchBindings = CollectNamesBoundByPattern(branch.Pattern);
                    var branchBound = bound.Union(branchBindings);
                    var branchLetRhsBuilder = letRhsByName.ToBuilder();

                    foreach (var n in branchBindings)
                        branchLetRhsBuilder[n] = new FlowBinding(caseExpr.Expression, []);

                    VisitApplications(
                        branch.Expression,
                        branchLetRhsBuilder.ToImmutable(),
                        branchBound,
                        onApplication);
                }

                break;

            case Expression.LetExpression letExpr:
                {
                    var declaredNames = ImmutableHashSet<string>.Empty.ToBuilder();
                    var letRhsBuilder = letRhsByName.ToBuilder();

                    foreach (var decl in letExpr.Declarations)
                    {
                        switch (decl)
                        {
                            case LetDeclaration.LetFunction lf:
                                {
                                    var name = lf.Function.Declaration.Name;
                                    declaredNames.Add(name);

                                    var paramArgs = lf.Function.Declaration.Arguments;

                                    letRhsBuilder[name] =
                                        new FlowBinding(
                                            lf.Function.Declaration.Expression,
                                            CollectNamesBoundByPatterns(paramArgs));

                                    break;
                                }

                            case LetDeclaration.LetDestructuring ld:
                                {
                                    var bindings = CollectNamesBoundByPattern(ld.Pattern);

                                    foreach (var n in bindings)
                                    {
                                        declaredNames.Add(n);
                                        letRhsBuilder[n] = new FlowBinding(ld.Expression, []);
                                    }

                                    break;
                                }

                            default:
                                throw new NotImplementedException(
                                    "VisitApplications does not handle let declaration variant: " +
                                    decl.GetType().Name);
                        }
                    }

                    var newBound = bound.Union(declaredNames.ToImmutable());
                    var newLetRhs = letRhsBuilder.ToImmutable();

                    foreach (var decl in letExpr.Declarations)
                    {
                        switch (decl)
                        {
                            case LetDeclaration.LetFunction lf:
                                {
                                    var innerBound = newBound;

                                    foreach (var arg in lf.Function.Declaration.Arguments)
                                        innerBound = innerBound.Union(CollectNamesBoundByPattern(arg));

                                    var innerLetRhs = RemoveKeys(newLetRhs, innerBound);

                                    VisitApplications(
                                        lf.Function.Declaration.Expression,
                                        innerLetRhs,
                                        innerBound,
                                        onApplication);

                                    break;
                                }

                            case LetDeclaration.LetDestructuring ld:
                                VisitApplications(ld.Expression, newLetRhs, newBound, onApplication);
                                break;
                        }
                    }

                    VisitApplications(letExpr.Expression, newLetRhs, newBound, onApplication);

                    break;
                }

            case Expression.OperatorApplication opApp:
                VisitApplications(opApp.Left, letRhsByName, bound, onApplication);
                VisitApplications(opApp.Right, letRhsByName, bound, onApplication);
                break;

            case Expression.ListExpr listExpr:
                foreach (var e in listExpr.Elements)
                    VisitApplications(e, letRhsByName, bound, onApplication);

                break;

            case Expression.TupledExpression tupled:
                foreach (var e in tupled.Elements)
                    VisitApplications(e, letRhsByName, bound, onApplication);

                break;

            case Expression.RecordExpr recordExpr:
                foreach (var f in recordExpr.Fields)
                    VisitApplications(f.Value, letRhsByName, bound, onApplication);

                break;

            case Expression.RecordUpdateExpression recordUpdate:
                foreach (var f in recordUpdate.Fields)
                    VisitApplications(f.Value, letRhsByName, bound, onApplication);

                break;

            case Expression.RecordAccess recordAccess:
                VisitApplications(recordAccess.Record, letRhsByName, bound, onApplication);
                break;

            case Expression.Negation negation:
                VisitApplications(negation.Expression, letRhsByName, bound, onApplication);
                break;

            case SyntaxTypes.Expression.UnitExpr:
            case SyntaxTypes.Expression.StringLiteral:
            case SyntaxTypes.Expression.CharLiteral:
            case SyntaxTypes.Expression.IntegerLiteral:
            case SyntaxTypes.Expression.FloatLiteral:
            case SyntaxTypes.Expression.Identifier:
            case SyntaxTypes.Expression.PrefixOperator:
            case SyntaxTypes.Expression.RecordAccessFunction:
            case SyntaxTypes.Expression.GLSLExpression:
                break;

            default:
                throw new NotImplementedException(
                    "VisitApplications does not handle expression variant: " +
                    expression.GetType().Name);
        }
    }

    /// <summary>
    /// Pre-order traversal visitor that invokes <paramref name="onNode"/> for
    /// <paramref name="expression"/> and every nested <see cref="Expression"/>
    /// reachable through it. The scope argument passed to the callback is
    /// <paramref name="initialScope"/> extended at every binding site (lambda parameter,
    /// let-function name + parameters, let-destructure pattern, case-branch pattern).
    /// </summary>
    public static void WalkExpressionsWithScope(
        Expression expression,
        ImmutableHashSet<string> initialScope,
        Action<Expression, ImmutableHashSet<string>> onNode)
    {
        onNode(expression, initialScope);

        switch (expression)
        {
            case Expression.LambdaExpression lambda:
                {
                    var bodyScope = ExtendScopeWithPatternList(initialScope, lambda.Arguments);
                    WalkExpressionsWithScope(lambda.Expression, bodyScope, onNode);
                    break;
                }

            case Expression.LetExpression letExpr:
                {
                    var letScope = initialScope;

                    foreach (var decl in letExpr.Declarations)
                        letScope = AddLetDeclarationBindingsToScope(decl, letScope);

                    foreach (var decl in letExpr.Declarations)
                    {
                        switch (decl)
                        {
                            case LetDeclaration.LetFunction letFunc:
                                {
                                    var impl = letFunc.Function.Declaration;
                                    var fnScope = ExtendScopeWithPatternList(letScope, impl.Arguments);
                                    WalkExpressionsWithScope(impl.Expression, fnScope, onNode);
                                    break;
                                }

                            case LetDeclaration.LetDestructuring letDest:
                                WalkExpressionsWithScope(letDest.Expression, letScope, onNode);
                                break;
                        }
                    }

                    WalkExpressionsWithScope(letExpr.Expression, letScope, onNode);
                    break;
                }

            case Expression.CaseExpression caseExpr:
                {
                    WalkExpressionsWithScope(caseExpr.Expression, initialScope, onNode);

                    foreach (var arm in caseExpr.Cases)
                    {
                        var armScope = ExtendScopeWithPattern(initialScope, arm.Pattern);
                        WalkExpressionsWithScope(arm.Expression, armScope, onNode);
                    }

                    break;
                }

            case Expression.Application app:
                WalkExpressionsWithScope(app.Function, initialScope, onNode);

                foreach (var arg in app.Arguments)
                    WalkExpressionsWithScope(arg, initialScope, onNode);

                break;

            case Expression.OperatorApplication opApp:
                WalkExpressionsWithScope(opApp.Left, initialScope, onNode);
                WalkExpressionsWithScope(opApp.Right, initialScope, onNode);
                break;

            case Expression.IfBlock ifBlock:
                WalkExpressionsWithScope(ifBlock.Condition, initialScope, onNode);
                WalkExpressionsWithScope(ifBlock.ThenBlock, initialScope, onNode);
                WalkExpressionsWithScope(ifBlock.ElseBlock, initialScope, onNode);
                break;

            case Expression.ListExpr listExpr:
                foreach (var element in listExpr.Elements)
                    WalkExpressionsWithScope(element, initialScope, onNode);

                break;

            case Expression.TupledExpression tupled:
                foreach (var element in tupled.Elements)
                    WalkExpressionsWithScope(element, initialScope, onNode);

                break;

            case Expression.RecordExpr recordExpr:
                foreach (var field in recordExpr.Fields)
                    WalkExpressionsWithScope(field.Value, initialScope, onNode);

                break;

            case Expression.RecordUpdateExpression recordUpdate:
                foreach (var field in recordUpdate.Fields)
                    WalkExpressionsWithScope(field.Value, initialScope, onNode);

                break;

            case Expression.RecordAccess recordAccess:
                WalkExpressionsWithScope(recordAccess.Record, initialScope, onNode);
                break;

            case Expression.Negation negation:
                WalkExpressionsWithScope(negation.Expression, initialScope, onNode);
                break;

            // Leaf variants — already visited via onNode at the top.
            case SyntaxTypes.Expression.Identifier:
            case SyntaxTypes.Expression.UnitExpr:
            case SyntaxTypes.Expression.StringLiteral:
            case SyntaxTypes.Expression.CharLiteral:
            case SyntaxTypes.Expression.IntegerLiteral:
            case SyntaxTypes.Expression.FloatLiteral:
            case SyntaxTypes.Expression.PrefixOperator:
            case SyntaxTypes.Expression.RecordAccessFunction:
            case SyntaxTypes.Expression.GLSLExpression:
                break;
        }
    }

    private static ImmutableHashSet<string> ExtendScopeWithPatternList(
        ImmutableHashSet<string> scope,
        IReadOnlyList<Pattern> patterns)
    {
        var extended = scope;

        foreach (var pattern in patterns)
            extended = ExtendScopeWithPattern(extended, pattern);

        return extended;
    }

    private static ImmutableHashSet<string> ExtendScopeWithPattern(
        ImmutableHashSet<string> scope,
        Pattern pattern)
    {
        var names = CollectNamesBoundByPattern(pattern);

        return names.Count is 0 ? scope : scope.Union(names);
    }

    private static ImmutableHashSet<string> AddLetDeclarationBindingsToScope(
        LetDeclaration letDecl,
        ImmutableHashSet<string> scope)
    {
        switch (letDecl)
        {
            case LetDeclaration.LetFunction lf:
                return scope.Add(lf.Function.Declaration.Name);

            case LetDeclaration.LetDestructuring ld:
                return ExtendScopeWithPattern(scope, ld.Pattern);

            default:
                return scope;
        }
    }

    /// <summary>
    /// Builds an index from (module key, simple declaration name) to the corresponding
    /// <see cref="DeclQualifiedName"/> for every key in <paramref name="declarations"/>. Used to
    /// resolve unqualified references against the enclosing module.
    /// </summary>
    public static IReadOnlyDictionary<(string moduleKey, string declName), DeclQualifiedName>
        BuildModuleKeyAndDeclNameIndex(
        IReadOnlyDictionary<DeclQualifiedName, Declaration> declarations)
    {
        var byModuleAndName =
            new Dictionary<(string moduleKey, string declName), DeclQualifiedName>(declarations.Count);

        foreach (var key in declarations.Keys)
            byModuleAndName[(string.Join(".", key.Namespaces), key.DeclName)] = key;

        return byModuleAndName;
    }

    /// <summary>
    /// Yields the return leaves reachable through let bodies, conditional branches, and case arms.
    /// </summary>
    public static IEnumerable<Expression> EnumerateReturnLeaves(
        Expression expression)
    {
        var stack = new Stack<Expression>();
        stack.Push(expression);

        while (stack.Count > 0)
        {
            var current = stack.Pop();

            switch (current)
            {
                case Expression.LetExpression letExpr:
                    stack.Push(letExpr.Expression);
                    break;

                case Expression.IfBlock ifBlock:
                    stack.Push(ifBlock.ElseBlock);
                    stack.Push(ifBlock.ThenBlock);
                    break;

                case Expression.CaseExpression caseExpr:
                    if (caseExpr.Cases.Count is 0)
                    {
                        yield return current;
                        break;
                    }

                    for (var i = caseExpr.Cases.Count - 1; i >= 0; i--)
                        stack.Push(caseExpr.Cases[i].Expression);

                    break;

                default:
                    yield return current;
                    break;
            }
        }
    }
}
