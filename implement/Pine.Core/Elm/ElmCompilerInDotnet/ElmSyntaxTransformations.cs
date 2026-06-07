using Pine.Core.CodeAnalysis;
using Pine.Core.Elm.ElmSyntax.SyntaxModel;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

using ModuleName = System.Collections.Generic.IReadOnlyList<string>;
using SyntaxTypes = Pine.Core.Elm.ElmSyntax.Stil4mElmSyntax7;

// Alias to avoid ambiguity with System.Range
using Range = Pine.Core.Elm.ElmSyntax.SyntaxModel.Range;

namespace Pine.Core.Elm.ElmCompilerInDotnet;

/// <summary>
/// Pure syntax transformations that operate only on Elm syntax elements.
/// Extracted from <see cref="ElmSyntaxOptimization"/> for clarity: these methods do not
/// depend on inlining context, function dictionaries, or module resolution.
/// </summary>
internal static class ElmSyntaxTransformations
{
    /// <summary>
    /// Zero-based location used for generated syntax nodes.
    /// Per design notes: "we use the value 0 for all locations (row, column) and ranges for newly created syntax nodes."
    /// These will be used in future cross-module inlining when creating new syntax nodes.
    /// </summary>
    internal static readonly Location s_zeroLocation = new(Row: 0, Column: 0);

    /// <summary>
    /// Zero range used for generated syntax nodes. See <see cref="s_zeroLocation"/> for details.
    /// </summary>
    internal static readonly Range s_zeroRange = new(Start: s_zeroLocation, End: s_zeroLocation);

    /// <summary>
    /// Result of deconstructing an expression into a constructor application
    /// with its name and field expressions.
    /// </summary>
    internal sealed record ConstructorApplication(
        SyntaxTypes.QualifiedNameRef ConstructorName,
        IReadOnlyList<Node<SyntaxTypes.Expression>> FieldExpressions);

    internal static Node<SyntaxTypes.Declaration> ParenthesizeDeclaration(
        Node<SyntaxTypes.Declaration> declNode)
    {
        if (declNode.Value is not SyntaxTypes.Declaration.FunctionDeclaration funcDecl)
        {
            return declNode;
        }

        var impl = funcDecl.Function.Declaration.Value;

        var parenthesizedExpr = ParenthesizeApplicationArgumentsRecursive(impl.Expression);

        var newImpl = impl with { Expression = parenthesizedExpr };

        var newFunc =
            funcDecl.Function with
            {
                Declaration =
                new Node<SyntaxTypes.FunctionImplementation>(
                    funcDecl.Function.Declaration.Range,
                    newImpl)
            };

        return
            new Node<SyntaxTypes.Declaration>(
                declNode.Range,
                new SyntaxTypes.Declaration.FunctionDeclaration(newFunc));
    }

    internal static bool TryCollapseSingleChoiceWrapperPassThroughLet(
        IReadOnlyList<Node<SyntaxTypes.Expression.LetDeclaration>> declarations,
        Node<SyntaxTypes.Expression> body,
        out Node<SyntaxTypes.Expression> collapsed)
    {
        collapsed = null!;

        if (declarations.Count is not 1 ||
            declarations[0].Value is not SyntaxTypes.Expression.LetDeclaration.LetDestructuring letDestr)
        {
            return false;
        }

        var namedPattern = TryUnwrapToNamedPattern(letDestr.Pattern.Value);

        if (namedPattern is null)
        {
            return false;
        }

        if (TryDeconstructConstructorApplication(body) is not { } ctorApp)
        {
            return false;
        }

        if (!AreEquivalentConstructorNames(namedPattern.Name, ctorApp.ConstructorName) ||
            namedPattern.Arguments.Count != ctorApp.FieldExpressions.Count)
        {
            return false;
        }

        for (var index = 0; index < namedPattern.Arguments.Count; index++)
        {
            if (SyntaxTypes.SyntaxAnalysis.UnwrapParenthesized(namedPattern.Arguments[index].Value) is not SyntaxTypes.Pattern.VarPattern varPattern)
            {
                return false;
            }

            if (!IsReferencePreservingWrapperField(ctorApp.FieldExpressions[index].Value, varPattern.Name))
            {
                return false;
            }
        }

        collapsed = letDestr.Expression;
        return true;
    }

    /// <summary>
    /// Like <see cref="TryDeconstructConstructorApplication(SyntaxTypes.Expression)"/> but
    /// restricted to references whose name starts with an uppercase letter (i.e. looks like a constructor).
    /// </summary>
    internal static ConstructorApplication? TryDeconstructExplicitConstructorApplication(
        SyntaxTypes.Expression expr)
    {
        if (TryDeconstructConstructorApplication(expr) is { } result &&
            LooksLikeConstructorName(result.ConstructorName.Name))
        {
            return result;
        }

        return null;
    }

    internal static bool LooksLikeConstructorName(string name) =>
        name.Length > 0 && char.IsUpper(name[0]);

    internal static SyntaxTypes.Expression? TryBetaReduceGeneratedApplication(
        SyntaxTypes.Expression.Application app)
    {
        if (app.Arguments.Count < 2 ||
            SyntaxTypes.SyntaxAnalysis.UnwrapParenthesized(app.Arguments[0].Value) is not SyntaxTypes.Expression.LambdaExpression lambda)
        {
            return null;
        }

        var consumedArgs = Math.Min(lambda.Lambda.Arguments.Count, app.Arguments.Count - 1);

        var substitutedBody =
            ApplyConsumedArgumentBindings(
                lambda.Lambda.Expression,
                lambda.Lambda.Arguments,
                [.. app.Arguments.Skip(1).Take(consumedArgs)]);

        if (app.Arguments.Count - 1 < lambda.Lambda.Arguments.Count)
        {
            return
                new SyntaxTypes.Expression.LambdaExpression(
                    new SyntaxTypes.LambdaStruct(
                        [.. lambda.Lambda.Arguments.Skip(app.Arguments.Count - 1)],
                        substitutedBody));
        }

        if (app.Arguments.Count - 1 == lambda.Lambda.Arguments.Count)
        {
            return substitutedBody.Value;
        }

        return
            new SyntaxTypes.Expression.Application(
                [.. new[] { substitutedBody }.Concat(app.Arguments.Skip(lambda.Lambda.Arguments.Count + 1))]);
    }

    internal static Node<SyntaxTypes.Expression> ApplyConsumedArgumentBindings(
        Node<SyntaxTypes.Expression> body,
        IReadOnlyList<Node<SyntaxTypes.Pattern>> parameters,
        IReadOnlyList<Node<SyntaxTypes.Expression>> consumedArgs)
    {
        var letDeclarations = new List<Node<SyntaxTypes.Expression.LetDeclaration>>();
        var substitutions = new Dictionary<string, Node<SyntaxTypes.Expression>>();

        for (var index = 0; index < consumedArgs.Count; index++)
        {
            var parameter = parameters[index];
            var argument = consumedArgs[index];

            switch (SyntaxTypes.SyntaxAnalysis.UnwrapParenthesized(parameter.Value))
            {
                case SyntaxTypes.Pattern.VarPattern varPattern:
                    substitutions[varPattern.Name] = argument;
                    break;

                case SyntaxTypes.Pattern.AllPattern:
                case SyntaxTypes.Pattern.UnitPattern:
                    break;

                default:
                    letDeclarations.Add(
                        new Node<SyntaxTypes.Expression.LetDeclaration>(
                            s_zeroRange,
                            new SyntaxTypes.Expression.LetDeclaration.LetDestructuring(
                                Pattern: parameter,
                                Expression: argument)));

                    break;
            }
        }

        var substitutedBody = SubstituteInExpression(body, substitutions);

        if (letDeclarations.Count is 0)
        {
            return substitutedBody;
        }

        return
            new Node<SyntaxTypes.Expression>(
                s_zeroRange,
                new SyntaxTypes.Expression.LetExpression(
                    new SyntaxTypes.Expression.LetBlock(
                        Declarations: [.. letDeclarations],
                        Expression: substitutedBody)));
    }

    internal static bool IsReferencePreservingWrapperField(
        SyntaxTypes.Expression expr,
        string variableName)
    {
        expr = SyntaxTypes.SyntaxAnalysis.UnwrapParenthesized(expr);

        if (IsLocalVariableReference(expr, variableName))
        {
            return true;
        }

        if (expr is not SyntaxTypes.Expression.LambdaExpression lambda)
        {
            return false;
        }

        var expectedArguments = new List<Node<SyntaxTypes.Expression>>();

        foreach (var parameter in lambda.Lambda.Arguments)
        {
            switch (SyntaxTypes.SyntaxAnalysis.UnwrapParenthesized(parameter.Value))
            {
                case SyntaxTypes.Pattern.VarPattern varPattern:
                    expectedArguments.Add(
                        new Node<SyntaxTypes.Expression>(
                            s_zeroRange,
                            new SyntaxTypes.Expression.FunctionOrValue([], varPattern.Name)));

                    break;

                case SyntaxTypes.Pattern.UnitPattern:
                    expectedArguments.Add(
                        new Node<SyntaxTypes.Expression>(
                            s_zeroRange,
                            new SyntaxTypes.Expression.UnitExpr()));

                    break;

                default:
                    return false;
            }
        }

        var lambdaBody = SyntaxTypes.SyntaxAnalysis.UnwrapParenthesized(lambda.Lambda.Expression.Value);

        if (lambdaBody is not SyntaxTypes.Expression.Application app ||
            app.Arguments.Count != expectedArguments.Count + 1 ||
            !IsLocalVariableReference(SyntaxTypes.SyntaxAnalysis.UnwrapParenthesized(app.Arguments[0].Value), variableName))
        {
            return false;
        }

        for (var index = 0; index < expectedArguments.Count; index++)
        {
            if (!expectedArguments[index].Value.Equals(
                SyntaxTypes.SyntaxAnalysis.UnwrapParenthesized(app.Arguments[index + 1].Value)))
            {
                return false;
            }
        }

        return true;
    }

    /// <summary>
    /// Wraps Application arguments (except the function position) in ParenthesizedExpression
    /// when they are themselves Application expressions with multiple arguments.
    /// This ensures correct rendering after pipe operator desugaring and specialization.
    /// </summary>
    internal static SyntaxTypes.Expression ParenthesizeApplicationArguments(SyntaxTypes.Expression expr)
    {
        if (expr is not SyntaxTypes.Expression.Application app || app.Arguments.Count < 2)
        {
            return expr;
        }

        var newArgs = new List<Node<SyntaxTypes.Expression>>(app.Arguments.Count) { app.Arguments[0] };

        for (var i = 1; i < app.Arguments.Count; i++)
        {
            var arg = app.Arguments[i];

            if (NeedsParenthesesInApplicationArgument(arg.Value))
            {
                newArgs.Add(
                    new Node<SyntaxTypes.Expression>(
                        arg.Range,
                        new SyntaxTypes.Expression.ParenthesizedExpression(arg)));
            }
            else
            {
                newArgs.Add(arg);
            }
        }

        return new SyntaxTypes.Expression.Application([.. newArgs]);
    }

    /// <summary>
    /// Constructs an <see cref="SyntaxTypes.Expression.Application"/> from
    /// the given argument list (where <c>arguments[0]</c> is the function-position
    /// head and <c>arguments[1..]</c> are the arguments to apply to it),
    /// flattening any nested-curried form.
    /// 
    /// <para>
    /// If the head expression (after unwrapping <see cref="SyntaxTypes.Expression.ParenthesizedExpression"/>)
    /// is itself an <see cref="SyntaxTypes.Expression.Application"/>, its
    /// arguments are spliced in front of the new arguments to produce a
    /// single flat <see cref="SyntaxTypes.Expression.Application"/>.
    /// The transformation is applied recursively in case the inner head
    /// is itself nested.
    /// </para>
    /// 
    /// <para>
    /// Concretely:
    /// <code>
    /// FlattenNestedApplicationHead([Application[h, a, b], c, d])
    /// =&gt; Application[h, a, b, c, d]
    /// </code>
    /// This matters because <c>ExpressionCompiler.CompileApplication</c>
    /// only takes the direct-call fast path when the head is a
    /// <see cref="SyntaxTypes.Expression.FunctionOrValue"/>. If the head
    /// is an <see cref="SyntaxTypes.Expression.Application"/>, the head
    /// is recursively compiled as a partial application — emitting a
    /// closure via <c>FunctionValueBuilder.EmitFunctionValueWithEnvFunctions</c>
    /// — and the outer call then dispatches generically on the closure.
    /// Flattening into a single <see cref="SyntaxTypes.Expression.Application"/>
    /// turns this into a single direct (saturated) call, eliminating the
    /// closure allocation.
    /// </para>
    /// 
    /// <para>
    /// This helper also re-runs <see cref="ParenthesizeApplicationArguments"/>
    /// on the result so the rendered output preserves correct
    /// parenthesization for the (now-promoted) inner arguments.
    /// </para>
    /// </summary>
    internal static SyntaxTypes.Expression.Application ConstructFlatApplication(
        IReadOnlyList<Node<SyntaxTypes.Expression>> arguments)
    {
        if (arguments.Count is 0)
        {
            return new SyntaxTypes.Expression.Application(arguments);
        }

        var headNode = arguments[0];
        var headExpr = SyntaxTypes.SyntaxAnalysis.UnwrapParenthesized(headNode.Value);

        if (headExpr is not SyntaxTypes.Expression.Application innerApp)
        {
            return new SyntaxTypes.Expression.Application(arguments);
        }

        // Recurse: the inner head might itself be nested.
        var flattenedInner = ConstructFlatApplication(innerApp.Arguments);

        var combined =
            new List<Node<SyntaxTypes.Expression>>(
                flattenedInner.Arguments.Count + arguments.Count - 1);

        combined.AddRange(flattenedInner.Arguments);

        for (var i = 1; i < arguments.Count; i++)
        {
            combined.Add(arguments[i]);
        }

        var rebuilt =
            (SyntaxTypes.Expression.Application)ParenthesizeApplicationArguments(
                new SyntaxTypes.Expression.Application(combined));

        return rebuilt;
    }

    /// <summary>
    /// Recursively walks the expression tree and replaces every
    /// <see cref="SyntaxTypes.Expression.Application"/> whose head is
    /// (after parenthesis unwrapping) itself an
    /// <see cref="SyntaxTypes.Expression.Application"/> with the
    /// equivalent flat form via <see cref="ConstructFlatApplication"/>.
    /// 
    /// <para>
    /// This is the global-normalization counterpart to
    /// <see cref="ConstructFlatApplication"/>: while construction-site
    /// callers (e.g. pipe-operator desugaring) should preferably build
    /// flat Applications directly, this pass is a final guard that
    /// catches any nested form introduced by substitution-based
    /// rewrites elsewhere in the pipeline.
    /// </para>
    /// </summary>
    internal static Node<SyntaxTypes.Expression> FlattenAllNestedApplicationHeads(
        Node<SyntaxTypes.Expression> exprNode)
    {
        return
            RewriteExpressionTree(
                exprNode,
                (app, recurse) =>
                {
                    var rewrittenArgs = new List<Node<SyntaxTypes.Expression>>(app.Arguments.Count);

                    for (var i = 0; i < app.Arguments.Count; i++)
                    {
                        rewrittenArgs.Add(recurse(app.Arguments[i]));
                    }

                    return ConstructFlatApplication(rewrittenArgs);
                });
    }

    internal static bool NeedsParenthesesInApplicationArgument(SyntaxTypes.Expression argument) =>
        argument switch
        {
            SyntaxTypes.Expression.Application innerApp => innerApp.Arguments.Count > 1,
            SyntaxTypes.Expression.LetExpression => true,
            SyntaxTypes.Expression.CaseExpression => true,
            SyntaxTypes.Expression.LambdaExpression => true,
            SyntaxTypes.Expression.IfBlock => true,
            SyntaxTypes.Expression.OperatorApplication => true,
            SyntaxTypes.Expression.Negation => true,

            // All other expression variants do not need extra parentheses when used
            // as an Application argument: literals, leaves and already-delimited forms.
            SyntaxTypes.Expression.UnitExpr or
            SyntaxTypes.Expression.Literal or
            SyntaxTypes.Expression.CharLiteral or
            SyntaxTypes.Expression.Integer or
            SyntaxTypes.Expression.Hex or
            SyntaxTypes.Expression.Floatable or
            SyntaxTypes.Expression.ListExpr or
            SyntaxTypes.Expression.FunctionOrValue or
            SyntaxTypes.Expression.PrefixOperator or
            SyntaxTypes.Expression.ParenthesizedExpression or
            SyntaxTypes.Expression.TupledExpression or
            SyntaxTypes.Expression.RecordExpr or
            SyntaxTypes.Expression.RecordAccess or
            SyntaxTypes.Expression.RecordAccessFunction or
            SyntaxTypes.Expression.RecordUpdateExpression or
            SyntaxTypes.Expression.GLSLExpression =>
            false,

            _ =>
            throw new NotImplementedException(
                "NeedsParenthesesInApplicationArgument does not handle expression variant: " +
                argument.GetType().Name)
        };

    /// <summary>
    /// Recursively walks all expressions in a declaration and ensures that Application arguments
    /// that are themselves Applications are wrapped in ParenthesizedExpression.
    /// This is applied as a post-processing step after inlining to ensure all generated
    /// expressions have correct parenthesization for rendering.
    /// </summary>
    internal static Node<SyntaxTypes.Expression> ParenthesizeApplicationArgumentsRecursive(
        Node<SyntaxTypes.Expression> exprNode)
    {
        var expr = exprNode.Value;

        static Node<SyntaxTypes.Expression> Recurse(Node<SyntaxTypes.Expression> e) =>
            ParenthesizeApplicationArgumentsRecursive(e);

        var result =
            expr switch
            {
                SyntaxTypes.Expression.Application app when app.Arguments.Count >= 2 =>
                ParenthesizeApplicationArguments(
                    new SyntaxTypes.Expression.Application(
                        [.. app.Arguments.Select(Recurse)])),

                _ =>
                MapChildExpressions(expr, Recurse)
            };

        return new Node<SyntaxTypes.Expression>(exprNode.Range, result);
    }

    /// <summary>
    /// Resolves a <see cref="SyntaxTypes.Expression.FunctionOrValue"/>
    /// reference into a fully-qualified name. References without an
    /// explicit module qualifier are interpreted as belonging to the
    /// declaring module.
    /// </summary>
    internal static DeclQualifiedName ResolveReference(
        SyntaxTypes.Expression.FunctionOrValue reference,
        ModuleName currentModuleName)
    {
        if (reference.ModuleName.Count is 0)
            return DeclQualifiedName.Create(currentModuleName, reference.Name);

        return DeclQualifiedName.Create(reference.ModuleName, reference.Name);
    }

    /// <summary>
    /// Resolves a <see cref="SyntaxTypes.QualifiedNameRef"/> (e.g. a
    /// constructor name appearing in a pattern or constructor
    /// application) into a fully-qualified name. References without an
    /// explicit module qualifier are interpreted as belonging to the
    /// declaring module.
    /// </summary>
    internal static DeclQualifiedName ResolveReference(
        SyntaxTypes.QualifiedNameRef qname,
        ModuleName currentModuleName)
    {
        if (qname.ModuleName.Count is 0)
            return DeclQualifiedName.Create(currentModuleName, qname.Name);

        return DeclQualifiedName.Create(qname.ModuleName, qname.Name);
    }

    /// <summary>
    /// Returns true if the expression tree contains any structurally complex expressions
    /// (if-then-else, case, let-in, lambda) that could produce invalid syntax
    /// when substituted into arbitrary expression positions after inlining.
    /// </summary>
    internal static bool ContainsComplexExpression(SyntaxTypes.Expression expr)
    {
        var worklist = new Stack<SyntaxTypes.Expression>();
        worklist.Push(expr);

        while (worklist.Count > 0)
        {
            var current = worklist.Pop();

            if (current is SyntaxTypes.Expression.IfBlock or
                SyntaxTypes.Expression.CaseExpression or
                SyntaxTypes.Expression.LetExpression or
                SyntaxTypes.Expression.LambdaExpression)
            {
                return true;
            }

            SyntaxTypes.SyntaxAnalysis.ForEachChildExpression(current, worklist.Push);
        }

        return false;
    }

    /// <summary>
    /// Determines whether an expression is safe to substitute in any expression position
    /// when inlining a plain value. Only literal-like leaf expressions and simple
    /// constructor applications are considered safe.
    /// </summary>
    internal static bool IsPlainValueSafeToInline(SyntaxTypes.Expression expr) =>
        expr switch
        {
            SyntaxTypes.Expression.UnitExpr => true,
            SyntaxTypes.Expression.Literal => true,
            SyntaxTypes.Expression.CharLiteral => true,
            SyntaxTypes.Expression.Integer => true,
            SyntaxTypes.Expression.Hex => true,
            SyntaxTypes.Expression.Floatable => true,
            SyntaxTypes.Expression.FunctionOrValue => true,
            SyntaxTypes.Expression.ListExpr => true,
            SyntaxTypes.Expression.TupledExpression => true,
            SyntaxTypes.Expression.RecordExpr => true,
            SyntaxTypes.Expression.ParenthesizedExpression => true,
            SyntaxTypes.Expression.Negation => true,

            // All other expression variants are not considered safe for plain-value inlining
            // because they may have side-effects-like semantics (function application),
            // introduce control flow, or carry binding/scoping concerns that the caller
            // does not analyze here.
            SyntaxTypes.Expression.IfBlock or
            SyntaxTypes.Expression.PrefixOperator or
            SyntaxTypes.Expression.Application or
            SyntaxTypes.Expression.OperatorApplication or
            SyntaxTypes.Expression.LambdaExpression or
            SyntaxTypes.Expression.CaseExpression or
            SyntaxTypes.Expression.LetExpression or
            SyntaxTypes.Expression.RecordAccess or
            SyntaxTypes.Expression.RecordAccessFunction or
            SyntaxTypes.Expression.RecordUpdateExpression or
            SyntaxTypes.Expression.GLSLExpression =>
            false,

            _ =>
            throw new NotImplementedException(
                "IsPlainValueSafeToInline does not handle expression variant: " + expr.GetType().Name),
        };

    internal static bool BodyUnwrapsParameterAsConstructor(
        Node<SyntaxTypes.Expression> exprNode,
        string parameterName)
    {
        var worklist = new Stack<SyntaxTypes.Expression>();
        worklist.Push(exprNode.Value);

        while (worklist.Count > 0)
        {
            var expr = worklist.Pop();

            switch (expr)
            {
                case SyntaxTypes.Expression.LetExpression letExpr:
                    if (letExpr.Value.Declarations.Any(
                        declaration =>
                        declaration.Value is SyntaxTypes.Expression.LetDeclaration.LetDestructuring letDestr &&
                        IsLocalVariableReference(letDestr.Expression.Value, parameterName) &&
                        IsConstructorPattern(letDestr.Pattern.Value)))
                        return true;

                    break;

                case SyntaxTypes.Expression.CaseExpression caseExpr:
                    if (IsLocalVariableReference(caseExpr.CaseBlock.Expression.Value, parameterName) &&
                        caseExpr.CaseBlock.Cases.Any(c => IsConstructorPattern(c.Pattern.Value)))
                        return true;

                    break;

                // All other expression variants do not themselves witness a constructor
                // unwrap of <paramref name="parameterName"/> at this node; recursion into
                // their children is handled below by EnqueueChildExpressions.
                case SyntaxTypes.Expression.UnitExpr:
                case SyntaxTypes.Expression.Literal:
                case SyntaxTypes.Expression.CharLiteral:
                case SyntaxTypes.Expression.Integer:
                case SyntaxTypes.Expression.Hex:
                case SyntaxTypes.Expression.Floatable:
                case SyntaxTypes.Expression.Negation:
                case SyntaxTypes.Expression.ListExpr:
                case SyntaxTypes.Expression.FunctionOrValue:
                case SyntaxTypes.Expression.IfBlock:
                case SyntaxTypes.Expression.PrefixOperator:
                case SyntaxTypes.Expression.ParenthesizedExpression:
                case SyntaxTypes.Expression.Application:
                case SyntaxTypes.Expression.OperatorApplication:
                case SyntaxTypes.Expression.TupledExpression:
                case SyntaxTypes.Expression.LambdaExpression:
                case SyntaxTypes.Expression.RecordExpr:
                case SyntaxTypes.Expression.RecordAccess:
                case SyntaxTypes.Expression.RecordAccessFunction:
                case SyntaxTypes.Expression.RecordUpdateExpression:
                case SyntaxTypes.Expression.GLSLExpression:
                    break;

                default:
                    throw new NotImplementedException(
                        "BodyUnwrapsParameterAsConstructor does not handle expression variant: " +
                        expr.GetType().Name);
            }

            SyntaxTypes.SyntaxAnalysis.ForEachChildExpression(expr, worklist.Push);
        }

        return false;
    }

    internal static bool IsConstructorPattern(SyntaxTypes.Pattern pattern)
    {
        return pattern switch
        {
            SyntaxTypes.Pattern.NamedPattern => true,

            // Alias patterns like `((Parser parse) as element)` still represent a constructor
            // pattern for specialization purposes, so unwrap the alias and inspect the inner pattern.
            SyntaxTypes.Pattern.AsPattern asPattern => IsConstructorPattern(asPattern.Pattern.Value),
            SyntaxTypes.Pattern.ParenthesizedPattern paren => IsConstructorPattern(paren.Pattern.Value),

            _ =>
            false
        };
    }

    internal static SyntaxTypes.Pattern.NamedPattern? TryUnwrapToNamedPattern(SyntaxTypes.Pattern pattern)
    {
        return pattern switch
        {
            SyntaxTypes.Pattern.NamedPattern np => np,
            SyntaxTypes.Pattern.AsPattern ap => TryUnwrapToNamedPattern(ap.Pattern.Value),
            SyntaxTypes.Pattern.ParenthesizedPattern pp => TryUnwrapToNamedPattern(pp.Pattern.Value),

            _ =>
            null
        };
    }

    internal static string? TryGetAliasNameFromPattern(SyntaxTypes.Pattern pattern)
    {
        return pattern switch
        {
            SyntaxTypes.Pattern.AsPattern ap => ap.Name.Value,
            SyntaxTypes.Pattern.ParenthesizedPattern pp => TryGetAliasNameFromPattern(pp.Pattern.Value),

            _ =>
            null
        };
    }

    /// <summary>
    /// Peels nested <see cref="SyntaxTypes.Pattern.ParenthesizedPattern"/>
    /// and <see cref="SyntaxTypes.Pattern.AsPattern"/> wrappers off
    /// <paramref name="pattern"/>, returning the innermost pattern.
    /// The strict superset of <see cref="UnwrapParenthesizedPattern"/>:
    /// recurses into the inner pattern of every as-binder.
    /// </summary>
    internal static SyntaxTypes.Pattern PeelPatternParenthesesAndAsBinder(SyntaxTypes.Pattern pattern)
    {
        while (true)
        {
            switch (pattern)
            {
                case SyntaxTypes.Pattern.ParenthesizedPattern p:
                    pattern = p.Pattern.Value;
                    continue;

                case SyntaxTypes.Pattern.AsPattern a:
                    pattern = a.Pattern.Value;
                    continue;

                default:
                    return pattern;
            }
        }
    }

    /// <summary>
    /// Returns the bound name of a parameter pattern that is most useful
    /// for display. Recognises:
    /// <list type="bullet">
    /// <item>A bare <see cref="SyntaxTypes.Pattern.VarPattern"/> (the
    /// pattern's own name).</item>
    /// <item>An <see cref="SyntaxTypes.Pattern.AsPattern"/> (the
    /// <c>as</c>-name).</item>
    /// <item>A <see cref="SyntaxTypes.Pattern.NamedPattern"/> with a
    /// single argument (the inner var name; the destructuring shape
    /// <c>(Ctor inner)</c>).</item>
    /// <item>Any of the above wrapped in
    /// <see cref="SyntaxTypes.Pattern.ParenthesizedPattern"/>.</item>
    /// </list>
    /// Returns <c>null</c> for any other pattern shape.
    /// </summary>
    internal static string? TryGetParameterDisplayName(SyntaxTypes.Pattern pattern)
    {
        while (true)
        {
            switch (pattern)
            {
                case SyntaxTypes.Pattern.VarPattern vp:
                    return vp.Name;

                case SyntaxTypes.Pattern.AsPattern ap:
                    return ap.Name.Value;

                case SyntaxTypes.Pattern.ParenthesizedPattern pp:
                    pattern = pp.Pattern.Value;
                    continue;

                case SyntaxTypes.Pattern.NamedPattern np when np.Arguments.Count is 1:
                    pattern = np.Arguments[0].Value;
                    continue;

                default:
                    return null;
            }
        }
    }

    internal static ConstructorApplication? TryDeconstructConstructorApplication(
        Node<SyntaxTypes.Expression> exprNode)
    {
        return TryDeconstructConstructorApplication(exprNode.Value);
    }

    internal static ConstructorApplication? TryDeconstructConstructorApplication(
        SyntaxTypes.Expression expr)
    {
        switch (SyntaxTypes.SyntaxAnalysis.UnwrapParenthesized(expr))
        {
            case SyntaxTypes.Expression.FunctionOrValue funcOrValue:
                return
                    new ConstructorApplication(
                        new SyntaxTypes.QualifiedNameRef(funcOrValue.ModuleName, funcOrValue.Name),
                        []);

            case SyntaxTypes.Expression.Application app
            when app.Arguments.Count > 0 &&
                     app.Arguments[0].Value is SyntaxTypes.Expression.FunctionOrValue constructorRef:

                return
                    new ConstructorApplication(
                        new SyntaxTypes.QualifiedNameRef(constructorRef.ModuleName, constructorRef.Name),
                        [.. app.Arguments.Skip(1)]);

            // Application that does not match the constructor-shape guard above (e.g. the
            // function position is not a bare FunctionOrValue) is not a constructor
            // application.
            case SyntaxTypes.Expression.Application:
            // Other expression variants are simply not constructor applications.
            case SyntaxTypes.Expression.UnitExpr:
            case SyntaxTypes.Expression.Literal:
            case SyntaxTypes.Expression.CharLiteral:
            case SyntaxTypes.Expression.Integer:
            case SyntaxTypes.Expression.Hex:
            case SyntaxTypes.Expression.Floatable:
            case SyntaxTypes.Expression.Negation:
            case SyntaxTypes.Expression.ListExpr:
            case SyntaxTypes.Expression.IfBlock:
            case SyntaxTypes.Expression.PrefixOperator:
            // ParenthesizedExpression is unwrapped above and never reaches the switch.
            case SyntaxTypes.Expression.OperatorApplication:
            case SyntaxTypes.Expression.TupledExpression:
            case SyntaxTypes.Expression.LambdaExpression:
            case SyntaxTypes.Expression.CaseExpression:
            case SyntaxTypes.Expression.LetExpression:
            case SyntaxTypes.Expression.RecordExpr:
            case SyntaxTypes.Expression.RecordAccess:
            case SyntaxTypes.Expression.RecordAccessFunction:
            case SyntaxTypes.Expression.RecordUpdateExpression:
            case SyntaxTypes.Expression.GLSLExpression:
                return null;

            default:
                throw new NotImplementedException(
                    "TryDeconstructConstructorApplication does not handle expression variant: " +
                    expr.GetType().Name);
        }
    }

    internal static bool AreLetDeclarationsIgnorableForConstructorResolution(
        IReadOnlyList<Node<SyntaxTypes.Expression.LetDeclaration>> declarations)
    {
        foreach (var declaration in declarations)
        {
            if (declaration.Value is not SyntaxTypes.Expression.LetDeclaration.LetDestructuring letDestr)
            {
                return false;
            }

            if (SyntaxTypes.SyntaxAnalysis.UnwrapParenthesized(letDestr.Pattern.Value) is not SyntaxTypes.Pattern.AllPattern)
            {
                return false;
            }
        }

        return declarations.Count > 0;
    }

    internal static Node<SyntaxTypes.Expression> BuildConstructorApplication(
        SyntaxTypes.QualifiedNameRef constructorName,
        IReadOnlyList<Node<SyntaxTypes.Expression>> fieldExpressions)
    {
        var constructorExpr =
            new Node<SyntaxTypes.Expression>(
                s_zeroRange,
                new SyntaxTypes.Expression.FunctionOrValue(constructorName.ModuleName, constructorName.Name));

        if (fieldExpressions.Count is 0)
            return constructorExpr;

        return
            new Node<SyntaxTypes.Expression>(
                s_zeroRange,
                new SyntaxTypes.Expression.Application(
                    [.. new[] { constructorExpr }.Concat(fieldExpressions)]));
    }

    internal static bool IsLocalVariableReference(
        SyntaxTypes.Expression expr,
        string variableName)
    {
        return
            SyntaxTypes.SyntaxAnalysis.UnwrapParenthesized(expr) is SyntaxTypes.Expression.FunctionOrValue funcOrValue &&
            funcOrValue.ModuleName.Count is 0 &&
            funcOrValue.Name == variableName;
    }

    internal static bool AreEquivalentConstructorNames(
        SyntaxTypes.QualifiedNameRef left,
        SyntaxTypes.QualifiedNameRef right)
    {
        return
            left.Equals(right) ||
            (left.Name == right.Name &&
            (left.ModuleName.Count is 0 || right.ModuleName.Count is 0));
    }

    internal static bool AreEquivalentConstructorNames(
        SyntaxTypes.QualifiedNameRef left,
        DeclQualifiedName right)
    {
        return
            (left.Name == right.DeclName &&
            (left.ModuleName.Count is 0 || left.ModuleName.SequenceEqual(right.Namespaces)));
    }

    internal static int CountUnshadowedLocalVariableReferences(
        SyntaxTypes.Expression expr,
        string variableName)
    {
        return CountUnshadowedLocalVariableReferences(expr, variableName, shadowed: false);
    }

    internal static int CountUnshadowedLocalVariableReferences(
        SyntaxTypes.Expression expr,
        string variableName,
        bool shadowed)
    {
        switch (expr)
        {
            case SyntaxTypes.Expression.FunctionOrValue funcOrValue:
                return
                    shadowed ||
                    funcOrValue.ModuleName.Count is not 0 ||
                    funcOrValue.Name != variableName
                    ?
                    0
                    :
                    1;

            case SyntaxTypes.Expression.Application app:
                return
                    app.Arguments.Sum(
                        argument => CountUnshadowedLocalVariableReferences(argument.Value, variableName, shadowed));

            case SyntaxTypes.Expression.ParenthesizedExpression paren:
                return CountUnshadowedLocalVariableReferences(paren.Expression.Value, variableName, shadowed);

            case SyntaxTypes.Expression.IfBlock ifBlock:
                return
                    CountUnshadowedLocalVariableReferences(ifBlock.Condition.Value, variableName, shadowed) +
                    CountUnshadowedLocalVariableReferences(ifBlock.ThenBlock.Value, variableName, shadowed) +
                    CountUnshadowedLocalVariableReferences(ifBlock.ElseBlock.Value, variableName, shadowed);

            case SyntaxTypes.Expression.CaseExpression caseExpr:
                {
                    var caseExprCount =
                        CountUnshadowedLocalVariableReferences(caseExpr.CaseBlock.Expression.Value, variableName, shadowed);

                    foreach (var caseItem in caseExpr.CaseBlock.Cases)
                    {
                        caseExprCount +=
                            CountUnshadowedLocalVariableReferences(
                                caseItem.Expression.Value,
                                variableName,
                                shadowed || CollectPatternNames(caseItem.Pattern.Value).Contains(variableName));
                    }

                    return caseExprCount;
                }

            case SyntaxTypes.Expression.LetExpression letExpr:
                {
                    var letBoundNames = new HashSet<string>();
                    var letExprCount = 0;

                    foreach (var declaration in letExpr.Value.Declarations)
                    {
                        switch (declaration.Value)
                        {
                            case SyntaxTypes.Expression.LetDeclaration.LetFunction letFunc:
                                {
                                    var functionName = letFunc.Function.Declaration.Value.Name.Value;
                                    var functionShadowed = shadowed || functionName == variableName;

                                    letExprCount +=
                                        CountUnshadowedLocalVariableReferences(
                                            letFunc.Function.Declaration.Value.Expression.Value,
                                            variableName,
                                            functionShadowed ||
                                            letFunc.Function.Declaration.Value.Arguments.Any(
                                                arg => CollectPatternNames(arg.Value).Contains(variableName)));

                                    letBoundNames.Add(functionName);
                                    break;
                                }

                            case SyntaxTypes.Expression.LetDeclaration.LetDestructuring letDestr:
                                letExprCount +=
                                    CountUnshadowedLocalVariableReferences(
                                        letDestr.Expression.Value,
                                        variableName,
                                        shadowed);

                                foreach (var boundName in CollectPatternNames(letDestr.Pattern.Value))
                                {
                                    letBoundNames.Add(boundName);
                                }

                                break;

                            default:
                                throw new NotImplementedException(
                                    "CountUnshadowedLocalVariableReferences does not handle let declaration variant: " +
                                    declaration.Value.GetType().Name);
                        }
                    }

                    letExprCount +=
                        CountUnshadowedLocalVariableReferences(
                            letExpr.Value.Expression.Value,
                            variableName,
                            shadowed || letBoundNames.Contains(variableName));

                    return letExprCount;
                }

            case SyntaxTypes.Expression.LambdaExpression lambda:
                return
                    CountUnshadowedLocalVariableReferences(
                        lambda.Lambda.Expression.Value,
                        variableName,
                        shadowed ||
                        lambda.Lambda.Arguments.Any(
                            arg => CollectPatternNames(arg.Value).Contains(variableName)));

            case SyntaxTypes.Expression.ListExpr listExpr:
                return
                    listExpr.Elements.Sum(
                        element => CountUnshadowedLocalVariableReferences(element.Value, variableName, shadowed));

            case SyntaxTypes.Expression.TupledExpression tupled:
                return
                    tupled.Elements.Sum(
                        element => CountUnshadowedLocalVariableReferences(element.Value, variableName, shadowed));

            case SyntaxTypes.Expression.RecordExpr recordExpr:
                return
                    recordExpr.Fields.Sum(
                        field =>
                        CountUnshadowedLocalVariableReferences(field.Value.valueExpr.Value, variableName, shadowed));

            case SyntaxTypes.Expression.RecordUpdateExpression recordUpdate:

                // The RecordName references a local variable; count it as one use
                // when it matches the queried name and is not shadowed.
                var recordNameContribution =
                    !shadowed && recordUpdate.RecordName.Value == variableName
                    ?
                    1
                    :
                    0;

                return
                    recordNameContribution +
                    recordUpdate.Fields.Sum(
                        field =>
                        CountUnshadowedLocalVariableReferences(field.Value.valueExpr.Value, variableName, shadowed));

            case SyntaxTypes.Expression.RecordAccess recordAccess:
                return CountUnshadowedLocalVariableReferences(recordAccess.Record.Value, variableName, shadowed);

            case SyntaxTypes.Expression.Negation negation:
                return CountUnshadowedLocalVariableReferences(negation.Expression.Value, variableName, shadowed);

            case SyntaxTypes.Expression.OperatorApplication opApp:
                return
                    CountUnshadowedLocalVariableReferences(opApp.Left.Value, variableName, shadowed) +
                    CountUnshadowedLocalVariableReferences(opApp.Right.Value, variableName, shadowed);

            // Leaf expression variants (no nested expressions and no local variable references):
            // each contributes zero references regardless of the queried variable name.
            case SyntaxTypes.Expression.UnitExpr:
            case SyntaxTypes.Expression.Literal:
            case SyntaxTypes.Expression.CharLiteral:
            case SyntaxTypes.Expression.Integer:
            case SyntaxTypes.Expression.Hex:
            case SyntaxTypes.Expression.Floatable:
            case SyntaxTypes.Expression.PrefixOperator:
            case SyntaxTypes.Expression.RecordAccessFunction:
            case SyntaxTypes.Expression.GLSLExpression:
                return 0;

            default:
                throw new NotImplementedException(
                    "CountUnshadowedLocalVariableReferences does not handle expression variant: " + expr.GetType().Name);
        }
    }

    /// <summary>
    /// Rebuilds an expression by applying <paramref name="mapChild"/> to all immediate child
    /// expression nodes. This centralizes the ~15-case expression variant reconstruction pattern
    /// for tree-mapping operations (substitution, qualification, parenthesization, rewriting).
    /// Leaf expressions (FunctionOrValue, Literal, etc.) are returned unchanged.
    /// </summary>
    internal static SyntaxTypes.Expression MapChildExpressions(
        SyntaxTypes.Expression expr,
        Func<Node<SyntaxTypes.Expression>, Node<SyntaxTypes.Expression>> mapChild)
    {
        return expr switch
        {
            SyntaxTypes.Expression.Application app =>
            new SyntaxTypes.Expression.Application(
                [.. app.Arguments.Select(mapChild)]),

            SyntaxTypes.Expression.ParenthesizedExpression paren =>
            new SyntaxTypes.Expression.ParenthesizedExpression(
                mapChild(paren.Expression)),

            SyntaxTypes.Expression.IfBlock ifBlock =>
            new SyntaxTypes.Expression.IfBlock(
                mapChild(ifBlock.Condition),
                mapChild(ifBlock.ThenBlock),
                mapChild(ifBlock.ElseBlock)),

            SyntaxTypes.Expression.CaseExpression caseExpr =>
            new SyntaxTypes.Expression.CaseExpression(
                new SyntaxTypes.CaseBlock(
                    mapChild(caseExpr.CaseBlock.Expression),
                    [
                    .. caseExpr.CaseBlock.Cases.Select(
                        c => new SyntaxTypes.Case(c.Pattern, mapChild(c.Expression)))
                    ])),

            SyntaxTypes.Expression.LetExpression letExpr =>
            new SyntaxTypes.Expression.LetExpression(
                new SyntaxTypes.Expression.LetBlock(
                    Declarations:
                    [
                    .. letExpr.Value.Declarations.Select(
                        d =>
                        {
                            var rewrittenDecl =
                                d.Value switch
                                {
                                    SyntaxTypes.Expression.LetDeclaration.LetFunction letFunc =>
                                    new SyntaxTypes.Expression.LetDeclaration.LetFunction(
                                        letFunc.Function with
                                        {
                                            Declaration =
                                            new Node<SyntaxTypes.FunctionImplementation>(
                                                letFunc.Function.Declaration.Range,
                                                letFunc.Function.Declaration.Value with
                                                {
                                                    Expression =
                                                    mapChild(letFunc.Function.Declaration.Value.Expression)
                                                })
                                        }),

                                    SyntaxTypes.Expression.LetDeclaration.LetDestructuring letDestr =>
                                    new SyntaxTypes.Expression.LetDeclaration.LetDestructuring(
                                        letDestr.Pattern,
                                        mapChild(letDestr.Expression)),

                                    _ =>
                                    d.Value
                                };

                            return new Node<SyntaxTypes.Expression.LetDeclaration>(d.Range, rewrittenDecl);
                        })
                    ],
                    Expression:
                    mapChild(letExpr.Value.Expression))),

            SyntaxTypes.Expression.LambdaExpression lambda =>
            new SyntaxTypes.Expression.LambdaExpression(
                new SyntaxTypes.LambdaStruct(
                    lambda.Lambda.Arguments,
                    mapChild(lambda.Lambda.Expression))),

            SyntaxTypes.Expression.ListExpr listExpr =>
            new SyntaxTypes.Expression.ListExpr(
                [.. listExpr.Elements.Select(mapChild)]),

            SyntaxTypes.Expression.TupledExpression tupled =>
            new SyntaxTypes.Expression.TupledExpression(
                [.. tupled.Elements.Select(mapChild)]),

            SyntaxTypes.Expression.RecordExpr recordExpr =>
            new SyntaxTypes.Expression.RecordExpr(
                [
                .. recordExpr.Fields.Select(
                    f =>
                    new Node<(Node<string> fieldName, Node<SyntaxTypes.Expression> valueExpr)>(
                        f.Range,
                        (f.Value.fieldName, mapChild(f.Value.valueExpr))))
                ]),

            SyntaxTypes.Expression.RecordUpdateExpression recordUpdate =>
            new SyntaxTypes.Expression.RecordUpdateExpression(
                recordUpdate.RecordName,
                [
                .. recordUpdate.Fields.Select(
                    f =>
                    new Node<(Node<string> fieldName, Node<SyntaxTypes.Expression> valueExpr)>(
                        f.Range,
                        (f.Value.fieldName, mapChild(f.Value.valueExpr))))
                ]),

            SyntaxTypes.Expression.RecordAccess recordAccess =>
            new SyntaxTypes.Expression.RecordAccess(
                mapChild(recordAccess.Record),
                recordAccess.FieldName),

            SyntaxTypes.Expression.Negation negation =>
            new SyntaxTypes.Expression.Negation(
                mapChild(negation.Expression)),

            SyntaxTypes.Expression.OperatorApplication opApp =>
            new SyntaxTypes.Expression.OperatorApplication(
                opApp.Operator,
                opApp.Direction,
                mapChild(opApp.Left),
                mapChild(opApp.Right)),

            // Leaf expression variants have no child expressions to map; return them
            // unchanged. They are listed explicitly so that the throwing default below
            // never fires for valid expression values.
            SyntaxTypes.Expression.UnitExpr or
            SyntaxTypes.Expression.Literal or
            SyntaxTypes.Expression.CharLiteral or
            SyntaxTypes.Expression.Integer or
            SyntaxTypes.Expression.Hex or
            SyntaxTypes.Expression.Floatable or
            SyntaxTypes.Expression.FunctionOrValue or
            SyntaxTypes.Expression.PrefixOperator or
            SyntaxTypes.Expression.RecordAccessFunction or
            SyntaxTypes.Expression.GLSLExpression =>
            expr,

            _ =>
            throw new NotImplementedException(
                "MapChildExpressions does not handle expression variant: " + expr.GetType().Name)
        };
    }

    /// <summary>
    /// Scope-tracking variant of <see cref="MapChildExpressions"/>: rebuilds
    /// <paramref name="expr"/> by applying <paramref name="mapChild"/> to all
    /// immediate child expression nodes, threading a lexical scope of
    /// in-scope identifier names through the traversal.
    /// <para>
    /// Scope-extending expression variants (lambda, let, case) are handled
    /// specially: each child is invoked with the scope extended by the names
    /// introduced by the enclosing pattern (lambda / case arm) or by the
    /// mutually-visible bindings of the surrounding <c>let</c> block. For
    /// non-scope-extending variants the callback is invoked with
    /// <paramref name="currentScope"/> unchanged and the result is rebuilt
    /// via <see cref="MapChildExpressions"/>.
    /// </para>
    /// <para>
    /// Like <see cref="MapChildExpressions"/>, returns
    /// <paramref name="expr"/> unchanged (reference-equal) when no child
    /// was rewritten — callers can use this as a fast-path. See
    /// <c>explore/internal-analysis/2026-05-18-eliminate-higher-order-parameters-in-focused-tests.md</c>
    /// §11.13 for the motivating refactor.
    /// </para>
    /// </summary>
    internal static SyntaxTypes.Expression MapChildExpressionsWithScope(
        SyntaxTypes.Expression expr,
        ImmutableHashSet<string> currentScope,
        Func<Node<SyntaxTypes.Expression>, ImmutableHashSet<string>, Node<SyntaxTypes.Expression>> mapChild)
    {
        switch (expr)
        {
            case SyntaxTypes.Expression.LambdaExpression lambda:
                {
                    var bodyScope = ExtendScopeWithPatternList(currentScope, lambda.Lambda.Arguments);

                    var bodyNode = mapChild(lambda.Lambda.Expression, bodyScope);

                    if (ReferenceEquals(bodyNode, lambda.Lambda.Expression))
                        return expr;

                    return
                        new SyntaxTypes.Expression.LambdaExpression(
                            new SyntaxTypes.LambdaStruct(
                                lambda.Lambda.Arguments,
                                bodyNode));
                }

            case SyntaxTypes.Expression.LetExpression letExpr:
                {
                    // Mutual recursion: every let-bound name is visible to
                    // every declaration body and to the let's final
                    // expression.
                    var letScope = currentScope;

                    foreach (var declNode in letExpr.Value.Declarations)
                        letScope = AddLetDeclarationBindingsToScope(declNode.Value, letScope);

                    var newDecls =
                        new List<Node<SyntaxTypes.Expression.LetDeclaration>>(letExpr.Value.Declarations.Count);

                    var declsChanged = false;

                    foreach (var declNode in letExpr.Value.Declarations)
                    {
                        var rewrittenDecl =
                            MapChildExpressionsInLetDeclarationWithScope(declNode.Value, letScope, mapChild);

                        if (!ReferenceEquals(rewrittenDecl, declNode.Value))
                            declsChanged = true;

                        newDecls.Add(
                            new Node<SyntaxTypes.Expression.LetDeclaration>(declNode.Range, rewrittenDecl));
                    }

                    var bodyNode = mapChild(letExpr.Value.Expression, letScope);

                    if (!declsChanged && ReferenceEquals(bodyNode, letExpr.Value.Expression))
                        return expr;

                    return
                        new SyntaxTypes.Expression.LetExpression(
                            new SyntaxTypes.Expression.LetBlock(
                                Declarations: newDecls,
                                Expression: bodyNode));
                }

            case SyntaxTypes.Expression.CaseExpression caseExpr:
                {
                    var scrut = mapChild(caseExpr.CaseBlock.Expression, currentScope);

                    var newArms = new List<SyntaxTypes.Case>(caseExpr.CaseBlock.Cases.Count);

                    var armsChanged = false;

                    foreach (var arm in caseExpr.CaseBlock.Cases)
                    {
                        var armScope = ExtendScopeWithPattern(currentScope, arm.Pattern.Value);

                        var armBody = mapChild(arm.Expression, armScope);

                        if (!ReferenceEquals(armBody, arm.Expression))
                            armsChanged = true;

                        newArms.Add(new SyntaxTypes.Case(arm.Pattern, armBody));
                    }

                    if (!armsChanged && ReferenceEquals(scrut, caseExpr.CaseBlock.Expression))
                        return expr;

                    return
                        new SyntaxTypes.Expression.CaseExpression(
                            new SyntaxTypes.CaseBlock(
                                Expression: scrut,
                                Cases: newArms));
                }

            default:
                return MapChildExpressions(expr, child => mapChild(child, currentScope));
        }
    }

    private static SyntaxTypes.Expression.LetDeclaration
        MapChildExpressionsInLetDeclarationWithScope(
        SyntaxTypes.Expression.LetDeclaration letDecl,
        ImmutableHashSet<string> letScope,
        Func<Node<SyntaxTypes.Expression>, ImmutableHashSet<string>, Node<SyntaxTypes.Expression>> mapChild)
    {
        switch (letDecl)
        {
            case SyntaxTypes.Expression.LetDeclaration.LetFunction letFunc:
                {
                    var impl = letFunc.Function.Declaration.Value;

                    var fnScope = ExtendScopeWithPatternList(letScope, impl.Arguments);

                    var bodyNode = mapChild(impl.Expression, fnScope);

                    if (ReferenceEquals(bodyNode, impl.Expression))
                        return letDecl;

                    var newImpl = impl with { Expression = bodyNode };

                    var newFunc =
                        letFunc.Function with
                        {
                            Declaration =
                            new Node<SyntaxTypes.FunctionImplementation>(
                                letFunc.Function.Declaration.Range,
                                newImpl),
                        };

                    return new SyntaxTypes.Expression.LetDeclaration.LetFunction(newFunc);
                }

            case SyntaxTypes.Expression.LetDeclaration.LetDestructuring letDest:
                {
                    var rewrittenExpr = mapChild(letDest.Expression, letScope);

                    if (ReferenceEquals(rewrittenExpr, letDest.Expression))
                        return letDecl;

                    return
                        new SyntaxTypes.Expression.LetDeclaration.LetDestructuring(
                            letDest.Pattern,
                            rewrittenExpr);
                }

            default:
                return letDecl;
        }
    }

    /// <summary>
    /// Pre-order traversal visitor that invokes <paramref name="onNode"/>
    /// for <paramref name="expression"/> and every nested
    /// <see cref="SyntaxTypes.Expression"/> reachable through it. The
    /// scope argument passed to the callback is
    /// <paramref name="initialScope"/> extended at every binding site
    /// (lambda parameter, let-function name + parameters, let-destructure
    /// pattern, case-branch pattern) according to the same policy used
    /// by <see cref="MapChildExpressionsWithScope"/>.
    /// <para>
    /// This is the read-only counterpart of
    /// <see cref="MapChildExpressionsWithScope"/>; use it when you need
    /// to inspect every node under a stable scope without rebuilding the
    /// expression tree.
    /// </para>
    /// </summary>
    public static void WalkExpressionsWithScope(
        SyntaxTypes.Expression expression,
        ImmutableHashSet<string> initialScope,
        Action<SyntaxTypes.Expression, ImmutableHashSet<string>> onNode)
    {
        onNode(expression, initialScope);

        switch (expression)
        {
            case SyntaxTypes.Expression.LambdaExpression lambda:
                {
                    var bodyScope = ExtendScopeWithPatternList(initialScope, lambda.Lambda.Arguments);
                    WalkExpressionsWithScope(lambda.Lambda.Expression.Value, bodyScope, onNode);
                    break;
                }

            case SyntaxTypes.Expression.LetExpression letExpr:
                {
                    var letScope = initialScope;

                    foreach (var declNode in letExpr.Value.Declarations)
                        letScope = AddLetDeclarationBindingsToScope(declNode.Value, letScope);

                    foreach (var declNode in letExpr.Value.Declarations)
                    {
                        switch (declNode.Value)
                        {
                            case SyntaxTypes.Expression.LetDeclaration.LetFunction letFunc:
                                {
                                    var impl = letFunc.Function.Declaration.Value;
                                    var fnScope = ExtendScopeWithPatternList(letScope, impl.Arguments);
                                    WalkExpressionsWithScope(impl.Expression.Value, fnScope, onNode);
                                    break;
                                }

                            case SyntaxTypes.Expression.LetDeclaration.LetDestructuring letDest:
                                WalkExpressionsWithScope(letDest.Expression.Value, letScope, onNode);
                                break;
                        }
                    }

                    WalkExpressionsWithScope(letExpr.Value.Expression.Value, letScope, onNode);
                    break;
                }

            case SyntaxTypes.Expression.CaseExpression caseExpr:
                {
                    WalkExpressionsWithScope(caseExpr.CaseBlock.Expression.Value, initialScope, onNode);

                    foreach (var arm in caseExpr.CaseBlock.Cases)
                    {
                        var armScope = ExtendScopeWithPattern(initialScope, arm.Pattern.Value);
                        WalkExpressionsWithScope(arm.Expression.Value, armScope, onNode);
                    }

                    break;
                }

            case SyntaxTypes.Expression.Application app:
                foreach (var arg in app.Arguments)
                    WalkExpressionsWithScope(arg.Value, initialScope, onNode);

                break;

            case SyntaxTypes.Expression.OperatorApplication opApp:
                WalkExpressionsWithScope(opApp.Left.Value, initialScope, onNode);
                WalkExpressionsWithScope(opApp.Right.Value, initialScope, onNode);
                break;

            case SyntaxTypes.Expression.ParenthesizedExpression paren:
                WalkExpressionsWithScope(paren.Expression.Value, initialScope, onNode);
                break;

            case SyntaxTypes.Expression.IfBlock ifBlock:
                WalkExpressionsWithScope(ifBlock.Condition.Value, initialScope, onNode);
                WalkExpressionsWithScope(ifBlock.ThenBlock.Value, initialScope, onNode);
                WalkExpressionsWithScope(ifBlock.ElseBlock.Value, initialScope, onNode);
                break;

            case SyntaxTypes.Expression.ListExpr listExpr:
                foreach (var element in listExpr.Elements)
                    WalkExpressionsWithScope(element.Value, initialScope, onNode);

                break;

            case SyntaxTypes.Expression.TupledExpression tupled:
                foreach (var element in tupled.Elements)
                    WalkExpressionsWithScope(element.Value, initialScope, onNode);

                break;

            case SyntaxTypes.Expression.RecordExpr recordExpr:
                foreach (var field in recordExpr.Fields)
                    WalkExpressionsWithScope(field.Value.valueExpr.Value, initialScope, onNode);

                break;

            case SyntaxTypes.Expression.RecordUpdateExpression recordUpdate:
                foreach (var field in recordUpdate.Fields)
                    WalkExpressionsWithScope(field.Value.valueExpr.Value, initialScope, onNode);

                break;

            case SyntaxTypes.Expression.RecordAccess recordAccess:
                WalkExpressionsWithScope(recordAccess.Record.Value, initialScope, onNode);
                break;

            case SyntaxTypes.Expression.Negation negation:
                WalkExpressionsWithScope(negation.Expression.Value, initialScope, onNode);
                break;

            // Leaf variants — already visited via onNode at the top.
            case SyntaxTypes.Expression.FunctionOrValue:
            case SyntaxTypes.Expression.UnitExpr:
            case SyntaxTypes.Expression.Literal:
            case SyntaxTypes.Expression.CharLiteral:
            case SyntaxTypes.Expression.Integer:
            case SyntaxTypes.Expression.Hex:
            case SyntaxTypes.Expression.Floatable:
            case SyntaxTypes.Expression.PrefixOperator:
            case SyntaxTypes.Expression.RecordAccessFunction:
            case SyntaxTypes.Expression.GLSLExpression:
                break;
        }
    }

    private static ImmutableHashSet<string> ExtendScopeWithPatternList(
        ImmutableHashSet<string> scope,
        IReadOnlyList<Node<SyntaxTypes.Pattern>> patterns)
    {
        var extended = scope;

        foreach (var patternNode in patterns)
            extended = ExtendScopeWithPattern(extended, patternNode.Value);

        return extended;
    }

    private static ImmutableHashSet<string> ExtendScopeWithPattern(
        ImmutableHashSet<string> scope,
        SyntaxTypes.Pattern pattern)
    {
        var names = CollectPatternNames(pattern);

        return names.Count is 0 ? scope : scope.Union(names);
    }

    private static ImmutableHashSet<string> AddLetDeclarationBindingsToScope(
        SyntaxTypes.Expression.LetDeclaration letDecl,
        ImmutableHashSet<string> scope)
    {
        switch (letDecl)
        {
            case SyntaxTypes.Expression.LetDeclaration.LetFunction lf:
                return scope.Add(lf.Function.Declaration.Value.Name.Value);

            case SyntaxTypes.Expression.LetDeclaration.LetDestructuring ld:
                return ExtendScopeWithPattern(scope, ld.Pattern.Value);

            default:
                return scope;
        }
    }

    /// <summary>
    /// Unified expression tree rewriter. Recursively traverses all expression variants,
    /// delegating <see cref="SyntaxTypes.Expression.Application"/> nodes to the supplied
    /// <paramref name="rewriteApplication"/> function. All other expression variants are
    /// structurally rebuilt with their children rewritten via <see cref="MapChildExpressions"/>.
    /// </summary>
    internal static Node<SyntaxTypes.Expression> RewriteExpressionTree(
        Node<SyntaxTypes.Expression> exprNode,
        Func<SyntaxTypes.Expression.Application,
            Func<Node<SyntaxTypes.Expression>, Node<SyntaxTypes.Expression>>,
            SyntaxTypes.Expression> rewriteApplication)
    {
        Node<SyntaxTypes.Expression> Recurse(Node<SyntaxTypes.Expression> node) =>
            RewriteExpressionTree(node, rewriteApplication);

        var expr = exprNode.Value;

        var rewrittenExpr =
            expr switch
            {
                SyntaxTypes.Expression.Application app =>
                rewriteApplication(app, Recurse),

                // All other expression variants are rebuilt structurally with their
                // children rewritten. Each variant is enumerated explicitly so the
                // throwing default never fires for valid expression values.
                SyntaxTypes.Expression.UnitExpr or
                SyntaxTypes.Expression.Literal or
                SyntaxTypes.Expression.CharLiteral or
                SyntaxTypes.Expression.Integer or
                SyntaxTypes.Expression.Hex or
                SyntaxTypes.Expression.Floatable or
                SyntaxTypes.Expression.Negation or
                SyntaxTypes.Expression.ListExpr or
                SyntaxTypes.Expression.FunctionOrValue or
                SyntaxTypes.Expression.IfBlock or
                SyntaxTypes.Expression.PrefixOperator or
                SyntaxTypes.Expression.ParenthesizedExpression or
                SyntaxTypes.Expression.OperatorApplication or
                SyntaxTypes.Expression.TupledExpression or
                SyntaxTypes.Expression.LambdaExpression or
                SyntaxTypes.Expression.CaseExpression or
                SyntaxTypes.Expression.LetExpression or
                SyntaxTypes.Expression.RecordExpr or
                SyntaxTypes.Expression.RecordAccess or
                SyntaxTypes.Expression.RecordAccessFunction or
                SyntaxTypes.Expression.RecordUpdateExpression or
                SyntaxTypes.Expression.GLSLExpression =>
                MapChildExpressions(expr, Recurse),

                _ =>
                throw new NotImplementedException(
                    "RewriteExpressionTree does not handle expression variant: " + expr.GetType().Name)
            };

        return new Node<SyntaxTypes.Expression>(exprNode.Range, rewrittenExpr);
    }

    /// <summary>
    /// Public, clearly-named entry point for capture-avoiding parallel substitution.
    /// Replaces every free occurrence of each key <c>x_i</c> in <paramref name="exprNode"/>
    /// with the corresponding value <c>v_i</c> from <paramref name="substitutions"/>,
    /// applying both standard safety conditions:
    /// </summary>
    /// <remarks>
    /// <list type="number">
    /// <item><description>
    /// <b>Shadowed-substitutions:</b> when traversing into a binder (lambda parameter,
    /// case pattern, let-decl binder) whose pattern names contain some <c>x_i</c>,
    /// that key is removed from the substitution map for the subtree of the binder's
    /// body. (Inner shadowing wins.)
    /// </description></item>
    /// <item><description>
    /// <b>Free-variable-capture:</b> when a binder name <c>y</c> would capture some
    /// free variable of a substitution value <c>v_i</c> (i.e. <c>y</c> occurs free in
    /// some <c>v_i</c>), the binder is alpha-renamed to a fresh name BEFORE
    /// substitution proceeds so that <c>v_i</c>'s reference to its original outer
    /// <c>y</c> is preserved.
    /// </description></item>
    /// </list>
    /// <para>
    /// This is recommendation #1 from
    /// <c>explore/internal-analysis/2026-05-19-loop-int-list-regression-findings.md</c>:
    /// a single named API for capture-avoiding substitution makes it harder to
    /// accidentally call an unsafe primitive that only enforces one of the two
    /// safety conditions.
    /// </para>
    /// </remarks>
    /// <seealso cref="SubstituteInExpression"/>
    public static Node<SyntaxTypes.Expression> SubstituteCaptureAvoiding(
        Node<SyntaxTypes.Expression> exprNode,
        IReadOnlyDictionary<string, Node<SyntaxTypes.Expression>> substitutions) =>
        SubstituteInExpression(exprNode, substitutions);

    internal static Node<SyntaxTypes.Expression> SubstituteInExpression(
        Node<SyntaxTypes.Expression> exprNode,
        IReadOnlyDictionary<string, Node<SyntaxTypes.Expression>> substitutions)
    {
        var expr = exprNode.Value;

        var substitutedExpr =
            expr switch
            {
                SyntaxTypes.Expression.FunctionOrValue funcOrValue when funcOrValue.ModuleName.Count is 0 &&
                    substitutions.TryGetValue(funcOrValue.Name, out var replacement) =>
                replacement.Value,

                SyntaxTypes.Expression.CaseExpression caseExpr =>
                TrySubstituteSingleChoiceTagCase(caseExpr.CaseBlock, substitutions)?.Value ??
                new SyntaxTypes.Expression.CaseExpression(
                    SubstituteInCaseBlock(caseExpr.CaseBlock, substitutions)),

                SyntaxTypes.Expression.LetExpression letExpr =>
                new SyntaxTypes.Expression.LetExpression(
                    SubstituteInLetBlock(letExpr.Value, substitutions)),

                SyntaxTypes.Expression.LambdaExpression lambda =>
                new SyntaxTypes.Expression.LambdaExpression(
                    SubstituteInLambdaStruct(lambda.Lambda, substitutions)),

                SyntaxTypes.Expression.RecordUpdateExpression recordUpdate =>
                SubstituteInRecordUpdateExpression(recordUpdate, substitutions),

                // FunctionOrValue references that don't match the substitution guard above
                // (qualified, or unqualified but not in the substitutions map) pass through
                // unchanged. The bare-FunctionOrValue rewrite is captured by the guarded
                // case at the top; this case catches the remaining FunctionOrValue values
                // so the throwing default below never fires.
                SyntaxTypes.Expression.FunctionOrValue =>
                expr,

                // All other expression variants delegate to MapChildExpressions for
                // structural recursion. They are enumerated explicitly so that the
                // throwing default below never fires for valid expression values.
                SyntaxTypes.Expression.UnitExpr or
                SyntaxTypes.Expression.Literal or
                SyntaxTypes.Expression.CharLiteral or
                SyntaxTypes.Expression.Integer or
                SyntaxTypes.Expression.Hex or
                SyntaxTypes.Expression.Floatable or
                SyntaxTypes.Expression.Negation or
                SyntaxTypes.Expression.ListExpr or
                SyntaxTypes.Expression.IfBlock or
                SyntaxTypes.Expression.PrefixOperator or
                SyntaxTypes.Expression.ParenthesizedExpression or
                SyntaxTypes.Expression.Application or
                SyntaxTypes.Expression.OperatorApplication or
                SyntaxTypes.Expression.TupledExpression or
                SyntaxTypes.Expression.RecordExpr or
                SyntaxTypes.Expression.RecordAccess or
                SyntaxTypes.Expression.RecordAccessFunction or
                SyntaxTypes.Expression.GLSLExpression =>
                MapChildExpressions(expr, child => SubstituteInExpression(child, substitutions)),

                _ =>
                throw new NotImplementedException(
                    "SubstituteInExpression does not handle expression variant: " + expr.GetType().Name)
            };

        return new Node<SyntaxTypes.Expression>(exprNode.Range, substitutedExpr);
    }

    /// <summary>
    /// Substitutes inside a record-update expression <c>{ recordName | f = v, ... }</c>.
    /// The <c>recordName</c> is a local-variable reference (not a child expression), so the
    /// generic <see cref="MapChildExpressions"/> traversal silently skips it. This dedicated
    /// helper substitutes the field value expressions and then handles the <c>recordName</c>
    /// reference itself:
    /// <list type="bullet">
    /// <item>If no substitution applies, the <c>recordName</c> is preserved.</item>
    /// <item>If the substitution value is a bare local variable reference (a
    /// <see cref="SyntaxTypes.Expression.FunctionOrValue"/> with empty <c>ModuleName</c>),
    /// the <c>recordName</c> is renamed in place to that variable name.</item>
    /// <item>For any other (non-trivial) substitution value, the record-update is wrapped in a
    /// <c>let</c> that binds a fresh local to the substitution value, then references that
    /// fresh local in the <c>recordName</c> position. This preserves Elm's surface
    /// requirement that the head of a record update is a variable identifier while still
    /// passing through an arbitrary expression.</item>
    /// </list>
    /// Without this special case, substituting a parameter named after a record being
    /// updated would silently drop the substitution at the record-update site, producing
    /// an unbound reference at compile time.
    /// </summary>
    internal static SyntaxTypes.Expression SubstituteInRecordUpdateExpression(
        SyntaxTypes.Expression.RecordUpdateExpression recordUpdate,
        IReadOnlyDictionary<string, Node<SyntaxTypes.Expression>> substitutions)
    {
        var substitutedFields =
            recordUpdate.Fields
            .Select(
                f =>
                new Node<(Node<string> fieldName, Node<SyntaxTypes.Expression> valueExpr)>(
                    f.Range,
                    (f.Value.fieldName, SubstituteInExpression(f.Value.valueExpr, substitutions))))
            .ToList();

        if (!substitutions.TryGetValue(recordUpdate.RecordName.Value, out var replacement))
        {
            return
                new SyntaxTypes.Expression.RecordUpdateExpression(
                    recordUpdate.RecordName,
                    [.. substitutedFields]);
        }

        if (replacement.Value is SyntaxTypes.Expression.FunctionOrValue funcOrValue &&
            funcOrValue.ModuleName.Count is 0)
        {
            // Simple-rename case: the substitution value is itself a local variable
            // reference, so we can keep using record-update syntax with the new name.
            return
                new SyntaxTypes.Expression.RecordUpdateExpression(
                    new Node<string>(recordUpdate.RecordName.Range, funcOrValue.Name),
                    [.. substitutedFields]);
        }

        // General case: the substitution value is an arbitrary expression. Bind it to a
        // fresh local via a let-destructuring and reference that fresh local in the
        // record-update head position. The fresh name must avoid colliding with any free
        // variable of the substitution value or of the substituted field value expressions
        // (otherwise the let-binding would shadow a name that the inner expressions read).
        var avoidNames = new HashSet<string>();

        foreach (var name in SyntaxTypes.SyntaxAnalysis.CollectRemainingFreeVariables(replacement.Value))
            avoidNames.Add(name);

        foreach (var field in substitutedFields)
        {
            foreach (var name in SyntaxTypes.SyntaxAnalysis.CollectRemainingFreeVariables(field.Value.valueExpr.Value))
                avoidNames.Add(name);
        }

        var freshName =
            GenerateUniqueLocalName(
                "recordUpdateRecord_" + recordUpdate.RecordName.Value,
                avoidNames);

        var letDestructuring =
            new Node<SyntaxTypes.Expression.LetDeclaration>(
                s_zeroRange,
                new SyntaxTypes.Expression.LetDeclaration.LetDestructuring(
                    Pattern:
                    new Node<SyntaxTypes.Pattern>(
                        s_zeroRange,
                        new SyntaxTypes.Pattern.VarPattern(freshName)),
                    Expression: replacement));

        var rebuiltRecordUpdate =
            new SyntaxTypes.Expression.RecordUpdateExpression(
                new Node<string>(recordUpdate.RecordName.Range, freshName),
                [.. substitutedFields]);

        return
            new SyntaxTypes.Expression.LetExpression(
                new SyntaxTypes.Expression.LetBlock(
                    Declarations: [letDestructuring],
                    Expression:
                    new Node<SyntaxTypes.Expression>(s_zeroRange, rebuiltRecordUpdate)));
    }

    internal static Node<SyntaxTypes.Expression>? TrySubstituteSingleChoiceTagCase(
        SyntaxTypes.CaseBlock caseBlock,
        IReadOnlyDictionary<string, Node<SyntaxTypes.Expression>> substitutions)
    {
        if (caseBlock.Cases.Count is not 1)
            return null;

        var substitutedScrutinee = SubstituteInExpression(caseBlock.Expression, substitutions);

        if (TryDeconstructConstructorApplication(substitutedScrutinee) is not { } ctorApp)
            return null;

        var onlyCase = caseBlock.Cases[0];

        if (TryBindSingleChoiceTagPattern(
                onlyCase.Pattern.Value,
                ctorApp.ConstructorName,
                ctorApp.FieldExpressions) is not { } patternBindings)
            return null;

        var shadowedNames = CollectPatternNames(onlyCase.Pattern.Value);

        var combinedSubstitutions =
            substitutions
            .Where(kvp => !shadowedNames.Contains(kvp.Key))
            .ToDictionary(kvp => kvp.Key, kvp => kvp.Value);

        foreach (var binding in patternBindings)
        {
            combinedSubstitutions[binding.Key] = binding.Value;
        }

        return SubstituteInExpression(onlyCase.Expression, combinedSubstitutions);
    }

    internal static Dictionary<string, Node<SyntaxTypes.Expression>>? TryBindSingleChoiceTagPattern(
        SyntaxTypes.Pattern pattern,
        SyntaxTypes.QualifiedNameRef constructorName,
        IReadOnlyList<Node<SyntaxTypes.Expression>> fieldExpressions)
    {
        switch (pattern)
        {
            case SyntaxTypes.Pattern.ParenthesizedPattern parenthesizedPattern:
                return
                    TryBindSingleChoiceTagPattern(
                        parenthesizedPattern.Pattern.Value,
                        constructorName,
                        fieldExpressions);

            case SyntaxTypes.Pattern.NamedPattern namedPattern
            when AreEquivalentConstructorNames(namedPattern.Name, constructorName) &&
                     namedPattern.Arguments.Count == fieldExpressions.Count:

                var bindings = new Dictionary<string, Node<SyntaxTypes.Expression>>();

                for (var i = 0; i < namedPattern.Arguments.Count; i++)
                {
                    if (!TryBindSingleChoiceTagFieldPattern(
                            namedPattern.Arguments[i].Value,
                            fieldExpressions[i],
                            bindings))
                    {
                        return null;
                    }
                }

                return bindings;

            default:
                return null;
        }
    }

    internal static bool TryBindSingleChoiceTagFieldPattern(
        SyntaxTypes.Pattern pattern,
        Node<SyntaxTypes.Expression> fieldExpression,
        Dictionary<string, Node<SyntaxTypes.Expression>> bindings)
    {
        switch (pattern)
        {
            case SyntaxTypes.Pattern.VarPattern varPattern:
                bindings[varPattern.Name] = fieldExpression;
                return true;

            case SyntaxTypes.Pattern.AllPattern:
                return true;

            case SyntaxTypes.Pattern.ParenthesizedPattern parenthesizedPattern:
                return
                    TryBindSingleChoiceTagFieldPattern(
                        parenthesizedPattern.Pattern.Value,
                        fieldExpression,
                        bindings);

            default:
                return false;
        }
    }

    internal static SyntaxTypes.CaseBlock SubstituteInCaseBlock(
        SyntaxTypes.CaseBlock caseBlock,
        IReadOnlyDictionary<string, Node<SyntaxTypes.Expression>> substitutions)
    {
        return
            new SyntaxTypes.CaseBlock(
                Expression: SubstituteInExpression(caseBlock.Expression, substitutions),
                Cases: [.. caseBlock.Cases.Select(c => SubstituteInCase(c, substitutions))]);
    }

    internal static SyntaxTypes.Case SubstituteInCase(
        SyntaxTypes.Case caseItem,
        IReadOnlyDictionary<string, Node<SyntaxTypes.Expression>> substitutions)
    {
        // Capture-avoiding alpha-rename of pattern bindings (see comment on
        // SubstituteInLambdaStruct for rationale).
        var freeInValues = CollectFreeVariablesAcrossSubstitutionValues(substitutions);

        var pattern = caseItem.Pattern;
        var caseBodyExpression = caseItem.Expression;

        if (ShouldAlphaRenameForCaptureAvoidance([pattern], freeInValues))
        {
            var namesInScope = new HashSet<string>(freeInValues);

            var (renamedPattern, patternBindings) =
                RenamePatternBindings(pattern, namesInScope, crossModuleQualification: null);

            pattern = renamedPattern;

            caseBodyExpression =
                RenameExpressionBindings(
                    caseBodyExpression,
                    patternBindings,
                    namesInScope,
                    crossModuleQualification: null);
        }

        // Remove substitutions shadowed by the (post-rename) pattern.
        var shadowedNames = CollectPatternNames(pattern.Value);

        var filteredSubstitutions =
            substitutions
            .Where(kvp => !shadowedNames.Contains(kvp.Key))
            .ToDictionary(kvp => kvp.Key, kvp => kvp.Value);

        return
            new SyntaxTypes.Case(
                Pattern: pattern,
                Expression: SubstituteInExpression(caseBodyExpression, filteredSubstitutions));
    }

    internal static SyntaxTypes.Expression.LetBlock SubstituteInLetBlock(
        SyntaxTypes.Expression.LetBlock letBlock,
        IReadOnlyDictionary<string, Node<SyntaxTypes.Expression>> substitutions)
    {
        // Capture-avoiding alpha-rename of let-introduced names (see comment on
        // SubstituteInLambdaStruct). A let-block is a single mutual-recursion
        // group: all of its declarations share one scope that covers each
        // declaration body AND the in-expression, so a single collision among
        // the bindings forces all conflicting names to be renamed consistently
        // throughout the block.
        var freeInValues = CollectFreeVariablesAcrossSubstitutionValues(substitutions);

        var letDeclarations = letBlock.Declarations;
        var letBodyExpression = letBlock.Expression;

        // Collect all binder names introduced by the let-block.
        var letBinderPatterns = new List<Node<SyntaxTypes.Pattern>>();

        foreach (var decl in letDeclarations)
        {
            switch (decl.Value)
            {
                case SyntaxTypes.Expression.LetDeclaration.LetFunction letFunc:
                    {
                        var nameNode = letFunc.Function.Declaration.Value.Name;

                        letBinderPatterns.Add(
                            new Node<SyntaxTypes.Pattern>(
                                nameNode.Range,
                                new SyntaxTypes.Pattern.VarPattern(nameNode.Value)));

                        break;
                    }

                case SyntaxTypes.Expression.LetDeclaration.LetDestructuring letDestr:
                    {
                        letBinderPatterns.Add(letDestr.Pattern);

                        break;
                    }
            }
        }

        if (ShouldAlphaRenameForCaptureAvoidance(letBinderPatterns, freeInValues))
        {
            // Build a consistent rename map for all colliding let-introduced names.
            var namesInScope = new HashSet<string>(freeInValues);
            var letRenames = new Dictionary<string, string>();

            foreach (var decl in letDeclarations)
            {
                ModuleName introduced =
                    decl.Value switch
                    {
                        SyntaxTypes.Expression.LetDeclaration.LetFunction letFunc =>
                        new[] { letFunc.Function.Declaration.Value.Name.Value },

                        SyntaxTypes.Expression.LetDeclaration.LetDestructuring letDestr =>
                        CollectPatternNames(letDestr.Pattern.Value).ToList(),

                        _ =>
                        [],
                    };

                foreach (var name in introduced)
                {
                    if (letRenames.ContainsKey(name))
                        continue;

                    var chosen =
                        namesInScope.Contains(name)
                        ?
                        GenerateUniqueLocalName(name, namesInScope)
                        :
                        name;

                    namesInScope.Add(chosen);
                    letRenames[name] = chosen;
                }
            }

            // Apply the consistent rename across every declaration body (which can
            // call mutual siblings) and across the in-expression.
            letDeclarations =
                [.. letDeclarations.Select(d => RenameLetDeclaration(d, letRenames, namesInScope))];

            letBodyExpression =
                RenameExpressionBindings(
                    letBodyExpression,
                    letRenames,
                    namesInScope,
                    crossModuleQualification: null);
        }

        // Recompute the (possibly renamed) let-bound name set for shadowing.
        var letNames = new HashSet<string>();

        foreach (var decl in letDeclarations)
        {
            if (decl.Value is SyntaxTypes.Expression.LetDeclaration.LetFunction letFunc)
            {
                letNames.Add(letFunc.Function.Declaration.Value.Name.Value);
            }
            else if (decl.Value is SyntaxTypes.Expression.LetDeclaration.LetDestructuring letDestr)
            {
                foreach (var name in CollectPatternNames(letDestr.Pattern.Value))
                {
                    letNames.Add(name);
                }
            }
        }

        // Filter substitutions for the body (names introduced by let shadow outer substitutions)
        var filteredSubstitutions =
            substitutions
            .Where(kvp => !letNames.Contains(kvp.Key))
            .ToDictionary(kvp => kvp.Key, kvp => kvp.Value);

        return
            new SyntaxTypes.Expression.LetBlock(
                Declarations: [.. letDeclarations.Select(d => SubstituteInLetDeclaration(d, substitutions))],
                Expression: SubstituteInExpression(letBodyExpression, filteredSubstitutions));
    }

    /// <summary>
    /// Applies a name-rename map to a single let-declaration (function or destructuring).
    /// For let-functions: renames the function name (if mapped) and recursively renames
    /// references in the function body, treating its own parameters as a nested scope.
    /// For let-destructuring: renames the pattern's introduced names (if mapped) and
    /// rewrites references in the binding expression.
    /// </summary>
    private static Node<SyntaxTypes.Expression.LetDeclaration> RenameLetDeclaration(
        Node<SyntaxTypes.Expression.LetDeclaration> declNode,
        IReadOnlyDictionary<string, string> activeRenames,
        IReadOnlySet<string> namesInScope)
    {
        switch (declNode.Value)
        {
            case SyntaxTypes.Expression.LetDeclaration.LetFunction letFunc:
                {
                    var origImpl = letFunc.Function.Declaration.Value;

                    var newName =
                        activeRenames.TryGetValue(origImpl.Name.Value, out var renamedName)
                        ?
                        renamedName
                        :
                        origImpl.Name.Value;

                    var renamedExpression =
                        RenameExpressionBindings(
                            origImpl.Expression,
                            activeRenames,
                            namesInScope,
                            crossModuleQualification: null);

                    var newImpl =
                        new SyntaxTypes.FunctionImplementation(
                            Name: new Node<string>(origImpl.Name.Range, newName),
                            Arguments: origImpl.Arguments,
                            Expression: renamedExpression);

                    var newFunc =
                        letFunc.Function with
                        {
                            Declaration =
                            new Node<SyntaxTypes.FunctionImplementation>(
                                letFunc.Function.Declaration.Range,
                                newImpl)
                        };

                    return
                        new Node<SyntaxTypes.Expression.LetDeclaration>(
                            declNode.Range,
                            new SyntaxTypes.Expression.LetDeclaration.LetFunction(newFunc));
                }

            case SyntaxTypes.Expression.LetDeclaration.LetDestructuring letDestr:
                {
                    // Rebuild the pattern with renamed binder names; leave the
                    // binding expression unchanged (it's evaluated in the OUTER
                    // scope, where it cannot see the let's own introduced names).
                    var renamedPattern = RenamePatternWithMap(letDestr.Pattern, activeRenames);

                    return
                        new Node<SyntaxTypes.Expression.LetDeclaration>(
                            declNode.Range,
                            new SyntaxTypes.Expression.LetDeclaration.LetDestructuring(
                                renamedPattern,
                                letDestr.Expression));
                }

            default:
                return declNode;
        }
    }

    /// <summary>
    /// Rebuilds a pattern with every <c>VarPattern</c> name replaced via
    /// <paramref name="renames"/> when present. Used when alpha-renaming let
    /// destructurings: the binding expression keeps its outer-scope semantics
    /// and only the pattern's bound names need to be relabeled.
    /// </summary>
    private static Node<SyntaxTypes.Pattern> RenamePatternWithMap(
        Node<SyntaxTypes.Pattern> patternNode,
        IReadOnlyDictionary<string, string> renames)
    {
        SyntaxTypes.Pattern RewriteValue(SyntaxTypes.Pattern pattern)
        {
            return pattern switch
            {
                SyntaxTypes.Pattern.VarPattern v when renames.TryGetValue(v.Name, out var renamed) =>
                new SyntaxTypes.Pattern.VarPattern(renamed),

                SyntaxTypes.Pattern.TuplePattern t =>
                new SyntaxTypes.Pattern.TuplePattern(
                    [.. t.Elements.Select(e => RenamePatternWithMap(e, renames))]),

                SyntaxTypes.Pattern.UnConsPattern unCons =>
                new SyntaxTypes.Pattern.UnConsPattern(
                    RenamePatternWithMap(unCons.Head, renames),
                    RenamePatternWithMap(unCons.Tail, renames)),

                SyntaxTypes.Pattern.ListPattern l =>
                new SyntaxTypes.Pattern.ListPattern(
                    [.. l.Elements.Select(e => RenamePatternWithMap(e, renames))]),

                SyntaxTypes.Pattern.NamedPattern n =>
                new SyntaxTypes.Pattern.NamedPattern(
                    n.Name,
                    [.. n.Arguments.Select(p => RenamePatternWithMap(p, renames))]),

                SyntaxTypes.Pattern.AsPattern a =>
                new SyntaxTypes.Pattern.AsPattern(
                    RenamePatternWithMap(a.Pattern, renames),
                    renames.TryGetValue(a.Name.Value, out var renamedAlias)
                    ?
                    new Node<string>(a.Name.Range, renamedAlias)
                    :
                    a.Name),

                SyntaxTypes.Pattern.ParenthesizedPattern p =>
                new SyntaxTypes.Pattern.ParenthesizedPattern(
                    RenamePatternWithMap(p.Pattern, renames)),

                _ =>
                pattern,
            };
        }

        return new Node<SyntaxTypes.Pattern>(patternNode.Range, RewriteValue(patternNode.Value));
    }

    internal static Node<SyntaxTypes.Expression.LetDeclaration> SubstituteInLetDeclaration(
        Node<SyntaxTypes.Expression.LetDeclaration> declNode,
        IReadOnlyDictionary<string, Node<SyntaxTypes.Expression>> substitutions)
    {
        var decl = declNode.Value;

        var substitutedDecl =
            decl switch
            {
                SyntaxTypes.Expression.LetDeclaration.LetFunction letFunc =>
                new SyntaxTypes.Expression.LetDeclaration.LetFunction(
                    SubstituteInFunctionStruct(letFunc.Function, substitutions)),

                SyntaxTypes.Expression.LetDeclaration.LetDestructuring letDestr =>
                new SyntaxTypes.Expression.LetDeclaration.LetDestructuring(
                    letDestr.Pattern,
                    SubstituteInExpression(letDestr.Expression, substitutions)),

                _ =>
                decl
            };

        return new Node<SyntaxTypes.Expression.LetDeclaration>(declNode.Range, substitutedDecl);
    }

    internal static SyntaxTypes.FunctionStruct SubstituteInFunctionStruct(
        SyntaxTypes.FunctionStruct func,
        IReadOnlyDictionary<string, Node<SyntaxTypes.Expression>> substitutions)
    {
        var impl = func.Declaration.Value;

        // Capture-avoiding alpha-rename of function parameters (see
        // SubstituteInLambdaStruct for rationale). Function-struct substitution
        // appears in let-functions; the same capture risk applies.
        var freeInValues = CollectFreeVariablesAcrossSubstitutionValues(substitutions);

        var renamedArguments = impl.Arguments;
        var renamedExpression = impl.Expression;

        if (ShouldAlphaRenameForCaptureAvoidance(impl.Arguments, freeInValues))
        {
            var renamedImpl = RenameBindingsAvoidingCapture(impl, freeInValues);
            renamedArguments = renamedImpl.Arguments;
            renamedExpression = renamedImpl.Expression;
        }

        // Remove substitutions shadowed by (post-rename) function parameters
        var paramNames = new HashSet<string>();

        foreach (var param in renamedArguments)
        {
            foreach (var name in CollectPatternNames(param.Value))
            {
                paramNames.Add(name);
            }
        }

        var filteredSubstitutions =
            substitutions
            .Where(kvp => !paramNames.Contains(kvp.Key))
            .ToDictionary(kvp => kvp.Key, kvp => kvp.Value);

        var substitutedImpl =
            new SyntaxTypes.FunctionImplementation(
                Name: impl.Name,
                Arguments: renamedArguments,
                Expression: SubstituteInExpression(renamedExpression, filteredSubstitutions));

        return
            func with
            {
                Declaration =
                new Node<SyntaxTypes.FunctionImplementation>(
                    func.Declaration.Range,
                    substitutedImpl)
            };
    }

    /// <summary>
    /// Collects the union of free variable names across the values in a
    /// substitution map. Used by the capture-avoiding substitution helpers
    /// (<see cref="SubstituteInLambdaStruct"/>, <see cref="SubstituteInCase"/>,
    /// <see cref="SubstituteInLetBlock"/>, <see cref="SubstituteInFunctionStruct"/>)
    /// to decide which binder names would capture a free variable from a
    /// substitution value and therefore must be alpha-renamed before substitution
    /// proceeds.
    /// </summary>
    private static HashSet<string> CollectFreeVariablesAcrossSubstitutionValues(
        IReadOnlyDictionary<string, Node<SyntaxTypes.Expression>> substitutions)
    {
        var freeInValues = new HashSet<string>();

        foreach (var (_, valueNode) in substitutions)
        {
            foreach (var name in SyntaxTypes.SyntaxAnalysis.CollectRemainingFreeVariables(valueNode.Value))
                freeInValues.Add(name);
        }

        return freeInValues;
    }

    internal static SyntaxTypes.LambdaStruct SubstituteInLambdaStruct(
        SyntaxTypes.LambdaStruct lambda,
        IReadOnlyDictionary<string, Node<SyntaxTypes.Expression>> substitutions)
    {
        // Capture-avoiding alpha-rename: if any lambda parameter binds a name that
        // also occurs FREE in a substitution value, the naive substitution would
        // capture that free reference (binding it to the lambda's parameter instead
        // of leaving it to refer to the outer scope it came from). Rename the
        // colliding parameters to fresh names before substituting.
        var freeInValues = CollectFreeVariablesAcrossSubstitutionValues(substitutions);

        var renamedLambda =
            ShouldAlphaRenameForCaptureAvoidance(lambda.Arguments, freeInValues)
            ?
            RenameBindingsAvoidingCapture(lambda, freeInValues)
            :
            lambda;

        // Remove substitutions shadowed by lambda parameters (post-rename names).
        var paramNames = new HashSet<string>();

        foreach (var param in renamedLambda.Arguments)
        {
            foreach (var name in CollectPatternNames(param.Value))
            {
                paramNames.Add(name);
            }
        }

        var filteredSubstitutions =
            substitutions
            .Where(kvp => !paramNames.Contains(kvp.Key))
            .ToDictionary(kvp => kvp.Key, kvp => kvp.Value);

        return
            new SyntaxTypes.LambdaStruct(
                Arguments: renamedLambda.Arguments,
                Expression: SubstituteInExpression(renamedLambda.Expression, filteredSubstitutions));
    }

    /// <summary>
    /// Returns true when any binder name in <paramref name="bindingPatterns"/>
    /// collides with a name in <paramref name="namesToAvoid"/> and therefore
    /// requires alpha-renaming to avoid capture during substitution.
    /// </summary>
    private static bool ShouldAlphaRenameForCaptureAvoidance(
        IReadOnlyList<Node<SyntaxTypes.Pattern>> bindingPatterns,
        IReadOnlyCollection<string> namesToAvoid)
    {
        if (namesToAvoid.Count is 0)
            return false;

        foreach (var pattern in bindingPatterns)
        {
            var patternNames = CollectPatternNames(pattern.Value);

            foreach (var name in patternNames)
            {
                if (namesToAvoid.Contains(name))
                    return true;
            }
        }

        return false;
    }

    internal static Node<(Node<string>, Node<SyntaxTypes.Expression>)> SubstituteInRecordField(
        Node<(Node<string> fieldName, Node<SyntaxTypes.Expression> valueExpr)> fieldNode,
        IReadOnlyDictionary<string, Node<SyntaxTypes.Expression>> substitutions)
    {
        var (fieldName, valueExpr) = fieldNode.Value;

        return
            new Node<(Node<string>, Node<SyntaxTypes.Expression>)>(
                fieldNode.Range,
                (fieldName, SubstituteInExpression(valueExpr, substitutions)));
    }

    internal static IReadOnlyDictionary<(string moduleKey, string declName), DeclQualifiedName>
        BuildModuleKeyAndDeclNameIndex(
        IReadOnlyDictionary<DeclQualifiedName, SyntaxTypes.Declaration> declarations)
    {
        var byModuleAndName =
            new Dictionary<(string moduleKey, string declName), DeclQualifiedName>(declarations.Count);

        foreach (var key in declarations.Keys)
        {
            byModuleAndName[(string.Join(".", key.Namespaces), key.DeclName)] = key;
        }

        return byModuleAndName;
    }

    internal static HashSet<string> CollectPatternNames(SyntaxTypes.Pattern pattern)
    {
        var names = new HashSet<string>();

        SyntaxTypes.SyntaxAnalysis.CollectNamesBoundByPatternInto(pattern, names);

        return names;
    }

    /// <summary>
    /// Union of every name bound by every pattern in <paramref name="patterns"/>.
    /// Convenience for parameter-list use-cases (function arguments,
    /// lambda arguments).
    /// </summary>
    internal static ImmutableHashSet<string> CollectNamesBoundByPatterns(
        IReadOnlyList<Node<SyntaxTypes.Pattern>> patterns)
    {
        return SyntaxTypes.SyntaxAnalysis.CollectNamesBoundByPatterns(patterns);
    }

    internal static void CollectPatternNamesRecursive(SyntaxTypes.Pattern pattern, HashSet<string> names) =>
        SyntaxTypes.SyntaxAnalysis.CollectNamesBoundByPatternInto(pattern, names);

    internal static string GenerateUniqueLocalName(
        string baseName,
        ISet<string> usedNames)
    {
        for (var suffix = 0; ; suffix++)
        {
            var candidate = baseName + "_" + suffix;

            if (!usedNames.Contains(candidate))
                return candidate;
        }
    }

    internal static SyntaxTypes.FunctionImplementation RenameBindingsAvoidingCapture(
        SyntaxTypes.FunctionImplementation implementation,
        IReadOnlySet<string> namesAlreadyInScope)
    {
        return RenameBindingsAvoidingCapture(implementation, namesAlreadyInScope, crossModuleQualification: null);
    }

    /// <summary>
    /// Combines local-binding renaming with cross-module reference qualification in a single pass.
    /// When <paramref name="crossModuleQualification"/> is provided, any unqualified <c>FunctionOrValue</c>
    /// reference that is NOT a local binding but IS a known module-level name in the callee module
    /// gets qualified with the callee module name. Similarly, unqualified <c>NamedPattern</c> constructor
    /// references are qualified.
    /// </summary>
    internal static SyntaxTypes.FunctionImplementation RenameBindingsAvoidingCapture(
        SyntaxTypes.FunctionImplementation implementation,
        IReadOnlySet<string> namesAlreadyInScope,
        CrossModuleQualification? crossModuleQualification)
    {
        var namesInScope = new HashSet<string>(namesAlreadyInScope);
        var activeRenames = new Dictionary<string, string>();

        var freshArguments =
            implementation.Arguments
            .Select(
                argument =>
                {
                    var (freshArgument, argumentBindings) =
                        RenamePatternBindings(argument, namesInScope, crossModuleQualification);

                    foreach (var binding in argumentBindings)
                        activeRenames[binding.Key] = binding.Value;

                    return freshArgument;
                })
            .ToList();

        var freshExpression =
            RenameExpressionBindings(
                implementation.Expression,
                activeRenames,
                namesInScope,
                crossModuleQualification);

        return
            implementation with
            {
                Arguments = [.. freshArguments],
                Expression = freshExpression
            };
    }

    internal static SyntaxTypes.LambdaStruct RenameBindingsAvoidingCapture(
        SyntaxTypes.LambdaStruct lambda,
        IReadOnlySet<string> namesAlreadyInScope)
    {
        return RenameBindingsAvoidingCapture(lambda, namesAlreadyInScope, crossModuleQualification: null);
    }

    internal static SyntaxTypes.LambdaStruct RenameBindingsAvoidingCapture(
        SyntaxTypes.LambdaStruct lambda,
        IReadOnlySet<string> namesAlreadyInScope,
        CrossModuleQualification? crossModuleQualification)
    {
        var namesInScope = new HashSet<string>(namesAlreadyInScope);
        var activeRenames = new Dictionary<string, string>();

        var freshArguments =
            lambda.Arguments
            .Select(
                argument =>
                {
                    var (freshArgument, argumentBindings) =
                        RenamePatternBindings(argument, namesInScope, crossModuleQualification);

                    foreach (var binding in argumentBindings)
                        activeRenames[binding.Key] = binding.Value;

                    return freshArgument;
                })
            .ToList();

        var freshExpression =
            RenameExpressionBindings(
                lambda.Expression,
                activeRenames,
                namesInScope,
                crossModuleQualification);

        return new SyntaxTypes.LambdaStruct([.. freshArguments], freshExpression);
    }

    internal static Node<SyntaxTypes.Expression> RenameBindingsAvoidingCapture(
        Node<SyntaxTypes.Expression> expression,
        IReadOnlySet<string> namesAlreadyInScope)
    {
        return
            RenameExpressionBindings(
                expression,
                new Dictionary<string, string>(),
                new HashSet<string>(namesAlreadyInScope),
                crossModuleQualification: null);
    }

    /// <summary>
    /// Applies cross-module reference qualification to an expression, without any names to avoid.
    /// Used for plain value inlining where there are no local names to rename, only
    /// module-level references to qualify.
    /// </summary>
    internal static Node<SyntaxTypes.Expression> RenameBindingsAvoidingCapture(
        Node<SyntaxTypes.Expression> expression,
        CrossModuleQualification crossModuleQualification)
    {
        return
            RenameExpressionBindings(
                expression,
                new Dictionary<string, string>(),
                new HashSet<string>(),
                crossModuleQualification);
    }

    private static Node<SyntaxTypes.Expression> RenameExpressionBindings(
        Node<SyntaxTypes.Expression> expressionNode,
        IReadOnlyDictionary<string, string> activeRenames,
        IReadOnlySet<string> namesInScope,
        CrossModuleQualification? crossModuleQualification = null)
    {
        SyntaxTypes.Expression RenameExpressionValue(SyntaxTypes.Expression expression)
        {
            switch (expression)
            {
                case SyntaxTypes.Expression.FunctionOrValue funcOrValue
                when funcOrValue.ModuleName.Count is 0 &&
                     activeRenames.TryGetValue(funcOrValue.Name, out var renamedVariable):

                    return new SyntaxTypes.Expression.FunctionOrValue([], renamedVariable);

                // Cross-module qualification: qualify unqualified references to callee module-level names.
                // Skip names that are local variables in scope (even if not renamed).
                case SyntaxTypes.Expression.FunctionOrValue funcOrValue
                when crossModuleQualification is not null &&
                     funcOrValue.ModuleName.Count is 0 &&
                     !activeRenames.ContainsKey(funcOrValue.Name) &&
                     !namesInScope.Contains(funcOrValue.Name) &&
                     crossModuleQualification.CalleeModuleLevelNames.Contains(funcOrValue.Name):

                    return
                        new SyntaxTypes.Expression.FunctionOrValue(
                            crossModuleQualification.CalleeModuleName,
                            funcOrValue.Name);

                case SyntaxTypes.Expression.LambdaExpression lambdaExpression:
                    {
                        var lambdaScopeNames = new HashSet<string>(namesInScope);
                        var lambdaRenames = new Dictionary<string, string>(activeRenames);

                        var freshArguments =
                            lambdaExpression.Lambda.Arguments
                            .Select(
                                argument =>
                                {
                                    var (freshArgument, argumentBindings) =
                                        RenamePatternBindings(argument, lambdaScopeNames, crossModuleQualification);

                                    foreach (var binding in argumentBindings)
                                        lambdaRenames[binding.Key] = binding.Value;

                                    return freshArgument;
                                })
                            .ToList();

                        var freshBody =
                            RenameExpressionBindings(
                                lambdaExpression.Lambda.Expression,
                                lambdaRenames,
                                lambdaScopeNames,
                                crossModuleQualification);

                        return
                            new SyntaxTypes.Expression.LambdaExpression(
                                new SyntaxTypes.LambdaStruct([.. freshArguments], freshBody));
                    }

                case SyntaxTypes.Expression.CaseExpression caseExpression:
                    {
                        var freshCaseExpression =
                            RenameExpressionBindings(
                                caseExpression.CaseBlock.Expression,
                                activeRenames,
                                namesInScope,
                                crossModuleQualification);

                        var freshCases =
                            caseExpression.CaseBlock.Cases
                            .Select(
                                caseItem =>
                                {
                                    var branchScopeNames = new HashSet<string>(namesInScope);
                                    var branchRenames = new Dictionary<string, string>(activeRenames);

                                    var (freshPattern, patternBindings) =
                                        RenamePatternBindings(caseItem.Pattern, branchScopeNames, crossModuleQualification);

                                    foreach (var binding in patternBindings)
                                        branchRenames[binding.Key] = binding.Value;

                                    var freshBody =
                                        RenameExpressionBindings(
                                            caseItem.Expression,
                                            branchRenames,
                                            branchScopeNames,
                                            crossModuleQualification);

                                    return new SyntaxTypes.Case(freshPattern, freshBody);
                                })
                            .ToList();

                        return
                            new SyntaxTypes.Expression.CaseExpression(
                                new SyntaxTypes.CaseBlock(
                                    freshCaseExpression,
                                    [.. freshCases]));
                    }

                case SyntaxTypes.Expression.LetExpression letExpression:
                    {
                        var letScopeNames = new HashSet<string>(namesInScope);
                        var letVisibleRenames = new Dictionary<string, string>(activeRenames);

                        var renamedNames = new List<Node<string>?>(letExpression.Value.Declarations.Count);

                        var renamedPatterns =
                            new List<Node<SyntaxTypes.Pattern>?>(letExpression.Value.Declarations.Count);

                        var destructuringBindings =
                            new List<Dictionary<string, string>?>(letExpression.Value.Declarations.Count);

                        foreach (var declaration in letExpression.Value.Declarations)
                        {
                            switch (declaration.Value)
                            {
                                case SyntaxTypes.Expression.LetDeclaration.LetFunction letFunction:
                                    {
                                        var originalName = letFunction.Function.Declaration.Value.Name.Value;

                                        var chosenName =
                                            letScopeNames.Contains(originalName)
                                            ?
                                            GenerateUniqueLocalName(originalName, letScopeNames)
                                            :
                                            originalName;

                                        letScopeNames.Add(chosenName);
                                        letVisibleRenames[originalName] = chosenName;

                                        renamedNames.Add(
                                            originalName == chosenName
                                            ?
                                            letFunction.Function.Declaration.Value.Name
                                            :
                                            new Node<string>(
                                                letFunction.Function.Declaration.Value.Name.Range,
                                                chosenName));

                                        renamedPatterns.Add(null);
                                        destructuringBindings.Add(null);
                                        break;
                                    }

                                case SyntaxTypes.Expression.LetDeclaration.LetDestructuring letDestructuring:
                                    {
                                        var (freshPattern, patternBindings) =
                                            RenamePatternBindings(
                                                letDestructuring.Pattern,
                                                letScopeNames,
                                                crossModuleQualification);

                                        foreach (var binding in patternBindings)
                                            letVisibleRenames[binding.Key] = binding.Value;

                                        renamedNames.Add(null);
                                        renamedPatterns.Add(freshPattern);
                                        destructuringBindings.Add(patternBindings);
                                        break;
                                    }

                                default:
                                    renamedNames.Add(null);
                                    renamedPatterns.Add(null);
                                    destructuringBindings.Add(null);
                                    break;
                            }
                        }

                        var freshDeclarations =
                            new List<Node<SyntaxTypes.Expression.LetDeclaration>>(letExpression.Value.Declarations.Count);

                        for (var declarationIndex = 0;
                            declarationIndex < letExpression.Value.Declarations.Count;
                            declarationIndex++)
                        {
                            var declaration = letExpression.Value.Declarations[declarationIndex];

                            switch (declaration.Value)
                            {
                                case SyntaxTypes.Expression.LetDeclaration.LetFunction letFunction:
                                    {
                                        var functionScopeNames = new HashSet<string>(letScopeNames);
                                        var functionRenames = new Dictionary<string, string>(letVisibleRenames);
                                        var functionArguments = new List<Node<SyntaxTypes.Pattern>>();

                                        foreach (var argument in letFunction.Function.Declaration.Value.Arguments)
                                        {
                                            var (freshArgument, argumentBindings) =
                                                RenamePatternBindings(argument, functionScopeNames, crossModuleQualification);

                                            foreach (var binding in argumentBindings)
                                                functionRenames[binding.Key] = binding.Value;

                                            functionArguments.Add(freshArgument);
                                        }

                                        var freshFunctionExpression =
                                            RenameExpressionBindings(
                                                letFunction.Function.Declaration.Value.Expression,
                                                functionRenames,
                                                functionScopeNames,
                                                crossModuleQualification);

                                        var freshImplementation =
                                            letFunction.Function.Declaration.Value with
                                            {
                                                Name =
                                                renamedNames[declarationIndex] ??
                                                letFunction.Function.Declaration.Value.Name,
                                                Arguments = [.. functionArguments],
                                                Expression = freshFunctionExpression
                                            };

                                        freshDeclarations.Add(
                                            new Node<SyntaxTypes.Expression.LetDeclaration>(
                                                declaration.Range,
                                                new SyntaxTypes.Expression.LetDeclaration.LetFunction(
                                                    letFunction.Function with
                                                    {
                                                        Declaration =
                                                        new Node<SyntaxTypes.FunctionImplementation>(
                                                            letFunction.Function.Declaration.Range,
                                                            freshImplementation)
                                                    })));

                                        break;
                                    }

                                case SyntaxTypes.Expression.LetDeclaration.LetDestructuring letDestructuring:
                                    {
                                        var destructuringRenames = new Dictionary<string, string>(letVisibleRenames);
                                        var destructuringScopeNames = new HashSet<string>(letScopeNames);
                                        var patternBindings = destructuringBindings[declarationIndex] ?? [];

                                        // For the RHS expression, references to the pattern's own bindings
                                        // should resolve to the *outer* scope (Elm semantics: the RHS is
                                        // evaluated before the binding takes effect).
                                        // However, we must NOT remove the name from destructuringScopeNames
                                        // because inner bindings (nested lets, lambdas, etc.) inside the RHS
                                        // still need to know the name is "in scope" to avoid introducing a
                                        // clash with the enclosing let-block's binding.
                                        foreach (var binding in patternBindings)
                                        {
                                            if (activeRenames.TryGetValue(binding.Key, out var visibleOuterName))
                                                destructuringRenames[binding.Key] = visibleOuterName;

                                            else
                                                destructuringRenames.Remove(binding.Key);
                                        }

                                        var freshExpression =
                                            RenameExpressionBindings(
                                                letDestructuring.Expression,
                                                destructuringRenames,
                                                destructuringScopeNames,
                                                crossModuleQualification);

                                        freshDeclarations.Add(
                                            new Node<SyntaxTypes.Expression.LetDeclaration>(
                                                declaration.Range,
                                                new SyntaxTypes.Expression.LetDeclaration.LetDestructuring(
                                                    renamedPatterns[declarationIndex] ?? letDestructuring.Pattern,
                                                    freshExpression)));

                                        break;
                                    }

                                default:
                                    freshDeclarations.Add(declaration);
                                    break;
                            }
                        }

                        var freshBody =
                            RenameExpressionBindings(
                                letExpression.Value.Expression,
                                letVisibleRenames,
                                letScopeNames,
                                crossModuleQualification);

                        return
                            new SyntaxTypes.Expression.LetExpression(
                                new SyntaxTypes.Expression.LetBlock(
                                    [.. freshDeclarations],
                                    freshBody));
                    }

                case SyntaxTypes.Expression.RecordUpdateExpression recordUpdate:
                    {
                        // RecordName references a local variable. If a rename applies to
                        // it, swap the reference to the renamed name; otherwise leave the
                        // RecordName unchanged. (Cross-module qualification does not apply
                        // because record-update heads must be local variables.)
                        // Field value expressions are recursed into normally.
                        var renamedRecordName =
                            activeRenames.TryGetValue(recordUpdate.RecordName.Value, out var renamedName)
                            ?
                            new Node<string>(recordUpdate.RecordName.Range, renamedName)
                            :
                            recordUpdate.RecordName;

                        var renamedFields =
                            recordUpdate.Fields
                            .Select(
                                field =>
                                new Node<(Node<string>, Node<SyntaxTypes.Expression>)>(
                                    field.Range,
                                    (field.Value.fieldName,
                                    RenameExpressionBindings(
                                        field.Value.valueExpr,
                                        activeRenames,
                                        namesInScope,
                                        crossModuleQualification))))
                            .ToList();

                        return
                            new SyntaxTypes.Expression.RecordUpdateExpression(
                                renamedRecordName,
                                [.. renamedFields]);
                    }

                // FunctionOrValue references that don't match the rename / cross-module
                // qualification guards above pass through unchanged. Listing the variant
                // explicitly keeps the throwing default below from firing.
                case SyntaxTypes.Expression.FunctionOrValue:
                    return expression;

                // All other expression variants delegate to MapChildExpressions for
                // structural recursion. Each variant is enumerated explicitly so that the
                // throwing default below never fires for valid expression values.
                case SyntaxTypes.Expression.UnitExpr:
                case SyntaxTypes.Expression.Literal:
                case SyntaxTypes.Expression.CharLiteral:
                case SyntaxTypes.Expression.Integer:
                case SyntaxTypes.Expression.Hex:
                case SyntaxTypes.Expression.Floatable:
                case SyntaxTypes.Expression.Negation:
                case SyntaxTypes.Expression.ListExpr:
                case SyntaxTypes.Expression.IfBlock:
                case SyntaxTypes.Expression.PrefixOperator:
                case SyntaxTypes.Expression.ParenthesizedExpression:
                case SyntaxTypes.Expression.Application:
                case SyntaxTypes.Expression.OperatorApplication:
                case SyntaxTypes.Expression.TupledExpression:
                case SyntaxTypes.Expression.RecordExpr:
                case SyntaxTypes.Expression.RecordAccess:
                case SyntaxTypes.Expression.RecordAccessFunction:
                case SyntaxTypes.Expression.GLSLExpression:
                    return
                        MapChildExpressions(
                            expression,
                            child =>
                            RenameExpressionBindings(child, activeRenames, namesInScope, crossModuleQualification));

                default:
                    throw new NotImplementedException(
                        "RenameExpressionBindings does not handle expression variant: " +
                        expression.GetType().Name);
            }
        }

        return new Node<SyntaxTypes.Expression>(expressionNode.Range, RenameExpressionValue(expressionNode.Value));
    }

    private static (Node<SyntaxTypes.Pattern> Pattern, Dictionary<string, string> Bindings) RenamePatternBindings(
        Node<SyntaxTypes.Pattern> patternNode,
        ISet<string> namesInScope,
        CrossModuleQualification? crossModuleQualification = null)
    {
        var bindings = new Dictionary<string, string>();

        SyntaxTypes.Pattern RenamePatternValue(SyntaxTypes.Pattern pattern)
        {
            switch (pattern)
            {
                case SyntaxTypes.Pattern.VarPattern varPattern:
                    {
                        var chosenName =
                            namesInScope.Contains(varPattern.Name)
                            ?
                            GenerateUniqueLocalName(varPattern.Name, namesInScope)
                            :
                            varPattern.Name;

                        namesInScope.Add(chosenName);
                        bindings[varPattern.Name] = chosenName;

                        return new SyntaxTypes.Pattern.VarPattern(chosenName);
                    }

                case SyntaxTypes.Pattern.TuplePattern tuplePattern:
                    {
                        var freshElements = new List<Node<SyntaxTypes.Pattern>>(tuplePattern.Elements.Count);

                        foreach (var element in tuplePattern.Elements)
                        {
                            var (freshElement, elementBindings) =
                                RenamePatternBindings(element, namesInScope, crossModuleQualification);

                            foreach (var binding in elementBindings)
                                bindings[binding.Key] = binding.Value;

                            freshElements.Add(freshElement);
                        }

                        return new SyntaxTypes.Pattern.TuplePattern([.. freshElements]);
                    }

                case SyntaxTypes.Pattern.RecordPattern recordPattern:
                    return
                        new SyntaxTypes.Pattern.RecordPattern(
                            [
                            .. recordPattern.Fields.Select(
                                field =>
                                {
                                    var chosenName =
                                        namesInScope.Contains(field.Value)
                                        ?
                                        GenerateUniqueLocalName(field.Value, namesInScope)
                                        :
                                        field.Value;

                                    namesInScope.Add(chosenName);
                                    bindings[field.Value] = chosenName;

                                    return
                                        field.Value == chosenName
                                        ?
                                        field
                                        :
                                        new Node<string>(field.Range, chosenName);
                                })
                            ]);

                case SyntaxTypes.Pattern.UnConsPattern unconsPattern:
                    {
                        var (freshHead, headBindings) =
                            RenamePatternBindings(unconsPattern.Head, namesInScope, crossModuleQualification);

                        var (freshTail, tailBindings) =
                            RenamePatternBindings(unconsPattern.Tail, namesInScope, crossModuleQualification);

                        foreach (var binding in headBindings)
                            bindings[binding.Key] = binding.Value;

                        foreach (var binding in tailBindings)
                            bindings[binding.Key] = binding.Value;

                        return new SyntaxTypes.Pattern.UnConsPattern(freshHead, freshTail);
                    }

                case SyntaxTypes.Pattern.ListPattern listPattern:
                    {
                        var freshElements = new List<Node<SyntaxTypes.Pattern>>(listPattern.Elements.Count);

                        foreach (var element in listPattern.Elements)
                        {
                            var (freshElement, elementBindings) =
                                RenamePatternBindings(element, namesInScope, crossModuleQualification);

                            foreach (var binding in elementBindings)
                                bindings[binding.Key] = binding.Value;

                            freshElements.Add(freshElement);
                        }

                        return new SyntaxTypes.Pattern.ListPattern([.. freshElements]);
                    }

                case SyntaxTypes.Pattern.NamedPattern namedPattern:
                    {
                        var freshArguments = new List<Node<SyntaxTypes.Pattern>>(namedPattern.Arguments.Count);

                        foreach (var argument in namedPattern.Arguments)
                        {
                            var (freshArgument, argumentBindings) =
                                RenamePatternBindings(argument, namesInScope, crossModuleQualification);

                            foreach (var binding in argumentBindings)
                                bindings[binding.Key] = binding.Value;

                            freshArguments.Add(freshArgument);
                        }

                        // Qualify unqualified constructor references from the callee module
                        var qualifiedName = namedPattern.Name;

                        if (crossModuleQualification is not null &&
                            namedPattern.Name.ModuleName.Count is 0 &&
                            crossModuleQualification.CalleeModuleLevelNames.Contains(namedPattern.Name.Name))
                        {
                            qualifiedName =
                                new SyntaxTypes.QualifiedNameRef(
                                    crossModuleQualification.CalleeModuleName,
                                    namedPattern.Name.Name);
                        }

                        return new SyntaxTypes.Pattern.NamedPattern(qualifiedName, [.. freshArguments]);
                    }

                case SyntaxTypes.Pattern.AsPattern asPattern:
                    {
                        var (freshInnerPattern, innerBindings) =
                            RenamePatternBindings(asPattern.Pattern, namesInScope, crossModuleQualification);

                        foreach (var binding in innerBindings)
                            bindings[binding.Key] = binding.Value;

                        var chosenAlias =
                            namesInScope.Contains(asPattern.Name.Value)
                            ?
                            GenerateUniqueLocalName(asPattern.Name.Value, namesInScope)
                            :
                            asPattern.Name.Value;

                        namesInScope.Add(chosenAlias);
                        bindings[asPattern.Name.Value] = chosenAlias;

                        return
                            new SyntaxTypes.Pattern.AsPattern(
                                freshInnerPattern,
                                asPattern.Name.Value == chosenAlias
                                ?
                                asPattern.Name
                                :
                                new Node<string>(asPattern.Name.Range, chosenAlias));
                    }

                case SyntaxTypes.Pattern.ParenthesizedPattern parenthesizedPattern:
                    {
                        var (freshPattern, childBindings) =
                            RenamePatternBindings(parenthesizedPattern.Pattern, namesInScope, crossModuleQualification);

                        foreach (var binding in childBindings)
                            bindings[binding.Key] = binding.Value;

                        return new SyntaxTypes.Pattern.ParenthesizedPattern(freshPattern);
                    }

                default:
                    return pattern;
            }
        }

        return
            (new Node<SyntaxTypes.Pattern>(patternNode.Range, RenamePatternValue(patternNode.Value)),
            bindings);
    }


    /// <summary>
    /// Context for qualifying unqualified references during cross-module inlining.
    /// When a function body from the callee module is inlined into a different module,
    /// unqualified references to module-level declarations must be qualified with
    /// the callee module name to avoid misresolution at the call site.
    /// </summary>
    internal sealed record CrossModuleQualification(
        ModuleName CalleeModuleName,
        IReadOnlySet<string> CalleeModuleLevelNames);

}
