using Pine.Core.CodeAnalysis;
using Pine.Core.Elm.ElmSyntax.SyntaxModel;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

using ModuleName = System.Collections.Generic.IReadOnlyList<string>;
using Stil4mElmSyntax7 = Pine.Core.Elm.ElmSyntax.Stil4mElmSyntax7;

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
    internal static readonly Location ZeroLocation = new(Row: 0, Column: 0);

    /// <summary>
    /// Zero range used for generated syntax nodes. See <see cref="ZeroLocation"/> for details.
    /// </summary>
    internal static readonly Range ZeroRange = new(Start: ZeroLocation, End: ZeroLocation);

    /// <summary>
    /// Result of deconstructing an expression into a constructor application
    /// with its name and field expressions.
    /// </summary>
    internal sealed record ConstructorApplication(
        Stil4mElmSyntax7.QualifiedNameRef ConstructorName,
        IReadOnlyList<Node<Stil4mElmSyntax7.Expression>> FieldExpressions);

    internal static bool TryCollapseSingleChoiceWrapperPassThroughLet(
        IReadOnlyList<Node<Stil4mElmSyntax7.Expression.LetDeclaration>> declarations,
        Node<Stil4mElmSyntax7.Expression> body,
        out Node<Stil4mElmSyntax7.Expression> collapsed)
    {
        collapsed = null!;

        if (declarations.Count is not 1 ||
            declarations[0].Value is not Stil4mElmSyntax7.Expression.LetDeclaration.LetDestructuring letDestr)
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
            if (Stil4mElmSyntax7.SyntaxAnalysis.UnwrapParenthesized(namedPattern.Arguments[index].Value) is not Stil4mElmSyntax7.Pattern.VarPattern varPattern)
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
    /// Like <see cref="TryDeconstructConstructorApplication(Stil4mElmSyntax7.Expression)"/> but
    /// restricted to references whose name starts with an uppercase letter (i.e. looks like a constructor).
    /// </summary>
    internal static ConstructorApplication? TryDeconstructExplicitConstructorApplication(
        Stil4mElmSyntax7.Expression expr)
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

    internal static Stil4mElmSyntax7.Expression? TryBetaReduceGeneratedApplication(
        Stil4mElmSyntax7.Expression.Application app)
    {
        if (app.Arguments.Count < 2 ||
            Stil4mElmSyntax7.SyntaxAnalysis.UnwrapParenthesized(app.Arguments[0].Value) is not Stil4mElmSyntax7.Expression.LambdaExpression lambda)
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
                new Stil4mElmSyntax7.Expression.LambdaExpression(
                    new Stil4mElmSyntax7.LambdaStruct(
                        [.. lambda.Lambda.Arguments.Skip(app.Arguments.Count - 1)],
                        substitutedBody));
        }

        if (app.Arguments.Count - 1 == lambda.Lambda.Arguments.Count)
        {
            return substitutedBody.Value;
        }

        return
            new Stil4mElmSyntax7.Expression.Application(
                [.. new[] { substitutedBody }.Concat(app.Arguments.Skip(lambda.Lambda.Arguments.Count + 1))]);
    }

    internal static Node<Stil4mElmSyntax7.Expression> ApplyConsumedArgumentBindings(
        Node<Stil4mElmSyntax7.Expression> body,
        IReadOnlyList<Node<Stil4mElmSyntax7.Pattern>> parameters,
        IReadOnlyList<Node<Stil4mElmSyntax7.Expression>> consumedArgs)
    {
        var letDeclarations = new List<Node<Stil4mElmSyntax7.Expression.LetDeclaration>>();
        var substitutions = new Dictionary<string, Node<Stil4mElmSyntax7.Expression>>();

        for (var index = 0; index < consumedArgs.Count; index++)
        {
            var parameter = parameters[index];
            var argument = consumedArgs[index];

            switch (Stil4mElmSyntax7.SyntaxAnalysis.UnwrapParenthesized(parameter.Value))
            {
                case Stil4mElmSyntax7.Pattern.VarPattern varPattern:
                    substitutions[varPattern.Name] = argument;
                    break;

                case Stil4mElmSyntax7.Pattern.AllPattern:
                case Stil4mElmSyntax7.Pattern.UnitPattern:
                    break;

                default:
                    letDeclarations.Add(
                        new Node<Stil4mElmSyntax7.Expression.LetDeclaration>(
                            ZeroRange,
                            new Stil4mElmSyntax7.Expression.LetDeclaration.LetDestructuring(
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
            new Node<Stil4mElmSyntax7.Expression>(
                ZeroRange,
                new Stil4mElmSyntax7.Expression.LetExpression(
                    new Stil4mElmSyntax7.Expression.LetBlock(
                        Declarations: [.. letDeclarations],
                        Expression: substitutedBody)));
    }

    internal static bool IsReferencePreservingWrapperField(
        Stil4mElmSyntax7.Expression expr,
        string variableName)
    {
        expr = Stil4mElmSyntax7.SyntaxAnalysis.UnwrapParenthesized(expr);

        if (IsLocalVariableReference(expr, variableName))
        {
            return true;
        }

        if (expr is not Stil4mElmSyntax7.Expression.LambdaExpression lambda)
        {
            return false;
        }

        var expectedArguments = new List<Node<Stil4mElmSyntax7.Expression>>();

        foreach (var parameter in lambda.Lambda.Arguments)
        {
            switch (Stil4mElmSyntax7.SyntaxAnalysis.UnwrapParenthesized(parameter.Value))
            {
                case Stil4mElmSyntax7.Pattern.VarPattern varPattern:
                    expectedArguments.Add(
                        new Node<Stil4mElmSyntax7.Expression>(
                            ZeroRange,
                            new Stil4mElmSyntax7.Expression.FunctionOrValue([], varPattern.Name)));

                    break;

                case Stil4mElmSyntax7.Pattern.UnitPattern:
                    expectedArguments.Add(
                        new Node<Stil4mElmSyntax7.Expression>(
                            ZeroRange,
                            new Stil4mElmSyntax7.Expression.UnitExpr()));

                    break;

                default:
                    return false;
            }
        }

        var lambdaBody = Stil4mElmSyntax7.SyntaxAnalysis.UnwrapParenthesized(lambda.Lambda.Expression.Value);

        if (lambdaBody is not Stil4mElmSyntax7.Expression.Application app ||
            app.Arguments.Count != expectedArguments.Count + 1 ||
            !IsLocalVariableReference(Stil4mElmSyntax7.SyntaxAnalysis.UnwrapParenthesized(app.Arguments[0].Value), variableName))
        {
            return false;
        }

        for (var index = 0; index < expectedArguments.Count; index++)
        {
            if (!expectedArguments[index].Value.Equals(
                Stil4mElmSyntax7.SyntaxAnalysis.UnwrapParenthesized(app.Arguments[index + 1].Value)))
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
    internal static Stil4mElmSyntax7.Expression ParenthesizeApplicationArguments(Stil4mElmSyntax7.Expression expr)
    {
        if (expr is not Stil4mElmSyntax7.Expression.Application app || app.Arguments.Count < 2)
        {
            return expr;
        }

        var newArgs = new List<Node<Stil4mElmSyntax7.Expression>>(app.Arguments.Count) { app.Arguments[0] };

        for (var i = 1; i < app.Arguments.Count; i++)
        {
            var arg = app.Arguments[i];

            if (NeedsParenthesesInApplicationArgument(arg.Value))
            {
                newArgs.Add(
                    new Node<Stil4mElmSyntax7.Expression>(
                        arg.Range,
                        new Stil4mElmSyntax7.Expression.ParenthesizedExpression(arg)));
            }
            else
            {
                newArgs.Add(arg);
            }
        }

        return new Stil4mElmSyntax7.Expression.Application([.. newArgs]);
    }

    internal static bool NeedsParenthesesInApplicationArgument(Stil4mElmSyntax7.Expression argument) =>
        argument switch
        {
            Stil4mElmSyntax7.Expression.Application innerApp => innerApp.Arguments.Count > 1,
            Stil4mElmSyntax7.Expression.LetExpression => true,
            Stil4mElmSyntax7.Expression.CaseExpression => true,
            Stil4mElmSyntax7.Expression.LambdaExpression => true,
            Stil4mElmSyntax7.Expression.IfBlock => true,
            Stil4mElmSyntax7.Expression.OperatorApplication => true,
            Stil4mElmSyntax7.Expression.Negation => true,

            // All other expression variants do not need extra parentheses when used
            // as an Application argument: literals, leaves and already-delimited forms.
            Stil4mElmSyntax7.Expression.UnitExpr or
            Stil4mElmSyntax7.Expression.Literal or
            Stil4mElmSyntax7.Expression.CharLiteral or
            Stil4mElmSyntax7.Expression.Integer or
            Stil4mElmSyntax7.Expression.Hex or
            Stil4mElmSyntax7.Expression.Floatable or
            Stil4mElmSyntax7.Expression.ListExpr or
            Stil4mElmSyntax7.Expression.FunctionOrValue or
            Stil4mElmSyntax7.Expression.PrefixOperator or
            Stil4mElmSyntax7.Expression.ParenthesizedExpression or
            Stil4mElmSyntax7.Expression.TupledExpression or
            Stil4mElmSyntax7.Expression.RecordExpr or
            Stil4mElmSyntax7.Expression.RecordAccess or
            Stil4mElmSyntax7.Expression.RecordAccessFunction or
            Stil4mElmSyntax7.Expression.RecordUpdateExpression or
            Stil4mElmSyntax7.Expression.GLSLExpression =>
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
    internal static Node<Stil4mElmSyntax7.Expression> ParenthesizeApplicationArgumentsRecursive(
        Node<Stil4mElmSyntax7.Expression> exprNode)
    {
        var expr = exprNode.Value;

        static Node<Stil4mElmSyntax7.Expression> Recurse(Node<Stil4mElmSyntax7.Expression> e) =>
            ParenthesizeApplicationArgumentsRecursive(e);

        var result =
            expr switch
            {
                Stil4mElmSyntax7.Expression.Application app when app.Arguments.Count >= 2 =>
                ParenthesizeApplicationArguments(
                    new Stil4mElmSyntax7.Expression.Application(
                        [.. app.Arguments.Select(Recurse)])),

                _ =>
                MapChildExpressions(expr, Recurse)
            };

        return new Node<Stil4mElmSyntax7.Expression>(exprNode.Range, result);
    }

    /// <summary>
    /// Resolves a <see cref="Stil4mElmSyntax7.Expression.FunctionOrValue"/>
    /// reference into a fully-qualified name. References without an
    /// explicit module qualifier are interpreted as belonging to the
    /// declaring module.
    /// </summary>
    internal static DeclQualifiedName ResolveReference(
        Stil4mElmSyntax7.Expression.FunctionOrValue reference,
        ModuleName currentModuleName)
    {
        if (reference.ModuleName.Count is 0)
            return DeclQualifiedName.Create(currentModuleName, reference.Name);

        return DeclQualifiedName.Create(reference.ModuleName, reference.Name);
    }

    /// <summary>
    /// Resolves a <see cref="Stil4mElmSyntax7.QualifiedNameRef"/> (e.g. a
    /// constructor name appearing in a pattern or constructor
    /// application) into a fully-qualified name. References without an
    /// explicit module qualifier are interpreted as belonging to the
    /// declaring module.
    /// </summary>
    internal static DeclQualifiedName ResolveReference(
        Stil4mElmSyntax7.QualifiedNameRef qname,
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
    internal static bool ContainsComplexExpression(Stil4mElmSyntax7.Expression expr)
    {
        var worklist = new Stack<Stil4mElmSyntax7.Expression>();
        worklist.Push(expr);

        while (worklist.Count > 0)
        {
            var current = worklist.Pop();

            if (current is Stil4mElmSyntax7.Expression.IfBlock or
                Stil4mElmSyntax7.Expression.CaseExpression or
                Stil4mElmSyntax7.Expression.LetExpression or
                Stil4mElmSyntax7.Expression.LambdaExpression)
            {
                return true;
            }

            Stil4mElmSyntax7.SyntaxAnalysis.ForEachChildExpression(current, worklist.Push);
        }

        return false;
    }

    /// <summary>
    /// Determines whether an expression is safe to substitute in any expression position
    /// when inlining a plain value. Only literal-like leaf expressions and simple
    /// constructor applications are considered safe.
    /// </summary>
    internal static bool IsPlainValueSafeToInline(Stil4mElmSyntax7.Expression expr) =>
        expr switch
        {
            Stil4mElmSyntax7.Expression.UnitExpr => true,
            Stil4mElmSyntax7.Expression.Literal => true,
            Stil4mElmSyntax7.Expression.CharLiteral => true,
            Stil4mElmSyntax7.Expression.Integer => true,
            Stil4mElmSyntax7.Expression.Hex => true,
            Stil4mElmSyntax7.Expression.Floatable => true,
            Stil4mElmSyntax7.Expression.FunctionOrValue => true,
            Stil4mElmSyntax7.Expression.ListExpr => true,
            Stil4mElmSyntax7.Expression.TupledExpression => true,
            Stil4mElmSyntax7.Expression.RecordExpr => true,
            Stil4mElmSyntax7.Expression.ParenthesizedExpression => true,
            Stil4mElmSyntax7.Expression.Negation => true,

            // All other expression variants are not considered safe for plain-value inlining
            // because they may have side-effects-like semantics (function application),
            // introduce control flow, or carry binding/scoping concerns that the caller
            // does not analyze here.
            Stil4mElmSyntax7.Expression.IfBlock or
            Stil4mElmSyntax7.Expression.PrefixOperator or
            Stil4mElmSyntax7.Expression.Application or
            Stil4mElmSyntax7.Expression.OperatorApplication or
            Stil4mElmSyntax7.Expression.LambdaExpression or
            Stil4mElmSyntax7.Expression.CaseExpression or
            Stil4mElmSyntax7.Expression.LetExpression or
            Stil4mElmSyntax7.Expression.RecordAccess or
            Stil4mElmSyntax7.Expression.RecordAccessFunction or
            Stil4mElmSyntax7.Expression.RecordUpdateExpression or
            Stil4mElmSyntax7.Expression.GLSLExpression =>
            false,

            _ =>
            throw new NotImplementedException(
                "IsPlainValueSafeToInline does not handle expression variant: " + expr.GetType().Name),
        };

    internal static bool BodyUnwrapsParameterAsConstructor(
        Node<Stil4mElmSyntax7.Expression> exprNode,
        string parameterName)
    {
        var worklist = new Stack<Stil4mElmSyntax7.Expression>();
        worklist.Push(exprNode.Value);

        while (worklist.Count > 0)
        {
            var expr = worklist.Pop();

            switch (expr)
            {
                case Stil4mElmSyntax7.Expression.LetExpression letExpr:
                    if (letExpr.Value.Declarations.Any(
                        declaration =>
                        declaration.Value is Stil4mElmSyntax7.Expression.LetDeclaration.LetDestructuring letDestr &&
                        IsLocalVariableReference(letDestr.Expression.Value, parameterName) &&
                        IsConstructorPattern(letDestr.Pattern.Value)))
                        return true;

                    break;

                case Stil4mElmSyntax7.Expression.CaseExpression caseExpr:
                    if (IsLocalVariableReference(caseExpr.CaseBlock.Expression.Value, parameterName) &&
                        caseExpr.CaseBlock.Cases.Any(c => IsConstructorPattern(c.Pattern.Value)))
                        return true;

                    break;

                // All other expression variants do not themselves witness a constructor
                // unwrap of <paramref name="parameterName"/> at this node; recursion into
                // their children is handled below by EnqueueChildExpressions.
                case Stil4mElmSyntax7.Expression.UnitExpr:
                case Stil4mElmSyntax7.Expression.Literal:
                case Stil4mElmSyntax7.Expression.CharLiteral:
                case Stil4mElmSyntax7.Expression.Integer:
                case Stil4mElmSyntax7.Expression.Hex:
                case Stil4mElmSyntax7.Expression.Floatable:
                case Stil4mElmSyntax7.Expression.Negation:
                case Stil4mElmSyntax7.Expression.ListExpr:
                case Stil4mElmSyntax7.Expression.FunctionOrValue:
                case Stil4mElmSyntax7.Expression.IfBlock:
                case Stil4mElmSyntax7.Expression.PrefixOperator:
                case Stil4mElmSyntax7.Expression.ParenthesizedExpression:
                case Stil4mElmSyntax7.Expression.Application:
                case Stil4mElmSyntax7.Expression.OperatorApplication:
                case Stil4mElmSyntax7.Expression.TupledExpression:
                case Stil4mElmSyntax7.Expression.LambdaExpression:
                case Stil4mElmSyntax7.Expression.RecordExpr:
                case Stil4mElmSyntax7.Expression.RecordAccess:
                case Stil4mElmSyntax7.Expression.RecordAccessFunction:
                case Stil4mElmSyntax7.Expression.RecordUpdateExpression:
                case Stil4mElmSyntax7.Expression.GLSLExpression:
                    break;

                default:
                    throw new NotImplementedException(
                        "BodyUnwrapsParameterAsConstructor does not handle expression variant: " +
                        expr.GetType().Name);
            }

            Stil4mElmSyntax7.SyntaxAnalysis.ForEachChildExpression(expr, worklist.Push);
        }

        return false;
    }

    internal static bool IsConstructorPattern(Stil4mElmSyntax7.Pattern pattern)
    {
        return pattern switch
        {
            Stil4mElmSyntax7.Pattern.NamedPattern => true,

            // Alias patterns like `((Parser parse) as element)` still represent a constructor
            // pattern for specialization purposes, so unwrap the alias and inspect the inner pattern.
            Stil4mElmSyntax7.Pattern.AsPattern asPattern => IsConstructorPattern(asPattern.Pattern.Value),
            Stil4mElmSyntax7.Pattern.ParenthesizedPattern paren => IsConstructorPattern(paren.Pattern.Value),

            _ =>
            false
        };
    }

    internal static Stil4mElmSyntax7.Pattern.NamedPattern? TryUnwrapToNamedPattern(Stil4mElmSyntax7.Pattern pattern)
    {
        return pattern switch
        {
            Stil4mElmSyntax7.Pattern.NamedPattern np => np,
            Stil4mElmSyntax7.Pattern.AsPattern ap => TryUnwrapToNamedPattern(ap.Pattern.Value),
            Stil4mElmSyntax7.Pattern.ParenthesizedPattern pp => TryUnwrapToNamedPattern(pp.Pattern.Value),

            _ =>
            null
        };
    }

    internal static string? TryGetAliasNameFromPattern(Stil4mElmSyntax7.Pattern pattern)
    {
        return pattern switch
        {
            Stil4mElmSyntax7.Pattern.AsPattern ap => ap.Name.Value,
            Stil4mElmSyntax7.Pattern.ParenthesizedPattern pp => TryGetAliasNameFromPattern(pp.Pattern.Value),

            _ =>
            null
        };
    }

    /// <summary>
    /// Peels nested <see cref="Stil4mElmSyntax7.Pattern.ParenthesizedPattern"/>
    /// and <see cref="Stil4mElmSyntax7.Pattern.AsPattern"/> wrappers off
    /// <paramref name="pattern"/>, returning the innermost pattern.
    /// </summary>
    internal static Stil4mElmSyntax7.Pattern PeelPatternParenthesesAndAsBinder(Stil4mElmSyntax7.Pattern pattern)
    {
        while (true)
        {
            switch (pattern)
            {
                case Stil4mElmSyntax7.Pattern.ParenthesizedPattern p:
                    pattern = p.Pattern.Value;
                    continue;

                case Stil4mElmSyntax7.Pattern.AsPattern a:
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
    /// <item>A bare <see cref="Stil4mElmSyntax7.Pattern.VarPattern"/> (the
    /// pattern's own name).</item>
    /// <item>An <see cref="Stil4mElmSyntax7.Pattern.AsPattern"/> (the
    /// <c>as</c>-name).</item>
    /// <item>A <see cref="Stil4mElmSyntax7.Pattern.NamedPattern"/> with a
    /// single argument (the inner var name; the destructuring shape
    /// <c>(Ctor inner)</c>).</item>
    /// <item>Any of the above wrapped in
    /// <see cref="Stil4mElmSyntax7.Pattern.ParenthesizedPattern"/>.</item>
    /// </list>
    /// Returns <c>null</c> for any other pattern shape.
    /// </summary>
    internal static string? TryGetParameterDisplayName(Stil4mElmSyntax7.Pattern pattern)
    {
        while (true)
        {
            switch (pattern)
            {
                case Stil4mElmSyntax7.Pattern.VarPattern vp:
                    return vp.Name;

                case Stil4mElmSyntax7.Pattern.AsPattern ap:
                    return ap.Name.Value;

                case Stil4mElmSyntax7.Pattern.ParenthesizedPattern pp:
                    pattern = pp.Pattern.Value;
                    continue;

                case Stil4mElmSyntax7.Pattern.NamedPattern np when np.Arguments.Count is 1:
                    pattern = np.Arguments[0].Value;
                    continue;

                default:
                    return null;
            }
        }
    }

    internal static ConstructorApplication? TryDeconstructConstructorApplication(
        Node<Stil4mElmSyntax7.Expression> exprNode)
    {
        return TryDeconstructConstructorApplication(exprNode.Value);
    }

    internal static ConstructorApplication? TryDeconstructConstructorApplication(
        Stil4mElmSyntax7.Expression expr)
    {
        switch (Stil4mElmSyntax7.SyntaxAnalysis.UnwrapParenthesized(expr))
        {
            case Stil4mElmSyntax7.Expression.FunctionOrValue funcOrValue:
                return
                    new ConstructorApplication(
                        new Stil4mElmSyntax7.QualifiedNameRef(funcOrValue.ModuleName, funcOrValue.Name),
                        []);

            case Stil4mElmSyntax7.Expression.Application app
            when app.Arguments.Count > 0 &&
                     app.Arguments[0].Value is Stil4mElmSyntax7.Expression.FunctionOrValue constructorRef:

                return
                    new ConstructorApplication(
                        new Stil4mElmSyntax7.QualifiedNameRef(constructorRef.ModuleName, constructorRef.Name),
                        [.. app.Arguments.Skip(1)]);

            // Application that does not match the constructor-shape guard above (e.g. the
            // function position is not a bare FunctionOrValue) is not a constructor
            // application.
            case Stil4mElmSyntax7.Expression.Application:
            // Other expression variants are simply not constructor applications.
            case Stil4mElmSyntax7.Expression.UnitExpr:
            case Stil4mElmSyntax7.Expression.Literal:
            case Stil4mElmSyntax7.Expression.CharLiteral:
            case Stil4mElmSyntax7.Expression.Integer:
            case Stil4mElmSyntax7.Expression.Hex:
            case Stil4mElmSyntax7.Expression.Floatable:
            case Stil4mElmSyntax7.Expression.Negation:
            case Stil4mElmSyntax7.Expression.ListExpr:
            case Stil4mElmSyntax7.Expression.IfBlock:
            case Stil4mElmSyntax7.Expression.PrefixOperator:
            // ParenthesizedExpression is unwrapped above and never reaches the switch.
            case Stil4mElmSyntax7.Expression.OperatorApplication:
            case Stil4mElmSyntax7.Expression.TupledExpression:
            case Stil4mElmSyntax7.Expression.LambdaExpression:
            case Stil4mElmSyntax7.Expression.CaseExpression:
            case Stil4mElmSyntax7.Expression.LetExpression:
            case Stil4mElmSyntax7.Expression.RecordExpr:
            case Stil4mElmSyntax7.Expression.RecordAccess:
            case Stil4mElmSyntax7.Expression.RecordAccessFunction:
            case Stil4mElmSyntax7.Expression.RecordUpdateExpression:
            case Stil4mElmSyntax7.Expression.GLSLExpression:
                return null;

            default:
                throw new NotImplementedException(
                    "TryDeconstructConstructorApplication does not handle expression variant: " +
                    expr.GetType().Name);
        }
    }

    internal static bool AreLetDeclarationsIgnorableForConstructorResolution(
        IReadOnlyList<Node<Stil4mElmSyntax7.Expression.LetDeclaration>> declarations)
    {
        foreach (var declaration in declarations)
        {
            if (declaration.Value is not Stil4mElmSyntax7.Expression.LetDeclaration.LetDestructuring letDestr)
            {
                return false;
            }

            if (Stil4mElmSyntax7.SyntaxAnalysis.UnwrapParenthesized(letDestr.Pattern.Value) is not Stil4mElmSyntax7.Pattern.AllPattern)
            {
                return false;
            }
        }

        return declarations.Count > 0;
    }

    internal static Node<Stil4mElmSyntax7.Expression> BuildConstructorApplication(
        Stil4mElmSyntax7.QualifiedNameRef constructorName,
        IReadOnlyList<Node<Stil4mElmSyntax7.Expression>> fieldExpressions)
    {
        var constructorExpr =
            new Node<Stil4mElmSyntax7.Expression>(
                ZeroRange,
                new Stil4mElmSyntax7.Expression.FunctionOrValue(constructorName.ModuleName, constructorName.Name));

        if (fieldExpressions.Count is 0)
            return constructorExpr;

        return
            new Node<Stil4mElmSyntax7.Expression>(
                ZeroRange,
                new Stil4mElmSyntax7.Expression.Application(
                    [.. new[] { constructorExpr }.Concat(fieldExpressions)]));
    }

    internal static bool IsLocalVariableReference(
        Stil4mElmSyntax7.Expression expr,
        string variableName)
    {
        return
            Stil4mElmSyntax7.SyntaxAnalysis.UnwrapParenthesized(expr) is Stil4mElmSyntax7.Expression.FunctionOrValue funcOrValue &&
            funcOrValue.ModuleName.Count is 0 &&
            funcOrValue.Name == variableName;
    }

    internal static bool AreEquivalentConstructorNames(
        Stil4mElmSyntax7.QualifiedNameRef left,
        Stil4mElmSyntax7.QualifiedNameRef right)
    {
        return
            left.Equals(right) ||
            (left.Name == right.Name &&
            (left.ModuleName.Count is 0 || right.ModuleName.Count is 0));
    }

    internal static bool AreEquivalentConstructorNames(
        Stil4mElmSyntax7.QualifiedNameRef left,
        DeclQualifiedName right)
    {
        return
            (left.Name == right.DeclName &&
            (left.ModuleName.Count is 0 || left.ModuleName.SequenceEqual(right.Namespaces)));
    }

    internal static int CountUnshadowedLocalVariableReferences(
        Stil4mElmSyntax7.Expression expr,
        string variableName)
    {
        return CountUnshadowedLocalVariableReferences(expr, variableName, shadowed: false);
    }

    internal static int CountUnshadowedLocalVariableReferences(
        Stil4mElmSyntax7.Expression expr,
        string variableName,
        bool shadowed)
    {
        switch (expr)
        {
            case Stil4mElmSyntax7.Expression.FunctionOrValue funcOrValue:
                return
                    shadowed ||
                    funcOrValue.ModuleName.Count is not 0 ||
                    funcOrValue.Name != variableName
                    ?
                    0
                    :
                    1;

            case Stil4mElmSyntax7.Expression.Application app:
                return
                    app.Arguments.Sum(
                        argument => CountUnshadowedLocalVariableReferences(argument.Value, variableName, shadowed));

            case Stil4mElmSyntax7.Expression.ParenthesizedExpression paren:
                return CountUnshadowedLocalVariableReferences(paren.Expression.Value, variableName, shadowed);

            case Stil4mElmSyntax7.Expression.IfBlock ifBlock:
                return
                    CountUnshadowedLocalVariableReferences(ifBlock.Condition.Value, variableName, shadowed) +
                    CountUnshadowedLocalVariableReferences(ifBlock.ThenBlock.Value, variableName, shadowed) +
                    CountUnshadowedLocalVariableReferences(ifBlock.ElseBlock.Value, variableName, shadowed);

            case Stil4mElmSyntax7.Expression.CaseExpression caseExpr:
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

            case Stil4mElmSyntax7.Expression.LetExpression letExpr:
                {
                    var letBoundNames = new HashSet<string>();
                    var letExprCount = 0;

                    foreach (var declaration in letExpr.Value.Declarations)
                    {
                        switch (declaration.Value)
                        {
                            case Stil4mElmSyntax7.Expression.LetDeclaration.LetFunction letFunc:
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

                            case Stil4mElmSyntax7.Expression.LetDeclaration.LetDestructuring letDestr:
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

            case Stil4mElmSyntax7.Expression.LambdaExpression lambda:
                return
                    CountUnshadowedLocalVariableReferences(
                        lambda.Lambda.Expression.Value,
                        variableName,
                        shadowed ||
                        lambda.Lambda.Arguments.Any(
                            arg => CollectPatternNames(arg.Value).Contains(variableName)));

            case Stil4mElmSyntax7.Expression.ListExpr listExpr:
                return
                    listExpr.Elements.Sum(
                        element => CountUnshadowedLocalVariableReferences(element.Value, variableName, shadowed));

            case Stil4mElmSyntax7.Expression.TupledExpression tupled:
                return
                    tupled.Elements.Sum(
                        element => CountUnshadowedLocalVariableReferences(element.Value, variableName, shadowed));

            case Stil4mElmSyntax7.Expression.RecordExpr recordExpr:
                return
                    recordExpr.Fields.Sum(
                        field =>
                        CountUnshadowedLocalVariableReferences(field.Value.valueExpr.Value, variableName, shadowed));

            case Stil4mElmSyntax7.Expression.RecordUpdateExpression recordUpdate:

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

            case Stil4mElmSyntax7.Expression.RecordAccess recordAccess:
                return CountUnshadowedLocalVariableReferences(recordAccess.Record.Value, variableName, shadowed);

            case Stil4mElmSyntax7.Expression.Negation negation:
                return CountUnshadowedLocalVariableReferences(negation.Expression.Value, variableName, shadowed);

            case Stil4mElmSyntax7.Expression.OperatorApplication opApp:
                return
                    CountUnshadowedLocalVariableReferences(opApp.Left.Value, variableName, shadowed) +
                    CountUnshadowedLocalVariableReferences(opApp.Right.Value, variableName, shadowed);

            // Leaf expression variants (no nested expressions and no local variable references):
            // each contributes zero references regardless of the queried variable name.
            case Stil4mElmSyntax7.Expression.UnitExpr:
            case Stil4mElmSyntax7.Expression.Literal:
            case Stil4mElmSyntax7.Expression.CharLiteral:
            case Stil4mElmSyntax7.Expression.Integer:
            case Stil4mElmSyntax7.Expression.Hex:
            case Stil4mElmSyntax7.Expression.Floatable:
            case Stil4mElmSyntax7.Expression.PrefixOperator:
            case Stil4mElmSyntax7.Expression.RecordAccessFunction:
            case Stil4mElmSyntax7.Expression.GLSLExpression:
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
    internal static Stil4mElmSyntax7.Expression MapChildExpressions(
        Stil4mElmSyntax7.Expression expr,
        Func<Node<Stil4mElmSyntax7.Expression>, Node<Stil4mElmSyntax7.Expression>> mapChild)
    {
        return expr switch
        {
            Stil4mElmSyntax7.Expression.Application app =>
            new Stil4mElmSyntax7.Expression.Application(
                [.. app.Arguments.Select(mapChild)]),

            Stil4mElmSyntax7.Expression.ParenthesizedExpression paren =>
            new Stil4mElmSyntax7.Expression.ParenthesizedExpression(
                mapChild(paren.Expression)),

            Stil4mElmSyntax7.Expression.IfBlock ifBlock =>
            new Stil4mElmSyntax7.Expression.IfBlock(
                mapChild(ifBlock.Condition),
                mapChild(ifBlock.ThenBlock),
                mapChild(ifBlock.ElseBlock)),

            Stil4mElmSyntax7.Expression.CaseExpression caseExpr =>
            new Stil4mElmSyntax7.Expression.CaseExpression(
                new Stil4mElmSyntax7.CaseBlock(
                    mapChild(caseExpr.CaseBlock.Expression),
                    [
                    .. caseExpr.CaseBlock.Cases.Select(
                        c => new Stil4mElmSyntax7.Case(c.Pattern, mapChild(c.Expression)))
                    ])),

            Stil4mElmSyntax7.Expression.LetExpression letExpr =>
            new Stil4mElmSyntax7.Expression.LetExpression(
                new Stil4mElmSyntax7.Expression.LetBlock(
                    Declarations:
                    [
                    .. letExpr.Value.Declarations.Select(
                        d =>
                        {
                            var rewrittenDecl =
                                d.Value switch
                                {
                                    Stil4mElmSyntax7.Expression.LetDeclaration.LetFunction letFunc =>
                                    new Stil4mElmSyntax7.Expression.LetDeclaration.LetFunction(
                                        letFunc.Function with
                                        {
                                            Declaration =
                                            new Node<Stil4mElmSyntax7.FunctionImplementation>(
                                                letFunc.Function.Declaration.Range,
                                                letFunc.Function.Declaration.Value with
                                                {
                                                    Expression =
                                                    mapChild(letFunc.Function.Declaration.Value.Expression)
                                                })
                                        }),

                                    Stil4mElmSyntax7.Expression.LetDeclaration.LetDestructuring letDestr =>
                                    new Stil4mElmSyntax7.Expression.LetDeclaration.LetDestructuring(
                                        letDestr.Pattern,
                                        mapChild(letDestr.Expression)),

                                    _ =>
                                    d.Value
                                };

                            return new Node<Stil4mElmSyntax7.Expression.LetDeclaration>(d.Range, rewrittenDecl);
                        })
                    ],
                    Expression:
                    mapChild(letExpr.Value.Expression))),

            Stil4mElmSyntax7.Expression.LambdaExpression lambda =>
            new Stil4mElmSyntax7.Expression.LambdaExpression(
                new Stil4mElmSyntax7.LambdaStruct(
                    lambda.Lambda.Arguments,
                    mapChild(lambda.Lambda.Expression))),

            Stil4mElmSyntax7.Expression.ListExpr listExpr =>
            new Stil4mElmSyntax7.Expression.ListExpr(
                [.. listExpr.Elements.Select(mapChild)]),

            Stil4mElmSyntax7.Expression.TupledExpression tupled =>
            new Stil4mElmSyntax7.Expression.TupledExpression(
                [.. tupled.Elements.Select(mapChild)]),

            Stil4mElmSyntax7.Expression.RecordExpr recordExpr =>
            new Stil4mElmSyntax7.Expression.RecordExpr(
                [
                .. recordExpr.Fields.Select(
                    f =>
                    new Node<(Node<string> fieldName, Node<Stil4mElmSyntax7.Expression> valueExpr)>(
                        f.Range,
                        (f.Value.fieldName, mapChild(f.Value.valueExpr))))
                ]),

            Stil4mElmSyntax7.Expression.RecordUpdateExpression recordUpdate =>
            new Stil4mElmSyntax7.Expression.RecordUpdateExpression(
                recordUpdate.RecordName,
                [
                .. recordUpdate.Fields.Select(
                    f =>
                    new Node<(Node<string> fieldName, Node<Stil4mElmSyntax7.Expression> valueExpr)>(
                        f.Range,
                        (f.Value.fieldName, mapChild(f.Value.valueExpr))))
                ]),

            Stil4mElmSyntax7.Expression.RecordAccess recordAccess =>
            new Stil4mElmSyntax7.Expression.RecordAccess(
                mapChild(recordAccess.Record),
                recordAccess.FieldName),

            Stil4mElmSyntax7.Expression.Negation negation =>
            new Stil4mElmSyntax7.Expression.Negation(
                mapChild(negation.Expression)),

            Stil4mElmSyntax7.Expression.OperatorApplication opApp =>
            new Stil4mElmSyntax7.Expression.OperatorApplication(
                opApp.Operator,
                opApp.Direction,
                mapChild(opApp.Left),
                mapChild(opApp.Right)),

            // Leaf expression variants have no child expressions to map; return them
            // unchanged. They are listed explicitly so that the throwing default below
            // never fires for valid expression values.
            Stil4mElmSyntax7.Expression.UnitExpr or
            Stil4mElmSyntax7.Expression.Literal or
            Stil4mElmSyntax7.Expression.CharLiteral or
            Stil4mElmSyntax7.Expression.Integer or
            Stil4mElmSyntax7.Expression.Hex or
            Stil4mElmSyntax7.Expression.Floatable or
            Stil4mElmSyntax7.Expression.FunctionOrValue or
            Stil4mElmSyntax7.Expression.PrefixOperator or
            Stil4mElmSyntax7.Expression.RecordAccessFunction or
            Stil4mElmSyntax7.Expression.GLSLExpression =>
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
    internal static Stil4mElmSyntax7.Expression MapChildExpressionsWithScope(
        Stil4mElmSyntax7.Expression expr,
        ImmutableHashSet<string> currentScope,
        Func<Node<Stil4mElmSyntax7.Expression>, ImmutableHashSet<string>, Node<Stil4mElmSyntax7.Expression>> mapChild)
    {
        switch (expr)
        {
            case Stil4mElmSyntax7.Expression.LambdaExpression lambda:
                {
                    var bodyScope = ExtendScopeWithPatternList(currentScope, lambda.Lambda.Arguments);

                    var bodyNode = mapChild(lambda.Lambda.Expression, bodyScope);

                    if (ReferenceEquals(bodyNode, lambda.Lambda.Expression))
                        return expr;

                    return
                        new Stil4mElmSyntax7.Expression.LambdaExpression(
                            new Stil4mElmSyntax7.LambdaStruct(
                                lambda.Lambda.Arguments,
                                bodyNode));
                }

            case Stil4mElmSyntax7.Expression.LetExpression letExpr:
                {
                    // Mutual recursion: every let-bound name is visible to
                    // every declaration body and to the let's final
                    // expression.
                    var letScope = currentScope;

                    foreach (var declNode in letExpr.Value.Declarations)
                        letScope = AddLetDeclarationBindingsToScope(declNode.Value, letScope);

                    var newDecls =
                        new List<Node<Stil4mElmSyntax7.Expression.LetDeclaration>>(letExpr.Value.Declarations.Count);

                    var declsChanged = false;

                    foreach (var declNode in letExpr.Value.Declarations)
                    {
                        var rewrittenDecl =
                            MapChildExpressionsInLetDeclarationWithScope(declNode.Value, letScope, mapChild);

                        if (!ReferenceEquals(rewrittenDecl, declNode.Value))
                            declsChanged = true;

                        newDecls.Add(
                            new Node<Stil4mElmSyntax7.Expression.LetDeclaration>(declNode.Range, rewrittenDecl));
                    }

                    var bodyNode = mapChild(letExpr.Value.Expression, letScope);

                    if (!declsChanged && ReferenceEquals(bodyNode, letExpr.Value.Expression))
                        return expr;

                    return
                        new Stil4mElmSyntax7.Expression.LetExpression(
                            new Stil4mElmSyntax7.Expression.LetBlock(
                                Declarations: newDecls,
                                Expression: bodyNode));
                }

            case Stil4mElmSyntax7.Expression.CaseExpression caseExpr:
                {
                    var scrut = mapChild(caseExpr.CaseBlock.Expression, currentScope);

                    var newArms = new List<Stil4mElmSyntax7.Case>(caseExpr.CaseBlock.Cases.Count);

                    var armsChanged = false;

                    foreach (var arm in caseExpr.CaseBlock.Cases)
                    {
                        var armScope = ExtendScopeWithPattern(currentScope, arm.Pattern.Value);

                        var armBody = mapChild(arm.Expression, armScope);

                        if (!ReferenceEquals(armBody, arm.Expression))
                            armsChanged = true;

                        newArms.Add(new Stil4mElmSyntax7.Case(arm.Pattern, armBody));
                    }

                    if (!armsChanged && ReferenceEquals(scrut, caseExpr.CaseBlock.Expression))
                        return expr;

                    return
                        new Stil4mElmSyntax7.Expression.CaseExpression(
                            new Stil4mElmSyntax7.CaseBlock(
                                Expression: scrut,
                                Cases: newArms));
                }

            default:
                return MapChildExpressions(expr, child => mapChild(child, currentScope));
        }
    }

    private static Stil4mElmSyntax7.Expression.LetDeclaration
        MapChildExpressionsInLetDeclarationWithScope(
        Stil4mElmSyntax7.Expression.LetDeclaration letDecl,
        ImmutableHashSet<string> letScope,
        Func<Node<Stil4mElmSyntax7.Expression>, ImmutableHashSet<string>, Node<Stil4mElmSyntax7.Expression>> mapChild)
    {
        switch (letDecl)
        {
            case Stil4mElmSyntax7.Expression.LetDeclaration.LetFunction letFunc:
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
                            new Node<Stil4mElmSyntax7.FunctionImplementation>(
                                letFunc.Function.Declaration.Range,
                                newImpl),
                        };

                    return new Stil4mElmSyntax7.Expression.LetDeclaration.LetFunction(newFunc);
                }

            case Stil4mElmSyntax7.Expression.LetDeclaration.LetDestructuring letDest:
                {
                    var rewrittenExpr = mapChild(letDest.Expression, letScope);

                    if (ReferenceEquals(rewrittenExpr, letDest.Expression))
                        return letDecl;

                    return
                        new Stil4mElmSyntax7.Expression.LetDeclaration.LetDestructuring(
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
    /// <see cref="Stil4mElmSyntax7.Expression"/> reachable through it. The
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
        Stil4mElmSyntax7.Expression expression,
        ImmutableHashSet<string> initialScope,
        Action<Stil4mElmSyntax7.Expression, ImmutableHashSet<string>> onNode)
    {
        onNode(expression, initialScope);

        switch (expression)
        {
            case Stil4mElmSyntax7.Expression.LambdaExpression lambda:
                {
                    var bodyScope = ExtendScopeWithPatternList(initialScope, lambda.Lambda.Arguments);
                    WalkExpressionsWithScope(lambda.Lambda.Expression.Value, bodyScope, onNode);
                    break;
                }

            case Stil4mElmSyntax7.Expression.LetExpression letExpr:
                {
                    var letScope = initialScope;

                    foreach (var declNode in letExpr.Value.Declarations)
                        letScope = AddLetDeclarationBindingsToScope(declNode.Value, letScope);

                    foreach (var declNode in letExpr.Value.Declarations)
                    {
                        switch (declNode.Value)
                        {
                            case Stil4mElmSyntax7.Expression.LetDeclaration.LetFunction letFunc:
                                {
                                    var impl = letFunc.Function.Declaration.Value;
                                    var fnScope = ExtendScopeWithPatternList(letScope, impl.Arguments);
                                    WalkExpressionsWithScope(impl.Expression.Value, fnScope, onNode);
                                    break;
                                }

                            case Stil4mElmSyntax7.Expression.LetDeclaration.LetDestructuring letDest:
                                WalkExpressionsWithScope(letDest.Expression.Value, letScope, onNode);
                                break;
                        }
                    }

                    WalkExpressionsWithScope(letExpr.Value.Expression.Value, letScope, onNode);
                    break;
                }

            case Stil4mElmSyntax7.Expression.CaseExpression caseExpr:
                {
                    WalkExpressionsWithScope(caseExpr.CaseBlock.Expression.Value, initialScope, onNode);

                    foreach (var arm in caseExpr.CaseBlock.Cases)
                    {
                        var armScope = ExtendScopeWithPattern(initialScope, arm.Pattern.Value);
                        WalkExpressionsWithScope(arm.Expression.Value, armScope, onNode);
                    }

                    break;
                }

            case Stil4mElmSyntax7.Expression.Application app:
                foreach (var arg in app.Arguments)
                    WalkExpressionsWithScope(arg.Value, initialScope, onNode);

                break;

            case Stil4mElmSyntax7.Expression.OperatorApplication opApp:
                WalkExpressionsWithScope(opApp.Left.Value, initialScope, onNode);
                WalkExpressionsWithScope(opApp.Right.Value, initialScope, onNode);
                break;

            case Stil4mElmSyntax7.Expression.ParenthesizedExpression paren:
                WalkExpressionsWithScope(paren.Expression.Value, initialScope, onNode);
                break;

            case Stil4mElmSyntax7.Expression.IfBlock ifBlock:
                WalkExpressionsWithScope(ifBlock.Condition.Value, initialScope, onNode);
                WalkExpressionsWithScope(ifBlock.ThenBlock.Value, initialScope, onNode);
                WalkExpressionsWithScope(ifBlock.ElseBlock.Value, initialScope, onNode);
                break;

            case Stil4mElmSyntax7.Expression.ListExpr listExpr:
                foreach (var element in listExpr.Elements)
                    WalkExpressionsWithScope(element.Value, initialScope, onNode);

                break;

            case Stil4mElmSyntax7.Expression.TupledExpression tupled:
                foreach (var element in tupled.Elements)
                    WalkExpressionsWithScope(element.Value, initialScope, onNode);

                break;

            case Stil4mElmSyntax7.Expression.RecordExpr recordExpr:
                foreach (var field in recordExpr.Fields)
                    WalkExpressionsWithScope(field.Value.valueExpr.Value, initialScope, onNode);

                break;

            case Stil4mElmSyntax7.Expression.RecordUpdateExpression recordUpdate:
                foreach (var field in recordUpdate.Fields)
                    WalkExpressionsWithScope(field.Value.valueExpr.Value, initialScope, onNode);

                break;

            case Stil4mElmSyntax7.Expression.RecordAccess recordAccess:
                WalkExpressionsWithScope(recordAccess.Record.Value, initialScope, onNode);
                break;

            case Stil4mElmSyntax7.Expression.Negation negation:
                WalkExpressionsWithScope(negation.Expression.Value, initialScope, onNode);
                break;

            // Leaf variants — already visited via onNode at the top.
            case Stil4mElmSyntax7.Expression.FunctionOrValue:
            case Stil4mElmSyntax7.Expression.UnitExpr:
            case Stil4mElmSyntax7.Expression.Literal:
            case Stil4mElmSyntax7.Expression.CharLiteral:
            case Stil4mElmSyntax7.Expression.Integer:
            case Stil4mElmSyntax7.Expression.Hex:
            case Stil4mElmSyntax7.Expression.Floatable:
            case Stil4mElmSyntax7.Expression.PrefixOperator:
            case Stil4mElmSyntax7.Expression.RecordAccessFunction:
            case Stil4mElmSyntax7.Expression.GLSLExpression:
                break;
        }
    }

    private static ImmutableHashSet<string> ExtendScopeWithPatternList(
        ImmutableHashSet<string> scope,
        IReadOnlyList<Node<Stil4mElmSyntax7.Pattern>> patterns)
    {
        var extended = scope;

        foreach (var patternNode in patterns)
            extended = ExtendScopeWithPattern(extended, patternNode.Value);

        return extended;
    }

    private static ImmutableHashSet<string> ExtendScopeWithPattern(
        ImmutableHashSet<string> scope,
        Stil4mElmSyntax7.Pattern pattern)
    {
        var names = CollectPatternNames(pattern);

        return names.Count is 0 ? scope : scope.Union(names);
    }

    private static ImmutableHashSet<string> AddLetDeclarationBindingsToScope(
        Stil4mElmSyntax7.Expression.LetDeclaration letDecl,
        ImmutableHashSet<string> scope)
    {
        switch (letDecl)
        {
            case Stil4mElmSyntax7.Expression.LetDeclaration.LetFunction lf:
                return scope.Add(lf.Function.Declaration.Value.Name.Value);

            case Stil4mElmSyntax7.Expression.LetDeclaration.LetDestructuring ld:
                return ExtendScopeWithPattern(scope, ld.Pattern.Value);

            default:
                return scope;
        }
    }

    /// <summary>
    /// Unified expression tree rewriter. Recursively traverses all expression variants,
    /// delegating <see cref="Stil4mElmSyntax7.Expression.Application"/> nodes to the supplied
    /// <paramref name="rewriteApplication"/> function. All other expression variants are
    /// structurally rebuilt with their children rewritten via <see cref="MapChildExpressions"/>.
    /// </summary>
    internal static Node<Stil4mElmSyntax7.Expression> RewriteExpressionTree(
        Node<Stil4mElmSyntax7.Expression> exprNode,
        Func<Stil4mElmSyntax7.Expression.Application,
            Func<Node<Stil4mElmSyntax7.Expression>, Node<Stil4mElmSyntax7.Expression>>,
            Stil4mElmSyntax7.Expression> rewriteApplication)
    {
        Node<Stil4mElmSyntax7.Expression> Recurse(Node<Stil4mElmSyntax7.Expression> node) =>
            RewriteExpressionTree(node, rewriteApplication);

        var expr = exprNode.Value;

        var rewrittenExpr =
            expr switch
            {
                Stil4mElmSyntax7.Expression.Application app =>
                rewriteApplication(app, Recurse),

                // All other expression variants are rebuilt structurally with their
                // children rewritten. Each variant is enumerated explicitly so the
                // throwing default never fires for valid expression values.
                Stil4mElmSyntax7.Expression.UnitExpr or
                Stil4mElmSyntax7.Expression.Literal or
                Stil4mElmSyntax7.Expression.CharLiteral or
                Stil4mElmSyntax7.Expression.Integer or
                Stil4mElmSyntax7.Expression.Hex or
                Stil4mElmSyntax7.Expression.Floatable or
                Stil4mElmSyntax7.Expression.Negation or
                Stil4mElmSyntax7.Expression.ListExpr or
                Stil4mElmSyntax7.Expression.FunctionOrValue or
                Stil4mElmSyntax7.Expression.IfBlock or
                Stil4mElmSyntax7.Expression.PrefixOperator or
                Stil4mElmSyntax7.Expression.ParenthesizedExpression or
                Stil4mElmSyntax7.Expression.OperatorApplication or
                Stil4mElmSyntax7.Expression.TupledExpression or
                Stil4mElmSyntax7.Expression.LambdaExpression or
                Stil4mElmSyntax7.Expression.CaseExpression or
                Stil4mElmSyntax7.Expression.LetExpression or
                Stil4mElmSyntax7.Expression.RecordExpr or
                Stil4mElmSyntax7.Expression.RecordAccess or
                Stil4mElmSyntax7.Expression.RecordAccessFunction or
                Stil4mElmSyntax7.Expression.RecordUpdateExpression or
                Stil4mElmSyntax7.Expression.GLSLExpression =>
                MapChildExpressions(expr, Recurse),

                _ =>
                throw new NotImplementedException(
                    "RewriteExpressionTree does not handle expression variant: " + expr.GetType().Name)
            };

        return new Node<Stil4mElmSyntax7.Expression>(exprNode.Range, rewrittenExpr);
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
    public static Node<Stil4mElmSyntax7.Expression> SubstituteCaptureAvoiding(
        Node<Stil4mElmSyntax7.Expression> exprNode,
        IReadOnlyDictionary<string, Node<Stil4mElmSyntax7.Expression>> substitutions) =>
        SubstituteInExpression(exprNode, substitutions);

    internal static Node<Stil4mElmSyntax7.Expression> SubstituteInExpression(
        Node<Stil4mElmSyntax7.Expression> exprNode,
        IReadOnlyDictionary<string, Node<Stil4mElmSyntax7.Expression>> substitutions)
    {
        var expr = exprNode.Value;

        var substitutedExpr =
            expr switch
            {
                Stil4mElmSyntax7.Expression.FunctionOrValue funcOrValue when funcOrValue.ModuleName.Count is 0 &&
                    substitutions.TryGetValue(funcOrValue.Name, out var replacement) =>
                replacement.Value,

                Stil4mElmSyntax7.Expression.CaseExpression caseExpr =>
                TrySubstituteSingleChoiceTagCase(caseExpr.CaseBlock, substitutions)?.Value ??
                new Stil4mElmSyntax7.Expression.CaseExpression(
                    SubstituteInCaseBlock(caseExpr.CaseBlock, substitutions)),

                Stil4mElmSyntax7.Expression.LetExpression letExpr =>
                new Stil4mElmSyntax7.Expression.LetExpression(
                    SubstituteInLetBlock(letExpr.Value, substitutions)),

                Stil4mElmSyntax7.Expression.LambdaExpression lambda =>
                new Stil4mElmSyntax7.Expression.LambdaExpression(
                    SubstituteInLambdaStruct(lambda.Lambda, substitutions)),

                Stil4mElmSyntax7.Expression.RecordUpdateExpression recordUpdate =>
                SubstituteInRecordUpdateExpression(recordUpdate, substitutions),

                // FunctionOrValue references that don't match the substitution guard above
                // (qualified, or unqualified but not in the substitutions map) pass through
                // unchanged. The bare-FunctionOrValue rewrite is captured by the guarded
                // case at the top; this case catches the remaining FunctionOrValue values
                // so the throwing default below never fires.
                Stil4mElmSyntax7.Expression.FunctionOrValue =>
                expr,

                // All other expression variants delegate to MapChildExpressions for
                // structural recursion. They are enumerated explicitly so that the
                // throwing default below never fires for valid expression values.
                Stil4mElmSyntax7.Expression.UnitExpr or
                Stil4mElmSyntax7.Expression.Literal or
                Stil4mElmSyntax7.Expression.CharLiteral or
                Stil4mElmSyntax7.Expression.Integer or
                Stil4mElmSyntax7.Expression.Hex or
                Stil4mElmSyntax7.Expression.Floatable or
                Stil4mElmSyntax7.Expression.Negation or
                Stil4mElmSyntax7.Expression.ListExpr or
                Stil4mElmSyntax7.Expression.IfBlock or
                Stil4mElmSyntax7.Expression.PrefixOperator or
                Stil4mElmSyntax7.Expression.ParenthesizedExpression or
                Stil4mElmSyntax7.Expression.Application or
                Stil4mElmSyntax7.Expression.OperatorApplication or
                Stil4mElmSyntax7.Expression.TupledExpression or
                Stil4mElmSyntax7.Expression.RecordExpr or
                Stil4mElmSyntax7.Expression.RecordAccess or
                Stil4mElmSyntax7.Expression.RecordAccessFunction or
                Stil4mElmSyntax7.Expression.GLSLExpression =>
                MapChildExpressions(expr, child => SubstituteInExpression(child, substitutions)),

                _ =>
                throw new NotImplementedException(
                    "SubstituteInExpression does not handle expression variant: " + expr.GetType().Name)
            };

        return new Node<Stil4mElmSyntax7.Expression>(exprNode.Range, substitutedExpr);
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
    /// <see cref="Stil4mElmSyntax7.Expression.FunctionOrValue"/> with empty <c>ModuleName</c>),
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
    internal static Stil4mElmSyntax7.Expression SubstituteInRecordUpdateExpression(
        Stil4mElmSyntax7.Expression.RecordUpdateExpression recordUpdate,
        IReadOnlyDictionary<string, Node<Stil4mElmSyntax7.Expression>> substitutions)
    {
        var substitutedFields =
            recordUpdate.Fields
            .Select(
                f =>
                new Node<(Node<string> fieldName, Node<Stil4mElmSyntax7.Expression> valueExpr)>(
                    f.Range,
                    (f.Value.fieldName, SubstituteInExpression(f.Value.valueExpr, substitutions))))
            .ToList();

        if (!substitutions.TryGetValue(recordUpdate.RecordName.Value, out var replacement))
        {
            return
                new Stil4mElmSyntax7.Expression.RecordUpdateExpression(
                    recordUpdate.RecordName,
                    [.. substitutedFields]);
        }

        if (replacement.Value is Stil4mElmSyntax7.Expression.FunctionOrValue funcOrValue &&
            funcOrValue.ModuleName.Count is 0)
        {
            // Simple-rename case: the substitution value is itself a local variable
            // reference, so we can keep using record-update syntax with the new name.
            return
                new Stil4mElmSyntax7.Expression.RecordUpdateExpression(
                    new Node<string>(recordUpdate.RecordName.Range, funcOrValue.Name),
                    [.. substitutedFields]);
        }

        // General case: the substitution value is an arbitrary expression. Bind it to a
        // fresh local via a let-destructuring and reference that fresh local in the
        // record-update head position. The fresh name must avoid colliding with any free
        // variable of the substitution value or of the substituted field value expressions
        // (otherwise the let-binding would shadow a name that the inner expressions read).
        var avoidNames = new HashSet<string>();

        foreach (var name in Stil4mElmSyntax7.SyntaxAnalysis.CollectRemainingFreeVariables(replacement.Value))
            avoidNames.Add(name);

        foreach (var field in substitutedFields)
        {
            foreach (var name in Stil4mElmSyntax7.SyntaxAnalysis.CollectRemainingFreeVariables(field.Value.valueExpr.Value))
                avoidNames.Add(name);
        }

        var freshName =
            GenerateUniqueLocalName(
                "recordUpdateRecord_" + recordUpdate.RecordName.Value,
                avoidNames);

        var letDestructuring =
            new Node<Stil4mElmSyntax7.Expression.LetDeclaration>(
                ZeroRange,
                new Stil4mElmSyntax7.Expression.LetDeclaration.LetDestructuring(
                    Pattern:
                    new Node<Stil4mElmSyntax7.Pattern>(
                        ZeroRange,
                        new Stil4mElmSyntax7.Pattern.VarPattern(freshName)),
                    Expression: replacement));

        var rebuiltRecordUpdate =
            new Stil4mElmSyntax7.Expression.RecordUpdateExpression(
                new Node<string>(recordUpdate.RecordName.Range, freshName),
                [.. substitutedFields]);

        return
            new Stil4mElmSyntax7.Expression.LetExpression(
                new Stil4mElmSyntax7.Expression.LetBlock(
                    Declarations: [letDestructuring],
                    Expression:
                    new Node<Stil4mElmSyntax7.Expression>(ZeroRange, rebuiltRecordUpdate)));
    }

    internal static Node<Stil4mElmSyntax7.Expression>? TrySubstituteSingleChoiceTagCase(
        Stil4mElmSyntax7.CaseBlock caseBlock,
        IReadOnlyDictionary<string, Node<Stil4mElmSyntax7.Expression>> substitutions)
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

    internal static Dictionary<string, Node<Stil4mElmSyntax7.Expression>>? TryBindSingleChoiceTagPattern(
        Stil4mElmSyntax7.Pattern pattern,
        Stil4mElmSyntax7.QualifiedNameRef constructorName,
        IReadOnlyList<Node<Stil4mElmSyntax7.Expression>> fieldExpressions)
    {
        switch (pattern)
        {
            case Stil4mElmSyntax7.Pattern.ParenthesizedPattern parenthesizedPattern:
                return
                    TryBindSingleChoiceTagPattern(
                        parenthesizedPattern.Pattern.Value,
                        constructorName,
                        fieldExpressions);

            case Stil4mElmSyntax7.Pattern.NamedPattern namedPattern
            when AreEquivalentConstructorNames(namedPattern.Name, constructorName) &&
                     namedPattern.Arguments.Count == fieldExpressions.Count:

                var bindings = new Dictionary<string, Node<Stil4mElmSyntax7.Expression>>();

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
        Stil4mElmSyntax7.Pattern pattern,
        Node<Stil4mElmSyntax7.Expression> fieldExpression,
        Dictionary<string, Node<Stil4mElmSyntax7.Expression>> bindings)
    {
        switch (pattern)
        {
            case Stil4mElmSyntax7.Pattern.VarPattern varPattern:
                bindings[varPattern.Name] = fieldExpression;
                return true;

            case Stil4mElmSyntax7.Pattern.AllPattern:
                return true;

            case Stil4mElmSyntax7.Pattern.ParenthesizedPattern parenthesizedPattern:
                return
                    TryBindSingleChoiceTagFieldPattern(
                        parenthesizedPattern.Pattern.Value,
                        fieldExpression,
                        bindings);

            default:
                return false;
        }
    }

    internal static Stil4mElmSyntax7.CaseBlock SubstituteInCaseBlock(
        Stil4mElmSyntax7.CaseBlock caseBlock,
        IReadOnlyDictionary<string, Node<Stil4mElmSyntax7.Expression>> substitutions)
    {
        return
            new Stil4mElmSyntax7.CaseBlock(
                Expression: SubstituteInExpression(caseBlock.Expression, substitutions),
                Cases: [.. caseBlock.Cases.Select(c => SubstituteInCase(c, substitutions))]);
    }

    internal static Stil4mElmSyntax7.Case SubstituteInCase(
        Stil4mElmSyntax7.Case caseItem,
        IReadOnlyDictionary<string, Node<Stil4mElmSyntax7.Expression>> substitutions)
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
            new Stil4mElmSyntax7.Case(
                Pattern: pattern,
                Expression: SubstituteInExpression(caseBodyExpression, filteredSubstitutions));
    }

    internal static Stil4mElmSyntax7.Expression.LetBlock SubstituteInLetBlock(
        Stil4mElmSyntax7.Expression.LetBlock letBlock,
        IReadOnlyDictionary<string, Node<Stil4mElmSyntax7.Expression>> substitutions)
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
        var letBinderPatterns = new List<Node<Stil4mElmSyntax7.Pattern>>();

        foreach (var decl in letDeclarations)
        {
            switch (decl.Value)
            {
                case Stil4mElmSyntax7.Expression.LetDeclaration.LetFunction letFunc:
                    {
                        var nameNode = letFunc.Function.Declaration.Value.Name;

                        letBinderPatterns.Add(
                            new Node<Stil4mElmSyntax7.Pattern>(
                                nameNode.Range,
                                new Stil4mElmSyntax7.Pattern.VarPattern(nameNode.Value)));

                        break;
                    }

                case Stil4mElmSyntax7.Expression.LetDeclaration.LetDestructuring letDestr:
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
                        Stil4mElmSyntax7.Expression.LetDeclaration.LetFunction letFunc =>
                        new[] { letFunc.Function.Declaration.Value.Name.Value },

                        Stil4mElmSyntax7.Expression.LetDeclaration.LetDestructuring letDestr =>
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
            if (decl.Value is Stil4mElmSyntax7.Expression.LetDeclaration.LetFunction letFunc)
            {
                letNames.Add(letFunc.Function.Declaration.Value.Name.Value);
            }
            else if (decl.Value is Stil4mElmSyntax7.Expression.LetDeclaration.LetDestructuring letDestr)
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
            new Stil4mElmSyntax7.Expression.LetBlock(
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
    private static Node<Stil4mElmSyntax7.Expression.LetDeclaration> RenameLetDeclaration(
        Node<Stil4mElmSyntax7.Expression.LetDeclaration> declNode,
        IReadOnlyDictionary<string, string> activeRenames,
        IReadOnlySet<string> namesInScope)
    {
        switch (declNode.Value)
        {
            case Stil4mElmSyntax7.Expression.LetDeclaration.LetFunction letFunc:
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
                        new Stil4mElmSyntax7.FunctionImplementation(
                            Name: new Node<string>(origImpl.Name.Range, newName),
                            Arguments: origImpl.Arguments,
                            Expression: renamedExpression);

                    var newFunc =
                        letFunc.Function with
                        {
                            Declaration =
                            new Node<Stil4mElmSyntax7.FunctionImplementation>(
                                letFunc.Function.Declaration.Range,
                                newImpl)
                        };

                    return
                        new Node<Stil4mElmSyntax7.Expression.LetDeclaration>(
                            declNode.Range,
                            new Stil4mElmSyntax7.Expression.LetDeclaration.LetFunction(newFunc));
                }

            case Stil4mElmSyntax7.Expression.LetDeclaration.LetDestructuring letDestr:
                {
                    // Rebuild the pattern with renamed binder names; leave the
                    // binding expression unchanged (it's evaluated in the OUTER
                    // scope, where it cannot see the let's own introduced names).
                    var renamedPattern = RenamePatternWithMap(letDestr.Pattern, activeRenames);

                    return
                        new Node<Stil4mElmSyntax7.Expression.LetDeclaration>(
                            declNode.Range,
                            new Stil4mElmSyntax7.Expression.LetDeclaration.LetDestructuring(
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
    private static Node<Stil4mElmSyntax7.Pattern> RenamePatternWithMap(
        Node<Stil4mElmSyntax7.Pattern> patternNode,
        IReadOnlyDictionary<string, string> renames)
    {
        Stil4mElmSyntax7.Pattern RewriteValue(Stil4mElmSyntax7.Pattern pattern)
        {
            return pattern switch
            {
                Stil4mElmSyntax7.Pattern.VarPattern v when renames.TryGetValue(v.Name, out var renamed) =>
                new Stil4mElmSyntax7.Pattern.VarPattern(renamed),

                Stil4mElmSyntax7.Pattern.TuplePattern t =>
                new Stil4mElmSyntax7.Pattern.TuplePattern(
                    [.. t.Elements.Select(e => RenamePatternWithMap(e, renames))]),

                Stil4mElmSyntax7.Pattern.UnConsPattern unCons =>
                new Stil4mElmSyntax7.Pattern.UnConsPattern(
                    RenamePatternWithMap(unCons.Head, renames),
                    RenamePatternWithMap(unCons.Tail, renames)),

                Stil4mElmSyntax7.Pattern.ListPattern l =>
                new Stil4mElmSyntax7.Pattern.ListPattern(
                    [.. l.Elements.Select(e => RenamePatternWithMap(e, renames))]),

                Stil4mElmSyntax7.Pattern.NamedPattern n =>
                new Stil4mElmSyntax7.Pattern.NamedPattern(
                    n.Name,
                    [.. n.Arguments.Select(p => RenamePatternWithMap(p, renames))]),

                Stil4mElmSyntax7.Pattern.AsPattern a =>
                new Stil4mElmSyntax7.Pattern.AsPattern(
                    RenamePatternWithMap(a.Pattern, renames),
                    renames.TryGetValue(a.Name.Value, out var renamedAlias)
                    ?
                    new Node<string>(a.Name.Range, renamedAlias)
                    :
                    a.Name),

                Stil4mElmSyntax7.Pattern.ParenthesizedPattern p =>
                new Stil4mElmSyntax7.Pattern.ParenthesizedPattern(
                    RenamePatternWithMap(p.Pattern, renames)),

                _ =>
                pattern,
            };
        }

        return new Node<Stil4mElmSyntax7.Pattern>(patternNode.Range, RewriteValue(patternNode.Value));
    }

    internal static Node<Stil4mElmSyntax7.Expression.LetDeclaration> SubstituteInLetDeclaration(
        Node<Stil4mElmSyntax7.Expression.LetDeclaration> declNode,
        IReadOnlyDictionary<string, Node<Stil4mElmSyntax7.Expression>> substitutions)
    {
        var decl = declNode.Value;

        var substitutedDecl =
            decl switch
            {
                Stil4mElmSyntax7.Expression.LetDeclaration.LetFunction letFunc =>
                new Stil4mElmSyntax7.Expression.LetDeclaration.LetFunction(
                    SubstituteInFunctionStruct(letFunc.Function, substitutions)),

                Stil4mElmSyntax7.Expression.LetDeclaration.LetDestructuring letDestr =>
                new Stil4mElmSyntax7.Expression.LetDeclaration.LetDestructuring(
                    letDestr.Pattern,
                    SubstituteInExpression(letDestr.Expression, substitutions)),

                _ =>
                decl
            };

        return new Node<Stil4mElmSyntax7.Expression.LetDeclaration>(declNode.Range, substitutedDecl);
    }

    internal static Stil4mElmSyntax7.FunctionStruct SubstituteInFunctionStruct(
        Stil4mElmSyntax7.FunctionStruct func,
        IReadOnlyDictionary<string, Node<Stil4mElmSyntax7.Expression>> substitutions)
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
            new Stil4mElmSyntax7.FunctionImplementation(
                Name: impl.Name,
                Arguments: renamedArguments,
                Expression: SubstituteInExpression(renamedExpression, filteredSubstitutions));

        return
            func with
            {
                Declaration =
                new Node<Stil4mElmSyntax7.FunctionImplementation>(
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
        IReadOnlyDictionary<string, Node<Stil4mElmSyntax7.Expression>> substitutions)
    {
        var freeInValues = new HashSet<string>();

        foreach (var (_, valueNode) in substitutions)
        {
            foreach (var name in Stil4mElmSyntax7.SyntaxAnalysis.CollectRemainingFreeVariables(valueNode.Value))
                freeInValues.Add(name);
        }

        return freeInValues;
    }

    internal static Stil4mElmSyntax7.LambdaStruct SubstituteInLambdaStruct(
        Stil4mElmSyntax7.LambdaStruct lambda,
        IReadOnlyDictionary<string, Node<Stil4mElmSyntax7.Expression>> substitutions)
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
            new Stil4mElmSyntax7.LambdaStruct(
                Arguments: renamedLambda.Arguments,
                Expression: SubstituteInExpression(renamedLambda.Expression, filteredSubstitutions));
    }

    /// <summary>
    /// Returns true when any binder name in <paramref name="bindingPatterns"/>
    /// collides with a name in <paramref name="namesToAvoid"/> and therefore
    /// requires alpha-renaming to avoid capture during substitution.
    /// </summary>
    private static bool ShouldAlphaRenameForCaptureAvoidance(
        IReadOnlyList<Node<Stil4mElmSyntax7.Pattern>> bindingPatterns,
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

    internal static Node<(Node<string>, Node<Stil4mElmSyntax7.Expression>)> SubstituteInRecordField(
        Node<(Node<string> fieldName, Node<Stil4mElmSyntax7.Expression> valueExpr)> fieldNode,
        IReadOnlyDictionary<string, Node<Stil4mElmSyntax7.Expression>> substitutions)
    {
        var (fieldName, valueExpr) = fieldNode.Value;

        return
            new Node<(Node<string>, Node<Stil4mElmSyntax7.Expression>)>(
                fieldNode.Range,
                (fieldName, SubstituteInExpression(valueExpr, substitutions)));
    }

    internal static IReadOnlyDictionary<(string moduleKey, string declName), DeclQualifiedName>
        BuildModuleKeyAndDeclNameIndex(
        IReadOnlyDictionary<DeclQualifiedName, Stil4mElmSyntax7.Declaration> declarations)
    {
        var byModuleAndName =
            new Dictionary<(string moduleKey, string declName), DeclQualifiedName>(declarations.Count);

        foreach (var key in declarations.Keys)
        {
            byModuleAndName[(string.Join(".", key.Namespaces), key.DeclName)] = key;
        }

        return byModuleAndName;
    }

    internal static HashSet<string> CollectPatternNames(Stil4mElmSyntax7.Pattern pattern)
    {
        var names = new HashSet<string>();

        Stil4mElmSyntax7.SyntaxAnalysis.CollectNamesBoundByPatternInto(pattern, names);

        return names;
    }

    /// <summary>
    /// Union of every name bound by every pattern in <paramref name="patterns"/>.
    /// Convenience for parameter-list use-cases (function arguments,
    /// lambda arguments).
    /// </summary>
    internal static ImmutableHashSet<string> CollectNamesBoundByPatterns(
        IReadOnlyList<Node<Stil4mElmSyntax7.Pattern>> patterns)
    {
        return Stil4mElmSyntax7.SyntaxAnalysis.CollectNamesBoundByPatterns(patterns);
    }

    internal static void CollectPatternNamesRecursive(Stil4mElmSyntax7.Pattern pattern, HashSet<string> names) =>
        Stil4mElmSyntax7.SyntaxAnalysis.CollectNamesBoundByPatternInto(pattern, names);

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

    internal static Stil4mElmSyntax7.FunctionImplementation RenameBindingsAvoidingCapture(
        Stil4mElmSyntax7.FunctionImplementation implementation,
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
    internal static Stil4mElmSyntax7.FunctionImplementation RenameBindingsAvoidingCapture(
        Stil4mElmSyntax7.FunctionImplementation implementation,
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

    internal static Stil4mElmSyntax7.LambdaStruct RenameBindingsAvoidingCapture(
        Stil4mElmSyntax7.LambdaStruct lambda,
        IReadOnlySet<string> namesAlreadyInScope)
    {
        return RenameBindingsAvoidingCapture(lambda, namesAlreadyInScope, crossModuleQualification: null);
    }

    internal static Stil4mElmSyntax7.LambdaStruct RenameBindingsAvoidingCapture(
        Stil4mElmSyntax7.LambdaStruct lambda,
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

        return new Stil4mElmSyntax7.LambdaStruct([.. freshArguments], freshExpression);
    }

    internal static Node<Stil4mElmSyntax7.Expression> RenameBindingsAvoidingCapture(
        Node<Stil4mElmSyntax7.Expression> expression,
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
    internal static Node<Stil4mElmSyntax7.Expression> RenameBindingsAvoidingCapture(
        Node<Stil4mElmSyntax7.Expression> expression,
        CrossModuleQualification crossModuleQualification)
    {
        return
            RenameExpressionBindings(
                expression,
                new Dictionary<string, string>(),
                new HashSet<string>(),
                crossModuleQualification);
    }

    private static Node<Stil4mElmSyntax7.Expression> RenameExpressionBindings(
        Node<Stil4mElmSyntax7.Expression> expressionNode,
        IReadOnlyDictionary<string, string> activeRenames,
        IReadOnlySet<string> namesInScope,
        CrossModuleQualification? crossModuleQualification = null)
    {
        Stil4mElmSyntax7.Expression RenameExpressionValue(Stil4mElmSyntax7.Expression expression)
        {
            switch (expression)
            {
                case Stil4mElmSyntax7.Expression.FunctionOrValue funcOrValue
                when funcOrValue.ModuleName.Count is 0 &&
                     activeRenames.TryGetValue(funcOrValue.Name, out var renamedVariable):

                    return new Stil4mElmSyntax7.Expression.FunctionOrValue([], renamedVariable);

                // Cross-module qualification: qualify unqualified references to callee module-level names.
                // Skip names that are local variables in scope (even if not renamed).
                case Stil4mElmSyntax7.Expression.FunctionOrValue funcOrValue
                when crossModuleQualification is not null &&
                     funcOrValue.ModuleName.Count is 0 &&
                     !activeRenames.ContainsKey(funcOrValue.Name) &&
                     !namesInScope.Contains(funcOrValue.Name) &&
                     crossModuleQualification.CalleeModuleLevelNames.Contains(funcOrValue.Name):

                    return
                        new Stil4mElmSyntax7.Expression.FunctionOrValue(
                            crossModuleQualification.CalleeModuleName,
                            funcOrValue.Name);

                case Stil4mElmSyntax7.Expression.LambdaExpression lambdaExpression:
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
                            new Stil4mElmSyntax7.Expression.LambdaExpression(
                                new Stil4mElmSyntax7.LambdaStruct([.. freshArguments], freshBody));
                    }

                case Stil4mElmSyntax7.Expression.CaseExpression caseExpression:
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

                                    return new Stil4mElmSyntax7.Case(freshPattern, freshBody);
                                })
                            .ToList();

                        return
                            new Stil4mElmSyntax7.Expression.CaseExpression(
                                new Stil4mElmSyntax7.CaseBlock(
                                    freshCaseExpression,
                                    [.. freshCases]));
                    }

                case Stil4mElmSyntax7.Expression.LetExpression letExpression:
                    {
                        var letScopeNames = new HashSet<string>(namesInScope);
                        var letVisibleRenames = new Dictionary<string, string>(activeRenames);

                        var renamedNames = new List<Node<string>?>(letExpression.Value.Declarations.Count);

                        var renamedPatterns =
                            new List<Node<Stil4mElmSyntax7.Pattern>?>(letExpression.Value.Declarations.Count);

                        var destructuringBindings =
                            new List<Dictionary<string, string>?>(letExpression.Value.Declarations.Count);

                        foreach (var declaration in letExpression.Value.Declarations)
                        {
                            switch (declaration.Value)
                            {
                                case Stil4mElmSyntax7.Expression.LetDeclaration.LetFunction letFunction:
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

                                case Stil4mElmSyntax7.Expression.LetDeclaration.LetDestructuring letDestructuring:
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
                            new List<Node<Stil4mElmSyntax7.Expression.LetDeclaration>>(
                                letExpression.Value.Declarations.Count);

                        for (var declarationIndex = 0;
                            declarationIndex < letExpression.Value.Declarations.Count;
                            declarationIndex++)
                        {
                            var declaration = letExpression.Value.Declarations[declarationIndex];

                            switch (declaration.Value)
                            {
                                case Stil4mElmSyntax7.Expression.LetDeclaration.LetFunction letFunction:
                                    {
                                        var functionScopeNames = new HashSet<string>(letScopeNames);
                                        var functionRenames = new Dictionary<string, string>(letVisibleRenames);
                                        var functionArguments = new List<Node<Stil4mElmSyntax7.Pattern>>();

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
                                            new Node<Stil4mElmSyntax7.Expression.LetDeclaration>(
                                                declaration.Range,
                                                new Stil4mElmSyntax7.Expression.LetDeclaration.LetFunction(
                                                    letFunction.Function with
                                                    {
                                                        Declaration =
                                                        new Node<Stil4mElmSyntax7.FunctionImplementation>(
                                                            letFunction.Function.Declaration.Range,
                                                            freshImplementation)
                                                    })));

                                        break;
                                    }

                                case Stil4mElmSyntax7.Expression.LetDeclaration.LetDestructuring letDestructuring:
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
                                            new Node<Stil4mElmSyntax7.Expression.LetDeclaration>(
                                                declaration.Range,
                                                new Stil4mElmSyntax7.Expression.LetDeclaration.LetDestructuring(
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
                            new Stil4mElmSyntax7.Expression.LetExpression(
                                new Stil4mElmSyntax7.Expression.LetBlock(
                                    [.. freshDeclarations],
                                    freshBody));
                    }

                case Stil4mElmSyntax7.Expression.RecordUpdateExpression recordUpdate:
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
                                new Node<(Node<string>, Node<Stil4mElmSyntax7.Expression>)>(
                                    field.Range,
                                    (field.Value.fieldName,
                                    RenameExpressionBindings(
                                        field.Value.valueExpr,
                                        activeRenames,
                                        namesInScope,
                                        crossModuleQualification))))
                            .ToList();

                        return
                            new Stil4mElmSyntax7.Expression.RecordUpdateExpression(
                                renamedRecordName,
                                [.. renamedFields]);
                    }

                // FunctionOrValue references that don't match the rename / cross-module
                // qualification guards above pass through unchanged. Listing the variant
                // explicitly keeps the throwing default below from firing.
                case Stil4mElmSyntax7.Expression.FunctionOrValue:
                    return expression;

                // All other expression variants delegate to MapChildExpressions for
                // structural recursion. Each variant is enumerated explicitly so that the
                // throwing default below never fires for valid expression values.
                case Stil4mElmSyntax7.Expression.UnitExpr:
                case Stil4mElmSyntax7.Expression.Literal:
                case Stil4mElmSyntax7.Expression.CharLiteral:
                case Stil4mElmSyntax7.Expression.Integer:
                case Stil4mElmSyntax7.Expression.Hex:
                case Stil4mElmSyntax7.Expression.Floatable:
                case Stil4mElmSyntax7.Expression.Negation:
                case Stil4mElmSyntax7.Expression.ListExpr:
                case Stil4mElmSyntax7.Expression.IfBlock:
                case Stil4mElmSyntax7.Expression.PrefixOperator:
                case Stil4mElmSyntax7.Expression.ParenthesizedExpression:
                case Stil4mElmSyntax7.Expression.Application:
                case Stil4mElmSyntax7.Expression.OperatorApplication:
                case Stil4mElmSyntax7.Expression.TupledExpression:
                case Stil4mElmSyntax7.Expression.RecordExpr:
                case Stil4mElmSyntax7.Expression.RecordAccess:
                case Stil4mElmSyntax7.Expression.RecordAccessFunction:
                case Stil4mElmSyntax7.Expression.GLSLExpression:
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

        return new Node<Stil4mElmSyntax7.Expression>(expressionNode.Range, RenameExpressionValue(expressionNode.Value));
    }

    private static (Node<Stil4mElmSyntax7.Pattern> Pattern, Dictionary<string, string> Bindings) RenamePatternBindings(
        Node<Stil4mElmSyntax7.Pattern> patternNode,
        ISet<string> namesInScope,
        CrossModuleQualification? crossModuleQualification = null)
    {
        var bindings = new Dictionary<string, string>();

        Stil4mElmSyntax7.Pattern RenamePatternValue(Stil4mElmSyntax7.Pattern pattern)
        {
            switch (pattern)
            {
                case Stil4mElmSyntax7.Pattern.VarPattern varPattern:
                    {
                        var chosenName =
                            namesInScope.Contains(varPattern.Name)
                            ?
                            GenerateUniqueLocalName(varPattern.Name, namesInScope)
                            :
                            varPattern.Name;

                        namesInScope.Add(chosenName);
                        bindings[varPattern.Name] = chosenName;

                        return new Stil4mElmSyntax7.Pattern.VarPattern(chosenName);
                    }

                case Stil4mElmSyntax7.Pattern.TuplePattern tuplePattern:
                    {
                        var freshElements = new List<Node<Stil4mElmSyntax7.Pattern>>(tuplePattern.Elements.Count);

                        foreach (var element in tuplePattern.Elements)
                        {
                            var (freshElement, elementBindings) =
                                RenamePatternBindings(element, namesInScope, crossModuleQualification);

                            foreach (var binding in elementBindings)
                                bindings[binding.Key] = binding.Value;

                            freshElements.Add(freshElement);
                        }

                        return new Stil4mElmSyntax7.Pattern.TuplePattern([.. freshElements]);
                    }

                case Stil4mElmSyntax7.Pattern.RecordPattern recordPattern:
                    return
                        new Stil4mElmSyntax7.Pattern.RecordPattern(
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

                case Stil4mElmSyntax7.Pattern.UnConsPattern unconsPattern:
                    {
                        var (freshHead, headBindings) =
                            RenamePatternBindings(unconsPattern.Head, namesInScope, crossModuleQualification);

                        var (freshTail, tailBindings) =
                            RenamePatternBindings(unconsPattern.Tail, namesInScope, crossModuleQualification);

                        foreach (var binding in headBindings)
                            bindings[binding.Key] = binding.Value;

                        foreach (var binding in tailBindings)
                            bindings[binding.Key] = binding.Value;

                        return new Stil4mElmSyntax7.Pattern.UnConsPattern(freshHead, freshTail);
                    }

                case Stil4mElmSyntax7.Pattern.ListPattern listPattern:
                    {
                        var freshElements = new List<Node<Stil4mElmSyntax7.Pattern>>(listPattern.Elements.Count);

                        foreach (var element in listPattern.Elements)
                        {
                            var (freshElement, elementBindings) =
                                RenamePatternBindings(element, namesInScope, crossModuleQualification);

                            foreach (var binding in elementBindings)
                                bindings[binding.Key] = binding.Value;

                            freshElements.Add(freshElement);
                        }

                        return new Stil4mElmSyntax7.Pattern.ListPattern([.. freshElements]);
                    }

                case Stil4mElmSyntax7.Pattern.NamedPattern namedPattern:
                    {
                        var freshArguments = new List<Node<Stil4mElmSyntax7.Pattern>>(namedPattern.Arguments.Count);

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
                                new Stil4mElmSyntax7.QualifiedNameRef(
                                    crossModuleQualification.CalleeModuleName,
                                    namedPattern.Name.Name);
                        }

                        return new Stil4mElmSyntax7.Pattern.NamedPattern(qualifiedName, [.. freshArguments]);
                    }

                case Stil4mElmSyntax7.Pattern.AsPattern asPattern:
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
                            new Stil4mElmSyntax7.Pattern.AsPattern(
                                freshInnerPattern,
                                asPattern.Name.Value == chosenAlias
                                ?
                                asPattern.Name
                                :
                                new Node<string>(asPattern.Name.Range, chosenAlias));
                    }

                case Stil4mElmSyntax7.Pattern.ParenthesizedPattern parenthesizedPattern:
                    {
                        var (freshPattern, childBindings) =
                            RenamePatternBindings(parenthesizedPattern.Pattern, namesInScope, crossModuleQualification);

                        foreach (var binding in childBindings)
                            bindings[binding.Key] = binding.Value;

                        return new Stil4mElmSyntax7.Pattern.ParenthesizedPattern(freshPattern);
                    }

                default:
                    return pattern;
            }
        }

        return
            (new Node<Stil4mElmSyntax7.Pattern>(patternNode.Range, RenamePatternValue(patternNode.Value)),
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
