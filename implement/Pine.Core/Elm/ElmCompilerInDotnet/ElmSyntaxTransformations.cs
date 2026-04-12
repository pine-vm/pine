using Pine.Core.CodeAnalysis;
using Pine.Core.Elm.ElmSyntax.SyntaxModel;
using System;
using System.Collections.Generic;
using System.Linq;

using SyntaxTypes = Pine.Core.Elm.ElmSyntax.Stil4mElmSyntax7;

// Alias to avoid ambiguity with System.Range
using Range = Pine.Core.Elm.ElmSyntax.SyntaxModel.Range;

namespace Pine.Core.Elm.ElmCompilerInDotnet;

/// <summary>
/// Pure syntax transformations that operate only on Elm syntax elements.
/// Extracted from <see cref="Inlining"/> for clarity: these methods do not
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
            if (UnwrapParenthesizedPattern(namedPattern.Arguments[index].Value) is not SyntaxTypes.Pattern.VarPattern varPattern)
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
            UnwrapParenthesized(app.Arguments[0].Value) is not SyntaxTypes.Expression.LambdaExpression lambda)
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

            switch (UnwrapParenthesizedPattern(parameter.Value))
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
        expr = UnwrapParenthesized(expr);

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
            switch (UnwrapParenthesizedPattern(parameter.Value))
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

        var lambdaBody = UnwrapParenthesized(lambda.Lambda.Expression.Value);

        if (lambdaBody is not SyntaxTypes.Expression.Application app ||
            app.Arguments.Count != expectedArguments.Count + 1 ||
            !IsLocalVariableReference(UnwrapParenthesized(app.Arguments[0].Value), variableName))
        {
            return false;
        }

        for (var index = 0; index < expectedArguments.Count; index++)
        {
            if (!expectedArguments[index].Value.Equals(UnwrapParenthesized(app.Arguments[index + 1].Value)))
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

            _ =>
            false
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
    /// Unwraps nested ParenthesizedExpression wrappers to get the inner expression.
    /// </summary>
    internal static SyntaxTypes.Expression UnwrapParenthesized(SyntaxTypes.Expression expr)
    {
        while (expr is SyntaxTypes.Expression.ParenthesizedExpression paren)
        {
            expr = paren.Expression.Value;
        }

        return expr;
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

            EnqueueChildExpressions(current, worklist);
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

            _ =>
            false,
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
            }

            EnqueueChildExpressions(expr, worklist);
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

    internal static SyntaxTypes.Pattern UnwrapParenthesizedPattern(SyntaxTypes.Pattern pattern)
    {
        return pattern switch
        {
            SyntaxTypes.Pattern.ParenthesizedPattern pp => UnwrapParenthesizedPattern(pp.Pattern.Value),

            _ =>
            pattern
        };
    }

    internal static ConstructorApplication? TryDeconstructConstructorApplication(
        Node<SyntaxTypes.Expression> exprNode)
    {
        return TryDeconstructConstructorApplication(exprNode.Value);
    }

    internal static ConstructorApplication? TryDeconstructConstructorApplication(
        SyntaxTypes.Expression expr)
    {
        switch (UnwrapParenthesized(expr))
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

            default:
                return null;
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

            if (UnwrapParenthesizedPattern(letDestr.Pattern.Value) is not SyntaxTypes.Pattern.AllPattern)
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
            UnwrapParenthesized(expr) is SyntaxTypes.Expression.FunctionOrValue funcOrValue &&
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
                return
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

            default:
                return 0;
        }
    }

    /// <summary>
    /// Enqueues all immediate child expressions of an expression node onto the given worklist.
    /// This centralizes the ~15-case expression variant traversal for iterative walkers,
    /// ensuring consistency and avoiding the need to duplicate the switch in every walker.
    /// Uses an iterative worklist to avoid stack overflow on deeply nested expressions.
    /// </summary>
    internal static void EnqueueChildExpressions(
        SyntaxTypes.Expression expr,
        Stack<SyntaxTypes.Expression> worklist)
    {
        switch (expr)
        {
            case SyntaxTypes.Expression.Application app:
                foreach (var arg in app.Arguments)
                    worklist.Push(arg.Value);

                break;

            case SyntaxTypes.Expression.ParenthesizedExpression paren:
                worklist.Push(paren.Expression.Value);
                break;

            case SyntaxTypes.Expression.IfBlock ifBlock:
                worklist.Push(ifBlock.Condition.Value);
                worklist.Push(ifBlock.ThenBlock.Value);
                worklist.Push(ifBlock.ElseBlock.Value);
                break;

            case SyntaxTypes.Expression.CaseExpression caseExpr:
                worklist.Push(caseExpr.CaseBlock.Expression.Value);

                foreach (var c in caseExpr.CaseBlock.Cases)
                    worklist.Push(c.Expression.Value);

                break;

            case SyntaxTypes.Expression.LetExpression letExpr:
                foreach (var decl in letExpr.Value.Declarations)
                {
                    switch (decl.Value)
                    {
                        case SyntaxTypes.Expression.LetDeclaration.LetFunction lf:
                            worklist.Push(lf.Function.Declaration.Value.Expression.Value);
                            break;

                        case SyntaxTypes.Expression.LetDeclaration.LetDestructuring ld:
                            worklist.Push(ld.Expression.Value);
                            break;
                    }
                }

                worklist.Push(letExpr.Value.Expression.Value);
                break;

            case SyntaxTypes.Expression.LambdaExpression lambda:
                worklist.Push(lambda.Lambda.Expression.Value);
                break;

            case SyntaxTypes.Expression.ListExpr listExpr:
                foreach (var e in listExpr.Elements)
                    worklist.Push(e.Value);

                break;

            case SyntaxTypes.Expression.TupledExpression tupled:
                foreach (var e in tupled.Elements)
                    worklist.Push(e.Value);

                break;

            case SyntaxTypes.Expression.RecordExpr recordExpr:
                foreach (var f in recordExpr.Fields)
                    worklist.Push(f.Value.valueExpr.Value);

                break;

            case SyntaxTypes.Expression.RecordUpdateExpression recordUpdate:
                foreach (var f in recordUpdate.Fields)
                    worklist.Push(f.Value.valueExpr.Value);

                break;

            case SyntaxTypes.Expression.RecordAccess recordAccess:
                worklist.Push(recordAccess.Record.Value);
                break;

            case SyntaxTypes.Expression.Negation negation:
                worklist.Push(negation.Expression.Value);
                break;

            case SyntaxTypes.Expression.OperatorApplication opApp:
                worklist.Push(opApp.Left.Value);
                worklist.Push(opApp.Right.Value);
                break;
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
                                    (SyntaxTypes.Expression.LetDeclaration)
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

            _ =>
            expr
        };
    }

    /// <summary>
    /// Counts the number of expression nodes in the AST, up to a maximum.
    /// Uses an iterative worklist to avoid stack overflow.
    /// </summary>
    internal static int CountExpressionNodes(SyntaxTypes.Expression body, int max = 3000)
    {
        var count = 0;
        var worklist = new Stack<SyntaxTypes.Expression>();
        worklist.Push(body);

        while (worklist.Count > 0 && count < max)
        {
            count++;
            EnqueueChildExpressions(worklist.Pop(), worklist);
        }

        return count;
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

                _ =>
                MapChildExpressions(expr, Recurse)
            };

        return new Node<SyntaxTypes.Expression>(exprNode.Range, rewrittenExpr);
    }

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

                _ =>
                MapChildExpressions(expr, child => SubstituteInExpression(child, substitutions))
            };

        return new Node<SyntaxTypes.Expression>(exprNode.Range, substitutedExpr);
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
        // Remove substitutions shadowed by the pattern
        var shadowedNames = CollectPatternNames(caseItem.Pattern.Value);

        var filteredSubstitutions =
            substitutions
            .Where(kvp => !shadowedNames.Contains(kvp.Key))
            .ToDictionary(kvp => kvp.Key, kvp => kvp.Value);

        return
            new SyntaxTypes.Case(
                Pattern: caseItem.Pattern,
                Expression: SubstituteInExpression(caseItem.Expression, filteredSubstitutions));
    }

    internal static SyntaxTypes.Expression.LetBlock SubstituteInLetBlock(
        SyntaxTypes.Expression.LetBlock letBlock,
        IReadOnlyDictionary<string, Node<SyntaxTypes.Expression>> substitutions)
    {
        // Collect names introduced by let declarations
        var letNames = new HashSet<string>();

        foreach (var decl in letBlock.Declarations)
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
                Declarations: [.. letBlock.Declarations.Select(d => SubstituteInLetDeclaration(d, substitutions))],
                Expression: SubstituteInExpression(letBlock.Expression, filteredSubstitutions));
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

        // Remove substitutions shadowed by function parameters
        var paramNames = new HashSet<string>();

        foreach (var param in impl.Arguments)
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
                Arguments: impl.Arguments,
                Expression: SubstituteInExpression(impl.Expression, filteredSubstitutions));

        return
            func with
            {
                Declaration =
                new Node<SyntaxTypes.FunctionImplementation>(
                    func.Declaration.Range,
                    substitutedImpl)
            };
    }

    internal static SyntaxTypes.LambdaStruct SubstituteInLambdaStruct(
        SyntaxTypes.LambdaStruct lambda,
        IReadOnlyDictionary<string, Node<SyntaxTypes.Expression>> substitutions)
    {
        // Remove substitutions shadowed by lambda parameters
        var paramNames = new HashSet<string>();

        foreach (var param in lambda.Arguments)
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
                Arguments: lambda.Arguments,
                Expression: SubstituteInExpression(lambda.Expression, filteredSubstitutions));
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

    internal static HashSet<string> CollectPatternNames(SyntaxTypes.Pattern pattern)
    {
        var names = new HashSet<string>();

        CollectPatternNamesRecursive(pattern, names);

        return names;
    }

    internal static void CollectPatternNamesRecursive(SyntaxTypes.Pattern pattern, HashSet<string> names)
    {
        switch (pattern)
        {
            case SyntaxTypes.Pattern.VarPattern varPattern:
                names.Add(varPattern.Name);
                break;

            case SyntaxTypes.Pattern.TuplePattern tuplePattern:
                foreach (var elem in tuplePattern.Elements)
                {
                    CollectPatternNamesRecursive(elem.Value, names);
                }

                break;

            case SyntaxTypes.Pattern.RecordPattern recordPattern:
                foreach (var field in recordPattern.Fields)
                {
                    names.Add(field.Value);
                }

                break;

            case SyntaxTypes.Pattern.UnConsPattern unconsPattern:
                CollectPatternNamesRecursive(unconsPattern.Head.Value, names);
                CollectPatternNamesRecursive(unconsPattern.Tail.Value, names);
                break;

            case SyntaxTypes.Pattern.ListPattern listPattern:
                foreach (var elem in listPattern.Elements)
                {
                    CollectPatternNamesRecursive(elem.Value, names);
                }

                break;

            case SyntaxTypes.Pattern.NamedPattern namedPattern:
                foreach (var arg in namedPattern.Arguments)
                {
                    CollectPatternNamesRecursive(arg.Value, names);
                }

                break;

            case SyntaxTypes.Pattern.AsPattern asPattern:
                names.Add(asPattern.Name.Value);
                CollectPatternNamesRecursive(asPattern.Pattern.Value, names);
                break;

            case SyntaxTypes.Pattern.ParenthesizedPattern parenPattern:
                CollectPatternNamesRecursive(parenPattern.Pattern.Value, names);
                break;

                // Other pattern types don't introduce names
        }
    }

}
