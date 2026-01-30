using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using Pine.Core.Elm.ElmSyntax.SyntaxModel;

using SyntaxTypes = Pine.Core.Elm.ElmSyntax.Stil4mElmSyntax7;

// Alias to avoid ambiguity with System.Range
using Range = Pine.Core.Elm.ElmSyntax.SyntaxModel.Range;

namespace Pine.Core.Elm.ElmCompilerInDotnet;

/// <summary>
/// Provides lambda lifting services for Elm modules.
/// Lambda lifting transforms closures into top-level functions by making captured variables explicit parameters.
/// 
/// Design:
/// - Naming convention: containingFunction__lifted__lambdaIdentifier
/// - Zero captured bindings: no extra first parameter (lifted function has same signature as original lambda)
/// - Single captured binding: plain parameter (e.g., `f`)
/// - Multiple captured bindings: tuple parameter, ordered alphabetically (e.g., `( a, b, c )`)
/// - Lifted functions appear AFTER the originating function declaration
/// </summary>
public static class LambdaLifting
{
    /// <summary>
    /// Performs lambda lifting on the given Elm module.
    /// Transforms closures into top-level functions with explicit captured parameters.
    /// </summary>
    /// <param name="module">The Elm module to transform.</param>
    /// <returns>The transformed module with lifted lambdas as top-level functions.</returns>
    public static SyntaxTypes.File LiftLambdas(SyntaxTypes.File module)
    {
        var newDeclarations = new List<Node<SyntaxTypes.Declaration>>();

        foreach (var declaration in module.Declarations)
        {
            if (declaration.Value is SyntaxTypes.Declaration.FunctionDeclaration funcDecl)
            {
                var functionName = funcDecl.Function.Declaration.Value.Name.Value;
                var context = new LiftingContext(functionName);

                // Collect parameter names as bound variables
                var paramNames = CollectPatternNames(funcDecl.Function.Declaration.Value.Arguments);
                context = context.WithBoundVariables(paramNames);

                // Transform the function body
                var (transformedExpr, liftedFunctions) = TransformExpression(
                    funcDecl.Function.Declaration.Value.Expression,
                    context);

                // Create the transformed function declaration
                var transformedFuncImpl = funcDecl.Function.Declaration.Value with
                {
                    Expression = transformedExpr
                };

                var transformedFunc = funcDecl.Function with
                {
                    Declaration = funcDecl.Function.Declaration with { Value = transformedFuncImpl }
                };

                var transformedDecl = new SyntaxTypes.Declaration.FunctionDeclaration(transformedFunc);
                newDeclarations.Add(declaration with { Value = transformedDecl });

                // Add lifted functions AFTER the originating function
                foreach (var liftedFunc in liftedFunctions)
                {
                    newDeclarations.Add(liftedFunc);
                }
            }
            else
            {
                // Keep non-function declarations as-is
                newDeclarations.Add(declaration);
            }
        }

        return module with { Declarations = newDeclarations };
    }

    /// <summary>
    /// Context for lambda lifting, tracking the containing function name, bound variables, and lambda counter.
    /// </summary>
    private record LiftingContext(
        string ContainingFunctionName,
        ImmutableHashSet<string> BoundVariables,
        int LambdaCounter = 0)
    {
        public LiftingContext(string containingFunctionName)
            : this(containingFunctionName, [], 0)
        {
        }

        public LiftingContext WithBoundVariables(IEnumerable<string> variables) =>
            this with { BoundVariables = BoundVariables.Union(variables) };

        public LiftingContext WithBoundVariable(string variable) =>
            this with { BoundVariables = BoundVariables.Add(variable) };

        public (LiftingContext, int) NextLambdaId()
        {
            var nextId = LambdaCounter + 1;
            return (this with { LambdaCounter = nextId }, nextId);
        }
    }

    /// <summary>
    /// Transforms an expression, lifting any lambdas found within it.
    /// </summary>
    private static (Node<SyntaxTypes.Expression>, IReadOnlyList<Node<SyntaxTypes.Declaration>>) TransformExpression(
        Node<SyntaxTypes.Expression> exprNode,
        LiftingContext context)
    {
        var liftedFunctions = new List<Node<SyntaxTypes.Declaration>>();

        var (transformedExpr, newContext) = TransformExpressionInner(exprNode, context, liftedFunctions);

        return (transformedExpr, liftedFunctions);
    }

    private static (Node<SyntaxTypes.Expression>, LiftingContext) TransformExpressionInner(
        Node<SyntaxTypes.Expression> exprNode,
        LiftingContext context,
        List<Node<SyntaxTypes.Declaration>> liftedFunctions)
    {
        var expr = exprNode.Value;

        switch (expr)
        {
            case SyntaxTypes.Expression.LambdaExpression lambdaExpr:
                return LiftLambda(exprNode, lambdaExpr, context, liftedFunctions);

            case SyntaxTypes.Expression.LetExpression letExpr:
                return TransformLetExpression(exprNode, letExpr, context, liftedFunctions);

            case SyntaxTypes.Expression.Application appExpr:
                return TransformApplication(exprNode, appExpr, context, liftedFunctions);

            case SyntaxTypes.Expression.OperatorApplication opApp:
                return TransformOperatorApplication(exprNode, opApp, context, liftedFunctions);

            case SyntaxTypes.Expression.IfBlock ifBlock:
                return TransformIfBlock(exprNode, ifBlock, context, liftedFunctions);

            case SyntaxTypes.Expression.CaseExpression caseExpr:
                return TransformCaseExpression(exprNode, caseExpr, context, liftedFunctions);

            case SyntaxTypes.Expression.TupledExpression tupled:
                return TransformTupledExpression(exprNode, tupled, context, liftedFunctions);

            case SyntaxTypes.Expression.ListExpr listExpr:
                return TransformListExpression(exprNode, listExpr, context, liftedFunctions);

            case SyntaxTypes.Expression.ParenthesizedExpression parenExpr:
                return TransformParenthesizedExpression(exprNode, parenExpr, context, liftedFunctions);

            case SyntaxTypes.Expression.RecordExpr recordExpr:
                return TransformRecordExpression(exprNode, recordExpr, context, liftedFunctions);

            case SyntaxTypes.Expression.RecordAccess recordAccess:
                return TransformRecordAccess(exprNode, recordAccess, context, liftedFunctions);

            case SyntaxTypes.Expression.RecordUpdateExpression recordUpdate:
                return TransformRecordUpdateExpression(exprNode, recordUpdate, context, liftedFunctions);

            case SyntaxTypes.Expression.Negation negation:
                return TransformNegation(exprNode, negation, context, liftedFunctions);

            // Leaf expressions - no transformation needed
            case SyntaxTypes.Expression.FunctionOrValue:
            case SyntaxTypes.Expression.Integer:
            case SyntaxTypes.Expression.Literal:
            case SyntaxTypes.Expression.CharLiteral:
            case SyntaxTypes.Expression.Hex:
            case SyntaxTypes.Expression.Floatable:
            case SyntaxTypes.Expression.UnitExpr:
            case SyntaxTypes.Expression.RecordAccessFunction:
            case SyntaxTypes.Expression.PrefixOperator:
                return (exprNode, context);

            default:
                throw new NotImplementedException(
                    $"Lambda lifting not implemented for expression type: {expr.GetType().Name}");
        }
    }

    private static (Node<SyntaxTypes.Expression>, LiftingContext) LiftLambda(
        Node<SyntaxTypes.Expression> exprNode,
        SyntaxTypes.Expression.LambdaExpression lambdaExpr,
        LiftingContext context,
        List<Node<SyntaxTypes.Declaration>> liftedFunctions)
    {
        var lambda = lambdaExpr.Lambda;

        // Get lambda parameter names
        var lambdaParamNames = CollectPatternNames(lambda.Arguments);

        // Find free variables in the lambda body (variables used but not bound by lambda params)
        var freeVariables =
            FindFreeVariables(lambda.Expression, [.. lambdaParamNames])
            .Where(v => context.BoundVariables.Contains(v))
            .OrderBy(v => v)
            .ToList();

        // Generate unique name for the lifted function
        var (newContext, lambdaId) = context.NextLambdaId();
        var liftedFunctionName = $"{context.ContainingFunctionName}__lifted__lambda{lambdaId}";

        // Transform the lambda body with updated context
        var lambdaBodyContext = newContext.WithBoundVariables(lambdaParamNames);
        var (transformedBody, finalContext) = TransformExpressionInner(lambda.Expression, lambdaBodyContext, liftedFunctions);

        // Create the lifted function
        var liftedFuncDecl = CreateLiftedFunction(
            liftedFunctionName,
            freeVariables,
            lambda.Arguments,
            transformedBody,
            exprNode.Range);

        liftedFunctions.Add(liftedFuncDecl);

        // Create the replacement expression (reference to lifted function with captured args)
        var replacementExpr = CreateLiftedFunctionCall(
            liftedFunctionName,
            freeVariables,
            exprNode.Range);

        return (replacementExpr, finalContext with { LambdaCounter = finalContext.LambdaCounter });
    }

    private static (Node<SyntaxTypes.Expression>, LiftingContext) TransformLetExpression(
        Node<SyntaxTypes.Expression> exprNode,
        SyntaxTypes.Expression.LetExpression letExpr,
        LiftingContext context,
        List<Node<SyntaxTypes.Declaration>> liftedFunctions)
    {
        var letBlock = letExpr.Value;
        var newDeclarations = new List<Node<SyntaxTypes.Expression.LetDeclaration>>();
        var currentContext = context;

        // First pass: collect all names bound by let declarations
        var letBoundNames = new List<string>();
        foreach (var decl in letBlock.Declarations)
        {
            switch (decl.Value)
            {
                case SyntaxTypes.Expression.LetDeclaration.LetFunction letFunc:
                    letBoundNames.Add(letFunc.Function.Declaration.Value.Name.Value);
                    break;

                case SyntaxTypes.Expression.LetDeclaration.LetDestructuring letDestr:
                    letBoundNames.AddRange(CollectPatternNames([letDestr.Pattern]));
                    break;
            }
        }

        // Add let-bound names to context
        currentContext = currentContext.WithBoundVariables(letBoundNames);

        // Collect local functions that will be lifted (functions with parameters or lambda assignments)
        // and build a mapping from their local names to their lifted names
        var localFunctionLiftedNames = new Dictionary<string, string>();
        foreach (var decl in letBlock.Declarations)
        {
            if (decl.Value is SyntaxTypes.Expression.LetDeclaration.LetFunction letFunc)
            {
                var bindingName = letFunc.Function.Declaration.Value.Name.Value;
                var funcExpr = letFunc.Function.Declaration.Value.Expression;

                // Check if this is a lambda assignment or a local function with parameters
                var isLambdaAssignment = funcExpr.Value is SyntaxTypes.Expression.LambdaExpression &&
                    letFunc.Function.Declaration.Value.Arguments.Count is 0;
                var isLocalFunctionWithParams = letFunc.Function.Declaration.Value.Arguments.Count > 0;

                if (isLambdaAssignment || isLocalFunctionWithParams)
                {
                    var liftedFunctionName = $"{context.ContainingFunctionName}__lifted__{bindingName}";
                    localFunctionLiftedNames[bindingName] = liftedFunctionName;
                }
            }
        }

        // Second pass: transform each declaration
        foreach (var decl in letBlock.Declarations)
        {
            switch (decl.Value)
            {
                case SyntaxTypes.Expression.LetDeclaration.LetFunction letFunc:
                    {
                        var funcParamNames = CollectPatternNames(letFunc.Function.Declaration.Value.Arguments);
                        var funcContext = currentContext.WithBoundVariables(funcParamNames);
                        var funcExpr = letFunc.Function.Declaration.Value.Expression;

                        // Check if this is a lambda assignment (e.g., filterFn = \x -> ...)
                        if (funcExpr.Value is SyntaxTypes.Expression.LambdaExpression innerLambda &&
                            letFunc.Function.Declaration.Value.Arguments.Count is 0)
                        {
                            // This is a named lambda - lift it with the let-binding name
                            var (transformedLetDecl, updatedContext) = LiftNamedLambda(
                                decl,
                                letFunc,
                                innerLambda,
                                currentContext,
                                liftedFunctions,
                                localFunctionLiftedNames);

                            newDeclarations.Add(transformedLetDecl);
                            currentContext = updatedContext;
                        }
                        // Check if this is a local function with parameters (e.g., factorial x = ...)
                        else if (letFunc.Function.Declaration.Value.Arguments.Count > 0)
                        {
                            // Lift local function with parameters
                            var (transformedLetDecl, updatedContext) = LiftLocalFunction(
                                decl,
                                letFunc,
                                currentContext,
                                liftedFunctions,
                                localFunctionLiftedNames);

                            newDeclarations.Add(transformedLetDecl);
                            currentContext = updatedContext;
                        }
                        else
                        {
                            var (transformedExpr, _) = TransformExpressionInner(
                                funcExpr, funcContext, liftedFunctions);

                            var transformedFuncImpl = letFunc.Function.Declaration.Value with
                            {
                                Expression = transformedExpr
                            };

                            var transformedFunc = letFunc.Function with
                            {
                                Declaration = letFunc.Function.Declaration with { Value = transformedFuncImpl }
                            };

                            var transformedLetFunc = new SyntaxTypes.Expression.LetDeclaration.LetFunction(transformedFunc);
                            newDeclarations.Add(decl with { Value = transformedLetFunc });
                        }
                        break;
                    }

                case SyntaxTypes.Expression.LetDeclaration.LetDestructuring letDestr:
                    {
                        var (transformedExpr, _) = TransformExpressionInner(
                            letDestr.Expression, currentContext, liftedFunctions);

                        var transformedDestr = new SyntaxTypes.Expression.LetDeclaration.LetDestructuring(
                            letDestr.Pattern, transformedExpr);
                        newDeclarations.Add(decl with { Value = transformedDestr });
                        break;
                    }
            }
        }

        // Transform the let body expression
        var (transformedBody, finalContext) = TransformExpressionInner(
            letBlock.Expression, currentContext, liftedFunctions);

        var newLetBlock = new SyntaxTypes.Expression.LetBlock(newDeclarations, transformedBody);
        var newLetExpr = new SyntaxTypes.Expression.LetExpression(newLetBlock);

        return (exprNode with { Value = newLetExpr }, finalContext);
    }

    private static (Node<SyntaxTypes.Expression.LetDeclaration>, LiftingContext) LiftNamedLambda(
        Node<SyntaxTypes.Expression.LetDeclaration> declNode,
        SyntaxTypes.Expression.LetDeclaration.LetFunction letFunc,
        SyntaxTypes.Expression.LambdaExpression lambdaExpr,
        LiftingContext context,
        List<Node<SyntaxTypes.Declaration>> liftedFunctions,
        IReadOnlyDictionary<string, string> localFunctionLiftedNames)
    {
        var lambda = lambdaExpr.Lambda;
        var bindingName = letFunc.Function.Declaration.Value.Name.Value;

        // Get lambda parameter names
        var lambdaParamNames = CollectPatternNames(lambda.Arguments);

        // Find free variables in the lambda body, excluding other local functions that will be lifted
        var freeVariables =
            FindFreeVariables(lambda.Expression, [.. lambdaParamNames])
            .Where(v => context.BoundVariables.Contains(v) && v != bindingName && !localFunctionLiftedNames.ContainsKey(v))
            .OrderBy(v => v)
            .ToList();

        // Generate lifted function name using the let-binding name
        var liftedFunctionName = $"{context.ContainingFunctionName}__lifted__{bindingName}";

        // Transform the lambda body
        var lambdaBodyContext = context.WithBoundVariables(lambdaParamNames);
        var (transformedBody, _) = TransformExpressionInner(lambda.Expression, lambdaBodyContext, liftedFunctions);

        // Substitute references to other local functions with their lifted names
        var substitutionsWithSelf = new Dictionary<string, string>(localFunctionLiftedNames);
        var substitutedBody = SubstituteVariableReferences(transformedBody, substitutionsWithSelf);

        // Create the lifted function
        var liftedFuncDecl = CreateLiftedFunction(
            liftedFunctionName,
            freeVariables,
            lambda.Arguments,
            substitutedBody,
            declNode.Range);

        liftedFunctions.Add(liftedFuncDecl);

        // Create the replacement: bindingName = liftedFunctionName capturedArgs
        var replacementExpr = CreateLiftedFunctionCall(
            liftedFunctionName,
            freeVariables,
            declNode.Range);

        // Create a new let function that just assigns the partial application
        var newFuncImpl = new SyntaxTypes.FunctionImplementation(
            letFunc.Function.Declaration.Value.Name,
            [],
            replacementExpr);

        var newFunc = new SyntaxTypes.FunctionStruct(
            letFunc.Function.Documentation,
            null,
            new Node<SyntaxTypes.FunctionImplementation>(declNode.Range, newFuncImpl));

        var newLetFunc = new SyntaxTypes.Expression.LetDeclaration.LetFunction(newFunc);

        return (declNode with { Value = newLetFunc }, context);
    }

    private static (Node<SyntaxTypes.Expression.LetDeclaration>, LiftingContext) LiftLocalFunction(
        Node<SyntaxTypes.Expression.LetDeclaration> declNode,
        SyntaxTypes.Expression.LetDeclaration.LetFunction letFunc,
        LiftingContext context,
        List<Node<SyntaxTypes.Declaration>> liftedFunctions,
        IReadOnlyDictionary<string, string> localFunctionLiftedNames)
    {
        var bindingName = letFunc.Function.Declaration.Value.Name.Value;
        var funcParams = letFunc.Function.Declaration.Value.Arguments;

        // Get function parameter names
        var funcParamNames = CollectPatternNames(funcParams);

        // Find free variables in the function body, excluding other local functions that will be lifted
        var freeVariables =
            FindFreeVariables(letFunc.Function.Declaration.Value.Expression, [.. funcParamNames])
            .Where(v => context.BoundVariables.Contains(v) && v != bindingName && !localFunctionLiftedNames.ContainsKey(v))
            .OrderBy(v => v)
            .ToList();

        // Generate lifted function name using the function name
        var liftedFunctionName = $"{context.ContainingFunctionName}__lifted__{bindingName}";

        // Transform the function body (first transform any nested lambdas/local functions)
        var funcBodyContext = context.WithBoundVariables(funcParamNames);
        var (transformedBody, _) = TransformExpressionInner(letFunc.Function.Declaration.Value.Expression, funcBodyContext, liftedFunctions);

        // Substitute references to self and other local functions with their lifted names
        var substitutionsWithSelf = new Dictionary<string, string>(localFunctionLiftedNames);
        var substitutedBody = SubstituteVariableReferences(transformedBody, substitutionsWithSelf);

        // Create the lifted function
        var liftedFuncDecl = CreateLiftedFunction(
            liftedFunctionName,
            freeVariables,
            funcParams,
            substitutedBody,
            declNode.Range);

        liftedFunctions.Add(liftedFuncDecl);

        // Create the replacement: bindingName = liftedFunctionName capturedArgs
        var replacementExpr = CreateLiftedFunctionCall(
            liftedFunctionName,
            freeVariables,
            declNode.Range);

        // Create a new let function that just assigns the partial application
        var newFuncImpl = new SyntaxTypes.FunctionImplementation(
            letFunc.Function.Declaration.Value.Name,
            [],
            replacementExpr);

        var newFunc = new SyntaxTypes.FunctionStruct(
            letFunc.Function.Documentation,
            null,
            new Node<SyntaxTypes.FunctionImplementation>(declNode.Range, newFuncImpl));

        var newLetFunc = new SyntaxTypes.Expression.LetDeclaration.LetFunction(newFunc);

        return (declNode with { Value = newLetFunc }, context);
    }

    private static Node<SyntaxTypes.Expression> SubstituteVariableReferences(
        Node<SyntaxTypes.Expression> exprNode,
        IReadOnlyDictionary<string, string> substitutions)
    {
        var expr = exprNode.Value;

        switch (expr)
        {
            case SyntaxTypes.Expression.FunctionOrValue funcOrVal:
                // Only substitute local references (empty module name)
                if (funcOrVal.ModuleName.Count is 0 && substitutions.TryGetValue(funcOrVal.Name, out var newName))
                {
                    var newExpr = new SyntaxTypes.Expression.FunctionOrValue([], newName);
                    return exprNode with { Value = newExpr };
                }
                return exprNode;

            case SyntaxTypes.Expression.Application appExpr:
                var newArgs = appExpr.Arguments.Select(a => SubstituteVariableReferences(a, substitutions)).ToList();
                return exprNode with { Value = new SyntaxTypes.Expression.Application(newArgs) };

            case SyntaxTypes.Expression.OperatorApplication opApp:
                return exprNode with
                {
                    Value = new SyntaxTypes.Expression.OperatorApplication(
                        opApp.Operator,
                        opApp.Direction,
                        SubstituteVariableReferences(opApp.Left, substitutions),
                        SubstituteVariableReferences(opApp.Right, substitutions))
                };

            case SyntaxTypes.Expression.IfBlock ifBlock:
                return exprNode with
                {
                    Value = new SyntaxTypes.Expression.IfBlock(
                        SubstituteVariableReferences(ifBlock.Condition, substitutions),
                        SubstituteVariableReferences(ifBlock.ThenBlock, substitutions),
                        SubstituteVariableReferences(ifBlock.ElseBlock, substitutions))
                };

            case SyntaxTypes.Expression.CaseExpression caseExpr:
                var newScrutinee = SubstituteVariableReferences(caseExpr.CaseBlock.Expression, substitutions);
                var newCases = caseExpr.CaseBlock.Cases
                    .Select(c => new SyntaxTypes.Case(c.Pattern, SubstituteVariableReferences(c.Expression, substitutions)))
                    .ToList();
                return exprNode with
                {
                    Value = new SyntaxTypes.Expression.CaseExpression(new SyntaxTypes.CaseBlock(newScrutinee, newCases))
                };

            case SyntaxTypes.Expression.LetExpression letExpr:
                var newDecls = letExpr.Value.Declarations.Select(d => SubstituteInLetDeclaration(d, substitutions)).ToList();
                var newBody = SubstituteVariableReferences(letExpr.Value.Expression, substitutions);
                return exprNode with
                {
                    Value = new SyntaxTypes.Expression.LetExpression(new SyntaxTypes.Expression.LetBlock(newDecls, newBody))
                };

            case SyntaxTypes.Expression.LambdaExpression lambdaExpr:
                var newLambdaBody = SubstituteVariableReferences(lambdaExpr.Lambda.Expression, substitutions);
                return exprNode with
                {
                    Value = new SyntaxTypes.Expression.LambdaExpression(
                        new SyntaxTypes.LambdaStruct(lambdaExpr.Lambda.Arguments, newLambdaBody))
                };

            case SyntaxTypes.Expression.TupledExpression tupled:
                var newElements = tupled.Elements.Select(e => SubstituteVariableReferences(e, substitutions)).ToList();
                return exprNode with { Value = new SyntaxTypes.Expression.TupledExpression(newElements) };

            case SyntaxTypes.Expression.ListExpr listExpr:
                var newListElements = listExpr.Elements.Select(e => SubstituteVariableReferences(e, substitutions)).ToList();
                return exprNode with { Value = new SyntaxTypes.Expression.ListExpr(newListElements) };

            case SyntaxTypes.Expression.ParenthesizedExpression parenExpr:
                return exprNode with
                {
                    Value = new SyntaxTypes.Expression.ParenthesizedExpression(
                        SubstituteVariableReferences(parenExpr.Expression, substitutions))
                };

            case SyntaxTypes.Expression.RecordExpr recordExpr:
                var newFields = recordExpr.Fields
                    .Select(f => f with { Value = (f.Value.fieldName, SubstituteVariableReferences(f.Value.valueExpr, substitutions)) })
                    .ToList();
                return exprNode with { Value = new SyntaxTypes.Expression.RecordExpr(newFields) };

            case SyntaxTypes.Expression.RecordAccess recordAccess:
                return exprNode with
                {
                    Value = new SyntaxTypes.Expression.RecordAccess(
                        SubstituteVariableReferences(recordAccess.Record, substitutions),
                        recordAccess.FieldName)
                };

            case SyntaxTypes.Expression.RecordUpdateExpression recordUpdate:

                var newUpdateFields =
                    recordUpdate.Fields
                    .Select(f => f with { Value = (f.Value.fieldName, SubstituteVariableReferences(f.Value.valueExpr, substitutions)) })
                    .ToList();

                // Check if record name needs substitution
                var newRecordName = recordUpdate.RecordName;

                if (substitutions.TryGetValue(recordUpdate.RecordName.Value, out var newRecName))
                {
                    newRecordName = recordUpdate.RecordName with { Value = newRecName };
                }

                return exprNode with
                {
                    Value = new SyntaxTypes.Expression.RecordUpdateExpression(newRecordName, newUpdateFields)
                };

            case SyntaxTypes.Expression.Negation negation:
                return exprNode with
                {
                    Value = new SyntaxTypes.Expression.Negation(
                        SubstituteVariableReferences(negation.Expression, substitutions))
                };

            // Leaf expressions - no substitution needed
            case SyntaxTypes.Expression.Integer:
            case SyntaxTypes.Expression.Literal:
            case SyntaxTypes.Expression.CharLiteral:
            case SyntaxTypes.Expression.Hex:
            case SyntaxTypes.Expression.Floatable:
            case SyntaxTypes.Expression.UnitExpr:
            case SyntaxTypes.Expression.RecordAccessFunction:
            case SyntaxTypes.Expression.PrefixOperator:
                return exprNode;

            default:
                throw new NotImplementedException(
                    $"SubstituteVariableReferences not implemented for expression type: {expr.GetType().Name}");
        }
    }

    private static Node<SyntaxTypes.Expression.LetDeclaration> SubstituteInLetDeclaration(
        Node<SyntaxTypes.Expression.LetDeclaration> declNode,
        IReadOnlyDictionary<string, string> substitutions)
    {
        switch (declNode.Value)
        {
            case SyntaxTypes.Expression.LetDeclaration.LetFunction letFunc:
                var newFuncExpr = SubstituteVariableReferences(letFunc.Function.Declaration.Value.Expression, substitutions);
                var newFuncImpl = letFunc.Function.Declaration.Value with { Expression = newFuncExpr };
                var newFunc = letFunc.Function with { Declaration = letFunc.Function.Declaration with { Value = newFuncImpl } };
                return declNode with { Value = new SyntaxTypes.Expression.LetDeclaration.LetFunction(newFunc) };

            case SyntaxTypes.Expression.LetDeclaration.LetDestructuring letDestr:
                var newDestrExpr = SubstituteVariableReferences(letDestr.Expression, substitutions);
                return declNode with { Value = new SyntaxTypes.Expression.LetDeclaration.LetDestructuring(letDestr.Pattern, newDestrExpr) };

            default:
                return declNode;
        }
    }

    private static Node<SyntaxTypes.Declaration> CreateLiftedFunction(
        string functionName,
        IReadOnlyList<string> capturedVariables,
        IReadOnlyList<Node<SyntaxTypes.Pattern>> lambdaParams,
        Node<SyntaxTypes.Expression> body,
        Range range)
    {
        var allParams = new List<Node<SyntaxTypes.Pattern>>();

        // Add captured variables as first parameter(s)
        if (capturedVariables.Count is 1)
        {
            // Single capture: plain parameter
            var capturePattern = new SyntaxTypes.Pattern.VarPattern(capturedVariables[0]);
            allParams.Add(new Node<SyntaxTypes.Pattern>(range, capturePattern));
        }
        else if (capturedVariables.Count > 1)
        {
            // Multiple captures: tuple pattern
            var tupleElements =
                capturedVariables
                .Select(v => new Node<SyntaxTypes.Pattern>(
                    range,
                    new SyntaxTypes.Pattern.VarPattern(v)))
                .ToList();

            var tuplePattern = new SyntaxTypes.Pattern.TuplePattern(tupleElements);
            allParams.Add(new Node<SyntaxTypes.Pattern>(range, tuplePattern));
        }
        // Zero captures: no extra parameter

        // Add original lambda parameters
        allParams.AddRange(lambdaParams);

        var funcImpl = new SyntaxTypes.FunctionImplementation(
            new Node<string>(range, functionName),
            allParams,
            body);

        var funcStruct =
            new SyntaxTypes.FunctionStruct(
                null,
                null,
                new Node<SyntaxTypes.FunctionImplementation>(range, funcImpl));

        var funcDecl = new SyntaxTypes.Declaration.FunctionDeclaration(funcStruct);

        return new Node<SyntaxTypes.Declaration>(range, funcDecl);
    }

    private static Node<SyntaxTypes.Expression> CreateLiftedFunctionCall(
        string functionName,
        IReadOnlyList<string> capturedVariables,
        Range range)
    {
        // Reference to the lifted function
        var funcRef = new SyntaxTypes.Expression.FunctionOrValue([], functionName);
        var funcRefNode = new Node<SyntaxTypes.Expression>(range, funcRef);

        if (capturedVariables.Count is 0)
        {
            // No captures: just return the function reference
            return funcRefNode;
        }
        else if (capturedVariables.Count is 1)
        {
            // Single capture: function application with single argument
            var argRef = new SyntaxTypes.Expression.FunctionOrValue([], capturedVariables[0]);
            var argNode = new Node<SyntaxTypes.Expression>(range, argRef);

            var app = new SyntaxTypes.Expression.Application([funcRefNode, argNode]);
            return new Node<SyntaxTypes.Expression>(range, app);
        }
        else
        {
            // Multiple captures: function application with tuple argument
            var tupleElements = capturedVariables
                .Select(v => new Node<SyntaxTypes.Expression>(
                    range,
                    new SyntaxTypes.Expression.FunctionOrValue([], v)))
                .ToList();

            var tupleExpr = new SyntaxTypes.Expression.TupledExpression(tupleElements);
            var tupleNode = new Node<SyntaxTypes.Expression>(range, tupleExpr);

            var app = new SyntaxTypes.Expression.Application([funcRefNode, tupleNode]);
            return new Node<SyntaxTypes.Expression>(range, app);
        }
    }

    private static ImmutableHashSet<string> FindFreeVariables(
        Node<SyntaxTypes.Expression> exprNode,
        ImmutableHashSet<string> boundVariables)
    {
        var expr = exprNode.Value;

        switch (expr)
        {
            case SyntaxTypes.Expression.FunctionOrValue funcOrVal:
                // Only consider local variables (empty module name)
                if (funcOrVal.ModuleName.Count is 0 && !boundVariables.Contains(funcOrVal.Name))
                {
                    return [funcOrVal.Name];
                }
                return [];

            case SyntaxTypes.Expression.LambdaExpression lambdaExpr:
                var lambdaParams = CollectPatternNames(lambdaExpr.Lambda.Arguments);
                var newBound = boundVariables.Union(lambdaParams);
                return FindFreeVariables(lambdaExpr.Lambda.Expression, newBound);

            case SyntaxTypes.Expression.LetExpression letExpr:
                var letBound = boundVariables;
                foreach (var decl in letExpr.Value.Declarations)
                {
                    switch (decl.Value)
                    {
                        case SyntaxTypes.Expression.LetDeclaration.LetFunction letFunc:
                            letBound = letBound.Add(letFunc.Function.Declaration.Value.Name.Value);
                            break;

                        case SyntaxTypes.Expression.LetDeclaration.LetDestructuring letDestr:
                            letBound = letBound.Union(CollectPatternNames([letDestr.Pattern]));
                            break;
                    }
                }

                var letFreeVars = ImmutableHashSet<string>.Empty;
                foreach (var decl in letExpr.Value.Declarations)
                {
                    switch (decl.Value)
                    {
                        case SyntaxTypes.Expression.LetDeclaration.LetFunction letFunc:
                            var funcParams = CollectPatternNames(letFunc.Function.Declaration.Value.Arguments);
                            letFreeVars = letFreeVars.Union(FindFreeVariables(letFunc.Function.Declaration.Value.Expression, letBound.Union(funcParams)));
                            break;

                        case SyntaxTypes.Expression.LetDeclaration.LetDestructuring letDestr:
                            letFreeVars = letFreeVars.Union(FindFreeVariables(letDestr.Expression, letBound));
                            break;
                    }
                }
                return letFreeVars.Union(FindFreeVariables(letExpr.Value.Expression, letBound));

            case SyntaxTypes.Expression.Application appExpr:
                return appExpr.Arguments.Aggregate(
                    ImmutableHashSet<string>.Empty,
                    (acc, arg) => acc.Union(FindFreeVariables(arg, boundVariables)));

            case SyntaxTypes.Expression.OperatorApplication opApp:
                return FindFreeVariables(opApp.Left, boundVariables)
                    .Union(FindFreeVariables(opApp.Right, boundVariables));

            case SyntaxTypes.Expression.IfBlock ifBlock:
                return FindFreeVariables(ifBlock.Condition, boundVariables)
                    .Union(FindFreeVariables(ifBlock.ThenBlock, boundVariables))
                    .Union(FindFreeVariables(ifBlock.ElseBlock, boundVariables));

            case SyntaxTypes.Expression.CaseExpression caseExpr:

                var caseFreeVars =
                    FindFreeVariables(caseExpr.CaseBlock.Expression, boundVariables);

                foreach (var caseItem in caseExpr.CaseBlock.Cases)
                {
                    var casePatternNames = CollectPatternNames([caseItem.Pattern]);
                    caseFreeVars = caseFreeVars.Union(FindFreeVariables(caseItem.Expression, boundVariables.Union(casePatternNames)));
                }

                return caseFreeVars;

            case SyntaxTypes.Expression.TupledExpression tupled:
                return tupled.Elements.Aggregate(
                    ImmutableHashSet<string>.Empty,
                    (acc, elem) => acc.Union(FindFreeVariables(elem, boundVariables)));

            case SyntaxTypes.Expression.ListExpr listExpr:
                return listExpr.Elements.Aggregate(
                    ImmutableHashSet<string>.Empty,
                    (acc, elem) => acc.Union(FindFreeVariables(elem, boundVariables)));

            case SyntaxTypes.Expression.ParenthesizedExpression parenExpr:
                return FindFreeVariables(parenExpr.Expression, boundVariables);

            case SyntaxTypes.Expression.RecordExpr recordExpr:
                return recordExpr.Fields.Aggregate(
                    ImmutableHashSet<string>.Empty,
                    (acc, field) => acc.Union(FindFreeVariables(field.Value.valueExpr, boundVariables)));

            case SyntaxTypes.Expression.RecordAccess recordAccess:
                return FindFreeVariables(recordAccess.Record, boundVariables);

            case SyntaxTypes.Expression.RecordUpdateExpression recordUpdate:
                // The record name is a variable reference
                var recordFreeVars = !boundVariables.Contains(recordUpdate.RecordName.Value)
                    ? [recordUpdate.RecordName.Value]
                    : ImmutableHashSet<string>.Empty;
                return recordUpdate.Fields.Aggregate(
                    recordFreeVars,
                    (acc, field) => acc.Union(FindFreeVariables(field.Value.valueExpr, boundVariables)));

            case SyntaxTypes.Expression.Negation negation:
                return FindFreeVariables(negation.Expression, boundVariables);

            // Leaf expressions - no variables
            case SyntaxTypes.Expression.Integer:
            case SyntaxTypes.Expression.Literal:
            case SyntaxTypes.Expression.CharLiteral:
            case SyntaxTypes.Expression.Hex:
            case SyntaxTypes.Expression.Floatable:
            case SyntaxTypes.Expression.UnitExpr:
            case SyntaxTypes.Expression.RecordAccessFunction:
            case SyntaxTypes.Expression.PrefixOperator:
                return [];

            default:
                throw new NotImplementedException(
                    $"FindFreeVariables not implemented for expression type: {expr.GetType().Name}");
        }
    }

    private static ImmutableList<string> CollectPatternNames(IReadOnlyList<Node<SyntaxTypes.Pattern>> patterns)
    {
        return patterns.Aggregate(
            ImmutableList<string>.Empty,
            (acc, pattern) => acc.AddRange(CollectPatternNamesInner(pattern.Value)));
    }

    private static ImmutableList<string> CollectPatternNamesInner(SyntaxTypes.Pattern pattern)
    {
        switch (pattern)
        {
            case SyntaxTypes.Pattern.VarPattern varPat:
                return [varPat.Name];

            case SyntaxTypes.Pattern.TuplePattern tuplePat:
                return tuplePat.Elements.Aggregate(
                    ImmutableList<string>.Empty,
                    (acc, elem) => acc.AddRange(CollectPatternNamesInner(elem.Value)));

            case SyntaxTypes.Pattern.RecordPattern recordPat:
                return recordPat.Fields.Aggregate(
                    ImmutableList<string>.Empty,
                    (acc, field) => acc.Add(field.Value));

            case SyntaxTypes.Pattern.AsPattern asPat:
                return CollectPatternNamesInner(asPat.Pattern.Value).Add(asPat.Name.Value);

            case SyntaxTypes.Pattern.ParenthesizedPattern parenPat:
                return CollectPatternNamesInner(parenPat.Pattern.Value);

            case SyntaxTypes.Pattern.ListPattern listPat:
                return listPat.Elements.Aggregate(
                    ImmutableList<string>.Empty,
                    (acc, elem) => acc.AddRange(CollectPatternNamesInner(elem.Value)));

            case SyntaxTypes.Pattern.UnConsPattern unconsPat:
                return CollectPatternNamesInner(unconsPat.Head.Value)
                    .AddRange(CollectPatternNamesInner(unconsPat.Tail.Value));

            case SyntaxTypes.Pattern.NamedPattern namedPat:
                return namedPat.Arguments.Aggregate(
                    ImmutableList<string>.Empty,
                    (acc, arg) => acc.AddRange(CollectPatternNamesInner(arg.Value)));

            // Patterns that don't bind names
            case SyntaxTypes.Pattern.AllPattern:
            case SyntaxTypes.Pattern.UnitPattern:
            case SyntaxTypes.Pattern.CharPattern:
            case SyntaxTypes.Pattern.StringPattern:
            case SyntaxTypes.Pattern.IntPattern:
            case SyntaxTypes.Pattern.HexPattern:
            case SyntaxTypes.Pattern.FloatPattern:
                return [];

            default:
                throw new NotImplementedException(
                    $"CollectPatternNames not implemented for pattern type: {pattern.GetType().Name}");
        }
    }

    // Transform methods for other expression types

    private static (Node<SyntaxTypes.Expression>, LiftingContext) TransformApplication(
        Node<SyntaxTypes.Expression> exprNode,
        SyntaxTypes.Expression.Application appExpr,
        LiftingContext context,
        List<Node<SyntaxTypes.Declaration>> liftedFunctions)
    {
        var transformedArgs = new List<Node<SyntaxTypes.Expression>>();
        var currentContext = context;

        foreach (var arg in appExpr.Arguments)
        {
            var (transformedArg, newContext) = TransformExpressionInner(arg, currentContext, liftedFunctions);
            transformedArgs.Add(transformedArg);
            currentContext = newContext;
        }

        var newApp = new SyntaxTypes.Expression.Application(transformedArgs);
        return (exprNode with { Value = newApp }, currentContext);
    }

    private static (Node<SyntaxTypes.Expression>, LiftingContext) TransformOperatorApplication(
        Node<SyntaxTypes.Expression> exprNode,
        SyntaxTypes.Expression.OperatorApplication opApp,
        LiftingContext context,
        List<Node<SyntaxTypes.Declaration>> liftedFunctions)
    {
        var (transformedLeft, ctx1) =
            TransformExpressionInner(opApp.Left, context, liftedFunctions);

        var (transformedRight, ctx2) =
            TransformExpressionInner(opApp.Right, ctx1, liftedFunctions);

        var newOpApp = new SyntaxTypes.Expression.OperatorApplication(
            opApp.Operator, opApp.Direction, transformedLeft, transformedRight);
        return (exprNode with { Value = newOpApp }, ctx2);
    }

    private static (Node<SyntaxTypes.Expression>, LiftingContext) TransformIfBlock(
        Node<SyntaxTypes.Expression> exprNode,
        SyntaxTypes.Expression.IfBlock ifBlock,
        LiftingContext context,
        List<Node<SyntaxTypes.Declaration>> liftedFunctions)
    {
        var (transformedCond, ctx1) =
            TransformExpressionInner(ifBlock.Condition, context, liftedFunctions);

        var (transformedThen, ctx2) =
            TransformExpressionInner(ifBlock.ThenBlock, ctx1, liftedFunctions);

        var (transformedElse, ctx3) =
            TransformExpressionInner(ifBlock.ElseBlock, ctx2, liftedFunctions);

        var newIfBlock = new SyntaxTypes.Expression.IfBlock(transformedCond, transformedThen, transformedElse);

        return (exprNode with { Value = newIfBlock }, ctx3);
    }

    private static (Node<SyntaxTypes.Expression>, LiftingContext) TransformCaseExpression(
        Node<SyntaxTypes.Expression> exprNode,
        SyntaxTypes.Expression.CaseExpression caseExpr,
        LiftingContext context,
        List<Node<SyntaxTypes.Declaration>> liftedFunctions)
    {
        var (transformedScrutinee, ctx1) =
            TransformExpressionInner(caseExpr.CaseBlock.Expression, context, liftedFunctions);

        var transformedCases = new List<SyntaxTypes.Case>();
        var currentContext = ctx1;

        foreach (var caseItem in caseExpr.CaseBlock.Cases)
        {
            var patternNames = CollectPatternNames([caseItem.Pattern]);
            var caseContext = currentContext.WithBoundVariables(patternNames);

            var (transformedExpr, newContext) =
                TransformExpressionInner(caseItem.Expression, caseContext, liftedFunctions);

            transformedCases.Add(new SyntaxTypes.Case(caseItem.Pattern, transformedExpr));

            currentContext = newContext;
        }

        var newCaseBlock = new SyntaxTypes.CaseBlock(transformedScrutinee, transformedCases);
        var newCaseExpr = new SyntaxTypes.Expression.CaseExpression(newCaseBlock);
        return (exprNode with { Value = newCaseExpr }, currentContext);
    }

    private static (Node<SyntaxTypes.Expression>, LiftingContext) TransformTupledExpression(
        Node<SyntaxTypes.Expression> exprNode,
        SyntaxTypes.Expression.TupledExpression tupled,
        LiftingContext context,
        List<Node<SyntaxTypes.Declaration>> liftedFunctions)
    {
        var transformedElements = new List<Node<SyntaxTypes.Expression>>();
        var currentContext = context;

        foreach (var elem in tupled.Elements)
        {
            var (transformedElem, newContext) =
                TransformExpressionInner(elem, currentContext, liftedFunctions);

            transformedElements.Add(transformedElem);
            currentContext = newContext;
        }

        var newTupled = new SyntaxTypes.Expression.TupledExpression(transformedElements);
        return (exprNode with { Value = newTupled }, currentContext);
    }

    private static (Node<SyntaxTypes.Expression>, LiftingContext) TransformListExpression(
        Node<SyntaxTypes.Expression> exprNode,
        SyntaxTypes.Expression.ListExpr listExpr,
        LiftingContext context,
        List<Node<SyntaxTypes.Declaration>> liftedFunctions)
    {
        var transformedElements = new List<Node<SyntaxTypes.Expression>>();
        var currentContext = context;

        foreach (var elem in listExpr.Elements)
        {
            var (transformedElem, newContext) =
                TransformExpressionInner(elem, currentContext, liftedFunctions);

            transformedElements.Add(transformedElem);
            currentContext = newContext;
        }

        var newListExpr = new SyntaxTypes.Expression.ListExpr(transformedElements);
        return (exprNode with { Value = newListExpr }, currentContext);
    }

    private static (Node<SyntaxTypes.Expression>, LiftingContext) TransformParenthesizedExpression(
        Node<SyntaxTypes.Expression> exprNode,
        SyntaxTypes.Expression.ParenthesizedExpression parenExpr,
        LiftingContext context,
        List<Node<SyntaxTypes.Declaration>> liftedFunctions)
    {
        var (transformedInner, newContext) =
            TransformExpressionInner(parenExpr.Expression, context, liftedFunctions);

        var newParenExpr = new SyntaxTypes.Expression.ParenthesizedExpression(transformedInner);
        return (exprNode with { Value = newParenExpr }, newContext);
    }

    private static (Node<SyntaxTypes.Expression>, LiftingContext) TransformRecordExpression(
        Node<SyntaxTypes.Expression> exprNode,
        SyntaxTypes.Expression.RecordExpr recordExpr,
        LiftingContext context,
        List<Node<SyntaxTypes.Declaration>> liftedFunctions)
    {
        var transformedFields =
            new List<Node<(Node<string> fieldName, Node<SyntaxTypes.Expression> valueExpr)>>();

        var currentContext = context;

        foreach (var field in recordExpr.Fields)
        {
            var (transformedValue, newContext) = TransformExpressionInner(field.Value.valueExpr, currentContext, liftedFunctions);
            transformedFields.Add(field with { Value = (field.Value.fieldName, transformedValue) });
            currentContext = newContext;
        }

        var newRecordExpr = new SyntaxTypes.Expression.RecordExpr(transformedFields);
        return (exprNode with { Value = newRecordExpr }, currentContext);
    }

    private static (Node<SyntaxTypes.Expression>, LiftingContext) TransformRecordAccess(
        Node<SyntaxTypes.Expression> exprNode,
        SyntaxTypes.Expression.RecordAccess recordAccess,
        LiftingContext context,
        List<Node<SyntaxTypes.Declaration>> liftedFunctions)
    {
        var (transformedRecord, newContext) = TransformExpressionInner(recordAccess.Record, context, liftedFunctions);
        var newRecordAccess = new SyntaxTypes.Expression.RecordAccess(transformedRecord, recordAccess.FieldName);
        return (exprNode with { Value = newRecordAccess }, newContext);
    }

    private static (Node<SyntaxTypes.Expression>, LiftingContext) TransformRecordUpdateExpression(
        Node<SyntaxTypes.Expression> exprNode,
        SyntaxTypes.Expression.RecordUpdateExpression recordUpdate,
        LiftingContext context,
        List<Node<SyntaxTypes.Declaration>> liftedFunctions)
    {
        var transformedFields = new List<Node<(Node<string> fieldName, Node<SyntaxTypes.Expression> valueExpr)>>();
        var currentContext = context;

        foreach (var field in recordUpdate.Fields)
        {
            var (transformedValue, newContext) = TransformExpressionInner(field.Value.valueExpr, currentContext, liftedFunctions);
            transformedFields.Add(field with { Value = (field.Value.fieldName, transformedValue) });
            currentContext = newContext;
        }

        var newRecordUpdate = new SyntaxTypes.Expression.RecordUpdateExpression(recordUpdate.RecordName, transformedFields);
        return (exprNode with { Value = newRecordUpdate }, currentContext);
    }

    private static (Node<SyntaxTypes.Expression>, LiftingContext) TransformNegation(
        Node<SyntaxTypes.Expression> exprNode,
        SyntaxTypes.Expression.Negation negation,
        LiftingContext context,
        List<Node<SyntaxTypes.Declaration>> liftedFunctions)
    {
        var (transformedExpr, newContext) = TransformExpressionInner(negation.Expression, context, liftedFunctions);
        var newNegation = new SyntaxTypes.Expression.Negation(transformedExpr);
        return (exprNode with { Value = newNegation }, newContext);
    }
}
