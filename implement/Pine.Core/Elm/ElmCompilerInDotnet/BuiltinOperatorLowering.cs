using Pine.Core.Elm.ElmSyntax.SyntaxModel;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

using ModuleName = System.Collections.Generic.IReadOnlyList<string>;
using SyntaxTypes = Pine.Core.Elm.ElmSyntax.Stil4mElmSyntax7;

namespace Pine.Core.Elm.ElmCompilerInDotnet;

/// <summary>
/// Rewrites canonicalized Elm syntax so builtin operators and selected core arithmetic helpers
/// are expressed in a form that later compilation stages can map directly to Pine builtins.
/// </summary>
public static class BuiltinOperatorLowering
{
    /// <summary>
    /// Enumerates the Elm operators recognized by the lowering stage.
    /// Each member represents a distinct lowering strategy, not a Pine builtin name.
    /// </summary>
    private enum LoweredOperator
    {
        IntAdd,
        IntSub,
        IntMul,
        Equal,
        IntLt,
        IntGt,
        IntLe,
        IntGe,
        BoolAnd,
    }

    /// <summary>
    /// Zero-based location used for generated syntax nodes introduced by lowering.
    /// </summary>
    private static readonly Location s_zeroLocation = new(Row: 0, Column: 0);

    /// <summary>
    /// Zero range used for generated syntax nodes introduced by lowering.
    /// </summary>
    private static readonly Range s_zeroRange = new(Start: s_zeroLocation, End: s_zeroLocation);

    /// <summary>
    /// Type and scope information threaded through recursive lowering of a single declaration body.
    /// </summary>
    private record RewriteContext(
        string CurrentModuleName,
        ImmutableDictionary<string, int> ParameterNames,
        ImmutableDictionary<string, TypeInference.InferredType> ParameterTypes,
        ImmutableDictionary<string, TypeInference.InferredType> LocalBindingTypes,
        IReadOnlyDictionary<QualifiedNameRef, FunctionTypeInfo> FunctionTypes,
        IReadOnlyDictionary<QualifiedNameRef, TypeInference.InferredType> AliasTypes,
        ImmutableDictionary<string, TypeInference.InferredType> FunctionSignatures);

    /// <summary>
    /// Applies builtin-operator lowering to a set of Elm modules that have already passed earlier
    /// syntax optimization stages.
    /// </summary>
    /// <param name="modules">The Elm modules to rewrite.</param>
    /// <returns>The rewritten modules keyed by module name, or an error message.</returns>
    public static Result<string, IReadOnlyDictionary<ModuleName, SyntaxTypes.File>> Apply(
        IReadOnlyList<SyntaxTypes.File> modules)
    {
        var functionTypes = BuildFunctionTypes(modules);
        var aliasTypes = BuildAliasTypes(modules);
        var functionSignatures = BuildFunctionSignatures(modules);

        var result =
            new Dictionary<ModuleName, SyntaxTypes.File>(
                EnumerableExtensions.EqualityComparer<ModuleName>());

        foreach (var module in modules)
        {
            var moduleName = SyntaxTypes.Module.GetModuleName(module.ModuleDefinition.Value).Value;
            var rewritten = RewriteModule(module, functionTypes, aliasTypes, functionSignatures);
            result[moduleName] = rewritten;
        }

        return result;
    }

    private static SyntaxTypes.File RewriteModule(
        SyntaxTypes.File module,
        IReadOnlyDictionary<QualifiedNameRef, FunctionTypeInfo> functionTypes,
        IReadOnlyDictionary<QualifiedNameRef, TypeInference.InferredType> aliasTypes,
        ImmutableDictionary<string, TypeInference.InferredType> functionSignatures)
    {
        var moduleName = SyntaxTypes.Module.GetModuleName(module.ModuleDefinition.Value).Value;
        var moduleNameString = string.Join(".", moduleName);

        var rewrittenDeclarations =
            module.Declarations
            .Select(
                declaration =>
                RewriteDeclaration(
                    declaration,
                    moduleNameString,
                    functionTypes,
                    aliasTypes,
                    functionSignatures))
            .ToList();

        return module with { Declarations = rewrittenDeclarations };
    }

    private static Node<SyntaxTypes.Declaration> RewriteDeclaration(
        Node<SyntaxTypes.Declaration> declarationNode,
        string moduleName,
        IReadOnlyDictionary<QualifiedNameRef, FunctionTypeInfo> functionTypes,
        IReadOnlyDictionary<QualifiedNameRef, TypeInference.InferredType> aliasTypes,
        ImmutableDictionary<string, TypeInference.InferredType> functionSignatures)
    {
        if (declarationNode.Value is not SyntaxTypes.Declaration.FunctionDeclaration functionDeclaration)
            return declarationNode;

        var implementation = functionDeclaration.Function.Declaration.Value;

        var inferred =
            TypeInference.InferFunctionDeclarationType(
                implementation.Expression.Value,
                implementation.Arguments,
                moduleName,
                functionSignatures);

        var explicitParameterTypes =
            BuildExplicitParameterTypes(functionDeclaration.Function);

        var context =
            new RewriteContext(
                CurrentModuleName: moduleName,
                ParameterNames: BuildParameterNames(implementation.Arguments),
                ParameterTypes:
                explicitParameterTypes.Count > 0
                ?
                explicitParameterTypes.ToImmutableDictionary(kvp => kvp.Key, kvp => kvp.Value)
                :
                inferred.parameterTypes,
                LocalBindingTypes: [],
                FunctionTypes: functionTypes,
                AliasTypes: aliasTypes,
                FunctionSignatures: functionSignatures);

        var expectedReturnType =
            TypeInference.GetFunctionReturnType(functionDeclaration.Function) is { } explicitReturnType &&
            explicitReturnType is not TypeInference.InferredType.UnknownType
            ?
            explicitReturnType
            :
            inferred.returnType;

        var rewrittenImplementation =
            implementation with
            {
                Expression = RewriteExpression(implementation.Expression, context, expectedReturnType)
            };

        return
            declarationNode with
            {
                Value =
                new SyntaxTypes.Declaration.FunctionDeclaration(
                    functionDeclaration.Function with
                    {
                        Declaration =
                        new Node<SyntaxTypes.FunctionImplementation>(
                            functionDeclaration.Function.Declaration.Range,
                            rewrittenImplementation)
                    })
            };
    }

    private static Node<SyntaxTypes.Expression> RewriteExpression(
        Node<SyntaxTypes.Expression> expressionNode,
        RewriteContext context,
        TypeInference.InferredType? expectedType = null)
    {
        var expandedExpectedType = ExpandAliasType(expectedType, context.AliasTypes);

        var rewrittenExpression =
            expressionNode.Value switch
            {
                SyntaxTypes.Expression.Application application =>
                RewriteApplication(application, context, expandedExpectedType),

                SyntaxTypes.Expression.ParenthesizedExpression parenthesized =>
                new SyntaxTypes.Expression.ParenthesizedExpression(
                    RewriteExpression(parenthesized.Expression, context, expandedExpectedType)),

                SyntaxTypes.Expression.IfBlock ifBlock =>
                new SyntaxTypes.Expression.IfBlock(
                    RewriteExpression(ifBlock.Condition, context),
                    RewriteExpression(ifBlock.ThenBlock, context, expandedExpectedType),
                    RewriteExpression(ifBlock.ElseBlock, context, expandedExpectedType)),

                SyntaxTypes.Expression.CaseExpression caseExpression =>
                new SyntaxTypes.Expression.CaseExpression(
                    new SyntaxTypes.CaseBlock(
                        RewriteExpression(caseExpression.CaseBlock.Expression, context),
                        [
                        .. caseExpression.CaseBlock.Cases.Select(
                            caseItem =>
                            new SyntaxTypes.Case(
                                caseItem.Pattern,
                                RewriteExpression(caseItem.Expression, context, expandedExpectedType)))
                        ])),

                SyntaxTypes.Expression.LetExpression letExpression =>
                RewriteLetExpression(letExpression, context, expandedExpectedType),

                SyntaxTypes.Expression.LambdaExpression lambdaExpression =>
                new SyntaxTypes.Expression.LambdaExpression(
                    RewriteLambda(lambdaExpression.Lambda, context, expandedExpectedType)),

                SyntaxTypes.Expression.ListExpr listExpression =>
                new SyntaxTypes.Expression.ListExpr(
                    [
                    ..listExpression.Elements.Select(
                        element =>
                        RewriteExpression(
                            element,
                            context,
                            expandedExpectedType is TypeInference.InferredType.ListType listType
                            ?
                            listType.ElementType
                            :
                            null))
                    ]),

                SyntaxTypes.Expression.TupledExpression tupleExpression =>
                new SyntaxTypes.Expression.TupledExpression(
                    [.. tupleExpression.Elements.Select(element => RewriteExpression(element, context))]),

                SyntaxTypes.Expression.RecordExpr recordExpression =>
                new SyntaxTypes.Expression.RecordExpr(
                    [
                    .. recordExpression.Fields.Select(
                        field =>
                        new Node<(Node<string>, Node<SyntaxTypes.Expression>)>(
                            field.Range,
                            (field.Value.fieldName,
                            RewriteExpression(
                                field.Value.valueExpr,
                                context,
                                expandedExpectedType is TypeInference.InferredType.RecordType expectedRecordType
                                ?
                                expectedRecordType.Fields
                                .FirstOrDefault(expectedField => expectedField.FieldName == field.Value.fieldName.Value)
                                .FieldType
                                :
                                null))))
                    ]),

                SyntaxTypes.Expression.RecordUpdateExpression recordUpdate =>
                new SyntaxTypes.Expression.RecordUpdateExpression(
                    recordUpdate.RecordName,
                    [
                    .. recordUpdate.Fields.Select(
                        field =>
                        new Node<(Node<string>, Node<SyntaxTypes.Expression>)>(
                            field.Range,
                            (field.Value.fieldName, RewriteExpression(field.Value.valueExpr, context))))
                    ]),

                SyntaxTypes.Expression.RecordAccess recordAccess =>
                new SyntaxTypes.Expression.RecordAccess(
                    RewriteExpression(recordAccess.Record, context),
                    recordAccess.FieldName),

                SyntaxTypes.Expression.Negation negation =>
                new SyntaxTypes.Expression.Negation(
                    RewriteExpression(negation.Expression, context)),

                SyntaxTypes.Expression.OperatorApplication operatorApplication =>
                new SyntaxTypes.Expression.OperatorApplication(
                    operatorApplication.Operator,
                    operatorApplication.Direction,
                    RewriteExpression(operatorApplication.Left, context),
                    RewriteExpression(operatorApplication.Right, context)),

                _ =>
                expressionNode.Value
            };

        return new Node<SyntaxTypes.Expression>(expressionNode.Range, rewrittenExpression);
    }

    private static SyntaxTypes.Expression RewriteApplication(
        SyntaxTypes.Expression.Application application,
        RewriteContext context,
        TypeInference.InferredType? expectedType)
    {
        var expectedArgumentTypes = GetExpectedArgumentTypes(application, context);

        var rewrittenArguments = new List<Node<SyntaxTypes.Expression>>(application.Arguments.Count);

        for (var i = 0; i < application.Arguments.Count; i++)
        {
            rewrittenArguments.Add(
                RewriteExpression(
                    application.Arguments[i],
                    context,
                    i is 0
                    ?
                    null
                    :
                    expectedArgumentTypes.ElementAtOrDefault(i - 1)));
        }

        if (rewrittenArguments.Count is 3 &&
            TryMapBuiltinOperator(rewrittenArguments[0].Value) is { } loweredOp)
        {
            var leftType =
                TypeInference.InferExpressionType(
                    rewrittenArguments[1].Value,
                    context.ParameterNames,
                    context.ParameterTypes,
                    context.LocalBindingTypes,
                    context.CurrentModuleName,
                    context.FunctionTypes);

            var rightType =
                TypeInference.InferExpressionType(
                    rewrittenArguments[2].Value,
                    context.ParameterNames,
                    context.ParameterTypes,
                    context.LocalBindingTypes,
                    context.CurrentModuleName,
                    context.FunctionTypes);

            if (loweredOp is LoweredOperator.Equal)
            {
                if (ProvesPrimitiveEqualityBuiltin(leftType, rightType))
                {
                    return
                        BuildBuiltinApplication(
                            "equal",
                            rewrittenArguments[1],
                            rewrittenArguments[2]);
                }
            }
            else if (loweredOp is LoweredOperator.IntLt or LoweredOperator.IntGt or LoweredOperator.IntLe or LoweredOperator.IntGe)
            {
                if (ProvesIntegerBuiltin(leftType, rightType))
                {
                    return
                        BuildIntComparisonApplication(
                            loweredOp,
                            rewrittenArguments[1],
                            rewrittenArguments[2]);
                }
            }
            else if (loweredOp is LoweredOperator.BoolAnd)
            {
                if (TryMergeChainedIntIsSortedAsc(
                    rewrittenArguments[1].Value,
                    rewrittenArguments[2].Value) is { } merged)
                {
                    return merged;
                }
            }
            else if (expectedType is TypeInference.InferredType.IntType ||
                ProvesIntegerBuiltin(leftType, rightType))
            {
                return loweredOp switch
                {
                    LoweredOperator.IntSub =>
                    BuildBuiltinSubtractionApplication(
                        rewrittenArguments[1],
                        rewrittenArguments[2]),

                    LoweredOperator.IntAdd =>
                    BuildBuiltinApplication(
                        "int_add",
                        rewrittenArguments[1],
                        rewrittenArguments[2]),

                    LoweredOperator.IntMul =>
                    BuildBuiltinApplication(
                        "int_mul",
                        rewrittenArguments[1],
                        rewrittenArguments[2]),

                    _ =>
                    new SyntaxTypes.Expression.Application(rewrittenArguments)
                };
            }
        }

        return new SyntaxTypes.Expression.Application(rewrittenArguments);
    }

    private static bool ProvesIntegerBuiltin(
        TypeInference.InferredType leftType,
        TypeInference.InferredType rightType) =>
        (leftType is TypeInference.InferredType.IntType &&
        (rightType is TypeInference.InferredType.IntType or TypeInference.InferredType.NumberType)) ||
        (rightType is TypeInference.InferredType.IntType &&
        (leftType is TypeInference.InferredType.IntType or TypeInference.InferredType.NumberType));

    private static bool ProvesPrimitiveEqualityBuiltin(
        TypeInference.InferredType leftType,
        TypeInference.InferredType rightType) =>
        (leftType is TypeInference.InferredType.IntType &&
        rightType is TypeInference.InferredType.IntType or TypeInference.InferredType.NumberType) ||
        (rightType is TypeInference.InferredType.IntType &&
        leftType is TypeInference.InferredType.IntType or TypeInference.InferredType.NumberType) ||
        (leftType is TypeInference.InferredType.FloatType &&
        rightType is TypeInference.InferredType.FloatType or TypeInference.InferredType.NumberType) ||
        (rightType is TypeInference.InferredType.FloatType &&
        leftType is TypeInference.InferredType.FloatType or TypeInference.InferredType.NumberType) ||
        (leftType is TypeInference.InferredType.StringType &&
        rightType is TypeInference.InferredType.StringType) ||
        (leftType is TypeInference.InferredType.CharType &&
        rightType is TypeInference.InferredType.CharType) ||
        (leftType is TypeInference.InferredType.BoolType &&
        rightType is TypeInference.InferredType.BoolType);

    private static SyntaxTypes.Expression RewriteLetExpression(
        SyntaxTypes.Expression.LetExpression letExpression,
        RewriteContext context,
        TypeInference.InferredType? expectedType)
    {
        var localBindingTypes = context.LocalBindingTypes.ToBuilder();

        foreach (var declaration in letExpression.Value.Declarations)
        {
            switch (declaration.Value)
            {
                case SyntaxTypes.Expression.LetDeclaration.LetFunction letFunction:
                    {
                        var implementation = letFunction.Function.Declaration.Value;

                        localBindingTypes[implementation.Name.Value] =
                            TypeInference.BuildFunctionTypeFromSignatureOrNull(letFunction.Function)
                            ??
                            BuildInferredFunctionType(
                                implementation.Expression.Value,
                                implementation.Arguments,
                                context.CurrentModuleName,
                                context.FunctionSignatures);

                        break;
                    }

                case SyntaxTypes.Expression.LetDeclaration.LetDestructuring letDestructuring
                when letDestructuring.Pattern.Value is SyntaxTypes.Pattern.VarPattern varPattern:

                    localBindingTypes[varPattern.Name] =
                        TypeInference.InferExpressionType(
                            letDestructuring.Expression.Value,
                            context.ParameterNames,
                            context.ParameterTypes,
                            localBindingTypes.ToImmutable(),
                            context.CurrentModuleName,
                            context.FunctionTypes);

                    break;
            }
        }

        var letContext = context with { LocalBindingTypes = localBindingTypes.ToImmutable() };

        var rewrittenDeclarations =
            letExpression.Value.Declarations
            .Select(declaration => RewriteLetDeclaration(declaration, letContext))
            .ToList();

        return
            new SyntaxTypes.Expression.LetExpression(
                new SyntaxTypes.Expression.LetBlock(
                    rewrittenDeclarations,
                    RewriteExpression(letExpression.Value.Expression, letContext, expectedType)));
    }

    private static Node<SyntaxTypes.Expression.LetDeclaration> RewriteLetDeclaration(
        Node<SyntaxTypes.Expression.LetDeclaration> declarationNode,
        RewriteContext context)
    {
        var rewritten =
            declarationNode.Value switch
            {
                SyntaxTypes.Expression.LetDeclaration.LetFunction letFunction =>
                RewriteLetFunctionDeclaration(letFunction, context),

                SyntaxTypes.Expression.LetDeclaration.LetDestructuring letDestructuring =>
                new SyntaxTypes.Expression.LetDeclaration.LetDestructuring(
                    letDestructuring.Pattern,
                    RewriteExpression(letDestructuring.Expression, context)),

                _ =>
                declarationNode.Value
            };

        return new Node<SyntaxTypes.Expression.LetDeclaration>(declarationNode.Range, rewritten);
    }

    private static SyntaxTypes.Expression.LetDeclaration RewriteLetFunctionDeclaration(
        SyntaxTypes.Expression.LetDeclaration.LetFunction letFunction,
        RewriteContext context)
    {
        var implementation = letFunction.Function.Declaration.Value;

        var inferred =
            TypeInference.InferFunctionDeclarationType(
                implementation.Expression.Value,
                implementation.Arguments,
                context.CurrentModuleName,
                context.FunctionSignatures);

        var explicitParameterTypes = BuildExplicitParameterTypes(letFunction.Function);

        var nestedContext =
            context with
            {
                ParameterNames = BuildParameterNames(implementation.Arguments),
                ParameterTypes =
                explicitParameterTypes.Count > 0
                ?
                explicitParameterTypes
                :
                inferred.parameterTypes,
            };

        var expectedReturnType =
            TypeInference.GetFunctionReturnType(letFunction.Function) is { } explicitReturnType &&
            explicitReturnType is not TypeInference.InferredType.UnknownType
            ?
            explicitReturnType
            :
            inferred.returnType;

        return
            new SyntaxTypes.Expression.LetDeclaration.LetFunction(
                letFunction.Function with
                {
                    Declaration =
                    new Node<SyntaxTypes.FunctionImplementation>(
                        letFunction.Function.Declaration.Range,
                        implementation with
                        {
                            Expression = RewriteExpression(implementation.Expression, nestedContext, expectedReturnType)
                        })
                });
    }

    private static SyntaxTypes.LambdaStruct RewriteLambda(
        SyntaxTypes.LambdaStruct lambda,
        RewriteContext context,
        TypeInference.InferredType? expectedType)
    {
        var inferred =
            TypeInference.InferFunctionDeclarationType(
                lambda.Expression.Value,
                lambda.Arguments,
                context.CurrentModuleName,
                context.FunctionSignatures);

        var nestedContext =
            context with
            {
                ParameterNames = BuildParameterNames(lambda.Arguments),
                ParameterTypes =
                MergeExpectedLambdaParameterTypes(
                    lambda.Arguments,
                    inferred.parameterTypes,
                    expectedType),
            };

        return
            lambda with
            {
                Expression =
                RewriteExpression(
                    lambda.Expression,
                    nestedContext,
                    expectedType is TypeInference.InferredType.FunctionType functionType
                    ?
                    functionType.ReturnType
                    :
                    null)
            };
    }

    private static ImmutableDictionary<string, TypeInference.InferredType> MergeExpectedLambdaParameterTypes(
        IReadOnlyList<Node<SyntaxTypes.Pattern>> arguments,
        ImmutableDictionary<string, TypeInference.InferredType> inferredParameterTypes,
        TypeInference.InferredType? expectedType)
    {
        if (expectedType is not TypeInference.InferredType.FunctionType)
        {
            return inferredParameterTypes;
        }

        var mergedParameterTypes = inferredParameterTypes.ToBuilder();
        var remainingExpectedType = expectedType;

        for (var index = 0; index < arguments.Count; index++)
        {
            if (arguments[index].Value is not SyntaxTypes.Pattern.VarPattern varPattern ||
                remainingExpectedType is not TypeInference.InferredType.FunctionType functionType)
            {
                break;
            }

            mergedParameterTypes[varPattern.Name] =
                ChooseLambdaParameterType(
                    functionType.ArgumentType,
                    mergedParameterTypes.GetValueOrDefault(varPattern.Name));

            remainingExpectedType = functionType.ReturnType;
        }

        return mergedParameterTypes.ToImmutable();
    }

    private static TypeInference.InferredType ChooseLambdaParameterType(
        TypeInference.InferredType expectedType,
        TypeInference.InferredType? inferredType)
    {
        if (inferredType is null)
        {
            return expectedType;
        }

        return expectedType switch
        {
            TypeInference.InferredType.UnknownType or TypeInference.InferredType.TypeVariable => inferredType,

            TypeInference.InferredType.NumberType when inferredType is TypeInference.InferredType.IntType or TypeInference.InferredType.FloatType =>
            inferredType,

            _ =>
            expectedType
        };
    }

    private static LoweredOperator? TryMapBuiltinOperator(
        SyntaxTypes.Expression functionExpression)
    {
        if (functionExpression is SyntaxTypes.Expression.FunctionOrValue functionOrValue &&
            functionOrValue.ModuleName.Count is 1 &&
            functionOrValue.ModuleName[0] is "Basics")
        {
            return functionOrValue.Name switch
            {
                "add" => LoweredOperator.IntAdd,
                "sub" => LoweredOperator.IntSub,
                "mul" => LoweredOperator.IntMul,
                "eq" => LoweredOperator.Equal,
                "lt" => LoweredOperator.IntLt,
                "gt" => LoweredOperator.IntGt,
                "le" => LoweredOperator.IntLe,
                "ge" => LoweredOperator.IntGe,
                "and" => LoweredOperator.BoolAnd,

                _ =>
                null
            };
        }

        if (functionExpression is SyntaxTypes.Expression.PrefixOperator prefixOperator)
        {
            return prefixOperator.Operator switch
            {
                "+" => LoweredOperator.IntAdd,
                "-" => LoweredOperator.IntSub,
                "*" => LoweredOperator.IntMul,
                "==" => LoweredOperator.Equal,
                "<" => LoweredOperator.IntLt,
                ">" => LoweredOperator.IntGt,
                "<=" => LoweredOperator.IntLe,
                ">=" => LoweredOperator.IntGe,
                "&&" => LoweredOperator.BoolAnd,

                _ =>
                null
            };
        }

        return null;
    }

    private static SyntaxTypes.Expression BuildBuiltinApplication(
        string builtinName,
        Node<SyntaxTypes.Expression> left,
        Node<SyntaxTypes.Expression> right)
    {
        var builtinReference =
            new Node<SyntaxTypes.Expression>(
                s_zeroRange,
                new SyntaxTypes.Expression.FunctionOrValue(["Pine_builtin"], builtinName));

        var operands =
            new Node<SyntaxTypes.Expression>(
                s_zeroRange,
                new SyntaxTypes.Expression.ListExpr([left, right]));

        return new SyntaxTypes.Expression.Application([builtinReference, operands]);
    }

    private static SyntaxTypes.Expression BuildBuiltinSubtractionApplication(
        Node<SyntaxTypes.Expression> left,
        Node<SyntaxTypes.Expression> right)
    {
        var negatedRight =
            new Node<SyntaxTypes.Expression>(
                s_zeroRange,
                BuildBuiltinApplication(
                    "int_mul",
                    new Node<SyntaxTypes.Expression>(
                        s_zeroRange,
                        new SyntaxTypes.Expression.Integer(-1)),
                    right));

        return BuildBuiltinApplication("int_add", left, negatedRight);
    }

    /// <summary>
    /// Builds an <c>int_is_sorted_asc</c> application for Int comparison operators.
    /// <para>
    /// All four comparison operators (<c>lt</c>, <c>gt</c>, <c>le</c>, <c>ge</c>) are expressed
    /// using <c>Pine_builtin.int_is_sorted_asc</c>:
    /// </para>
    /// <list type="bullet">
    /// <item><c>a &lt;= b</c> → <c>int_is_sorted_asc [ a, b ]</c></item>
    /// <item><c>a &gt;= b</c> → <c>int_is_sorted_asc [ b, a ]</c></item>
    /// <item><c>a &lt; b</c>  → <c>int_is_sorted_asc [ int_add [ a, 1 ], b ]</c> (with literal optimization)</item>
    /// <item><c>a &gt; b</c>  → <c>int_is_sorted_asc [ int_add [ b, 1 ], a ]</c> (with literal optimization)</item>
    /// </list>
    /// </summary>
    private static SyntaxTypes.Expression BuildIntComparisonApplication(
        LoweredOperator loweredOp,
        Node<SyntaxTypes.Expression> left,
        Node<SyntaxTypes.Expression> right)
    {
        var isStrict = loweredOp is LoweredOperator.IntLt or LoweredOperator.IntGt;
        var swapOperands = loweredOp is LoweredOperator.IntGt or LoweredOperator.IntGe;

        var (first, second) = swapOperands ? (right, left) : (left, right);

        if (isStrict)
        {
            return BuildStrictIntIsSortedAscApplication(first, second);
        }

        return BuildIntIsSortedAscApplication([first, second]);
    }

    /// <summary>
    /// Builds a strict integer comparison (<c>&lt;</c> / <c>&gt;</c>) using <c>int_is_sorted_asc</c>
    /// with an offset of +1 on the first operand.
    /// <para>
    /// Since <c>int_is_sorted_asc</c> checks <c>&lt;=</c>, we convert strict <c>&lt;</c>
    /// to <c>a + 1 &lt;= b</c>. When either operand is a literal, the offset is folded
    /// into the literal to avoid emitting <c>int_add</c>.
    /// </para>
    /// </summary>
    private static SyntaxTypes.Expression BuildStrictIntIsSortedAscApplication(
        Node<SyntaxTypes.Expression> first,
        Node<SyntaxTypes.Expression> second)
    {
        // If the first operand is a literal, fold +1 into it directly.
        if (first.Value is SyntaxTypes.Expression.Integer firstLiteral)
        {
            var adjustedFirst =
                new Node<SyntaxTypes.Expression>(
                    s_zeroRange,
                    new SyntaxTypes.Expression.Integer(firstLiteral.Value + 1));

            return BuildIntIsSortedAscApplication([adjustedFirst, second]);
        }

        // If the first operand is a negation of a literal, fold +1 into it.
        if (first.Value is SyntaxTypes.Expression.Negation { Expression: { Value: SyntaxTypes.Expression.Integer negatedLiteral } })
        {
            var adjustedFirst =
                new Node<SyntaxTypes.Expression>(
                    s_zeroRange,
                    new SyntaxTypes.Expression.Integer(-negatedLiteral.Value + 1));

            return BuildIntIsSortedAscApplication([adjustedFirst, second]);
        }

        // If the second operand is a literal, subtract 1 from it to avoid int_add on first.
        if (second.Value is SyntaxTypes.Expression.Integer secondLiteral)
        {
            var adjustedSecond =
                new Node<SyntaxTypes.Expression>(
                    s_zeroRange,
                    new SyntaxTypes.Expression.Integer(secondLiteral.Value - 1));

            return BuildIntIsSortedAscApplication([first, adjustedSecond]);
        }

        // If the second operand is a negation of a literal, fold -1 into it.
        if (second.Value is SyntaxTypes.Expression.Negation { Expression: { Value: SyntaxTypes.Expression.Integer negatedSecondLiteral } })
        {
            var adjustedSecond =
                new Node<SyntaxTypes.Expression>(
                    s_zeroRange,
                    new SyntaxTypes.Expression.Integer(-negatedSecondLiteral.Value - 1));

            return BuildIntIsSortedAscApplication([first, adjustedSecond]);
        }

        // General case: offset the first operand with int_add [first, 1].
        var offsetFirst =
            new Node<SyntaxTypes.Expression>(
                s_zeroRange,
                BuildBuiltinApplication(
                    "int_add",
                    first,
                    new Node<SyntaxTypes.Expression>(
                        s_zeroRange,
                        new SyntaxTypes.Expression.Integer(1))));

        return BuildIntIsSortedAscApplication([offsetFirst, second]);
    }

    /// <summary>
    /// Builds a <c>Pine_builtin.int_is_sorted_asc</c> application with the given operands list.
    /// </summary>
    private static SyntaxTypes.Expression BuildIntIsSortedAscApplication(
        IReadOnlyList<Node<SyntaxTypes.Expression>> operands)
    {
        var builtinReference =
            new Node<SyntaxTypes.Expression>(
                s_zeroRange,
                new SyntaxTypes.Expression.FunctionOrValue(["Pine_builtin"], "int_is_sorted_asc"));

        var operandsList =
            new Node<SyntaxTypes.Expression>(
                s_zeroRange,
                new SyntaxTypes.Expression.ListExpr([.. operands]));

        return new SyntaxTypes.Expression.Application([builtinReference, operandsList]);
    }

    /// <summary>
    /// Tries to merge two <c>int_is_sorted_asc</c> applications connected by <c>&amp;&amp;</c>
    /// into a single call when they share a common middle operand.
    /// <para>
    /// For non-strict comparisons (<c>&lt;=</c>), when the last element of the left list
    /// equals the first element of the right list, they are merged by removing the duplicate:
    /// <c>int_is_sorted_asc [ a, b ] &amp;&amp; int_is_sorted_asc [ b, c ]</c>
    /// → <c>int_is_sorted_asc [ a, b, c ]</c>.
    /// </para>
    /// <para>
    /// For strict comparisons (<c>&lt;</c>), when the first element of the right list is
    /// <c>int_add [ lastOfLeft, 1 ]</c>, both lists are concatenated (keeping all elements):
    /// <c>int_is_sorted_asc [ a+1, b ] &amp;&amp; int_is_sorted_asc [ b+1, c ]</c>
    /// → <c>int_is_sorted_asc [ a+1, b, b+1, c ]</c>.
    /// </para>
    /// </summary>
    private static SyntaxTypes.Expression? TryMergeChainedIntIsSortedAsc(
        SyntaxTypes.Expression leftExpr,
        SyntaxTypes.Expression rightExpr)
    {
        if (TryExtractIntIsSortedAscOperands(leftExpr) is not { } leftOperands ||
            TryExtractIntIsSortedAscOperands(rightExpr) is not { } rightOperands)
        {
            return null;
        }

        if (leftOperands.Count is 0 || rightOperands.Count is 0)
        {
            return null;
        }

        var leftLast = leftOperands[^1];
        var rightFirst = rightOperands[0];

        // Case 1: Exact match on shared middle operand (e.g., <= chains).
        if (SyntaxExpressionsAreEqual(leftLast.Value, rightFirst.Value))
        {
            var mergedOperands = new List<Node<SyntaxTypes.Expression>>(leftOperands.Count + rightOperands.Count - 1);
            mergedOperands.AddRange(leftOperands);

            for (var i = 1; i < rightOperands.Count; i++)
            {
                mergedOperands.Add(rightOperands[i]);
            }

            return BuildIntIsSortedAscApplication(mergedOperands);
        }

        // Case 2: Strict chain where rightFirst is int_add [leftLast, 1] (e.g., < chains).
        if (IsIntAddOffsetByOne(leftLast.Value, rightFirst.Value))
        {
            var mergedOperands = new List<Node<SyntaxTypes.Expression>>(leftOperands.Count + rightOperands.Count);
            mergedOperands.AddRange(leftOperands);
            mergedOperands.AddRange(rightOperands);

            return BuildIntIsSortedAscApplication(mergedOperands);
        }

        return null;
    }

    /// <summary>
    /// Checks whether <paramref name="candidate"/> is <c>int_add [ <paramref name="baseExpr"/>, 1 ]</c>,
    /// indicating a +1 offset relationship used in strict comparison chains.
    /// </summary>
    private static bool IsIntAddOffsetByOne(
        SyntaxTypes.Expression baseExpr,
        SyntaxTypes.Expression candidate)
    {
        if (candidate is not SyntaxTypes.Expression.Application app ||
            app.Arguments.Count is not 2 ||
            app.Arguments[0].Value is not SyntaxTypes.Expression.FunctionOrValue fv ||
            fv.ModuleName is not ["Pine_builtin"] ||
            fv.Name is not "int_add" ||
            app.Arguments[1].Value is not SyntaxTypes.Expression.ListExpr listExpr ||
            listExpr.Elements.Count is not 2)
        {
            return false;
        }

        return
            (SyntaxExpressionsAreEqual(listExpr.Elements[0].Value, baseExpr) &&
            listExpr.Elements[1].Value is SyntaxTypes.Expression.Integer { Value: 1 }) ||
            (SyntaxExpressionsAreEqual(listExpr.Elements[1].Value, baseExpr) &&
            listExpr.Elements[0].Value is SyntaxTypes.Expression.Integer { Value: 1 });
    }

    /// <summary>
    /// Extracts the operand list from an <c>int_is_sorted_asc</c> application,
    /// or returns null if the expression is not such an application.
    /// </summary>
    private static IReadOnlyList<Node<SyntaxTypes.Expression>>? TryExtractIntIsSortedAscOperands(
        SyntaxTypes.Expression expression)
    {
        if (expression is not SyntaxTypes.Expression.Application application ||
            application.Arguments.Count is not 2)
        {
            return null;
        }

        if (application.Arguments[0].Value is not SyntaxTypes.Expression.FunctionOrValue functionOrValue ||
            functionOrValue.ModuleName is not ["Pine_builtin"] ||
            functionOrValue.Name is not "int_is_sorted_asc")
        {
            return null;
        }

        if (application.Arguments[1].Value is not SyntaxTypes.Expression.ListExpr listExpr)
        {
            return null;
        }

        return listExpr.Elements;
    }

    /// <summary>
    /// Compares two syntax expressions for structural equality,
    /// used to detect shared middle operands in chained comparisons.
    /// <para>
    /// Only handles expression types that typically appear as comparison operands:
    /// variable references, integer literals, applications (like <c>int_add</c>), lists, and negations.
    /// Other expression types (if-blocks, let-blocks, records, etc.) return <c>false</c>,
    /// which safely prevents chain merging for those cases.
    /// </para>
    /// </summary>
    private static bool SyntaxExpressionsAreEqual(
        SyntaxTypes.Expression left,
        SyntaxTypes.Expression right)
    {
        return (left, right) switch
        {
            (SyntaxTypes.Expression.FunctionOrValue leftFv, SyntaxTypes.Expression.FunctionOrValue rightFv) =>
            leftFv.Name == rightFv.Name &&
            leftFv.ModuleName.Count == rightFv.ModuleName.Count &&
            leftFv.ModuleName.Zip(rightFv.ModuleName).All(pair => pair.First == pair.Second),

            (SyntaxTypes.Expression.Integer leftInt, SyntaxTypes.Expression.Integer rightInt) =>
            leftInt.Value == rightInt.Value,

            (SyntaxTypes.Expression.Application leftApp, SyntaxTypes.Expression.Application rightApp) =>
            leftApp.Arguments.Count == rightApp.Arguments.Count &&
            leftApp.Arguments.Zip(rightApp.Arguments).All(
                pair => SyntaxExpressionsAreEqual(pair.First.Value, pair.Second.Value)),

            (SyntaxTypes.Expression.ListExpr leftList, SyntaxTypes.Expression.ListExpr rightList) =>
            leftList.Elements.Count == rightList.Elements.Count &&
            leftList.Elements.Zip(rightList.Elements).All(
                pair => SyntaxExpressionsAreEqual(pair.First.Value, pair.Second.Value)),

            (SyntaxTypes.Expression.Negation leftNeg, SyntaxTypes.Expression.Negation rightNeg) =>
            SyntaxExpressionsAreEqual(leftNeg.Expression.Value, rightNeg.Expression.Value),

            _ =>
            false
        };
    }

    private static ImmutableDictionary<QualifiedNameRef, FunctionTypeInfo> BuildFunctionTypes(
        IReadOnlyList<SyntaxTypes.File> modules)
    {
        var result = new Dictionary<QualifiedNameRef, FunctionTypeInfo>();

        foreach (var module in modules)
        {
            var moduleName = SyntaxTypes.Module.GetModuleName(module.ModuleDefinition.Value).Value;

            foreach (var declaration in module.Declarations.Select(node => node.Value).OfType<SyntaxTypes.Declaration.FunctionDeclaration>())
            {
                var functionName = declaration.Function.Declaration.Value.Name.Value;

                result[QualifiedNameHelper.ToQualifiedNameRef(moduleName, functionName)] =
                    new FunctionTypeInfo(
                        TypeInference.GetFunctionReturnType(declaration),
                        TypeInference.GetFunctionParameterTypes(declaration));
            }
        }

        return result.ToImmutableDictionary();
    }

    private static ImmutableDictionary<QualifiedNameRef, TypeInference.InferredType> BuildAliasTypes(
        IReadOnlyList<SyntaxTypes.File> modules)
    {
        var result = new Dictionary<QualifiedNameRef, TypeInference.InferredType>();

        foreach (var module in modules)
        {
            var moduleName = SyntaxTypes.Module.GetModuleName(module.ModuleDefinition.Value).Value;

            foreach (var declaration in module.Declarations.Select(node => node.Value).OfType<SyntaxTypes.Declaration.AliasDeclaration>())
            {
                result[QualifiedNameHelper.ToQualifiedNameRef(moduleName, declaration.TypeAlias.Name.Value)] =
                    TypeInference.TypeAnnotationToInferredType(declaration.TypeAlias.TypeAnnotation.Value);
            }
        }

        return result.ToImmutableDictionary();
    }

    private static ImmutableDictionary<string, TypeInference.InferredType> BuildFunctionSignatures(
        IReadOnlyList<SyntaxTypes.File> modules)
    {
        var builder = ImmutableDictionary.CreateBuilder<string, TypeInference.InferredType>();

        foreach (var module in modules)
        {
            var moduleName = SyntaxTypes.Module.GetModuleName(module.ModuleDefinition.Value).Value;
            var moduleNameString = string.Join(".", moduleName);

            foreach (var signature in TypeInference.BuildFunctionSignaturesMap(module, moduleNameString))
            {
                builder[signature.Key] = signature.Value;
            }
        }

        return builder.ToImmutable();
    }

    private static ImmutableDictionary<string, int> BuildParameterNames(
        IReadOnlyList<Node<SyntaxTypes.Pattern>> arguments)
    {
        var builder = ImmutableDictionary.CreateBuilder<string, int>();

        for (var index = 0; index < arguments.Count; index++)
        {
            if (arguments[index].Value is SyntaxTypes.Pattern.VarPattern varPattern)
            {
                builder[varPattern.Name] = index;
            }
        }

        return builder.ToImmutable();
    }

    private static IReadOnlyList<TypeInference.InferredType?> GetExpectedArgumentTypes(
        SyntaxTypes.Expression.Application application,
        RewriteContext context)
    {
        if (application.Arguments.Count is 0 ||
            application.Arguments[0].Value is not SyntaxTypes.Expression.FunctionOrValue functionOrValue)
        {
            return [];
        }

        QualifiedNameRef qualifiedName =
            functionOrValue.ModuleName.Count > 0
            ?
            QualifiedNameHelper.ToQualifiedNameRef(functionOrValue.ModuleName, functionOrValue.Name)
            :
            QualifiedNameHelper.FromQualifiedNameString(context.CurrentModuleName + "." + functionOrValue.Name);

        if (!context.FunctionTypes.TryGetValue(qualifiedName, out var functionTypeInfo))
        {
            var qualifiedNameString =
                QualifiedNameHelper.ToQualifiedNameString(qualifiedName.ModuleName, qualifiedName.Name);

            if (!context.FunctionSignatures.TryGetValue(qualifiedNameString, out var functionSignatureType))
            {
                return [];
            }

            return
                [
                .. ExtractFunctionParameterTypes(functionSignatureType)
                .Select(parameterType => ExpandAliasType(parameterType, context.AliasTypes))
                .Cast<TypeInference.InferredType?>()
                ];
        }

        return
            [
            .. functionTypeInfo.ParameterTypes
            .Select(parameterType => ExpandAliasType(parameterType, context.AliasTypes))
            .Cast<TypeInference.InferredType?>()
            ];
    }

    private static IReadOnlyList<TypeInference.InferredType> ExtractFunctionParameterTypes(
        TypeInference.InferredType functionType)
    {
        var parameterTypes = new List<TypeInference.InferredType>();
        var remainingType = functionType;

        while (remainingType is TypeInference.InferredType.FunctionType nextFunctionType)
        {
            parameterTypes.Add(nextFunctionType.ArgumentType);
            remainingType = nextFunctionType.ReturnType;
        }

        return parameterTypes;
    }

    private static TypeInference.InferredType? ExpandAliasType(
        TypeInference.InferredType? inferredType,
        IReadOnlyDictionary<QualifiedNameRef, TypeInference.InferredType> aliasTypes)
    {
        return inferredType switch
        {
            null => null,

            TypeInference.InferredType.ChoiceType choiceType when aliasTypes.TryGetValue(
                QualifiedNameHelper.ToQualifiedNameRef(choiceType.ModuleName, choiceType.TypeName),
                out var aliasType) =>
            ExpandAliasType(aliasType, aliasTypes),

            TypeInference.InferredType.RecordType recordType =>
            new TypeInference.InferredType.RecordType(
                [
                ..recordType.Fields.Select(
                    field => (field.FieldName, ExpandAliasType(field.FieldType, aliasTypes) ?? field.FieldType))
                ]),

            TypeInference.InferredType.ListType listType =>
            new TypeInference.InferredType.ListType(
                ExpandAliasType(listType.ElementType, aliasTypes) ?? listType.ElementType),

            TypeInference.InferredType.FunctionType functionType =>
            new TypeInference.InferredType.FunctionType(
                ExpandAliasType(functionType.ArgumentType, aliasTypes) ?? functionType.ArgumentType,
                ExpandAliasType(functionType.ReturnType, aliasTypes) ?? functionType.ReturnType),

            _ =>
            inferredType
        };
    }

    private static TypeInference.InferredType BuildInferredFunctionType(
        SyntaxTypes.Expression expression,
        IReadOnlyList<Node<SyntaxTypes.Pattern>> arguments,
        string moduleName,
        ImmutableDictionary<string, TypeInference.InferredType> functionSignatures)
    {
        var inferred =
            TypeInference.InferFunctionDeclarationType(
                expression,
                arguments,
                moduleName,
                functionSignatures);

        return
            TypeInference.BuildFunctionType(
                arguments,
                inferred.parameterTypes,
                inferred.returnType);
    }

    private static ImmutableDictionary<string, TypeInference.InferredType> BuildExplicitParameterTypes(
        SyntaxTypes.FunctionStruct function)
    {
        var annotatedParameterTypes = TypeInference.GetFunctionParameterTypes(function);

        if (annotatedParameterTypes.Count is 0)
            return [];

        var builder = ImmutableDictionary.CreateBuilder<string, TypeInference.InferredType>();

        for (var index = 0; index < annotatedParameterTypes.Count && index < function.Declaration.Value.Arguments.Count; index++)
        {
            if (function.Declaration.Value.Arguments[index].Value is SyntaxTypes.Pattern.VarPattern varPattern)
            {
                builder[varPattern.Name] = annotatedParameterTypes[index];
            }
        }

        return builder.ToImmutable();
    }
}
