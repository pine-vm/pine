using Pine.Core.Elm.ElmSyntax.SyntaxModel;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

using ModuleName = System.Collections.Generic.IReadOnlyList<string>;
using SyntaxModelTypes = Pine.Core.Elm.ElmSyntax.SyntaxModel;
using SyntaxTypes = Pine.Core.Elm.ElmSyntax.Stil4mElmSyntax7;

namespace Pine.Core.Elm.ElmCompilerInDotnet;

/// <summary>
/// Rewrites canonicalized Elm syntax so builtin operators and selected core arithmetic helpers
/// are expressed in a form that later compilation stages can map directly to Pine builtins.
/// </summary>
public static class BuiltinOperatorLowering
{
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
        IReadOnlyDictionary<SyntaxModelTypes.QualifiedNameRef, FunctionTypeInfo> FunctionTypes,
        IReadOnlyDictionary<SyntaxModelTypes.QualifiedNameRef, TypeInference.InferredType> AliasTypes,
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
        IReadOnlyDictionary<SyntaxModelTypes.QualifiedNameRef, FunctionTypeInfo> functionTypes,
        IReadOnlyDictionary<SyntaxModelTypes.QualifiedNameRef, TypeInference.InferredType> aliasTypes,
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
        IReadOnlyDictionary<SyntaxModelTypes.QualifiedNameRef, FunctionTypeInfo> functionTypes,
        IReadOnlyDictionary<SyntaxModelTypes.QualifiedNameRef, TypeInference.InferredType> aliasTypes,
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
                    ..
                    listExpression.Elements.Select(
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
            TryMapBuiltinOperator(rewrittenArguments[0].Value, out var builtinName))
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

            if (builtinName is "equal")
            {
                if (ProvesPrimitiveEqualityBuiltin(leftType, rightType))
                {
                    return
                        BuildBuiltinApplication(
                            builtinName,
                            rewrittenArguments[1],
                            rewrittenArguments[2]);
                }
            }
            else if (expectedType is TypeInference.InferredType.IntType ||
                ProvesIntegerBuiltin(leftType, rightType))
            {
                return
                    builtinName switch
                    {
                        "int_sub" =>
                        BuildBuiltinSubtractionApplication(
                            rewrittenArguments[1],
                            rewrittenArguments[2]),

                        _ =>
                        BuildBuiltinApplication(
                            builtinName,
                            rewrittenArguments[1],
                            rewrittenArguments[2])
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
                ParameterTypes = MergeExpectedLambdaParameterTypes(
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
                ChooseLambdaParameterType(functionType.ArgumentType, mergedParameterTypes.GetValueOrDefault(varPattern.Name));

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
            TypeInference.InferredType.NumberType when inferredType is TypeInference.InferredType.IntType or TypeInference.InferredType.FloatType => inferredType,
            _ => expectedType
        };
    }

    private static bool TryMapBuiltinOperator(
        SyntaxTypes.Expression functionExpression,
        out string builtinName)
    {
        if (functionExpression is SyntaxTypes.Expression.FunctionOrValue functionOrValue &&
            functionOrValue.ModuleName.Count is 1 &&
            functionOrValue.ModuleName[0] is "Basics")
        {
            if (functionOrValue.Name is "add")
            {
                builtinName = "int_add";
                return true;
            }

            if (functionOrValue.Name is "sub")
            {
                builtinName = "int_sub";
                return true;
            }

            if (functionOrValue.Name is "mul")
            {
                builtinName = "int_mul";
                return true;
            }

            if (functionOrValue.Name is "eq")
            {
                builtinName = "equal";
                return true;
            }
        }

        if (functionExpression is SyntaxTypes.Expression.PrefixOperator prefixOperator)
        {
            if (prefixOperator.Operator is "+")
            {
                builtinName = "int_add";
                return true;
            }

            if (prefixOperator.Operator is "-")
            {
                builtinName = "int_sub";
                return true;
            }

            if (prefixOperator.Operator is "*")
            {
                builtinName = "int_mul";
                return true;
            }

            if (prefixOperator.Operator is "==")
            {
                builtinName = "equal";
                return true;
            }
        }

        builtinName = "";
        return false;
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

    private static ImmutableDictionary<SyntaxModelTypes.QualifiedNameRef, FunctionTypeInfo> BuildFunctionTypes(
        IReadOnlyList<SyntaxTypes.File> modules)
    {
        var result = new Dictionary<SyntaxModelTypes.QualifiedNameRef, FunctionTypeInfo>();

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

    private static ImmutableDictionary<SyntaxModelTypes.QualifiedNameRef, TypeInference.InferredType> BuildAliasTypes(
        IReadOnlyList<SyntaxTypes.File> modules)
    {
        var result = new Dictionary<SyntaxModelTypes.QualifiedNameRef, TypeInference.InferredType>();

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

        SyntaxModelTypes.QualifiedNameRef qualifiedName =
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
                ExtractFunctionParameterTypes(functionSignatureType)
                .Select(parameterType => ExpandAliasType(parameterType, context.AliasTypes))
                .Cast<TypeInference.InferredType?>()
                .ToList();
        }

        return
            functionTypeInfo.ParameterTypes
            .Select(parameterType => ExpandAliasType(parameterType, context.AliasTypes))
            .Cast<TypeInference.InferredType?>()
            .ToList();
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
        IReadOnlyDictionary<SyntaxModelTypes.QualifiedNameRef, TypeInference.InferredType> aliasTypes)
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
                    [..
                    recordType.Fields.Select(
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
