using Pine.Core.CodeAnalysis;
using Pine.Core.CommonEncodings;
using Pine.Core.Elm.ElmSyntax.SyntaxModel;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Numerics;

using SyntaxTypes = Pine.Core.Elm.ElmSyntax.ElmSyntaxAbstract;

namespace Pine.Core.Elm.ElmCompilerInDotnet;

/// <summary>
/// Rewrites canonicalized Elm syntax so builtin operators and selected core arithmetic helpers
/// are expressed in a form that later compilation stages can map directly to Pine builtins.
/// <para>
/// This stage operates exclusively on the abstract Elm syntax model
/// (<see cref="Pine.Core.Elm.ElmSyntax.ElmSyntaxAbstract"/>): there is no source-location
/// tracking, no concrete-syntax <c>Node&lt;T&gt;</c> wrapper, and no bridging/conversion to or
/// from the concrete <c>Stil4mElmSyntax7</c> model. Callers are expected to supply and receive
/// declarations already expressed in the abstract model.
/// </para>
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
        NotEqual,
        IntLt,
        IntGt,
        IntLe,
        IntGe,
        BoolAnd,
        BoolOr,
    }

    private record RewriteContext(
        string CurrentModuleName,
        ImmutableDictionary<string, int> ParameterNames,
        ImmutableDictionary<string, TypeInference.InferredType> ParameterTypes,
        ImmutableDictionary<string, TypeInference.InferredType> LocalBindingTypes,
        IReadOnlyDictionary<QualifiedNameRef, FunctionTypeInfo> FunctionTypes,
        IReadOnlyDictionary<QualifiedNameRef, TypeInference.InferredType> AliasTypes,
        IReadOnlyDictionary<QualifiedNameRef, TypeInference.ChoiceTypeDefinition> ChoiceTypeDefinitions,
        ImmutableDictionary<string, TypeInference.InferredType> FunctionSignatures);

    /// <summary>
    /// Applies builtin-operator lowering to a flat dictionary of Elm declarations
    /// (in the abstract syntax model) that have already passed earlier syntax optimization stages.
    /// </summary>
    /// <param name="declarations">The flat declaration dictionary to rewrite.</param>
    /// <returns>The rewritten declarations, or an error message.</returns>
    public static Result<string, ImmutableDictionary<DeclQualifiedName, SyntaxTypes.Declaration>> Apply(
        ImmutableDictionary<DeclQualifiedName, SyntaxTypes.Declaration> declarations)
    {
        var functionTypes = BuildFunctionTypes(declarations);
        var aliasTypes = BuildAliasTypes(declarations);

        var choiceTypeDefinitions =
            TypeInference.BuildChoiceTypeDefinitions(declarations);

        var functionSignatures = BuildFunctionSignatures(declarations);

        var resultBuilder =
            ImmutableDictionary.CreateBuilder<DeclQualifiedName, SyntaxTypes.Declaration>();

        foreach (var (key, decl) in declarations)
        {
            var moduleNameString = string.Join(".", key.Namespaces);

            var rewritten =
                RewriteDeclaration(
                    decl,
                    moduleNameString,
                    functionTypes,
                    aliasTypes,
                    choiceTypeDefinitions,
                    functionSignatures);

            resultBuilder[key] = rewritten;
        }

        return resultBuilder.ToImmutable();
    }

    private static SyntaxTypes.Declaration RewriteDeclaration(
        SyntaxTypes.Declaration declaration,
        string moduleName,
        IReadOnlyDictionary<QualifiedNameRef, FunctionTypeInfo> functionTypes,
        IReadOnlyDictionary<QualifiedNameRef, TypeInference.InferredType> aliasTypes,
        IReadOnlyDictionary<QualifiedNameRef, TypeInference.ChoiceTypeDefinition> choiceTypeDefinitions,
        ImmutableDictionary<string, TypeInference.InferredType> functionSignatures)
    {
        if (declaration is not SyntaxTypes.Declaration.FunctionDeclaration functionDeclaration)
            return declaration;

        var implementation = functionDeclaration.Function.Declaration;

        var inferred =
            TypeInference.InferFunctionDeclarationType(
                implementation.Expression,
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
                ChoiceTypeDefinitions: choiceTypeDefinitions,
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
            new SyntaxTypes.Declaration.FunctionDeclaration(
                functionDeclaration.Function with
                {
                    Declaration = rewrittenImplementation
                });
    }

    private static SyntaxTypes.Expression RewriteExpression(
        SyntaxTypes.Expression expression,
        RewriteContext context,
        TypeInference.InferredType? expectedType = null)
    {
        var expandedExpectedType = ExpandAliasType(expectedType, context.AliasTypes);

        return
            expression switch
            {
                SyntaxTypes.Expression.Application application =>
                RewriteApplication(application, context, expandedExpectedType),

                SyntaxTypes.Expression.IfBlock ifBlock =>
                new SyntaxTypes.Expression.IfBlock(
                    RewriteExpression(ifBlock.Condition, context),
                    RewriteExpression(ifBlock.ThenBlock, context, expandedExpectedType),
                    RewriteExpression(ifBlock.ElseBlock, context, expandedExpectedType)),

                SyntaxTypes.Expression.CaseExpression caseExpression =>
                new SyntaxTypes.Expression.CaseExpression(
                    RewriteExpression(caseExpression.Expression, context),
                    [
                    .. caseExpression.Cases.Select(
                        caseItem =>
                        new SyntaxTypes.Case(
                            caseItem.Pattern,
                            RewriteExpression(caseItem.Expression, context, expandedExpectedType)))
                    ]),

                SyntaxTypes.Expression.LetExpression letExpression =>
                RewriteLetExpression(letExpression, context, expandedExpectedType),

                SyntaxTypes.Expression.LambdaExpression lambdaExpression =>
                RewriteLambda(lambdaExpression, context, expandedExpectedType),

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
                        new SyntaxTypes.RecordSetter(
                            field.FieldName,
                            field.FieldNameValue,
                            RewriteExpression(
                                field.Value,
                                context,
                                expandedExpectedType is TypeInference.InferredType.RecordType expectedRecordType
                                ?
                                expectedRecordType.Fields
                                .FirstOrDefault(expectedField => expectedField.FieldName == field.FieldName)
                                .FieldType
                                :
                                null)))
                    ]),

                SyntaxTypes.Expression.RecordUpdateExpression recordUpdate =>
                new SyntaxTypes.Expression.RecordUpdateExpression(
                    recordUpdate.RecordName,
                    [
                    .. recordUpdate.Fields.Select(
                        field =>
                        new SyntaxTypes.RecordSetter(
                            field.FieldName,
                            field.FieldNameValue,
                            RewriteExpression(field.Value, context)))
                    ]),

                SyntaxTypes.Expression.RecordAccess recordAccess =>
                new SyntaxTypes.Expression.RecordAccess(
                    RewriteExpression(recordAccess.Record, context),
                    recordAccess.FieldName,
                    recordAccess.FieldNameValue),

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
                expression
            };
    }

    private static SyntaxTypes.Expression RewriteApplication(
        SyntaxTypes.Expression.Application application,
        RewriteContext context,
        TypeInference.InferredType? expectedType)
    {
        var rewrittenFunction = RewriteExpression(application.Function, context);

        var expectedArgumentTypes = GetExpectedArgumentTypes(application, context);

        var rewrittenArguments = new List<SyntaxTypes.Expression>(application.Arguments.Count);

        for (var i = 0; i < application.Arguments.Count; i++)
        {
            rewrittenArguments.Add(
                RewriteExpression(
                    application.Arguments[i],
                    context,
                    expectedArgumentTypes.ElementAtOrDefault(i)));
        }

        if (rewrittenArguments.Count is 2 &&
            TryMapBuiltinOperator(rewrittenFunction) is { } loweredOp)
        {
            var left = rewrittenArguments[0];
            var right = rewrittenArguments[1];

            var leftType =
                TypeInference.InferExpressionType(
                    left,
                    context.ParameterNames,
                    context.ParameterTypes,
                    context.LocalBindingTypes,
                    context.CurrentModuleName,
                    context.FunctionTypes);

            var rightType =
                TypeInference.InferExpressionType(
                    right,
                    context.ParameterNames,
                    context.ParameterTypes,
                    context.LocalBindingTypes,
                    context.CurrentModuleName,
                    context.FunctionTypes);

            if (loweredOp is LoweredOperator.Equal)
            {
                if (ProvesPrimitiveEqualityBuiltin(leftType, rightType, context))
                {
                    return
                        BuildBuiltinApplication(
                            "equal",
                            left,
                            right);
                }
            }
            else if (loweredOp is LoweredOperator.NotEqual)
            {
                if (ProvesPrimitiveEqualityBuiltin(leftType, rightType, context))
                {
                    // Lower `a /= b` (and the equivalent `Basics.neq a b`) to
                    //   `if Pine_builtin.equal [ a, b ] then Basics.False else Basics.True`
                    // when the operand type supports primitive equality. Mirrors the
                    // `LoweredOperator.Equal` branch above; the `if-then-else` form
                    // (rather than e.g. `Pine_kernel.equal [ Pine_kernel.equal [a, b], False ]`)
                    // matches the canonical lowering style used for `&&` / `||`,
                    // composes cleanly with downstream constant-folding on Bool, and
                    // sidesteps the FalseValue / TrueValue / skip-byte representational
                    // collision documented in PineKernelValues.
                    var equalApplication =
                        BuildBuiltinApplication(
                            "equal",
                            left,
                            right);

                    return
                        BuildIfBlock(
                            equalApplication,
                            BuildBasicsBoolReference(value: false),
                            BuildBasicsBoolReference(value: true));
                }
            }
            else if (loweredOp is LoweredOperator.IntLt or LoweredOperator.IntGt or LoweredOperator.IntLe or LoweredOperator.IntGe)
            {
                if (ProvesIntegerBuiltin(leftType, rightType))
                {
                    return
                        BuildIntComparisonApplication(
                            loweredOp,
                            left,
                            right);
                }
            }
            else if (loweredOp is LoweredOperator.BoolAnd)
            {
                if (TryMergeChainedIntIsSortedAsc(left, right) is { } merged)
                {
                    return merged;
                }

                // Lower `a && b` to `if a then b else False`. This expresses the
                // short-circuiting semantics of `&&` directly in terms of the
                // primitive conditional expression and gives downstream stages a
                // single canonical form to optimize (e.g., constant folding when
                // `a` reduces to a literal Bool).
                return
                    BuildIfBlock(
                        left,
                        right,
                        BuildBasicsBoolReference(value: false));
            }
            else if (loweredOp is LoweredOperator.BoolOr)
            {
                // Lower `a || b` to `if a then True else b`. As with `&&`, this
                // makes the short-circuiting semantics of `||` explicit.
                return
                    BuildIfBlock(
                        left,
                        BuildBasicsBoolReference(value: true),
                        right);
            }
            else if (expectedType is TypeInference.InferredType.IntType ||
                ProvesIntegerBuiltin(leftType, rightType))
            {
                return loweredOp switch
                {
                    LoweredOperator.IntSub =>
                    BuildBuiltinSubtractionApplication(left, right),

                    LoweredOperator.IntAdd =>
                    BuildBuiltinApplication("int_add", left, right),

                    LoweredOperator.IntMul =>
                    BuildBuiltinApplication("int_mul", left, right),

                    _ =>
                    new SyntaxTypes.Expression.Application(rewrittenFunction, rewrittenArguments)
                };
            }
        }

        return new SyntaxTypes.Expression.Application(rewrittenFunction, rewrittenArguments);
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
        TypeInference.InferredType rightType,
        RewriteContext context) =>
        TypeSupportsPrimitiveEquality(leftType, context, []) ||
        TypeSupportsPrimitiveEquality(rightType, context, []);

    /// <summary>
    /// Returns <c>true</c> when the given type is guaranteed to never contain a Dict or Set,
    /// meaning Pine structural equality is sufficient for Elm <c>==</c>.
    /// <para>
    /// The <paramref name="visiting"/> set prevents infinite recursion when a choice type
    /// refers to itself (directly or indirectly).
    /// </para>
    /// </summary>
    private static bool TypeSupportsPrimitiveEquality(
        TypeInference.InferredType type,
        RewriteContext context,
        HashSet<QualifiedNameRef> visiting)
    {
        switch (type)
        {
            case TypeInference.InferredType.IntType:
            case TypeInference.InferredType.StringType:
            case TypeInference.InferredType.CharType:
            case TypeInference.InferredType.BoolType:
                return true;

            // FloatType and NumberType are NOT safe for primitive equality:
            // different Pine representations (numerator/denominator pairs) can represent
            // the same float value and must be treated as equal in Elm.

            case TypeInference.InferredType.TupleType tupleType:
                return
                    tupleType.ElementTypes.All(
                        elementType => TypeSupportsPrimitiveEquality(elementType, context, visiting));

            case TypeInference.InferredType.ListType listType:
                return TypeSupportsPrimitiveEquality(listType.ElementType, context, visiting);

            case TypeInference.InferredType.ChoiceType choiceType:
                {
                    var qualifiedName =
                        QualifiedNameHelper.ToQualifiedNameRef(choiceType.ModuleName, choiceType.TypeName);

                    // First expand aliases — the ChoiceType might actually be an alias for a concrete type.
                    if (context.AliasTypes.TryGetValue(qualifiedName, out var aliasType))
                    {
                        return TypeSupportsPrimitiveEquality(aliasType, context, visiting);
                    }

                    // Recognize List.List as a list type: safe if its element type is safe.
                    if (choiceType.ModuleName is ["List"] && choiceType.TypeName is "List" &&
                        choiceType.TypeArguments.Count is 1)
                    {
                        return TypeSupportsPrimitiveEquality(choiceType.TypeArguments[0], context, visiting);
                    }

                    // Dict and Set can contain different Pine values that compare equal in Elm,
                    // so they are never safe for primitive equality.
                    if ((choiceType.ModuleName is ["Dict"] && choiceType.TypeName is "Dict") ||
                        (choiceType.ModuleName is ["Set"] && choiceType.TypeName is "Set"))
                    {
                        return false;
                    }

                    // Guard against infinite recursion for recursive types.
                    if (!visiting.Add(qualifiedName))
                        return true;

                    try
                    {
                        if (!context.ChoiceTypeDefinitions.TryGetValue(qualifiedName, out var definition))
                            return false;

                        return
                            definition.Constructors.All(
                                ctor =>
                                ctor.ArgumentTypes.All(
                                    argType =>
                                    TypeSupportsPrimitiveEquality(argType, context, visiting)));
                    }
                    finally
                    {
                        visiting.Remove(qualifiedName);
                    }
                }

            default:
                return false;
        }
    }

    private static SyntaxTypes.Expression RewriteLetExpression(
        SyntaxTypes.Expression.LetExpression letExpression,
        RewriteContext context,
        TypeInference.InferredType? expectedType)
    {
        var localBindingTypes = context.LocalBindingTypes.ToBuilder();

        foreach (var declaration in letExpression.Declarations)
        {
            switch (declaration)
            {
                case SyntaxTypes.LetDeclaration.LetFunction letFunction:
                    {
                        var implementation = letFunction.Function.Declaration;

                        localBindingTypes[implementation.Name] =
                            TypeInference.BuildFunctionTypeFromSignatureOrNull(letFunction.Function)
                            ??
                            BuildInferredFunctionType(
                                implementation.Expression,
                                implementation.Arguments,
                                context.CurrentModuleName,
                                context.FunctionSignatures);

                        break;
                    }

                case SyntaxTypes.LetDeclaration.LetDestructuring letDestructuring
                when letDestructuring.Pattern is SyntaxTypes.Pattern.VarPattern varPattern:

                    localBindingTypes[varPattern.Name] =
                        TypeInference.InferExpressionType(
                            letDestructuring.Expression,
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
            letExpression.Declarations
            .Select(declaration => RewriteLetDeclaration(declaration, letContext))
            .ToList();

        return
            new SyntaxTypes.Expression.LetExpression(
                rewrittenDeclarations,
                RewriteExpression(letExpression.Expression, letContext, expectedType));
    }

    private static SyntaxTypes.LetDeclaration RewriteLetDeclaration(
        SyntaxTypes.LetDeclaration declaration,
        RewriteContext context)
    {
        return
            declaration switch
            {
                SyntaxTypes.LetDeclaration.LetFunction letFunction =>
                RewriteLetFunctionDeclaration(letFunction, context),

                SyntaxTypes.LetDeclaration.LetDestructuring letDestructuring =>
                new SyntaxTypes.LetDeclaration.LetDestructuring(
                    letDestructuring.Pattern,
                    RewriteExpression(letDestructuring.Expression, context)),

                _ =>
                declaration
            };
    }

    private static SyntaxTypes.LetDeclaration RewriteLetFunctionDeclaration(
        SyntaxTypes.LetDeclaration.LetFunction letFunction,
        RewriteContext context)
    {
        var implementation = letFunction.Function.Declaration;

        var inferred =
            TypeInference.InferFunctionDeclarationType(
                implementation.Expression,
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
            new SyntaxTypes.LetDeclaration.LetFunction(
                letFunction.Function with
                {
                    Declaration =
                    implementation with
                    {
                        Expression = RewriteExpression(implementation.Expression, nestedContext, expectedReturnType)
                    }
                });
    }

    private static SyntaxTypes.Expression.LambdaExpression RewriteLambda(
        SyntaxTypes.Expression.LambdaExpression lambda,
        RewriteContext context,
        TypeInference.InferredType? expectedType)
    {
        var inferred =
            TypeInference.InferFunctionDeclarationType(
                lambda.Expression,
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
        IReadOnlyList<SyntaxTypes.Pattern> arguments,
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
            if (arguments[index] is not SyntaxTypes.Pattern.VarPattern varPattern ||
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
        if (functionExpression is SyntaxTypes.Expression.Identifier functionOrValue &&
            functionOrValue.QualifiedName.Namespaces.Count is 1 &&
            functionOrValue.QualifiedName.Namespaces[0] is "Basics")
        {
            return functionOrValue.QualifiedName.DeclName switch
            {
                "add" => LoweredOperator.IntAdd,
                "sub" => LoweredOperator.IntSub,
                "mul" => LoweredOperator.IntMul,
                "eq" => LoweredOperator.Equal,
                "neq" => LoweredOperator.NotEqual,
                "lt" => LoweredOperator.IntLt,
                "gt" => LoweredOperator.IntGt,
                "le" => LoweredOperator.IntLe,
                "ge" => LoweredOperator.IntGe,
                "and" => LoweredOperator.BoolAnd,
                "or" => LoweredOperator.BoolOr,

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
                "/=" => LoweredOperator.NotEqual,
                "<" => LoweredOperator.IntLt,
                ">" => LoweredOperator.IntGt,
                "<=" => LoweredOperator.IntLe,
                ">=" => LoweredOperator.IntGe,
                "&&" => LoweredOperator.BoolAnd,
                "||" => LoweredOperator.BoolOr,

                _ =>
                null
            };
        }

        return null;
    }

    private static SyntaxTypes.Expression BuildBuiltinApplication(
        string builtinName,
        SyntaxTypes.Expression left,
        SyntaxTypes.Expression right) =>
        new SyntaxTypes.Expression.Application(
            SyntaxTypes.Expression.Identifier.Create(["Pine_builtin"], builtinName),
            [new SyntaxTypes.Expression.ListExpr([left, right])]);

    /// <summary>
    /// Builds a reference to <c>Basics.True</c> or <c>Basics.False</c>, used by the
    /// <see cref="LoweredOperator.BoolAnd"/> / <see cref="LoweredOperator.BoolOr"/>
    /// lowerings that translate <c>&amp;&amp;</c> / <c>||</c> (and the equivalent
    /// <c>Basics.and</c> / <c>Basics.or</c> applications) into <c>if-then-else</c>
    /// expressions.
    /// </summary>
    private static SyntaxTypes.Expression BuildBasicsBoolReference(bool value) =>
        SyntaxTypes.Expression.Identifier.Create(["Basics"], value ? "True" : "False");

    /// <summary>
    /// Builds an <c>if-then-else</c> expression from the supplied condition,
    /// then-branch and else-branch expressions.
    /// </summary>
    private static SyntaxTypes.Expression BuildIfBlock(
        SyntaxTypes.Expression condition,
        SyntaxTypes.Expression thenBranch,
        SyntaxTypes.Expression elseBranch) =>
        new SyntaxTypes.Expression.IfBlock(condition, thenBranch, elseBranch);

    private static SyntaxTypes.Expression BuildBuiltinSubtractionApplication(
        SyntaxTypes.Expression left,
        SyntaxTypes.Expression right)
    {
        var negatedRight =
            BuildBuiltinApplication(
                "int_mul",
                BuildIntegerLiteral(-1),
                right);

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
        SyntaxTypes.Expression left,
        SyntaxTypes.Expression right)
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
    /// to <c>a + 1 &lt;= b</c>. When either operand is an integer literal (optionally negated),
    /// the offset is folded into the literal to avoid emitting <c>int_add</c>.
    /// </para>
    /// </summary>
    private static SyntaxTypes.Expression BuildStrictIntIsSortedAscApplication(
        SyntaxTypes.Expression first,
        SyntaxTypes.Expression second)
    {
        // If the first operand is a literal (or a negation of one), fold +1 into it directly.
        if (TryGetIntegerLiteralValue(first) is { } firstLiteral)
        {
            return BuildIntIsSortedAscApplication([BuildIntegerLiteral(firstLiteral + 1), second]);
        }

        // If the second operand is a literal (or a negation of one), subtract 1 from it
        // to avoid emitting int_add on the first operand.
        if (TryGetIntegerLiteralValue(second) is { } secondLiteral)
        {
            return BuildIntIsSortedAscApplication([first, BuildIntegerLiteral(secondLiteral - 1)]);
        }

        // General case: offset the first operand with int_add [first, 1].
        var offsetFirst =
            BuildBuiltinApplication(
                "int_add",
                first,
                BuildIntegerLiteral(1));

        return BuildIntIsSortedAscApplication([offsetFirst, second]);
    }

    /// <summary>
    /// Extracts the numeric value of an integer literal, or the negated value of a
    /// negated integer literal, otherwise <c>null</c>.
    /// </summary>
    private static BigInteger? TryGetIntegerLiteralValue(SyntaxTypes.Expression expression) =>
        expression switch
        {
            SyntaxTypes.Expression.IntegerLiteral integer => integer.Value,
            SyntaxTypes.Expression.Negation { Expression: SyntaxTypes.Expression.IntegerLiteral negated } => -negated.Value,

            _ =>
            null,
        };

    /// <summary>
    /// Builds an integer literal expression, precomputing its <see cref="PineValue"/> encoding.
    /// </summary>
    private static SyntaxTypes.Expression BuildIntegerLiteral(BigInteger value) =>
        new SyntaxTypes.Expression.IntegerLiteral(value, IntegerEncoding.EncodeSignedInteger(value));

    /// <summary>
    /// Builds a <c>Pine_builtin.int_is_sorted_asc</c> application with the given operands list.
    /// </summary>
    private static SyntaxTypes.Expression BuildIntIsSortedAscApplication(
        IReadOnlyList<SyntaxTypes.Expression> operands) =>
        new SyntaxTypes.Expression.Application(
            SyntaxTypes.Expression.Identifier.Create(["Pine_builtin"], "int_is_sorted_asc"),
            [new SyntaxTypes.Expression.ListExpr([.. operands])]);

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
        if (SyntaxExpressionsAreEqual(leftLast, rightFirst))
        {
            var mergedOperands = new List<SyntaxTypes.Expression>(leftOperands.Count + rightOperands.Count - 1);
            mergedOperands.AddRange(leftOperands);

            for (var i = 1; i < rightOperands.Count; i++)
            {
                mergedOperands.Add(rightOperands[i]);
            }

            return BuildIntIsSortedAscApplication(mergedOperands);
        }

        // Case 2: Strict chain where rightFirst is int_add [leftLast, 1] (e.g., < chains).
        if (IsIntAddOffsetByOne(leftLast, rightFirst))
        {
            var mergedOperands = new List<SyntaxTypes.Expression>(leftOperands.Count + rightOperands.Count);
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
            app.Function is not SyntaxTypes.Expression.Identifier fv ||
            fv.QualifiedName.Namespaces is not ["Pine_builtin"] ||
            fv.QualifiedName.DeclName is not "int_add" ||
            app.Arguments.Count is not 1 ||
            app.Arguments[0] is not SyntaxTypes.Expression.ListExpr listExpr ||
            listExpr.Elements.Count is not 2)
        {
            return false;
        }

        return
            (SyntaxExpressionsAreEqual(listExpr.Elements[0], baseExpr) &&
            IsIntegerLiteral(listExpr.Elements[1], 1)) ||
            (SyntaxExpressionsAreEqual(listExpr.Elements[1], baseExpr) &&
            IsIntegerLiteral(listExpr.Elements[0], 1));
    }

    private static bool IsIntegerLiteral(SyntaxTypes.Expression expression, BigInteger value) =>
        expression is SyntaxTypes.Expression.IntegerLiteral integer && integer.Value == value;

    /// <summary>
    /// Extracts the operand list from an <c>int_is_sorted_asc</c> application,
    /// or returns null if the expression is not such an application.
    /// </summary>
    private static IReadOnlyList<SyntaxTypes.Expression>? TryExtractIntIsSortedAscOperands(
        SyntaxTypes.Expression expression)
    {
        if (expression is not SyntaxTypes.Expression.Application application ||
            application.Function is not SyntaxTypes.Expression.Identifier functionOrValue ||
            functionOrValue.QualifiedName.Namespaces is not ["Pine_builtin"] ||
            functionOrValue.QualifiedName.DeclName is not "int_is_sorted_asc" ||
            application.Arguments.Count is not 1 ||
            application.Arguments[0] is not SyntaxTypes.Expression.ListExpr listExpr)
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
            (SyntaxTypes.Expression.Identifier leftFv, SyntaxTypes.Expression.Identifier rightFv) =>
            leftFv.QualifiedName.Equals(rightFv.QualifiedName),

            (SyntaxTypes.Expression.IntegerLiteral leftInt, SyntaxTypes.Expression.IntegerLiteral rightInt) =>
            leftInt.Value == rightInt.Value,

            (SyntaxTypes.Expression.Application leftApp, SyntaxTypes.Expression.Application rightApp) =>
            SyntaxExpressionsAreEqual(leftApp.Function, rightApp.Function) &&
            leftApp.Arguments.Count == rightApp.Arguments.Count &&
            leftApp.Arguments.Zip(rightApp.Arguments).All(
                pair => SyntaxExpressionsAreEqual(pair.First, pair.Second)),

            (SyntaxTypes.Expression.ListExpr leftList, SyntaxTypes.Expression.ListExpr rightList) =>
            leftList.Elements.Count == rightList.Elements.Count &&
            leftList.Elements.Zip(rightList.Elements).All(
                pair => SyntaxExpressionsAreEqual(pair.First, pair.Second)),

            (SyntaxTypes.Expression.Negation leftNeg, SyntaxTypes.Expression.Negation rightNeg) =>
            SyntaxExpressionsAreEqual(leftNeg.Expression, rightNeg.Expression),

            _ =>
            false
        };
    }

    private static ImmutableDictionary<QualifiedNameRef, FunctionTypeInfo> BuildFunctionTypes(
        ImmutableDictionary<DeclQualifiedName, SyntaxTypes.Declaration> declarations)
    {
        var result = new Dictionary<QualifiedNameRef, FunctionTypeInfo>();

        foreach (var (key, decl) in declarations)
        {
            if (decl is SyntaxTypes.Declaration.FunctionDeclaration declaration)
            {
                var functionName = declaration.Function.Declaration.Name;

                result[QualifiedNameHelper.ToQualifiedNameRef(key.Namespaces, functionName)] =
                    new FunctionTypeInfo(
                        TypeInference.GetFunctionReturnType(declaration),
                        TypeInference.GetFunctionParameterTypes(declaration));
            }
        }

        return result.ToImmutableDictionary();
    }

    private static ImmutableDictionary<QualifiedNameRef, TypeInference.InferredType> BuildAliasTypes(
        ImmutableDictionary<DeclQualifiedName, SyntaxTypes.Declaration> declarations)
    {
        var result = new Dictionary<QualifiedNameRef, TypeInference.InferredType>();

        foreach (var (key, decl) in declarations)
        {
            if (decl is SyntaxTypes.Declaration.AliasDeclaration declaration)
            {
                result[QualifiedNameHelper.ToQualifiedNameRef(key.Namespaces, declaration.TypeAlias.Name)] =
                    TypeInference.TypeAnnotationToInferredType(declaration.TypeAlias.TypeAnnotation);
            }
        }

        return result.ToImmutableDictionary();
    }

    private static ImmutableDictionary<string, TypeInference.InferredType> BuildFunctionSignatures(
        ImmutableDictionary<DeclQualifiedName, SyntaxTypes.Declaration> declarations)
    {
        var builder = ImmutableDictionary.CreateBuilder<string, TypeInference.InferredType>();

        foreach (var (key, decl) in declarations)
        {
            var moduleNameString = string.Join(".", key.Namespaces);

            TypeInference.CollectFunctionSignaturesFromDeclaration(
                decl,
                moduleNameString,
                builder);
        }

        return builder.ToImmutable();
    }

    private static ImmutableDictionary<string, int> BuildParameterNames(
        IReadOnlyList<SyntaxTypes.Pattern> arguments)
    {
        var builder = ImmutableDictionary.CreateBuilder<string, int>();

        for (var index = 0; index < arguments.Count; index++)
        {
            if (arguments[index] is SyntaxTypes.Pattern.VarPattern varPattern)
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
        if (application.Function is not SyntaxTypes.Expression.Identifier functionOrValue)
        {
            return [];
        }

        var qualifiedName =
            functionOrValue.QualifiedName.Namespaces.Count > 0
            ?
            QualifiedNameHelper.ToQualifiedNameRef(
                functionOrValue.QualifiedName.Namespaces,
                functionOrValue.QualifiedName.DeclName)
            :
            QualifiedNameHelper.FromQualifiedNameString(
                context.CurrentModuleName + "." + functionOrValue.QualifiedName.DeclName);

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
        IReadOnlyList<SyntaxTypes.Pattern> arguments,
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
        var annotatedParameterTypes =
            TypeInference.GetFunctionParameterTypes(function);

        if (annotatedParameterTypes.Count is 0)
            return [];

        var builder = ImmutableDictionary.CreateBuilder<string, TypeInference.InferredType>();

        for (var index = 0; index < annotatedParameterTypes.Count && index < function.Declaration.Arguments.Count; index++)
        {
            if (function.Declaration.Arguments[index] is SyntaxTypes.Pattern.VarPattern varPattern)
            {
                builder[varPattern.Name] = annotatedParameterTypes[index];
            }
        }

        return builder.ToImmutable();
    }
}
