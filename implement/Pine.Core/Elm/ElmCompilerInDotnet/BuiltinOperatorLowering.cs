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
/// (<see cref="ElmSyntax.ElmSyntaxAbstract"/>): there is no source-location
/// tracking, no concrete-syntax <c>Node&lt;T&gt;</c> wrapper, and no bridging/conversion to or
/// from the concrete <c>Stil4mElmSyntax7</c> model. Callers are expected to supply and receive
/// declarations already expressed in the abstract model.
/// </para>
/// </summary>
public static class BuiltinOperatorLowering
{
    internal sealed record Configuration(
        bool LowerBuiltinOperators,
        bool LowerElmCoreBasics);

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
        Append,
    }

    private record RewriteContext(
        Configuration Configuration,
        string CurrentModuleName,
        ImmutableDictionary<string, int> ParameterNames,
        ImmutableDictionary<string, TypeInference.InferredType> ParameterTypes,
        ImmutableDictionary<string, TypeInference.InferredType> LocalBindingTypes,
        IReadOnlyDictionary<QualifiedNameRef, FunctionTypeInfo> FunctionTypes,
        IReadOnlyDictionary<QualifiedNameRef, TypeInference.TypeAliasDefinition> AliasTypes,
        IReadOnlyDictionary<QualifiedNameRef, TypeInference.ChoiceTypeDefinition> ChoiceTypeDefinitions,
        IReadOnlyDictionary<QualifiedNameRef, IReadOnlyList<TypeInference.InferredType>> ConstructorArgumentTypes,
        ImmutableDictionary<string, TypeInference.InferredType> FunctionSignatures);

    /// <summary>
    /// Applies builtin-operator lowering to a flat dictionary of Elm declarations
    /// (in the abstract syntax model) that have already passed earlier syntax optimization stages.
    /// </summary>
    /// <param name="declarations">The flat declaration dictionary to rewrite.</param>
    /// <returns>The rewritten declarations, or an error message.</returns>
    public static Result<string, ImmutableDictionary<DeclQualifiedName, SyntaxTypes.Declaration>> Apply(
        ImmutableDictionary<DeclQualifiedName, SyntaxTypes.Declaration> declarations) =>
        Apply(
            declarations,
            new Configuration(
                LowerBuiltinOperators: true,
                LowerElmCoreBasics: false));

    internal static Result<string, ImmutableDictionary<DeclQualifiedName, SyntaxTypes.Declaration>> ApplyElmCoreBasics(
        ImmutableDictionary<DeclQualifiedName, SyntaxTypes.Declaration> declarations) =>
        Apply(
            declarations,
            new Configuration(
                LowerBuiltinOperators: false,
                LowerElmCoreBasics: true));

    internal static Result<string, ImmutableDictionary<DeclQualifiedName, SyntaxTypes.Declaration>> Apply(
        ImmutableDictionary<DeclQualifiedName, SyntaxTypes.Declaration> declarations,
        Configuration configuration)
    {
        if (!configuration.LowerBuiltinOperators &&
            !configuration.LowerElmCoreBasics)
        {
            return declarations;
        }

        var functionTypes = BuildFunctionTypes(declarations);
        var aliasTypes = BuildAliasTypes(declarations);

        var choiceTypeDefinitions =
            TypeInference.BuildChoiceTypeDefinitions(declarations);

        var constructorArgumentTypes =
            BuildConstructorArgumentTypes(choiceTypeDefinitions, aliasTypes);

        var functionSignatures = BuildFunctionSignatures(declarations);

        var resultBuilder =
            ImmutableDictionary.CreateBuilder<DeclQualifiedName, SyntaxTypes.Declaration>();

        foreach (var (key, decl) in declarations)
        {
            var moduleNameString = string.Join(".", key.Namespaces);

            var rewritten =
                RewriteDeclaration(
                    decl,
                    configuration,
                    moduleNameString,
                    functionTypes,
                    aliasTypes,
                    choiceTypeDefinitions,
                    constructorArgumentTypes,
                    functionSignatures);

            resultBuilder[key] = rewritten;
        }

        return resultBuilder.ToImmutable();
    }

    private static SyntaxTypes.Declaration RewriteDeclaration(
        SyntaxTypes.Declaration declaration,
        Configuration configuration,
        string moduleName,
        IReadOnlyDictionary<QualifiedNameRef, FunctionTypeInfo> functionTypes,
        IReadOnlyDictionary<QualifiedNameRef, TypeInference.TypeAliasDefinition> aliasTypes,
        IReadOnlyDictionary<QualifiedNameRef, TypeInference.ChoiceTypeDefinition> choiceTypeDefinitions,
        IReadOnlyDictionary<QualifiedNameRef, IReadOnlyList<TypeInference.InferredType>> constructorArgumentTypes,
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
            ExpandAliasTypes(
                BuildExplicitParameterTypes(functionDeclaration.Function),
                aliasTypes);

        var context =
            new RewriteContext(
                Configuration: configuration,
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
                ConstructorArgumentTypes: constructorArgumentTypes,
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
                RewriteCaseExpression(caseExpression, context, expandedExpectedType),

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

    private static SyntaxTypes.Expression.CaseExpression RewriteCaseExpression(
        SyntaxTypes.Expression.CaseExpression caseExpression,
        RewriteContext context,
        TypeInference.InferredType? expectedType)
    {
        var scrutineeType =
            InferExpressionType(caseExpression.Expression, context);

        var rewrittenCases = new List<SyntaxTypes.Case>(caseExpression.Cases.Count);

        foreach (var caseItem in caseExpression.Cases)
        {
            var constructorArgumentTypes = context.ConstructorArgumentTypes;

            if (scrutineeType is not null &&
                caseItem.Pattern is SyntaxTypes.Pattern.NamedPattern namedPattern)
            {
                var constructorName =
                    new QualifiedNameRef(
                        namedPattern.Name.ModuleName,
                        namedPattern.Name.Name);

                if (context.FunctionTypes.TryGetValue(constructorName, out var constructorType))
                {
                    constructorArgumentTypes =
                        constructorArgumentTypes
                        .ToImmutableDictionary()
                        .SetItem(
                            constructorName,
                            TypeInference.SpecializeTypesFromMatch(
                                constructorType.ReturnType,
                                scrutineeType,
                                constructorType.ParameterTypes));
                }
            }

            var caseLocalBindingTypes =
                TypeInference.ExtractPatternBindingTypesWithConstructors(
                    caseItem.Pattern,
                    constructorArgumentTypes,
                    context.LocalBindingTypes);

            if (scrutineeType is not null)
            {
                caseLocalBindingTypes =
                    TypeInference.ExtractPatternBindingTypesFromInferred(
                        caseItem.Pattern,
                        scrutineeType,
                        caseLocalBindingTypes,
                        constructorArgumentTypes);
            }

            rewrittenCases.Add(
                new SyntaxTypes.Case(
                    caseItem.Pattern,
                    RewriteExpression(
                        caseItem.Expression,
                        context with { LocalBindingTypes = caseLocalBindingTypes },
                        expectedType)));
        }

        return
            new SyntaxTypes.Expression.CaseExpression(
                RewriteExpression(caseExpression.Expression, context),
                rewrittenCases);
    }

    private static SyntaxTypes.Expression RewriteApplication(
        SyntaxTypes.Expression.Application application,
        RewriteContext context,
        TypeInference.InferredType? expectedType)
    {
        TypeInference.InferredType functionExpectedType =
            expectedType ?? new TypeInference.InferredType.UnknownType();

        for (var argumentIndex = application.Arguments.Count - 1; argumentIndex >= 0; argumentIndex--)
        {
            functionExpectedType =
                new TypeInference.InferredType.FunctionType(
                    InferExpressionType(application.Arguments[argumentIndex], context),
                    functionExpectedType);
        }

        var rewrittenFunction =
            RewriteExpression(application.Function, context, functionExpectedType);

        var expectedArgumentTypes = GetExpectedArgumentTypes(application, context, expectedType);

        var rewrittenArguments = new List<SyntaxTypes.Expression>(application.Arguments.Count);

        for (var i = 0; i < application.Arguments.Count; i++)
        {
            rewrittenArguments.Add(
                RewriteExpression(
                    application.Arguments[i],
                    context,
                    expectedArgumentTypes.ElementAtOrDefault(i)));
        }

        var rewrittenApplication =
            new SyntaxTypes.Expression.Application(rewrittenFunction, rewrittenArguments);

        if (context.Configuration.LowerBuiltinOperators)
        {
            while (TryLowerPipe(rewrittenApplication) is { } loweredPipe)
                rewrittenApplication = loweredPipe;
        }

        rewrittenFunction = rewrittenApplication.Function;
        rewrittenArguments = [.. rewrittenApplication.Arguments];

        if (context.Configuration.LowerElmCoreBasics &&
            rewrittenApplication.Function is SyntaxTypes.Expression.Identifier
            {
                QualifiedName.Namespaces: ["Basics"],
                QualifiedName.DeclName: "min" or "max" or "negate" or "abs" or "clamp"
            })
        {
            var argumentTypes =
                rewrittenArguments
                .Select(argument => InferExpressionType(argument, context))
                .ToList();

            if (ElmCoreBasicsLowering.TryLowerApplication(
                rewrittenApplication,
                argumentTypes,
                expectedType) is { } loweredCoreBasics)
            {
                return loweredCoreBasics;
            }
        }

        if (rewrittenArguments.Count is 2)
        {
            var left = rewrittenArguments[0];
            var right = rewrittenArguments[1];

            var leftType =
                InferExpressionType(left, context);

            var rightType =
                InferExpressionType(right, context);

            if (!context.Configuration.LowerBuiltinOperators ||
                TryMapBuiltinOperator(rewrittenFunction) is not { } loweredOp)
            {
                return rewrittenApplication;
            }

            if (loweredOp is LoweredOperator.Append)
            {
                if (leftType is TypeInference.InferredType.StringType ||
                    rightType is TypeInference.InferredType.StringType)
                {
                    return BuildStringAppendApplication(left, right);
                }

                if (leftType is TypeInference.InferredType.ListType ||
                    rightType is TypeInference.InferredType.ListType)
                {
                    return BuildBuiltinApplication("concat", left, right);
                }
            }
            else if (loweredOp is LoweredOperator.Equal)
            {
                if (IsEmptyList(left) ||
                    IsEmptyList(right) ||
                    ProvesPrimitiveEqualityBuiltin(left, leftType, right, rightType, context))
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
                if (IsEmptyList(left) ||
                    IsEmptyList(right) ||
                    ProvesPrimitiveEqualityBuiltin(left, leftType, right, rightType, context))
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
                    rewrittenApplication
                };
            }
        }

        return rewrittenApplication;
    }

    /// <summary>
    /// Lowers saturated pipe operators to plain applications, appending the piped value to any
    /// arguments already applied to the function.
    /// </summary>
    private static SyntaxTypes.Expression.Application? TryLowerPipe(
        SyntaxTypes.Expression.Application application)
    {
        if (application.Function is not SyntaxTypes.Expression.Identifier
            {
                QualifiedName.Namespaces: ["Basics"],
                QualifiedName.DeclName: "apR" or "apL"
            } pipeIdentifier ||
            application.Arguments.Count < 2)
        {
            return null;
        }

        if (pipeIdentifier.QualifiedName.DeclName is "apR")
        {
            return
                ApplyPipeArguments(
                    application.Arguments[1],
                    [application.Arguments[0], .. application.Arguments.Skip(2)]);
        }

        return
            ApplyPipeArguments(
                application.Arguments[0],
                [.. application.Arguments.Skip(1)]);
    }

    private static SyntaxTypes.Expression.Application ApplyPipeArguments(
        SyntaxTypes.Expression function,
        IReadOnlyList<SyntaxTypes.Expression> arguments)
    {
        if (function is SyntaxTypes.Expression.Application existingApplication)
        {
            return
                new SyntaxTypes.Expression.Application(
                    existingApplication.Function,
                    [.. existingApplication.Arguments, .. arguments]);
        }

        return new SyntaxTypes.Expression.Application(function, arguments);
    }

    private static bool IsEmptyList(SyntaxTypes.Expression expression) =>
        expression is SyntaxTypes.Expression.ListExpr { Elements.Count: 0 };

    private static bool ProvesIntegerBuiltin(
        TypeInference.InferredType leftType,
        TypeInference.InferredType rightType) =>
        (leftType is TypeInference.InferredType.IntType &&
        (rightType is TypeInference.InferredType.IntType or TypeInference.InferredType.NumberType)) ||
        (rightType is TypeInference.InferredType.IntType &&
        (leftType is TypeInference.InferredType.IntType or TypeInference.InferredType.NumberType));

    private static bool ProvesPrimitiveEqualityBuiltin(
        SyntaxTypes.Expression left,
        TypeInference.InferredType leftType,
        SyntaxTypes.Expression right,
        TypeInference.InferredType rightType,
        RewriteContext context) =>
        ExpressionSupportsPrimitiveEquality(left, leftType, context) ||
        ExpressionSupportsPrimitiveEquality(right, rightType, context);

    private static bool ExpressionSupportsPrimitiveEquality(
        SyntaxTypes.Expression expression,
        TypeInference.InferredType type,
        RewriteContext context) =>
        TypeSupportsPrimitiveEquality(type, context, []) ||
        IsPayloadFreeChoiceTag(expression, context);

    private static bool IsPayloadFreeChoiceTag(
        SyntaxTypes.Expression expression,
        RewriteContext context)
    {
        if (expression is not SyntaxTypes.Expression.Identifier identifier)
            return false;

        var qualifiedName =
            identifier.QualifiedName.Namespaces.Count > 0
            ?
            QualifiedNameHelper.ToQualifiedNameRef(
                identifier.QualifiedName.Namespaces,
                identifier.QualifiedName.DeclName)
            :
            QualifiedNameHelper.FromQualifiedNameString(
                context.CurrentModuleName + "." + identifier.QualifiedName.DeclName);

        return
            context.ChoiceTypeDefinitions.Any(
                choiceType =>
                choiceType.Key.ModuleName.SequenceEqual(qualifiedName.ModuleName) &&
                choiceType.Value.Constructors.Any(
                    constructor =>
                    constructor.TagName == qualifiedName.Name &&
                    constructor.ArgumentTypes.Count is 0));
    }

    /// <summary>
    /// Returns <c>true</c> when Pine structural equality has the same semantics as Elm
    /// <c>==</c> for the given type.
    /// <para>
    /// The <paramref name="visiting"/> map prevents infinite recursion when a choice type
    /// refers to itself and rejects recursive occurrences with different type arguments.
    /// </para>
    /// </summary>
    private static bool TypeSupportsPrimitiveEquality(
        TypeInference.InferredType type,
        RewriteContext context,
        Dictionary<QualifiedNameRef, TypeInference.InferredType.ChoiceType> visiting)
    {
        switch (type)
        {
            case TypeInference.InferredType.IntType:
            case TypeInference.InferredType.StringType:
            case TypeInference.InferredType.CharType:
            case TypeInference.InferredType.BoolType:
                return true;

            case TypeInference.InferredType.FloatType:
            case TypeInference.InferredType.NumberType:
            case TypeInference.InferredType.TypeVariable:
                return false;

            case TypeInference.InferredType.TupleType tupleType:
                return
                    tupleType.ElementTypes.All(
                        elementType => TypeSupportsPrimitiveEquality(elementType, context, visiting));

            case TypeInference.InferredType.ListType listType:
                return TypeSupportsPrimitiveEquality(listType.ElementType, context, visiting);

            case TypeInference.InferredType.RecordType recordType:
                return
                    recordType.Fields.All(
                        field => TypeSupportsPrimitiveEquality(field.FieldType, context, visiting));

            case TypeInference.InferredType.ChoiceType choiceType:
                {
                    var qualifiedName =
                        QualifiedNameHelper.ToQualifiedNameRef(choiceType.ModuleName, choiceType.TypeName);

                    // First expand aliases — the ChoiceType might actually be an alias for a concrete type.
                    if (context.AliasTypes.ContainsKey(qualifiedName))
                    {
                        if (!visiting.TryAdd(qualifiedName, choiceType))
                            return false;

                        try
                        {
                            return
                                TypeSupportsPrimitiveEquality(
                                    TypeInference.ExpandTypeAliases(choiceType, context.AliasTypes),
                                    context,
                                    visiting);
                        }
                        finally
                        {
                            visiting.Remove(qualifiedName);
                        }
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
                    if (visiting.TryGetValue(qualifiedName, out var activeChoiceType))
                        return InferredTypesAreEquivalent(activeChoiceType, choiceType);

                    visiting.Add(qualifiedName, choiceType);

                    try
                    {
                        if (!context.ChoiceTypeDefinitions.TryGetValue(qualifiedName, out var definition))
                            return false;

                        var declaredChoiceType =
                            new TypeInference.InferredType.ChoiceType(
                                choiceType.ModuleName,
                                choiceType.TypeName,
                                [
                                .. definition.TypeParameters
                                .Select(
                                    typeParameter =>
                                    new TypeInference.InferredType.TypeVariable(typeParameter))
                                ]);

                        return
                            definition.Constructors.All(
                                ctor =>
                                TypeInference.SpecializeTypesFromMatch(
                                    declaredChoiceType,
                                    choiceType,
                                    ctor.ArgumentTypes)
                                .All(
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

    private static bool InferredTypesAreEquivalent(
        TypeInference.InferredType left,
        TypeInference.InferredType right)
    {
        switch (left)
        {
            case TypeInference.InferredType.IntType:
                return right is TypeInference.InferredType.IntType;

            case TypeInference.InferredType.FloatType:
                return right is TypeInference.InferredType.FloatType;

            case TypeInference.InferredType.StringType:
                return right is TypeInference.InferredType.StringType;

            case TypeInference.InferredType.CharType:
                return right is TypeInference.InferredType.CharType;

            case TypeInference.InferredType.BoolType:
                return right is TypeInference.InferredType.BoolType;

            case TypeInference.InferredType.NumberType:
                return right is TypeInference.InferredType.NumberType;

            case TypeInference.InferredType.TupleType leftTuple:
                return
                    right is TypeInference.InferredType.TupleType rightTuple &&
                    leftTuple.ElementTypes.Count == rightTuple.ElementTypes.Count &&
                    leftTuple.ElementTypes
                    .Zip(rightTuple.ElementTypes)
                    .All(pair => InferredTypesAreEquivalent(pair.First, pair.Second));

            case TypeInference.InferredType.RecordType leftRecord:
                return
                    right is TypeInference.InferredType.RecordType rightRecord &&
                    RecordFieldsAreEquivalent(leftRecord.Fields, rightRecord.Fields);

            case TypeInference.InferredType.OpenRecordType leftOpenRecord:
                return
                    right is TypeInference.InferredType.OpenRecordType rightOpenRecord &&
                    leftOpenRecord.ExtensionVariable == rightOpenRecord.ExtensionVariable &&
                    RecordFieldsAreEquivalent(leftOpenRecord.KnownFields, rightOpenRecord.KnownFields);

            case TypeInference.InferredType.FunctionType leftFunction:
                return
                    right is TypeInference.InferredType.FunctionType rightFunction &&
                    InferredTypesAreEquivalent(leftFunction.ArgumentType, rightFunction.ArgumentType) &&
                    InferredTypesAreEquivalent(leftFunction.ReturnType, rightFunction.ReturnType);

            case TypeInference.InferredType.ListType leftList:
                return
                    right is TypeInference.InferredType.ListType rightList &&
                    InferredTypesAreEquivalent(leftList.ElementType, rightList.ElementType);

            case TypeInference.InferredType.ChoiceType leftChoice:
                return
                    right is TypeInference.InferredType.ChoiceType rightChoice &&
                    leftChoice.ModuleName.SequenceEqual(rightChoice.ModuleName) &&
                    leftChoice.TypeName == rightChoice.TypeName &&
                    leftChoice.TypeArguments.Count == rightChoice.TypeArguments.Count &&
                    leftChoice.TypeArguments
                    .Zip(rightChoice.TypeArguments)
                    .All(pair => InferredTypesAreEquivalent(pair.First, pair.Second));

            case TypeInference.InferredType.TypeVariable leftVariable:
                return
                    right is TypeInference.InferredType.TypeVariable rightVariable &&
                    leftVariable.Name == rightVariable.Name &&
                    leftVariable.Constraint == rightVariable.Constraint;

            case TypeInference.InferredType.UnknownType:
                return right is TypeInference.InferredType.UnknownType;

            default:
                throw new System.NotImplementedException(
                    "InferredTypesAreEquivalent does not handle inferred type variant: " +
                    left.GetType().Name);
        }
    }

    private static bool RecordFieldsAreEquivalent(
        IReadOnlyList<(string FieldName, TypeInference.InferredType FieldType)> leftFields,
        IReadOnlyList<(string FieldName, TypeInference.InferredType FieldType)> rightFields) =>
        leftFields.Count == rightFields.Count &&
        leftFields
        .Zip(rightFields)
        .All(
            pair =>
            pair.First.FieldName == pair.Second.FieldName &&
            InferredTypesAreEquivalent(pair.First.FieldType, pair.Second.FieldType));

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

                case SyntaxTypes.LetDeclaration.LetDestructuring letDestructuring:
                    var bindingExpressionType =
                        InferExpressionType(
                            letDestructuring.Expression,
                            context with { LocalBindingTypes = localBindingTypes.ToImmutable() });

                    var bindingsFromPattern =
                        TypeInference.ExtractPatternBindingTypesWithConstructors(
                            letDestructuring.Pattern,
                            context.ConstructorArgumentTypes,
                            localBindingTypes.ToImmutable());

                    bindingsFromPattern =
                        TypeInference.ExtractPatternBindingTypesFromInferred(
                            letDestructuring.Pattern,
                            bindingExpressionType,
                            bindingsFromPattern,
                            context.ConstructorArgumentTypes);

                    localBindingTypes.Clear();
                    localBindingTypes.AddRange(bindingsFromPattern);

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

        explicitParameterTypes =
            ExpandAliasTypes(explicitParameterTypes, context.AliasTypes);

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
                LocalBindingTypes =
                BuildNestedLocalBindingTypes(
                    context,
                    SyntaxTypes.SyntaxAnalysis.CollectNamesBoundByPatterns(implementation.Arguments)),
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
                    expectedType,
                    context.ConstructorArgumentTypes),
                LocalBindingTypes =
                BuildNestedLocalBindingTypes(
                    context,
                    SyntaxTypes.SyntaxAnalysis.CollectNamesBoundByPatterns(lambda.Arguments)),
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
        TypeInference.InferredType? expectedType,
        IReadOnlyDictionary<QualifiedNameRef, IReadOnlyList<TypeInference.InferredType>> constructorArgumentTypes)
    {
        if (expectedType is not TypeInference.InferredType.FunctionType)
        {
            return inferredParameterTypes;
        }

        var mergedParameterTypes = inferredParameterTypes.ToBuilder();
        var remainingExpectedType = expectedType;

        for (var index = 0; index < arguments.Count; index++)
        {
            if (remainingExpectedType is not TypeInference.InferredType.FunctionType functionType)
            {
                break;
            }

            if (arguments[index] is SyntaxTypes.Pattern.VarPattern varPattern)
            {
                mergedParameterTypes[varPattern.Name] =
                    ChooseLambdaParameterType(
                        functionType.ArgumentType,
                        mergedParameterTypes.GetValueOrDefault(varPattern.Name));
            }
            else
            {
                var bindings =
                    TypeInference.ExtractPatternBindingTypesFromInferred(
                        arguments[index],
                        functionType.ArgumentType,
                        mergedParameterTypes.ToImmutable(),
                        constructorArgumentTypes);

                mergedParameterTypes.Clear();
                mergedParameterTypes.AddRange(bindings);
            }

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

    private static ImmutableDictionary<string, TypeInference.InferredType> BuildNestedLocalBindingTypes(
        RewriteContext context,
        IReadOnlySet<string> nestedParameterNames)
    {
        var localBindings = context.LocalBindingTypes.ToBuilder();

        foreach (var (name, type) in context.ParameterTypes)
        {
            if (!nestedParameterNames.Contains(name))
                localBindings[name] = type;
        }

        return localBindings.ToImmutable();
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
                "append" => LoweredOperator.Append,

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
                "++" => LoweredOperator.Append,

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

    private static SyntaxTypes.Expression BuildStringAppendApplication(
        SyntaxTypes.Expression left,
        SyntaxTypes.Expression right) =>
        new SyntaxTypes.Expression.Application(
            SyntaxTypes.Expression.Identifier.Create(["String"], "String"),
            [
            BuildBuiltinApplication(
                "concat",
                BuildStringContentAccess(left),
                BuildStringContentAccess(right)),
            ]);

    private static SyntaxTypes.Expression BuildStringContentAccess(
        SyntaxTypes.Expression stringExpression) =>
        BuildBuiltinUnaryApplication(
            "head",
            BuildBuiltinApplication(
                "skip",
                BuildIntegerLiteral(2),
                stringExpression));

    private static SyntaxTypes.Expression BuildBuiltinUnaryApplication(
        string builtinName,
        SyntaxTypes.Expression argument) =>
        new SyntaxTypes.Expression.Application(
            SyntaxTypes.Expression.Identifier.Create(["Pine_builtin"], builtinName),
            [argument]);

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

            SyntaxTypes.Expression.Negation { Expression: SyntaxTypes.Expression.IntegerLiteral negated } =>
            -negated.Value,

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

        var signatures = BuildFunctionSignatures(declarations);

        foreach (var (qualifiedNameString, signatureType) in signatures)
        {
            var qualifiedName = QualifiedNameHelper.FromQualifiedNameString(qualifiedNameString);

            if (result.ContainsKey(qualifiedName))
                continue;

            var parameterTypes = ExtractFunctionParameterTypes(signatureType);
            var returnType = signatureType;

            while (returnType is TypeInference.InferredType.FunctionType functionType)
                returnType = functionType.ReturnType;

            result[qualifiedName] = new FunctionTypeInfo(returnType, parameterTypes);
        }

        return result.ToImmutableDictionary();
    }

    private static ImmutableDictionary<QualifiedNameRef, TypeInference.TypeAliasDefinition> BuildAliasTypes(
        ImmutableDictionary<DeclQualifiedName, SyntaxTypes.Declaration> declarations)
    {
        var result = new Dictionary<QualifiedNameRef, TypeInference.TypeAliasDefinition>();

        foreach (var (key, decl) in declarations)
        {
            if (decl is SyntaxTypes.Declaration.AliasDeclaration declaration)
            {
                result[QualifiedNameHelper.ToQualifiedNameRef(key.Namespaces, declaration.TypeAlias.Name)] =
                    new TypeInference.TypeAliasDefinition(
                        declaration.TypeAlias.Generics,
                        TypeInference.TypeAnnotationToInferredType(declaration.TypeAlias.TypeAnnotation));
            }
        }

        return result.ToImmutableDictionary();
    }

    private static ImmutableDictionary<QualifiedNameRef, IReadOnlyList<TypeInference.InferredType>>
        BuildConstructorArgumentTypes(
        IReadOnlyDictionary<QualifiedNameRef, TypeInference.ChoiceTypeDefinition> choiceTypeDefinitions,
        IReadOnlyDictionary<QualifiedNameRef, TypeInference.TypeAliasDefinition> aliasTypes)
    {
        var result =
            ImmutableDictionary.CreateBuilder<QualifiedNameRef, IReadOnlyList<TypeInference.InferredType>>();

        foreach (var (choiceTypeName, definition) in choiceTypeDefinitions)
        {
            foreach (var constructor in definition.Constructors)
            {
                result[
                    QualifiedNameHelper.ToQualifiedNameRef(
                        choiceTypeName.ModuleName,
                        constructor.TagName)] =
                    [
                    .. constructor.ArgumentTypes.Select(
                        argumentType => ExpandAliasType(argumentType, aliasTypes) ?? argumentType)
                    ];
            }
        }

        return result.ToImmutable();
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
        RewriteContext context,
        TypeInference.InferredType? expectedType)
    {
        if (application.Function is not SyntaxTypes.Expression.Identifier functionOrValue)
        {
            return [];
        }

        if (functionOrValue.QualifiedName.Namespaces is ["Basics"] &&
            application.Arguments.Count is 2 &&
            functionOrValue.QualifiedName.DeclName is "apR" or "apL")
        {
            var valueArgumentIndex =
                functionOrValue.QualifiedName.DeclName is "apR" ? 0 : 1;

            var valueType =
                InferExpressionType(application.Arguments[valueArgumentIndex], context);

            var appliedFunctionType =
                new TypeInference.InferredType.FunctionType(
                    valueType,
                    expectedType ?? new TypeInference.InferredType.UnknownType());

            return
                functionOrValue.QualifiedName.DeclName is "apR"
                ?
                [valueType, appliedFunctionType]
                :
                [appliedFunctionType, valueType];
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

        IReadOnlyList<TypeInference.InferredType> parameterTypes;
        TypeInference.InferredType returnType;

        if (!context.FunctionTypes.TryGetValue(qualifiedName, out var functionTypeInfo))
        {
            var qualifiedNameString =
                QualifiedNameHelper.ToQualifiedNameString(qualifiedName.ModuleName, qualifiedName.Name);

            if (!context.FunctionSignatures.TryGetValue(qualifiedNameString, out var functionSignatureType))
            {
                return [];
            }

            parameterTypes = ExtractFunctionParameterTypes(functionSignatureType);
            returnType = GetFunctionReturnType(functionSignatureType);
        }
        else
        {
            parameterTypes = functionTypeInfo.ParameterTypes;
            returnType = functionTypeInfo.ReturnType;
        }

        parameterTypes =
            [
            .. parameterTypes.Select(
                parameterType => ExpandAliasType(parameterType, context.AliasTypes) ?? parameterType)
            ];

        returnType = ExpandAliasType(returnType, context.AliasTypes) ?? returnType;

        var actualArgumentTypes =
            application.Arguments
            .Select(
                argument =>
                InferExpressionType(argument, context))
            .ToList();

        IReadOnlyList<TypeInference.InferredType> specializedParameterTypes =
            TypeInference.SpecializeTypesFromArguments(
                parameterTypes,
                actualArgumentTypes);

        if (expectedType is not null)
        {
            var partialResultType = returnType;

            for (var index = parameterTypes.Count - 1; index >= application.Arguments.Count; index--)
            {
                partialResultType =
                    new TypeInference.InferredType.FunctionType(
                        parameterTypes[index],
                        partialResultType);
            }

            specializedParameterTypes =
                TypeInference.SpecializeTypesFromMatch(
                    partialResultType,
                    expectedType,
                    specializedParameterTypes);
        }

        return
            [
            .. specializedParameterTypes
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

    private static TypeInference.InferredType GetFunctionReturnType(
        TypeInference.InferredType functionType)
    {
        while (functionType is TypeInference.InferredType.FunctionType nextFunctionType)
            functionType = nextFunctionType.ReturnType;

        return functionType;
    }

    private static TypeInference.InferredType? ExpandAliasType(
        TypeInference.InferredType? inferredType,
        IReadOnlyDictionary<QualifiedNameRef, TypeInference.TypeAliasDefinition> aliasTypes)
    {
        return
            inferredType is null
            ?
            null
            :
            TypeInference.ExpandTypeAliases(inferredType, aliasTypes);
    }

    private static TypeInference.InferredType InferExpressionType(
        SyntaxTypes.Expression expression,
        RewriteContext context)
    {
        if (expression is SyntaxTypes.Expression.RecordAccess recordAccess)
        {
            var recordType = InferExpressionType(recordAccess.Record, context);

            if (recordType is TypeInference.InferredType.RecordType closedRecord)
            {
                var fieldType =
                    closedRecord.Fields
                    .FirstOrDefault(field => field.FieldName == recordAccess.FieldName)
                    .FieldType;

                if (fieldType is not null)
                    return ExpandAliasType(fieldType, context.AliasTypes) ?? fieldType;
            }

            if (recordType is TypeInference.InferredType.OpenRecordType openRecord)
            {
                var fieldType =
                    openRecord.KnownFields
                    .FirstOrDefault(field => field.FieldName == recordAccess.FieldName)
                    .FieldType;

                if (fieldType is not null)
                    return ExpandAliasType(fieldType, context.AliasTypes) ?? fieldType;
            }
        }

        var inferredType =
            TypeInference.InferExpressionType(
                expression,
                context.ParameterNames,
                context.ParameterTypes,
                context.LocalBindingTypes,
                context.CurrentModuleName,
                context.FunctionTypes);

        return ExpandAliasType(inferredType, context.AliasTypes) ?? inferredType;
    }

    private static ImmutableDictionary<string, TypeInference.InferredType> ExpandAliasTypes(
        ImmutableDictionary<string, TypeInference.InferredType> inferredTypes,
        IReadOnlyDictionary<QualifiedNameRef, TypeInference.TypeAliasDefinition> aliasTypes) =>
        inferredTypes.ToImmutableDictionary(
            entry => entry.Key,
            entry => ExpandAliasType(entry.Value, aliasTypes) ?? entry.Value);

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
