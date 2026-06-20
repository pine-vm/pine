using Pine.Core.CommonEncodings;
using Pine.Core.Elm.ElmCompilerInDotnet;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Numerics;

namespace Pine.Core.Elm.ElmSyntax.ElmSyntaxAbstract;

/// <summary>
/// Syntax transformations lowering operator applications to function applications.
/// </summary>
public static class OperatorLowering
{
    /// <summary>
    /// Per-declaration context used to infer operand types when deciding whether an
    /// operator application can be lowered to an integer-specific Pine builtin.
    /// </summary>
    private record RewriteContext(
        ImmutableDictionary<string, int> ParameterNames,
        ImmutableDictionary<string, TypeInference.InferredType> ParameterTypes,
        string ModuleName,
        IReadOnlyDictionary<string, IReadOnlyList<TypeInference.InferredType>> FunctionParameterTypes,
        ImmutableDictionary<string, TypeInference.InferredType> FunctionSignatures);

    private enum BasicsOperator
    {
        Add,
        Sub,
        Mul,
        Eq,
        Neq,
        Lt,
        Gt,
        Le,
        Ge,
        And,
        Or,
    }

    /// <summary>
    /// Configuration options for operator lowering.
    /// </summary>
    /// <param name="LowerPipes">
    /// Enable lowering pipe operators like (|&gt;) and (&lt;|).
    /// </param>
    /// <param name="LowerBasicsArithmeticOperators">
    /// Enable lowering basics arithmetic operators like (+), (-), (*), (/) and (//).
    /// For operators admitting `number` operands, also use type inference to substitute the integer-specific Pine builtin functions where operands are inferred to be `Int`.
    /// </param>
    /// <param name="LowerBasicsEqualityOperators">
    /// Enable lowering basic equality operators like (==) and (!=).
    /// </param>
    /// <param name="LowerBasicsComparisonOperators">
    /// Enable lowering basic comparison operators like (&lt;), (&lt;=), (&gt;), and (&gt;=).
    /// Also use type inference to substitute the integer-specific Pine builtin functions where operands are inferred to be `Int`.
    /// </param>
    /// <param name="LowerBasicsLogicalOperators">
    /// Enable lowering basic logical operators like (&amp;&amp;) and (||).
    /// </param>
    public record Config(
        bool LowerPipes,
        bool LowerBasicsArithmeticOperators,
        bool LowerBasicsEqualityOperators,
        bool LowerBasicsComparisonOperators,
        bool LowerBasicsLogicalOperators);

    /// <summary>
    /// Rewrites the given abstract Elm syntax <see cref="File"/> by lowering operator applications to function applications.
    /// </summary>
    public static File RewriteOperators(
        File file,
        Config config)
    {
        var moduleName = GetModuleName(file);

        var functionSignatures = BuildFunctionSignatures(file, moduleName);

        var functionParameterTypes = BuildFunctionParameterTypes(file);

        var rewrittenDeclarations =
            file.Declarations
            .Select(
                declaration =>
                RewriteDeclaration(declaration, config, moduleName, functionParameterTypes, functionSignatures))
            .ToList();

        return file with { Declarations = rewrittenDeclarations };
    }

    private static string GetModuleName(File file) =>
        file.ModuleDefinition switch
        {
            Module.NormalModule normalModule => string.Join(".", normalModule.ModuleData.ModuleName),
            Module.PortModule portModule => string.Join(".", portModule.ModuleData.ModuleName),
            Module.EffectModule effectModule => string.Join(".", effectModule.ModuleData.ModuleName),

            _ =>
            "",
        };

    private static ImmutableDictionary<string, TypeInference.InferredType> BuildFunctionSignatures(
        File file,
        string moduleName)
    {
        var builder = ImmutableDictionary.CreateBuilder<string, TypeInference.InferredType>();

        foreach (var declaration in file.Declarations)
        {
            TypeInference.CollectFunctionSignaturesFromDeclaration(declaration, moduleName, builder);
        }

        return builder.ToImmutable();
    }

    /// <summary>
    /// Builds a lookup, keyed by (unqualified) function name, of the parameter types declared
    /// in each top-level function's type annotation. Used to propagate expected argument types
    /// (notably function-typed arguments such as lambdas) into nested expressions.
    /// </summary>
    private static IReadOnlyDictionary<string, IReadOnlyList<TypeInference.InferredType>> BuildFunctionParameterTypes(
        File file)
    {
        var result = new Dictionary<string, IReadOnlyList<TypeInference.InferredType>>();

        foreach (var declaration in file.Declarations)
        {
            if (declaration is not Declaration.FunctionDeclaration functionDeclaration)
                continue;

            var parameterTypes = TypeInference.GetFunctionParameterTypes(functionDeclaration.Function);

            if (parameterTypes.Count is 0)
                continue;

            result[functionDeclaration.Function.Declaration.Name] = parameterTypes;
        }

        return result;
    }

    private static Declaration RewriteDeclaration(
        Declaration declaration,
        Config config,
        string moduleName,
        IReadOnlyDictionary<string, IReadOnlyList<TypeInference.InferredType>> functionParameterTypes,
        ImmutableDictionary<string, TypeInference.InferredType> functionSignatures)
    {
        if (declaration is not Declaration.FunctionDeclaration functionDeclaration)
            return declaration;

        var function = functionDeclaration.Function;
        var implementation = function.Declaration;

        var context = BuildContext(function, moduleName, functionParameterTypes, functionSignatures);

        var rewrittenExpression =
            RewriteExpression(implementation.Expression, context, config, expectedType: null);

        return
            new Declaration.FunctionDeclaration(
                function with
                {
                    Declaration = implementation with { Expression = rewrittenExpression }
                });
    }

    private static RewriteContext BuildContext(
        FunctionStruct function,
        string moduleName,
        IReadOnlyDictionary<string, IReadOnlyList<TypeInference.InferredType>> functionParameterTypes,
        ImmutableDictionary<string, TypeInference.InferredType> functionSignatures)
    {
        var parameterNamesBuilder = ImmutableDictionary.CreateBuilder<string, int>();

        var parameterTypesBuilder =
            ImmutableDictionary.CreateBuilder<string, TypeInference.InferredType>();

        var parameterTypes = TypeInference.GetFunctionParameterTypes(function);

        var arguments = function.Declaration.Arguments;

        for (var i = 0; i < arguments.Count; i++)
        {
            if (arguments[i] is not Pattern.VarPattern varPattern)
                continue;

            parameterNamesBuilder[varPattern.Name] = i;

            if (i < parameterTypes.Count)
                parameterTypesBuilder[varPattern.Name] = parameterTypes[i];
        }

        return
            new RewriteContext(
                parameterNamesBuilder.ToImmutable(),
                parameterTypesBuilder.ToImmutable(),
                moduleName,
                functionParameterTypes,
                functionSignatures);
    }

    private static Expression RewriteExpression(
        Expression expression,
        RewriteContext context,
        Config config,
        TypeInference.InferredType? expectedType)
    {
        switch (expression)
        {
            case Expression.Application application:
                {
                    var expectedArgumentTypes = GetExpectedArgumentTypes(application, context);

                    var rewrittenArguments =
                        application.Arguments
                        .Select(
                            (argument, index) =>
                            RewriteExpression(
                                argument,
                                context,
                                config,
                                expectedArgumentTypes.ElementAtOrDefault(index)))
                        .ToList();

                    var rewrittenFunction =
                        RewriteExpression(application.Function, context, config, expectedType: null);

                    var rewrittenApplication =
                        new Expression.Application(rewrittenFunction, rewrittenArguments);

                    if (TryLowerPipe(rewrittenApplication, config) is { } loweredPipe)
                        return loweredPipe;

                    return RewriteApplication(rewrittenApplication, context, config);
                }

            case Expression.ListExpr listExpr:
                return
                    new Expression.ListExpr(
                        [
                        .. listExpr.Elements.Select(
                            element => RewriteExpression(element, context, config, expectedType: null))
                        ]);

            case Expression.TupledExpression tupled:
                return
                    new Expression.TupledExpression(
                        [
                        .. tupled.Elements.Select(
                            element => RewriteExpression(element, context, config, expectedType: null))
                        ]);

            case Expression.IfBlock ifBlock:
                return
                    new Expression.IfBlock(
                        RewriteExpression(ifBlock.Condition, context, config, expectedType: null),
                        RewriteExpression(ifBlock.ThenBlock, context, config, expectedType),
                        RewriteExpression(ifBlock.ElseBlock, context, config, expectedType));

            case Expression.Negation negation:
                return
                    new Expression.Negation(
                        RewriteExpression(negation.Expression, context, config, expectedType: null));

            case Expression.LambdaExpression lambda:
                return RewriteLambda(lambda, context, config, expectedType);

            case Expression.CaseExpression caseExpression:
                return
                    new Expression.CaseExpression(
                        RewriteExpression(caseExpression.Expression, context, config, expectedType: null),
                        [
                        .. caseExpression.Cases.Select(
                            caseItem =>
                            new Case(
                                caseItem.Pattern,
                                RewriteExpression(caseItem.Expression, context, config, expectedType)))
                        ]);

            case Expression.LetExpression letExpression:
                return
                    new Expression.LetExpression(
                        [
                        .. letExpression.Declarations.Select(letDecl => RewriteLetDeclaration(letDecl, context, config))
                        ],
                        RewriteExpression(letExpression.Expression, context, config, expectedType));

            case Expression.RecordExpr recordExpr:
                return
                    new Expression.RecordExpr(
                        [
                        .. recordExpr.Fields.Select(
                            field =>
                            field with { Value = RewriteExpression(field.Value, context, config, expectedType: null) })
                        ]);

            case Expression.RecordUpdateExpression recordUpdate:
                return
                    new Expression.RecordUpdateExpression(
                        recordUpdate.RecordName,
                        [
                        .. recordUpdate.Fields.Select(
                            field =>
                            field with { Value = RewriteExpression(field.Value, context, config, expectedType: null) })
                        ]);

            case Expression.RecordAccess recordAccess:
                return
                    recordAccess with
                    {
                        Record = RewriteExpression(recordAccess.Record, context, config, expectedType: null)
                    };

            default:
                return expression;
        }
    }

    private static Expression RewriteLambda(
        Expression.LambdaExpression lambda,
        RewriteContext context,
        Config config,
        TypeInference.InferredType? expectedType)
    {
        var inferred =
            TypeInference.InferFunctionDeclarationType(
                lambda.Expression,
                lambda.Arguments,
                context.ModuleName,
                context.FunctionSignatures);

        var parameterNamesBuilder = ImmutableDictionary.CreateBuilder<string, int>();

        for (var index = 0; index < lambda.Arguments.Count; index++)
        {
            if (lambda.Arguments[index] is Pattern.VarPattern varPattern)
                parameterNamesBuilder[varPattern.Name] = index;
        }

        var nestedContext =
            context with
            {
                ParameterNames = parameterNamesBuilder.ToImmutable(),
                ParameterTypes =
                MergeExpectedLambdaParameterTypes(
                    lambda.Arguments,
                    inferred.parameterTypes,
                    expectedType),
            };

        var bodyExpectedType =
            expectedType is TypeInference.InferredType.FunctionType functionType
            ?
            functionType.ReturnType
            :
            null;

        return
            new Expression.LambdaExpression(
                lambda.Arguments,
                RewriteExpression(lambda.Expression, nestedContext, config, bodyExpectedType));
    }

    private static ImmutableDictionary<string, TypeInference.InferredType> MergeExpectedLambdaParameterTypes(
        IReadOnlyList<Pattern> arguments,
        ImmutableDictionary<string, TypeInference.InferredType> inferredParameterTypes,
        TypeInference.InferredType? expectedType)
    {
        if (expectedType is not TypeInference.InferredType.FunctionType)
            return inferredParameterTypes;

        var mergedParameterTypes = inferredParameterTypes.ToBuilder();
        var remainingExpectedType = expectedType;

        for (var index = 0; index < arguments.Count; index++)
        {
            if (arguments[index] is not Pattern.VarPattern varPattern ||
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
            return expectedType;

        return expectedType switch
        {
            TypeInference.InferredType.UnknownType or TypeInference.InferredType.TypeVariable => inferredType,

            TypeInference.InferredType.NumberType when inferredType is TypeInference.InferredType.IntType or TypeInference.InferredType.FloatType =>
            inferredType,

            _ =>
            expectedType
        };
    }

    private static LetDeclaration RewriteLetDeclaration(
        LetDeclaration letDeclaration,
        RewriteContext context,
        Config config)
    {
        switch (letDeclaration)
        {
            case LetDeclaration.LetFunction letFunction:
                {
                    var function = letFunction.Function;

                    return
                        new LetDeclaration.LetFunction(
                            function with
                            {
                                Declaration =
                                function.Declaration with
                                {
                                    Expression =
                                    RewriteExpression(function.Declaration.Expression, context, config, expectedType: null)
                                }
                            });
                }

            case LetDeclaration.LetDestructuring letDestructuring:
                return
                    new LetDeclaration.LetDestructuring(
                        letDestructuring.Pattern,
                        RewriteExpression(letDestructuring.Expression, context, config, expectedType: null));

            default:
                return letDeclaration;
        }
    }

    /// <summary>
    /// Determines the expected parameter types for the function being applied, keyed by the
    /// (unqualified) function name. Used to propagate expected types (notably function-typed
    /// arguments such as lambdas) into the rewritten arguments.
    /// </summary>
    private static IReadOnlyList<TypeInference.InferredType?> GetExpectedArgumentTypes(
        Expression.Application application,
        RewriteContext context)
    {
        if (application.Function is not Expression.FunctionOrValue functionOrValue)
            return [];

        if (!context.FunctionParameterTypes.TryGetValue(functionOrValue.QualifiedName.DeclName, out var parameterTypes))
            return [];

        return [.. parameterTypes.Cast<TypeInference.InferredType?>()];
    }

    /// <summary>
    /// Lowers pipe operator applications to plain function applications where all arguments are
    /// given locally.
    /// <list type="bullet">
    /// <item><c>x |&gt; f</c> (canonicalized to <c>Basics.apR x f</c>) → <c>f x</c></item>
    /// <item><c>f &lt;| x</c> (canonicalized to <c>Basics.apL f x</c>) → <c>f x</c></item>
    /// </list>
    /// When the function operand is itself an application (e.g. <c>x |&gt; f a b</c>), the piped
    /// argument is appended to the existing argument list (<c>f a b x</c>), keeping all arguments
    /// local to a single application. Nested pipelines are lowered through ordinary recursion on
    /// the operands. Returns <c>null</c> when the application is not a (saturated) pipe operator.
    /// </summary>
    private static Expression? TryLowerPipe(
        Expression.Application application,
        Config config)
    {
        if (!config.LowerPipes)
            return null;

        if (application.Function is not Expression.FunctionOrValue functionOrValue)
            return null;

        if (functionOrValue.QualifiedName.Namespaces is not ["Basics"])
            return null;

        // Both pipe operators require at least two operands (the piped value and the function).
        if (application.Arguments.Count < 2)
            return null;

        switch (functionOrValue.QualifiedName.DeclName)
        {
            case "apR":
                {
                    // Basics.apR left right  →  right left (with any extra arguments appended).
                    var pipedFunction = application.Arguments[1];

                    var arguments =
                        new List<Expression>(application.Arguments.Count - 1)
                        {
                            application.Arguments[0],
                        };

                    arguments.AddRange(application.Arguments.Skip(2));

                    return ApplyPipeArguments(pipedFunction, arguments);
                }

            case "apL":
                {
                    // Basics.apL left right  →  left right (with any extra arguments appended).
                    var pipedFunction = application.Arguments[0];

                    var arguments = application.Arguments.Skip(1).ToList();

                    return ApplyPipeArguments(pipedFunction, arguments);
                }

            default:
                return null;
        }
    }

    /// <summary>
    /// Applies <paramref name="arguments"/> to <paramref name="function"/>, flattening into a
    /// single application when the function is already an application so that all arguments are
    /// given locally.
    /// </summary>
    private static Expression ApplyPipeArguments(
        Expression function,
        IReadOnlyList<Expression> arguments)
    {
        if (arguments.Count is 0)
            return function;

        if (function is Expression.Application existingApplication)
        {
            return
                new Expression.Application(
                    existingApplication.Function,
                    [.. existingApplication.Arguments, .. arguments]);
        }

        return new Expression.Application(function, arguments);
    }

    /// <summary>
    /// Lowers a canonicalized operator application (e.g. <c>Basics.add left right</c>) to the
    /// corresponding Pine builtin or short-circuiting <c>if-then-else</c> form when applicable.
    /// Applications that are not recognized operators, or whose operands cannot be proven to
    /// satisfy the required types, are returned unchanged.
    /// </summary>
    private static Expression RewriteApplication(
        Expression.Application application,
        RewriteContext context,
        Config config)
    {
        if (application.Arguments.Count is not 2)
            return application;

        if (TryMapBasicsOperator(application.Function, config) is not { } operatorKind)
            return application;

        var left = application.Arguments[0];
        var right = application.Arguments[1];

        var leftType = InferType(left, context);
        var rightType = InferType(right, context);

        switch (operatorKind)
        {
            case BasicsOperator.And:
                {
                    // Lower `a && b` to `if a then b else Basics.False`, after first attempting to
                    // merge chained integer comparisons into a single `int_is_sorted_asc` call.
                    if (TryMergeChainedIntIsSortedAsc(left, right) is { } merged)
                        return merged;

                    return new Expression.IfBlock(left, right, BuildBasicsBoolReference(value: false));
                }

            case BasicsOperator.Or:

                // Lower `a || b` to `if a then Basics.True else b`.
                return new Expression.IfBlock(left, BuildBasicsBoolReference(value: true), right);

            case BasicsOperator.Eq:
                if (ProvesIntegerBuiltin(leftType, rightType))
                    return BuildBuiltinApplication("equal", [left, right]);

                break;

            case BasicsOperator.Neq:
                if (ProvesIntegerBuiltin(leftType, rightType))
                {
                    // Lower `a /= b` to `if Pine_builtin.equal [ a, b ] then Basics.False else Basics.True`.
                    return
                        new Expression.IfBlock(
                            BuildBuiltinApplication("equal", [left, right]),
                            BuildBasicsBoolReference(value: false),
                            BuildBasicsBoolReference(value: true));
                }

                break;

            case BasicsOperator.Lt:
            case BasicsOperator.Gt:
            case BasicsOperator.Le:
            case BasicsOperator.Ge:
                if (ProvesIntegerBuiltin(leftType, rightType))
                    return BuildIntComparisonApplication(operatorKind, left, right);

                break;

            case BasicsOperator.Add:
                if (ProvesIntegerBuiltin(leftType, rightType))
                    return BuildBuiltinApplication("int_add", [left, right]);

                break;

            case BasicsOperator.Mul:
                if (ProvesIntegerBuiltin(leftType, rightType))
                    return BuildBuiltinApplication("int_mul", [left, right]);

                break;

            case BasicsOperator.Sub:
                if (ProvesIntegerBuiltin(leftType, rightType))
                {
                    return
                        BuildBuiltinApplication(
                            "int_add",
                            [left, BuildBuiltinApplication("int_mul", [BuildIntegerLiteral(-1), right])]);
                }

                break;
        }

        return application;
    }

    private static BasicsOperator? TryMapBasicsOperator(
        Expression function,
        Config config)
    {
        if (function is not Expression.FunctionOrValue functionOrValue)
            return null;

        if (functionOrValue.QualifiedName.Namespaces is not ["Basics"])
            return null;

        return functionOrValue.QualifiedName.DeclName switch
        {
            "add" when config.LowerBasicsArithmeticOperators => BasicsOperator.Add,
            "sub" when config.LowerBasicsArithmeticOperators => BasicsOperator.Sub,
            "mul" when config.LowerBasicsArithmeticOperators => BasicsOperator.Mul,
            "eq" when config.LowerBasicsEqualityOperators => BasicsOperator.Eq,
            "neq" when config.LowerBasicsEqualityOperators => BasicsOperator.Neq,
            "lt" when config.LowerBasicsComparisonOperators => BasicsOperator.Lt,
            "gt" when config.LowerBasicsComparisonOperators => BasicsOperator.Gt,
            "le" when config.LowerBasicsComparisonOperators => BasicsOperator.Le,
            "ge" when config.LowerBasicsComparisonOperators => BasicsOperator.Ge,
            "and" when config.LowerBasicsLogicalOperators => BasicsOperator.And,
            "or" when config.LowerBasicsLogicalOperators => BasicsOperator.Or,

            _ =>
            null,
        };
    }

    private static TypeInference.InferredType InferType(
        Expression expression,
        RewriteContext context) =>
        TypeInference.InferExpressionType(
            expression,
            context.ParameterNames,
            context.ParameterTypes);

    /// <summary>
    /// Returns <c>true</c> when both operands are proven to be <c>Int</c> (allowing the
    /// polymorphic <c>number</c> type to combine with a concrete <c>Int</c>).
    /// </summary>
    private static bool ProvesIntegerBuiltin(
        TypeInference.InferredType leftType,
        TypeInference.InferredType rightType) =>
        (leftType is TypeInference.InferredType.IntType &&
        rightType is TypeInference.InferredType.IntType or TypeInference.InferredType.NumberType) ||
        (rightType is TypeInference.InferredType.IntType &&
        leftType is TypeInference.InferredType.IntType or TypeInference.InferredType.NumberType);

    /// <summary>
    /// Builds an <c>int_is_sorted_asc</c> application for an Int comparison operator.
    /// <list type="bullet">
    /// <item><c>a &lt;= b</c> → <c>int_is_sorted_asc [ a, b ]</c></item>
    /// <item><c>a &gt;= b</c> → <c>int_is_sorted_asc [ b, a ]</c></item>
    /// <item><c>a &lt; b</c>  → <c>int_is_sorted_asc [ int_add [ a, 1 ], b ]</c> (with literal folding)</item>
    /// <item><c>a &gt; b</c>  → <c>int_is_sorted_asc [ int_add [ b, 1 ], a ]</c> (with literal folding)</item>
    /// </list>
    /// </summary>
    private static Expression BuildIntComparisonApplication(
        BasicsOperator operatorKind,
        Expression left,
        Expression right)
    {
        var isStrict = operatorKind is BasicsOperator.Lt or BasicsOperator.Gt;
        var swapOperands = operatorKind is BasicsOperator.Gt or BasicsOperator.Ge;

        var (first, second) = swapOperands ? (right, left) : (left, right);

        if (isStrict)
            return BuildStrictIntIsSortedAscApplication(first, second);

        return BuildIntIsSortedAscApplication([first, second]);
    }

    /// <summary>
    /// Builds a strict integer comparison (<c>&lt;</c> / <c>&gt;</c>) using <c>int_is_sorted_asc</c>
    /// with an offset of +1 on the first operand. Since <c>int_is_sorted_asc</c> checks <c>&lt;=</c>,
    /// strict <c>a &lt; b</c> becomes <c>a + 1 &lt;= b</c>. When either operand is an integer literal,
    /// the offset is folded into the literal to avoid emitting <c>int_add</c>.
    /// </summary>
    private static Expression BuildStrictIntIsSortedAscApplication(
        Expression first,
        Expression second)
    {
        if (TryGetIntegerLiteralValue(first) is { } firstLiteral)
            return BuildIntIsSortedAscApplication([BuildIntegerLiteral(firstLiteral + 1), second]);

        if (TryGetIntegerLiteralValue(second) is { } secondLiteral)
            return BuildIntIsSortedAscApplication([first, BuildIntegerLiteral(secondLiteral - 1)]);

        var offsetFirst = BuildBuiltinApplication("int_add", [first, BuildIntegerLiteral(1)]);

        return BuildIntIsSortedAscApplication([offsetFirst, second]);
    }

    private static Expression BuildIntIsSortedAscApplication(
        IReadOnlyList<Expression> operands) =>
        new Expression.Application(
            Expression.FunctionOrValue.Create(["Pine_builtin"], "int_is_sorted_asc"),
            [new Expression.ListExpr([.. operands])]);

    /// <summary>
    /// Tries to merge two <c>int_is_sorted_asc</c> applications connected by <c>&amp;&amp;</c>
    /// into a single call when they share a common middle operand.
    /// <para>
    /// For non-strict comparisons (<c>&lt;=</c>), when the last element of the left list equals
    /// the first element of the right list, they are merged by removing the duplicate:
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
    private static Expression? TryMergeChainedIntIsSortedAsc(
        Expression leftExpr,
        Expression rightExpr)
    {
        if (TryExtractIntIsSortedAscOperands(leftExpr) is not { } leftOperands ||
            TryExtractIntIsSortedAscOperands(rightExpr) is not { } rightOperands)
        {
            return null;
        }

        if (leftOperands.Count is 0 || rightOperands.Count is 0)
            return null;

        var leftLast = leftOperands[^1];
        var rightFirst = rightOperands[0];

        // Case 1: Exact match on shared middle operand (e.g., <= chains).
        if (leftLast.Equals(rightFirst))
        {
            var mergedOperands = new List<Expression>(leftOperands.Count + rightOperands.Count - 1);
            mergedOperands.AddRange(leftOperands);

            for (var i = 1; i < rightOperands.Count; i++)
                mergedOperands.Add(rightOperands[i]);

            return BuildIntIsSortedAscApplication(mergedOperands);
        }

        // Case 2: Strict chain where rightFirst is int_add [leftLast, 1] (e.g., < chains).
        if (IsIntAddOffsetByOne(leftLast, rightFirst))
        {
            var mergedOperands = new List<Expression>(leftOperands.Count + rightOperands.Count);
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
        Expression baseExpr,
        Expression candidate)
    {
        if (candidate is not Expression.Application app ||
            app.Function is not Expression.FunctionOrValue fv ||
            fv.QualifiedName.Namespaces is not ["Pine_builtin"] ||
            fv.QualifiedName.DeclName is not "int_add" ||
            app.Arguments.Count is not 1 ||
            app.Arguments[0] is not Expression.ListExpr listExpr ||
            listExpr.Elements.Count is not 2)
        {
            return false;
        }

        return
            (listExpr.Elements[0].Equals(baseExpr) && IsIntegerLiteral(listExpr.Elements[1], 1)) ||
            (listExpr.Elements[1].Equals(baseExpr) && IsIntegerLiteral(listExpr.Elements[0], 1));
    }

    /// <summary>
    /// Extracts the operand list from an <c>int_is_sorted_asc</c> application,
    /// or returns null if the expression is not such an application.
    /// </summary>
    private static IReadOnlyList<Expression>? TryExtractIntIsSortedAscOperands(
        Expression expression)
    {
        if (expression is not Expression.Application application ||
            application.Function is not Expression.FunctionOrValue functionOrValue ||
            functionOrValue.QualifiedName.Namespaces is not ["Pine_builtin"] ||
            functionOrValue.QualifiedName.DeclName is not "int_is_sorted_asc" ||
            application.Arguments.Count is not 1 ||
            application.Arguments[0] is not Expression.ListExpr listExpr)
        {
            return null;
        }

        return listExpr.Elements;
    }

    private static bool IsIntegerLiteral(Expression expression, BigInteger value) =>
        expression is Expression.Integer integer && integer.Value == value;

    private static BigInteger? TryGetIntegerLiteralValue(Expression expression) =>
        expression switch
        {
            Expression.Integer integer => integer.Value,
            Expression.Negation { Expression: Expression.Integer negated } => -negated.Value,

            _ =>
            null,
        };

    private static Expression BuildBuiltinApplication(
        string builtinName,
        IReadOnlyList<Expression> operands) =>
        new Expression.Application(
            Expression.FunctionOrValue.Create(["Pine_builtin"], builtinName),
            [new Expression.ListExpr(operands)]);

    /// <summary>
    /// Builds a reference to <c>Basics.True</c> or <c>Basics.False</c>, used by the logical
    /// operator lowerings that translate <c>&amp;&amp;</c> / <c>||</c> (and the equivalent
    /// <c>Basics.and</c> / <c>Basics.or</c> applications) into <c>if-then-else</c> expressions.
    /// </summary>
    private static Expression BuildBasicsBoolReference(bool value) =>
        Expression.FunctionOrValue.Create(["Basics"], value ? "True" : "False");

    private static Expression BuildIntegerLiteral(BigInteger value) =>
        new Expression.Integer(value, IntegerEncoding.EncodeSignedInteger(value));
}
