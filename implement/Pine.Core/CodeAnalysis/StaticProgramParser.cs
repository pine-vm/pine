using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

namespace Pine.Core.CodeAnalysis;

/// <summary>
/// Parse a set of Pine values into a static program representation.
/// Since Pine is an inherently dynamic language, not every Pine program can be mapped to a static program.
/// <para>
/// For any questions about the design or implementation, see the design notes in 'parsing-as-a-static-program.md'
/// </para>
/// </summary>
public class StaticProgramParser
{
    /// <summary>
    /// Describes the origin of a crash that happens if the program takes the containing branch at runtime.
    /// </summary>
    public abstract record CrashOrigin
    {
        /// <summary>
        /// Originating from a <see cref="Expression.ParseAndEval"/> proven to never evaluate to a valid encoded expression.
        /// </summary>
        public sealed record CrashFromParseAndEval
            : CrashOrigin;

        /// <summary>
        /// Originating from an invocation proven to lead to infinite recursion.
        /// </summary>
        public sealed record CrashInfiniteRecursion
            : CrashOrigin;
    }

    /// <summary>
    /// Represents an error that occurred during parsing of a static program.
    /// Contains a descriptive message and the path through identifiers where the error occurred.
    /// </summary>
    /// <typeparam name="IdentifierT">Type of the identifier used in the program.</typeparam>
    /// <param name="Message">A descriptive message explaining the parsing error.</param>
    /// <param name="Path">The path of identifiers from the root to where the error occurred, enabling error localization.</param>
    public record ParseError<IdentifierT>(
        string Message,
        ImmutableStack<IdentifierT> Path);

    /// <summary>
    /// </summary>
    /// <param name="Ident">
    /// Identifier to represent occurences of the identified value in the resulting static program model.
    /// </param>
    /// <param name="ContinueParse">
    /// Whether also to parse the contained function and add further declarations found in there to the overall static program model.
    /// </param>
    /// <param name="OriginalFunctionValue">
    /// Optional original function declaration value to use when parsing if ContinueParse is true.
    /// This is needed when the identified value is an extracted inner value (e.g., from BuildApplicationFromFunctionRecord)
    /// but the full function declaration with parameter wrappers is needed for proper parsing.
    /// If null, the identified value itself is used for parsing.
    /// </param>
    public record IdentifyResponse<IdentifierT>(
        IdentifierT Ident,
        bool ContinueParse,
        PineValue? OriginalFunctionValue = null);

    /// <summary>
    /// Represents a parsed function extracted from a Pine value.
    /// Contains the parameter expressions and the function body expression.
    /// </summary>
    /// <typeparam name="IdentifierT">Type of the identifier used in the program.</typeparam>
    /// <param name="ParametersExprs">
    /// List of expressions representing the function's parameters.
    /// These are used to identify references to parameters in the body.
    /// </param>
    /// <param name="BodyExpression">
    /// The expression representing the function's body after parsing.
    /// </param>
    public record ParsedFunction<IdentifierT>(
        IReadOnlyList<StaticExpression<IdentifierT>> ParametersExprs,
        StaticExpression<IdentifierT> BodyExpression);

    /// <summary>
    /// Parse a set of Pine values into a static program representation.
    /// </summary>
    public static Result<ParseError<IdentifierT>, IReadOnlyDictionary<IdentifierT, ParsedFunction<IdentifierT>>>
        ParseProgram<IdentifierT>(
        IReadOnlyDictionary<IdentifierT, PineValue> roots,
        StaticProgramParserConfig<IdentifierT> parseConfig,
        PineVMParseCache parseCache)
        where IdentifierT : notnull
    {
        var processed =
            new Dictionary<IdentifierT, ParsedFunction<IdentifierT>>();

        var toProcess =
            new Queue<(IdentifierT ident, PineValue value, ImmutableStack<IdentifierT> path)>();

        foreach (var kvp in roots)
        {
            toProcess.Enqueue((kvp.Key, kvp.Value, ImmutableStack<IdentifierT>.Empty));
        }

        while (toProcess.Count > 0)
        {
            var (ident, value, path) = toProcess.Dequeue();

            if (processed.ContainsKey(ident))
            {
                continue;
            }

            var parseExprResult =
                ParseValueAsFunction(
                    path,
                    value,
                    parseConfig,
                    parseCache);

            if (parseExprResult.IsErrOrNull() is { } exprErr)
            {
                return
                    new ParseError<IdentifierT>(
                        exprErr,
                        path.Push(ident));
            }

            if (parseExprResult.IsOkOrNullable() is not { } parseOk)
            {
                throw new NotImplementedException(
                    "Unexpected parse result type: " + parseExprResult.GetType().FullName);
            }

            processed[ident] = parseOk.current;

            foreach (var dep in parseOk.dependencies)
            {
                toProcess.Enqueue((dep.Key, dep.Value, path.Push(ident)));
            }
        }

        return processed.ToImmutableDictionary();
    }

    private static Result<string, (ParsedFunction<IdentifierT> current, ImmutableDictionary<IdentifierT, PineValue> dependencies)>
        ParseValueAsFunction<IdentifierT>(
        ImmutableStack<IdentifierT> path,
        PineValue functionValue,
        StaticProgramParserConfig<IdentifierT> parseConfig,
        PineVMParseCache parseCache)
        where IdentifierT : notnull
    {
        var parsePineExprResult =
            FunctionRecord.ParseFunctionRecordTagged(functionValue, parseCache);

        if (parsePineExprResult.IsErrOrNull() is { } parseErr)
        {
            /*
            return
                new ParseError<IdentifierT>(
                    parseErr,
                    path.Push(ident));
            */

            /*
             * The new Elm compiler emits declaration without parameters directly as values in the module,
             * without wrapping them in an encoded expression.
             * */

            return (new ParsedFunction<IdentifierT>([], StaticExpression<IdentifierT>.LiteralInstance(functionValue)), []);
        }

        if (parsePineExprResult.IsOkOrNull() is not { } parsedFunction)
        {
            throw new NotImplementedException(
                "Unexpected parse result type: " + parsePineExprResult.GetType().FullName);
        }

        // Build the environment class using the same logic as the old parser.
        // This handles the case where the inner function structure may have indirection
        // (e.g., inner function is a ParseAndEval that references env[0][0]).
        var (_, innerExpr, envClass) =
            NamesFromCompiledEnv.BuildApplicationFromFunctionRecord(parsedFunction, arguments: [], parseCache);

        var parseBodyExprResult =
            ParseExpression(
                path,
                innerExpr,
                envClass,
                parseConfig,
                parseCache);

        if (parseBodyExprResult.IsErrOrNull() is { } exprErr)
        {
            return exprErr;
        }

        if (parseBodyExprResult.IsOkOrNullable() is not { } parseBodyOk)
        {
            throw new NotImplementedException(
                "Unexpected parse result type: " + parseBodyExprResult.GetType().FullName);
        }

        /*
         * Construction of these parameter expressions is currently only a guess, based on the layout emitted by the previous Elm compiler.
         * TODO: Probably these should come from the function value parser (e.g. FunctionRecord.ParseFunctionRecordTagged) to maintain consistency.
         * */

        var parametersExprs =
            Enumerable.Range(0, parsedFunction.ParameterCount)
            .Select(paramIndex => StaticExpressionExtension.BuildPathToExpression([1, paramIndex], StaticExpression<IdentifierT>.EnvironmentInstance))
            .ToArray();

        return (new ParsedFunction<IdentifierT>(parametersExprs, parseBodyOk.current), parseBodyOk.dependencies);
    }

    private static Result<string, (StaticExpression<IdentifierT> current, ImmutableDictionary<IdentifierT, PineValue> dependencies)>
        ParseExpression<IdentifierT>(
        ImmutableStack<IdentifierT> path,
        Expression expression,
        PineValueClass envClass,
        StaticProgramParserConfig<IdentifierT> parseConfig,
        PineVMParseCache parseCache)
        where IdentifierT : notnull
    {
        if (expression is Expression.Literal literal)
        {
            if (parseConfig.IdentifyInstanceOptional(path, literal.Value) is { } identifyResult)
            {
                var node =
                    StaticExpression<IdentifierT>.FunctionApplicationInstance(
                        functionName: identifyResult.Ident,
                        arguments: StaticExpression<IdentifierT>.ListInstance([]));

                var dependencies =
                    identifyResult.ContinueParse
                    ?
                    ImmutableDictionary<IdentifierT, PineValue>.Empty.Add(identifyResult.Ident, literal.Value)
                    :
                    [];

                return (node, dependencies);
            }

            return (StaticExpression<IdentifierT>.LiteralInstance(literal.Value), ImmutableDictionary<IdentifierT, PineValue>.Empty);
        }

        if (CodeAnalysis.TryParseExprAsPathInEnv(expression) is { } pathInEnv)
        {
            if (pathInEnv.Count > 1 && pathInEnv[0] is 1)
            {
                var parameterIndex = pathInEnv[1];

                if (pathInEnv.Count is 2)
                {
                    // Simple parameter reference like env[1][0]
                    var parameterExpr =
                        StaticExpression<IdentifierT>.ParameterReference(parameterIndex);

                    return (parameterExpr, ImmutableDictionary<IdentifierT, PineValue>.Empty);
                }

                // For tuple patterns, nested tuple patterns, or choice type deconstruction patterns
                // like env[1][0][0] or env[1][0][1][0], build a full path expression
                // that will be rendered as param_1_0[0] or param_1_0[1][0].
                // Examples:
                // - Tuple: `(x, y)` generates env[1][0][0] and env[1][0][1]
                // - Choice: `(TagAlfa a)` generates env[1][0][1][0]
                var pathExpr =
                    StaticExpressionExtension.BuildPathToExpression(
                        pathInEnv,
                        StaticExpression<IdentifierT>.EnvironmentInstance);

                return (pathExpr, ImmutableDictionary<IdentifierT, PineValue>.Empty);
            }
        }

        if (expression is Expression.List listExpr)
        {
            var items = new StaticExpression<IdentifierT>[listExpr.Items.Count];
            var allDependencies = ImmutableDictionary<IdentifierT, PineValue>.Empty;

            for (var i = 0; i < listExpr.Items.Count; i++)
            {
                var itemExpr = listExpr.Items[i];

                var parseItemResult =
                    ParseExpression(
                        path,
                        itemExpr,
                        envClass,
                        parseConfig,
                        parseCache);

                if (parseItemResult.IsErrOrNull() is { } itemErr)
                {
                    return itemErr;
                }

                if (parseItemResult.IsOkOrNullable() is not { } itemOk)
                {
                    throw new NotImplementedException(
                        "Unexpected parse result type: " + parseItemResult.GetType().FullName);
                }

                items[i] = itemOk.current;

                allDependencies =
                    allDependencies.AddRange(itemOk.dependencies);
            }

            var listNode = StaticExpression<IdentifierT>.ListInstance(items);

            return (listNode, allDependencies);
        }

        if (expression is Expression.KernelApplication kernelApp)
        {
            var parseInputResult =
                 ParseExpression(
                     path,
                     kernelApp.Input,
                     envClass,
                     parseConfig,
                     parseCache);

            if (parseInputResult.IsErrOrNull() is { } inputErr)
            {
                return inputErr;
            }

            if (parseInputResult.IsOkOrNullable() is not { } inputOk)
            {
                throw new NotImplementedException(
                    "Unexpected parse result type: " + parseInputResult.GetType().FullName);
            }

            var appNode =
                StaticExpression<IdentifierT>.KernelApplicationInstance(
                    kernelApp.Function,
                    inputOk.current);

            return (appNode, inputOk.dependencies);
        }

        if (expression is Expression.Conditional conditionalExpr)
        {
            var parseConditionResult =
                ParseExpression(
                    path,
                    conditionalExpr.Condition,
                    envClass,
                    parseConfig,
                    parseCache);

            if (parseConditionResult.IsErrOrNull() is { } conditionErr)
            {
                return conditionErr;
            }

            if (parseConditionResult.IsOkOrNullable() is not { } conditionOk)
            {
                throw new NotImplementedException(
                    "Unexpected parse result type: " + parseConditionResult.GetType().FullName);
            }

            var parseTrueBranchResult =
                ParseExpression(
                    path,
                    conditionalExpr.TrueBranch,
                    envClass,
                    parseConfig,
                    parseCache);

            if (parseTrueBranchResult.IsErrOrNull() is { } trueBranchErr)
            {
                return trueBranchErr;
            }

            if (parseTrueBranchResult.IsOkOrNullable() is not { } trueBranchOk)
            {
                throw new NotImplementedException(
                    "Unexpected parse result type: " + parseTrueBranchResult.GetType().FullName);
            }

            var parseFalseBranchResult =
                ParseExpression(
                    path,
                    conditionalExpr.FalseBranch,
                    envClass,
                    parseConfig,
                    parseCache);

            if (parseFalseBranchResult.IsErrOrNull() is { } falseBranchErr)
            {
                return falseBranchErr;
            }

            if (parseFalseBranchResult.IsOkOrNullable() is not { } falseBranchOk)
            {
                throw new NotImplementedException(
                    "Unexpected parse result type: " + parseFalseBranchResult.GetType().FullName);
            }

            var conditionalNode =
                StaticExpression<IdentifierT>.ConditionalInstance(
                    condition: conditionOk.current,
                    trueBranch: trueBranchOk.current,
                    falseBranch: falseBranchOk.current);

            var allDependencies =
                conditionOk.dependencies
                .AddRange(trueBranchOk.dependencies)
                .AddRange(falseBranchOk.dependencies);

            return (conditionalNode, allDependencies);
        }

        if (expression is Expression.ParseAndEval parseAndEvalExpr)
        {
            return ParseCurriedFunctionApplication(
                path,
                parseAndEvalExpr,
                envClass,
                parseConfig,
                parseCache);
        }

        if (expression is Expression.Environment)
        {
            /*
             * In this parser, we must map all references to the environment either to a parameter or to an identifier.
             * 
            return (StaticExpression<IdentifierT>.EnvironmentInstance, ImmutableDictionary<IdentifierT, PineValue>.Empty);
            */

            return "Encountered environment reference without resolution to a parameter or identifier.";
        }

        throw new NotImplementedException(
            "Unexpected expression type: " + expression.GetType().FullName);
    }

    /// <summary>
    /// Parses curried function application by collecting all arguments from nested ParseAndEval expressions.
    /// Pattern: ParseAndEval(ParseAndEval(..., arg1), arg2) represents (f arg1) arg2
    /// </summary>
    private static Result<string, (StaticExpression<IdentifierT> current, ImmutableDictionary<IdentifierT, PineValue> dependencies)>
        ParseCurriedFunctionApplication<IdentifierT>(
        ImmutableStack<IdentifierT> path,
        Expression.ParseAndEval parseAndEvalExpr,
        PineValueClass envClass,
        StaticProgramParserConfig<IdentifierT> parseConfig,
        PineVMParseCache parseCache)
        where IdentifierT : notnull
    {
        // Collect arguments by traversing nested ParseAndEval from outermost to innermost.
        // Example: ((f arg1) arg2) is represented as ParseAndEval(ParseAndEval(Literal(f), arg1), arg2)
        // We collect [arg2, arg1] and then reverse to get [arg1, arg2] for the function application.
        var collectedArgs = new List<Expression>();
        var currentExpr = parseAndEvalExpr;

        while (currentExpr.Encoded is Expression.ParseAndEval nestedParseAndEval)
        {
            collectedArgs.Add(currentExpr.Environment);
            currentExpr = nestedParseAndEval;
        }

        // Add the innermost argument
        collectedArgs.Add(currentExpr.Environment);

        // Try to resolve the function being called
        var resolveFunctionResult =
            ResolveFunctionFromEncoded(
                currentExpr.Encoded,
                currentExpr.Environment,
                envClass,
                parseCache);

        if (resolveFunctionResult.IsErrOrNull() is { } resolveErr)
        {
            return resolveErr;
        }

        if (resolveFunctionResult.IsOkOrNullable() is not { } resolvedFunction)
        {
            throw new NotImplementedException(
                "Unexpected resolve result type: " + resolveFunctionResult.GetType().FullName);
        }

        // Reached the innermost level with the actual function reference
        var identifyFunctionResult =
            parseConfig.IdentifyInstanceRequired(
                path,
                resolvedFunction.functionValue);

        // For full function applications, parse arguments from the environment structure
        if (resolvedFunction.isFullApplication)
        {
            // Full application: environment is [[functions], [arguments]]
            // Parse the arguments from environment[1]
            var parseArgsResult =
                ParseFullApplicationArguments(
                    path,
                    currentExpr.Environment,
                    envClass,
                    parseConfig,
                    parseCache);

            if (parseArgsResult.IsErrOrNull() is { } argsErr)
            {
                return argsErr;
            }

            if (parseArgsResult.IsOkOrNullable() is not { } argsOk)
            {
                throw new NotImplementedException(
                    "Unexpected parse result type: " + parseArgsResult.GetType().FullName);
            }

            var fullAppNode =
                StaticExpression<IdentifierT>.FunctionApplicationInstance(
                    functionName: identifyFunctionResult.Ident,
                    arguments: StaticExpression<IdentifierT>.ListInstance(argsOk.parsedArgs));

            var fullAppDeps = argsOk.dependencies;

            if (identifyFunctionResult.ContinueParse)
            {
                // Use OriginalFunctionValue if provided, otherwise fall back to resolved value
                var valueForParsing = identifyFunctionResult.OriginalFunctionValue ?? resolvedFunction.functionValue;
                fullAppDeps =
                    fullAppDeps.Add(identifyFunctionResult.Ident, valueForParsing);
            }

            return (fullAppNode, fullAppDeps);
        }

        // Curried/partial application: arguments are collected from nested ParseAndEval
        // Reverse to get arguments in application order (innermost/first argument first)
        collectedArgs.Reverse();

        // Parse all collected arguments
        var parsedArgs = new StaticExpression<IdentifierT>[collectedArgs.Count];
        var allDependencies = ImmutableDictionary<IdentifierT, PineValue>.Empty;

        for (var i = 0; i < collectedArgs.Count; i++)
        {
            var parseArgumentResult =
                ParseExpression(
                    path,
                    collectedArgs[i],
                    envClass,
                    parseConfig,
                    parseCache);

            if (parseArgumentResult.IsErrOrNull() is { } argErr)
            {
                return "Failed to parse ParseAndEval environment: " + argErr;
            }

            if (parseArgumentResult.IsOkOrNullable() is not { } argOk)
            {
                throw new NotImplementedException(
                    "Unexpected parse result type: " + parseArgumentResult.GetType().FullName);
            }

            parsedArgs[i] = argOk.current;
            allDependencies = allDependencies.AddRange(argOk.dependencies);
        }

        var node =
            StaticExpression<IdentifierT>.FunctionApplicationInstance(
                functionName: identifyFunctionResult.Ident,
                arguments: StaticExpression<IdentifierT>.ListInstance(parsedArgs));

        if (identifyFunctionResult.ContinueParse)
        {
            // Use OriginalFunctionValue if provided, otherwise fall back to resolved value
            var valueForParsing = identifyFunctionResult.OriginalFunctionValue ?? resolvedFunction.functionValue;
            allDependencies =
                allDependencies.Add(identifyFunctionResult.Ident, valueForParsing);
        }

        return (node, allDependencies);
    }

    /// <summary>
    /// Resolves the function being called from the encoded expression.
    /// Handles both literal function values and path expressions that reference functions in the environment.
    /// </summary>
    private static Result<string, (PineValue functionValue, bool isFullApplication)>
        ResolveFunctionFromEncoded(
        Expression encoded,
        Expression environment,
        PineValueClass envClass,
        PineVMParseCache parseCache)
    {
        // Case 1: Encoded is a literal - direct function reference
        if (encoded is Expression.Literal encodedLiteral)
        {
            return (encodedLiteral.Value, isFullApplication: false);
        }

        // Case 2: Encoded is a path expression like env[0][i] - function from environment
        // This is used for full function applications where environment is [[functions], [arguments]]
        if (CodeAnalysis.TryParseExprAsPathInEnv(encoded) is { } pathInEnv)
        {
            // Path must start with 0 to reference the functions list
            if (pathInEnv.Count >= 2 && pathInEnv[0] is 0)
            {
                // Try to look up the function value from the environment class
                // This handles self-recursion where env[0][i] refers to a known function
                if (envClass.TryGetValue(pathInEnv) is { } functionValue)
                {
                    return (functionValue, isFullApplication: true);
                }

                // Look up the function from the environment expression
                // Environment structure: [[function_values], [arguments]]
                // We need to get environment[0][functionIndex]
                var functionIndex = pathInEnv[1];

                var functionExprResult =
                    TryGetExpressionAtPath(environment, [0, functionIndex]);

                if (functionExprResult is { } functionExpr)
                {
                    // The function expression should be resolvable to a literal
                    // It could be another path expression (for recursion) or a literal

                    // Try to evaluate it independently
                    var evalResult =
                        ReducePineExpression.TryEvaluateExpressionIndependent(
                            functionExpr,
                            parseCache);

                    if (evalResult.IsOkOrNull() is { } evalFunctionValue)
                    {
                        return (evalFunctionValue, isFullApplication: true);
                    }

                    // If we can't evaluate independently, check if it's a path that references
                    // a function we already know about (self-recursion case)
                    if (CodeAnalysis.TryParseExprAsPathInEnv(functionExpr) is { } nestedPath &&
                        envClass.TryGetValue(nestedPath) is { } nestedFunctionValue)
                    {
                        return (nestedFunctionValue, isFullApplication: true);
                    }

                    return "Could not resolve function expression at path [" +
                           string.Join(",", pathInEnv) + "]: " + functionExpr.GetType().Name;
                }

                return "Could not find expression at path [0, " + functionIndex + "] in environment";
            }
        }

        return "ParseAndEval encoded is not a Literal or path to function: " +
               encoded.GetType().FullName;
    }

    /// <summary>
    /// Tries to get the expression at the given path within a list expression structure.
    /// </summary>
    private static Expression? TryGetExpressionAtPath(Expression expr, IReadOnlyList<int> path)
    {
        var current = expr;

        for (var i = 0; i < path.Count; i++)
        {
            if (current is not Expression.List listExpr)
            {
                return null;
            }

            var index = path[i];

            if (index < 0 || index >= listExpr.Items.Count)
            {
                return null;
            }

            current = listExpr.Items[index];
        }

        return current;
    }

    /// <summary>
    /// Parses arguments from a full function application environment.
    /// Environment structure: [[functions], [arguments]]
    /// </summary>
    private static Result<string, (StaticExpression<IdentifierT>[] parsedArgs, ImmutableDictionary<IdentifierT, PineValue> dependencies)>
        ParseFullApplicationArguments<IdentifierT>(
        ImmutableStack<IdentifierT> path,
        Expression environment,
        PineValueClass envClass,
        StaticProgramParserConfig<IdentifierT> parseConfig,
        PineVMParseCache parseCache)
        where IdentifierT : notnull
    {
        // Environment is [[functions], [arguments]]
        // We need to parse environment[1] which contains the arguments list

        if (environment is not Expression.List envList || envList.Items.Count < 2)
        {
            return "Full application environment is not a list with at least 2 items";
        }

        var argsListExpr = envList.Items[1];

        if (argsListExpr is not Expression.List argsList)
        {
            return "Arguments portion of environment is not a list";
        }

        var parsedArgs = new StaticExpression<IdentifierT>[argsList.Items.Count];
        var allDependencies = ImmutableDictionary<IdentifierT, PineValue>.Empty;

        for (var i = 0; i < argsList.Items.Count; i++)
        {
            var parseArgResult =
                ParseExpression(
                    path,
                    argsList.Items[i],
                    envClass,
                    parseConfig,
                    parseCache);

            if (parseArgResult.IsErrOrNull() is { } argErr)
            {
                return "Failed to parse argument " + i + ": " + argErr;
            }

            if (parseArgResult.IsOkOrNullable() is not { } argOk)
            {
                throw new NotImplementedException(
                    "Unexpected parse result type: " + parseArgResult.GetType().FullName);
            }

            parsedArgs[i] = argOk.current;
            allDependencies = allDependencies.AddRange(argOk.dependencies);
        }

        return (parsedArgs, allDependencies);
    }
}
