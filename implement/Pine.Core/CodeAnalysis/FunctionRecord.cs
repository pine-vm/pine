using Pine.Core.CommonEncodings;
using System;

namespace Pine.Core.CodeAnalysis;

/// <summary>
/// Structured representation of a (possibly curried) Elm function at runtime.
/// </summary>
/// <param name="InnerFunction">Expression body implementing the function.</param>
/// <param name="ParameterCount">Total number of parameters expected.</param>
/// <param name="EnvFunctions">Captured function values used by the closure.</param>
/// <param name="ArgumentsAlreadyCollected">Arguments already supplied (for partial application scenarios).</param>
public record FunctionRecord(
    Expression InnerFunction,
    int ParameterCount,
    ReadOnlyMemory<PineValue> EnvFunctions,
    ReadOnlyMemory<PineValue> ArgumentsAlreadyCollected)
{
    /// <summary>
    /// Analog to the 'parseFunctionRecordFromValueTagged' function in FirCompiler.elm.
    /// Accepts either a tagged value ("Function"), a nested wrapper form from 
    /// <see cref="FunctionValueBuilder.EmitFunctionValue"/>, or a raw value (zero-argument function literal shortcut).
    /// </summary>
    /// <param name="pineValue">Encoded value representing a function (possibly tagged).</param>
    /// <param name="parseCache">Cache used to parse the inner expression.</param>
    /// <returns>Parsed <see cref="FunctionRecord"/> or error description.</returns>
    public static Result<string, FunctionRecord> ParseFunctionRecordTagged(
        PineValue pineValue,
        PineVMParseCache parseCache)
    {
        var parseTaggedResult = ElmInteractiveEnvironment.ParseTagged(pineValue);

        if (parseTaggedResult.IsOkOrNullable() is { } taggedFunctionDeclaration)
        {
            if (taggedFunctionDeclaration.name is "Function")
            {
                return ParseFunctionRecord(taggedFunctionDeclaration.value, parseCache);
            }

            // Check for nested wrapper form (ParseAndEval expression for 0/1 params)
            if (taggedFunctionDeclaration.name is "ParseAndEval")
            {
                return ParseNestedWrapperForm(pineValue, parseCache);
            }

            // Check for multi-parameter nested wrapper form (List expression that builds encoding dynamically)
            if (taggedFunctionDeclaration.name is "List")
            {
                return ParseMultiParamNestedWrapperForm(pineValue, parseCache);
            }
        }

        if (parseTaggedResult.IsErrOrNull() is { } err)
        {
            return "Failed to parse tagged function record: " + err;
        }

        /*
         * If the declaration has zero parameters, it could be encoded as plain value without wrapping in a 'Function' record.
         * */

        return
            new FunctionRecord(
                InnerFunction: Expression.LiteralInstance(pineValue),
                ParameterCount: 0,
                EnvFunctions: ReadOnlyMemory<PineValue>.Empty,
                ArgumentsAlreadyCollected: ReadOnlyMemory<PineValue>.Empty);
    }

    /// <summary>
    /// Parses a function record from the nested wrapper form emitted by 
    /// <see cref="FunctionValueBuilder.EmitFunctionValue"/> for 0 or 1 parameter functions.
    /// </summary>
    private static Result<string, FunctionRecord> ParseNestedWrapperForm(
        PineValue encodedWrapper,
        PineVMParseCache parseCache)
    {
        // For 0 params: ParseAndEval(innerExpr, [envFuncs, []])
        // For 1 param: ParseAndEval(innerExpr, [envFuncsLiteral, [Environment]])

        var parseExprResult = parseCache.ParseExpression(encodedWrapper);

        if (parseExprResult.IsErrOrNull() is { } parseErr)
        {
            return "Failed to parse nested wrapper expression: " + parseErr;
        }

        if (parseExprResult.IsOkOrNull() is not Expression.ParseAndEval outerParseAndEval)
        {
            return "Nested wrapper must be a ParseAndEval expression";
        }

        // The inner expression is encoded as a literal in the 'encoded' field
        if (outerParseAndEval.Encoded is not Expression.Literal encodedLiteral)
        {
            return "Expected Literal in ParseAndEval.Encoded";
        }

        var innerExprResult = parseCache.ParseExpression(encodedLiteral.Value);

        if (innerExprResult.IsErrOrNull() is { } innerErr)
        {
            return "Failed to parse inner expression: " + innerErr;
        }

        if (innerExprResult.IsOkOrNull() is not { } innerExpression)
        {
            return "Failed to extract inner expression";
        }

        // Extract env functions from the environment structure
        // Environment structure: [envFuncsExpr, argsExpr]
        if (outerParseAndEval.Environment is not Expression.List envList || envList.Items.Count != 2)
        {
            return "Expected environment to be a list of 2 elements";
        }

        // Parse env functions - must be one of:
        // - Expression.Literal containing a PineValue.ListValue (single literal with list)
        // - Expression.List of Expression.Literal items (list of individual literals)
        var envFunctionsResult = ParseEnvFunctionsFromExpression(envList.Items[0]);

        if (envFunctionsResult.IsErrOrNull() is { } envFuncsErr)
        {
            return "Failed to parse env functions: " + envFuncsErr;
        }

        if (envFunctionsResult.IsOkOrNull() is not { } envFunctions)
        {
            return "Failed to extract env functions";
        }

        // Determine parameter count - must match exact expected forms
        var argsExpr = envList.Items[1];

        if (argsExpr is not Expression.List argsList)
        {
            return "Expected args expression to be a List";
        }

        int parameterCount;

        if (argsList.Items.Count is 0)
        {
            parameterCount = 0;
        }
        else if (argsList.Items.Count is 1 && argsList.Items[0] is Expression.Environment)
        {
            parameterCount = 1;
        }
        else
        {
            return "Unexpected args list structure - expected [] or [Environment]";
        }

        return
            new FunctionRecord(
                InnerFunction: innerExpression,
                ParameterCount: parameterCount,
                EnvFunctions: envFunctions,
                ArgumentsAlreadyCollected: ReadOnlyMemory<PineValue>.Empty);
    }

    /// <summary>
    /// Parses env functions from an expression. Supports two forms:
    /// 1. Single Literal containing a ListValue of env functions
    /// 2. List expression containing only Literal items
    /// </summary>
    private static Result<string, PineValue[]> ParseEnvFunctionsFromExpression(Expression expr)
    {
        // Form 1: Single literal containing a list value
        if (expr is Expression.Literal envFuncsLiteral)
        {
            if (envFuncsLiteral.Value is PineValue.ListValue envFuncsList)
            {
                return envFuncsList.Items.ToArray();
            }

            return "Expected Literal to contain a ListValue for env functions";
        }

        // Form 2: List of literals
        if (expr is Expression.List envFuncListExpr)
        {
            var funcs = new PineValue[envFuncListExpr.Items.Count];

            for (var i = 0; i < envFuncListExpr.Items.Count; i++)
            {
                var item = envFuncListExpr.Items[i];

                if (item is not Expression.Literal lit)
                {
                    return "Expected all items in env functions list to be Literals";
                }

                funcs[i] = lit.Value;
            }

            return funcs;
        }

        return "Expected env functions to be either a Literal (containing list) or List of Literals";
    }

    /// <summary>
    /// Parses a function record from a multi-parameter nested wrapper form.
    /// Multi-parameter wrappers use List expressions that build encoding dynamically.
    /// 
    /// Expected structure matches what PartialApplicationWrapper.EmitFunctionValue produces:
    /// - Level 0: List expression producing ["ParseAndEval", [Literal(level1), ...]]
    /// - Level 1..N-2: List expression producing ["ParseAndEval", [Literal(levelN), ...]]
    /// - Level N-1 (innermost): ParseAndEval(innerExpr, [envFuncs, ...])
    /// </summary>
    private static Result<string, FunctionRecord> ParseMultiParamNestedWrapperForm(
        PineValue encodedWrapper,
        PineVMParseCache parseCache)
    {
        var parseExprResult = parseCache.ParseExpression(encodedWrapper);

        if (parseExprResult.IsErrOrNull() is { } parseErr)
        {
            return "Failed to parse nested wrapper expression: " + parseErr;
        }

        if (parseExprResult.IsOkOrNull() is not Expression.List outerList)
        {
            return "Multi-param wrapper must be a List expression";
        }

        // Traverse the nested structure following the exact expected pattern
        var traverseResult = TraverseMultiParamWrapperStrict(encodedWrapper, parseCache);

        if (traverseResult.IsErrOrNull() is { } traverseErr)
        {
            return traverseErr;
        }

        if (traverseResult.IsOkOrNullable() is not { } result)
        {
            return "Failed to traverse multi-param wrapper";
        }

        return
            new FunctionRecord(
                InnerFunction: result.innerExpr,
                ParameterCount: result.paramCount,
                EnvFunctions: result.envFuncs,
                ArgumentsAlreadyCollected: ReadOnlyMemory<PineValue>.Empty);
    }

    /// <summary>
    /// Strictly traverses multi-parameter wrapper structure following the exact expected pattern.
    /// Returns error if the structure doesn't match what PartialApplicationWrapper produces.
    /// </summary>
    private static Result<string, (int paramCount, Expression innerExpr, PineValue[] envFuncs)> TraverseMultiParamWrapperStrict(
        PineValue encodedWrapper,
        PineVMParseCache parseCache)
    {
        // Multi-param wrappers have structure:
        // Level 0: List expression ["ParseAndEval", [Literal(level1), envStructure]]
        //   where envStructure builds [[Literal([env])], Environment]
        // Level 1..N-2: List expression with similar structure, using concat for captured args
        // Level N-1 (innermost): ParseAndEval(innerExpr, [envFuncs, concat(captured, [env])])

        var levels = 1;
        var currentValue = encodedWrapper;

        while (true)
        {
            var parseResult = parseCache.ParseExpression(currentValue);

            if (parseResult.IsOkOrNull() is not Expression.List currentList)
            {
                return "Expected List expression at wrapper level " + levels;
            }

            // Find the Literal in the structure that contains the next level
            // The structure is ["ParseAndEval", [Literal(nextLevel), ...]]
            // So we expect items[0] = "ParseAndEval" tag, items[1] = [encoded, env]
            var nextLevelResult = ExtractNextLevelFromListExpression(currentList, parseCache);

            if (nextLevelResult.IsErrOrNull() is { } extractErr)
            {
                return "Failed to extract next level at level " + levels + ": " + extractErr;
            }

            if (nextLevelResult.IsOkOrNull() is not { } nextLevelValue)
            {
                return "Failed to extract next level value at level " + levels;
            }

            // Parse the next level
            var nextParseResult = parseCache.ParseExpression(nextLevelValue);

            if (nextParseResult.IsErrOrNull() is { } nextParseErr)
            {
                return "Failed to parse next level at level " + levels + ": " + nextParseErr;
            }

            if (nextParseResult.IsOkOrNull() is Expression.ParseAndEval parseAndEval)
            {
                // This is the innermost level - extract inner expression and env functions
                levels++;

                // Extract inner expression from Encoded
                if (parseAndEval.Encoded is not Expression.Literal innerLit)
                {
                    return "Expected Literal in innermost ParseAndEval.Encoded";
                }

                var innerParseResult = parseCache.ParseExpression(innerLit.Value);
                if (innerParseResult.IsErrOrNull() is { } innerErr)
                {
                    return "Failed to parse inner expression: " + innerErr;
                }

                if (innerParseResult.IsOkOrNull() is not { } innerExpression)
                {
                    return "Failed to extract inner expression";
                }

                // Extract env functions from environment structure
                if (parseAndEval.Environment is not Expression.List envList || envList.Items.Count < 1)
                {
                    return "Expected environment to be a list with at least 1 element";
                }

                var envFuncsResult = ParseEnvFunctionsFromExpression(envList.Items[0]);

                if (envFuncsResult.IsErrOrNull() is { } envFuncsErr)
                {
                    return "Failed to parse env functions in innermost level: " + envFuncsErr;
                }

                if (envFuncsResult.IsOkOrNull() is not { } envFunctions)
                {
                    return "Failed to extract env functions";
                }

                return (levels, innerExpression, envFunctions);
            }
            else if (nextParseResult.IsOkOrNull() is Expression.List)
            {
                // This is another wrapper level
                levels++;
                currentValue = nextLevelValue;
            }
            else
            {
                return "Unexpected expression type at level " + levels + ": expected List or ParseAndEval";
            }
        }
    }

    /// <summary>
    /// Extracts the next level value from a List expression that represents a wrapper level.
    /// The structure is: ["ParseAndEval", [[Literal(nextLevel), envStructure]]]
    /// We need to find the Literal containing the next level.
    /// </summary>
    private static Result<string, PineValue> ExtractNextLevelFromListExpression(
        Expression.List listExpr,
        PineVMParseCache parseCache)
    {
        // The List expression builds ["ParseAndEval", [encoded, env]]
        // We need to find the Literal that contains the next level
        // Expected structure: items[0] = ParseAndEvalTag, items[1] = [inner items]
        // Where inner items[0][0] = Literal containing next level

        if (listExpr.Items.Count is 2)
        {
            var secondItem = listExpr.Items[1];

            var literalResult = FindFirstValidNextLevelLiteral(secondItem, parseCache);

            if (literalResult.IsOkOrNull() is { } nextLevel)
            {
                return nextLevel;
            }
        }

        return "Could not find next level Literal in wrapper List expression";
    }

    /// <summary>
    /// Recursively searches for the first Literal that contains a valid next level expression.
    /// </summary>
    private static Result<string, PineValue> FindFirstValidNextLevelLiteral(
        Expression expr,
        PineVMParseCache parseCache)
    {
        switch (expr)
        {
            case Expression.Literal lit:
                {
                    // Check if this literal contains a valid expression (List or ParseAndEval)
                    var parseResult = parseCache.ParseExpression(lit.Value);
                    if (parseResult.IsOkOrNull() is Expression.List or Expression.ParseAndEval)
                    {
                        return lit.Value;
                    }
                    return "Literal does not contain a valid next level expression";
                }

            case Expression.List list:
                {
                    // Recursively search in list items
                    foreach (var item in list.Items)
                    {
                        var result = FindFirstValidNextLevelLiteral(item, parseCache);
                        if (result.IsOkOrNull() is { } value)
                        {
                            return value;
                        }
                    }
                    return "No valid next level Literal found in List";
                }

            default:
                return "Expression is not a Literal or List";
        }
    }

    /// <summary>
    /// Wraps the function record encoding with the "Function" tag.
    /// Inverse of <see cref="ParseFunctionRecordTagged(PineValue, PineVMParseCache)"/>.
    /// </summary>
    /// <param name="functionRecord">Function record to encode.</param>
    /// <returns>Tagged <see cref="PineValue"/> representation.</returns>
    public static PineValue EncodeFunctionRecordInValueTagged(
        FunctionRecord functionRecord)
    {
        return
            PineValue.List(
                [
                StringEncoding.ValueFromString("Function"),
                EncodeFunctionRecordInValue(functionRecord)
                ]);
    }

    /// <summary>
    /// Parses a function record from a tagged or raw value using a structural view (<see cref="PineValueClass"/>).
    /// If the value is tagged with name "Function", the payload is parsed as a function record.
    /// Otherwise, a plain value is interpreted as a zero-parameter function (literal shortcut).
    /// </summary>
    /// <param name="valueClass">Structural view of the candidate value.</param>
    /// <param name="parseCache">Cache used to parse the inner expression.</param>
    /// <returns>
    /// On success: parsed <see cref="FunctionRecord"/>.
    /// On failure: error message describing why the value could not be parsed as a function record.
    /// </returns>
    public static Result<string, FunctionRecord> ParseFunctionRecordTagged(
        PineValueClass valueClass,
        PineVMParseCache parseCache)
    {
        var parseTaggedResult = ElmInteractiveEnvironment.ParseTagged(valueClass);

        if (parseTaggedResult.IsOkOrNullable() is { } taggedFunctionDeclaration &&
            taggedFunctionDeclaration.name is "Function")
        {
            return ParseFunctionRecord(taggedFunctionDeclaration.value, parseCache);
        }

        if (parseTaggedResult.IsErrOrNull() is { } err)
        {
            return "Failed to parse tagged function record: " + err;
        }

        if (valueClass.TryGetValue([]) is { } overallValue)
        {
            /*
             * If the declaration has zero parameters, it could be encoded as plain value without wrapping in a 'Function' record.
             * */

            return
                new FunctionRecord(
                    InnerFunction: Expression.LiteralInstance(overallValue),
                    ParameterCount: 0,
                    EnvFunctions: ReadOnlyMemory<PineValue>.Empty,
                    ArgumentsAlreadyCollected: ReadOnlyMemory<PineValue>.Empty);
        }

        throw new NotImplementedException(
            "Unexpected result type: " + parseTaggedResult.GetType());
    }

    /// <summary>
    /// Analog to the 'parseFunctionRecordFromValue' function in FirCompiler.elm.
    /// Expects a list of four elements (inner function expression, parameter count, env functions, collected arguments).
    /// </summary>
    /// <param name="pineValue">Encoded list value.</param>
    /// <param name="parseCache">Cache for parsing the inner expression.</param>
    /// <returns>Parsed <see cref="FunctionRecord"/> or error message.</returns>
    public static Result<string, FunctionRecord> ParseFunctionRecord(
        PineValue pineValue,
        PineVMParseCache parseCache)
    {
        if (PineValueExtension.ValueFromPathOrNull(pineValue, [0]) is not { } innerExprValue)
        {
            return "Function record missing inner function at [0]";
        }

        var parseInnerExprResult = parseCache.ParseExpression(innerExprValue);

        {
            if (parseInnerExprResult.IsErrOrNull() is { } err)
            {
                return "Failed to parse inner function: " + err;
            }
        }

        if (parseInnerExprResult.IsOkOrNull() is not { } innerFunction)
        {
            throw new NotImplementedException(
                "Unexpected result type: " + parseInnerExprResult.GetType());
        }

        if (PineValueExtension.ValueFromPathOrNull(pineValue, [1]) is not { } paramCountValue)
        {
            return "Function record missing parameter count at [1]";
        }

        var parseFunctionParameterCountResult =
            IntegerEncoding.ParseSignedIntegerStrict(paramCountValue);
        {
            if (parseFunctionParameterCountResult.IsErrOrNull() is { } err)
            {
                return "Failed to decode function parameter count: " + err;
            }
        }

        if (parseFunctionParameterCountResult.IsOkOrNullable() is not { } functionParameterCount)
        {
            throw new NotImplementedException(
                "Unexpected result type: " + parseFunctionParameterCountResult.GetType());
        }

        if (PineValueExtension.ValueFromPathOrNull(pineValue, [2]) is not { } envFunctionsValue)
        {
            return "Function record missing env functions at [2]";
        }

        if (envFunctionsValue is not PineValue.ListValue envFunctionsList)
        {
            return "envFunctionsValue is not a list";
        }

        if (PineValueExtension.ValueFromPathOrNull(pineValue, [3]) is not { } argumentsAlreadyCollectedValue)
        {
            return "Function record missing collected arguments at [3]";
        }

        if (argumentsAlreadyCollectedValue is not PineValue.ListValue argumentsAlreadyCollectedList)
        {
            return "argumentsAlreadyCollectedValue is not a list";
        }

        return
            new FunctionRecord(
                InnerFunction: innerFunction,
                ParameterCount: (int)functionParameterCount,
                EnvFunctions: envFunctionsList.Items.ToArray(),
                ArgumentsAlreadyCollected: argumentsAlreadyCollectedList.Items.ToArray());
    }

    /// <summary>
    /// Parses a function record from a structural view (<see cref="PineValueClass"/>).
    /// </summary>
    /// <param name="valueClass">Structural view of the encoded list value.</param>
    /// <param name="parseCache">Cache for parsing the inner expression.</param>
    /// <returns>Parsed <see cref="FunctionRecord"/> or error message.</returns>
    public static Result<string, FunctionRecord> ParseFunctionRecord(
        PineValueClass valueClass,
        PineVMParseCache parseCache)
    {
        if (valueClass.TryGetValue([0]) is not { } innerExprValue)
        {
            return "Function record missing inner function at [0]";
        }

        var parseInnerExprResult = parseCache.ParseExpression(innerExprValue);

        {
            if (parseInnerExprResult.IsErrOrNull() is { } err)
            {
                return "Failed to parse inner function: " + err;
            }
        }

        if (parseInnerExprResult.IsOkOrNull() is not { } innerFunction)
        {
            throw new NotImplementedException(
                "Unexpected result type: " + parseInnerExprResult.GetType());
        }

        if (valueClass.TryGetValue([1]) is not { } paramCountValue)
        {
            return "Function record missing parameter count at [1]";
        }

        var parseFunctionParameterCountResult =
            IntegerEncoding.ParseSignedIntegerStrict(paramCountValue);

        {
            if (parseFunctionParameterCountResult.IsErrOrNull() is { } err)
            {
                return "Failed to decode function parameter count: " + err;
            }
        }

        if (parseFunctionParameterCountResult.IsOkOrNullable() is not { } functionParameterCount)
        {
            throw new NotImplementedException(
                "Unexpected result type: " + parseFunctionParameterCountResult.GetType());
        }

        if (valueClass.TryGetValue([2]) is not { } envFunctionsValue)
        {
            return "Function record missing env functions at [2]";
        }

        if (envFunctionsValue is not PineValue.ListValue envFunctionsList)
        {
            return "envFunctionsValue is not a list";
        }

        if (valueClass.TryGetValue([3]) is not { } argumentsAlreadyCollectedValue)
        {
            return "Function record missing collected arguments at [3]";
        }

        if (argumentsAlreadyCollectedValue is not PineValue.ListValue argumentsAlreadyCollectedList)
        {
            return "argumentsAlreadyCollectedValue is not a list";
        }

        return
            new FunctionRecord(
                InnerFunction: innerFunction,
                ParameterCount: (int)functionParameterCount,
                EnvFunctions: envFunctionsList.Items.ToArray(),
                ArgumentsAlreadyCollected: argumentsAlreadyCollectedList.Items.ToArray());
    }

    /// <summary>
    /// Serializes a <see cref="FunctionRecord"/> into its value representation.
    /// Inverse of <see cref="ParseFunctionRecord(PineValue, PineVMParseCache)"/>.
    /// </summary>
    public static PineValue EncodeFunctionRecordInValue(
        FunctionRecord functionRecord)
    {
        var innerFunctionValue =
            ExpressionEncoding.EncodeExpressionAsValue(functionRecord.InnerFunction);

        return
            PineValue.List(
                [
                innerFunctionValue,
                IntegerEncoding.EncodeSignedInteger(functionRecord.ParameterCount),
                PineValue.List(functionRecord.EnvFunctions.ToArray()),
                PineValue.List(functionRecord.ArgumentsAlreadyCollected.ToArray())
                ]);
    }
}
