using Pine.Core.PopularEncodings;
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
    /// Accepts either a tagged value ("Function") or a raw value (zero-argument function literal shortcut).
    /// </summary>
    /// <param name="pineValue">Encoded value representing a function (possibly tagged).</param>
    /// <param name="parseCache">Cache used to parse the inner expression.</param>
    /// <returns>Parsed <see cref="FunctionRecord"/> or error description.</returns>
    public static Result<string, FunctionRecord> ParseFunctionRecordTagged(
        PineValue pineValue,
        PineVMParseCache parseCache)
    {
        return
            ParseFunctionRecordTagged(PineValueClass.CreateEquals(pineValue), parseCache);
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
        return
            ParseFunctionRecord(PineValueClass.CreateEquals(pineValue), parseCache);
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
