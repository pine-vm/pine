using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.CommonEncodings;
using System;
using System.Collections.Immutable;
using Xunit;

namespace Pine.Core.Tests.CodeAnalysis;

using PineVM = Core.Interpreter.IntermediateVM.PineVM;

/// <summary>
/// Tests for the <see cref="PartialApplicationWrapper"/> class that creates nested wrappers
/// for partial application of Elm functions.
/// </summary>
public class PartialApplicationWrapperTests
{
    private static readonly PineVMParseCache s_parseCache = new();

    #region Zero Parameter Tests

    [Fact]
    public void EmitFunctionValue_ZeroParameters_ReturnsDirectInvocationWrapper()
    {
        // A zero-parameter function just returns a constant
        var literalResult = PineValue.Blob([42]);
        var innerExpression = Expression.LiteralInstance(literalResult);

        var functionValue =
            PartialApplicationWrapper.EmitFunctionValue(
                innerExpression,
                parameterCount: 0,
                envFunctions: []);

        // The wrapper should be parseable as an expression
        var parseResult = s_parseCache.ParseExpression(functionValue);
        parseResult.IsOkOrNull().Should().NotBeNull();
    }

    [Fact]
    public void EmitFunctionValue_ZeroParameters_EvaluatesCorrectly()
    {
        // Inner expression returns a constant
        var expectedResult = PineValue.Blob([1, 2, 3]);
        var innerExpression = Expression.LiteralInstance(expectedResult);

        var functionValue =
            PartialApplicationWrapper.EmitFunctionValue(
                innerExpression,
                parameterCount: 0,
                envFunctions: []);

        // Evaluate the wrapper with empty environment (no args needed)
        var result = EvaluateEncodedExpression(functionValue, PineValue.EmptyList);
        result.Should().Be(expectedResult);
    }

    #endregion

    #region Single Parameter Tests

    [Fact]
    public void EmitFunctionValue_SingleParameter_ReturnsValidWrapper()
    {
        // Inner expression returns its argument (identity function)
        // Inner expression expects env = [envFuncs, [arg]]
        // So to return arg, we access env[1][0]
        var innerExpression = BuildExpressionForPathInEnvironment([0, 0]);

        var functionValue =
            PartialApplicationWrapper.EmitFunctionValue(
                innerExpression,
                parameterCount: 1,
                envFunctions: []);

        // Should be parseable
        var parseResult = s_parseCache.ParseExpression(functionValue);

        parseResult.IsOkOrNull().Should().NotBeNull();
    }

    [Fact]
    public void EmitFunctionValue_SingleParameter_EvaluatesCorrectly()
    {
        // Inner expression returns its single argument
        // env = [envFuncs, [arg]], so arg is at env[1][0]
        var innerExpression = BuildExpressionForPathInEnvironment([1, 0]);

        var functionValue =
            PartialApplicationWrapper.EmitFunctionValue(
                innerExpression,
                parameterCount: 1,
                envFunctions: []);

        // Evaluate with arg = [10, 20, 30]
        var argValue = PineValue.List([PineValue.Blob([10]), PineValue.Blob([20]), PineValue.Blob([30])]);
        var result = EvaluateEncodedExpression(functionValue, argValue);

        result.Should().Be(argValue);
    }

    [Fact]
    public void EmitFunctionValue_SingleParameter_WithEnvFunctions()
    {
        // Inner expression accesses an env function and the argument
        // env = [envFuncs, [arg]]
        // envFuncs[0] is a blob
        var envFunction = PineValue.Blob([99, 88, 77]);

        // Return [envFuncs[0], arg]
        var envFuncAccess = BuildExpressionForPathInEnvironment([0, 0]);
        var argAccess = BuildExpressionForPathInEnvironment([1, 0]);

        var innerExpression = Expression.ListInstance([envFuncAccess, argAccess]);

        var functionValue =
            PartialApplicationWrapper.EmitFunctionValue(
                innerExpression,
                parameterCount: 1,
                envFunctions: [envFunction]);

        var argValue = PineValue.Blob([1, 2, 3]);
        var result = EvaluateEncodedExpression(functionValue, argValue);

        result.Should().BeOfType<PineValue.ListValue>();
        var resultList = (PineValue.ListValue)result;
        resultList.Items.Length.Should().Be(2);
        resultList.Items.Span[0].Should().Be(envFunction);
        resultList.Items.Span[1].Should().Be(argValue);
    }

    #endregion

    #region Two Parameter Tests

    [Fact]
    public void EmitFunctionValue_TwoParameters_ReturnsValidWrapper()
    {
        // Inner expression returns [arg0, arg1]
        // env = [envFuncs, [arg0, arg1]]
        var arg0Access = BuildExpressionForPathInEnvironment([0, 0]);
        var arg1Access = BuildExpressionForPathInEnvironment([0, 1]);

        var innerExpression = Expression.ListInstance([arg0Access, arg1Access]);

        var functionValue =
            PartialApplicationWrapper.EmitFunctionValue(
                innerExpression,
                parameterCount: 2,
                envFunctions: []);

        // Should be parseable
        var parseResult = s_parseCache.ParseExpression(functionValue);
        parseResult.IsOkOrNull().Should().NotBeNull();
    }

    [Fact]
    public void EmitFunctionValue_TwoParameters_IncrementalApplication()
    {
        // Inner expression returns [arg0, arg1]
        // env = [envFuncs, [arg0, arg1]]
        var arg0Access = BuildExpressionForPathInEnvironment([1, 0]);
        var arg1Access = BuildExpressionForPathInEnvironment([1, 1]);

        var innerExpression = Expression.ListInstance([arg0Access, arg1Access]);

        var functionValue =
            PartialApplicationWrapper.EmitFunctionValue(
                innerExpression,
                parameterCount: 2,
                envFunctions: []);

        var arg0 = PineValue.Blob([11, 22]);
        var arg1 = PineValue.Blob([33, 44]);

        // Step 1: Apply first argument (arg0)
        var partiallyApplied = EvaluateEncodedExpression(functionValue, arg0);

        // partiallyApplied should be an encoded expression (a list representing ParseAndEval encoding)
        partiallyApplied.Should().BeOfType<PineValue.ListValue>();

        // Step 2: Apply second argument (arg1)
        var finalResult = EvaluateEncodedExpression(partiallyApplied, arg1);

        // Should be [arg0, arg1]
        finalResult.Should().Be(
            PineValue.List(
                [
                arg0,
                arg1
                ]));
    }

    #endregion

    #region Three Parameter Tests

    [Fact]
    public void EmitFunctionValue_ThreeParameters_IncrementalApplication()
    {
        // Inner expression returns [arg0, arg1, arg2]
        var arg0Access = BuildExpressionForPathInEnvironment([1, 0]);
        var arg1Access = BuildExpressionForPathInEnvironment([1, 1]);
        var arg2Access = BuildExpressionForPathInEnvironment([1, 2]);

        var innerExpression = Expression.ListInstance([arg0Access, arg1Access, arg2Access]);

        var functionValue =
            PartialApplicationWrapper.EmitFunctionValue(
                innerExpression,
                parameterCount: 3,
                envFunctions: []);

        var arg0 = PineValue.Blob([1]);
        var arg1 = PineValue.Blob([2]);
        var arg2 = PineValue.Blob([3]);

        // Step 1: Apply arg0
        var partial1 = EvaluateEncodedExpression(functionValue, arg0);

        // Step 2: Apply arg1
        var partial2 = EvaluateEncodedExpression(partial1, arg1);

        // Step 3: Apply arg2
        var finalResult = EvaluateEncodedExpression(partial2, arg2);

        // Should be [arg0, arg1, arg2]
        finalResult.Should().Be(
            PineValue.List(
                [
                arg0,
                arg1,
                arg2
                ]));
    }

    [Fact]
    public void EmitFunctionValue_ThreeParameters_WithEnvFunctions_IncrementalApplication()
    {
        // Inner expression returns [envFunc0, envFunc1, arg0 + arg1 + arg2]
        // This test verifies that environment functions are correctly integrated across all wrapper levels

        var envFunc0 =
            StringEncoding.ValueFromString("Arancini");

        var envFunc1 =
            StringEncoding.ValueFromString("Biscotti");

        // Access env functions: env[0][0] and env[0][1]
        var envFunc0Access = BuildExpressionForPathInEnvironment([0, 0]);
        var envFunc1Access = BuildExpressionForPathInEnvironment([0, 1]);

        // Access arguments: env[1][0], env[1][1], env[1][2]
        var arg0Access = BuildExpressionForPathInEnvironment([1, 0]);
        var arg1Access = BuildExpressionForPathInEnvironment([1, 1]);
        var arg2Access = BuildExpressionForPathInEnvironment([1, 2]);

        // Create product expression: arg0 * arg1
        var productExpr =
            Expression.KernelApplicationInstance(
                function: nameof(KernelFunction.int_mul),
                input: Expression.ListInstance([arg0Access, arg1Access]));

        // Create sum expression: (arg0 * arg1) + arg2

        var sumExpr =
            Expression.KernelApplicationInstance(
                function: nameof(KernelFunction.int_add),
                input: Expression.ListInstance([productExpr, arg2Access]));

        // Return [envFunc0, envFunc1, product]
        var innerExpression = Expression.ListInstance([envFunc0Access, sumExpr, envFunc1Access]);

        var functionValue =
            PartialApplicationWrapper.EmitFunctionValue(
                innerExpression,
                parameterCount: 3,
                envFunctions: [envFunc0, envFunc1]);

        var arg0 = IntegerEncoding.EncodeSignedInteger(13);
        var arg1 = IntegerEncoding.EncodeSignedInteger(17);
        var arg2 = IntegerEncoding.EncodeSignedInteger(21);

        // Step 1: Apply arg0
        var partial1 = EvaluateEncodedExpression(functionValue, arg0);

        // Step 2: Apply arg1
        var partial2 = EvaluateEncodedExpression(partial1, arg1);

        // Step 3: Apply arg2
        var finalResult = EvaluateEncodedExpression(partial2, arg2);

        var expectedSum = 13 * 17 + 21;

        // Should be [envFunc0, expectedSum, envFunc1]

        finalResult.Should().Be(
            PineValue.List(
                [
                envFunc0,
                IntegerEncoding.EncodeSignedInteger(expectedSum),
                envFunc1
                ]));
    }

    #endregion

    [Fact]
    public void EmitFunctionValue_ZeroParameters()
    {
        // A zero-parameter function returns a constant
        var expectedResult = PineValue.Blob([42, 43]);
        var innerExpression = Expression.LiteralInstance(expectedResult);

        // Encode as nested wrapper
        var functionValue =
            PartialApplicationWrapper.EmitFunctionValue(
                innerExpression,
                parameterCount: 0,
                envFunctions: []);

        // Evaluate
        var result = EvaluateEncodedExpression(functionValue, PineValue.EmptyList);
        result.Should().Be(expectedResult);
    }

    [Fact]
    public void EmitFunctionValue_SingleParameter()
    {
        // A function that returns its argument
        var innerExpression = BuildExpressionForPathInEnvironment([1, 0]);

        // Encode as nested wrapper
        var functionValue =
            PartialApplicationWrapper.EmitFunctionValue(
                innerExpression,
                parameterCount: 1,
                envFunctions: []);

        var arg = PineValue.Blob([1, 2, 3]);
        var result = EvaluateEncodedExpression(functionValue, arg);
        result.Should().Be(arg);
    }

    [Fact]
    public void EmitFunctionValue_WithEnvFunctions()
    {
        // A function that uses an env function and returns [envFunc[0], arg0]
        var envFuncAccess = BuildExpressionForPathInEnvironment([0, 0]);
        var argAccess = BuildExpressionForPathInEnvironment([1, 0]);
        var innerExpression = Expression.ListInstance([envFuncAccess, argAccess]);

        var envFunction = PineValue.Blob([99]);

        // Encode as nested wrapper
        var functionValue =
            PartialApplicationWrapper.EmitFunctionValue(
                innerExpression,
                parameterCount: 1,
                envFunctions: [envFunction]);

        var arg = PineValue.Blob([1, 2, 3]);
        var finalResult = EvaluateEncodedExpression(functionValue, arg);

        finalResult.Should().Be(
            PineValue.List(
                [
                envFunction,
                arg
                ]));
    }

    [Fact]
    public void EmitFunctionValue_TwoParameters_WithEnvFunctions_IncrementalApplication()
    {
        // A function that uses env functions and returns [envFunc0, envFunc1, arg0 + arg1]
        var envFunc0Access = BuildExpressionForPathInEnvironment([0, 0]);
        var envFunc1Access = BuildExpressionForPathInEnvironment([0, 1]);
        var arg0Access = BuildExpressionForPathInEnvironment([1, 0]);
        var arg1Access = BuildExpressionForPathInEnvironment([1, 1]);

        // sum = arg0 + arg1
        var sumExpr = Expression.KernelApplicationInstance(
            function: nameof(KernelFunction.int_add),
            input: Expression.ListInstance([arg0Access, arg1Access]));

        var innerExpression = Expression.ListInstance([envFunc0Access, envFunc1Access, sumExpr]);

        var envFunc0 = PineValue.Blob([100]);
        var envFunc1 = PineValue.Blob([200]);

        // Encode as nested wrapper
        var functionValue =
            PartialApplicationWrapper.EmitFunctionValue(
                innerExpression,
                parameterCount: 2,
                envFunctions: [envFunc0, envFunc1]);

        var arg0 = IntegerEncoding.EncodeSignedInteger(10);
        var arg1 = IntegerEncoding.EncodeSignedInteger(32);

        // Apply arg0
        var partial = EvaluateEncodedExpression(functionValue, arg0);

        // Apply arg1
        var finalResult = EvaluateEncodedExpression(partial, arg1);

        // Should be [envFunc0, envFunc1, 10 + 32 = 42]
        finalResult.Should().Be(
            PineValue.List(
                [
                envFunc0,
                envFunc1,
                IntegerEncoding.EncodeSignedInteger(42)
                ]));
    }

    #region Dynamic Interpreter Tests

    [Fact]
    public void DynamicTest_SingleParameter_SingleEvaluationPerArgument()
    {
        // Create a simple identity function wrapper
        var innerExpression = BuildExpressionForPathInEnvironment([1, 0]);

        var functionValue =
            PartialApplicationWrapper.EmitFunctionValue(
                innerExpression,
                parameterCount: 1,
                envFunctions: []);

        var arg = PineValue.Blob([42]);
        var result = EvaluateEncodedExpression(functionValue, arg);

        // For single param, one evaluation should produce the final result
        result.Should().Be(arg);
    }

    [Fact]
    public void DynamicTest_TwoParameters_TwoEvaluations()
    {
        // Identity function for first arg: returns arg0
        var innerExpression = BuildExpressionForPathInEnvironment([1, 0]);

        var functionValue =
            PartialApplicationWrapper.EmitFunctionValue(
                innerExpression,
                parameterCount: 2,
                envFunctions: []);

        var arg0 = PineValue.Blob([1]);
        var arg1 = PineValue.Blob([2]);

        // First evaluation: apply arg0
        var partial = EvaluateEncodedExpression(functionValue, arg0);

        // Second evaluation: apply arg1
        var result = EvaluateEncodedExpression(partial, arg1);

        // Inner function returns arg0, so result should be arg0
        result.Should().Be(arg0);
    }

    [Fact]
    public void DynamicTest_IntAddition_TwoParameters()
    {
        // Create a function that adds two integers
        // Inner env = [envFuncs, [a, b]]
        var arg0 = BuildExpressionForPathInEnvironment([1, 0]);
        var arg1 = BuildExpressionForPathInEnvironment([1, 1]);

        var innerExpression =
            Expression.KernelApplicationInstance(
                function: nameof(KernelFunction.int_add),
                input: Expression.ListInstance([arg0, arg1]));

        var functionValue =
            PartialApplicationWrapper.EmitFunctionValue(
                innerExpression,
                parameterCount: 2,
                envFunctions: []);

        var a = IntegerEncoding.EncodeSignedInteger(10);
        var b = IntegerEncoding.EncodeSignedInteger(32);

        // First evaluation: apply a
        var partial = EvaluateEncodedExpression(functionValue, a);

        // Second evaluation: apply b
        var finalResult = EvaluateEncodedExpression(partial, b);

        // Should be 10 + 32 = 42
        finalResult.Should().Be(IntegerEncoding.EncodeSignedInteger(42));
    }

    #endregion

    #region ParseFunctionRecord with Nested Wrapper Tests

    [Fact]
    public void ParseFunctionRecord_ZeroParameters_ParsesCorrectly()
    {
        var expectedResult = PineValue.Blob([42, 43]);
        var innerExpression = Expression.LiteralInstance(expectedResult);

        var functionValue =
            PartialApplicationWrapper.EmitFunctionValue(
                innerExpression,
                parameterCount: 0,
                envFunctions: []);

        // Parse the nested wrapper form
        var parseResult = FunctionRecord.ParseFunctionRecordTagged(functionValue, s_parseCache);

        parseResult.IsOkOrNull().Should().NotBeNull();
        var record = parseResult.IsOkOrNull()!;

        record.ParameterCount.Should().Be(0);
        record.EnvFunctions.Length.Should().Be(0);
        record.ArgumentsAlreadyCollected.Length.Should().Be(0);
    }

    [Fact]
    public void ParseFunctionRecord_SingleParameter_ParsesCorrectly()
    {
        var innerExpression = BuildExpressionForPathInEnvironment([1, 0]);

        var functionValue =
            PartialApplicationWrapper.EmitFunctionValue(
                innerExpression,
                parameterCount: 1,
                envFunctions: []);

        // Parse the nested wrapper form
        var parseResult = FunctionRecord.ParseFunctionRecordTagged(functionValue, s_parseCache);

        parseResult.IsOkOrNull().Should().NotBeNull();
        var record = parseResult.IsOkOrNull()!;

        record.ParameterCount.Should().Be(1);
        record.EnvFunctions.Length.Should().Be(0);
        record.ArgumentsAlreadyCollected.Length.Should().Be(0);
    }

    [Fact]
    public void ParseFunctionRecord_SingleParameter_WithEnvFunctions_ParsesParameterCount()
    {
        var envFunc0 = PineValue.Blob([100]);
        var envFunc1 = PineValue.Blob([200]);

        var innerExpression = Expression.ListInstance(
            [
            BuildExpressionForPathInEnvironment([0, 0]),
            BuildExpressionForPathInEnvironment([0, 1]),
            BuildExpressionForPathInEnvironment([1, 0])
            ]);

        var functionValue =
            PartialApplicationWrapper.EmitFunctionValue(
                innerExpression,
                parameterCount: 1,
                envFunctions: [envFunc0, envFunc1]);

        // Parse the nested wrapper form
        var parseResult = FunctionRecord.ParseFunctionRecordTagged(functionValue, s_parseCache);

        parseResult.IsOkOrNull().Should().NotBeNull();
        var record = parseResult.IsOkOrNull()!;

        // Verify parameter count is correctly parsed
        record.ParameterCount.Should().Be(1);
        record.ArgumentsAlreadyCollected.Length.Should().Be(0);

        // Note: env functions parsing from nested wrapper form requires traversing to innermost level
        // The current implementation extracts env functions from the outermost level
        record.EnvFunctions.Length.Should().Be(2);
        record.EnvFunctions.Span[0].Should().Be(envFunc0);
        record.EnvFunctions.Span[1].Should().Be(envFunc1);
    }

    [Fact]
    public void ParseFunctionRecord_TwoParameters_ParsesParameterCount()
    {
        var innerExpression = Expression.ListInstance(
            [
            BuildExpressionForPathInEnvironment([1, 0]),
            BuildExpressionForPathInEnvironment([1, 1])
            ]);

        var functionValue =
            PartialApplicationWrapper.EmitFunctionValue(
                innerExpression,
                parameterCount: 2,
                envFunctions: []);

        // Parse the nested wrapper form
        var parseResult = FunctionRecord.ParseFunctionRecordTagged(functionValue, s_parseCache);

        parseResult.IsOkOrNull().Should().NotBeNull();
        var record = parseResult.IsOkOrNull()!;

        record.ParameterCount.Should().Be(2);
        record.EnvFunctions.Length.Should().Be(0);
        record.ArgumentsAlreadyCollected.Length.Should().Be(0);
    }

    [Fact]
    public void ParseFunctionRecord_ThreeParameters_ParsesParameterCount()
    {
        var innerExpression = Expression.ListInstance(
            [
            BuildExpressionForPathInEnvironment([1, 0]),
            BuildExpressionForPathInEnvironment([1, 1]),
            BuildExpressionForPathInEnvironment([1, 2])
            ]);

        var functionValue =
            PartialApplicationWrapper.EmitFunctionValue(
                innerExpression,
                parameterCount: 3,
                envFunctions: []);

        // Parse the nested wrapper form
        var parseResult = FunctionRecord.ParseFunctionRecordTagged(functionValue, s_parseCache);

        parseResult.IsOkOrNull().Should().NotBeNull();
        var record = parseResult.IsOkOrNull()!;

        record.ParameterCount.Should().Be(3);
        record.EnvFunctions.Length.Should().Be(0);
        record.ArgumentsAlreadyCollected.Length.Should().Be(0);
    }

    #endregion

    #region Helper Methods

    private static Expression BuildExpressionForPathInEnvironment(
        ReadOnlySpan<int> path) =>
        ExpressionBuilder.BuildExpressionForPathInExpression(
            path,
            Expression.EnvironmentInstance);

    /// <summary>
    /// Creates an expression that gets an item at the specified index from a list.
    /// Uses <see cref="ExpressionBuilder.BuildExpressionForPathInExpression"/> with a single-element path.
    /// </summary>
    private static Expression ListItemFromIndex(int index, Expression listExpr) =>
        ExpressionBuilder.BuildExpressionForPathInExpression([index], listExpr);

    /// <summary>
    /// Evaluates the given expression with the specified environment value.
    /// This is a helper function to simplify test code by encapsulating VM creation and evaluation.
    /// </summary>
    /// <param name="expression">The expression to evaluate.</param>
    /// <param name="environment">The environment value to pass to the expression.</param>
    /// <param name="parseAndEvalCountLimit">Optional limit on ParseAndEval operations (default: 100).</param>
    /// <returns>The result of evaluating the expression.</returns>
    private static PineValue EvaluateExpression(
        Expression expression,
        PineValue environment,
        int parseAndEvalCountLimit = 100)
    {
        var vm = PineVM.CreateCustom(
            evalCache: null,
            evaluationConfigDefault: null,
            reportFunctionApplication: null,
            compilationEnvClasses: null,
            disableReductionInCompilation: false,
            selectPrecompiled: null,
            skipInlineForExpression: _ => false,
            enableTailRecursionOptimization: false,
            parseCache: s_parseCache,
            precompiledLeaves: ImmutableDictionary<PineValue, Func<PineValue, PineValue?>>.Empty,
            reportEnterPrecompiledLeaf: null,
            reportExitPrecompiledLeaf: null,
            optimizationParametersSerial: null,
            cacheFileStore: null);

        var result =
            vm.EvaluateExpressionOnCustomStack(
                expression,
                environment,
                config: new PineVM.EvaluationConfig(ParseAndEvalCountLimit: parseAndEvalCountLimit))
            .Extract(err => throw new InvalidOperationException($"Evaluation failed: {err}"));

        return result.ReturnValue.Evaluate();
    }

    /// <summary>
    /// Parses a PineValue as an expression and evaluates it with the specified environment.
    /// This is a convenience overload for incremental application scenarios.
    /// </summary>
    /// <param name="encodedExpression">The encoded expression value to parse and evaluate.</param>
    /// <param name="environment">The environment value to pass to the expression.</param>
    /// <param name="parseAndEvalCountLimit">Optional limit on ParseAndEval operations (default: 100).</param>
    /// <returns>The result of evaluating the expression.</returns>
    private static PineValue EvaluateEncodedExpression(
        PineValue encodedExpression,
        PineValue environment,
        int parseAndEvalCountLimit = 100)
    {
        var parsed =
            s_parseCache.ParseExpression(encodedExpression)
            .Extract(err => throw new InvalidOperationException($"Failed to parse expression: {err}"));

        return EvaluateExpression(parsed, environment, parseAndEvalCountLimit);
    }

    #endregion
}
