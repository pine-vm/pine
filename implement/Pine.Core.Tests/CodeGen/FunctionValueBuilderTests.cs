using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.CodeGen;
using Pine.Core.CommonEncodings;
using Pine.Core.Elm;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using Xunit;

namespace Pine.Core.Tests.CodeGen;

using PineVM = Core.Interpreter.IntermediateVM.PineVM;

/// <summary>
/// Tests for the <see cref="FunctionValueBuilder"/> class that creates nested wrappers
/// for generic and partial application of Elm functions.
/// 
/// <para>
/// For background on function values and generic function application, see
/// <see href="https://github.com/pine-vm/pine/blob/fa0af408c25311d1fd3b5f6ba68d12197fcd4f8b/implement/Pine.Core/Elm/ElmCompilerInDotnet/elm-compiler-implementation-guide.md"></see>
/// </para>
/// </summary>
public class FunctionValueBuilderTests
{
    private static readonly PineVMParseCache s_parseCache = new();

    #region Zero Parameter Tests

    [Fact]
    public void EmitFunctionValue_ZeroParameters_ReturnsDirectInvocationWrapper()
    {
        // A zero-parameter function just returns a constant
        var literalResult = PineValue.Blob([42]);
        var innerExpression = Expression.LitralInst(literalResult);

        var functionValue =
            FunctionValueBuilder.EmitFunctionValueWithEnvFunctions(
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
        var innerExpression = Expression.LitralInst(expectedResult);

        var functionValue =
            FunctionValueBuilder.EmitFunctionValueWithEnvFunctions(
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
        // Inner expression expects env = [envFuncs, arg]
        // So to return arg, we access env[1]
        var innerExpression = BuildExpressionForPathInEnvironment([0, 0]);

        var functionValue =
            FunctionValueBuilder.EmitFunctionValueWithEnvFunctions(
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
        // env = [envFuncs, arg], so arg is at env[1]
        var innerExpression = BuildExpressionForPathInEnvironment([1]);

        var functionValue =
            FunctionValueBuilder.EmitFunctionValueWithEnvFunctions(
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
        // env = [envFuncs, arg]
        // envFuncs[0] is a blob
        var envFunction = PineValue.Blob([99, 88, 77]);

        // Return [envFuncs[0], arg]
        var envFuncAccess = BuildExpressionForPathInEnvironment([0, 0]);
        var argAccess = BuildExpressionForPathInEnvironment([1]);

        var innerExpression = Expression.ListInst([envFuncAccess, argAccess]);

        var functionValue =
            FunctionValueBuilder.EmitFunctionValueWithEnvFunctions(
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
        // env = [envFuncs, arg0, arg1]
        var arg0Access = BuildExpressionForPathInEnvironment([0, 0]);
        var arg1Access = BuildExpressionForPathInEnvironment([0, 1]);

        var innerExpression = Expression.ListInst([arg0Access, arg1Access]);

        var functionValue =
            FunctionValueBuilder.EmitFunctionValueWithEnvFunctions(
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
        // env = [envFuncs, arg0, arg1]
        var arg0Access = BuildExpressionForPathInEnvironment([1]);
        var arg1Access = BuildExpressionForPathInEnvironment([2]);

        var innerExpression = Expression.ListInst([arg0Access, arg1Access]);

        var functionValue =
            FunctionValueBuilder.EmitFunctionValueWithEnvFunctions(
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
        var arg0Access = BuildExpressionForPathInEnvironment([1]);
        var arg1Access = BuildExpressionForPathInEnvironment([2]);
        var arg2Access = BuildExpressionForPathInEnvironment([3]);

        var innerExpression = Expression.ListInst([arg0Access, arg1Access, arg2Access]);

        var functionValue =
            FunctionValueBuilder.EmitFunctionValueWithEnvFunctions(
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

        // Access arguments: env[1], env[2], env[3]
        var arg0Access = BuildExpressionForPathInEnvironment([1]);
        var arg1Access = BuildExpressionForPathInEnvironment([2]);
        var arg2Access = BuildExpressionForPathInEnvironment([3]);

        // Create product expression: arg0 * arg1
        var productExpr =
            Expression.BuiltinInst(
                function: nameof(BuiltinFunction.int_mul),
                input: Expression.ListInst([arg0Access, arg1Access]));

        // Create sum expression: (arg0 * arg1) + arg2

        var sumExpr =
            Expression.BuiltinInst(
                function: nameof(BuiltinFunction.int_add),
                input: Expression.ListInst([productExpr, arg2Access]));

        // Return [envFunc0, envFunc1, product]
        var innerExpression = Expression.ListInst([envFunc0Access, sumExpr, envFunc1Access]);

        var functionValue =
            FunctionValueBuilder.EmitFunctionValueWithEnvFunctions(
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
        var innerExpression = Expression.LitralInst(expectedResult);

        // Encode as nested wrapper
        var functionValue =
            FunctionValueBuilder.EmitFunctionValueWithEnvFunctions(
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
        var innerExpression = BuildExpressionForPathInEnvironment([1]);

        // Encode as nested wrapper
        var functionValue =
            FunctionValueBuilder.EmitFunctionValueWithEnvFunctions(
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
        var argAccess = BuildExpressionForPathInEnvironment([1]);
        var innerExpression = Expression.ListInst([envFuncAccess, argAccess]);

        var envFunction = PineValue.Blob([99]);

        // Encode as nested wrapper
        var functionValue =
            FunctionValueBuilder.EmitFunctionValueWithEnvFunctions(
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
        var arg0Access = BuildExpressionForPathInEnvironment([1]);
        var arg1Access = BuildExpressionForPathInEnvironment([2]);

        // sum = arg0 + arg1
        var sumExpr =
            Expression.BuiltinInst(
                function: nameof(BuiltinFunction.int_add),
                input: Expression.ListInst([arg0Access, arg1Access]));

        var innerExpression = Expression.ListInst([envFunc0Access, envFunc1Access, sumExpr]);

        var envFunc0 = PineValue.Blob([100]);
        var envFunc1 = PineValue.Blob([200]);

        // Encode as nested wrapper
        var functionValue =
            FunctionValueBuilder.EmitFunctionValueWithEnvFunctions(
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
        var innerExpression = BuildExpressionForPathInEnvironment([1]);

        var functionValue =
            FunctionValueBuilder.EmitFunctionValueWithEnvFunctions(
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
        var innerExpression = BuildExpressionForPathInEnvironment([1]);

        var functionValue =
            FunctionValueBuilder.EmitFunctionValueWithEnvFunctions(
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
        // Inner env = [envFuncs, a, b]
        var arg0 = BuildExpressionForPathInEnvironment([1]);
        var arg1 = BuildExpressionForPathInEnvironment([2]);

        var innerExpression =
            Expression.BuiltinInst(
                function: nameof(BuiltinFunction.int_add),
                input: Expression.ListInst([arg0, arg1]));

        var functionValue =
            FunctionValueBuilder.EmitFunctionValueWithEnvFunctions(
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
        var innerExpression = Expression.LitralInst(expectedResult);

        var functionValue =
            FunctionValueBuilder.EmitFunctionValueWithEnvFunctions(
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
        var innerExpression = BuildExpressionForPathInEnvironment([1]);

        var functionValue =
            FunctionValueBuilder.EmitFunctionValueWithEnvFunctions(
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

        var innerExpression =
            Expression.ListInst(
                [
                BuildExpressionForPathInEnvironment([0, 0]),
                BuildExpressionForPathInEnvironment([0, 1]),
                BuildExpressionForPathInEnvironment([1])
                ]);

        var functionValue =
            FunctionValueBuilder.EmitFunctionValueWithEnvFunctions(
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
        var innerExpression =
            Expression.ListInst(
                [
                BuildExpressionForPathInEnvironment([1]),
                BuildExpressionForPathInEnvironment([2])
                ]);

        var functionValue =
            FunctionValueBuilder.EmitFunctionValueWithEnvFunctions(
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
        var innerExpression =
            Expression.ListInst(
                [
                BuildExpressionForPathInEnvironment([1]),
                BuildExpressionForPathInEnvironment([2]),
                BuildExpressionForPathInEnvironment([3])
                ]);

        var functionValue =
            FunctionValueBuilder.EmitFunctionValueWithEnvFunctions(
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

    [Fact]
    public void ParseFunctionRecord_ThreeParameters_AfterFirstApplication_ParsesCollectedArgument()
    {
        var innerExpression =
            Expression.ListInst(
                [
                BuildExpressionForPathInEnvironment([1]),
                BuildExpressionForPathInEnvironment([2]),
                BuildExpressionForPathInEnvironment([3])
                ]);

        var functionValue =
            FunctionValueBuilder.EmitFunctionValueWithEnvFunctions(
                innerExpression,
                parameterCount: 3,
                envFunctions: []);

        var firstArgument = PineValue.Blob([42]);
        var partiallyAppliedFunction = EvaluateEncodedExpression(functionValue, firstArgument);

        var parseResult =
            FunctionRecord.ParseFunctionRecordTagged(partiallyAppliedFunction, s_parseCache);

        parseResult.IsOkOrNull().Should().NotBeNull();
        var record = parseResult.IsOkOrNull()!;

        record.ParameterCount.Should().Be(3);
        record.EnvFunctions.Length.Should().Be(0);
        record.ArgumentsAlreadyCollected.Span.ToArray().Should().Equal(firstArgument);
    }

    [Fact]
    public void ParseFunctionRecord_FourParameters_ParsesParameterCount()
    {
        var innerExpression =
            Expression.ListInst(
                [
                BuildExpressionForPathInEnvironment([1]),
                BuildExpressionForPathInEnvironment([2]),
                BuildExpressionForPathInEnvironment([3]),
                BuildExpressionForPathInEnvironment([4])
                ]);

        var functionValue =
            FunctionValueBuilder.EmitFunctionValueWithEnvFunctions(
                innerExpression,
                parameterCount: 4,
                envFunctions: []);

        // Parse the nested wrapper form
        var parseResult = FunctionRecord.ParseFunctionRecordTagged(functionValue, s_parseCache);

        parseResult.IsOkOrNull().Should().NotBeNull();
        var record = parseResult.IsOkOrNull()!;

        record.ParameterCount.Should().Be(4);
        record.EnvFunctions.Length.Should().Be(0);
        record.ArgumentsAlreadyCollected.Length.Should().Be(0);
    }

    [Theory]
    [InlineData(1, 0)]
    [InlineData(2, 0)]
    [InlineData(3, 0)]
    [InlineData(3, 1)]
    [InlineData(3, 2)]
    public void ParseFunctionRecord_CurriedTemplate_ParsesCapturedArguments(
        int parameterCount,
        int capturedArgumentCount)
    {
        var innerExpression =
            Expression.ListInst(
                Enumerable.Range(1, parameterCount)
                .Select(index => BuildExpressionForPathInEnvironment([index]))
                .ToArray());

        var envFunction = PineValue.Blob([100]);

        var functionValue =
            FunctionValueBuilder.TryBuildCurriedFunctionValueAsTemplate(
                innerExpression,
                parameterCount,
                envFunctions: [envFunction])
            ??
            throw new Exception("Failed to build curried function template");

        var capturedArguments =
            Enumerable.Range(0, capturedArgumentCount)
            .Select(index => IntegerEncoding.EncodeSignedInteger(index + 10))
            .ToArray();

        foreach (var capturedArgument in capturedArguments)
            functionValue = EvaluateEncodedExpression(functionValue, capturedArgument);

        var parseResult =
            FunctionRecord.ParseFunctionRecordTagged(functionValue, s_parseCache);

        var record =
            parseResult.IsOkOrNull()
            ??
            throw new Exception("Failed to parse curried function template: " + parseResult);

        record.ParameterCount.Should().Be(parameterCount);
        record.EnvFunctions.Span.ToArray().Should().Equal(envFunction);
        record.ArgumentsAlreadyCollected.Span.ToArray().Should().Equal(capturedArguments);
        record.UsesNestedArgFormat.Should().BeFalse();
    }

    [Fact]
    public void ParseFunctionRecord_TwoParameters_WithMultipleEnvFunctions_ParsesCorrectly()
    {
        var envFunc0 = PineValue.Blob([100]);
        var envFunc1 = PineValue.Blob([200]);
        var envFunc2 = PineValue.Blob([250]);

        var innerExpression =
            Expression.ListInst(
                [
                BuildExpressionForPathInEnvironment([0, 0]),
                BuildExpressionForPathInEnvironment([0, 1]),
                BuildExpressionForPathInEnvironment([0, 2]),
                BuildExpressionForPathInEnvironment([1]),
                BuildExpressionForPathInEnvironment([2])
                ]);

        var functionValue =
            FunctionValueBuilder.EmitFunctionValueWithEnvFunctions(
                innerExpression,
                parameterCount: 2,
                envFunctions: [envFunc0, envFunc1, envFunc2]);

        // Parse the nested wrapper form
        var parseResult = FunctionRecord.ParseFunctionRecordTagged(functionValue, s_parseCache);

        parseResult.IsOkOrNull().Should().NotBeNull();
        var record = parseResult.IsOkOrNull()!;

        record.ParameterCount.Should().Be(2);
        record.ArgumentsAlreadyCollected.Length.Should().Be(0);

        // Verify env functions are correctly parsed
        record.EnvFunctions.Length.Should().Be(3);
        record.EnvFunctions.Span[0].Should().Be(envFunc0);
        record.EnvFunctions.Span[1].Should().Be(envFunc1);
        record.EnvFunctions.Span[2].Should().Be(envFunc2);
    }

    [Fact]
    public void ParseFunctionRecord_ThreeParameters_WithMultipleEnvFunctions_ParsesCorrectly()
    {
        var envFunc0 = StringEncoding.ValueFromString("Env0");
        var envFunc1 = StringEncoding.ValueFromString("Env1");

        var innerExpression =
            Expression.ListInst(
                [
                BuildExpressionForPathInEnvironment([0, 0]),
                BuildExpressionForPathInEnvironment([0, 1]),
                BuildExpressionForPathInEnvironment([1]),
                BuildExpressionForPathInEnvironment([2]),
                BuildExpressionForPathInEnvironment([3])
                ]);

        var functionValue =
            FunctionValueBuilder.EmitFunctionValueWithEnvFunctions(
                innerExpression,
                parameterCount: 3,
                envFunctions: [envFunc0, envFunc1]);

        // Parse the nested wrapper form
        var parseResult = FunctionRecord.ParseFunctionRecordTagged(functionValue, s_parseCache);

        parseResult.IsOkOrNull().Should().NotBeNull();
        var record = parseResult.IsOkOrNull()!;

        record.ParameterCount.Should().Be(3);
        record.ArgumentsAlreadyCollected.Length.Should().Be(0);

        // Verify env functions are correctly parsed
        record.EnvFunctions.Length.Should().Be(2);
        record.EnvFunctions.Span[0].Should().Be(envFunc0);
        record.EnvFunctions.Span[1].Should().Be(envFunc1);
    }

    [Fact]
    public void ParseFunctionRecord_FourParameters_WithMultipleEnvFunctions_ParsesCorrectly()
    {
        var envFunc0 = PineValue.Blob([10]);
        var envFunc1 = PineValue.Blob([20]);
        var envFunc2 = PineValue.Blob([30]);

        var innerExpression =
            Expression.ListInst(
                [
                BuildExpressionForPathInEnvironment([0, 0]),
                BuildExpressionForPathInEnvironment([0, 1]),
                BuildExpressionForPathInEnvironment([0, 2]),
                BuildExpressionForPathInEnvironment([1]),
                BuildExpressionForPathInEnvironment([2]),
                BuildExpressionForPathInEnvironment([3]),
                BuildExpressionForPathInEnvironment([4])
                ]);

        var functionValue =
            FunctionValueBuilder.EmitFunctionValueWithEnvFunctions(
                innerExpression,
                parameterCount: 4,
                envFunctions: [envFunc0, envFunc1, envFunc2]);

        // Parse the nested wrapper form
        var parseResult = FunctionRecord.ParseFunctionRecordTagged(functionValue, s_parseCache);

        parseResult.IsOkOrNull().Should().NotBeNull();
        var record = parseResult.IsOkOrNull()!;

        record.ParameterCount.Should().Be(4);
        record.ArgumentsAlreadyCollected.Length.Should().Be(0);

        // Verify env functions are correctly parsed
        record.EnvFunctions.Length.Should().Be(3);
        record.EnvFunctions.Span[0].Should().Be(envFunc0);
        record.EnvFunctions.Span[1].Should().Be(envFunc1);
        record.EnvFunctions.Span[2].Should().Be(envFunc2);
    }

    #endregion

    #region EmitFunctionExpression Tests

    [Fact]
    public void EmitFunctionExpression_ZeroParameters_EvaluatesCorrectly()
    {
        // Inner expression returns a constant
        var expectedResult = PineValue.Blob([1, 2, 3]);
        var innerExpression = Expression.LitralInst(expectedResult);

        // Build an expression that produces the function value
        var functionExpression =
            FunctionValueBuilder.EmitFunctionExpression(
                innerExpression,
                parameterCount: 0,
                envFunctionsExprs: []);

        // Evaluate the expression to get the function value
        var functionValue = EvaluateExpression(functionExpression, PineValue.EmptyList);

        // Verify it produces the same result as EmitFunctionValue
        var expectedFunctionValue =
            FunctionValueBuilder.EmitFunctionValueWithEnvFunctions(
                innerExpression,
                parameterCount: 0,
                envFunctions: []);

        functionValue.Should().Be(expectedFunctionValue);

        // Also verify the function works correctly
        var result = EvaluateEncodedExpression(functionValue, PineValue.EmptyList);
        result.Should().Be(expectedResult);
    }

    [Fact]
    public void EmitFunctionExpression_SingleParameter_EvaluatesCorrectly()
    {
        // Inner expression returns its single argument
        // env = [envFuncs, arg], so arg is at env[1]
        var innerExpression = BuildExpressionForPathInEnvironment([1]);

        var functionExpression =
            FunctionValueBuilder.EmitFunctionExpression(
                innerExpression,
                parameterCount: 1,
                envFunctionsExprs: []);

        // Evaluate the expression to get the function value
        var functionValue = EvaluateExpression(functionExpression, PineValue.EmptyList);

        // Verify it produces the same result as EmitFunctionValue
        var expectedFunctionValue =
            FunctionValueBuilder.EmitFunctionValueWithEnvFunctions(
                innerExpression,
                parameterCount: 1,
                envFunctions: []);

        functionValue.Should().Be(expectedFunctionValue);

        // Also verify the function works correctly
        var argValue = PineValue.List([PineValue.Blob([10]), PineValue.Blob([20]), PineValue.Blob([30])]);
        var result = EvaluateEncodedExpression(functionValue, argValue);

        result.Should().Be(argValue);
    }

    [Fact]
    public void EmitFunctionExpression_SingleParameter_WithEnvFunctions()
    {
        // Inner expression accesses an env function and the argument
        // env = [envFuncs, arg]
        // envFuncs[0] is a blob
        var envFunction = PineValue.Blob([99, 88, 77]);

        // Return [envFuncs[0], arg]
        var envFuncAccess = BuildExpressionForPathInEnvironment([0, 0]);
        var argAccess = BuildExpressionForPathInEnvironment([1]);

        var innerExpression = Expression.ListInst([envFuncAccess, argAccess]);

        // Use Literal expression for env function - simplest way to test
        var envFunctionExpr = Expression.LitralInst(envFunction);

        var functionExpression =
            FunctionValueBuilder.EmitFunctionExpression(
                innerExpression,
                parameterCount: 1,
                envFunctionsExprs: [envFunctionExpr]);

        // Evaluate the expression (env doesn't matter since we use Literal)
        var functionValue = EvaluateExpression(functionExpression, PineValue.EmptyList);

        // Verify it produces the same result as EmitFunctionValue
        var expectedFunctionValue =
            FunctionValueBuilder.EmitFunctionValueWithEnvFunctions(
                innerExpression,
                parameterCount: 1,
                envFunctions: [envFunction]);

        functionValue.Should().Be(expectedFunctionValue);

        // Also verify the function works correctly
        var argValue = PineValue.Blob([1, 2, 3]);
        var result = EvaluateEncodedExpression(functionValue, argValue);

        result.Should().BeOfType<PineValue.ListValue>();
        var resultList = (PineValue.ListValue)result;
        resultList.Items.Length.Should().Be(2);
        resultList.Items.Span[0].Should().Be(envFunction);
        resultList.Items.Span[1].Should().Be(argValue);
    }

    [Fact]
    public void EmitFunctionExpression_TwoParameters_IncrementalApplication()
    {
        // Inner expression returns [arg0, arg1]
        // env = [envFuncs, arg0, arg1]
        var arg0Access = BuildExpressionForPathInEnvironment([1]);
        var arg1Access = BuildExpressionForPathInEnvironment([2]);

        var innerExpression = Expression.ListInst([arg0Access, arg1Access]);

        var functionExpression =
            FunctionValueBuilder.EmitFunctionExpression(
                innerExpression,
                parameterCount: 2,
                envFunctionsExprs: []);

        // Evaluate the expression to get the function value
        var functionValue = EvaluateExpression(functionExpression, PineValue.EmptyList);

        // TODO: Re-enable once implementation produces exact same structure
        // Verify it produces the same result as EmitFunctionValue
        // var expectedFunctionValue =
        //     PartialApplicationWrapper.EmitFunctionValue(
        //         innerExpression,
        //         parameterCount: 2,
        //         envFunctions: []);
        // functionValue.Should().Be(expectedFunctionValue);

        // Verify the function works correctly
        var arg0 = PineValue.Blob([11, 22]);
        var arg1 = PineValue.Blob([33, 44]);

        // Step 1: Apply first argument (arg0)
        var partiallyApplied = EvaluateEncodedExpression(functionValue, arg0);

        // partiallyApplied should be an encoded expression
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

    [Fact]
    public void EmitFunctionExpression_ThreeParameters_IncrementalApplication()
    {
        // Inner expression returns [arg0, arg1, arg2]
        var arg0Access = BuildExpressionForPathInEnvironment([1]);
        var arg1Access = BuildExpressionForPathInEnvironment([2]);
        var arg2Access = BuildExpressionForPathInEnvironment([3]);

        var innerExpression = Expression.ListInst([arg0Access, arg1Access, arg2Access]);

        var functionExpression =
            FunctionValueBuilder.EmitFunctionExpression(
                innerExpression,
                parameterCount: 3,
                envFunctionsExprs: []);

        // Evaluate the expression to get the function value
        var functionValue = EvaluateExpression(functionExpression, PineValue.EmptyList);

        // TODO: Re-enable once implementation produces exact same structure
        // var expectedFunctionValue =
        //     PartialApplicationWrapper.EmitFunctionValue(
        //         innerExpression,
        //         parameterCount: 3,
        //         envFunctions: []);
        // functionValue.Should().Be(expectedFunctionValue);

        // Verify the function works correctly
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
    public void EmitFunctionExpression_ThreeParameters_WithEnvFunctions_IncrementalApplication()
    {
        // Inner expression returns [envFunc0, (arg0 * arg1 + arg2), envFunc1]
        // This test verifies that environment functions are correctly captured from the outer
        // environment during the first evaluation.

        var envFunc0 =
            StringEncoding.ValueFromString("Arancini");

        var envFunc1 =
            StringEncoding.ValueFromString("Biscotti");

        // Access env functions: env[0][0] and env[0][1]
        var envFunc0Access = BuildExpressionForPathInEnvironment([0, 0]);
        var envFunc1Access = BuildExpressionForPathInEnvironment([0, 1]);

        // Access arguments: env[1], env[2], env[3]
        var arg0Access = BuildExpressionForPathInEnvironment([1]);
        var arg1Access = BuildExpressionForPathInEnvironment([2]);
        var arg2Access = BuildExpressionForPathInEnvironment([3]);

        // Create product expression: arg0 * arg1
        var productExpr =
            Expression.BuiltinInst(
                function: nameof(BuiltinFunction.int_mul),
                input: Expression.ListInst([arg0Access, arg1Access]));

        // Create sum expression: (arg0 * arg1) + arg2

        var sumExpr =
            Expression.BuiltinInst(
                function: nameof(BuiltinFunction.int_add),
                input: Expression.ListInst([productExpr, arg2Access]));

        // Return [envFunc0, sum, envFunc1]
        var innerExpression = Expression.ListInst([envFunc0Access, sumExpr, envFunc1Access]);

        // Use environment-dependent expressions for env functions.
        // These expressions reference the outer environment where the function expression will be evaluated.
        // outer env = [envFunc0, envFunc1]
        var envFuncExpr0 = BuildExpressionForPathInEnvironment([0]);
        var envFuncExpr1 = BuildExpressionForPathInEnvironment([1]);

        var functionExpression =
            FunctionValueBuilder.EmitFunctionExpression(
                innerExpression,
                parameterCount: 3,
                envFunctionsExprs: [envFuncExpr0, envFuncExpr1]);

        // Evaluate the expression with an outer environment containing the env functions.
        var outerEnv = PineValue.List([envFunc0, envFunc1]);
        var functionValue = EvaluateExpression(functionExpression, outerEnv);

        // Verify the function value matches what EmitFunctionValueWithEnvFunctions produces
        var expectedFunctionValue =
            FunctionValueBuilder.EmitFunctionValueWithEnvFunctions(
                innerExpression,
                parameterCount: 3,
                envFunctions: [envFunc0, envFunc1]);

        functionValue.Should().Be(expectedFunctionValue);

        // Verify the function works correctly
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

    [Fact]
    public void EmitFunctionExpression_TwoParameters_WithEnvFunctions_IncrementalApplication()
    {
        // A function that uses env functions and returns [envFunc0, envFunc1, arg0 + arg1]
        // The env functions are fetched from the outer environment (not literals),
        // so they must be captured during the first evaluation.
        var envFunc0Access = BuildExpressionForPathInEnvironment([0, 0]);
        var envFunc1Access = BuildExpressionForPathInEnvironment([0, 1]);
        var arg0Access = BuildExpressionForPathInEnvironment([1]);
        var arg1Access = BuildExpressionForPathInEnvironment([2]);

        // sum = arg0 + arg1
        var sumExpr =
            Expression.BuiltinInst(
                function: nameof(BuiltinFunction.int_add),
                input: Expression.ListInst([arg0Access, arg1Access]));

        var innerExpression = Expression.ListInst([envFunc0Access, envFunc1Access, sumExpr]);

        var envFunc0 = PineValue.Blob([100]);
        var envFunc1 = PineValue.Blob([200]);

        // Use environment-dependent expressions for env functions.
        // These expressions reference the outer environment where the function expression will be evaluated.
        // outer env = [envFunc0, envFunc1]
        var envFuncExpr0 = BuildExpressionForPathInEnvironment([0]);
        var envFuncExpr1 = BuildExpressionForPathInEnvironment([1]);

        var functionExpression =
            FunctionValueBuilder.EmitFunctionExpression(
                innerExpression,
                parameterCount: 2,
                envFunctionsExprs: [envFuncExpr0, envFuncExpr1]);

        // Evaluate the expression with an outer environment containing the env functions.
        // The function expression should capture these values during this first evaluation.
        var outerEnv = PineValue.List([envFunc0, envFunc1]);
        var functionValue = EvaluateExpression(functionExpression, outerEnv);

        // Verify the function value matches what EmitFunctionValueWithEnvFunctions produces
        var expectedFunctionValue =
            FunctionValueBuilder.EmitFunctionValueWithEnvFunctions(
                innerExpression,
                parameterCount: 2,
                envFunctions: [envFunc0, envFunc1]);

        functionValue.Should().Be(expectedFunctionValue);

        // Verify the function works correctly
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

    #endregion


    #region BuildCurriedFunctionValueAsTemplate Tests

    [Fact]
    public void BuildCurriedFunctionValueAsTemplate_OneParameter()
    {
        var innerExpr =
            Expression.BuiltinInst(
                function: nameof(KernelFunction.int_add),
                input: Expression.ListInst(
                    [
                    Expression.BuiltinInst(
                        function: nameof(KernelFunction.int_mul),
                        input: Expression.ListInst(
                            [
                            BuildExpressionForPathInEnvironment([1]),
                            Expression.LitralInst(IntegerEncoding.EncodeSignedInteger(17))
                            ])),

                    Expression.LitralInst(IntegerEncoding.EncodeSignedInteger(19))
                    ]));

        var templateValue =
            FunctionValueBuilder.TryBuildCurriedFunctionValueAsTemplate(
                innerExpr,
                parameterCount: 1);

        templateValue.Should().NotBeNull();

        var parseFirstLevelResult =
            ExpressionEncoding.ParseExpressionFromValue(templateValue);

        if (parseFirstLevelResult.IsErrOrNull() is { } err)
        {
            throw new Exception($"Failed to parse template value: {err}");
        }

        var parseFirstLevelOk =
            parseFirstLevelResult.IsOkOrNull()
            ??
            throw new NotImplementedException(
                "Unexpected result type from parsing: " + parseFirstLevelResult);

        parseFirstLevelOk.BuiltinCount.Should().Be(0);

        var plainApplicationExpr =
            new Expression.Eval(
                encoded: Expression.LitralInst(templateValue),
                environment: Expression.LitralInst(IntegerEncoding.EncodeSignedInteger(41)));

        var evaluated =
            EvaluateExpression(plainApplicationExpr, PineValue.List([]));

        evaluated.Should().NotBeNull();

        var expectedValue =
            IntegerEncoding.EncodeSignedInteger(17 * 41 + 19);

        var expectedValueRendering =
            RenderValueAsElmExpression(expectedValue);

        var evaluatedRendering =
            RenderValueAsElmExpression(evaluated);

        evaluatedRendering.Should().Be(expectedValueRendering);
    }

    [Fact]
    public void BuildCurriedFunctionValueAsTemplate_TwoParameters()
    {
        var innerExpr =
            Expression.BuiltinInst(
                function: nameof(KernelFunction.int_add),
                input: Expression.ListInst(
                    [
                    Expression.BuiltinInst(
                        function: nameof(KernelFunction.int_mul),
                        input: Expression.ListInst(
                            [
                            BuildExpressionForPathInEnvironment([1]),
                            Expression.LitralInst(IntegerEncoding.EncodeSignedInteger(17))
                            ])),

                    Expression.LitralInst(IntegerEncoding.EncodeSignedInteger(19)),

                    BuildExpressionForPathInEnvironment([2]),
                    ]));

        var templateValue =
            FunctionValueBuilder.TryBuildCurriedFunctionValueAsTemplate(
                innerExpr,
                parameterCount: 2);

        templateValue.Should().NotBeNull();

        {
            // Assert template composition properties

            var parseFirstLevelResult =
                ExpressionEncoding.ParseExpressionFromValue(templateValue);

            {
                if (parseFirstLevelResult.IsErrOrNull() is { } err)
                {
                    throw new Exception($"Failed to parse template value: {err}");
                }
            }

            var parseFirstLevelOk =
                parseFirstLevelResult.IsOkOrNull()
                ??
                throw new NotImplementedException(
                    "Unexpected result type from parsing: " + parseFirstLevelResult);

            parseFirstLevelOk.BuiltinCount.Should().Be(0);
            parseFirstLevelOk.EvalCount.Should().Be(0);

            var simulateFirstApplication =
                EvaluateExpression(
                    parseFirstLevelOk,
                    IntegerEncoding.EncodeSignedInteger(1234567));

            var parseSecondLevelResult =
                ExpressionEncoding.ParseExpressionFromValue(simulateFirstApplication);

            {
                if (parseSecondLevelResult.IsErrOrNull() is { } err)
                {
                    throw new Exception($"Failed to parse template value: {err}");
                }
            }

            var parseSecondLevelOk =
                parseSecondLevelResult.IsOkOrNull()
                ??
                throw new NotImplementedException(
                    "Unexpected result type from parsing: " + parseSecondLevelResult);

            parseSecondLevelOk.BuiltinCount.Should().Be(0);
        }

        var firstArgPlainApplicationExpr =
            new Expression.Eval(
                encoded: Expression.LitralInst(templateValue),
                environment: Expression.LitralInst(IntegerEncoding.EncodeSignedInteger(41)));

        var secondArgPlainApplicationExpr =
            new Expression.Eval(
                encoded: firstArgPlainApplicationExpr,
                environment: Expression.LitralInst(IntegerEncoding.EncodeSignedInteger(43)));

        var evaluated =
            EvaluateExpression(secondArgPlainApplicationExpr, PineValue.List([]));

        evaluated.Should().NotBeNull();

        var expectedValue =
            IntegerEncoding.EncodeSignedInteger(17 * 41 + 19 + 43);

        var expectedValueRendering =
            RenderValueAsElmExpression(expectedValue);

        var evaluatedRendering =
            RenderValueAsElmExpression(evaluated);

        evaluatedRendering.Should().Be(expectedValueRendering);
    }

    [Fact]
    public void BuildCurriedFunctionValueAsTemplate_OneParameter_WithEnvFunctions()
    {
        // env = [envFunctions, arg0]
        // envFunctions[0] is at path [0, 0]; arg0 is at path [1].
        var envFunc0 = IntegerEncoding.EncodeSignedInteger(100);

        var innerExpr =
            Expression.BuiltinInst(
                function: nameof(KernelFunction.int_add),
                input: Expression.ListInst(
                    [
                    Expression.BuiltinInst(
                        function: nameof(KernelFunction.int_mul),
                        input: Expression.ListInst(
                            [
                            BuildExpressionForPathInEnvironment([1]),
                            Expression.LitralInst(IntegerEncoding.EncodeSignedInteger(17))
                            ])),

                    Expression.LitralInst(IntegerEncoding.EncodeSignedInteger(19)),

                    BuildExpressionForPathInEnvironment([0, 0]),
                    ]));

        var templateValue =
            FunctionValueBuilder.TryBuildCurriedFunctionValueAsTemplate(
                innerExpr,
                parameterCount: 1,
                envFunctions: [envFunc0]);

        templateValue.Should().NotBeNull();

        var parseFirstLevelResult =
            ExpressionEncoding.ParseExpressionFromValue(templateValue);

        if (parseFirstLevelResult.IsErrOrNull() is { } err)
        {
            throw new Exception($"Failed to parse template value: {err}");
        }

        var parseFirstLevelOk =
            parseFirstLevelResult.IsOkOrNull()
            ??
            throw new NotImplementedException(
                "Unexpected result type from parsing: " + parseFirstLevelResult);

        parseFirstLevelOk.BuiltinCount.Should().Be(0);

        var plainApplicationExpr =
            new Expression.Eval(
                encoded: Expression.LitralInst(templateValue),
                environment: Expression.LitralInst(IntegerEncoding.EncodeSignedInteger(41)));

        var evaluated =
            EvaluateExpression(plainApplicationExpr, PineValue.List([]));

        evaluated.Should().NotBeNull();

        var expectedValue =
            IntegerEncoding.EncodeSignedInteger(17 * 41 + 19 + 100);

        var expectedValueRendering =
            RenderValueAsElmExpression(expectedValue);

        var evaluatedRendering =
            RenderValueAsElmExpression(evaluated);

        evaluatedRendering.Should().Be(expectedValueRendering);
    }

    [Fact]
    public void BuildCurriedFunctionValueAsTemplate_TwoParameters_WithEnvFunctions()
    {
        // env = [envFunctions, arg0, arg1]
        // envFunctions[0] is at path [0, 0]; envFunctions[1] is at path [0, 1];
        // arg0 is at path [1]; arg1 is at path [2].
        var envFunc0 = IntegerEncoding.EncodeSignedInteger(100);
        var envFunc1 = IntegerEncoding.EncodeSignedInteger(200);

        var innerExpr =
            Expression.BuiltinInst(
                function: nameof(KernelFunction.int_add),
                input: Expression.ListInst(
                    [
                    Expression.BuiltinInst(
                        function: nameof(KernelFunction.int_mul),
                        input: Expression.ListInst(
                            [
                            BuildExpressionForPathInEnvironment([1]),
                            Expression.LitralInst(IntegerEncoding.EncodeSignedInteger(17))
                            ])),

                    Expression.LitralInst(IntegerEncoding.EncodeSignedInteger(19)),

                    BuildExpressionForPathInEnvironment([2]),

                    BuildExpressionForPathInEnvironment([0, 0]),

                    BuildExpressionForPathInEnvironment([0, 1]),
                    ]));

        var templateValue =
            FunctionValueBuilder.TryBuildCurriedFunctionValueAsTemplate(
                innerExpr,
                parameterCount: 2,
                envFunctions: [envFunc0, envFunc1]);

        templateValue.Should().NotBeNull();

        {
            // Assert template composition properties

            var parseFirstLevelResult =
                ExpressionEncoding.ParseExpressionFromValue(templateValue);

            {
                if (parseFirstLevelResult.IsErrOrNull() is { } err)
                {
                    throw new Exception($"Failed to parse template value: {err}");
                }
            }

            var parseFirstLevelOk =
                parseFirstLevelResult.IsOkOrNull()
                ??
                throw new NotImplementedException(
                    "Unexpected result type from parsing: " + parseFirstLevelResult);

            parseFirstLevelOk.BuiltinCount.Should().Be(0);
            parseFirstLevelOk.EvalCount.Should().Be(0);

            var simulateFirstApplication =
                EvaluateExpression(
                    parseFirstLevelOk,
                    IntegerEncoding.EncodeSignedInteger(1234567));

            var parseSecondLevelResult =
                ExpressionEncoding.ParseExpressionFromValue(simulateFirstApplication);

            {
                if (parseSecondLevelResult.IsErrOrNull() is { } err)
                {
                    throw new Exception($"Failed to parse template value: {err}");
                }
            }

            var parseSecondLevelOk =
                parseSecondLevelResult.IsOkOrNull()
                ??
                throw new NotImplementedException(
                    "Unexpected result type from parsing: " + parseSecondLevelResult);

            parseSecondLevelOk.BuiltinCount.Should().Be(0);
        }

        var firstArgPlainApplicationExpr =
            new Expression.Eval(
                encoded: Expression.LitralInst(templateValue),
                environment: Expression.LitralInst(IntegerEncoding.EncodeSignedInteger(41)));

        var secondArgPlainApplicationExpr =
            new Expression.Eval(
                encoded: firstArgPlainApplicationExpr,
                environment: Expression.LitralInst(IntegerEncoding.EncodeSignedInteger(43)));

        var evaluated =
            EvaluateExpression(secondArgPlainApplicationExpr, PineValue.List([]));

        evaluated.Should().NotBeNull();

        var expectedValue =
            IntegerEncoding.EncodeSignedInteger(17 * 41 + 19 + 43 + 100 + 200);

        var expectedValueRendering =
            RenderValueAsElmExpression(expectedValue);

        var evaluatedRendering =
            RenderValueAsElmExpression(evaluated);

        evaluatedRendering.Should().Be(expectedValueRendering);
    }

    #endregion

    #region ParseFunctionValue Symmetry Tests

    [Fact]
    public void ParseFunctionValue_WithEnvFunctions_ZeroParameters_SymmetryTest()
    {
        var expectedResult = PineValue.Blob([42, 43]);
        var innerExpression = Expression.LitralInst(expectedResult);

        var functionValue =
            FunctionValueBuilder.EmitFunctionValueWithEnvFunctions(
                innerExpression,
                parameterCount: 0,
                envFunctions: []);

        // Parse using the new ParseFunctionValue method
        var parseResult = FunctionRecord.ParseFunctionValue(functionValue, s_parseCache);

        parseResult.IsOkOrNull().Should().NotBeNull();
        var parsed = parseResult.IsOkOrNull()!;

        // Assert on the complete parse result
        parsed.Should().Be(
            new ParsedFunctionValue.WithEnvFunctions(
                InnerFunction: innerExpression,
                ParameterCount: 0,
                EnvFunctions: []));
    }

    [Fact]
    public void ParseFunctionValue_WithEnvFunctions_SingleParameter_SymmetryTest()
    {
        // Inner expression: returns [envFuncs[0], arg]
        // env = [envFuncs, arg]
        var envFunc = PineValue.Blob([99, 88]);

        var innerExpression =
            Expression.ListInst(
                [
                BuildExpressionForPathInEnvironment([0, 0]), // envFuncs[0]
                BuildExpressionForPathInEnvironment([1]) // arg
                ]);

        var functionValue =
            FunctionValueBuilder.EmitFunctionValueWithEnvFunctions(
                innerExpression,
                parameterCount: 1,
                envFunctions: [envFunc]);

        var parsed =
            FunctionRecord.ParseFunctionValue(functionValue, s_parseCache)
            .Extract(err => throw new InvalidOperationException($"Parse failed: {err}"));

        // Assert on the complete parse result
        parsed.Should().Be(
            new ParsedFunctionValue.WithEnvFunctions(
                InnerFunction: innerExpression,
                ParameterCount: 1,
                EnvFunctions: [envFunc]));

        // Verify the function works correctly (symmetry)
        var argValue = PineValue.Blob([1, 2, 3]);
        var result = EvaluateEncodedExpression(functionValue, argValue);

        result.Should().Be(PineValue.List([envFunc, argValue]));
    }

    [Fact]
    public void ParseFunctionValue_WithEnvFunctions_TwoParameters_SymmetryTest()
    {
        // Inner expression: returns [arg0, arg1]
        // env = [envFuncs, arg0, arg1]
        var innerExpression =
            Expression.ListInst(
                [
                BuildExpressionForPathInEnvironment([1]), // arg0
                BuildExpressionForPathInEnvironment([2]) // arg1
                ]);

        var functionValue =
            FunctionValueBuilder.EmitFunctionValueWithEnvFunctions(
                innerExpression,
                parameterCount: 2,
                envFunctions: []);

        var parsed =
            FunctionRecord.ParseFunctionValue(functionValue, s_parseCache)
            .Extract(err => throw new InvalidOperationException($"Parse failed: {err}"));

        // Assert on the complete parse result
        parsed.Should().Be(
            new ParsedFunctionValue.WithEnvFunctions(
                InnerFunction: innerExpression,
                ParameterCount: 2,
                EnvFunctions: []));

        // Verify the function works correctly (symmetry)
        var arg0 = PineValue.Blob([10]);
        var arg1 = PineValue.Blob([20]);

        var partial = EvaluateEncodedExpression(functionValue, arg0);
        var result = EvaluateEncodedExpression(partial, arg1);

        result.Should().Be(PineValue.List([arg0, arg1]));
    }

    [Fact]
    public void ParseFunctionValue_WithEnvFunctions_ThreeParameters_SymmetryTest()
    {
        // Inner expression: returns [envFuncs[0], arg0, arg1, arg2]
        // env = [envFuncs, arg0, arg1, arg2]
        var envFunc = PineValue.Blob([77]);

        var innerExpression =
            Expression.ListInst(
                [
                BuildExpressionForPathInEnvironment([0, 0]), // envFuncs[0]
                BuildExpressionForPathInEnvironment([1]), // arg0
                BuildExpressionForPathInEnvironment([2]), // arg1
                BuildExpressionForPathInEnvironment([3]) // arg2
                ]);

        var functionValue =
            FunctionValueBuilder.EmitFunctionValueWithEnvFunctions(
                innerExpression,
                parameterCount: 3,
                envFunctions: [envFunc]);

        var parsed =
            FunctionRecord.ParseFunctionValue(functionValue, s_parseCache)
            .Extract(err => throw new InvalidOperationException($"Parse failed: {err}"));

        // Assert on the complete parse result
        parsed.Should().Be(
            new ParsedFunctionValue.WithEnvFunctions(
                InnerFunction: innerExpression,
                ParameterCount: 3,
                EnvFunctions: [envFunc]));

        // Verify the function works correctly (symmetry)
        var arg0 = PineValue.Blob([1]);
        var arg1 = PineValue.Blob([2]);
        var arg2 = PineValue.Blob([3]);

        var partial1 = EvaluateEncodedExpression(functionValue, arg0);
        var partial2 = EvaluateEncodedExpression(partial1, arg1);
        var result = EvaluateEncodedExpression(partial2, arg2);

        result.Should().Be(PineValue.List([envFunc, arg0, arg1, arg2]));
    }

    [Fact]
    public void ParseFunctionValue_WithEnvFunctions_InnerExpressionPreserved()
    {
        // Create a distinct inner expression that we can verify is preserved
        var arg0Access = BuildExpressionForPathInEnvironment([1]);
        var arg1Access = BuildExpressionForPathInEnvironment([2]);

        var innerExpression =
            Expression.BuiltinInst(
                function: nameof(BuiltinFunction.int_add),
                input: Expression.ListInst([arg0Access, arg1Access]));

        var envFunc = PineValue.Blob([100]);

        var functionValue =
            FunctionValueBuilder.EmitFunctionValueWithEnvFunctions(
                innerExpression,
                parameterCount: 2,
                envFunctions: [envFunc]);

        var parsed =
            FunctionRecord.ParseFunctionValue(functionValue, s_parseCache)
            .Extract(err => throw new InvalidOperationException($"Parse failed: {err}"));

        // The inner function expression should be parseable and evaluate correctly
        // We verify by testing the entire function works
        var a = IntegerEncoding.EncodeSignedInteger(5);
        var b = IntegerEncoding.EncodeSignedInteger(7);

        var partial = EvaluateEncodedExpression(functionValue, a);
        var result = EvaluateEncodedExpression(partial, b);

        result.Should().Be(IntegerEncoding.EncodeSignedInteger(12));
    }

    #endregion


    #region EmitCurriedFunctionTemplateWithLeadingArgs (partial application) Tests

    // Distinct multipliers (primes) used to build a linear inner expression so that every argument
    // (and the env function) contributes a uniquely identifiable amount to the final result. This makes
    // accidental slot mix-ups observable in the computed value.
    private static readonly long[] s_partialAppCoefficients = [3, 5, 7, 11, 13];

    private const long s_partialAppConstant = 101;

    private const long s_partialAppEnvFuncCoefficient = 17;

    [Theory]
    // No env functions; vary total parameter count (2..5) and the number of leading args available at
    // the partial application site (0 .. parameterCount - 1).
    [InlineData(2, 0)]
    [InlineData(2, 1)]
    [InlineData(3, 0)]
    [InlineData(3, 1)]
    [InlineData(3, 2)]
    [InlineData(4, 0)]
    [InlineData(4, 1)]
    [InlineData(4, 2)]
    [InlineData(4, 3)]
    [InlineData(5, 0)]
    [InlineData(5, 1)]
    [InlineData(5, 2)]
    [InlineData(5, 3)]
    [InlineData(5, 4)]
    public void EmitCurriedFunctionTemplateWithLeadingArgs_MatchesPlainPartialApplication(
        int parameterCount,
        int leadingCount)
    {
        AssertPartialApplicationConsistency(parameterCount, leadingCount, withEnvFunctions: false);
    }

    [Theory]
    // Same coverage but with a non-empty env functions list placed at environment index 0.
    [InlineData(2, 0)]
    [InlineData(2, 1)]
    [InlineData(3, 0)]
    [InlineData(3, 1)]
    [InlineData(3, 2)]
    [InlineData(4, 0)]
    [InlineData(4, 1)]
    [InlineData(4, 2)]
    [InlineData(4, 3)]
    [InlineData(5, 0)]
    [InlineData(5, 1)]
    [InlineData(5, 2)]
    [InlineData(5, 3)]
    [InlineData(5, 4)]
    public void EmitCurriedFunctionTemplateWithLeadingArgs_WithEnvFunctions_MatchesPlainPartialApplication(
        int parameterCount,
        int leadingCount)
    {
        AssertPartialApplicationConsistency(parameterCount, leadingCount, withEnvFunctions: true);
    }

    [Fact]
    public void EmitCurriedFunctionTemplateWithLeadingArgs_EmbeddedArgsEvaluatedAtPartialApplicationSite()
    {
        // The leading argument expressions must be evaluated at the partial application site (i.e. against
        // the environment in which the emitted expression is evaluated), not embedded as constants. Here we
        // read the leading arguments out of the partial-application-site environment to confirm this.
        const int parameterCount = 4;
        const int leadingCount = 2;

        var innerExpr = BuildLinearInnerExpr(parameterCount, withEnvFunctions: false);

        var leadingValues = new[]
        {
            IntegerEncoding.EncodeSignedInteger(40),
            IntegerEncoding.EncodeSignedInteger(41),
        };

        // The partial application site environment is the list [leadingArg0, leadingArg1]; the embedded
        // argument expressions read items 0 and 1 from it.
        var siteEnvironment = PineValue.List([.. leadingValues]);

        var leadingArgExpressions = new Expression[]
        {
            ListItemFromIndex(0, Expression.EnvironmentInstance),
            ListItemFromIndex(1, Expression.EnvironmentInstance),
        };

        var emittedExpr =
            FunctionValueBuilder.EmitCurriedFunctionTemplateWithLeadingArgs(
                innerExpr,
                parameterCount,
                leadingArgExpressions);

        var emittedIntermediate =
            EvaluateExpression(emittedExpr, siteEnvironment);

        // Build the reference intermediate value by applying the same leading argument values, one at a
        // time, to the plain (zero-arg) template function value.
        var plainTemplate =
            FunctionValueBuilder.TryBuildCurriedFunctionValueAsTemplate(innerExpr, parameterCount);

        plainTemplate.Should().NotBeNull();

        var referenceIntermediate = plainTemplate;

        for (var i = 0; i < leadingCount; i++)
        {
            referenceIntermediate =
                EvaluateEncodedExpression(referenceIntermediate, leadingValues[i]);
        }

        emittedIntermediate.Should().Be(referenceIntermediate);
    }

    /// <summary>
    /// Verifies that the intermediate value emitted by
    /// <see cref="FunctionValueBuilder.EmitCurriedFunctionTemplateWithLeadingArgs"/> for
    /// <paramref name="leadingCount"/> leading arguments is identical to the intermediate value obtained by
    /// applying those same arguments, one at a time, to the plain (zero captured args) template function
    /// value produced by <see cref="FunctionValueBuilder.TryBuildCurriedFunctionValueAsTemplate"/>. It then
    /// applies the remaining arguments to both intermediate values and checks that the fully applied result
    /// matches the expected value computed independently.
    /// </summary>
    private static void AssertPartialApplicationConsistency(
        int parameterCount,
        int leadingCount,
        bool withEnvFunctions)
    {
        leadingCount.Should().BeInRange(0, parameterCount - 1);
        parameterCount.Should().BeLessThanOrEqualTo(s_partialAppCoefficients.Length);

        var envFunctions =
            withEnvFunctions
            ? new[] { IntegerEncoding.EncodeSignedInteger(1000) }
            : null;

        var innerExpr = BuildLinearInnerExpr(parameterCount, withEnvFunctions);

        // Concrete argument values, chosen distinct and non-trivial.
        var argValues = new PineValue[parameterCount];

        for (var i = 0; i < parameterCount; i++)
        {
            argValues[i] = IntegerEncoding.EncodeSignedInteger(50 + (i * 7) + 1);
        }

        // Reference intermediate: apply the leading arguments to the plain template, one at a time.
        var plainTemplate =
            FunctionValueBuilder.TryBuildCurriedFunctionValueAsTemplate(
                innerExpr,
                parameterCount,
                envFunctions);

        plainTemplate.Should().NotBeNull();

        var referenceIntermediate = plainTemplate;

        for (var i = 0; i < leadingCount; i++)
        {
            referenceIntermediate =
                EvaluateEncodedExpression(referenceIntermediate, argValues[i]);
        }

        // Emitted intermediate: embed the leading arguments directly at emission time.
        var leadingArgExpressions = new Expression[leadingCount];

        for (var i = 0; i < leadingCount; i++)
        {
            leadingArgExpressions[i] = Expression.LitralInst(argValues[i]);
        }

        var emittedExpr =
            FunctionValueBuilder.EmitCurriedFunctionTemplateWithLeadingArgs(
                innerExpr,
                parameterCount,
                leadingArgExpressions,
                envFunctions);

        var emittedIntermediate =
            EvaluateExpression(emittedExpr, PineValue.EmptyList);

        // The core property: both ways of obtaining the intermediate value must agree exactly.
        emittedIntermediate.Should().Be(
            referenceIntermediate,
            "the compact emission must produce the same intermediate value as applying the leading " +
            "arguments to the plain template");

        // The emitted intermediate must itself be a parseable expression with no builtin/kernel/eval nodes,
        // i.e. it is a clean template just like the plain partial-application intermediate.
        var parsedIntermediate =
            ExpressionEncoding.ParseExpressionFromValue(emittedIntermediate)
            .Extract(err => throw new InvalidOperationException($"Failed to parse intermediate: {err}"));

        parsedIntermediate.BuiltinCount.Should().Be(0);

        // Apply the remaining arguments to both intermediate values; both must reach the same final result,
        // which must equal the independently computed expected value.
        var expectedFinal =
            IntegerEncoding.EncodeSignedInteger(
                ComputeExpectedResult(parameterCount, argValues, withEnvFunctions));

        var emittedFinal = emittedIntermediate;
        var referenceFinal = referenceIntermediate;

        for (var i = leadingCount; i < parameterCount; i++)
        {
            emittedFinal = EvaluateEncodedExpression(emittedFinal, argValues[i]);
            referenceFinal = EvaluateEncodedExpression(referenceFinal, argValues[i]);
        }

        emittedFinal.Should().Be(expectedFinal);
        referenceFinal.Should().Be(expectedFinal);
    }

    /// <summary>
    /// Builds an inner function body computing a linear combination of the arguments (and optionally a
    /// single env function), so that each argument contributes a distinct, identifiable amount:
    /// <c>sum_i (arg_i * coeff_i) + constant [+ envFunc0 * envCoeff]</c>.
    /// The environment layout is <c>[envFunctions, arg_0, ..., arg_{n-1}]</c>; arg_i is at path [1 + i] and
    /// envFunc0 is at path [0, 0].
    /// </summary>
    private static Expression BuildLinearInnerExpr(int parameterCount, bool withEnvFunctions)
    {
        var terms = new List<Expression>(parameterCount + 2);

        for (var i = 0; i < parameterCount; i++)
        {
            terms.Add(
                Expression.BuiltinInst(
                    function: nameof(KernelFunction.int_mul),
                    input: Expression.ListInst(
                        [
                        BuildExpressionForPathInEnvironment([1 + i]),
                        Expression.LitralInst(
                            IntegerEncoding.EncodeSignedInteger(s_partialAppCoefficients[i]))
                        ])));
        }

        if (withEnvFunctions)
        {
            terms.Add(
                Expression.BuiltinInst(
                    function: nameof(KernelFunction.int_mul),
                    input: Expression.ListInst(
                        [
                        BuildExpressionForPathInEnvironment([0, 0]),
                        Expression.LitralInst(
                            IntegerEncoding.EncodeSignedInteger(s_partialAppEnvFuncCoefficient))
                        ])));
        }

        terms.Add(Expression.LitralInst(IntegerEncoding.EncodeSignedInteger(s_partialAppConstant)));

        return
            Expression.BuiltinInst(
                function: nameof(KernelFunction.int_add),
                input: Expression.ListInst(terms));
    }

    private static long ComputeExpectedResult(
        int parameterCount,
        IReadOnlyList<PineValue> argValues,
        bool withEnvFunctions)
    {
        var sum = s_partialAppConstant;

        for (var i = 0; i < parameterCount; i++)
        {
            var argInt =
                IntegerEncoding.ParseSignedIntegerStrict(argValues[i])
                .Extract(err => throw new InvalidOperationException($"Failed to parse arg: {err}"));

            sum += (long)argInt * s_partialAppCoefficients[i];
        }

        if (withEnvFunctions)
        {
            sum += 1000 * s_partialAppEnvFuncCoefficient;
        }

        return sum;
    }

    [Theory]
    // No env functions; vary total parameter count (2..5) and the number of leading args available at
    // the partial application site (0 .. parameterCount - 1).
    [InlineData(2, 0)]
    [InlineData(2, 1)]
    [InlineData(3, 0)]
    [InlineData(3, 1)]
    [InlineData(3, 2)]
    [InlineData(4, 0)]
    [InlineData(4, 1)]
    [InlineData(4, 2)]
    [InlineData(4, 3)]
    [InlineData(5, 0)]
    [InlineData(5, 1)]
    [InlineData(5, 2)]
    [InlineData(5, 3)]
    [InlineData(5, 4)]
    public void EmitCurriedFunctionTemplateFromEncodedBodyExpression_MatchesPlainPartialApplication(
        int parameterCount,
        int leadingCount)
    {
        AssertPartialApplicationFromExpressionConsistency(
            parameterCount, leadingCount, withEnvFunctions: false);
    }

    [Theory]
    // Same coverage but with a non-empty env functions list produced at environment index 0.
    [InlineData(2, 0)]
    [InlineData(2, 1)]
    [InlineData(3, 0)]
    [InlineData(3, 1)]
    [InlineData(3, 2)]
    [InlineData(4, 0)]
    [InlineData(4, 1)]
    [InlineData(4, 2)]
    [InlineData(4, 3)]
    [InlineData(5, 0)]
    [InlineData(5, 1)]
    [InlineData(5, 2)]
    [InlineData(5, 3)]
    [InlineData(5, 4)]
    public void EmitCurriedFunctionTemplateFromEncodedBodyExpression_WithEnvFunctions_MatchesPlainPartialApplication(
        int parameterCount,
        int leadingCount)
    {
        AssertPartialApplicationFromExpressionConsistency(
            parameterCount, leadingCount, withEnvFunctions: true);
    }

    [Fact]
    public void EmitCurriedFunctionTemplateFromEncodedBodyExpression_EmbedsBodyAndEnvFunctionsFromSite()
    {
        // The encoded body and the env-functions list must be read at the partial application site (i.e.
        // against the environment in which the emitted expression is evaluated), not embedded as constants
        // at emission time. Here we read both out of the partial-application-site environment to confirm
        // this, then verify the produced template behaves identically to the static (value-based) variant.
        const int parameterCount = 3;
        const int leadingCount = 1;

        var innerExpr = BuildLinearInnerExpr(parameterCount, withEnvFunctions: true);

        var innerExprEncoded = ExpressionEncoding.EncodeExpressionAsValue(innerExpr);

        var envFunctionsValue =
            PineValue.List([IntegerEncoding.EncodeSignedInteger(1000)]);

        var argValues = new PineValue[parameterCount];

        for (var i = 0; i < parameterCount; i++)
        {
            argValues[i] = IntegerEncoding.EncodeSignedInteger(50 + (i * 7) + 1);
        }

        var leadingArgExpressions = new Expression[leadingCount];

        for (var i = 0; i < leadingCount; i++)
        {
            leadingArgExpressions[i] = Expression.LitralInst(argValues[i]);
        }

        // The partial-application-site environment is [encodedBody, envFunctionsList]; the body and
        // env-functions expressions read items 0 and 1 from it.
        var siteEnvironment = PineValue.List([innerExprEncoded, envFunctionsValue]);

        var emittedExpr =
            FunctionValueBuilder.EmitCurriedFunctionTemplateWithLeadingArgsFromEncodedBodyExpression(
                ListItemFromIndex(0, Expression.EnvironmentInstance),
                parameterCount,
                leadingArgExpressions,
                ListItemFromIndex(1, Expression.EnvironmentInstance));

        var emittedIntermediate =
            EvaluateExpression(emittedExpr, siteEnvironment);

        // The static (value-based) variant fed the same body and env-functions values must produce the
        // identical intermediate value.
        var referenceExpr =
            FunctionValueBuilder.EmitCurriedFunctionTemplateWithLeadingArgsFromEncodedBody(
                innerExprEncoded,
                parameterCount,
                leadingArgExpressions,
                [IntegerEncoding.EncodeSignedInteger(1000)]);

        var referenceIntermediate =
            EvaluateExpression(referenceExpr, PineValue.EmptyList);

        emittedIntermediate.Should().Be(referenceIntermediate);
    }

    /// <summary>
    /// Verifies that <see cref="FunctionValueBuilder.EmitCurriedFunctionTemplateWithLeadingArgsFromEncodedBodyExpression"/>
    /// produces a clean template that, after applying the remaining arguments, reaches the same fully
    /// applied result as the plain partial-application reference. The encoded body and the env-functions
    /// list are supplied as expressions read from the partial-application-site environment (mirroring the
    /// same-SCC case). When <paramref name="withEnvFunctions"/> is set, the produced intermediate is also
    /// required to be structurally identical to the static (value-based) variant.
    /// </summary>
    private static void AssertPartialApplicationFromExpressionConsistency(
        int parameterCount,
        int leadingCount,
        bool withEnvFunctions)
    {
        leadingCount.Should().BeInRange(0, parameterCount - 1);
        parameterCount.Should().BeLessThanOrEqualTo(s_partialAppCoefficients.Length);

        var envFunctions =
            withEnvFunctions
            ? new[] { IntegerEncoding.EncodeSignedInteger(1000) }
            : null;

        var innerExpr = BuildLinearInnerExpr(parameterCount, withEnvFunctions);

        var innerExprEncoded = ExpressionEncoding.EncodeExpressionAsValue(innerExpr);

        // Concrete argument values, chosen distinct and non-trivial.
        var argValues = new PineValue[parameterCount];

        for (var i = 0; i < parameterCount; i++)
        {
            argValues[i] = IntegerEncoding.EncodeSignedInteger(50 + (i * 7) + 1);
        }

        var leadingArgExpressions = new Expression[leadingCount];

        for (var i = 0; i < leadingCount; i++)
        {
            leadingArgExpressions[i] = Expression.LitralInst(argValues[i]);
        }

        // Reference: the static (value-based) variant applied to the same leading arguments.
        var referenceExpr =
            FunctionValueBuilder.EmitCurriedFunctionTemplateWithLeadingArgsFromEncodedBody(
                innerExprEncoded,
                parameterCount,
                leadingArgExpressions,
                envFunctions);

        var referenceIntermediate =
            EvaluateExpression(referenceExpr, PineValue.EmptyList);

        // Expression-based emission: the encoded body and the env-functions list are read from the
        // partial-application-site environment instead of being embedded as static values.
        var envFunctionsValue =
            PineValue.List(envFunctions is null ? [] : [.. envFunctions]);

        var siteEnvironment = PineValue.List([innerExprEncoded, envFunctionsValue]);

        var emittedExpr =
            FunctionValueBuilder.EmitCurriedFunctionTemplateWithLeadingArgsFromEncodedBodyExpression(
                ListItemFromIndex(0, Expression.EnvironmentInstance),
                parameterCount,
                leadingArgExpressions,
                ListItemFromIndex(1, Expression.EnvironmentInstance));

        var emittedIntermediate =
            EvaluateExpression(emittedExpr, siteEnvironment);

        if (withEnvFunctions)
        {
            // With a non-empty env-functions list both variants embed the body and the env-functions list
            // as Literal nodes, so the produced intermediate values are structurally identical.
            emittedIntermediate.Should().Be(
                referenceIntermediate,
                "the expression-based emission must produce the same intermediate value as the static " +
                "variant when both carry the body and env-functions as literals");
        }

        // The emitted intermediate must itself be a parseable expression with no builtin/kernel/eval nodes,
        // i.e. it is a clean template just like the plain partial-application intermediate.
        var parsedIntermediate =
            ExpressionEncoding.ParseExpressionFromValue(emittedIntermediate)
            .Extract(err => throw new InvalidOperationException($"Failed to parse intermediate: {err}"));

        parsedIntermediate.BuiltinCount.Should().Be(0);

        // Apply the remaining arguments; the fully applied result must equal the independently computed
        // expected value, and must agree with the reference.
        var expectedFinal =
            IntegerEncoding.EncodeSignedInteger(
                ComputeExpectedResult(parameterCount, argValues, withEnvFunctions));

        var emittedFinal = emittedIntermediate;
        var referenceFinal = referenceIntermediate;

        for (var i = leadingCount; i < parameterCount; i++)
        {
            emittedFinal = EvaluateEncodedExpression(emittedFinal, argValues[i]);
            referenceFinal = EvaluateEncodedExpression(referenceFinal, argValues[i]);
        }

        emittedFinal.Should().Be(expectedFinal);
        referenceFinal.Should().Be(expectedFinal);
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

    private static string RenderValueAsElmExpression(PineValue pineValue)
    {
        var asElmValue =
            ElmValueEncoding.PineValueAsElmValue(pineValue, additionalReusableDecodings: null, reportNewDecoding: null)
            .Extract(err => throw new InvalidOperationException($"Failed to convert PineValue to ElmValue: {err}"));

        return ElmValue.RenderAsElmExpression(asElmValue).expressionString;
    }

    /// <summary>
    /// Evaluates the given expression with the specified environment value.
    /// This is a helper function to simplify test code by encapsulating VM creation and evaluation.
    /// </summary>
    /// <param name="expression">The expression to evaluate.</param>
    /// <param name="environment">The environment value to pass to the expression.</param>
    /// <param name="invocationCountLimit">Optional limit on invocations (default: 100).</param>
    /// <returns>The result of evaluating the expression.</returns>
    private static PineValue EvaluateExpression(
        Expression expression,
        PineValue environment,
        int invocationCountLimit = 100)
    {
        var vm =
            PineVM.CreateCustom(
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
                config: new PineVM.EvaluationConfig(
                    InvocationCountLimit: invocationCountLimit,
                    LoopIterationCountLimit: null,
                    StackDepthLimit: null))
            .Extract(err => throw new InvalidOperationException($"Evaluation failed: {err}"));

        return result.ReturnValue.Evaluate();
    }

    /// <summary>
    /// Parses a PineValue as an expression and evaluates it with the specified environment.
    /// This is a convenience overload for incremental application scenarios.
    /// </summary>
    /// <param name="encodedExpression">The encoded expression value to parse and evaluate.</param>
    /// <param name="environment">The environment value to pass to the expression.</param>
    /// <param name="invocationCountLimit">Optional limit on invocations (default: 100).</param>
    /// <returns>The result of evaluating the expression.</returns>
    private static PineValue EvaluateEncodedExpression(
        PineValue encodedExpression,
        PineValue environment,
        int invocationCountLimit = 100)
    {
        var parsed =
            s_parseCache.ParseExpression(encodedExpression)
            .Extract(err => throw new InvalidOperationException($"Failed to parse expression: {err}"));

        return EvaluateExpression(parsed, environment, invocationCountLimit);
    }

    #endregion
}
