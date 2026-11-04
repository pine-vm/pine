using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.Elm.Syntax.Interpreter;
using Pine.Core.Encodings;
using Pine.Core.Functional;
using Pine.Core.Internal;
using Pine.Core.PineLanguage;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Numerics;
using Xunit;

using ElmInterpreter = Pine.Core.Elm.Syntax.Interpreter.ElmSyntaxInterpreter;

namespace Pine.Core.Tests.Elm.Syntax.Interpreter;

/// <summary>
/// Compares whole-function Bitwise shortcuts with the bundled Elm source, including inputs
/// outside the signed 32-bit range and JavaScript-compatible shift-count masking.
/// </summary>
public class BitwiseBuiltinTests
{
    private static readonly Lazy<ElmInterpreter.Prepared> s_prepared =
        new(
            () => InterpreterTestHelper.PrepareKernelModules(
                "Basics.elm",
                "Bitwise.elm",
                "List.elm"));

    private static readonly Lazy<Func<ElmInterpreter.Application, ElmInterpreter.ApplicationResolution>> s_withBuiltins =
        new(() => ElmInterpreter.BuildResolvers(s_prepared.Value.Declarations));

    private static readonly Lazy<Func<ElmInterpreter.Application, ElmInterpreter.ApplicationResolution>> s_withoutBuiltins =
        new(() => ElmInterpreter.BuildResolvers(s_prepared.Value.Declarations, customFunctionResolvers: null));

    private static readonly IReadOnlyList<BigInteger> s_integers = IntegerSamples();

    private static IReadOnlyList<BigInteger> IntegerSamples()
    {
        var samples = new List<BigInteger> { 0, 1, -1 };

        foreach (var bit in new[] { 8, 16, 31, 32, 64, 128 })
        {
            var boundary = BigInteger.One << bit;

            foreach (var delta in new[] { -1, 0, 1 })
            {
                samples.Add(boundary + delta);
                samples.Add(-boundary + delta);
            }
        }

        var random = new Random(173);
        var bytes = new byte[8];

        for (var i = 0; i < 8; i++)
        {
            random.NextBytes(bytes);
            samples.Add(new BigInteger(bytes, isUnsigned: false, isBigEndian: true));
        }

        return samples;
    }

    private static Result<ElmInterpretationError, PineValueInProcess> Invoke(
        string functionName,
        IReadOnlyList<PineValue> arguments,
        bool enableBuiltins) =>
        ElmInterpreter.Interpret(
            DeclQualifiedName.Create(["Bitwise"], functionName),
            [.. arguments.Select(PineValueInProcess.Create)],
            enableBuiltins ? s_withBuiltins.Value : s_withoutBuiltins.Value);

    private static void AssertMatchesElm(string functionName, params PineValue[] arguments)
    {
        var actual =
            Invoke(functionName, arguments, enableBuiltins: true)
            .Extract(err => throw new Exception(err.ToString()))
            .Evaluate();

        var expected =
            Invoke(functionName, arguments, enableBuiltins: false)
            .Extract(err => throw new Exception(err.ToString()))
            .Evaluate();

        actual.Should().Be(expected, because: "Bitwise." + functionName + " must preserve the Elm-source result");
    }

    [Theory]
    [InlineData("and")]
    [InlineData("or")]
    [InlineData("xor")]
    public void Binary_builtins_match_Elm_across_integer_boundaries(string functionName)
    {
        foreach (var left in s_integers)
        {
            foreach (var right in s_integers)
            {
                AssertMatchesElm(
                    functionName,
                    IntegerEncoding.EncodeSignedInteger(left),
                    IntegerEncoding.EncodeSignedInteger(right));
            }
        }
    }

    [Fact]
    public void Complement_builtin_matches_Elm_across_integer_boundaries()
    {
        foreach (var integer in s_integers)
        {
            AssertMatchesElm("complement", IntegerEncoding.EncodeSignedInteger(integer));
        }
    }

    [Theory]
    [InlineData("shiftLeftBy")]
    [InlineData("shiftRightBy")]
    [InlineData("shiftRightZfBy")]
    public void Shift_builtins_match_Elm_with_offsets_masked_to_five_bits(string functionName)
    {
        foreach (var offset in new[] { 0, 1, 4, 7, 8, 15, 16, 24, 31, 32, 33, 63, 64, 127, 256, int.MaxValue })
        {
            foreach (var integer in s_integers)
            {
                AssertMatchesElm(
                    functionName,
                    IntegerEncoding.EncodeSignedInteger(offset),
                    IntegerEncoding.EncodeSignedInteger(integer));
            }
        }
    }

    [Theory]
    [InlineData(new byte[] { 2, 0 })]
    [InlineData(new byte[] { 2, 0, 0 })]
    [InlineData(new byte[] { 4, 0, 0 })]
    [InlineData(new byte[] { 2, 0, 1 })]
    [InlineData(new byte[] { 4, 0, 1 })]
    [InlineData(new byte[] { 2, 0, 128, 0, 0, 0 })]
    [InlineData(new byte[] { 4, 0, 128, 0, 0, 0 })]
    public void Builtins_preserve_relaxed_integer_encoding_behavior(byte[] bytes)
    {
        var value = PineValue.Blob(bytes);

        foreach (var functionName in new[] { "and", "or", "xor" })
        {
            AssertMatchesElm(functionName, value, IntegerEncoding.EncodeSignedInteger(-12345));
            AssertMatchesElm(functionName, IntegerEncoding.EncodeSignedInteger(0x12345678), value);
        }

        AssertMatchesElm("complement", value);

        foreach (var functionName in new[] { "shiftLeftBy", "shiftRightBy", "shiftRightZfBy" })
        {
            AssertMatchesElm(functionName, IntegerEncoding.EncodeSignedInteger(0), value);
            AssertMatchesElm(functionName, IntegerEncoding.EncodeSignedInteger(7), value);
            AssertMatchesElm(functionName, PineValue.Blob([4, 0, 1]), value);
        }
    }

    [Theory]
    [InlineData("Bitwise.and 305419896 -12345")]
    [InlineData("Bitwise.or 305419896 -12345")]
    [InlineData("Bitwise.xor 305419896 -12345")]
    [InlineData("Bitwise.complement -12345")]
    [InlineData("Bitwise.shiftLeftBy 7 -12345")]
    [InlineData("Bitwise.shiftRightBy 7 -12345")]
    [InlineData("Bitwise.shiftRightZfBy 7 -12345")]
    public void Builtins_avoid_nested_kernel_dispatch_and_reduce_interpreter_iterations(string expression)
    {
        var (actual, optimizedCounters) =
            ElmInterpreter.ParseAndInterpretWithCounters(expression, s_prepared.Value);

        var (expected, interpretedCounters) =
            ElmInterpreter.ParseAndInterpretWithCounters(
                expression,
                s_prepared.Value,
                enableDefaultBuiltins: false);

        actual.Extract(err => throw new Exception(err.ToString())).Evaluate()
            .Should().Be(expected.Extract(err => throw new Exception(err.ToString())).Evaluate());

        optimizedCounters.DirectFunctionApplicationCount.Should().Be(1);
        optimizedCounters.PineBuiltinInvocationCount.Should().Be(0);
        interpretedCounters.PineBuiltinInvocationCount.Should().BeGreaterThan(0);
        optimizedCounters.InstructionLoopCount.Should().BeLessThan(interpretedCounters.InstructionLoopCount / 4);
    }

    [Theory]
    [InlineData("let f = Bitwise.and 255 in f 12345")]
    [InlineData("let f = Bitwise.or 255 in f 12345")]
    [InlineData("let f = Bitwise.xor 255 in f 12345")]
    [InlineData("let f = Bitwise.shiftLeftBy 4 in f 12345")]
    [InlineData("let f = Bitwise.shiftRightBy 4 in f 12345")]
    [InlineData("let f = Bitwise.shiftRightZfBy 4 in f -12345")]
    [InlineData("List.map Bitwise.complement [ -12345, 0, 12345 ]")]
    [InlineData("List.map (Bitwise.and 255) [ -12345, 0, 12345 ]")]
    [InlineData("let fs = [ Bitwise.and, Bitwise.or, Bitwise.xor ] in List.map (\\f -> f 255 -12345) fs")]
    public void Builtins_preserve_partial_and_higher_order_application(string expression)
    {
        InterpreterTestHelper.EvaluateInModulesToPineValue(expression, s_prepared.Value)
            .Should().Be(
            InterpreterTestHelper.EvaluateInModulesWithoutBuiltinsToPineValue(expression, s_prepared.Value));
    }

    [Theory]
    [InlineData("Bitwise.shiftLeftBy 1 -2147483648", 0L)]
    [InlineData("Bitwise.shiftRightBy 31 -1", -1L)]
    [InlineData("Bitwise.shiftRightBy 4 -12345", -772L)]
    [InlineData("Bitwise.shiftRightZfBy 0 -1", 4294967295L)]
    public void Builtins_match_JavaScript_shift_edge_cases(string expression, long expected)
    {
        InterpreterTestHelper.EvaluateInModulesToPineValue(expression, s_prepared.Value)
            .Should().Be(IntegerEncoding.EncodeSignedInteger(expected));
    }

    [Theory]
    [InlineData("Bitwise.and [] 1")]
    [InlineData("Bitwise.or 1 []")]
    [InlineData("Bitwise.xor 1 \"not an integer\"")]
    [InlineData("Bitwise.complement []")]
    [InlineData("Bitwise.shiftLeftBy [] 1")]
    [InlineData("Bitwise.shiftRightBy 1 []")]
    [InlineData("Bitwise.shiftRightZfBy [] 1")]
    [InlineData("Bitwise.and 1 2 3")]
    [InlineData("Bitwise.complement 1 2")]
    public void Unsupported_arguments_fall_back_to_Elm(string expression)
    {
        var (actual, _) =
            ElmInterpreter.ParseAndInterpretWithCounters(expression, s_prepared.Value);

        var (expected, _) =
            ElmInterpreter.ParseAndInterpretWithCounters(
                expression,
                s_prepared.Value,
                enableDefaultBuiltins: false);

        if (expected.IsOkOrNull() is { } expectedValue)
        {
            actual.Extract(err => throw new Exception(err.ToString())).Evaluate()
                .Should().Be(expectedValue.Evaluate());
        }
        else
        {
            actual.IsErrOrNull().Should().NotBeNull();
        }
    }

    [Theory]
    [InlineData("shiftLeftBy")]
    [InlineData("shiftRightBy")]
    [InlineData("shiftRightZfBy")]
    public void Invalid_shift_offsets_preserve_Elm_success_or_failure(string functionName)
    {
        foreach (var offset in new BigInteger[] { -1, -7, -8, -9, -32, BigInteger.One << 80, -(BigInteger.One << 80) })
        {
            PineValue[] arguments =
                [
                IntegerEncoding.EncodeSignedInteger(offset),
                IntegerEncoding.EncodeSignedInteger(-12345),
                ];

            var actual = Invoke(functionName, arguments, enableBuiltins: true);
            var expected = Invoke(functionName, arguments, enableBuiltins: false);

            if (expected.IsOkOrNull() is { } expectedValue)
            {
                actual.Extract(err => throw new Exception(err.ToString())).Evaluate()
                    .Should().Be(expectedValue.Evaluate());
            }
            else
            {
                actual.IsErrOrNull().Should().NotBeNull();
            }
        }
    }
}
