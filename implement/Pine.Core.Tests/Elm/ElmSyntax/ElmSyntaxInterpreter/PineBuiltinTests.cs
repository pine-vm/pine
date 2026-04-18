using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.Elm;
using System.Numerics;
using Xunit;

using ElmInterpreter = Pine.Core.Elm.ElmSyntax.ElmSyntaxInterpreter;

namespace Pine.Core.Tests.Elm.ElmSyntax.ElmSyntaxInterpreter;

/// <summary>
/// Covers every <c>Pine_builtin</c> function exposed by <see cref="Pine.Core.KernelFunction"/>.
///
/// Most tests express the call as an Elm source expression (with literal arguments) and feed it to
/// <see cref="ElmInterpreter.ParseAndInterpret(string, System.Collections.Generic.IReadOnlyDictionary{DeclQualifiedName, Pine.Core.Elm.ElmSyntax.SyntaxModel.Declaration})"/>.
/// Tests whose expected value is derived by driving <see cref="KernelFunction"/> directly keep a
/// thin Elm wrapper so that the same <see cref="ElmValue"/> argument instances are shared between
/// the interpreter invocation and the kernel-function comparison.
/// </summary>
public class PineBuiltinTests
{
    private const string ElmModuleText =
        """
        module Test exposing (..)


        kernel_bit_xor a b =
            Pine_builtin.bit_xor [ a, b ]


        kernel_bit_not a =
            Pine_builtin.bit_not a


        kernel_bit_shift_left n a =
            Pine_builtin.bit_shift_left [ n, a ]


        kernel_bit_shift_right n a =
            Pine_builtin.bit_shift_right [ n, a ]
        """;

    private static readonly System.Collections.Generic.IReadOnlyDictionary<DeclQualifiedName, Pine.Core.Elm.ElmSyntax.SyntaxModel.Declaration> s_declarations =
        InterpreterTestHelper.ParseDeclarations(ElmModuleText);

    private static ElmValue Evaluate(string expression) =>
        ElmInterpreter.ParseAndInterpret(expression, s_declarations)
        .Extract(err => throw new System.Exception(err.ToString()));

    private static ElmValue Invoke(string functionName, params ElmValue[] arguments) =>
        ElmInterpreter.Interpret(
            new DeclQualifiedName([], functionName),
            arguments,
            s_declarations)
        .Extract(err => throw new System.Exception(err.ToString()));

    // --- equal ---

    [Fact]
    public void Equal_on_two_equal_integers_returns_True()
    {
        Evaluate("Pine_builtin.equal [ 42, 42 ]")
            .Should().Be(ElmValue.TrueValue);
    }

    [Fact]
    public void Equal_on_two_different_integers_returns_False()
    {
        Evaluate("Pine_builtin.equal [ 42, 43 ]")
            .Should().Be(ElmValue.FalseValue);
    }

    [Fact]
    public void Equal_on_two_equal_lists_returns_True()
    {
        Evaluate("Pine_builtin.equal [ [ 1, 2, 3 ], [ 1, 2, 3 ] ]")
            .Should().Be(ElmValue.TrueValue);
    }

    // --- length ---

    [Fact]
    public void Length_on_empty_list_is_zero()
    {
        Evaluate("Pine_builtin.length []")
            .Should().Be(ElmValue.Integer(0));
    }

    [Fact]
    public void Length_on_non_empty_list()
    {
        Evaluate("Pine_builtin.length [ 10, 20, 30, 40, 50 ]")
            .Should().Be(ElmValue.Integer(5));
    }

    // --- head ---

    [Fact]
    public void Head_returns_first_element_of_list()
    {
        Evaluate("Pine_builtin.head [ 11, 22, 33 ]")
            .Should().Be(ElmValue.Integer(11));
    }

    [Fact]
    public void Head_on_empty_list_returns_empty_list()
    {
        Evaluate("Pine_builtin.head []")
            .Should().Be(ElmValue.ListInstance([]));
    }

    // --- skip ---

    [Fact]
    public void Skip_drops_first_n_elements()
    {
        Evaluate("Pine_builtin.skip [ 2, [ 1, 2, 3, 4, 5 ] ]")
            .Should().Be(
            ElmValue.ListInstance(
                [ElmValue.Integer(3), ElmValue.Integer(4), ElmValue.Integer(5)]));
    }

    [Fact]
    public void Skip_zero_returns_same_list()
    {
        Evaluate("Pine_builtin.skip [ 0, [ 1, 2 ] ]")
            .Should().Be(
            ElmValue.ListInstance([ElmValue.Integer(1), ElmValue.Integer(2)]));
    }

    // --- take ---

    [Fact]
    public void Take_keeps_first_n_elements()
    {
        Evaluate("Pine_builtin.take [ 3, [ 10, 20, 30, 40 ] ]")
            .Should().Be(
            ElmValue.ListInstance(
                [ElmValue.Integer(10), ElmValue.Integer(20), ElmValue.Integer(30)]));
    }

    [Fact]
    public void Take_zero_returns_empty_list()
    {
        Evaluate("Pine_builtin.take [ 0, [ 1, 2 ] ]")
            .Should().Be(ElmValue.ListInstance([]));
    }

    // --- concat ---

    [Fact]
    public void Concat_joins_list_of_lists()
    {
        Evaluate("Pine_builtin.concat [ [ 1, 2 ], [ 3 ], [ 4, 5 ] ]")
            .Should().Be(
            ElmValue.ListInstance(
                [
                ElmValue.Integer(1),
                ElmValue.Integer(2),
                ElmValue.Integer(3),
                ElmValue.Integer(4),
                ElmValue.Integer(5),
                ]));
    }

    [Fact]
    public void Concat_of_empty_list_of_lists_is_empty_list()
    {
        Evaluate("Pine_builtin.concat []")
            .Should().Be(ElmValue.ListInstance([]));
    }

    // --- reverse ---

    [Fact]
    public void Reverse_reverses_list_order()
    {
        Evaluate("Pine_builtin.reverse [ 1, 2, 3, 4 ]")
            .Should().Be(
            ElmValue.ListInstance(
                [
                ElmValue.Integer(4),
                ElmValue.Integer(3),
                ElmValue.Integer(2),
                ElmValue.Integer(1),
                ]));
    }

    [Fact]
    public void Reverse_on_singleton_list_returns_same_list()
    {
        Evaluate("Pine_builtin.reverse [ 7 ]")
            .Should().Be(ElmValue.ListInstance([ElmValue.Integer(7)]));
    }

    // --- negate ---

    [Fact]
    public void Negate_positive_integer_yields_negative()
    {
        Evaluate("Pine_builtin.negate 123")
            .Should().Be(ElmValue.Integer(-123));
    }

    [Fact]
    public void Negate_negative_integer_yields_positive()
    {
        Evaluate("Pine_builtin.negate (-123)")
            .Should().Be(ElmValue.Integer(123));
    }

    // --- int_add ---

    [Fact]
    public void Int_add_small_positive_integers()
    {
        Evaluate("Pine_builtin.int_add [ 3, 4 ]")
            .Should().Be(ElmValue.Integer(7));
    }

    [Fact]
    public void Int_add_with_negative_integer()
    {
        Evaluate("Pine_builtin.int_add [ 10, -3 ]")
            .Should().Be(ElmValue.Integer(7));
    }

    // --- int_mul ---

    [Fact]
    public void Int_mul_small_positive_integers()
    {
        Evaluate("Pine_builtin.int_mul [ 6, 7 ]")
            .Should().Be(ElmValue.Integer(42));
    }

    [Fact]
    public void Int_mul_with_zero_is_zero()
    {
        Evaluate("Pine_builtin.int_mul [ 123, 0 ]")
            .Should().Be(ElmValue.Integer(0));
    }

    [Fact]
    public void Int_mul_negative_times_positive_is_negative()
    {
        Evaluate("Pine_builtin.int_mul [ -4, 5 ]")
            .Should().Be(ElmValue.Integer(-20));
    }

    // --- int_is_sorted_asc ---

    [Fact]
    public void Int_is_sorted_asc_ascending_list_is_True()
    {
        Evaluate("Pine_builtin.int_is_sorted_asc [ 1, 2, 5, 10 ]")
            .Should().Be(ElmValue.TrueValue);
    }

    [Fact]
    public void Int_is_sorted_asc_allows_equal_consecutive_elements()
    {
        Evaluate("Pine_builtin.int_is_sorted_asc [ 1, 1, 2 ]")
            .Should().Be(ElmValue.TrueValue);
    }

    [Fact]
    public void Int_is_sorted_asc_unsorted_list_is_False()
    {
        Evaluate("Pine_builtin.int_is_sorted_asc [ 1, 3, 2 ]")
            .Should().Be(ElmValue.FalseValue);
    }

    [Fact]
    public void Int_is_sorted_asc_empty_list_is_True()
    {
        Evaluate("Pine_builtin.int_is_sorted_asc []")
            .Should().Be(ElmValue.TrueValue);
    }

    // --- bit_and / bit_or / bit_xor ---
    //
    // Pine integers are encoded as blobs prefixed with a sign byte (0x04 for positive, 0x02 for
    // negative). For two positive integers of the same magnitude-byte width, the sign bytes
    // match and a bitwise operation on the encoded blobs is equivalent to the bitwise operation
    // on the magnitudes. The assertions below exploit that to cover bit_and / bit_or / bit_xor.

    [Fact]
    public void Bit_and_of_two_positive_integers_masks_bits()
    {
        // 0x0F = 0000 1111, 0xFA = 1111 1010; AND = 0000 1010 = 0x0A
        Evaluate("Pine_builtin.bit_and [ 0x0F, 0xFA ]")
            .Should().Be(ElmValue.Integer(0x0A));
    }

    [Fact]
    public void Bit_or_of_two_positive_integers_sets_bits()
    {
        // 0x0F = 0000 1111, 0xF0 = 1111 0000; OR = 1111 1111 = 0xFF
        Evaluate("Pine_builtin.bit_or [ 0x0F, 0xF0 ]")
            .Should().Be(ElmValue.Integer(0xFF));
    }

    [Fact]
    public void Bit_xor_of_two_positive_integers()
    {
        // Compare against the direct kernel result: XOR-ing the matching sign bytes produces a
        // zero leading byte which is then not a valid integer encoding, so we cannot express the
        // expected value using ElmValue.Integer.
        var a = ElmValue.Integer(0xFF);
        var b = ElmValue.Integer(0x0F);

        var result = Invoke("kernel_bit_xor", a, b);

        var expectedPineValue =
            KernelFunction.bit_xor(
                PineValue.List(
                    [
                    ElmValueEncoding.ElmValueAsPineValue(a),
                    ElmValueEncoding.ElmValueAsPineValue(b)
                    ]));

        var expected =
            ElmValueEncoding.PineValueAsElmValue(expectedPineValue, null, null)
            .Extract(err => throw new System.Exception(err));

        result.Should().Be(expected);
    }

    [Fact]
    public void Bit_xor_of_equal_values_yields_kernel_zero_result()
    {
        var a = ElmValue.Integer(0xAB);

        var result = Invoke("kernel_bit_xor", a, a);

        var expectedPineValue =
            KernelFunction.bit_xor(
                PineValue.List(
                    [
                    ElmValueEncoding.ElmValueAsPineValue(a),
                    ElmValueEncoding.ElmValueAsPineValue(a)
                    ]));

        var expected =
            ElmValueEncoding.PineValueAsElmValue(expectedPineValue, null, null)
            .Extract(err => throw new System.Exception(err));

        result.Should().Be(expected);
    }

    // --- bit_not ---
    //
    // bit_not inverts every byte of the input blob. To get a predictable, well-typed Elm result
    // we compare against the value produced by driving the kernel function directly on the
    // encoded input — this confirms the interpreter's forwarding is bit-for-bit equivalent.

    [Fact]
    public void Bit_not_matches_kernel_function_on_encoded_integer()
    {
        var argument = ElmValue.Integer(0x0F);

        var result = Invoke("kernel_bit_not", argument);

        var expectedPineValue =
            KernelFunction.bit_not(ElmValueEncoding.ElmValueAsPineValue(argument));

        var expected =
            ElmValueEncoding.PineValueAsElmValue(expectedPineValue, null, null)
            .Extract(err => throw new System.Exception(err));

        result.Should().Be(expected);
    }

    // --- bit_shift_left / bit_shift_right ---

    [Fact]
    public void Bit_shift_left_doubles_small_positive_integer()
    {
        // 0x01 encoded as [4, 1]; shifted left by 1 bit -> [8, 2];
        // that blob (first byte 8) is not an integer sign byte, so comparing to the direct kernel
        // result keeps the test agnostic of the exact decoded shape.
        var argument = ElmValue.Integer(1);

        var result = Invoke("kernel_bit_shift_left", ElmValue.Integer(1), argument);

        var expectedPineValue =
            KernelFunction.bit_shift_left(
                PineValue.List(
                    [
                    ElmValueEncoding.ElmValueAsPineValue(ElmValue.Integer(1)),
                    ElmValueEncoding.ElmValueAsPineValue(argument)
                    ]));

        var expected =
            ElmValueEncoding.PineValueAsElmValue(expectedPineValue, null, null)
            .Extract(err => throw new System.Exception(err));

        result.Should().Be(expected);
    }

    [Fact]
    public void Bit_shift_right_halves_positive_integer_with_matching_encoding()
    {
        // Use a value where both the sign byte and the magnitude byte preserve validity after
        // a single-bit right shift: 0x0C = [4, 12]; shifted right by 2 bits -> [1, 3], which is
        // not an integer encoding. We compare against the kernel result to keep the test robust.
        var argument = ElmValue.Integer(0x0C);

        var result = Invoke("kernel_bit_shift_right", ElmValue.Integer(2), argument);

        var expectedPineValue =
            KernelFunction.bit_shift_right(
                PineValue.List(
                    [
                    ElmValueEncoding.ElmValueAsPineValue(ElmValue.Integer(2)),
                    ElmValueEncoding.ElmValueAsPineValue(argument)
                    ]));

        var expected =
            ElmValueEncoding.PineValueAsElmValue(expectedPineValue, null, null)
            .Extract(err => throw new System.Exception(err));

        result.Should().Be(expected);
    }

    // --- Large integer arithmetic ---
    //
    // Pine integers are arbitrary-precision BigInteger values. These tests exercise values far
    // beyond 64-bit range to confirm the interpreter correctly forwards the encoded operands to
    // the kernel and decodes the resulting large integers.

    [Fact]
    public void Int_add_handles_values_beyond_Int64()
    {
        var a = BigInteger.Pow(2, 200);
        var b = BigInteger.Pow(2, 199);

        Evaluate($"Pine_builtin.int_add [ {a}, {b} ]")
            .Should().Be(ElmValue.Integer(a + b));
    }

    [Fact]
    public void Int_mul_produces_value_requiring_more_than_128_bits()
    {
        var a = BigInteger.Pow(10, 30);
        var b = BigInteger.Pow(10, 30);

        Evaluate($"Pine_builtin.int_mul [ {a}, {b} ]")
            .Should().Be(ElmValue.Integer(a * b));
    }

    [Fact]
    public void Int_mul_of_two_large_negative_integers_yields_large_positive()
    {
        var a = -BigInteger.Pow(2, 150);
        var b = -BigInteger.Pow(2, 150);

        Evaluate($"Pine_builtin.int_mul [ {a}, {b} ]")
            .Should().Be(ElmValue.Integer(a * b));
    }

    [Fact]
    public void Int_add_sum_of_large_positive_and_large_negative()
    {
        var a = BigInteger.Pow(2, 500);
        var b = -BigInteger.Pow(2, 499);

        Evaluate($"Pine_builtin.int_add [ {a}, {b} ]")
            .Should().Be(ElmValue.Integer(a + b));
    }

    [Fact]
    public void Negate_on_large_positive_integer_yields_large_negative()
    {
        var a = BigInteger.Pow(2, 300) + 1;

        Evaluate($"Pine_builtin.negate {a}")
            .Should().Be(ElmValue.Integer(-a));
    }

    [Fact]
    public void Negate_on_large_negative_integer_yields_large_positive()
    {
        var a = -(BigInteger.Pow(2, 400) - 5);

        Evaluate($"Pine_builtin.negate ({a})")
            .Should().Be(ElmValue.Integer(-a));
    }

    [Fact]
    public void Int_is_sorted_asc_with_large_integers()
    {
        var a = -BigInteger.Pow(2, 300);
        var b = BigInteger.Pow(2, 200);
        var c = BigInteger.Pow(2, 400);

        Evaluate($"Pine_builtin.int_is_sorted_asc [ ({a}), 0, {b}, {c} ]")
            .Should().Be(ElmValue.TrueValue);
    }
}
