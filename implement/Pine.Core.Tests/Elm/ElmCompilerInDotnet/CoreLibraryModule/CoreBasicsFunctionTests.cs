using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.Elm;
using Pine.Core.Elm.ElmCompilerInDotnet.CoreLibraryModule;
using Pine.Core.Interpreter.IntermediateVM;
using Xunit;

namespace Pine.Core.Tests.Elm.ElmCompilerInDotnet.CoreLibraryModule;

public class CoreBasicsFunctionTests
{
    [Fact]
    public void Int_sub_zero()
    {
        var evalResult =
            ApplyDirectBinary(
                CoreBasics.Int_sub,
                ElmValue.Integer(13),
                ElmValue.Integer(0));

        evalResult.value.Should().Be(ElmValue.Integer(13));
    }

    [Fact]
    public void Int_sub_17()
    {
        var evalResult =
            ApplyDirectBinary(
                CoreBasics.Int_sub,
                ElmValue.Integer(13),
                ElmValue.Integer(17));

        evalResult.value.Should().Be(ElmValue.Integer(-4));
    }

    [Fact]
    public void Generic_sub_int_13_int_17()
    {
        var resultValue =
            ApplyGeneric(
                CoreBasics.Sub_FunctionValue(),
                [
                ElmValue.Integer(13),
                ElmValue.Integer(17)
                ]);

        resultValue.Should().Be(ElmValue.Integer(-4));
    }

    [Fact]
    public void Generic_sub_float_3_point_7_negative_float_0_point_3()
    {
        var resultValue =
            ApplyGeneric(
                CoreBasics.Sub_FunctionValue(),
                [
                ElmValue.ElmFloat.Convert(3.7),
                ElmValue.ElmFloat.Convert(-0.3),
                ]);

        resultValue.Should().Be(ElmValue.Integer(4));
    }

    [Fact]
    public void Generic_sub_int_13_float_zero_point_1()
    {
        var resultValue =
            ApplyGeneric(
                CoreBasics.Sub_FunctionValue(),
                [
                ElmValue.ElmFloat.Convert(13),
                ElmValue.ElmFloat.Convert(0.1),
                ]);

        resultValue.Should().Be(ElmValue.ElmFloat.Convert(12.9));
    }

    [Fact]
    public void Number_negate_17()
    {
        var resultValue =
            ApplyUnary(
                CoreBasics.Negate_FunctionValue(),
                ElmValue.Integer(17));

        resultValue.Should().Be(ElmValue.Integer(-17));
    }

    [Fact]
    public void Number_negate_minus_17()
    {
        var resultValue =
            ApplyUnary(
                CoreBasics.Negate_FunctionValue(),
                ElmValue.Integer(-17));

        resultValue.Should().Be(ElmValue.Integer(17));
    }

    [Fact]
    public void Number_negate_float_17_point_3()
    {
        var resultValue =
            ApplyUnary(
                CoreBasics.Negate_FunctionValue(),
                ElmValue.ElmFloat.Convert(17.3));

        resultValue.Should().Be(ElmValue.ElmFloat.Convert(-17.3));
    }

    // ========== Tests for Int_div ==========

    [Fact]
    public void Int_div_10_by_3()
    {
        var evalResult =
            ApplyDirectBinary(
                CoreBasics.Int_div,
                ElmValue.Integer(10),
                ElmValue.Integer(3));

        evalResult.value.Should().Be(ElmValue.Integer(3));
    }

    [Fact]
    public void Int_div_negative_10_by_3()
    {
        var evalResult =
            ApplyDirectBinary(
                CoreBasics.Int_div,
                ElmValue.Integer(-10),
                ElmValue.Integer(3));

        evalResult.value.Should().Be(ElmValue.Integer(-3));
    }

    [Fact]
    public void Int_div_negative_1000_by_3()
    {
        var evalResult =
            ApplyDirectBinary(
                CoreBasics.Int_div,
                ElmValue.Integer(-1_000),
                ElmValue.Integer(3));

        evalResult.value.Should().Be(ElmValue.Integer(-333));
    }

    [Fact]
    public void Int_div_1000_by_17()
    {
        var evalResult =
            ApplyDirectBinary(
                CoreBasics.Int_div,
                ElmValue.Integer(1_000),
                ElmValue.Integer(17));

        evalResult.value.Should().Be(ElmValue.Integer(58));
    }

    [Fact]
    public void Int_div_1_000_000_by_257()
    {
        var evalResult =
            ApplyDirectBinary(
                CoreBasics.Int_div,
                ElmValue.Integer(1_000_000),
                ElmValue.Integer(257));

        evalResult.value.Should().Be(ElmValue.Integer(3891));

        evalResult.profile.InvocationCount.Should().BeLessThan(40);
    }

    [Fact]
    public void Int_div_10_by_negative_3()
    {
        var evalResult =
            ApplyDirectBinary(
                CoreBasics.Int_div,
                ElmValue.Integer(10),
                ElmValue.Integer(-3));

        evalResult.value.Should().Be(ElmValue.Integer(-3));
    }

    [Fact]
    public void Int_div_by_zero_returns_zero()
    {
        var evalResult =
            ApplyDirectBinary(
                CoreBasics.Int_div,
                ElmValue.Integer(10),
                ElmValue.Integer(0));

        evalResult.value.Should().Be(ElmValue.Integer(0));
    }

    // ========== Tests for Generic_Addition ==========

    [Fact]
    public void Generic_add_int_13_int_17()
    {
        var resultValue =
            ApplyGeneric(
                CoreBasics.Add_FunctionValue(),
                [
                ElmValue.Integer(13),
                ElmValue.Integer(17)
                ]);

        resultValue.Should().Be(ElmValue.Integer(30));
    }

    [Fact]
    public void Generic_add_int_negative_5_int_3()
    {
        var resultValue =
            ApplyGeneric(
                CoreBasics.Add_FunctionValue(),
                [
                ElmValue.Integer(-5),
                ElmValue.Integer(3)
                ]);

        resultValue.Should().Be(ElmValue.Integer(-2));
    }

    [Fact]
    public void Generic_add_float_1_point_5_float_2_point_5()
    {
        var resultValue =
            ApplyGeneric(
                CoreBasics.Add_FunctionValue(),
                [
                ElmValue.ElmFloat.Convert(1.5),
                ElmValue.ElmFloat.Convert(2.5)
                ]);

        resultValue.Should().Be(ElmValue.Integer(4));
    }

    // ========== Tests for Int_div_FunctionValue ==========

    [Fact]
    public void Int_div_FunctionValue_10_by_3()
    {
        var resultValue =
            ApplyGeneric(
                CoreBasics.Int_div_FunctionValue(),
                [
                ElmValue.Integer(10),
                ElmValue.Integer(3)
                ]);

        resultValue.Should().Be(ElmValue.Integer(3));
    }

    [Fact]
    public void Int_div_FunctionValue_negative_10_by_3()
    {
        var resultValue =
            ApplyGeneric(
                CoreBasics.Int_div_FunctionValue(),
                [
                ElmValue.Integer(-10),
                ElmValue.Integer(3)
                ]);

        resultValue.Should().Be(ElmValue.Integer(-3));
    }

    [Fact]
    public void Int_div_FunctionValue_10_by_negative_3()
    {
        var resultValue =
            ApplyGeneric(
                CoreBasics.Int_div_FunctionValue(),
                [
                ElmValue.Integer(10),
                ElmValue.Integer(-3)
                ]);

        resultValue.Should().Be(ElmValue.Integer(-3));
    }

    [Fact]
    public void Int_div_FunctionValue_by_zero_returns_zero()
    {
        var resultValue =
            ApplyGeneric(
                CoreBasics.Int_div_FunctionValue(),
                [
                ElmValue.Integer(10),
                ElmValue.Integer(0)
                ]);

        resultValue.Should().Be(ElmValue.Integer(0));
    }

    [Fact]
    public void Int_div_FunctionValue_1000_by_17()
    {
        var resultValue =
            ApplyGeneric(
                CoreBasics.Int_div_FunctionValue(),
                [
                ElmValue.Integer(1_000),
                ElmValue.Integer(17)
                ]);

        resultValue.Should().Be(ElmValue.Integer(58));
    }

    // ========== Tests for modBy ==========

    [Fact]
    public void Int_modBy_3_of_10()
    {
        // modBy 3 10 == 1
        var evalResult =
            ApplyDirectBinary(
                CoreBasics.Int_modBy,
                ElmValue.Integer(3),
                ElmValue.Integer(10));

        evalResult.value.Should().Be(ElmValue.Integer(1));
    }

    [Fact]
    public void Int_modBy_3_of_negative_10()
    {
        // modBy 3 -10 == 2  (always positive result in Elm modBy)
        var evalResult =
            ApplyDirectBinary(
                CoreBasics.Int_modBy,
                ElmValue.Integer(3),
                ElmValue.Integer(-10));

        evalResult.value.Should().Be(ElmValue.Integer(2));
    }

    [Fact]
    public void Int_modBy_4_of_12()
    {
        // modBy 4 12 == 0  (exactly divisible)
        var evalResult =
            ApplyDirectBinary(
                CoreBasics.Int_modBy,
                ElmValue.Integer(4),
                ElmValue.Integer(12));

        evalResult.value.Should().Be(ElmValue.Integer(0));
    }

    [Fact]
    public void Int_modBy_5_of_3()
    {
        // modBy 5 3 == 3  (dividend < divisor)
        var evalResult =
            ApplyDirectBinary(
                CoreBasics.Int_modBy,
                ElmValue.Integer(5),
                ElmValue.Integer(3));

        evalResult.value.Should().Be(ElmValue.Integer(3));
    }

    [Fact]
    public void Int_modBy_7_of_negative_3()
    {
        // modBy 7 -3 == 4  (result is always non-negative when divisor is positive)
        var evalResult =
            ApplyDirectBinary(
                CoreBasics.Int_modBy,
                ElmValue.Integer(7),
                ElmValue.Integer(-3));

        evalResult.value.Should().Be(ElmValue.Integer(4));
    }

    [Fact]
    public void Int_modBy_1_of_100()
    {
        // modBy 1 100 == 0  (any number mod 1 is 0)
        var evalResult =
            ApplyDirectBinary(
                CoreBasics.Int_modBy,
                ElmValue.Integer(1),
                ElmValue.Integer(100));

        evalResult.value.Should().Be(ElmValue.Integer(0));
    }

    [Fact]
    public void Int_modBy_17_of_1000()
    {
        // modBy 17 1000 == 14  (1000 = 58*17 + 14)
        var evalResult =
            ApplyDirectBinary(
                CoreBasics.Int_modBy,
                ElmValue.Integer(17),
                ElmValue.Integer(1000));

        evalResult.value.Should().Be(ElmValue.Integer(14));
    }

    [Fact]
    public void Int_modBy_10_of_0()
    {
        // modBy 10 0 == 0  (0 mod anything is 0)
        var evalResult =
            ApplyDirectBinary(
                CoreBasics.Int_modBy,
                ElmValue.Integer(10),
                ElmValue.Integer(0));

        evalResult.value.Should().Be(ElmValue.Integer(0));
    }

    // ========== Tests for remainderBy ==========

    [Fact]
    public void Int_remainderBy_3_of_10()
    {
        // remainderBy 3 10 == 1
        var evalResult =
            ApplyDirectBinary(
                CoreBasics.Int_remainderBy,
                ElmValue.Integer(3),
                ElmValue.Integer(10));

        evalResult.value.Should().Be(ElmValue.Integer(1));
    }

    [Fact]
    public void Int_remainderBy_3_of_negative_10()
    {
        // remainderBy 3 -10 == -1  (sign follows dividend in remainder)
        var evalResult =
            ApplyDirectBinary(
                CoreBasics.Int_remainderBy,
                ElmValue.Integer(3),
                ElmValue.Integer(-10));

        evalResult.value.Should().Be(ElmValue.Integer(-1));
    }

    [Fact]
    public void Int_remainderBy_4_of_12()
    {
        // remainderBy 4 12 == 0  (exactly divisible)
        var evalResult =
            ApplyDirectBinary(
                CoreBasics.Int_remainderBy,
                ElmValue.Integer(4),
                ElmValue.Integer(12));

        evalResult.value.Should().Be(ElmValue.Integer(0));
    }

    [Fact]
    public void Int_remainderBy_5_of_3()
    {
        // remainderBy 5 3 == 3  (dividend < divisor)
        var evalResult =
            ApplyDirectBinary(
                CoreBasics.Int_remainderBy,
                ElmValue.Integer(5),
                ElmValue.Integer(3));

        evalResult.value.Should().Be(ElmValue.Integer(3));
    }

    [Fact]
    public void Int_remainderBy_7_of_negative_3()
    {
        // remainderBy 7 -3 == -3  (sign follows dividend)
        var evalResult =
            ApplyDirectBinary(
                CoreBasics.Int_remainderBy,
                ElmValue.Integer(7),
                ElmValue.Integer(-3));

        evalResult.value.Should().Be(ElmValue.Integer(-3));
    }

    [Fact]
    public void Int_remainderBy_1_of_100()
    {
        // remainderBy 1 100 == 0  (any number remainder 1 is 0)
        var evalResult =
            ApplyDirectBinary(
                CoreBasics.Int_remainderBy,
                ElmValue.Integer(1),
                ElmValue.Integer(100));

        evalResult.value.Should().Be(ElmValue.Integer(0));
    }

    [Fact]
    public void Int_remainderBy_17_of_1000()
    {
        // remainderBy 17 1000 == 14  (1000 = 58*17 + 14)
        var evalResult =
            ApplyDirectBinary(
                CoreBasics.Int_remainderBy,
                ElmValue.Integer(17),
                ElmValue.Integer(1000));

        evalResult.value.Should().Be(ElmValue.Integer(14));
    }

    [Fact]
    public void Int_remainderBy_17_of_negative_1000()
    {
        // remainderBy 17 -1000 == -14  (sign follows dividend)
        var evalResult =
            ApplyDirectBinary(
                CoreBasics.Int_remainderBy,
                ElmValue.Integer(17),
                ElmValue.Integer(-1000));

        evalResult.value.Should().Be(ElmValue.Integer(-14));
    }

    [Fact]
    public void Int_remainderBy_10_of_0()
    {
        // remainderBy 10 0 == 0  (0 remainder anything is 0)
        var evalResult =
            ApplyDirectBinary(
                CoreBasics.Int_remainderBy,
                ElmValue.Integer(10),
                ElmValue.Integer(0));

        evalResult.value.Should().Be(ElmValue.Integer(0));
    }

    // ========== Tests for modBy FunctionValue ==========

    [Fact]
    public void Int_modBy_FunctionValue_3_of_10()
    {
        var resultValue =
            ApplyGeneric(
                CoreBasics.Int_modBy_FunctionValue(),
                [
                ElmValue.Integer(3),
                ElmValue.Integer(10)
                ]);

        resultValue.Should().Be(ElmValue.Integer(1));
    }

    [Fact]
    public void Int_modBy_FunctionValue_3_of_negative_10()
    {
        var resultValue =
            ApplyGeneric(
                CoreBasics.Int_modBy_FunctionValue(),
                [
                ElmValue.Integer(3),
                ElmValue.Integer(-10)
                ]);

        resultValue.Should().Be(ElmValue.Integer(2));
    }

    // ========== Tests for remainderBy FunctionValue ==========

    [Fact]
    public void Int_remainderBy_FunctionValue_3_of_10()
    {
        var resultValue =
            ApplyGeneric(
                CoreBasics.RemainderBy_FunctionValue(),
                [
                ElmValue.Integer(3),
                ElmValue.Integer(10)
                ]);

        resultValue.Should().Be(ElmValue.Integer(1));
    }

    [Fact]
    public void Int_remainderBy_FunctionValue_3_of_negative_10()
    {
        var resultValue =
            ApplyGeneric(
                CoreBasics.RemainderBy_FunctionValue(),
                [
                ElmValue.Integer(3),
                ElmValue.Integer(-10)
                ]);

        resultValue.Should().Be(ElmValue.Integer(-1));
    }

    // ========== Tests for eq ==========

    [Fact]
    public void Eq_int_equal()
    {
        var resultValue =
            ApplyGeneric(
                CoreBasics.Eq_FunctionValue(),
                [
                ElmValue.Integer(42),
                ElmValue.Integer(42)
                ]);

        resultValue.Should().Be(ElmValue.TrueValue);
    }

    [Fact]
    public void Eq_int_not_equal()
    {
        var resultValue =
            ApplyGeneric(
                CoreBasics.Eq_FunctionValue(),
                [
                ElmValue.Integer(42),
                ElmValue.Integer(43)
                ]);

        resultValue.Should().Be(ElmValue.FalseValue);
    }

    [Fact]
    public void Eq_string_equal()
    {
        var resultValue =
            ApplyGeneric(
                CoreBasics.Eq_FunctionValue(),
                [
                ElmValue.StringInstance("hello"),
                ElmValue.StringInstance("hello")
                ]);

        resultValue.Should().Be(ElmValue.TrueValue);
    }

    [Fact]
    public void Eq_string_not_equal()
    {
        var resultValue =
            ApplyGeneric(
                CoreBasics.Eq_FunctionValue(),
                [
                ElmValue.StringInstance("hello"),
                ElmValue.StringInstance("world")
                ]);

        resultValue.Should().Be(ElmValue.FalseValue);
    }

    [Fact]
    public void Eq_tuple_equal()
    {
        var tuple1 = ElmValue.TupleInstance(ElmValue.Integer(1), ElmValue.Integer(2));
        var tuple2 = ElmValue.TupleInstance(ElmValue.Integer(1), ElmValue.Integer(2));

        var resultValue =
            ApplyGeneric(
                CoreBasics.Eq_FunctionValue(),
                [tuple1, tuple2]);

        resultValue.Should().Be(ElmValue.TrueValue);
    }

    [Fact]
    public void Eq_tuple_not_equal()
    {
        var tuple1 = ElmValue.TupleInstance(ElmValue.Integer(1), ElmValue.Integer(2));
        var tuple2 = ElmValue.TupleInstance(ElmValue.Integer(1), ElmValue.Integer(3));

        var resultValue =
            ApplyGeneric(
                CoreBasics.Eq_FunctionValue(),
                [tuple1, tuple2]);

        resultValue.Should().Be(ElmValue.FalseValue);
    }

    [Fact]
    public void Eq_float_equal()
    {
        var resultValue =
            ApplyGeneric(
                CoreBasics.Eq_FunctionValue(),
                [
                ElmValue.ElmFloat.Convert(3.14),
                ElmValue.ElmFloat.Convert(3.14)
                ]);

        resultValue.Should().Be(ElmValue.TrueValue);
    }

    [Fact]
    public void Eq_float_not_equal()
    {
        var resultValue =
            ApplyGeneric(
                CoreBasics.Eq_FunctionValue(),
                [
                ElmValue.ElmFloat.Convert(3.14),
                ElmValue.ElmFloat.Convert(3.15)
                ]);

        resultValue.Should().Be(ElmValue.FalseValue);
    }

    [Fact]
    public void Eq_int_and_equivalent_float()
    {
        // Test that 4 == Elm_Float 4 1 (which is 4.0) returns True
        var resultValue =
            ApplyGeneric(
                CoreBasics.Eq_FunctionValue(),
                [
                ElmValue.Integer(4),
                ElmValue.ElmFloat.Convert(4.0)
                ]);

        resultValue.Should().Be(ElmValue.TrueValue);
    }

    [Fact]
    public void Eq_float_and_equivalent_int()
    {
        // Test that Elm_Float 4 1 (which is 4.0) == 4 returns True
        var resultValue =
            ApplyGeneric(
                CoreBasics.Eq_FunctionValue(),
                [
                ElmValue.ElmFloat.Convert(4.0),
                ElmValue.Integer(4)
                ]);

        resultValue.Should().Be(ElmValue.TrueValue);
    }

    // ========== Tests for neq ==========

    [Fact]
    public void Neq_int_equal()
    {
        var resultValue =
            ApplyGeneric(
                CoreBasics.Neq_FunctionValue(),
                [
                ElmValue.Integer(42),
                ElmValue.Integer(42)
                ]);

        resultValue.Should().Be(ElmValue.FalseValue);
    }

    [Fact]
    public void Neq_int_not_equal()
    {
        var resultValue =
            ApplyGeneric(
                CoreBasics.Neq_FunctionValue(),
                [
                ElmValue.Integer(42),
                ElmValue.Integer(43)
                ]);

        resultValue.Should().Be(ElmValue.TrueValue);
    }

    // ========== Tests for compare ==========

    [Fact]
    public void Compare_int_less_than()
    {
        var resultValue =
            ApplyGeneric(
                CoreBasics.Compare_FunctionValue(),
                [
                ElmValue.Integer(3),
                ElmValue.Integer(5)
                ]);

        resultValue.Should().Be(ElmValue.TagInstance("LT", []));
    }

    [Fact]
    public void Compare_int_equal()
    {
        var resultValue =
            ApplyGeneric(
                CoreBasics.Compare_FunctionValue(),
                [
                ElmValue.Integer(5),
                ElmValue.Integer(5)
                ]);

        resultValue.Should().Be(ElmValue.TagInstance("EQ", []));
    }

    [Fact]
    public void Compare_int_greater_than()
    {
        var resultValue =
            ApplyGeneric(
                CoreBasics.Compare_FunctionValue(),
                [
                ElmValue.Integer(7),
                ElmValue.Integer(5)
                ]);

        resultValue.Should().Be(ElmValue.TagInstance("GT", []));
    }

    [Fact]
    public void Compare_float_less_than()
    {
        var resultValue =
            ApplyGeneric(
                CoreBasics.Compare_FunctionValue(),
                [
                ElmValue.ElmFloat.Convert(1.5),
                ElmValue.ElmFloat.Convert(2.5)
                ]);

        resultValue.Should().Be(ElmValue.TagInstance("LT", []));
    }

    [Fact]
    public void Compare_float_equal()
    {
        var resultValue =
            ApplyGeneric(
                CoreBasics.Compare_FunctionValue(),
                [
                ElmValue.ElmFloat.Convert(3.14),
                ElmValue.ElmFloat.Convert(3.14)
                ]);

        resultValue.Should().Be(ElmValue.TagInstance("EQ", []));
    }

    [Fact]
    public void Compare_float_greater_than()
    {
        var resultValue =
            ApplyGeneric(
                CoreBasics.Compare_FunctionValue(),
                [
                ElmValue.ElmFloat.Convert(5.5),
                ElmValue.ElmFloat.Convert(2.5)
                ]);

        resultValue.Should().Be(ElmValue.TagInstance("GT", []));
    }

    [Fact]
    public void Compare_int_vs_float()
    {
        // compare 3 (Elm_Float 7 2) = compare 3 3.5 = LT
        var resultValue =
            ApplyGeneric(
                CoreBasics.Compare_FunctionValue(),
                [
                ElmValue.Integer(3),
                ElmValue.ElmFloat.Convert(3.5)
                ]);

        resultValue.Should().Be(ElmValue.TagInstance("LT", []));
    }

    [Fact]
    public void Compare_float_vs_int()
    {
        // compare (Elm_Float 7 2) 3 = compare 3.5 3 = GT
        var resultValue =
            ApplyGeneric(
                CoreBasics.Compare_FunctionValue(),
                [
                ElmValue.ElmFloat.Convert(3.5),
                ElmValue.Integer(3)
                ]);

        resultValue.Should().Be(ElmValue.TagInstance("GT", []));
    }

    // ========== Tests for string comparison ==========

    [Fact]
    public void Compare_string_less_than()
    {
        var resultValue =
            ApplyGeneric(
                CoreBasics.Compare_FunctionValue(),
                [
                ElmValue.StringInstance("apple"),
                ElmValue.StringInstance("banana")
                ]);

        resultValue.Should().Be(ElmValue.TagInstance("LT", []));
    }

    [Fact]
    public void Compare_string_equal()
    {
        var resultValue =
            ApplyGeneric(
                CoreBasics.Compare_FunctionValue(),
                [
                ElmValue.StringInstance("hello"),
                ElmValue.StringInstance("hello")
                ]);

        resultValue.Should().Be(ElmValue.TagInstance("EQ", []));
    }

    [Fact]
    public void Compare_string_greater_than()
    {
        var resultValue =
            ApplyGeneric(
                CoreBasics.Compare_FunctionValue(),
                [
                ElmValue.StringInstance("zebra"),
                ElmValue.StringInstance("apple")
                ]);

        resultValue.Should().Be(ElmValue.TagInstance("GT", []));
    }

    [Fact]
    public void Compare_string_prefix()
    {
        // "app" < "apple" (shorter string is less than longer with same prefix)
        var resultValue =
            ApplyGeneric(
                CoreBasics.Compare_FunctionValue(),
                [
                ElmValue.StringInstance("app"),
                ElmValue.StringInstance("apple")
                ]);

        resultValue.Should().Be(ElmValue.TagInstance("LT", []));
    }

    [Fact]
    public void Compare_string_empty()
    {
        // "" < "a" (empty string is less than non-empty)
        var resultValue =
            ApplyGeneric(
                CoreBasics.Compare_FunctionValue(),
                [
                ElmValue.StringInstance(""),
                ElmValue.StringInstance("a")
                ]);

        resultValue.Should().Be(ElmValue.TagInstance("LT", []));
    }

    // ========== Tests for tuple comparison ==========

    [Fact]
    public void Compare_tuple_less_than_first_element()
    {
        // (1, 5) < (2, 3) - first element determines order
        var resultValue =
            ApplyGeneric(
                CoreBasics.Compare_FunctionValue(),
                [
                ElmValue.ListInstance([ElmValue.Integer(1), ElmValue.Integer(5)]),
                ElmValue.ListInstance([ElmValue.Integer(2), ElmValue.Integer(3)])
                ]);

        resultValue.Should().Be(ElmValue.TagInstance("LT", []));
    }

    [Fact]
    public void Compare_tuple_less_than_second_element()
    {
        // (1, 2) < (1, 3) - first elements equal, second determines order
        var resultValue =
            ApplyGeneric(
                CoreBasics.Compare_FunctionValue(),
                [
                ElmValue.ListInstance([ElmValue.Integer(1), ElmValue.Integer(2)]),
                ElmValue.ListInstance([ElmValue.Integer(1), ElmValue.Integer(3)])
                ]);

        resultValue.Should().Be(ElmValue.TagInstance("LT", []));
    }

    [Fact]
    public void Compare_tuple_equal()
    {
        // (1, 2) == (1, 2)
        var resultValue =
            ApplyGeneric(
                CoreBasics.Compare_FunctionValue(),
                [
                ElmValue.ListInstance([ElmValue.Integer(1), ElmValue.Integer(2)]),
                ElmValue.ListInstance([ElmValue.Integer(1), ElmValue.Integer(2)])
                ]);

        resultValue.Should().Be(ElmValue.TagInstance("EQ", []));
    }

    [Fact]
    public void Compare_tuple_greater_than()
    {
        // (3, 1) > (2, 9)
        var resultValue =
            ApplyGeneric(
                CoreBasics.Compare_FunctionValue(),
                [
                ElmValue.ListInstance([ElmValue.Integer(3), ElmValue.Integer(1)]),
                ElmValue.ListInstance([ElmValue.Integer(2), ElmValue.Integer(9)])
                ]);

        resultValue.Should().Be(ElmValue.TagInstance("GT", []));
    }

    [Fact]
    public void Compare_triple_by_third_element()
    {
        // (1, 2, 3) < (1, 2, 4) - first two elements equal, third determines order
        var resultValue =
            ApplyGeneric(
                CoreBasics.Compare_FunctionValue(),
                [
                ElmValue.ListInstance([ElmValue.Integer(1), ElmValue.Integer(2), ElmValue.Integer(3)]),
                ElmValue.ListInstance([ElmValue.Integer(1), ElmValue.Integer(2), ElmValue.Integer(4)])
                ]);

        resultValue.Should().Be(ElmValue.TagInstance("LT", []));
    }

    // ========== Tests for lt ==========

    [Fact]
    public void Lt_int_less_than()
    {
        var resultValue =
            ApplyGeneric(
                CoreBasics.Lt_FunctionValue(),
                [
                ElmValue.Integer(3),
                ElmValue.Integer(5)
                ]);

        resultValue.Should().Be(ElmValue.TrueValue);
    }

    [Fact]
    public void Lt_int_equal()
    {
        var resultValue =
            ApplyGeneric(
                CoreBasics.Lt_FunctionValue(),
                [
                ElmValue.Integer(5),
                ElmValue.Integer(5)
                ]);

        resultValue.Should().Be(ElmValue.FalseValue);
    }

    [Fact]
    public void Lt_int_greater_than()
    {
        var resultValue =
            ApplyGeneric(
                CoreBasics.Lt_FunctionValue(),
                [
                ElmValue.Integer(7),
                ElmValue.Integer(5)
                ]);

        resultValue.Should().Be(ElmValue.FalseValue);
    }

    // Note: String comparison in the current implementation uses Pine_kernel.int_is_sorted_asc
    // which compares the raw Pine value representation. For strings encoded as [String, chars],
    // this compares the tag first, then the characters.

    // ========== Tests for gt ==========

    [Fact]
    public void Gt_int_less_than()
    {
        var resultValue =
            ApplyGeneric(
                CoreBasics.Gt_FunctionValue(),
                [
                ElmValue.Integer(3),
                ElmValue.Integer(5)
                ]);

        resultValue.Should().Be(ElmValue.FalseValue);
    }

    [Fact]
    public void Gt_int_equal()
    {
        var resultValue =
            ApplyGeneric(
                CoreBasics.Gt_FunctionValue(),
                [
                ElmValue.Integer(5),
                ElmValue.Integer(5)
                ]);

        resultValue.Should().Be(ElmValue.FalseValue);
    }

    [Fact]
    public void Gt_int_greater_than()
    {
        var resultValue =
            ApplyGeneric(
                CoreBasics.Gt_FunctionValue(),
                [
                ElmValue.Integer(7),
                ElmValue.Integer(5)
                ]);

        resultValue.Should().Be(ElmValue.TrueValue);
    }

    // ========== Tests for le ==========

    [Fact]
    public void Le_int_less_than()
    {
        var resultValue =
            ApplyGeneric(
                CoreBasics.Le_FunctionValue(),
                [
                ElmValue.Integer(3),
                ElmValue.Integer(5)
                ]);

        resultValue.Should().Be(ElmValue.TrueValue);
    }

    [Fact]
    public void Le_int_equal()
    {
        var resultValue =
            ApplyGeneric(
                CoreBasics.Le_FunctionValue(),
                [
                ElmValue.Integer(5),
                ElmValue.Integer(5)
                ]);

        resultValue.Should().Be(ElmValue.TrueValue);
    }

    [Fact]
    public void Le_int_greater_than()
    {
        var resultValue =
            ApplyGeneric(
                CoreBasics.Le_FunctionValue(),
                [
                ElmValue.Integer(7),
                ElmValue.Integer(5)
                ]);

        resultValue.Should().Be(ElmValue.FalseValue);
    }

    // ========== Tests for ge ==========

    [Fact]
    public void Ge_int_less_than()
    {
        var resultValue =
            ApplyGeneric(
                CoreBasics.Ge_FunctionValue(),
                [
                ElmValue.Integer(3),
                ElmValue.Integer(5)
                ]);

        resultValue.Should().Be(ElmValue.FalseValue);
    }

    [Fact]
    public void Ge_int_equal()
    {
        var resultValue =
            ApplyGeneric(
                CoreBasics.Ge_FunctionValue(),
                [
                ElmValue.Integer(5),
                ElmValue.Integer(5)
                ]);

        resultValue.Should().Be(ElmValue.TrueValue);
    }

    [Fact]
    public void Ge_int_greater_than()
    {
        var resultValue =
            ApplyGeneric(
                CoreBasics.Ge_FunctionValue(),
                [
                ElmValue.Integer(7),
                ElmValue.Integer(5)
                ]);

        resultValue.Should().Be(ElmValue.TrueValue);
    }

    // ========== Tests for and (&&) ==========

    [Fact]
    public void And_true_true()
    {
        var resultValue =
            ApplyDirectBinary(
                CoreBasics.Generic_And,
                ElmValue.TrueValue,
                ElmValue.TrueValue);

        resultValue.value.Should().Be(ElmValue.TrueValue);
    }

    [Fact]
    public void And_true_false()
    {
        var resultValue =
            ApplyDirectBinary(
                CoreBasics.Generic_And,
                ElmValue.TrueValue,
                ElmValue.FalseValue);

        resultValue.value.Should().Be(ElmValue.FalseValue);
    }

    [Fact]
    public void And_false_true()
    {
        var resultValue =
            ApplyDirectBinary(
                CoreBasics.Generic_And,
                ElmValue.FalseValue,
                ElmValue.TrueValue);

        resultValue.value.Should().Be(ElmValue.FalseValue);
    }

    [Fact]
    public void And_false_false()
    {
        var resultValue =
            ApplyDirectBinary(
                CoreBasics.Generic_And,
                ElmValue.FalseValue,
                ElmValue.FalseValue);

        resultValue.value.Should().Be(ElmValue.FalseValue);
    }

    // ========== Tests for or (||) ==========

    [Fact]
    public void Or_true_true()
    {
        var resultValue =
            ApplyDirectBinary(
                CoreBasics.Generic_Or,
                ElmValue.TrueValue,
                ElmValue.TrueValue);

        resultValue.value.Should().Be(ElmValue.TrueValue);
    }

    [Fact]
    public void Or_true_false()
    {
        var resultValue =
            ApplyDirectBinary(
                CoreBasics.Generic_Or,
                ElmValue.TrueValue,
                ElmValue.FalseValue);

        resultValue.value.Should().Be(ElmValue.TrueValue);
    }

    [Fact]
    public void Or_false_true()
    {
        var resultValue =
            ApplyDirectBinary(
                CoreBasics.Generic_Or,
                ElmValue.FalseValue,
                ElmValue.TrueValue);

        resultValue.value.Should().Be(ElmValue.TrueValue);
    }

    [Fact]
    public void Or_false_false()
    {
        var resultValue =
            ApplyDirectBinary(
                CoreBasics.Generic_Or,
                ElmValue.FalseValue,
                ElmValue.FalseValue);

        resultValue.value.Should().Be(ElmValue.FalseValue);
    }

    // ========== Tests for xor ==========

    [Fact]
    public void Xor_true_true()
    {
        var resultValue =
            ApplyDirectBinary(
                CoreBasics.Generic_Xor,
                ElmValue.TrueValue,
                ElmValue.TrueValue);

        resultValue.value.Should().Be(ElmValue.FalseValue);
    }

    [Fact]
    public void Xor_true_false()
    {
        var resultValue =
            ApplyDirectBinary(
                CoreBasics.Generic_Xor,
                ElmValue.TrueValue,
                ElmValue.FalseValue);

        resultValue.value.Should().Be(ElmValue.TrueValue);
    }

    [Fact]
    public void Xor_false_true()
    {
        var resultValue =
            ApplyDirectBinary(
                CoreBasics.Generic_Xor,
                ElmValue.FalseValue,
                ElmValue.TrueValue);

        resultValue.value.Should().Be(ElmValue.TrueValue);
    }

    [Fact]
    public void Xor_false_false()
    {
        var resultValue =
            ApplyDirectBinary(
                CoreBasics.Generic_Xor,
                ElmValue.FalseValue,
                ElmValue.FalseValue);

        resultValue.value.Should().Be(ElmValue.FalseValue);
    }

    // ========== Tests for not ==========

    [Fact]
    public void Not_true()
    {
        var resultValue =
            ApplyUnary(
                CoreBasics.Not_FunctionValue(),
                ElmValue.TrueValue);

        resultValue.Should().Be(ElmValue.FalseValue);
    }

    [Fact]
    public void Not_false()
    {
        var resultValue =
            ApplyUnary(
                CoreBasics.Not_FunctionValue(),
                ElmValue.FalseValue);

        resultValue.Should().Be(ElmValue.TrueValue);
    }

    // ========== Tests for negate ==========

    [Fact]
    public void Negate_positive_int()
    {
        var resultValue =
            ApplyUnary(
                CoreBasics.Negate_FunctionValue(),
                ElmValue.Integer(42));

        resultValue.Should().Be(ElmValue.Integer(-42));
    }

    [Fact]
    public void Negate_negative_int()
    {
        var resultValue =
            ApplyUnary(
                CoreBasics.Negate_FunctionValue(),
                ElmValue.Integer(-42));

        resultValue.Should().Be(ElmValue.Integer(42));
    }

    [Fact]
    public void Negate_zero()
    {
        var resultValue =
            ApplyUnary(
                CoreBasics.Negate_FunctionValue(),
                ElmValue.Integer(0));

        resultValue.Should().Be(ElmValue.Integer(0));
    }

    // ========== Tests for min ==========

    [Fact]
    public void Min_first_smaller()
    {
        var resultValue =
            ApplyGeneric(
                CoreBasics.Min_FunctionValue(),
                [
                ElmValue.Integer(3),
                ElmValue.Integer(7)
                ]);

        resultValue.Should().Be(ElmValue.Integer(3));
    }

    [Fact]
    public void Min_second_smaller()
    {
        var resultValue =
            ApplyGeneric(
                CoreBasics.Min_FunctionValue(),
                [
                ElmValue.Integer(7),
                ElmValue.Integer(3)
                ]);

        resultValue.Should().Be(ElmValue.Integer(3));
    }

    [Fact]
    public void Min_equal()
    {
        var resultValue =
            ApplyGeneric(
                CoreBasics.Min_FunctionValue(),
                [
                ElmValue.Integer(5),
                ElmValue.Integer(5)
                ]);

        resultValue.Should().Be(ElmValue.Integer(5));
    }

    // ========== Tests for max ==========

    [Fact]
    public void Max_first_larger()
    {
        var resultValue =
            ApplyGeneric(
                CoreBasics.Max_FunctionValue(),
                [
                ElmValue.Integer(7),
                ElmValue.Integer(3)
                ]);

        resultValue.Should().Be(ElmValue.Integer(7));
    }

    [Fact]
    public void Max_second_larger()
    {
        var resultValue =
            ApplyGeneric(
                CoreBasics.Max_FunctionValue(),
                [
                ElmValue.Integer(3),
                ElmValue.Integer(7)
                ]);

        resultValue.Should().Be(ElmValue.Integer(7));
    }

    [Fact]
    public void Max_equal()
    {
        var resultValue =
            ApplyGeneric(
                CoreBasics.Max_FunctionValue(),
                [
                ElmValue.Integer(5),
                ElmValue.Integer(5)
                ]);

        resultValue.Should().Be(ElmValue.Integer(5));
    }

    // ========== Tests for identity ==========

    [Fact]
    public void Identity_int()
    {
        var resultValue =
            ApplyUnary(
                CoreBasics.Identity_FunctionValue(),
                ElmValue.Integer(42));

        resultValue.Should().Be(ElmValue.Integer(42));
    }

    [Fact]
    public void Identity_string()
    {
        var resultValue =
            ApplyUnary(
                CoreBasics.Identity_FunctionValue(),
                ElmValue.StringInstance("hello"));

        resultValue.Should().Be(ElmValue.StringInstance("hello"));
    }

    // ========== Tests for always ==========

    [Fact]
    public void Always_returns_first()
    {
        var resultValue =
            ApplyGeneric(
                CoreBasics.Always_FunctionValue(),
                [
                ElmValue.Integer(42),
                ElmValue.StringInstance("ignored")
                ]);

        resultValue.Should().Be(ElmValue.Integer(42));
    }

    private static readonly PineVMParseCache s_pineVMParseCache = new();

    #region TryInterpret Tests

    [Fact]
    public void TryInterpret_Add_returns_number_add()
    {
        var leftExpr =
            Expression.LiteralInstance(ElmValueEncoding.ElmValueAsPineValue(ElmValue.Integer(13)));

        var rightExpr =
            Expression.LiteralInstance(ElmValueEncoding.ElmValueAsPineValue(ElmValue.Integer(17)));

        var composed =
            CoreBasics.Generic_Add(leftExpr, rightExpr);

        var result = CoreBasics.Identify(composed);

        result.Should().NotBeNull();

        result.Value.declName.Should().Be("add");

        result.Value.argsExprs.Should().Equal([leftExpr, rightExpr]);
    }

    [Fact]
    public void TryInterpret_Sub_returns_number_sub()
    {
        var leftExpr =
            Expression.LiteralInstance(ElmValueEncoding.ElmValueAsPineValue(ElmValue.Integer(13)));

        var rightExpr =
            Expression.LiteralInstance(ElmValueEncoding.ElmValueAsPineValue(ElmValue.Integer(17)));

        var composed =
            CoreBasics.Generic_Sub(leftExpr, rightExpr);

        var result = CoreBasics.Identify(composed);

        result.Should().NotBeNull();

        result.Value.declName.Should().Be("sub");

        result.Value.argsExprs.Should().Equal([leftExpr, rightExpr]);
    }

    [Fact]
    public void TryInterpret_Mul_returns_number_mul()
    {
        var leftExpr =
            Expression.LiteralInstance(ElmValueEncoding.ElmValueAsPineValue(ElmValue.Integer(13)));

        var rightExpr =
            Expression.LiteralInstance(ElmValueEncoding.ElmValueAsPineValue(ElmValue.Integer(17)));

        var composed =
            CoreBasics.Generic_Mul(leftExpr, rightExpr);

        var result = CoreBasics.Identify(composed);

        result.Should().NotBeNull();
        result.Value.declName.Should().Be("mul");
        result.Value.argsExprs.Should().Equal([leftExpr, rightExpr]);
    }

    [Fact]
    public void TryInterpret_Int_div_returns_int_div()
    {
        var leftExpr =
            Expression.LiteralInstance(ElmValueEncoding.ElmValueAsPineValue(ElmValue.Integer(21)));

        var rightExpr =
            Expression.LiteralInstance(ElmValueEncoding.ElmValueAsPineValue(ElmValue.Integer(7)));

        var composed =
            CoreBasics.Int_div(leftExpr, rightExpr);

        var result = CoreBasics.Identify(composed);

        result.Should().NotBeNull();
        result.Value.declName.Should().Be("idiv");
        result.Value.argsExprs.Should().Equal([leftExpr, rightExpr]);
    }

    [Fact]
    public void TryInterpret_Eq_returns_eq()
    {
        var leftExpr =
            Expression.LiteralInstance(ElmValueEncoding.ElmValueAsPineValue(ElmValue.Integer(13)));

        var rightExpr =
            Expression.LiteralInstance(ElmValueEncoding.ElmValueAsPineValue(ElmValue.Integer(17)));

        var composed =
            CoreBasics.Generic_Eq(leftExpr, rightExpr);

        var result = CoreBasics.Identify(composed);

        result.Should().NotBeNull();
        result.Value.declName.Should().Be("eq");
        result.Value.argsExprs.Should().Equal([leftExpr, rightExpr]);
    }

    [Fact]
    public void TryInterpret_Neq_returns_neq()
    {
        var leftExpr =
            Expression.LiteralInstance(ElmValueEncoding.ElmValueAsPineValue(ElmValue.Integer(13)));

        var rightExpr =
            Expression.LiteralInstance(ElmValueEncoding.ElmValueAsPineValue(ElmValue.Integer(17)));

        var composed =
            CoreBasics.Generic_Neq(leftExpr, rightExpr);

        var result = CoreBasics.Identify(composed);

        result.Should().NotBeNull();
        result.Value.declName.Should().Be("neq");
        result.Value.argsExprs.Should().Equal([leftExpr, rightExpr]);
    }

    [Fact]
    public void TryInterpret_Compare_returns_compare()
    {
        var leftExpr =
            Expression.LiteralInstance(ElmValueEncoding.ElmValueAsPineValue(ElmValue.Integer(13)));

        var rightExpr =
            Expression.LiteralInstance(ElmValueEncoding.ElmValueAsPineValue(ElmValue.Integer(17)));

        var composed =
            CoreBasics.Generic_Compare(leftExpr, rightExpr);

        var result = CoreBasics.Identify(composed);

        result.Should().NotBeNull();
        result.Value.declName.Should().Be("compare");
        result.Value.argsExprs.Should().Equal([leftExpr, rightExpr]);
    }

    [Fact]
    public void TryInterpret_Lt_returns_lt()
    {
        var leftExpr =
            Expression.LiteralInstance(ElmValueEncoding.ElmValueAsPineValue(ElmValue.Integer(13)));

        var rightExpr =
            Expression.LiteralInstance(ElmValueEncoding.ElmValueAsPineValue(ElmValue.Integer(17)));

        var composed =
            CoreBasics.Generic_Lt(leftExpr, rightExpr);

        var result = CoreBasics.Identify(composed);

        result.Should().NotBeNull();
        result.Value.declName.Should().Be("lt");
        result.Value.argsExprs.Should().Equal([leftExpr, rightExpr]);
    }

    [Fact]
    public void TryInterpret_Gt_returns_gt()
    {
        var leftExpr =
            Expression.LiteralInstance(ElmValueEncoding.ElmValueAsPineValue(ElmValue.Integer(13)));

        var rightExpr =
            Expression.LiteralInstance(ElmValueEncoding.ElmValueAsPineValue(ElmValue.Integer(17)));

        var composed =
            CoreBasics.Generic_Gt(leftExpr, rightExpr);

        var result = CoreBasics.Identify(composed);

        result.Should().NotBeNull();
        result.Value.declName.Should().Be("gt");
        result.Value.argsExprs.Should().Equal([leftExpr, rightExpr]);
    }

    [Fact]
    public void TryInterpret_Le_returns_le()
    {
        var leftExpr =
            Expression.LiteralInstance(ElmValueEncoding.ElmValueAsPineValue(ElmValue.Integer(13)));

        var rightExpr =
            Expression.LiteralInstance(ElmValueEncoding.ElmValueAsPineValue(ElmValue.Integer(17)));

        var composed =
            CoreBasics.Generic_Le(leftExpr, rightExpr);

        var result = CoreBasics.Identify(composed);

        result.Should().NotBeNull();
        result.Value.declName.Should().Be("le");
        result.Value.argsExprs.Should().Equal([leftExpr, rightExpr]);
    }

    [Fact]
    public void TryInterpret_Ge_returns_ge()
    {
        var leftExpr =
            Expression.LiteralInstance(ElmValueEncoding.ElmValueAsPineValue(ElmValue.Integer(13)));

        var rightExpr =
            Expression.LiteralInstance(ElmValueEncoding.ElmValueAsPineValue(ElmValue.Integer(17)));

        var composed =
            CoreBasics.Generic_Ge(leftExpr, rightExpr);

        var result = CoreBasics.Identify(composed);

        result.Should().NotBeNull();
        result.Value.declName.Should().Be("ge");
        result.Value.argsExprs.Should().Equal([leftExpr, rightExpr]);
    }

    [Fact]
    public void TryInterpret_Min_returns_min()
    {
        var leftExpr =
            Expression.LiteralInstance(ElmValueEncoding.ElmValueAsPineValue(ElmValue.Integer(13)));

        var rightExpr =
            Expression.LiteralInstance(ElmValueEncoding.ElmValueAsPineValue(ElmValue.Integer(17)));

        var composed =
            CoreBasics.Generic_Min(leftExpr, rightExpr);

        var result = CoreBasics.Identify(composed);

        result.Should().NotBeNull();
        result.Value.declName.Should().Be("min");
        result.Value.argsExprs.Should().Equal([leftExpr, rightExpr]);
    }

    [Fact]
    public void TryInterpret_Max_returns_max()
    {
        var leftExpr =
            Expression.LiteralInstance(ElmValueEncoding.ElmValueAsPineValue(ElmValue.Integer(13)));

        var rightExpr =
            Expression.LiteralInstance(ElmValueEncoding.ElmValueAsPineValue(ElmValue.Integer(17)));

        var composed =
            CoreBasics.Generic_Max(leftExpr, rightExpr);

        var result = CoreBasics.Identify(composed);

        result.Should().NotBeNull();
        result.Value.declName.Should().Be("max");
        result.Value.argsExprs.Should().Equal([leftExpr, rightExpr]);
    }

    [Fact]
    public void TryInterpret_Always_returns_always()
    {
        var leftExpr =
            Expression.LiteralInstance(ElmValueEncoding.ElmValueAsPineValue(ElmValue.Integer(13)));

        var rightExpr =
            Expression.LiteralInstance(ElmValueEncoding.ElmValueAsPineValue(ElmValue.Integer(17)));

        var composed =
            CoreBasics.Generic_Always(leftExpr, rightExpr);

        var result = CoreBasics.Identify(composed);

        result.Should().NotBeNull();
        result.Value.declName.Should().Be("always");
        result.Value.argsExprs.Should().Equal([leftExpr, rightExpr]);
    }

    [Fact]
    public void TryInterpret_non_arithmetic_returns_null()
    {
        // A ParseAndEval that doesn't match any arithmetic pattern
        var someExpr = Expression.LiteralInstance(PineValue.EmptyList);

        var parseAndEval =
            new Expression.ParseAndEval(
                encoded: Expression.LiteralInstance(PineValue.EmptyList),
                environment: Expression.ListInstance([someExpr, someExpr]));

        var result = CoreBasics.Identify(parseAndEval);

        result.Should().BeNull();
    }

    [Fact]
    public void TryInterpret_non_ParseAndEval_returns_null()
    {
        // A non-ParseAndEval expression
        var literalExpr =
            Expression.LiteralInstance(ElmValueEncoding.ElmValueAsPineValue(ElmValue.Integer(42)));

        var result = CoreBasics.Identify(literalExpr);

        result.Should().BeNull();
    }

    [Fact]
    public void TryInterpret_wrong_environment_shape_returns_null()
    {
        // ParseAndEval with environment that's not a 2-element list
        var singleArgExpr =
            Expression.LiteralInstance(ElmValueEncoding.ElmValueAsPineValue(ElmValue.Integer(1)));

        var parseAndEval =
            new Expression.ParseAndEval(
                encoded: Expression.LiteralInstance(CoreBasics.Add_FunctionValue()),
                environment: Expression.ListInstance([singleArgExpr])); // Only 1 argument

        var result = CoreBasics.Identify(parseAndEval);

        result.Should().BeNull();
    }

    #endregion

    // ========== Tests for append (++) ==========

    #region append

    [Fact]
    public void Generic_append_string_string()
    {
        var resultValue =
            ApplyGeneric(
                CoreBasics.Append_FunctionValue(),
                [
                ElmValue.StringInstance("hello"),
                ElmValue.StringInstance(" world")
                ]);

        resultValue.Should().Be(ElmValue.StringInstance("hello world"));
    }

    [Fact]
    public void Generic_append_empty_strings()
    {
        var resultValue =
            ApplyGeneric(
                CoreBasics.Append_FunctionValue(),
                [
                ElmValue.StringInstance(""),
                ElmValue.StringInstance("")
                ]);

        resultValue.Should().Be(ElmValue.StringInstance(""));
    }

    [Fact]
    public void Generic_append_string_empty()
    {
        var resultValue =
            ApplyGeneric(
                CoreBasics.Append_FunctionValue(),
                [
                ElmValue.StringInstance("abc"),
                ElmValue.StringInstance("")
                ]);

        resultValue.Should().Be(ElmValue.StringInstance("abc"));
    }

    [Fact]
    public void Generic_append_empty_string()
    {
        var resultValue =
            ApplyGeneric(
                CoreBasics.Append_FunctionValue(),
                [
                ElmValue.StringInstance(""),
                ElmValue.StringInstance("xyz")
                ]);

        resultValue.Should().Be(ElmValue.StringInstance("xyz"));
    }

    [Fact]
    public void Generic_append_list_list()
    {
        var resultValue =
            ApplyGeneric(
                CoreBasics.Append_FunctionValue(),
                [
                ElmValue.ListInstance([ElmValue.Integer(1), ElmValue.Integer(2)]),
                ElmValue.ListInstance([ElmValue.Integer(3), ElmValue.Integer(4)])
                ]);

        resultValue.Should().Be(
            ElmValue.ListInstance(
                [
                ElmValue.Integer(1),
                ElmValue.Integer(2),
                ElmValue.Integer(3),
                ElmValue.Integer(4)
                ]));
    }

    [Fact]
    public void Generic_append_empty_lists()
    {
        var resultValue =
            ApplyGeneric(
                CoreBasics.Append_FunctionValue(),
                [
                ElmValue.ListInstance([]),
                ElmValue.ListInstance([])
                ]);

        resultValue.Should().Be(ElmValue.ListInstance([]));
    }

    [Fact]
    public void Generic_append_list_empty()
    {
        var resultValue =
            ApplyGeneric(
                CoreBasics.Append_FunctionValue(),
                [
                ElmValue.ListInstance([ElmValue.Integer(1)]),
                ElmValue.ListInstance([])
                ]);

        resultValue.Should().Be(ElmValue.ListInstance([ElmValue.Integer(1)]));
    }

    [Fact]
    public void Generic_append_empty_list()
    {
        var resultValue =
            ApplyGeneric(
                CoreBasics.Append_FunctionValue(),
                [
                ElmValue.ListInstance([]),
                ElmValue.ListInstance([ElmValue.Integer(2)])
                ]);

        resultValue.Should().Be(ElmValue.ListInstance([ElmValue.Integer(2)]));
    }

    [Fact]
    public void Identify_append_function_value()
    {
        var result = CoreBasics.IdentifyFunctionValue(CoreBasics.Append_FunctionValue());

        result.Should().Be("append");
    }

    #endregion

    private static readonly Core.Interpreter.IntermediateVM.PineVM s_vm =
        ElmCompilerTestHelper.PineVMForProfiling(_ => { });

    private static (ElmValue value, EvaluationReport profile) ApplyDirectBinary(
        System.Func<Expression, Expression, Expression> function,
        ElmValue left,
        ElmValue right) =>
        CoreLibraryTestHelper.ApplyDirectBinary(function, left, right);

    private static ElmValue ApplyUnary(
        PineValue functionValue,
        ElmValue argument) =>
        CoreLibraryTestHelper.ApplyUnary(functionValue, argument, s_vm);

    private static ElmValue ApplyGeneric(
        PineValue functionValue,
        ElmValue[] arguments) =>
        CoreLibraryTestHelper.ApplyGeneric(functionValue, arguments, s_vm);
}
