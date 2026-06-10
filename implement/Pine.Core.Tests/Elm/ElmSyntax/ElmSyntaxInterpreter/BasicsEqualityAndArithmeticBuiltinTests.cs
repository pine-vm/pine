using AwesomeAssertions;
using System;
using Xunit;

using ElmInterpreter = Pine.Core.Elm.ElmSyntax.ElmSyntaxInterpreter;

namespace Pine.Core.Tests.Elm.ElmSyntax.ElmSyntaxInterpreter;

/// <summary>
/// Exercises functions from <c>elm-kernel-modules/Basics.elm</c> — <c>eq</c>, <c>neq</c>,
/// <c>idiv</c>, and <c>mul</c> — analogous to <see cref="BasicsBuiltinTests"/> (which covers
/// <c>compare</c>). The kernel modules are loaded verbatim and canonicalized into a single
/// <see cref="ElmInterpreter.Prepared"/> program, then each scenario is evaluated to a
/// concrete <see cref="PineValue"/> via
/// <see cref="InterpreterTestHelper.EvaluateInModulesToPineValue(string, ElmInterpreter.Prepared)"/>
/// and compared against the <see cref="PineValue"/> of an expected Elm expression. Operating
/// on the <see cref="PineValue"/> directly avoids any conversion to <see cref="Core.Elm.ElmValue"/>;
/// none of these functions accepts a function argument, so plain-data operands always
/// materialize cleanly.
/// <para>
/// <c>eq</c> / <c>neq</c> are exercised both via direct application and via their infix
/// operators (<c>==</c> and <c>/=</c>), and <c>idiv</c> / <c>mul</c> both via direct
/// application and via their operators (<c>//</c> and <c>*</c>). The <c>Dict</c> scenarios
/// pin that <c>eq</c> treats two dicts holding the same key/value pairs as equal regardless
/// of the insertion order that built them (red-black trees with different internal balancing).
/// </para>
/// </summary>
public class BasicsEqualityAndArithmeticBuiltinTests
{
    private static readonly Lazy<ElmInterpreter.Prepared> s_prepared =
        new(() => InterpreterTestHelper.PrepareKernelModules(
            "Basics.elm",
            "List.elm",
            "Maybe.elm",
            "Char.elm",
            "Dict.elm"));

    private static PineValue Evaluate(string expression) =>
        InterpreterTestHelper.EvaluateInModulesToPineValue(expression, s_prepared.Value);

    private static void AssertEvaluatesEqual(string expression, string expectedExpression) =>
        Evaluate(expression).Should().Be(Evaluate(expectedExpression));

    // ============================================================
    // eq / == on primitives and structural values
    // ============================================================

    [Theory]
    [InlineData("Basics.eq 1 1", "True")]
    [InlineData("Basics.eq 1 2", "False")]
    [InlineData("Basics.eq -3 -3", "True")]
    [InlineData("Basics.eq \"abc\" \"abc\"", "True")]
    [InlineData("Basics.eq \"abc\" \"abd\"", "False")]
    [InlineData("Basics.eq 'a' 'a'", "True")]
    [InlineData("Basics.eq 'a' 'b'", "False")]
    [InlineData("Basics.eq [ 1, 2, 3 ] [ 1, 2, 3 ]", "True")]
    [InlineData("Basics.eq [ 1, 2, 3 ] [ 1, 2, 4 ]", "False")]
    [InlineData("Basics.eq ( 1, \"a\" ) ( 1, \"a\" )", "True")]
    [InlineData("Basics.eq ( 1, \"a\" ) ( 1, \"b\" )", "False")]
    [InlineData("Basics.eq { x = 1, y = 2 } { x = 1, y = 2 }", "True")]
    [InlineData("Basics.eq { x = 1, y = 2 } { x = 1, y = 3 }", "False")]
    public void Eq_matches_Elm_semantics(string expression, string expected) =>
        AssertEvaluatesEqual(expression, expected);

    [Theory]
    [InlineData("1 == 1", "True")]
    [InlineData("1 == 2", "False")]
    [InlineData("\"abc\" == \"abc\"", "True")]
    [InlineData("[ 1, 2 ] == [ 1, 2 ]", "True")]
    [InlineData("( 1, 2 ) == ( 1, 3 )", "False")]
    public void Eq_infix_operator_matches_Elm_semantics(string expression, string expected) =>
        AssertEvaluatesEqual(expression, expected);

    // ============================================================
    // neq / /=
    // ============================================================

    [Theory]
    [InlineData("Basics.neq 1 1", "False")]
    [InlineData("Basics.neq 1 2", "True")]
    [InlineData("Basics.neq \"abc\" \"abd\"", "True")]
    [InlineData("Basics.neq [ 1, 2 ] [ 1, 2 ]", "False")]
    public void Neq_matches_Elm_semantics(string expression, string expected) =>
        AssertEvaluatesEqual(expression, expected);

    [Theory]
    [InlineData("1 /= 1", "False")]
    [InlineData("1 /= 2", "True")]
    [InlineData("\"a\" /= \"b\"", "True")]
    public void Neq_infix_operator_matches_Elm_semantics(string expression, string expected) =>
        AssertEvaluatesEqual(expression, expected);

    // neq is the negation of eq for the same operands.
    [Theory]
    [InlineData("1 1")]
    [InlineData("1 2")]
    [InlineData("\"abc\" \"abc\"")]
    [InlineData("[ 1, 2, 3 ] [ 1, 2, 4 ]")]
    public void Neq_is_the_negation_of_eq(string operands) =>
        AssertEvaluatesEqual(
            "Basics.neq " + operands,
            "Basics.not (Basics.eq " + operands + ")");

    // ============================================================
    // eq on Dict values — independent of insertion order
    //
    // Dict.elm is a red-black tree, so different insertion orders can produce
    // structurally different trees that nevertheless represent the same key/value
    // mapping. `eq` routes RBNode values through an in-order traversal before
    // comparing, so the observable equality is order-independent.
    // ============================================================

    [Fact]
    public void Eq_on_empty_dicts_returns_True() =>
        AssertEvaluatesEqual("Basics.eq Dict.empty Dict.empty", "True");

    [Fact]
    public void Eq_on_dicts_built_in_different_insertion_orders_returns_True()
    {
        const string DictAscending =
            "Dict.fromList [ ( \"a\", 1 ), ( \"b\", 2 ), ( \"c\", 3 ), ( \"d\", 4 ), ( \"e\", 5 ) ]";

        const string DictPermuted =
            "Dict.fromList [ ( \"e\", 5 ), ( \"c\", 3 ), ( \"a\", 1 ), ( \"d\", 4 ), ( \"b\", 2 ) ]";

        AssertEvaluatesEqual(
            "Basics.eq (" + DictAscending + ") (" + DictPermuted + ")",
            "True");
    }

    [Fact]
    public void Eq_via_operator_on_dicts_built_in_different_insertion_orders_returns_True()
    {
        // Seven keys exercise the red-black tree's rebalancing rotations. Whatever the
        // internal tree shape, `==` must report the two dicts as equal.
        const string Ascending =
            "Dict.fromList [ ( 1, \"a\" ), ( 2, \"b\" ), ( 3, \"c\" ), ( 4, \"d\" ), ( 5, \"e\" ), ( 6, \"f\" ), ( 7, \"g\" ) ]";

        const string Descending =
            "Dict.fromList [ ( 7, \"g\" ), ( 6, \"f\" ), ( 5, \"e\" ), ( 4, \"d\" ), ( 3, \"c\" ), ( 2, \"b\" ), ( 1, \"a\" ) ]";

        AssertEvaluatesEqual("(" + Ascending + ") == (" + Descending + ")", "True");
    }

    [Fact]
    public void Eq_on_dicts_with_one_value_differing_returns_False()
    {
        const string DictA =
            "Dict.fromList [ ( \"a\", 1 ), ( \"b\", 2 ), ( \"c\", 3 ) ]";

        const string DictB =
            "Dict.fromList [ ( \"a\", 1 ), ( \"b\", 9 ), ( \"c\", 3 ) ]";

        AssertEvaluatesEqual(
            "Basics.eq (" + DictA + ") (" + DictB + ")",
            "False");
    }

    // ============================================================
    // idiv ( // ) — integer division truncating toward zero
    // ============================================================

    [Theory]
    [InlineData("Basics.idiv 7 2", "3")]
    [InlineData("Basics.idiv 6 2", "3")]
    [InlineData("Basics.idiv 7 3", "2")]
    [InlineData("Basics.idiv 0 5", "0")]
    [InlineData("Basics.idiv -7 2", "-3")]
    [InlineData("Basics.idiv 7 -2", "-3")]
    [InlineData("Basics.idiv -7 -2", "3")]
    [InlineData("Basics.idiv 5 0", "0")]
    public void Idiv_matches_Elm_semantics(string expression, string expected) =>
        AssertEvaluatesEqual(expression, expected);

    [Theory]
    [InlineData("7 // 2", "3")]
    [InlineData("9 // 3", "3")]
    [InlineData("-7 // 2", "-3")]
    public void Idiv_infix_operator_matches_Elm_semantics(string expression, string expected) =>
        AssertEvaluatesEqual(expression, expected);

    // ============================================================
    // mul ( * )
    // ============================================================

    [Theory]
    [InlineData("Basics.mul 6 7", "42")]
    [InlineData("Basics.mul 0 5", "0")]
    [InlineData("Basics.mul -3 4", "-12")]
    [InlineData("Basics.mul -3 -4", "12")]
    [InlineData("Basics.mul 123 1000", "123000")]
    public void Mul_matches_Elm_semantics(string expression, string expected) =>
        AssertEvaluatesEqual(expression, expected);

    [Theory]
    [InlineData("6 * 7", "42")]
    [InlineData("-3 * 4", "-12")]
    [InlineData("2 * 3 * 4", "24")]
    public void Mul_infix_operator_matches_Elm_semantics(string expression, string expected) =>
        AssertEvaluatesEqual(expression, expected);

    // ============================================================
    // modBy — modulo carrying the sign of the divisor
    // ============================================================

    [Theory]
    [InlineData("Basics.modBy 4 14", "2")]
    [InlineData("Basics.modBy 4 13", "1")]
    [InlineData("Basics.modBy 4 12", "0")]
    [InlineData("Basics.modBy 2 1", "1")]
    [InlineData("Basics.modBy 1 7", "0")]
    // Negative dividend: the result follows the (positive) divisor's sign.
    [InlineData("Basics.modBy 4 -1", "3")]
    [InlineData("Basics.modBy 4 -14", "2")]
    [InlineData("Basics.modBy 12 -1", "11")]
    [InlineData("Basics.modBy 3 -3", "0")]
    // Division by zero yields the dividend, matching the Elm kernel implementation.
    [InlineData("Basics.modBy 0 5", "5")]
    [InlineData("Basics.modBy 0 -5", "-5")]
    public void ModBy_matches_Elm_semantics(string expression, string expected) =>
        AssertEvaluatesEqual(expression, expected);

    // The builtin must produce exactly the same value as the user-defined Elm implementation
    // (compared with the default builtins disabled) for every operand-sign combination,
    // including negative divisors where modBy differs from remainderBy.
    [Theory]
    [InlineData("Basics.modBy 4 14")]
    [InlineData("Basics.modBy 4 -14")]
    [InlineData("Basics.modBy 7 100")]
    [InlineData("Basics.modBy 7 -100")]
    [InlineData("Basics.modBy -3 7")]
    [InlineData("Basics.modBy -3 -7")]
    [InlineData("Basics.modBy 1 123")]
    [InlineData("Basics.modBy 0 5")]
    [InlineData("17 |> Basics.modBy 5")]
    public void ModBy_builtin_matches_Elm_implementation(string expression) =>
        InterpreterTestHelper.EvaluateInModulesToPineValue(expression, s_prepared.Value)
        .Should().Be(
            InterpreterTestHelper.EvaluateInModulesWithoutBuiltinsToPineValue(expression, s_prepared.Value));
}
