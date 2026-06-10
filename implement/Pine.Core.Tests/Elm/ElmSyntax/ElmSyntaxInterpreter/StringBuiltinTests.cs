using AwesomeAssertions;
using System;
using Xunit;

using ElmInterpreter = Pine.Core.Elm.ElmSyntax.ElmSyntaxInterpreter;

namespace Pine.Core.Tests.Elm.ElmSyntax.ElmSyntaxInterpreter;

/// <summary>
/// Exercises functions from <c>elm-kernel-modules/String.elm</c> — <c>split</c>, <c>join</c>,
/// <c>lines</c>, <c>fromInt</c>, <c>toInt</c>, <c>toList</c>, <c>fromList</c>, <c>reverse</c>,
/// <c>slice</c>, <c>contains</c>, <c>trim</c> (via <c>trimLeftCountBytesTrimmed</c> /
/// <c>trimRightCountBytesRemaining</c>), <c>toFloat</c>, and <c>fromFloat</c> —
/// analogous to <see cref="BasicsBuiltinTests"/>. The kernel
/// modules are canonicalized into a single <see cref="ElmInterpreter.Prepared"/> program and
/// each scenario is evaluated to a concrete <see cref="PineValue"/> via
/// <see cref="InterpreterTestHelper.EvaluateInModulesToPineValue(string, ElmInterpreter.Prepared)"/>,
/// then compared against the <see cref="PineValue"/> of an expected Elm expression. <c>String</c>
/// is implicitly imported but is addressed here by its fully-qualified canonical name for
/// clarity.
/// </summary>
public class StringBuiltinTests
{
    private static readonly Lazy<ElmInterpreter.Prepared> s_prepared =
        new(() => InterpreterTestHelper.PrepareKernelModules(
            "String.elm",
            "List.elm",
            "Basics.elm",
            "Maybe.elm",
            "Char.elm"));

    private static PineValue Evaluate(string expression) =>
        InterpreterTestHelper.EvaluateInModulesToPineValue(expression, s_prepared.Value);

    private static void AssertEvaluatesEqual(string expression, string expectedExpression) =>
        Evaluate(expression).Should().Be(Evaluate(expectedExpression));

    /// <summary>
    /// Asserts that the builtin short-circuit produces exactly the same <see cref="PineValue"/> as
    /// the user-defined Elm-source implementation (builtins disabled), proving the builtin is a
    /// faithful replacement.
    /// </summary>
    private static void AssertBuiltinMatchesElm(string expression) =>
        InterpreterTestHelper.EvaluateInModulesToPineValue(expression, s_prepared.Value)
            .Should()
            .Be(InterpreterTestHelper.EvaluateInModulesWithoutBuiltinsToPineValue(expression, s_prepared.Value));

    // ============================================================
    // split
    // ============================================================

    [Theory]
    [InlineData("String.split \",\" \"a,b,c\"", "[ \"a\", \"b\", \"c\" ]")]
    [InlineData("String.split \",\" \"a\"", "[ \"a\" ]")]
    [InlineData("String.split \",\" \"\"", "[ \"\" ]")]
    [InlineData("String.split \",\" \"a,,c\"", "[ \"a\", \"\", \"c\" ]")]
    [InlineData("String.split \",\" \",a,\"", "[ \"\", \"a\", \"\" ]")]
    [InlineData("String.split \", \" \"a, b, c\"", "[ \"a\", \"b\", \"c\" ]")]
    [InlineData("String.split \"\" \"abc\"", "[ \"a\", \"b\", \"c\" ]")]
    public void Split_matches_Elm_semantics(string expression, string expected) =>
        AssertEvaluatesEqual(expression, expected);

    // ============================================================
    // join
    // ============================================================

    [Theory]
    [InlineData("String.join \", \" [ \"a\", \"b\", \"c\" ]", "\"a, b, c\"")]
    [InlineData("String.join \"\" [ \"a\", \"b\", \"c\" ]", "\"abc\"")]
    [InlineData("String.join \", \" [ \"a\" ]", "\"a\"")]
    [InlineData("String.join \", \" [ \"\" ]", "\"\"")]
    [InlineData("String.join \"-\" [ \"1\", \"2\", \"3\" ]", "\"1-2-3\"")]
    public void Join_matches_Elm_semantics(string expression, string expected) =>
        AssertEvaluatesEqual(expression, expected);

    // join and split are inverses when the separator does not occur in any segment.
    [Theory]
    [InlineData(",", "a,b,c")]
    [InlineData(", ", "alpha, beta, gamma")]
    public void Join_of_split_round_trips(string separator, string text) =>
        AssertEvaluatesEqual(
            "String.join \"" + separator + "\" (String.split \"" + separator + "\" \"" + text + "\")",
            "\"" + text + "\"");

    // ============================================================
    // lines
    // ============================================================

    [Theory]
    [InlineData("String.lines \"a\\nb\\nc\"", "[ \"a\", \"b\", \"c\" ]")]
    [InlineData("String.lines \"a\"", "[ \"a\" ]")]
    [InlineData("String.lines \"\"", "[ \"\" ]")]
    [InlineData("String.lines \"a\\n\\nb\"", "[ \"a\", \"\", \"b\" ]")]
    [InlineData("String.lines \"a\\r\\nb\"", "[ \"a\", \"b\" ]")]
    [InlineData("String.lines \"a\\nb\\n\"", "[ \"a\", \"b\", \"\" ]")]
    public void Lines_matches_Elm_semantics(string expression, string expected) =>
        AssertEvaluatesEqual(expression, expected);

    // ============================================================
    // fromInt
    // ============================================================

    [Theory]
    [InlineData("String.fromInt 0", "\"0\"")]
    [InlineData("String.fromInt 42", "\"42\"")]
    [InlineData("String.fromInt -7", "\"-7\"")]
    [InlineData("String.fromInt 1000000", "\"1000000\"")]
    [InlineData("String.fromInt -1000000", "\"-1000000\"")]
    public void FromInt_matches_Elm_semantics(string expression, string expected) =>
        AssertEvaluatesEqual(expression, expected);

    // ============================================================
    // toInt
    // ============================================================

    [Theory]
    [InlineData("String.toInt \"0\"", "Just 0")]
    [InlineData("String.toInt \"42\"", "Just 42")]
    [InlineData("String.toInt \"-7\"", "Just -7")]
    [InlineData("String.toInt \"+7\"", "Just 7")]
    [InlineData("String.toInt \"007\"", "Just 7")]
    [InlineData("String.toInt \"1000000\"", "Just 1000000")]
    [InlineData("String.toInt \"\"", "Nothing")]
    [InlineData("String.toInt \"-\"", "Nothing")]
    [InlineData("String.toInt \"12a\"", "Nothing")]
    [InlineData("String.toInt \"a12\"", "Nothing")]
    [InlineData("String.toInt \"1.5\"", "Nothing")]
    public void ToInt_matches_Elm_semantics(string expression, string expected) =>
        AssertEvaluatesEqual(expression, expected);

    // fromInt and toInt round-trip for any integer.
    [Theory]
    [InlineData("0")]
    [InlineData("42")]
    [InlineData("-7")]
    [InlineData("1234567")]
    public void ToInt_of_fromInt_round_trips(string integer) =>
        AssertEvaluatesEqual(
            "String.toInt (String.fromInt " + integer + ")",
            "Just " + integer);

    // ============================================================
    // toList / fromList
    // ============================================================

    [Theory]
    [InlineData("String.toList \"abc\"", "[ 'a', 'b', 'c' ]")]
    [InlineData("String.toList \"a\"", "[ 'a' ]")]
    [InlineData("String.toList \"\"", "[]")]
    [InlineData("String.toList \"1 2\"", "[ '1', ' ', '2' ]")]
    public void ToList_matches_Elm_semantics(string expression, string expected) =>
        AssertEvaluatesEqual(expression, expected);

    [Theory]
    [InlineData("String.fromList [ 'a', 'b', 'c' ]", "\"abc\"")]
    [InlineData("String.fromList [ 'a' ]", "\"a\"")]
    [InlineData("String.fromList []", "\"\"")]
    [InlineData("String.fromList [ '1', ' ', '2' ]", "\"1 2\"")]
    public void FromList_matches_Elm_semantics(string expression, string expected) =>
        AssertEvaluatesEqual(expression, expected);

    // fromList and toList are inverses.
    [Theory]
    [InlineData("abc")]
    [InlineData("hello world")]
    [InlineData("")]
    public void FromList_of_toList_round_trips(string text) =>
        AssertEvaluatesEqual(
            "String.fromList (String.toList \"" + text + "\")",
            "\"" + text + "\"");

    // ============================================================
    // reverse
    // ============================================================

    [Theory]
    [InlineData("String.reverse \"abc\"", "\"cba\"")]
    [InlineData("String.reverse \"a\"", "\"a\"")]
    [InlineData("String.reverse \"\"", "\"\"")]
    [InlineData("String.reverse \"stressed\"", "\"desserts\"")]
    public void Reverse_matches_Elm_semantics(string expression, string expected) =>
        AssertEvaluatesEqual(expression, expected);

    // reverse is an involution.
    [Theory]
    [InlineData("abc")]
    [InlineData("hello world")]
    public void Reverse_of_reverse_round_trips(string text) =>
        AssertEvaluatesEqual(
            "String.reverse (String.reverse \"" + text + "\")",
            "\"" + text + "\"");

    // ============================================================
    // slice
    // ============================================================

    [Theory]
    // Non-negative indexes.
    [InlineData("String.slice 7 9 \"snakes on a plane!\"", "\"on\"")]
    [InlineData("String.slice 0 6 \"snakes on a plane!\"", "\"snakes\"")]
    [InlineData("String.slice 0 0 \"abc\"", "\"\"")]
    [InlineData("String.slice 1 3 \"abcde\"", "\"bc\"")]
    // Negative end index (counts from the end).
    [InlineData("String.slice 0 -7 \"snakes on a plane!\"", "\"snakes on a\"")]
    [InlineData("String.slice 0 -1 \"abc\"", "\"ab\"")]
    // Negative start and end indexes.
    [InlineData("String.slice -6 -1 \"snakes on a plane!\"", "\"plane\"")]
    [InlineData("String.slice -3 -1 \"abcde\"", "\"cd\"")]
    // Negative start index, non-negative end index.
    [InlineData("String.slice -3 5 \"abcde\"", "\"cde\"")]
    // Whole string via negative bounds.
    [InlineData("String.slice -5 5 \"abcde\"", "\"abcde\"")]
    // Empty / out-of-order slices.
    [InlineData("String.slice 3 2 \"abcde\"", "\"\"")]
    [InlineData("String.slice -1 -3 \"abcde\"", "\"\"")]
    public void Slice_matches_Elm_semantics(string expression, string expected) =>
        AssertEvaluatesEqual(expression, expected);

    // ============================================================
    // contains
    // ============================================================

    [Theory]
    [InlineData("String.contains \"the\" \"theory\"", "True")]
    [InlineData("String.contains \"hat\" \"theory\"", "False")]
    [InlineData("String.contains \"the\" \"a theory\"", "True")]
    [InlineData("String.contains \"\" \"theory\"", "True")]
    [InlineData("String.contains \"\" \"\"", "True")]
    [InlineData("String.contains \"a\" \"\"", "False")]
    [InlineData("String.contains \"theory\" \"the\"", "False")]
    [InlineData("String.contains \"plane\" \"snakes on a plane!\"", "True")]
    public void Contains_matches_Elm_semantics(string expression, string expected) =>
        AssertEvaluatesEqual(expression, expected);

    // ============================================================
    // trim (exercises trimLeftCountBytesTrimmed / trimRightCountBytesRemaining)
    // ============================================================

    [Theory]
    [InlineData("String.trim \"  hats  \"", "\"hats\"")]
    [InlineData("String.trim \"\\n\\thats\\r\\n\"", "\"hats\"")]
    [InlineData("String.trim \"hats\"", "\"hats\"")]
    [InlineData("String.trim \"   \"", "\"\"")]
    [InlineData("String.trim \"\"", "\"\"")]
    [InlineData("String.trimLeft \"  hats  \"", "\"hats  \"")]
    [InlineData("String.trimRight \"  hats  \"", "\"  hats\"")]
    public void Trim_matches_Elm_semantics(string expression, string expected) =>
        AssertEvaluatesEqual(expression, expected);

    // ============================================================
    // toFloat / fromFloat
    // ============================================================

    // fromFloat applied to a plain integer reduces to fromInt.
    [Theory]
    [InlineData("String.fromFloat 0", "\"0\"")]
    [InlineData("String.fromFloat 42", "\"42\"")]
    [InlineData("String.fromFloat -7", "\"-7\"")]
    public void FromFloat_of_integer_matches_Elm_semantics(string expression, string expected) =>
        AssertEvaluatesEqual(expression, expected);

    // toFloat followed by fromFloat round-trips the decimal rendering of a number.
    [Theory]
    [InlineData("1.5")]
    [InlineData("0.5")]
    [InlineData("-1.5")]
    [InlineData("3.14")]
    [InlineData("42")]
    [InlineData("-7")]
    [InlineData("100.25")]
    [InlineData("0.125")]
    public void FromFloat_of_toFloat_round_trips(string number) =>
        AssertEvaluatesEqual(
            "case String.toFloat \"" + number + "\" of\n"
            + "    Just f ->\n"
            + "        String.fromFloat f\n\n"
            + "    Nothing ->\n"
            + "        \"ERR\"",
            "\"" + number + "\"");

    // toFloat returns Nothing for malformed input.
    [Theory]
    [InlineData("String.toFloat \"\"", "Nothing")]
    [InlineData("String.toFloat \"abc\"", "Nothing")]
    [InlineData("String.toFloat \"1.2.3\"", "Nothing")]
    [InlineData("String.toFloat \"1e\"", "Nothing")]
    public void ToFloat_of_malformed_input_is_Nothing(string expression, string expected) =>
        AssertEvaluatesEqual(expression, expected);

    // toFloat of scientific notation round-trips through fromFloat to a plain decimal.
    [Theory]
    [InlineData("15e-1", "1.5")]
    [InlineData("15e1", "150")]
    [InlineData("1.5e2", "150")]
    public void ToFloat_of_scientific_notation_round_trips(string number, string expectedDecimal) =>
        AssertEvaluatesEqual(
            "case String.toFloat \"" + number + "\" of\n"
            + "    Just f ->\n"
            + "        String.fromFloat f\n\n"
            + "    Nothing ->\n"
            + "        \"ERR\"",
            "\"" + expectedDecimal + "\"");

    // ============================================================
    // lines
    // ============================================================

    [Theory]
    [InlineData("String.lines \"a\\rb\"", "[ \"a\", \"b\" ]")]
    [InlineData("String.lines \"a\\r\\nb\\nc\"", "[ \"a\", \"b\", \"c\" ]")]
    public void Lines_with_carriage_returns_matches_Elm_semantics(string expression, string expected) =>
        AssertEvaluatesEqual(expression, expected);

    // ============================================================
    // Each builtin reproduces the user-defined Elm implementation exactly.
    // ============================================================

    [Theory]
    [InlineData("String.slice 7 9 \"snakes on a plane!\"")]
    [InlineData("String.slice 0 -7 \"snakes on a plane!\"")]
    [InlineData("String.slice -6 -1 \"snakes on a plane!\"")]
    [InlineData("String.slice -3 5 \"abcde\"")]
    [InlineData("String.slice 3 2 \"abcde\"")]
    [InlineData("String.lines \"a\\nb\\r\\nc\\rd\"")]
    [InlineData("String.contains \"the\" \"a theory\"")]
    [InlineData("String.contains \"\" \"theory\"")]
    [InlineData("String.contains \"xyz\" \"theory\"")]
    [InlineData("String.trim \"\\n\\t hats \\r\\n\"")]
    [InlineData("String.trimLeft \"  hats  \"")]
    [InlineData("String.trimRight \"  hats  \"")]
    [InlineData("String.toFloat \"1.5\"")]
    [InlineData("String.toFloat \"-3.14\"")]
    [InlineData("String.toFloat \"15e-1\"")]
    [InlineData("String.toFloat \"1.5E2\"")]
    [InlineData("String.toFloat \"abc\"")]
    [InlineData("String.toFloat \"\"")]
    [InlineData("String.fromFloat 42")]
    public void Builtin_matches_user_defined_Elm_implementation(string expression) =>
        AssertBuiltinMatchesElm(expression);
}
