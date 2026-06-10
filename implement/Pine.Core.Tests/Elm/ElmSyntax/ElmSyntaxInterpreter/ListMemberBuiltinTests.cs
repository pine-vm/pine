using AwesomeAssertions;
using System;
using Xunit;

using ElmInterpreter = Pine.Core.Elm.ElmSyntax.ElmSyntaxInterpreter;

namespace Pine.Core.Tests.Elm.ElmSyntax.ElmSyntaxInterpreter;

/// <summary>
/// Exercises <c>List.member</c> from <c>elm-kernel-modules/List.elm</c>, analogous to
/// <see cref="BasicsBuiltinTests"/>. The kernel modules are canonicalized into a single
/// <see cref="ElmInterpreter.Prepared"/> program and each scenario is evaluated to a concrete
/// <see cref="PineValue"/> via
/// <see cref="InterpreterTestHelper.EvaluateInModulesToPineValue(string, ElmInterpreter.Prepared)"/>,
/// then compared against the <see cref="PineValue"/> of an expected Elm expression.
/// </summary>
public class ListMemberBuiltinTests
{
    private static readonly Lazy<ElmInterpreter.Prepared> s_prepared =
        new(() => InterpreterTestHelper.PrepareKernelModules(
            "List.elm",
            "Basics.elm",
            "Maybe.elm",
            "Char.elm"));

    private static PineValue Evaluate(string expression) =>
        InterpreterTestHelper.EvaluateInModulesToPineValue(expression, s_prepared.Value);

    private static void AssertEvaluatesEqual(string expression, string expectedExpression) =>
        Evaluate(expression).Should().Be(Evaluate(expectedExpression));

    // ============================================================
    // member — primitives
    // ============================================================

    [Theory]
    [InlineData("List.member 2 [ 1, 2, 3 ]", "True")]
    [InlineData("List.member 4 [ 1, 2, 3 ]", "False")]
    [InlineData("List.member 1 [ 1 ]", "True")]
    [InlineData("List.member 1 []", "False")]
    [InlineData("List.member \"b\" [ \"a\", \"b\", \"c\" ]", "True")]
    [InlineData("List.member \"z\" [ \"a\", \"b\", \"c\" ]", "False")]
    [InlineData("List.member 'c' [ 'a', 'b', 'c' ]", "True")]
    [InlineData("List.member 'd' [ 'a', 'b', 'c' ]", "False")]
    public void Member_matches_Elm_semantics_for_primitives(string expression, string expected) =>
        AssertEvaluatesEqual(expression, expected);

    // ============================================================
    // member — structural values use structural equality
    // ============================================================

    [Theory]
    [InlineData("List.member [ 1, 2 ] [ [ 1, 2 ], [ 3, 4 ] ]", "True")]
    [InlineData("List.member [ 1, 3 ] [ [ 1, 2 ], [ 3, 4 ] ]", "False")]
    [InlineData("List.member ( 1, \"a\" ) [ ( 1, \"a\" ), ( 2, \"b\" ) ]", "True")]
    [InlineData("List.member ( 1, \"b\" ) [ ( 1, \"a\" ), ( 2, \"b\" ) ]", "False")]
    public void Member_uses_structural_equality(string expression, string expected) =>
        AssertEvaluatesEqual(expression, expected);
}
