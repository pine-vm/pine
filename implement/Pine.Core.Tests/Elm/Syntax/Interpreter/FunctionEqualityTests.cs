using AwesomeAssertions;
using Pine.Core.Internal;
using System;
using Xunit;

using ElmInterpreter = Pine.Core.Elm.Syntax.Interpreter.ElmSyntaxInterpreter;

namespace Pine.Core.Tests.Elm.Syntax.Interpreter;

public class FunctionEqualityTests
{
    private static readonly Lazy<ElmInterpreter.Prepared> s_prepared =
        new(
            () => InterpreterTestHelper.PrepareModulesFromSources(
                [
                    InterpreterTestHelper.LoadKernelModuleSource("Basics.elm"),
                    InterpreterTestHelper.LoadKernelModuleSource("List.elm"),
                    InterpreterTestHelper.LoadKernelModuleSource("Maybe.elm"),
                    InterpreterTestHelper.LoadKernelModuleSource("Char.elm"),
                    InterpreterTestHelper.LoadKernelModuleSource("Dict.elm"),
                    """
                    module Functions exposing (..)


                    identity value =
                        value


                    otherIdentity value =
                        value


                    first a b =
                        a


                    apply function argument =
                        function argument


                    makeClosure captured =
                        \argument -> ( captured, argument )
                    """,
                    """
                    module First exposing (..)


                    type OldChoice a b
                        = Empty
                        | Item a b
                        | Other a b


                    type alias Record a b =
                        { first : a, second : b }
                    """,
                    """
                    module Second exposing (..)


                    type NewChoice a b
                        = Empty
                        | Item a b
                    """,
                ]));

    private static void AssertEvaluatesTo(string expression, bool expected) =>
        InterpreterTestHelper.EvaluateInModulesToPineValue(expression, s_prepared.Value)
        .Should().Be(PineValueInProcess.CreateBool(expected).Evaluate(), expression);

    [Theory]
    [InlineData("Functions.identity", "Functions.identity", true)]
    [InlineData("Functions.identity", "Functions.otherIdentity", false)]
    [InlineData("Functions.first 1", "Functions.first 1", true)]
    [InlineData("Functions.first 1", "Functions.first 2", false)]
    [InlineData("Basics.add", "Basics.add", true)]
    [InlineData("Basics.add 1", "Basics.add 1", true)]
    [InlineData("Basics.add 1", "Basics.add 2", false)]
    [InlineData("Debug.toString", "Debug.toString", true)]
    [InlineData("Debug.log \"label\"", "Debug.log \"label\"", true)]
    [InlineData("Debug.log \"label\"", "Debug.log \"other\"", false)]
    [InlineData("Functions.apply Functions.identity", "Functions.apply Functions.identity", true)]
    [InlineData("Functions.apply Functions.identity", "Functions.apply Functions.otherIdentity", false)]
    [InlineData("Functions.apply (Functions.first 1)", "Functions.apply (Functions.first 1)", true)]
    [InlineData("Functions.apply (Functions.first 1)", "Functions.apply (Functions.first 2)", false)]
    [InlineData("Functions.apply (Functions.apply (Functions.first 1))", "Functions.apply (Functions.apply (Functions.first 1))", true)]
    [InlineData("Functions.apply (Functions.apply (Functions.first 1))", "Functions.apply (Functions.apply (Functions.first 2))", false)]
    [InlineData("Functions.first [ 1, 2 ]", "Functions.first [ 1, 2 ]", true)]
    [InlineData("Functions.first [ 1, 2 ]", "Functions.first [ 1, 3 ]", false)]
    [InlineData("Functions.first { nested = [ ( \"x\", Maybe.Just [ 1, 2 ] ) ] }", "Functions.first { nested = [ ( \"x\", Maybe.Just [ 1, 2 ] ) ] }", true)]
    [InlineData("Functions.first { nested = [ ( \"x\", Maybe.Just [ 1, 2 ] ) ] }", "Functions.first { nested = [ ( \"x\", Maybe.Just [ 1, 3 ] ) ] }", false)]
    [InlineData("Functions.first { nested = [ Maybe.Just (Functions.first 1) ] }", "Functions.first { nested = [ Maybe.Just (Functions.first 1) ] }", true)]
    [InlineData("Functions.first { nested = [ Maybe.Just (Functions.first 1) ] }", "Functions.first { nested = [ Maybe.Just (Functions.first 2) ] }", false)]
    [InlineData("Functions.makeClosure 1", "Functions.makeClosure 1", true)]
    [InlineData("Functions.makeClosure 1", "Functions.makeClosure 2", false)]
    [InlineData("[ Functions.first 1, Functions.first 2 ]", "[ Functions.first 1, Functions.first 2 ]", true)]
    [InlineData("[ Functions.first 1, Functions.first 2 ]", "[ Functions.first 1, Functions.first 3 ]", false)]
    [InlineData("{ function = Functions.first 1 }", "{ function = Functions.first 1 }", true)]
    [InlineData("{ function = Functions.first 1 }", "{ function = Functions.first 2 }", false)]
    [InlineData("Maybe.Just (Functions.first 1)", "Maybe.Just (Functions.first 1)", true)]
    [InlineData("Maybe.Just (Functions.first 1)", "Maybe.Just (Functions.first 2)", false)]
    [InlineData("First.Item", "First.Item", true)]
    [InlineData("First.Item 1", "First.Item 1", true)]
    [InlineData("First.Item 1", "First.Item 2", false)]
    [InlineData("First.Record", "First.Record", true)]
    [InlineData("First.Record 1", "First.Record 1", true)]
    [InlineData("First.Record 1", "First.Record 2", false)]
    [InlineData(".first", ".first", true)]
    [InlineData(".first", ".second", false)]
    public void Function_equality_supports_partial_applications_and_nested_values(
        string left,
        string right,
        bool expected)
    {
        AssertEvaluatesTo($"Basics.eq ({left}) ({right})", expected);
        AssertEvaluatesTo($"Basics.neq ({left}) ({right})", !expected);
        AssertEvaluatesTo($"({left}) == ({right})", expected);
        AssertEvaluatesTo($"({left}) /= ({right})", !expected);
        AssertEvaluatesTo($"Pine_builtin.equal [ ({left}), ({right}) ]", expected);
        AssertEvaluatesTo($"Pine_kernel.equal [ ({left}), ({right}) ]", expected);
    }

    [Theory]
    [InlineData("First.Empty", "Second.Empty", true)]
    [InlineData("First.Item 1 [ 2, 3 ]", "Second.Item 1 [ 2, 3 ]", true)]
    [InlineData("First.Item 1 [ 2, 3 ]", "Second.Item 1 [ 2, 4 ]", false)]
    [InlineData("First.Other 1 2", "Second.Item 1 2", false)]
    [InlineData("First.Item { nested = [ Second.Empty ] } (Functions.first 1)", "Second.Item { nested = [ First.Empty ] } (Functions.first 1)", true)]
    [InlineData("First.Item { nested = [ Second.Empty ] } (Functions.first 1)", "Second.Item { nested = [ First.Empty ] } (Functions.first 2)", false)]
    [InlineData("Functions.first (First.Item 1 2)", "Functions.first (Second.Item 1 2)", true)]
    [InlineData("Functions.first (First.Item 1 2)", "Functions.first (Second.Item 1 3)", false)]
    public void Choice_value_equality_is_structural_across_source_modules(
        string left,
        string right,
        bool expected)
    {
        AssertEvaluatesTo($"({left}) == ({right})", expected);
        AssertEvaluatesTo($"({left}) /= ({right})", !expected);
        AssertEvaluatesTo($"Pine_builtin.equal [ ({left}), ({right}) ]", expected);
    }

    [Theory]
    [InlineData("let function = \\value -> value in function == function", true)]
    [InlineData("List.member (Functions.first 1) [ Functions.first 2, Functions.first 1 ]", true)]
    [InlineData("List.member (Functions.first 3) [ Functions.first 2, Functions.first 1 ]", false)]
    [InlineData("Basics.eq [ ( Functions.identity, 1 ) ] [ ( Functions.identity, 1.0 ) ]", true)]
    [InlineData("Basics.eq [ ( Functions.identity, 1 ) ] [ ( Functions.identity, 2.0 ) ]", false)]
    [InlineData("Basics.eq [ Functions.identity ] [ 1 ]", false)]
    [InlineData("Basics.eq [ 1 ] [ Functions.identity ]", false)]
    [InlineData("Pine_builtin.equal []", true)]
    [InlineData("Pine_builtin.equal [ Functions.identity ]", true)]
    [InlineData("Pine_builtin.equal [ Functions.first 1, Functions.first 1, Functions.first 1 ]", true)]
    [InlineData("Pine_builtin.equal [ Functions.first 1, Functions.first 1, Functions.first 2 ]", false)]
    public void Function_equality_preserves_composed_operations(string expression, bool expected) =>
        AssertEvaluatesTo(expression, expected);

    [Theory]
    [InlineData("Functions.first 1", true)]
    [InlineData("Functions.first 2", false)]
    public void Function_equality_in_dicts_preserves_insertion_order_independence(string lastValue, bool expected) =>
        AssertEvaluatesTo(
            "Basics.eq " +
            "(Dict.fromList [ ( 1, Functions.first 1 ), ( 2, Functions.first 1 ), ( 3, Functions.first 1 ), ( 4, Functions.first 1 ), ( 5, Functions.first 1 ) ]) " +
            "(Dict.fromList [ ( 5, Functions.first 1 ), ( 3, Functions.first 1 ), ( 1, " +
            lastValue +
            " ), ( 4, Functions.first 1 ), ( 2, Functions.first 1 ) ])",
            expected);
}
