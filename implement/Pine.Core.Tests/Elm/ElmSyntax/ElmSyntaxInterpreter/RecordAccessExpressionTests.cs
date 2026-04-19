using AwesomeAssertions;
using Pine.Core.Elm;
using Xunit;

using ElmInterpreter = Pine.Core.Elm.ElmSyntax.ElmSyntaxInterpreter;

namespace Pine.Core.Tests.Elm.ElmSyntax.ElmSyntaxInterpreter;

/// <summary>
/// Covers evaluation of record-access expressions (<c>record.field</c>), including chains of
/// two and three nested accesses, and the use of accesses on records described by both closed
/// type aliases (<c>{ x : Int }</c>) and open / extensible record types (<c>{ r | x : Int }</c>).
/// All scenarios drive the interpreter via
/// <see cref="ElmInterpreter.ParseAndInterpret(string, System.Collections.Generic.IReadOnlyDictionary{Core.CodeAnalysis.DeclQualifiedName, Core.Elm.ElmSyntax.SyntaxModel.Declaration})"/>.
/// </summary>
public class RecordAccessExpressionTests
{
    [Fact]
    public void Access_single_field_from_let_bound_record()
    {
        InterpreterTestHelper.EvaluateOrCrash(
            """
            let
                point =
                    { x = 10, y = 20 }
            in
            point.x
            """)
            .Should().Be(ElmValue.Integer(10));
    }

    [Fact]
    public void Access_other_field_from_let_bound_record()
    {
        InterpreterTestHelper.EvaluateOrCrash(
            """
            let
                point =
                    { x = 10, y = 20 }
            in
            point.y
            """)
            .Should().Be(ElmValue.Integer(20));
    }

    [Fact]
    public void Access_field_on_record_literal_directly()
    {
        InterpreterTestHelper.EvaluateOrCrash("({ x = 10, y = 20 }).x")
            .Should().Be(ElmValue.Integer(10));
    }

    [Fact]
    public void Access_field_from_top_level_value()
    {
        var elmModuleText =
            """
            module Test exposing (..)


            origin =
                { x = 1, y = 2 }
            """;

        InterpreterTestHelper.EvaluateInModuleOrCrash("origin.y", elmModuleText)
            .Should().Be(ElmValue.Integer(2));
    }

    [Fact]
    public void Access_field_from_record_built_via_closed_alias_constructor()
    {
        // Constructor application puts the arguments into alias-declaration order; the
        // resulting record's fields are sorted alphabetically. Field access must still work
        // the same way regardless of the original parameter order.
        var elmModuleText =
            """
            module Test exposing (..)


            type alias Point =
                { x : Int, y : Int }
            """;

        InterpreterTestHelper.EvaluateInModuleOrCrash("(Point 10 20).x", elmModuleText)
            .Should().Be(ElmValue.Integer(10));
    }

    [Fact]
    public void Access_field_from_record_built_via_alias_with_reverse_order()
    {
        var elmModuleText =
            """
            module Test exposing (..)


            type alias Point =
                { y : Int, x : Int }
            """;

        // Point 10 20 binds y = 10, x = 20.
        InterpreterTestHelper.EvaluateInModuleOrCrash("(Point 10 20).x", elmModuleText)
            .Should().Be(ElmValue.Integer(20));

        InterpreterTestHelper.EvaluateInModuleOrCrash("(Point 10 20).y", elmModuleText)
            .Should().Be(ElmValue.Integer(10));
    }

    [Fact]
    public void Access_field_through_function_parameter_open_record_type()
    {
        // Open record type `{ r | x : Int }`: the function accepts any record that has at least
        // an `x` field, regardless of additional fields.
        var elmModuleText =
            """
            module Test exposing (..)


            getX : { r | x : Int } -> Int
            getX r =
                r.x
            """;

        InterpreterTestHelper.EvaluateInModuleOrCrash("getX { x = 7, y = 99, z = 100 }", elmModuleText)
            .Should().Be(ElmValue.Integer(7));

        InterpreterTestHelper.EvaluateInModuleOrCrash("getX { x = 42 }", elmModuleText)
            .Should().Be(ElmValue.Integer(42));
    }

    [Fact]
    public void Chain_of_two_record_accesses_closed_records()
    {
        InterpreterTestHelper.EvaluateOrCrash(
            """
            let
                outer =
                    { inner = { value = 123 } }
            in
            outer.inner.value
            """)
            .Should().Be(ElmValue.Integer(123));
    }

    [Fact]
    public void Chain_of_two_record_accesses_via_alias_constructors()
    {
        var elmModuleText =
            """
            module Test exposing (..)


            type alias Inner =
                { value : Int }


            type alias Outer =
                { inner : Inner }
            """;

        InterpreterTestHelper.EvaluateInModuleOrCrash("(Outer (Inner 7)).inner.value", elmModuleText)
            .Should().Be(ElmValue.Integer(7));
    }

    [Fact]
    public void Chain_of_three_record_accesses_closed_records()
    {
        InterpreterTestHelper.EvaluateOrCrash(
            """
            let
                root =
                    { level1 = { level2 = { level3 = 9 } } }
            in
            root.level1.level2.level3
            """)
            .Should().Be(ElmValue.Integer(9));
    }

    [Fact]
    public void Chain_of_three_record_accesses_via_alias_constructors()
    {
        var elmModuleText =
            """
            module Test exposing (..)


            type alias Level3 =
                { value : Int }


            type alias Level2 =
                { level3 : Level3 }


            type alias Level1 =
                { level2 : Level2 }


            type alias Root =
                { level1 : Level1 }
            """;

        InterpreterTestHelper.EvaluateInModuleOrCrash(
            "(Root (Level1 (Level2 (Level3 99)))).level1.level2.level3.value",
            elmModuleText)
            .Should().Be(ElmValue.Integer(99));
    }

    [Fact]
    public void Chain_of_three_record_accesses_through_open_record_parameter()
    {
        // Open record types at every level: each function only constrains the field it
        // accesses, so callers may pass records with additional fields.
        var elmModuleText =
            """
            module Test exposing (..)


            deepValue : { r | level1 : { s | level2 : { t | level3 : Int } } } -> Int
            deepValue r =
                r.level1.level2.level3
            """;

        InterpreterTestHelper.EvaluateInModuleOrCrash(
            """
            deepValue { level1 = { level2 = { level3 = 17, extra = 0 }, info = "x" }, tag = 1 }
            """,
            elmModuleText)
            .Should().Be(ElmValue.Integer(17));
    }

    [Fact]
    public void Access_field_from_function_returning_record()
    {
        // `(makePoint 5 9).y` exercises record-access on an arbitrary expression in the
        // record position: the parser produces a RecordAccess whose Record is the
        // parenthesised application of `makePoint`.
        var elmModuleText =
            """
            module Test exposing (..)


            makePoint : Int -> Int -> { x : Int, y : Int }
            makePoint x y =
                { x = x, y = y }
            """;

        InterpreterTestHelper.EvaluateInModuleOrCrash("(makePoint 5 9).y", elmModuleText)
            .Should().Be(ElmValue.Integer(9));
    }

    [Fact]
    public void Access_field_from_if_expression_yielding_record()
    {
        InterpreterTestHelper.EvaluateOrCrash(
            """
            let
                pickedRecord =
                    if Pine_kernel.equal [ 1, 1 ] then
                        { value = 100 }

                    else
                        { value = 0 }
            in
            pickedRecord.value
            """)
            .Should().Be(ElmValue.Integer(100));
    }

    [Fact]
    public void Access_record_field_holding_string_value()
    {
        InterpreterTestHelper.EvaluateOrCrash(
            """
            let
                user =
                    { name = "Alice", age = 30 }
            in
            user.name
            """)
            .Should().Be(ElmValue.StringInstance("Alice"));
    }

    [Fact]
    public void Access_field_used_in_arithmetic_expression()
    {
        InterpreterTestHelper.EvaluateOrCrash(
            """
            let
                point =
                    { x = 4, y = 6 }
            in
            Pine_builtin.int_add [ point.x, point.y ]
            """)
            .Should().Be(ElmValue.Integer(10));
    }
}
