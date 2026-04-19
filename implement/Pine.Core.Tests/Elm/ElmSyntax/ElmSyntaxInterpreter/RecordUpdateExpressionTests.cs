using AwesomeAssertions;
using Pine.Core.Elm;
using Xunit;

using ElmInterpreter = Pine.Core.Elm.ElmSyntax.ElmSyntaxInterpreter;

namespace Pine.Core.Tests.Elm.ElmSyntax.ElmSyntaxInterpreter;

/// <summary>
/// Covers evaluation of record-update expressions <c>{ record | field = value, ... }</c>:
/// updating one or several fields of an existing record while preserving any unmodified
/// fields. Both closed alias-typed records and open / extensible record types are exercised.
/// All tests drive the interpreter via
/// <see cref="ElmInterpreter.ParseAndInterpret(string, System.Collections.Generic.IReadOnlyDictionary{Core.CodeAnalysis.DeclQualifiedName, Core.Elm.ElmSyntax.SyntaxModel.Declaration})"/>.
/// </summary>
public class RecordUpdateExpressionTests
{
    [Fact]
    public void Update_single_field_of_let_bound_record()
    {
        InterpreterTestHelper.EvaluateOrCrashRendered(
            """
            let
                point =
                    { x = 10, y = 20 }
            in
            { point | x = 99 }
            """)
            .Should().Be("{ x = 99, y = 20 }");
    }

    [Fact]
    public void Update_other_field_preserves_first_field()
    {
        InterpreterTestHelper.EvaluateOrCrashRendered(
            """
            let
                point =
                    { x = 10, y = 20 }
            in
            { point | y = 77 }
            """)
            .Should().Be("{ x = 10, y = 77 }");
    }

    [Fact]
    public void Update_multiple_fields_in_single_expression()
    {
        InterpreterTestHelper.EvaluateOrCrashRendered(
            """
            let
                point =
                    { x = 10, y = 20, z = 30 }
            in
            { point | x = 1, z = 3 }
            """)
            .Should().Be("{ x = 1, y = 20, z = 3 }");
    }

    [Fact]
    public void Update_does_not_mutate_original_let_binding()
    {
        // The original record must remain unchanged after producing the update; we verify by
        // returning a tuple of the original and the updated value.
        InterpreterTestHelper.EvaluateOrCrashRendered(
            """
            let
                point =
                    { x = 1, y = 2 }

                updated =
                    { point | x = 100 }
            in
            ( point, updated )
            """)
            .Should().Be("[ { x = 1, y = 2 }, { x = 100, y = 2 } ]");
    }

    [Fact]
    public void Update_field_value_evaluates_arbitrary_expression()
    {
        // The right-hand side of a field update is a full expression; it must be evaluated
        // before being assigned. Use a kernel arithmetic operation to be sure evaluation
        // happens.
        InterpreterTestHelper.EvaluateOrCrashRendered(
            """
            let
                point =
                    { x = 10, y = 20 }
            in
            { point | x = Pine_builtin.int_add [ point.x, 5 ] }
            """)
            .Should().Be("{ x = 15, y = 20 }");
    }

    [Fact]
    public void Update_record_built_via_closed_alias_constructor()
    {
        var elmModuleText =
            """
            module Test exposing (..)


            type alias Point =
                { x : Int, y : Int }
            """;

        InterpreterTestHelper.EvaluateInModuleOrCrashRendered(
            """
            let
                p =
                    Point 1 2
            in
            { p | x = 42 }
            """,
            elmModuleText)
            .Should().Be("{ x = 42, y = 2 }");
    }

    [Fact]
    public void Update_top_level_record_value()
    {
        var elmModuleText =
            """
            module Test exposing (..)


            origin =
                { x = 0, y = 0 }
            """;

        InterpreterTestHelper.EvaluateInModuleOrCrashRendered("{ origin | x = 5 }", elmModuleText)
            .Should().Be("{ x = 5, y = 0 }");
    }

    [Fact]
    public void Update_via_function_with_open_record_type()
    {
        // Function accepts any record with at least an `x : Int` field and returns a record
        // of the same shape with `x` incremented.
        var elmModuleText =
            """
            module Test exposing (..)


            incrementX : { r | x : Int } -> { r | x : Int }
            incrementX record =
                { record | x = Pine_builtin.int_add [ record.x, 1 ] }
            """;

        InterpreterTestHelper.EvaluateInModuleOrCrashRendered(
            "incrementX { x = 10, y = 20 }",
            elmModuleText)
            .Should().Be("{ x = 11, y = 20 }");

        // Same function applied to a record that has additional fields beyond the constraint:
        // those fields must be preserved.
        InterpreterTestHelper.EvaluateInModuleOrCrashRendered(
            "incrementX { x = 1, y = 2, z = 3, label = \"hi\" }",
            elmModuleText)
            .Should().Be("{ label = \"hi\", x = 2, y = 2, z = 3 }");
    }

    [Fact]
    public void Update_value_stored_in_nested_field_via_let()
    {
        // Elm has no syntactic shortcut for nested updates; the canonical pattern is to
        // destructure with let bindings, update the inner record, then update the outer.
        InterpreterTestHelper.EvaluateOrCrashRendered(
            """
            let
                outer =
                    { inner = { value = 1 }, tag = "t" }

                newInner =
                    { value = 99 }
            in
            { outer | inner = newInner }
            """)
            .Should().Be("{ inner = { value = 99 }, tag = \"t\" }");
    }

    [Fact]
    public void Update_field_to_value_obtained_from_function_application()
    {
        // The expression on the right-hand side calls a user-defined function whose result is
        // then placed into the record field.
        var elmModuleText =
            """
            module Test exposing (..)


            double : Int -> Int
            double n =
                Pine_builtin.int_mul [ n, 2 ]
            """;

        InterpreterTestHelper.EvaluateInModuleOrCrashRendered(
            """
            let
                point =
                    { x = 5, y = 0 }
            in
            { point | y = double point.x }
            """,
            elmModuleText)
            .Should().Be("{ x = 5, y = 10 }");
    }

    [Fact]
    public void Update_field_with_record_valued_replacement_supports_chained_access()
    {
        InterpreterTestHelper.EvaluateOrCrash(
            """
            let
                outer =
                    { inner = { value = 0 } }
            in
            ({ outer | inner = { value = 7 } }).inner.value
            """)
            .Should().Be(ElmValue.Integer(7));
    }

    [Fact]
    public void Update_referencing_unknown_field_returns_error()
    {
        var result =
            InterpreterTestHelper.Evaluate(
                """
                let
                    point =
                        { x = 1, y = 2 }
                in
                { point | z = 3 }
                """);

        result.Unpack(
            fromErr: err =>
            {
                err.Message.Should().Contain("z");
                return 0;
            },
            fromOk: _ => throw new System.Exception("Expected an error result, got a value."));
    }
}
