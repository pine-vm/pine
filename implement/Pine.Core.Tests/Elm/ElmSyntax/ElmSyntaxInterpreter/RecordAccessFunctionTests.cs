using AwesomeAssertions;
using Pine.Core.Elm;
using Xunit;

namespace Pine.Core.Tests.Elm.ElmSyntax.ElmSyntaxInterpreter;

/// <summary>
/// Covers evaluation of record-access function literals (<c>.field</c>): a one-argument function
/// that, given a record, returns the named field. The tests apply such literals to records
/// directly, partially apply them, pass them as higher-order arguments, and chain them in let
/// bindings to model two- and three-deep nested record accesses. Both closed type-alias records
/// and open / extensible record types (<c>{ r | x : Int }</c>) are exercised.
/// </summary>
public class RecordAccessFunctionTests
{
    [Fact]
    public void Apply_record_access_function_directly_to_record_literal()
    {
        InterpreterTestHelper.EvaluateOrCrash(".x { x = 11, y = 22 }")
            .Should().Be(ElmValue.Integer(11));
    }

    [Fact]
    public void Apply_record_access_function_to_let_bound_record()
    {
        InterpreterTestHelper.EvaluateOrCrash(
            """
            let
                point =
                    { x = 11, y = 22 }
            in
            .y point
            """)
            .Should().Be(ElmValue.Integer(22));
    }

    [Fact]
    public void Bind_record_access_function_in_let_then_apply()
    {
        InterpreterTestHelper.EvaluateOrCrash(
            """
            let
                getX =
                    .x

                point =
                    { x = 5, y = 9 }
            in
            getX point
            """)
            .Should().Be(ElmValue.Integer(5));
    }

    [Fact]
    public void Record_access_function_works_on_alias_constructor_result_closed()
    {
        var elmModuleText =
            """
            module Test exposing (..)


            type alias Point =
                { x : Int, y : Int }
            """;

        InterpreterTestHelper.EvaluateInModuleOrCrash(".y (Point 10 20)", elmModuleText)
            .Should().Be(ElmValue.Integer(20));
    }

    [Fact]
    public void Record_access_function_works_with_open_record_parameter()
    {
        // The function `getField .x` accepts any record with at least an `x` field; the
        // helper uses `.x` as a higher-order accessor parameter.
        var elmModuleText =
            """
            module Test exposing (..)


            getField : ({ r | x : Int } -> Int) -> { r | x : Int } -> Int
            getField accessor record =
                accessor record
            """;

        InterpreterTestHelper.EvaluateInModuleOrCrash("getField .x { x = 7, y = 8 }", elmModuleText)
            .Should().Be(ElmValue.Integer(7));

        InterpreterTestHelper.EvaluateInModuleOrCrash("getField .x { x = 99 }", elmModuleText)
            .Should().Be(ElmValue.Integer(99));
    }

    [Fact]
    public void Record_access_function_chain_of_two_via_composition_in_let()
    {
        // The expression `.inner record |> .value` simulates a chain of two accesses using
        // record-access function literals rather than dotted access syntax.
        InterpreterTestHelper.EvaluateOrCrash(
            """
            let
                outer =
                    { inner = { value = 42 } }
            in
            .value (.inner outer)
            """)
            .Should().Be(ElmValue.Integer(42));
    }

    [Fact]
    public void Record_access_function_chain_of_three_via_nested_application()
    {
        InterpreterTestHelper.EvaluateOrCrash(
            """
            let
                root =
                    { level1 = { level2 = { level3 = 7 } } }
            in
            .level3 (.level2 (.level1 root))
            """)
            .Should().Be(ElmValue.Integer(7));
    }

    [Fact]
    public void Record_access_function_chain_of_three_via_alias_constructors()
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
            ".value (.level3 (.level2 (.level1 (Root (Level1 (Level2 (Level3 88)))))))",
            elmModuleText)
            .Should().Be(ElmValue.Integer(88));
    }

    [Fact]
    public void Record_access_function_chain_of_three_with_open_record_types()
    {
        var elmModuleText =
            """
            module Test exposing (..)


            deepGet : { r | level1 : { s | level2 : { t | level3 : Int } } } -> Int
            deepGet record =
                .level3 (.level2 (.level1 record))
            """;

        InterpreterTestHelper.EvaluateInModuleOrCrash(
            """
            deepGet { level1 = { level2 = { level3 = 21, extra = 0 }, tag = "a" }, n = 1 }
            """,
            elmModuleText)
            .Should().Be(ElmValue.Integer(21));
    }

    [Fact]
    public void Record_access_function_passed_to_higher_order_helper()
    {
        // A small home-grown List.map equivalent over a 2-tuple of records: applies the
        // accessor function to each record and returns a list of field values. Avoids any
        // dependency on the standard library's List module.
        var elmModuleText =
            """
            module Test exposing (..)


            mapPair : (a -> b) -> ( a, a ) -> ( b, b )
            mapPair f pair =
                case pair of
                    ( first, second ) ->
                        ( f first, f second )
            """;

        // Use ParseAndInterpret to evaluate `mapPair .x ( { x = 1, y = 2 }, { x = 3, y = 4 } )`
        // and inspect the resulting tuple.
        InterpreterTestHelper.EvaluateInModuleOrCrashRendered(
            "mapPair .x ( { x = 1, y = 2 }, { x = 3, y = 4 } )",
            elmModuleText)
            .Should().Be("[ 1, 3 ]");
    }

    [Fact]
    public void Record_access_function_evaluates_to_function_value()
    {
        // Without applying the accessor it should still evaluate to a function value (an
        // ElmFunction). We assert by binding it to a name and then applying it.
        InterpreterTestHelper.EvaluateOrCrash(
            """
            let
                accessor =
                    .name

                user =
                    { name = "Bob", age = 21 }
            in
            accessor user
            """)
            .Should().Be(ElmValue.StringInstance("Bob"));
    }

    [Fact]
    public void Record_access_function_works_with_field_holding_record_for_chained_access()
    {
        // Returning a nested record from the first accessor, then applying another accessor
        // on the result, is functionally equivalent to a dotted chain.
        InterpreterTestHelper.EvaluateOrCrash(
            """
            let
                wrapper =
                    { payload = { count = 11 } }
            in
            (.payload wrapper).count
            """)
            .Should().Be(ElmValue.Integer(11));
    }
}
