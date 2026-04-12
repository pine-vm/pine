using AwesomeAssertions;
using System.Collections.Generic;
using System.Collections.Immutable;
using Xunit;

using ST = Pine.Core.Elm.ElmCompilerInDotnet.StructuralType;

namespace Pine.Core.Tests.Elm.ElmCompilerInDotnet.StructuralType;

/// <summary>
/// Tests for <see cref="ST.Substitute"/> — type transformations such as
/// instantiating type variables (e.g., replacing a type variable with <c>Int</c> when
/// specializing a polymorphic function for a concrete call site).
/// </summary>
public class StructuralTypeTransformationTests
{
    [Fact]
    public void Substitute_type_variable_with_concrete_type()
    {
        var original = new ST.TypeVariable(0);

        var result =
            ST.Substitute(
                original,
                new Dictionary<int, ST> { [0] = ST.IntType.Instance });

        result.Should().Be(ST.IntType.Instance);
    }

    [Fact]
    public void Substitute_leaves_unmentioned_variables_unchanged()
    {
        var original = new ST.TypeVariable(1);

        var result =
            ST.Substitute(
                original,
                new Dictionary<int, ST> { [0] = ST.IntType.Instance });

        result.Should().Be(new ST.TypeVariable(1));
    }

    [Fact]
    public void Substitute_number_constraint_with_Int()
    {
        // add : number -> number -> number
        // specializing for Int: add : Int -> Int -> Int
        var original =
            new ST.FunctionType(
                new ST.ConstrainedVariable(0, ST.TypeConstraint.Number),
                new ST.FunctionType(
                    new ST.ConstrainedVariable(0, ST.TypeConstraint.Number),
                    new ST.ConstrainedVariable(0, ST.TypeConstraint.Number)));

        var result =
            ST.Substitute(
                original,
                new Dictionary<int, ST> { [0] = ST.IntType.Instance });

        var expected =
            new ST.FunctionType(
                ST.IntType.Instance,
                new ST.FunctionType(
                    ST.IntType.Instance,
                    ST.IntType.Instance));

        result.Should().Be(expected);
    }

    [Fact]
    public void Substitute_in_function_type_arguments_and_return()
    {
        // (a -> b) -> List a -> List b
        var original =
            new ST.FunctionType(
                new ST.FunctionType(
                    new ST.TypeVariable(0),
                    new ST.TypeVariable(1)),
                new ST.FunctionType(
                    new ST.ListType(new ST.TypeVariable(0)),
                    new ST.ListType(new ST.TypeVariable(1))));

        var result =
            ST.Substitute(
                original,
                new Dictionary<int, ST>
                {
                    [0] = ST.IntType.Instance,
                    [1] = ST.StringType.Instance,
                });

        var expected =
            new ST.FunctionType(
                new ST.FunctionType(
                    ST.IntType.Instance,
                    ST.StringType.Instance),
                new ST.FunctionType(
                    new ST.ListType(ST.IntType.Instance),
                    new ST.ListType(ST.StringType.Instance)));

        result.Should().Be(expected);
    }

    [Fact]
    public void Substitute_in_tuple_type()
    {
        var original =
            ST.TupleType.Create(
                [new ST.TypeVariable(0), new ST.TypeVariable(1)]);

        var result =
            ST.Substitute(
                original,
                new Dictionary<int, ST>
                {
                    [0] = ST.IntType.Instance,
                    [1] = ST.FloatType.Instance,
                });

        result.Should().Be(
            ST.TupleType.Create(
                [ST.IntType.Instance, ST.FloatType.Instance]));
    }

    [Fact]
    public void Substitute_in_closed_record()
    {
        var original =
            ST.ClosedRecord.Create(
                ImmutableDictionary<string, ST>.Empty
                .Add("value", new ST.TypeVariable(0))
                .Add("label", ST.StringType.Instance));

        var result =
            ST.Substitute(
                original,
                new Dictionary<int, ST> { [0] = ST.IntType.Instance });

        var expected =
            ST.ClosedRecord.Create(
                ImmutableDictionary<string, ST>.Empty
                .Add("value", ST.IntType.Instance)
                .Add("label", ST.StringType.Instance));

        result.Should().Be(expected);
    }

    [Fact]
    public void Substitute_in_open_record_fields()
    {
        // { a | name : b } with b -> String
        var original =
            new ST.OpenRecord(
                ImmutableDictionary<string, ST>.Empty
                .Add("name", new ST.TypeVariable(1)),
                0);

        var result =
            ST.Substitute(
                original,
                new Dictionary<int, ST> { [1] = ST.StringType.Instance });

        var expected =
            new ST.OpenRecord(
                ImmutableDictionary<string, ST>.Empty
                .Add("name", ST.StringType.Instance),
                0);

        result.Should().Be(expected);
    }

    [Fact]
    public void Substitute_row_variable_closes_open_record()
    {
        // { a | name : String } with a -> { age : Int }
        // Result: { name : String, age : Int } (closed)
        var original =
            new ST.OpenRecord(
                ImmutableDictionary<string, ST>.Empty
                .Add("name", ST.StringType.Instance),
                0);

        var rowReplacement =
            ST.ClosedRecord.Create(
                ImmutableDictionary<string, ST>.Empty
                .Add("age", ST.IntType.Instance));

        var result =
            ST.Substitute(
                original,
                new Dictionary<int, ST> { [0] = rowReplacement });

        var expected =
            ST.ClosedRecord.Create(
                ImmutableDictionary<string, ST>.Empty
                .Add("name", ST.StringType.Instance)
                .Add("age", ST.IntType.Instance));

        result.Should().Be(expected);
    }

    [Fact]
    public void Substitute_in_choice_type_tag_fields()
    {
        // Maybe a = Just a | Nothing
        // with a -> Int
        var original =
            ST.ChoiceType.Create(
                ImmutableDictionary<string, ImmutableList<ST>>.Empty
                .Add("Just", [new ST.TypeVariable(0)])
                .Add("Nothing", []));

        var result =
            ST.Substitute(
                original,
                new Dictionary<int, ST> { [0] = ST.IntType.Instance });

        var expected =
            ST.ChoiceType.Create(
                ImmutableDictionary<string, ImmutableList<ST>>.Empty
                .Add("Just", [ST.IntType.Instance])
                .Add("Nothing", []));

        result.Should().Be(expected);
    }

    [Fact]
    public void Substitute_preserves_Self_reference()
    {
        // List a = Cons a Self | Nil  with a -> Int
        var original =
            ST.ChoiceType.Create(
                ImmutableDictionary<string, ImmutableList<ST>>.Empty
                .Add("Cons", [new ST.TypeVariable(0), ST.Self.Instance])
                .Add("Nil", []));

        var result =
            ST.Substitute(
                original,
                new Dictionary<int, ST> { [0] = ST.IntType.Instance });

        var expected =
            ST.ChoiceType.Create(
                ImmutableDictionary<string, ImmutableList<ST>>.Empty
                .Add("Cons", [ST.IntType.Instance, ST.Self.Instance])
                .Add("Nil", []));

        result.Should().Be(expected);
    }

    [Fact]
    public void Substitute_leaves_primitives_unchanged()
    {
        var result =
            ST.Substitute(
                ST.IntType.Instance,
                new Dictionary<int, ST> { [0] = ST.StringType.Instance });

        result.Should().Be(ST.IntType.Instance);
    }

    [Fact]
    public void Substitute_comparable_with_Int()
    {
        // sort : List comparable -> List comparable
        // specializing for Int
        var original =
            new ST.FunctionType(
                new ST.ListType(
                    new ST.ConstrainedVariable(0, ST.TypeConstraint.Comparable)),
                new ST.ListType(
                    new ST.ConstrainedVariable(0, ST.TypeConstraint.Comparable)));

        var result =
            ST.Substitute(
                original,
                new Dictionary<int, ST> { [0] = ST.IntType.Instance });

        var expected =
            new ST.FunctionType(
                new ST.ListType(ST.IntType.Instance),
                new ST.ListType(ST.IntType.Instance));

        result.Should().Be(expected);
    }

    [Fact]
    public void Substitute_appendable_with_String()
    {
        // append : appendable -> appendable -> appendable
        // specializing for String
        var original =
            new ST.FunctionType(
                new ST.ConstrainedVariable(0, ST.TypeConstraint.Appendable),
                new ST.FunctionType(
                    new ST.ConstrainedVariable(0, ST.TypeConstraint.Appendable),
                    new ST.ConstrainedVariable(0, ST.TypeConstraint.Appendable)));

        var result =
            ST.Substitute(
                original,
                new Dictionary<int, ST> { [0] = ST.StringType.Instance });

        var expected =
            new ST.FunctionType(
                ST.StringType.Instance,
                new ST.FunctionType(
                    ST.StringType.Instance,
                    ST.StringType.Instance));

        result.Should().Be(expected);
    }
}
