using AwesomeAssertions;
using System.Collections.Immutable;
using Xunit;

using ST = Pine.Core.Elm.ElmCompilerInDotnet.StructuralType;

namespace Pine.Core.Tests.Elm.ElmCompilerInDotnet.StructuralType;

/// <summary>
/// Tests for <see cref="ST.IsAssignableTo"/> — the "fits into" / subsumption
/// check that determines whether a specific type satisfies the constraints of a more general
/// type. This is the check used to select the right specialization when multiple specialized
/// forms of a function exist.
/// </summary>
public class StructuralTypeAssignabilityTests
{
    [Fact]
    public void Same_type_is_assignable_to_itself()
    {
        ST.IsAssignableTo(
            ST.IntType.Instance,
            ST.IntType.Instance)
            .Should().BeTrue();
    }

    [Fact]
    public void Int_is_assignable_to_number()
    {
        ST.IsAssignableTo(
            ST.IntType.Instance,
            new ST.ConstrainedVariable(0, ST.TypeConstraint.Number))
            .Should().BeTrue();
    }

    [Fact]
    public void Float_is_assignable_to_number()
    {
        ST.IsAssignableTo(
            ST.FloatType.Instance,
            new ST.ConstrainedVariable(0, ST.TypeConstraint.Number))
            .Should().BeTrue();
    }

    [Fact]
    public void String_is_not_assignable_to_number()
    {
        ST.IsAssignableTo(
            ST.StringType.Instance,
            new ST.ConstrainedVariable(0, ST.TypeConstraint.Number))
            .Should().BeFalse();
    }

    [Fact]
    public void Int_is_assignable_to_comparable()
    {
        ST.IsAssignableTo(
            ST.IntType.Instance,
            new ST.ConstrainedVariable(0, ST.TypeConstraint.Comparable))
            .Should().BeTrue();
    }

    [Fact]
    public void String_is_assignable_to_comparable()
    {
        ST.IsAssignableTo(
            ST.StringType.Instance,
            new ST.ConstrainedVariable(0, ST.TypeConstraint.Comparable))
            .Should().BeTrue();
    }

    [Fact]
    public void Char_is_assignable_to_comparable()
    {
        ST.IsAssignableTo(
            ST.CharType.Instance,
            new ST.ConstrainedVariable(0, ST.TypeConstraint.Comparable))
            .Should().BeTrue();
    }

    [Fact]
    public void List_of_Int_is_assignable_to_comparable()
    {
        ST.IsAssignableTo(
            new ST.ListType(ST.IntType.Instance),
            new ST.ConstrainedVariable(0, ST.TypeConstraint.Comparable))
            .Should().BeTrue();
    }

    [Fact]
    public void Tuple_of_comparables_is_assignable_to_comparable()
    {
        ST.IsAssignableTo(
            ST.TupleType.Create([ST.IntType.Instance, ST.StringType.Instance]),
            new ST.ConstrainedVariable(0, ST.TypeConstraint.Comparable))
            .Should().BeTrue();
    }

    [Fact]
    public void Bool_is_not_assignable_to_comparable()
    {
        ST.IsAssignableTo(
            ST.BoolType.Instance,
            new ST.ConstrainedVariable(0, ST.TypeConstraint.Comparable))
            .Should().BeFalse();
    }

    [Fact]
    public void Number_constraint_is_assignable_to_comparable()
    {
        ST.IsAssignableTo(
            new ST.ConstrainedVariable(0, ST.TypeConstraint.Number),
            new ST.ConstrainedVariable(1, ST.TypeConstraint.Comparable))
            .Should().BeTrue();
    }

    [Fact]
    public void String_is_assignable_to_appendable()
    {
        ST.IsAssignableTo(
            ST.StringType.Instance,
            new ST.ConstrainedVariable(0, ST.TypeConstraint.Appendable))
            .Should().BeTrue();
    }

    [Fact]
    public void List_is_assignable_to_appendable()
    {
        ST.IsAssignableTo(
            new ST.ListType(ST.IntType.Instance),
            new ST.ConstrainedVariable(0, ST.TypeConstraint.Appendable))
            .Should().BeTrue();
    }

    [Fact]
    public void Int_is_not_assignable_to_appendable()
    {
        ST.IsAssignableTo(
            ST.IntType.Instance,
            new ST.ConstrainedVariable(0, ST.TypeConstraint.Appendable))
            .Should().BeFalse();
    }

    [Fact]
    public void Any_type_is_assignable_to_type_variable()
    {
        ST.IsAssignableTo(
            ST.IntType.Instance,
            new ST.TypeVariable(0))
            .Should().BeTrue();

        ST.IsAssignableTo(
            new ST.ListType(ST.StringType.Instance),
            new ST.TypeVariable(0))
            .Should().BeTrue();
    }

    [Fact]
    public void Closed_record_is_assignable_to_open_record_with_subset_of_fields()
    {
        var closedRecord =
            ST.ClosedRecord.Create(
                ImmutableDictionary<string, ST>.Empty
                .Add("name", ST.StringType.Instance)
                .Add("age", ST.IntType.Instance)
                .Add("email", ST.StringType.Instance));

        var openRecord =
            new ST.OpenRecord(
                ImmutableDictionary<string, ST>.Empty
                .Add("name", ST.StringType.Instance),
                0);

        ST.IsAssignableTo(closedRecord, openRecord).Should().BeTrue();
    }

    [Fact]
    public void Closed_record_is_not_assignable_to_open_record_with_missing_field()
    {
        var closedRecord =
            ST.ClosedRecord.Create(
                ImmutableDictionary<string, ST>.Empty
                .Add("name", ST.StringType.Instance));

        var openRecord =
            new ST.OpenRecord(
                ImmutableDictionary<string, ST>.Empty
                .Add("name", ST.StringType.Instance)
                .Add("age", ST.IntType.Instance),
                0);

        ST.IsAssignableTo(closedRecord, openRecord).Should().BeFalse();
    }

    [Fact]
    public void Closed_record_is_not_assignable_to_open_record_with_mismatched_field_type()
    {
        var closedRecord =
            ST.ClosedRecord.Create(
                ImmutableDictionary<string, ST>.Empty
                .Add("name", ST.IntType.Instance));

        var openRecord =
            new ST.OpenRecord(
                ImmutableDictionary<string, ST>.Empty
                .Add("name", ST.StringType.Instance),
                0);

        ST.IsAssignableTo(closedRecord, openRecord).Should().BeFalse();
    }

    [Fact]
    public void Open_record_fits_into_open_record_with_subset_of_fields()
    {
        var specific =
            new ST.OpenRecord(
                ImmutableDictionary<string, ST>.Empty
                .Add("name", ST.StringType.Instance)
                .Add("age", ST.IntType.Instance),
                0);

        var general =
            new ST.OpenRecord(
                ImmutableDictionary<string, ST>.Empty
                .Add("name", ST.StringType.Instance),
                1);

        ST.IsAssignableTo(specific, general).Should().BeTrue();
    }

    [Fact]
    public void Function_type_assignability_is_contravariant_in_argument()
    {
        // A function accepting a type variable (more general argument) fits into
        // a function expecting a concrete argument, because the general function
        // can handle any input including the concrete one.
        var general =
            new ST.FunctionType(
                new ST.TypeVariable(0),
                ST.IntType.Instance);

        var specific =
            new ST.FunctionType(
                ST.IntType.Instance,
                ST.IntType.Instance);

        // general fits where specific is expected (contravariant in argument):
        // general accepts anything, specific expects Int — general can substitute for specific.
        ST.IsAssignableTo(general, specific).Should().BeTrue();
    }

    [Fact]
    public void List_of_Int_is_assignable_to_list_of_number()
    {
        ST.IsAssignableTo(
            new ST.ListType(ST.IntType.Instance),
            new ST.ListType(
                new ST.ConstrainedVariable(0, ST.TypeConstraint.Number)))
            .Should().BeTrue();
    }

    [Fact]
    public void Choice_type_with_subset_of_tags_is_assignable()
    {
        // A choice type with fewer tags fits into one with more tags
        // (a value that can only be Nil can be used where Nil|Cons is expected).
        var specific =
            ST.ChoiceType.Create(
                ImmutableDictionary<string, ImmutableList<ST>>.Empty
                .Add("Nil", []));

        var general =
            ST.ChoiceType.Create(
                ImmutableDictionary<string, ImmutableList<ST>>.Empty
                .Add("Cons", [ST.IntType.Instance, ST.Self.Instance])
                .Add("Nil", []));

        ST.IsAssignableTo(specific, general).Should().BeTrue();
    }

    [Fact]
    public void Choice_type_with_extra_tag_is_not_assignable()
    {
        var specific =
            ST.ChoiceType.Create(
                ImmutableDictionary<string, ImmutableList<ST>>.Empty
                .Add("Cons", [ST.IntType.Instance, ST.Self.Instance])
                .Add("Nil", [])
                .Add("Extra", []));

        var general =
            ST.ChoiceType.Create(
                ImmutableDictionary<string, ImmutableList<ST>>.Empty
                .Add("Cons", [ST.IntType.Instance, ST.Self.Instance])
                .Add("Nil", []));

        ST.IsAssignableTo(specific, general).Should().BeFalse();
    }

    [Fact]
    public void Int_is_not_assignable_to_String()
    {
        ST.IsAssignableTo(
            ST.IntType.Instance,
            ST.StringType.Instance)
            .Should().BeFalse();
    }
}
