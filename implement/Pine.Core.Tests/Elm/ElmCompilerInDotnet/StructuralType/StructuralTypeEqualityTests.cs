using AwesomeAssertions;
using System.Collections.Immutable;
using Xunit;

using ST = Pine.Core.Elm.ElmCompilerInDotnet.StructuralType;

namespace Pine.Core.Tests.Elm.ElmCompilerInDotnet.StructuralType;

/// <summary>
/// Tests verifying that <see cref="StructuralType"/> variants implement value equality
/// correctly, including hash code consistency and collection-valued fields.
/// </summary>
public class StructuralTypeEqualityTests
{
    [Fact]
    public void Primitive_singleton_instances_are_equal()
    {
        ST.IntType.Instance.Should().Be(ST.IntType.Instance);
        ST.FloatType.Instance.Should().Be(ST.FloatType.Instance);
        ST.StringType.Instance.Should().Be(ST.StringType.Instance);
        ST.CharType.Instance.Should().Be(ST.CharType.Instance);
        ST.BoolType.Instance.Should().Be(ST.BoolType.Instance);
    }

    [Fact]
    public void Distinct_primitive_types_are_not_equal()
    {
        ST intType = ST.IntType.Instance;
        ST floatType = ST.FloatType.Instance;

        intType.Should().NotBe(floatType);
    }

    [Fact]
    public void Type_variables_with_same_name_are_equal()
    {
        var a1 = new ST.TypeVariable(0);
        var a2 = new ST.TypeVariable(0);

        a1.Should().Be(a2);
        a1.GetHashCode().Should().Be(a2.GetHashCode());
    }

    [Fact]
    public void Type_variables_with_different_names_are_not_equal()
    {
        var a = new ST.TypeVariable(0);
        var b = new ST.TypeVariable(1);

        a.Should().NotBe(b);
    }

    [Fact]
    public void Constrained_variables_with_same_name_and_constraint_are_equal()
    {
        var n1 = new ST.ConstrainedVariable(0, ST.TypeConstraint.Number);
        var n2 = new ST.ConstrainedVariable(0, ST.TypeConstraint.Number);

        n1.Should().Be(n2);
        n1.GetHashCode().Should().Be(n2.GetHashCode());
    }

    [Fact]
    public void Constrained_variables_with_different_constraints_are_not_equal()
    {
        var number = new ST.ConstrainedVariable(0, ST.TypeConstraint.Number);
        var comparable = new ST.ConstrainedVariable(0, ST.TypeConstraint.Comparable);

        number.Should().NotBe(comparable);
    }

    [Fact]
    public void Function_types_with_same_structure_are_equal()
    {
        var f1 =
            new ST.FunctionType(
                ST.IntType.Instance,
                ST.StringType.Instance);

        var f2 =
            new ST.FunctionType(
                ST.IntType.Instance,
                ST.StringType.Instance);

        f1.Should().Be(f2);
        f1.GetHashCode().Should().Be(f2.GetHashCode());
    }

    [Fact]
    public void Function_types_with_different_structure_are_not_equal()
    {
        var f1 =
            new ST.FunctionType(
                ST.IntType.Instance,
                ST.StringType.Instance);

        var f2 =
            new ST.FunctionType(
                ST.FloatType.Instance,
                ST.StringType.Instance);

        f1.Should().NotBe(f2);
    }

    [Fact]
    public void List_types_with_same_element_are_equal()
    {
        var l1 = new ST.ListType(ST.IntType.Instance);
        var l2 = new ST.ListType(ST.IntType.Instance);

        l1.Should().Be(l2);
        l1.GetHashCode().Should().Be(l2.GetHashCode());
    }

    [Fact]
    public void Tuple_types_with_same_elements_are_equal()
    {
        var t1 =
            ST.TupleType.Create(
                [ST.IntType.Instance, ST.StringType.Instance]);

        var t2 =
            ST.TupleType.Create(
                [ST.IntType.Instance, ST.StringType.Instance]);

        t1.Should().Be(t2);
        t1.GetHashCode().Should().Be(t2.GetHashCode());
    }

    [Fact]
    public void Tuple_types_with_different_elements_are_not_equal()
    {
        var t1 =
            ST.TupleType.Create(
                [ST.IntType.Instance, ST.StringType.Instance]);

        var t2 =
            ST.TupleType.Create(
                [ST.FloatType.Instance, ST.StringType.Instance]);

        t1.Should().NotBe(t2);
    }

    [Fact]
    public void Closed_records_with_same_fields_are_equal()
    {
        var r1 =
            ST.ClosedRecord.Create(
                ImmutableDictionary<string, ST>.Empty
                .Add("name", ST.StringType.Instance)
                .Add("age", ST.IntType.Instance));

        var r2 =
            ST.ClosedRecord.Create(
                ImmutableDictionary<string, ST>.Empty
                .Add("age", ST.IntType.Instance)
                .Add("name", ST.StringType.Instance));

        r1.Should().Be(r2);
        r1.GetHashCode().Should().Be(r2.GetHashCode());
    }

    [Fact]
    public void Closed_records_with_different_fields_are_not_equal()
    {
        var r1 =
            ST.ClosedRecord.Create(
                ImmutableDictionary<string, ST>.Empty
                .Add("name", ST.StringType.Instance));

        var r2 =
            ST.ClosedRecord.Create(
                ImmutableDictionary<string, ST>.Empty
                .Add("title", ST.StringType.Instance));

        r1.Should().NotBe(r2);
    }

    [Fact]
    public void Open_records_with_same_fields_and_row_variable_are_equal()
    {
        var r1 =
            new ST.OpenRecord(
                ImmutableDictionary<string, ST>.Empty
                .Add("name", ST.StringType.Instance),
                0);

        var r2 =
            new ST.OpenRecord(
                ImmutableDictionary<string, ST>.Empty
                .Add("name", ST.StringType.Instance),
                0);

        r1.Should().Be(r2);
        r1.GetHashCode().Should().Be(r2.GetHashCode());
    }

    [Fact]
    public void Open_records_with_different_row_variables_are_not_equal()
    {
        var r1 =
            new ST.OpenRecord(
                ImmutableDictionary<string, ST>.Empty
                .Add("name", ST.StringType.Instance),
                0);

        var r2 =
            new ST.OpenRecord(
                ImmutableDictionary<string, ST>.Empty
                .Add("name", ST.StringType.Instance),
                1);

        r1.Should().NotBe(r2);
    }

    [Fact]
    public void Choice_types_with_same_tags_are_equal()
    {
        var c1 =
            ST.ChoiceType.Create(
                ImmutableDictionary<string, ImmutableList<ST>>.Empty
                .Add("Just", [ST.IntType.Instance])
                .Add("Nothing", []));

        var c2 =
            ST.ChoiceType.Create(
                ImmutableDictionary<string, ImmutableList<ST>>.Empty
                .Add("Nothing", [])
                .Add("Just", [ST.IntType.Instance]));

        c1.Should().Be(c2);
        c1.GetHashCode().Should().Be(c2.GetHashCode());
    }

    [Fact]
    public void Choice_types_with_different_tags_are_not_equal()
    {
        var c1 =
            ST.ChoiceType.Create(
                ImmutableDictionary<string, ImmutableList<ST>>.Empty
                .Add("Just", [ST.IntType.Instance])
                .Add("Nothing", []));

        var c2 =
            ST.ChoiceType.Create(
                ImmutableDictionary<string, ImmutableList<ST>>.Empty
                .Add("Ok", [ST.IntType.Instance])
                .Add("Err", [ST.StringType.Instance]));

        c1.Should().NotBe(c2);
    }

    [Fact]
    public void Self_instances_are_equal()
    {
        ST.Self.Instance.Should().Be(ST.Self.Instance);
        ST.Self.Instance.GetHashCode().Should().Be(ST.Self.Instance.GetHashCode());
    }

    [Fact]
    public void Recursive_list_type_with_self_is_equal_to_identical_construction()
    {
        // List Int = Cons Int (List Int) | Nil
        // Structurally: ChoiceType([("Cons", [IntType, Self]), ("Nil", [])])
        var list1 =
            ST.ChoiceType.Create(
                ImmutableDictionary<string, ImmutableList<ST>>.Empty
                .Add("Cons", [ST.IntType.Instance, ST.Self.Instance])
                .Add("Nil", []));

        var list2 =
            ST.ChoiceType.Create(
                ImmutableDictionary<string, ImmutableList<ST>>.Empty
                .Add("Cons", [ST.IntType.Instance, ST.Self.Instance])
                .Add("Nil", []));

        list1.Should().Be(list2);
        list1.GetHashCode().Should().Be(list2.GetHashCode());
    }

    [Fact]
    public void Nested_function_types_are_equal()
    {
        // (a -> b) -> List a -> List b  (like List.map)
        var a = new ST.TypeVariable(0);
        var b = new ST.TypeVariable(1);

        var f1 =
            new ST.FunctionType(
                new ST.FunctionType(a, b),
                new ST.FunctionType(
                    new ST.ListType(a),
                    new ST.ListType(b)));

        var f2 =
            new ST.FunctionType(
                new ST.FunctionType(
                    new ST.TypeVariable(0),
                    new ST.TypeVariable(1)),
                new ST.FunctionType(
                    new ST.ListType(new ST.TypeVariable(0)),
                    new ST.ListType(new ST.TypeVariable(1))));

        f1.Should().Be(f2);
        f1.GetHashCode().Should().Be(f2.GetHashCode());
    }
}
