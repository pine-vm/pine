using AwesomeAssertions;
using Pine.Core.Elm.ElmCompilerInDotnet;
using Xunit;

namespace Pine.Core.Tests.Elm.ElmCompilerInDotnet.ElmCompilerTests;

/// <summary>
/// Unit tests for <see cref="TypeInference.TryUnify"/>, covering the inferred-type model
/// extensions added in Phase 1: open record types, constrained type variables, and
/// error-returning unification.
/// </summary>
public class TypeInferenceUnificationTests
{
    private static InferredTypeOk Ok(TypeInference.InferredType expected) =>
        new(expected);

    private readonly record struct InferredTypeOk(TypeInference.InferredType Expected);

    private static void AssertOk(
        Result<string, TypeInference.InferredType> actual,
        TypeInference.InferredType expected)
    {
        actual.Should().BeOfType<Result<string, TypeInference.InferredType>.Ok>(
            "expected unification to succeed; got: " + actual);

        ((Result<string, TypeInference.InferredType>.Ok)actual).Value
            .Should().Be(expected);
    }

    private static void AssertErr(Result<string, TypeInference.InferredType> actual)
    {
        actual.Should().BeOfType<Result<string, TypeInference.InferredType>.Err>(
            "expected unification to fail; got: " + actual);
    }

    // ---------- Identity / Unknown ----------

    [Fact]
    public void Unify_identical_concrete_types_succeeds()
    {
        AssertOk(
            TypeInference.TryUnify(TypeInference.InferredType.Int(), TypeInference.InferredType.Int()),
            TypeInference.InferredType.Int());

        AssertOk(
            TypeInference.TryUnify(TypeInference.InferredType.String(), TypeInference.InferredType.String()),
            TypeInference.InferredType.String());
    }

    [Fact]
    public void Unify_unknown_with_concrete_yields_concrete()
    {
        var unknown = new TypeInference.InferredType.UnknownType();

        AssertOk(
            TypeInference.TryUnify(unknown, TypeInference.InferredType.Int()),
            TypeInference.InferredType.Int());

        AssertOk(
            TypeInference.TryUnify(TypeInference.InferredType.String(), unknown),
            TypeInference.InferredType.String());
    }

    // ---------- Numeric ----------

    [Fact]
    public void Unify_number_with_Int_yields_Int()
    {
        AssertOk(
            TypeInference.TryUnify(TypeInference.InferredType.Number(), TypeInference.InferredType.Int()),
            TypeInference.InferredType.Int());

        AssertOk(
            TypeInference.TryUnify(TypeInference.InferredType.Int(), TypeInference.InferredType.Number()),
            TypeInference.InferredType.Int());
    }

    [Fact]
    public void Unify_number_with_Float_yields_Float()
    {
        AssertOk(
            TypeInference.TryUnify(TypeInference.InferredType.Number(), TypeInference.InferredType.Float()),
            TypeInference.InferredType.Float());
    }

    [Fact]
    public void Unify_Int_with_Float_fails()
    {
        AssertErr(
            TypeInference.TryUnify(TypeInference.InferredType.Int(), TypeInference.InferredType.Float()));

        AssertErr(
            TypeInference.TryUnify(TypeInference.InferredType.Float(), TypeInference.InferredType.Int()));
    }

    [Fact]
    public void Unify_number_with_String_fails()
    {
        AssertErr(
            TypeInference.TryUnify(TypeInference.InferredType.Number(), TypeInference.InferredType.String()));
    }

    // ---------- Free type variables ----------

    [Fact]
    public void Unify_free_type_variable_with_concrete_yields_concrete()
    {
        var freeA = new TypeInference.InferredType.TypeVariable("a");

        AssertOk(
            TypeInference.TryUnify(freeA, TypeInference.InferredType.Int()),
            TypeInference.InferredType.Int());

        AssertOk(
            TypeInference.TryUnify(TypeInference.InferredType.String(), freeA),
            TypeInference.InferredType.String());
    }

    [Fact]
    public void Unify_two_free_type_variables_succeeds()
    {
        var a = new TypeInference.InferredType.TypeVariable("a");
        var b = new TypeInference.InferredType.TypeVariable("b");

        var result = TypeInference.TryUnify(a, b);

        result.Should().BeOfType<Result<string, TypeInference.InferredType>.Ok>();
    }

    // ---------- Constrained type variables: appendable ----------

    [Fact]
    public void Unify_appendable_with_String_yields_String()
    {
        var appendable = new TypeInference.InferredType.TypeVariable("a", TypeVariableConstraint.Appendable);

        AssertOk(
            TypeInference.TryUnify(appendable, TypeInference.InferredType.String()),
            TypeInference.InferredType.String());
    }

    [Fact]
    public void Unify_appendable_with_List_yields_List()
    {
        var appendable = new TypeInference.InferredType.TypeVariable("a", TypeVariableConstraint.Appendable);
        var listOfInt = new TypeInference.InferredType.ListType(TypeInference.InferredType.Int());

        AssertOk(
            TypeInference.TryUnify(appendable, listOfInt),
            listOfInt);
    }

    [Fact]
    public void Unify_appendable_with_Int_fails()
    {
        var appendable = new TypeInference.InferredType.TypeVariable("a", TypeVariableConstraint.Appendable);

        AssertErr(
            TypeInference.TryUnify(appendable, TypeInference.InferredType.Int()));
    }

    [Fact]
    public void Unify_appendable_with_Char_fails()
    {
        var appendable = new TypeInference.InferredType.TypeVariable("a", TypeVariableConstraint.Appendable);

        AssertErr(
            TypeInference.TryUnify(appendable, TypeInference.InferredType.Char()));
    }

    // ---------- Constrained type variables: comparable ----------

    [Fact]
    public void Unify_comparable_with_Int_yields_Int()
    {
        var comparable = new TypeInference.InferredType.TypeVariable("a", TypeVariableConstraint.Comparable);

        AssertOk(
            TypeInference.TryUnify(comparable, TypeInference.InferredType.Int()),
            TypeInference.InferredType.Int());
    }

    [Fact]
    public void Unify_comparable_with_String_yields_String()
    {
        var comparable = new TypeInference.InferredType.TypeVariable("a", TypeVariableConstraint.Comparable);

        AssertOk(
            TypeInference.TryUnify(comparable, TypeInference.InferredType.String()),
            TypeInference.InferredType.String());
    }

    [Fact]
    public void Unify_comparable_with_List_of_comparable_succeeds()
    {
        var comparable = new TypeInference.InferredType.TypeVariable("a", TypeVariableConstraint.Comparable);
        var listOfInt = new TypeInference.InferredType.ListType(TypeInference.InferredType.Int());

        AssertOk(
            TypeInference.TryUnify(comparable, listOfInt),
            listOfInt);
    }

    [Fact]
    public void Unify_comparable_with_Bool_fails()
    {
        var comparable = new TypeInference.InferredType.TypeVariable("a", TypeVariableConstraint.Comparable);

        AssertErr(
            TypeInference.TryUnify(comparable, TypeInference.InferredType.Bool()));
    }

    // ---------- Constrained type variables: number ----------

    [Fact]
    public void Unify_number_constrained_var_with_String_fails()
    {
        var numVar = new TypeInference.InferredType.TypeVariable("a", TypeVariableConstraint.Number);

        AssertErr(
            TypeInference.TryUnify(numVar, TypeInference.InferredType.String()));
    }

    [Fact]
    public void Unify_number_var_with_Int_yields_Int()
    {
        var numVar = new TypeInference.InferredType.TypeVariable("a", TypeVariableConstraint.Number);

        AssertOk(
            TypeInference.TryUnify(numVar, TypeInference.InferredType.Int()),
            TypeInference.InferredType.Int());
    }

    // ---------- Constrained type variables: compappend ----------

    [Fact]
    public void Unify_compappend_with_String_yields_String()
    {
        var ca = new TypeInference.InferredType.TypeVariable("a", TypeVariableConstraint.CompAppend);

        AssertOk(
            TypeInference.TryUnify(ca, TypeInference.InferredType.String()),
            TypeInference.InferredType.String());
    }

    [Fact]
    public void Unify_compappend_with_List_of_comparable_succeeds()
    {
        var ca = new TypeInference.InferredType.TypeVariable("a", TypeVariableConstraint.CompAppend);
        var listOfChar = new TypeInference.InferredType.ListType(TypeInference.InferredType.Char());

        AssertOk(
            TypeInference.TryUnify(ca, listOfChar),
            listOfChar);
    }

    [Fact]
    public void Unify_compappend_with_List_of_Bool_fails()
    {
        var ca = new TypeInference.InferredType.TypeVariable("a", TypeVariableConstraint.CompAppend);
        var listOfBool = new TypeInference.InferredType.ListType(TypeInference.InferredType.Bool());

        AssertErr(
            TypeInference.TryUnify(ca, listOfBool));
    }

    // ---------- Constraint combination between two variables ----------

    [Fact]
    public void Unify_appendable_var_with_comparable_var_yields_compappend_var()
    {
        var app = new TypeInference.InferredType.TypeVariable("a", TypeVariableConstraint.Appendable);
        var cmp = new TypeInference.InferredType.TypeVariable("b", TypeVariableConstraint.Comparable);

        var result = TypeInference.TryUnify(app, cmp);

        result.Should().BeOfType<Result<string, TypeInference.InferredType>.Ok>();
        var value = ((Result<string, TypeInference.InferredType>.Ok)result).Value;
        value.Should().BeOfType<TypeInference.InferredType.TypeVariable>();

        ((TypeInference.InferredType.TypeVariable)value).Constraint
            .Should().Be(TypeVariableConstraint.CompAppend);
    }

    [Fact]
    public void Unify_number_var_with_appendable_var_fails()
    {
        var num = new TypeInference.InferredType.TypeVariable("a", TypeVariableConstraint.Number);
        var app = new TypeInference.InferredType.TypeVariable("b", TypeVariableConstraint.Appendable);

        AssertErr(TypeInference.TryUnify(num, app));
    }

    [Fact]
    public void Unify_number_var_with_comparable_var_yields_number_var()
    {
        var num = new TypeInference.InferredType.TypeVariable("a", TypeVariableConstraint.Number);
        var cmp = new TypeInference.InferredType.TypeVariable("b", TypeVariableConstraint.Comparable);

        var result = TypeInference.TryUnify(num, cmp);

        result.Should().BeOfType<Result<string, TypeInference.InferredType>.Ok>();
        var value = ((Result<string, TypeInference.InferredType>.Ok)result).Value;
        value.Should().BeOfType<TypeInference.InferredType.TypeVariable>();

        ((TypeInference.InferredType.TypeVariable)value).Constraint
            .Should().Be(TypeVariableConstraint.Number);
    }

    // ---------- Closed records ----------

    [Fact]
    public void Unify_closed_records_with_same_fields_succeeds()
    {
        var r1 =
            new TypeInference.InferredType.RecordType(
                [
                ("name", TypeInference.InferredType.String()),
                ("age", TypeInference.InferredType.Int())
                ]);

        var r2 =
            new TypeInference.InferredType.RecordType(
                [
                ("name", TypeInference.InferredType.String()),
                ("age", TypeInference.InferredType.Int())
                ]);

        var result = TypeInference.TryUnify(r1, r2);

        result.Should().BeOfType<Result<string, TypeInference.InferredType>.Ok>();
    }

    [Fact]
    public void Unify_closed_records_with_different_field_sets_fails()
    {
        var r1 =
            new TypeInference.InferredType.RecordType(
                [("name", TypeInference.InferredType.String())]);

        var r2 =
            new TypeInference.InferredType.RecordType(
                [("age", TypeInference.InferredType.Int())]);

        AssertErr(TypeInference.TryUnify(r1, r2));
    }

    [Fact]
    public void Unify_closed_records_unifies_field_types()
    {
        // { x : number } unified with { x : Int } should give { x : Int }
        var r1 =
            new TypeInference.InferredType.RecordType(
                [("x", TypeInference.InferredType.Number())]);

        var r2 =
            new TypeInference.InferredType.RecordType(
                [("x", TypeInference.InferredType.Int())]);

        var expected =
            new TypeInference.InferredType.RecordType(
                [("x", TypeInference.InferredType.Int())]);

        AssertOk(TypeInference.TryUnify(r1, r2), expected);
    }

    [Fact]
    public void Unify_closed_records_with_conflicting_field_types_fails()
    {
        // { x : Int } unified with { x : String } should fail
        var r1 =
            new TypeInference.InferredType.RecordType(
                [("x", TypeInference.InferredType.Int())]);

        var r2 =
            new TypeInference.InferredType.RecordType(
                [("x", TypeInference.InferredType.String())]);

        AssertErr(TypeInference.TryUnify(r1, r2));
    }

    [Fact]
    public void Unify_closed_records_with_different_field_counts_fails()
    {
        var r1 =
            new TypeInference.InferredType.RecordType(
                [
                ("name", TypeInference.InferredType.String()),
                ("age", TypeInference.InferredType.Int())
                ]);

        var r2 =
            new TypeInference.InferredType.RecordType(
                [("name", TypeInference.InferredType.String())]);

        AssertErr(TypeInference.TryUnify(r1, r2));
    }

    // ---------- Open record vs closed record ----------

    [Fact]
    public void Unify_open_record_with_closed_having_all_fields_yields_closed()
    {
        // { r | name : String } unified with { name : String, age : Int } → { name : String, age : Int }
        var open =
            new TypeInference.InferredType.OpenRecordType(
                "r",
                [("name", TypeInference.InferredType.String())]);

        var closed =
            new TypeInference.InferredType.RecordType(
                [
                ("age", TypeInference.InferredType.Int()),
                ("name", TypeInference.InferredType.String())
                ]);

        AssertOk(TypeInference.TryUnify(open, closed), closed);
        AssertOk(TypeInference.TryUnify(closed, open), closed);
    }

    [Fact]
    public void Unify_open_record_with_closed_lacking_required_field_fails()
    {
        // { r | foo : Int } unified with { bar : Int } → fail
        var open =
            new TypeInference.InferredType.OpenRecordType(
                "r",
                [("foo", TypeInference.InferredType.Int())]);

        var closed =
            new TypeInference.InferredType.RecordType(
                [("bar", TypeInference.InferredType.Int())]);

        AssertErr(TypeInference.TryUnify(open, closed));
        AssertErr(TypeInference.TryUnify(closed, open));
    }

    [Fact]
    public void Unify_open_record_with_closed_having_conflicting_field_type_fails()
    {
        var open =
            new TypeInference.InferredType.OpenRecordType(
                "r",
                [("foo", TypeInference.InferredType.Int())]);

        var closed =
            new TypeInference.InferredType.RecordType(
                [("foo", TypeInference.InferredType.String())]);

        AssertErr(TypeInference.TryUnify(open, closed));
    }

    [Fact]
    public void Unify_open_record_with_closed_propagates_field_constraint()
    {
        // { r | x : number } unified with { x : Int, y : String } → { x : Int, y : String }
        var open =
            new TypeInference.InferredType.OpenRecordType(
                "r",
                [("x", TypeInference.InferredType.Number())]);

        var closed =
            new TypeInference.InferredType.RecordType(
                [
                ("x", TypeInference.InferredType.Int()),
                ("y", TypeInference.InferredType.String())
                ]);

        AssertOk(TypeInference.TryUnify(open, closed), closed);
    }

    // ---------- Open record vs open record ----------

    [Fact]
    public void Unify_two_open_records_merges_known_fields()
    {
        // { r | x : Int } unified with { s | y : String } → an open record carrying both fields
        var open1 =
            new TypeInference.InferredType.OpenRecordType(
                "r",
                [("x", TypeInference.InferredType.Int())]);

        var open2 =
            new TypeInference.InferredType.OpenRecordType(
                "s",
                [("y", TypeInference.InferredType.String())]);

        var result = TypeInference.TryUnify(open1, open2);

        result.Should().BeOfType<Result<string, TypeInference.InferredType>.Ok>();
        var value = ((Result<string, TypeInference.InferredType>.Ok)result).Value;
        value.Should().BeOfType<TypeInference.InferredType.OpenRecordType>();

        var merged = (TypeInference.InferredType.OpenRecordType)value;
        merged.KnownFields.Should().HaveCount(2);
        merged.KnownFields.Should().Contain(("x", (TypeInference.InferredType)TypeInference.InferredType.Int()));
        merged.KnownFields.Should().Contain(("y", (TypeInference.InferredType)TypeInference.InferredType.String()));
    }

    [Fact]
    public void Unify_two_open_records_unifies_shared_fields()
    {
        // { r | x : number } unified with { s | x : Int } → an open record with x : Int
        var open1 =
            new TypeInference.InferredType.OpenRecordType(
                "r",
                [("x", TypeInference.InferredType.Number())]);

        var open2 =
            new TypeInference.InferredType.OpenRecordType(
                "s",
                [("x", TypeInference.InferredType.Int())]);

        var result = TypeInference.TryUnify(open1, open2);

        result.Should().BeOfType<Result<string, TypeInference.InferredType>.Ok>();

        var merged =
            (TypeInference.InferredType.OpenRecordType)((Result<string, TypeInference.InferredType>.Ok)result).Value;

        merged.KnownFields.Should().Contain(("x", (TypeInference.InferredType)TypeInference.InferredType.Int()));
    }

    [Fact]
    public void Unify_two_open_records_with_conflicting_shared_field_fails()
    {
        var open1 =
            new TypeInference.InferredType.OpenRecordType(
                "r",
                [("x", TypeInference.InferredType.Int())]);

        var open2 =
            new TypeInference.InferredType.OpenRecordType(
                "s",
                [("x", TypeInference.InferredType.String())]);

        AssertErr(TypeInference.TryUnify(open1, open2));
    }

    // ---------- Compound types: function / list / tuple ----------

    [Fact]
    public void Unify_function_types_propagates_inner_unification()
    {
        // (Int -> number) unified with (number -> Float) should fail because
        // the return position would require unifying number with Float (ok: Float)
        // and the argument position would require unifying Int with number (ok: Int).
        var f1 =
            new TypeInference.InferredType.FunctionType(
                TypeInference.InferredType.Int(),
                TypeInference.InferredType.Number());

        var f2 =
            new TypeInference.InferredType.FunctionType(
                TypeInference.InferredType.Number(),
                TypeInference.InferredType.Float());

        var expected =
            new TypeInference.InferredType.FunctionType(
                TypeInference.InferredType.Int(),
                TypeInference.InferredType.Float());

        AssertOk(TypeInference.TryUnify(f1, f2), expected);
    }

    [Fact]
    public void Unify_function_types_with_conflicting_argument_fails()
    {
        var f1 =
            new TypeInference.InferredType.FunctionType(
                TypeInference.InferredType.Int(),
                TypeInference.InferredType.Bool());

        var f2 =
            new TypeInference.InferredType.FunctionType(
                TypeInference.InferredType.String(),
                TypeInference.InferredType.Bool());

        AssertErr(TypeInference.TryUnify(f1, f2));
    }

    [Fact]
    public void Unify_list_types_with_conflicting_element_fails()
    {
        var l1 = new TypeInference.InferredType.ListType(TypeInference.InferredType.Int());
        var l2 = new TypeInference.InferredType.ListType(TypeInference.InferredType.String());

        AssertErr(TypeInference.TryUnify(l1, l2));
    }

    [Fact]
    public void Unify_tuple_types_with_arity_mismatch_fails()
    {
        var t1 =
            new TypeInference.InferredType.TupleType(
                [TypeInference.InferredType.Int(), TypeInference.InferredType.Int()]);

        var t2 =
            new TypeInference.InferredType.TupleType(
                [TypeInference.InferredType.Int(), TypeInference.InferredType.Int(), TypeInference.InferredType.Int()]);

        AssertErr(TypeInference.TryUnify(t1, t2));
    }

    // ---------- Type model equality ----------

    [Fact]
    public void RecordType_with_same_fields_is_equal()
    {
        var r1 =
            new TypeInference.InferredType.RecordType(
                [
                ("a", TypeInference.InferredType.Int()),
                ("b", TypeInference.InferredType.String())
                ]);

        var r2 =
            new TypeInference.InferredType.RecordType(
                [
                ("a", TypeInference.InferredType.Int()),
                ("b", TypeInference.InferredType.String())
                ]);

        r1.Should().Be(r2);
        r1.GetHashCode().Should().Be(r2.GetHashCode());
    }

    [Fact]
    public void OpenRecordType_with_same_extension_and_fields_is_equal()
    {
        var r1 =
            new TypeInference.InferredType.OpenRecordType(
                "r",
                [("a", TypeInference.InferredType.Int())]);

        var r2 =
            new TypeInference.InferredType.OpenRecordType(
                "r",
                [("a", TypeInference.InferredType.Int())]);

        r1.Should().Be(r2);
        r1.GetHashCode().Should().Be(r2.GetHashCode());
    }

    [Fact]
    public void OpenRecordType_with_different_extension_is_not_equal()
    {
        var r1 =
            new TypeInference.InferredType.OpenRecordType(
                "r",
                [("a", TypeInference.InferredType.Int())]);

        var r2 =
            new TypeInference.InferredType.OpenRecordType(
                "s",
                [("a", TypeInference.InferredType.Int())]);

        r1.Should().NotBe(r2);
    }

    [Fact]
    public void TypeVariable_default_constraint_is_None()
    {
        var v = new TypeInference.InferredType.TypeVariable("a");
        v.Constraint.Should().Be(TypeVariableConstraint.None);
    }

    [Fact]
    public void TypeVariables_with_different_constraints_are_not_equal()
    {
        var free = new TypeInference.InferredType.TypeVariable("a");
        var num = new TypeInference.InferredType.TypeVariable("a", TypeVariableConstraint.Number);

        free.Should().NotBe(num);
    }
}
