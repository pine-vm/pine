using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.CommonEncodings;
using Pine.Core.Elm;
using Pine.Core.Internal;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using Xunit;

using AbstractExpr = Pine.Core.Elm.ElmSyntax.ElmSyntaxAbstract.Expression;
using ElmInterpreter = Pine.Core.Elm.ElmSyntax.ElmSyntaxInterpreter;

namespace Pine.Core.Tests.Elm.ElmSyntax.ElmSyntaxInterpreter;

/// <summary>
/// Focused regression tests for <see cref="ElmInterpreter.ValuesEqualInProcess"/>, the structural
/// equality used by the infinite-recursion detector and the call-stack frames. The function compares
/// <see cref="PineValueInProcess"/> instances directly — including the interpreter's opaque variants
/// (closures, boxed values, partially-applied constructors and record-access chains) — without
/// projecting either operand to an <see cref="ElmValue"/>.
/// </summary>
public class ValuesEqualInProcessTests
{
    private static bool Eq(PineValueInProcess left, PineValueInProcess right) =>
        ElmInterpreter.ValuesEqualInProcess(left, right);

    private static PineValueInProcess Int(int value) =>
        PineValueInProcess.CreateInteger(value);

    private static PineValueInProcess Str(string value) =>
        ElmInterpreter.ToProcess(ElmValue.StringInstance(value));

    private static PineValueInProcess ListOf(params PineValueInProcess[] items) =>
        PineValueInProcess.CreateList(items);

    private static PineValueInProcess TagNameValue(string tagName) =>
        PineValueInProcess.Create(PopularEncodings.StringEncoding.ValueFromString(tagName));

    private static DeclQualifiedName Name(string declName) =>
        DeclQualifiedName.Create([], declName);

    private static ElmInterpreter.ElmClosureInProcess.SourceRef LambdaSource() =>
        new ElmInterpreter.ElmClosureInProcess.SourceRef.Lambda(
            new AbstractExpr.LambdaExpression(
                [],
                AbstractExpr.UnitExpr.Instance));

    private static ElmInterpreter.ElmClosureInProcess.SourceRef LambdaSourceReturning(int literal) =>
        new ElmInterpreter.ElmClosureInProcess.SourceRef.Lambda(
            new AbstractExpr.LambdaExpression(
                [],
                new AbstractExpr.Integer(
                    literal,
                    PopularEncodings.IntegerEncoding.EncodeSignedInteger(literal))));

    private static ElmInterpreter.ElmClosureInProcess Closure(
        ElmInterpreter.ElmClosureInProcess.SourceRef source,
        int parameterCount = 1,
        IReadOnlyList<PineValueInProcess>? collected = null,
        IReadOnlyDictionary<string, PineValueInProcess>? captured = null,
        DeclQualifiedName? topLevel = null) =>
        new(
            source,
            parameterCount,
            collected ?? [],
            captured ?? ImmutableDictionary<string, PineValueInProcess>.Empty,
            topLevel ?? Name("Top"));

    // ---- Reference identity ----

    [Fact]
    public void Reference_equal_values_are_equal()
    {
        var value = ListOf(Int(1), Int(2));

        Eq(value, value).Should().BeTrue();
    }

    [Fact]
    public void Reference_equal_closures_are_equal()
    {
        var closure = Closure(LambdaSource());

        Eq(closure, closure).Should().BeTrue();
    }

    // ---- Concrete integers ----

    [Fact]
    public void Equal_integers_are_equal()
    {
        Eq(Int(42), Int(42)).Should().BeTrue();
    }

    [Fact]
    public void Different_integers_are_not_equal()
    {
        Eq(Int(42), Int(43)).Should().BeFalse();
    }

    [Fact]
    public void Integer_lazy_and_evaluated_representations_are_equal()
    {
        var lazy = PineValueInProcess.CreateInteger(7);
        var evaluated = ElmInterpreter.ToProcess(ElmValue.Integer(7));

        Eq(lazy, evaluated).Should().BeTrue();
    }

    // ---- Concrete strings / blobs ----

    [Fact]
    public void Equal_strings_are_equal()
    {
        Eq(Str("hello"), Str("hello")).Should().BeTrue();
    }

    [Fact]
    public void Different_strings_are_not_equal()
    {
        Eq(Str("hello"), Str("world")).Should().BeFalse();
    }

    [Fact]
    public void Blob_is_not_equal_to_list()
    {
        Eq(Int(0), ListOf()).Should().BeFalse();
    }

    // ---- Concrete lists ----

    [Fact]
    public void Empty_lists_are_equal()
    {
        Eq(ListOf(), PineValueInProcess.EmptyList).Should().BeTrue();
    }

    [Fact]
    public void Equal_lists_are_equal()
    {
        Eq(ListOf(Int(1), Int(2), Int(3)), ListOf(Int(1), Int(2), Int(3))).Should().BeTrue();
    }

    [Fact]
    public void Lists_with_different_length_are_not_equal()
    {
        Eq(ListOf(Int(1), Int(2)), ListOf(Int(1), Int(2), Int(3))).Should().BeFalse();
    }

    [Fact]
    public void Lists_with_different_element_are_not_equal()
    {
        Eq(ListOf(Int(1), Int(2)), ListOf(Int(1), Int(9))).Should().BeFalse();
    }

    [Fact]
    public void Nested_lists_compare_structurally()
    {
        var left = ListOf(ListOf(Int(1)), ListOf(Int(2), Int(3)));
        var right = ListOf(ListOf(Int(1)), ListOf(Int(2), Int(3)));

        Eq(left, right).Should().BeTrue();
    }

    [Fact]
    public void Lazy_and_evaluated_list_representations_are_equal()
    {
        var lazy = ListOf(Int(1), Int(2));
        var evaluated =
            ElmInterpreter.ToProcess(
                new ElmValue.ElmList([ElmValue.Integer(1), ElmValue.Integer(2)]));

        Eq(lazy, evaluated).Should().BeTrue();
    }

    // ---- Concrete tagged values ----

    [Fact]
    public void Equal_tagged_values_are_equal()
    {
        var left = ElmInterpreter.ToProcess(ElmValue.TagInstance("Just", [ElmValue.Integer(1)]));
        var right = ElmInterpreter.ToProcess(ElmValue.TagInstance("Just", [ElmValue.Integer(1)]));

        Eq(left, right).Should().BeTrue();
    }

    [Fact]
    public void Tagged_values_with_different_tag_name_are_not_equal()
    {
        var left = ElmInterpreter.ToProcess(ElmValue.TagInstance("Just", [ElmValue.Integer(1)]));
        var right = ElmInterpreter.ToProcess(ElmValue.TagInstance("Err", [ElmValue.Integer(1)]));

        Eq(left, right).Should().BeFalse();
    }

    [Fact]
    public void Tagged_values_with_different_argument_are_not_equal()
    {
        var left = ElmInterpreter.ToProcess(ElmValue.TagInstance("Just", [ElmValue.Integer(1)]));
        var right = ElmInterpreter.ToProcess(ElmValue.TagInstance("Just", [ElmValue.Integer(2)]));

        Eq(left, right).Should().BeFalse();
    }

    // ---- Closures ----

    [Fact]
    public void Closures_with_equal_fields_are_equal()
    {
        var left = Closure(LambdaSource());
        var right = Closure(LambdaSource());

        Eq(left, right).Should().BeTrue();
    }

    [Fact]
    public void Closures_with_different_source_are_not_equal()
    {
        var left = Closure(LambdaSourceReturning(1));
        var right = Closure(LambdaSourceReturning(2));

        Eq(left, right).Should().BeFalse();
    }

    [Fact]
    public void Closures_with_different_parameter_count_are_not_equal()
    {
        var left = Closure(LambdaSource(), parameterCount: 1);
        var right = Closure(LambdaSource(), parameterCount: 2);

        Eq(left, right).Should().BeFalse();
    }

    [Fact]
    public void Closures_with_different_captured_top_level_are_not_equal()
    {
        var left = Closure(LambdaSource(), topLevel: Name("ModuleA"));
        var right = Closure(LambdaSource(), topLevel: Name("ModuleB"));

        Eq(left, right).Should().BeFalse();
    }

    [Fact]
    public void Closures_with_equal_collected_arguments_are_equal()
    {
        var left = Closure(LambdaSource(), collected: [Int(1), Str("x")]);
        var right = Closure(LambdaSource(), collected: [Int(1), Str("x")]);

        Eq(left, right).Should().BeTrue();
    }

    [Fact]
    public void Closures_with_different_collected_arguments_are_not_equal()
    {
        var left = Closure(LambdaSource(), collected: [Int(1)]);
        var right = Closure(LambdaSource(), collected: [Int(2)]);

        Eq(left, right).Should().BeFalse();
    }

    [Fact]
    public void Closures_with_different_collected_argument_count_are_not_equal()
    {
        var left = Closure(LambdaSource(), collected: [Int(1)]);
        var right = Closure(LambdaSource(), collected: [Int(1), Int(2)]);

        Eq(left, right).Should().BeFalse();
    }

    [Fact]
    public void Closures_with_equal_captured_bindings_are_equal()
    {
        var left =
            Closure(
                LambdaSource(),
                captured: new Dictionary<string, PineValueInProcess> { ["a"] = Int(1) });

        var right =
            Closure(
                LambdaSource(),
                captured: new Dictionary<string, PineValueInProcess> { ["a"] = Int(1) });

        Eq(left, right).Should().BeTrue();
    }

    [Fact]
    public void Closures_with_different_captured_binding_value_are_not_equal()
    {
        var left =
            Closure(
                LambdaSource(),
                captured: new Dictionary<string, PineValueInProcess> { ["a"] = Int(1) });

        var right =
            Closure(
                LambdaSource(),
                captured: new Dictionary<string, PineValueInProcess> { ["a"] = Int(2) });

        Eq(left, right).Should().BeFalse();
    }

    [Fact]
    public void Closures_with_different_captured_binding_key_are_not_equal()
    {
        var left =
            Closure(
                LambdaSource(),
                captured: new Dictionary<string, PineValueInProcess> { ["a"] = Int(1) });

        var right =
            Closure(
                LambdaSource(),
                captured: new Dictionary<string, PineValueInProcess> { ["b"] = Int(1) });

        Eq(left, right).Should().BeFalse();
    }

    [Fact]
    public void Self_capturing_closures_do_not_recurse_forever()
    {
        // A let-recursive closure can capture itself in its own environment. Comparing two such
        // closures must terminate via the cycle guard rather than overflowing the stack.
        var leftBindings = new Dictionary<string, PineValueInProcess>();
        var left = Closure(LambdaSource(), captured: leftBindings);
        leftBindings["self"] = left;

        var rightBindings = new Dictionary<string, PineValueInProcess>();
        var right = Closure(LambdaSource(), captured: rightBindings);
        rightBindings["self"] = right;

        Eq(left, right).Should().BeTrue();
    }

    [Fact]
    public void Closure_is_not_equal_to_concrete_value()
    {
        var closure = Closure(LambdaSource());

        Eq(closure, Int(1)).Should().BeFalse();
        Eq(closure, ListOf(Int(1))).Should().BeFalse();
    }

    // ---- Choice-tag constructors ----

    private static ElmInterpreter.ElmChoiceTagConstructorInProcess ChoiceCtor(
        string typeName,
        string tagName,
        int totalArity,
        params PineValueInProcess[] arguments) =>
        new(Name(typeName), tagName, totalArity, [.. arguments]);

    [Fact]
    public void Equal_choice_tag_constructors_are_equal()
    {
        Eq(ChoiceCtor("Maybe", "Just", 1), ChoiceCtor("Maybe", "Just", 1)).Should().BeTrue();
    }

    [Fact]
    public void Choice_tag_constructors_with_different_type_are_not_equal()
    {
        Eq(ChoiceCtor("Maybe", "Just", 1), ChoiceCtor("Result", "Just", 1)).Should().BeFalse();
    }

    [Fact]
    public void Choice_tag_constructors_with_different_tag_name_are_not_equal()
    {
        Eq(ChoiceCtor("Maybe", "Just", 1), ChoiceCtor("Maybe", "Nothing", 1)).Should().BeFalse();
    }

    [Fact]
    public void Choice_tag_constructors_with_different_arity_are_not_equal()
    {
        Eq(ChoiceCtor("Pair", "Pair", 2), ChoiceCtor("Pair", "Pair", 3)).Should().BeFalse();
    }

    [Fact]
    public void Choice_tag_constructors_with_equal_applied_arguments_are_equal()
    {
        Eq(ChoiceCtor("Maybe", "Just", 1, Int(1)), ChoiceCtor("Maybe", "Just", 1, Int(1)))
            .Should().BeTrue();
    }

    [Fact]
    public void Choice_tag_constructors_with_different_applied_arguments_are_not_equal()
    {
        Eq(ChoiceCtor("Maybe", "Just", 1, Int(1)), ChoiceCtor("Maybe", "Just", 1, Int(2)))
            .Should().BeFalse();
    }

    [Fact]
    public void Choice_tag_constructor_is_not_equal_to_record_constructor()
    {
        var choice = ChoiceCtor("Maybe", "Just", 1);
        var record = RecordCtor("Point", ["x"], Int(1));

        Eq(choice, record).Should().BeFalse();
    }

    // ---- Record-type constructors ----

    private static ElmInterpreter.ElmRecordTypeConstructorInProcess RecordCtor(
        string typeName,
        IReadOnlyList<string> fieldNames,
        params PineValueInProcess[] arguments) =>
        new(
            Name(typeName),
            [.. fieldNames.Select(fieldName => (fieldName, StringEncoding.ValueFromString(fieldName)))],
            [.. arguments]);

    [Fact]
    public void Equal_record_type_constructors_are_equal()
    {
        Eq(RecordCtor("Point", ["x", "y"]), RecordCtor("Point", ["x", "y"])).Should().BeTrue();
    }

    [Fact]
    public void Record_type_constructors_with_different_type_are_not_equal()
    {
        Eq(RecordCtor("Point", ["x", "y"]), RecordCtor("Vector", ["x", "y"])).Should().BeFalse();
    }

    [Fact]
    public void Record_type_constructors_with_different_field_names_are_not_equal()
    {
        Eq(RecordCtor("Point", ["x", "y"]), RecordCtor("Point", ["x", "z"])).Should().BeFalse();
    }

    [Fact]
    public void Record_type_constructors_with_different_field_order_are_not_equal()
    {
        Eq(RecordCtor("Point", ["x", "y"]), RecordCtor("Point", ["y", "x"])).Should().BeFalse();
    }

    [Fact]
    public void Record_type_constructors_with_equal_applied_arguments_are_equal()
    {
        Eq(RecordCtor("Point", ["x", "y"], Int(1)), RecordCtor("Point", ["x", "y"], Int(1)))
            .Should().BeTrue();
    }

    [Fact]
    public void Record_type_constructors_with_different_applied_arguments_are_not_equal()
    {
        Eq(RecordCtor("Point", ["x", "y"], Int(1)), RecordCtor("Point", ["x", "y"], Int(9)))
            .Should().BeFalse();
    }

    // ---- Record-access chains ----

    private static ElmInterpreter.ElmRecordAccessChainInProcess AccessChain(params string[] fieldNames) =>
        ElmInterpreter.ElmRecordAccessChainInProcess.CreateFromFieldNames(fieldNames);

    [Fact]
    public void Equal_record_access_chains_are_equal()
    {
        Eq(AccessChain("a", "b"), AccessChain("a", "b")).Should().BeTrue();
    }

    [Fact]
    public void Record_access_chains_with_different_fields_are_not_equal()
    {
        Eq(AccessChain("a", "b"), AccessChain("a", "c")).Should().BeFalse();
    }

    [Fact]
    public void Record_access_chains_with_different_length_are_not_equal()
    {
        Eq(AccessChain("a"), AccessChain("a", "b")).Should().BeFalse();
    }

    [Fact]
    public void Record_access_chain_is_not_equal_to_closure()
    {
        Eq(AccessChain("a"), Closure(LambdaSource())).Should().BeFalse();
    }

    // ---- Composite values embedding opaque leaves ----

    [Fact]
    public void Lists_containing_equal_closures_are_equal()
    {
        var left = ListOf(Int(1), Closure(LambdaSource()));
        var right = ListOf(Int(1), Closure(LambdaSource()));

        Eq(left, right).Should().BeTrue();
    }

    [Fact]
    public void Lists_containing_different_closures_are_not_equal()
    {
        var left = ListOf(Closure(LambdaSourceReturning(1)));
        var right = ListOf(Closure(LambdaSourceReturning(2)));

        Eq(left, right).Should().BeFalse();
    }

    [Fact]
    public void List_with_closure_is_not_equal_to_list_with_concrete_value()
    {
        var left = ListOf(Closure(LambdaSource()));
        var right = ListOf(Int(1));

        Eq(left, right).Should().BeFalse();
        Eq(right, left).Should().BeFalse();
    }

    [Fact]
    public void Nested_lists_with_equal_closures_are_equal()
    {
        var left = ListOf(ListOf(Closure(LambdaSource())), Int(7));
        var right = ListOf(ListOf(Closure(LambdaSource())), Int(7));

        Eq(left, right).Should().BeTrue();
    }

    [Fact]
    public void Tagged_values_containing_equal_closures_are_equal()
    {
        var left = PineValueInProcess.CreateTagged(TagNameValue("Just"), [Closure(LambdaSource())]);
        var right = PineValueInProcess.CreateTagged(TagNameValue("Just"), [Closure(LambdaSource())]);

        Eq(left, right).Should().BeTrue();
    }

    [Fact]
    public void Tagged_values_containing_different_closures_are_not_equal()
    {
        var left = PineValueInProcess.CreateTagged(TagNameValue("Just"), [Closure(LambdaSourceReturning(1))]);
        var right = PineValueInProcess.CreateTagged(TagNameValue("Just"), [Closure(LambdaSourceReturning(2))]);

        Eq(left, right).Should().BeFalse();
    }

    [Fact]
    public void Tagged_values_with_closures_and_different_tag_name_are_not_equal()
    {
        var left = PineValueInProcess.CreateTagged(TagNameValue("Just"), [Closure(LambdaSource())]);
        var right = PineValueInProcess.CreateTagged(TagNameValue("Err"), [Closure(LambdaSource())]);

        Eq(left, right).Should().BeFalse();
    }
}
