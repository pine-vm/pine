using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.CommonEncodings;
using Pine.Core.Elm.ElmCompilerInDotnet;
using Pine.Core.Tests.Elm.ElmCompilerTests;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Numerics;
using System.Text.Json;
using Xunit;

using Model = Pine.Core.Elm.ElmCompilerInDotnet.CompilerModel;
using IR = Pine.Core.Elm.ElmCompilerInDotnet.IntermediateRepresentation;
using Shape = Pine.Core.Elm.ElmCompilerInDotnet.StructuralType;
using InfixDirection = Pine.Core.Elm.ElmSyntax.SyntaxModel.InfixDirection;

namespace Pine.Core.Tests.Elm.ElmCompilerInDotnet.IntermediateRepresentation;

public class ModelTests
{
    private static readonly Model.FunctionId Owner =
        new Model.FunctionId.Source(DeclQualifiedName.Create(["Test"], "root"));

    private static Model.BindingId Id(int slot) => new Model.BindingId.Local(Owner, slot);

    private static IR.Expression Integer(int value) =>
        new IR.Expression.LiteralValue(IntegerEncoding.EncodeSignedInteger(value));

    private static IR.Expression Local(int slot) => new IR.Expression.Identifier(new Model.Reference.Local(Id(slot)));

    public static IEnumerable<object[]> Expressions()
    {
        IR.Expression[] values =
            [
                Integer(42),
                Local(0),
                new IR.Expression.Negation(Local(0)),
                new IR.Expression.ListExpr([Local(0), Integer(2)]),
                new IR.Expression.IfBlock(Local(0), Integer(1), Integer(2)),
                new IR.Expression.PrefixOperator("+"),
                new IR.Expression.Application(Local(0), [Integer(1)]),
                new IR.Expression.OperatorApplication("+", InfixDirection.Left, Local(0), Integer(1)),
                new IR.Expression.ProjectItem(Local(0), 1),
                new IR.Expression.CaseExpression(Local(0), [new(new Model.Pattern.VarPattern(Id(1)), Local(1))]),
                new IR.Expression.LetExpression([new(new Model.Pattern.VarPattern(Id(1)), Local(0))], Local(1)),
                new IR.Expression.RecordExpr([new("field", Integer(1))]),
                new IR.Expression.RecordAccess(Local(0), "field"),
                new IR.Expression.RecordAccessFunction("field"),
                new IR.Expression.RecordUpdateExpression(Local(0), [new("field", Integer(1))]),
                new IR.Expression.GLSLExpression("shader")
            ];

        return values.Select(value => new object[] { value });
    }

    public static IEnumerable<object[]> Patterns()
    {
        Model.Pattern[] values =
            [
                new Model.Pattern.AllPattern(),
                new Model.Pattern.VarPattern(Id(0)),
                new Model.Pattern.UnitPattern(),
                new Model.Pattern.CharPattern(0x1F600),
                new Model.Pattern.StringPattern("text\n"),
                new Model.Pattern.IntPattern(BigInteger.One << 100),
                new Model.Pattern.FloatPattern(1.25),
                new Model.Pattern.TuplePattern(
                    [new Model.Pattern.VarPattern(Id(0)), new Model.Pattern.VarPattern(Id(1))]),
                new Model.Pattern.ListPattern([new Model.Pattern.VarPattern(Id(0))]),
                new Model.Pattern.UnConsPattern(
                    new Model.Pattern.VarPattern(Id(0)),
                    new Model.Pattern.VarPattern(Id(1))),
                new Model.Pattern.NamedPattern(
                    DeclQualifiedName.Create(["Test"], "Tag"),
                    [
                    new Model.Pattern.VarPattern(Id(0))
                    ]),
                new Model.Pattern.AsPattern(new Model.Pattern.VarPattern(Id(0)), Id(1))
            ];

        return values.Select(value => new object[] { value });
    }

    [Theory]
    [MemberData(nameof(Patterns))]
    public void Every_resolved_pattern_round_trips_all_semantic_fields(Model.Pattern value)
    {
        var options = Model.CompilerModelJson.CreateOptions();
        var restored = JsonSerializer.Deserialize<Model.Pattern>(JsonSerializer.Serialize(value, options), options);
        restored.Should().Be(value);
        restored!.GetHashCode().Should().Be(value.GetHashCode());
    }

    [Theory]
    [MemberData(nameof(Expressions))]
    public void Resolved_IR_round_trips_every_variant_and_preserves_structural_equality(IR.Expression value)
    {
        var options = Model.CompilerModelJson.CreateOptions();
        var json = JsonSerializer.Serialize(value, options);
        var restored = JsonSerializer.Deserialize<IR.Expression>(json, options);
        restored.Should().Be(value);
        restored!.GetHashCode().Should().Be(value.GetHashCode());
    }

    [Fact]
    public void Expression_fixtures_cover_every_backend_variant() =>
        Expressions().Select(row => row[0].GetType()).Should().BeEquivalentTo(typeof(IR.Expression).GetNestedTypes());

    [Fact]
    public void Pattern_fixtures_cover_every_resolved_variant() =>
        Patterns().Select(row => row[0].GetType()).Should().BeEquivalentTo(typeof(Model.Pattern).GetNestedTypes());

    [Fact]
    public void Binding_identity_is_mandatory_and_has_no_competing_name()
    {
        var first = new Model.Pattern.VarPattern(Id(0));
        var second = new Model.Pattern.VarPattern(Id(1));
        first.Should().NotBe(second);
        new Model.Pattern.VarPattern(Id(0)).Should().Be(first);
        typeof(Model.Pattern.VarPattern).GetProperty("Name").Should().BeNull();
        typeof(Model.Pattern.AsPattern).GetProperty("Name").Should().BeNull();
        typeof(IR.Expression.RecordUpdateExpression).GetProperty("RecordName").Should().BeNull();
        typeof(IR.FunctionStruct).GetProperty("Name").Should().BeNull();
        typeof(Model.BindingId).GetNestedType("Unresolved").Should().BeNull();
    }

    [Fact]
    public void JSON_distinguishes_bindings_and_preserves_aliases_and_record_receivers()
    {
        var options = Model.CompilerModelJson.CreateOptions();
        Model.Pattern first = new Model.Pattern.VarPattern(Id(0));
        Model.Pattern second = new Model.Pattern.VarPattern(Id(1));
        JsonSerializer.Serialize(first, options).Should().NotBe(JsonSerializer.Serialize(second, options));
        Model.Pattern alias = new Model.Pattern.AsPattern(first, new Model.BindingId.Temporary(Owner, 2));
        JsonSerializer.Deserialize<Model.Pattern>(JsonSerializer.Serialize(alias, options), options).Should().Be(alias);
        IR.Expression update = new IR.Expression.RecordUpdateExpression(Local(1), [new("field", Local(0))]);

        JsonSerializer.Deserialize<IR.Expression>(JsonSerializer.Serialize(update, options), options).Should().Be(
            update);
    }

    [Fact]
    public void Nested_specialization_identities_round_trip_without_generated_names()
    {
        var constraints =
            new Model.ParameterConstraint[]
            {
                new Model.ParameterConstraint.KnownFunction(0, new Model.FunctionId.Lifted(Owner, 2)),
                new Model.ParameterConstraint.ClosedRecord(
                    1,
                    Shape.ClosedRecord.Create(
                        ImmutableDictionary<string, Shape>.Empty.Add("name", Shape.StringType.Instance)))
            };

        var id = Model.FunctionId.Specialized.Create(Owner, constraints);
        Model.BindingId binding = new Model.BindingId.Local(id, 3);
        var options = Model.CompilerModelJson.CreateOptions();
        var restored = JsonSerializer.Deserialize<Model.BindingId>(JsonSerializer.Serialize(binding, options), options);
        restored.Should().Be(binding);
        restored!.GetHashCode().Should().Be(binding.GetHashCode());
    }

    [Fact]
    public void Copying_and_rewriting_nodes_preserves_hash_contracts()
    {
        var first = new IR.Expression.RecordUpdateExpression(Local(0), [new("field", Integer(1))]);
        var rewritten = first with { Fields = [new("other", Integer(2))] };
        var constructed = new IR.Expression.RecordUpdateExpression(Local(0), [new("other", Integer(2))]);
        rewritten.Should().Be(constructed);
        rewritten.GetHashCode().Should().Be(constructed.GetHashCode());
        new HashSet<IR.Expression> { rewritten }.Should().Contain(constructed);
    }

    [Fact]
    public void Encoded_labels_are_derived_from_one_authoritative_field_name()
    {
        var field = new IR.RecordSetter("first", Integer(1));
        var renamed = field with { FieldName = "second" };
        renamed.FieldNameValue.Should().Be(StringEncoding.ValueFromString("second"));
        typeof(IR.RecordSetter).GetProperty("FieldNameValue")!.SetMethod.Should().BeNull();
    }

    [Fact]
    public void Missing_identities_and_negative_slots_are_rejected_at_construction()
    {
        Action[] invalid =
            [
                () => new Model.Pattern.VarPattern(null!),
                () => new Model.Pattern.AsPattern(new Model.Pattern.AllPattern(), null!),
                () => new Model.Reference.Local(null!),
                () => new Model.BindingId.Local(Owner, -1),
                () => new Model.BindingId.Temporary(Owner, -1),
                () => new Model.FunctionId.Lifted(Owner, -1),
                () => new Model.ParameterConstraint.KnownFunction(-1, Owner)
            ];

        foreach (var construct in invalid)
            construct.Should().Throw<ArgumentException>();

        typeof(Model.Pattern.VarPattern).GetProperty("Binding")!.SetMethod.Should().BeNull();
    }

    [Fact]
    public void Source_identities_do_not_retain_mutable_namespace_lists_or_inconsistent_names()
    {
        var namespaces = new[] { "Test" };
        var id = new Model.FunctionId.Source(DeclQualifiedName.Create(namespaces, "root"));
        var hash = id.GetHashCode();
        namespaces[0] = "Changed";
        id.Name.Namespaces.Should().Equal("Test");
        id.GetHashCode().Should().Be(hash);

        var invalid = () => new Model.FunctionId.Source(id.Name with { DeclName = "different" });
        invalid.Should().Throw<ArgumentException>();
    }

    [Fact]
    public void Source_metadata_cannot_provide_a_competing_function_implementation()
    {
        var declaration =
            new Core.Elm.ElmSyntax.ElmSyntaxAbstract.Declaration.FunctionDeclaration(
                new(null, new("root", [], Core.Elm.ElmSyntax.ElmSyntaxAbstract.Expression.UnitExpr.Instance)));

        var construct =
            () => new Model.Program(
                [],
                ImmutableDictionary<DeclQualifiedName, Core.Elm.ElmSyntax.ElmSyntaxAbstract.Declaration>.Empty
                .Add(DeclQualifiedName.Create(["Test"], "root"), declaration));

        construct.Should().Throw<ArgumentException>();
    }

    [Fact]
    public void Whole_program_JSON_uses_structural_entries_and_preserves_compilation()
    {
        var program =
            ElmCompiler.LowerToCompilerModel(
                TestCase.DefaultAppWithoutPackages(
                    [
                    """
                    module Test exposing (root)
                    type alias Person = { age : Int }
                    root : Person -> Int
                    root { age } = (\extra -> age + extra) 2
                    """
                    ]).AsFileTree(),
                [DeclQualifiedName.Create(["Test"], "root")])
            .Extract(error => throw new InvalidOperationException(error)).CompilerProgram;

        var options = Model.CompilerModelJson.CreateOptions();
        var json = JsonSerializer.Serialize(program, options);
        var restored = JsonSerializer.Deserialize<Model.Program>(json, options)!;
        restored.Functions.Should().BeEquivalentTo(program.Functions);
        restored.BindingNames.Should().BeEquivalentTo(program.BindingNames);
        Model.ProgramValidation.Validate(restored);

        var expected =
            Model.Compilation.Compile(program).Extract(error => throw new InvalidOperationException(error.ToString()));

        var actual =
            Model.Compilation.Compile(restored).Extract(error => throw new InvalidOperationException(error.ToString()));

        foreach (var (id, artifact) in expected)
            actual[id].Should().Be(artifact);
    }
}
