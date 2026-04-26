using AwesomeAssertions;
using Pine.Core.Elm.ElmCompilerInDotnet;
using Pine.Core.Elm.ElmSyntax.SyntaxModel;
using System.Collections.Generic;
using Xunit;

using SyntaxTypes = Pine.Core.Elm.ElmSyntax.Stil4mElmSyntax7;

namespace Pine.Core.Tests.Elm.ElmCompilerInDotnet.ElmCompilerTests;

/// <summary>
/// Phase 4 tests: <c>RecordUpdateExpression</c> (e.g. <c>{ r | x = e1, y = e2 }</c>)
/// constrains the updated parameter to an open record requiring at least the listed
/// fields, with each field's type taken from the assigned value's inferred type. The
/// expression's own type is the record type itself.
/// </summary>
public class TypeInferenceRecordUpdateTests
{
    private static readonly Range s_dummyRange =
        new(new Location(1, 1), new Location(1, 1));

    private static Node<TValue> Node<TValue>(TValue value) =>
        new(s_dummyRange, value);

    private static Node<(Node<string> fieldName, Node<SyntaxTypes.Expression> valueExpr)> Field(
        string name,
        SyntaxTypes.Expression value) =>
        Node((Node(name), Node(value)));

    [Fact]
    public void Record_update_on_parameter_constrains_to_open_record_with_inferred_field_type()
    {
        // alfa r = { r | name = "x" }
        var expression =
            new SyntaxTypes.Expression.RecordUpdateExpression(
                Node("r"),
                [Field("name", new SyntaxTypes.Expression.Literal("x"))]);

        var parameterTypes =
            TypeInference.InferParameterTypesFromUsage(
                expression,
                parameterNames: new Dictionary<string, int> { ["r"] = 0 },
                functionSignatures: new Dictionary<string, TypeInference.InferredType>());

        parameterTypes.Should().ContainKey("r");
        parameterTypes["r"].Should().BeOfType<TypeInference.InferredType.OpenRecordType>();

        var openRecord = (TypeInference.InferredType.OpenRecordType)parameterTypes["r"];
        openRecord.KnownFields.Should().ContainSingle();
        openRecord.KnownFields[0].FieldName.Should().Be("name");
        openRecord.KnownFields[0].FieldType.Should().BeOfType<TypeInference.InferredType.StringType>();
    }

    [Fact]
    public void Record_update_with_multiple_fields_constrains_to_open_record_with_all_fields()
    {
        // alfa r = { r | name = "x", age = 1 }
        var expression =
            new SyntaxTypes.Expression.RecordUpdateExpression(
                Node("r"),
                [
                Field("name", new SyntaxTypes.Expression.Literal("x")),
                Field("age", new SyntaxTypes.Expression.Integer(1)),
                ]);

        var parameterTypes =
            TypeInference.InferParameterTypesFromUsage(
                expression,
                parameterNames: new Dictionary<string, int> { ["r"] = 0 },
                functionSignatures: new Dictionary<string, TypeInference.InferredType>());

        parameterTypes.Should().ContainKey("r");
        parameterTypes["r"].Should().BeOfType<TypeInference.InferredType.OpenRecordType>();

        var openRecord = (TypeInference.InferredType.OpenRecordType)parameterTypes["r"];
        var fieldsByName = new Dictionary<string, TypeInference.InferredType>();

        foreach (var (name, type) in openRecord.KnownFields)
            fieldsByName[name] = type;

        fieldsByName.Should().ContainKey("name").WhoseValue
            .Should().BeOfType<TypeInference.InferredType.StringType>();

        fieldsByName.Should().ContainKey("age").WhoseValue
            .Should().BeOfType<TypeInference.InferredType.NumberType>();
    }

    [Fact]
    public void Record_update_returns_existing_parameter_type_when_already_known()
    {
        // If r already has a known closed record type, the update expression has that same type.
        var existingRecord =
            new TypeInference.InferredType.RecordType(
                [("name", new TypeInference.InferredType.StringType())]);

        var expression =
            new SyntaxTypes.Expression.RecordUpdateExpression(
                Node("r"),
                [Field("name", new SyntaxTypes.Expression.Literal("y"))]);

        var inferredType =
            TypeInference.InferExpressionType(
                expression,
                parameterNames: new Dictionary<string, int> { ["r"] = 0 },
                parameterTypes: new Dictionary<string, TypeInference.InferredType> { ["r"] = existingRecord });

        inferredType.Should().Be(existingRecord);
    }

    [Fact]
    public void Multiple_record_updates_on_same_parameter_accumulate_fields()
    {
        // alfa r = { a = { r | name = "x" }, b = { r | age = 1 } }
        var update1 =
            new SyntaxTypes.Expression.RecordUpdateExpression(
                Node("r"),
                [Field("name", new SyntaxTypes.Expression.Literal("x"))]);

        var update2 =
            new SyntaxTypes.Expression.RecordUpdateExpression(
                Node("r"),
                [Field("age", new SyntaxTypes.Expression.Integer(1))]);

        var fields =
            new List<Node<(Node<string> fieldName, Node<SyntaxTypes.Expression> valueExpr)>>
            {
                Field("a", update1),
                Field("b", update2),
            };

        var expression = new SyntaxTypes.Expression.RecordExpr(fields);

        var parameterTypes =
            TypeInference.InferParameterTypesFromUsage(
                expression,
                parameterNames: new Dictionary<string, int> { ["r"] = 0 },
                functionSignatures: new Dictionary<string, TypeInference.InferredType>());

        parameterTypes.Should().ContainKey("r");
        parameterTypes["r"].Should().BeOfType<TypeInference.InferredType.OpenRecordType>();

        var openRecord = (TypeInference.InferredType.OpenRecordType)parameterTypes["r"];
        var fieldNames = new HashSet<string>();

        foreach (var (name, _) in openRecord.KnownFields)
            fieldNames.Add(name);

        fieldNames.Should().BeEquivalentTo(["name", "age"]);
    }

    [Fact]
    public void Record_update_then_access_unifies_required_fields()
    {
        // alfa r = { x = r.email, y = { r | name = "x" } }
        // r should be constrained to require { email, name }.
        var fields =
            new List<Node<(Node<string> fieldName, Node<SyntaxTypes.Expression> valueExpr)>>
            {
                Field(
                    "x",
                    new SyntaxTypes.Expression.RecordAccess(
                        Node<SyntaxTypes.Expression>(new SyntaxTypes.Expression.FunctionOrValue([], "r")),
                        Node("email"))),
                Field(
                    "y",
                    new SyntaxTypes.Expression.RecordUpdateExpression(
                        Node("r"),
                        [Field("name", new SyntaxTypes.Expression.Literal("x"))])),
            };

        var expression = new SyntaxTypes.Expression.RecordExpr(fields);

        var parameterTypes =
            TypeInference.InferParameterTypesFromUsage(
                expression,
                parameterNames: new Dictionary<string, int> { ["r"] = 0 },
                functionSignatures: new Dictionary<string, TypeInference.InferredType>());

        parameterTypes.Should().ContainKey("r");
        parameterTypes["r"].Should().BeOfType<TypeInference.InferredType.OpenRecordType>();

        var openRecord = (TypeInference.InferredType.OpenRecordType)parameterTypes["r"];
        var fieldNames = new HashSet<string>();

        foreach (var (name, _) in openRecord.KnownFields)
            fieldNames.Add(name);

        fieldNames.Should().BeEquivalentTo(["email", "name"]);
    }
}
