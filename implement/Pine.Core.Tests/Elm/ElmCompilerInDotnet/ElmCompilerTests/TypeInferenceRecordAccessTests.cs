using AwesomeAssertions;
using Pine.Core.Elm.ElmCompilerInDotnet;
using Pine.Core.Elm.ElmSyntax.SyntaxModel;
using System.Collections.Generic;
using Xunit;

using SyntaxTypes = Pine.Core.Elm.ElmSyntax.Stil4mElmSyntax7;

namespace Pine.Core.Tests.Elm.ElmCompilerInDotnet.ElmCompilerTests;

/// <summary>
/// Phase 3 tests: <c>RecordAccess</c> (e.g. <c>r.x</c>) and <c>RecordAccessFunction</c>
/// (e.g. <c>.x</c>) drive open-record constraints for parameters, and look up known
/// field types when the record's type is already known.
/// </summary>
public class TypeInferenceRecordAccessTests
{
    private static readonly Range s_dummyRange =
        new(new Location(1, 1), new Location(1, 1));

    private static Node<TValue> Node<TValue>(TValue value) =>
        new(s_dummyRange, value);

    private static SyntaxTypes.Expression Param(string name) =>
        new SyntaxTypes.Expression.FunctionOrValue([], name);

    [Fact]
    public void Record_access_on_parameter_constrains_to_open_record_with_required_field()
    {
        // alfa r = r.name
        var expression =
            new SyntaxTypes.Expression.RecordAccess(Node(Param("r")), Node("name"));

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
    }

    [Fact]
    public void Multiple_record_accesses_on_same_parameter_accumulate_all_required_fields()
    {
        // alfa r = ((r.name, r.age), r.email) modeled as a tuple of accesses.
        // Express via a record literal with three fields whose values are r.x accesses,
        // because we don't have a Tuple constructor easily available here.
        var fields =
            new List<Node<(Node<string> fieldName, Node<SyntaxTypes.Expression> valueExpr)>>
            {
                Node((Node("a"), Node<SyntaxTypes.Expression>(
                    new SyntaxTypes.Expression.RecordAccess(Node(Param("r")), Node("name"))))),
                Node((Node("b"), Node<SyntaxTypes.Expression>(
                    new SyntaxTypes.Expression.RecordAccess(Node(Param("r")), Node("age"))))),
                Node((Node("c"), Node<SyntaxTypes.Expression>(
                    new SyntaxTypes.Expression.RecordAccess(Node(Param("r")), Node("email"))))),
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

        fieldNames.Should().BeEquivalentTo(["name", "age", "email"]);
    }

    [Fact]
    public void Record_access_function_is_inferred_as_function_from_open_record_to_field_type()
    {
        // .name : { r | name : a } -> a
        var expression = new SyntaxTypes.Expression.RecordAccessFunction(".name");

        var inferredType =
            TypeInference.InferExpressionType(
                expression,
                parameterNames: new Dictionary<string, int>(),
                parameterTypes: new Dictionary<string, TypeInference.InferredType>());

        inferredType.Should().BeOfType<TypeInference.InferredType.FunctionType>();

        var functionType = (TypeInference.InferredType.FunctionType)inferredType;
        functionType.ArgumentType.Should().BeOfType<TypeInference.InferredType.OpenRecordType>();

        var openRecord = (TypeInference.InferredType.OpenRecordType)functionType.ArgumentType;
        openRecord.KnownFields.Should().ContainSingle();
        openRecord.KnownFields[0].FieldName.Should().Be("name");

        // The function's return type should match the record's field type (the same fresh variable).
        functionType.ReturnType.Should().Be(openRecord.KnownFields[0].FieldType);
    }

    [Fact]
    public void Record_access_on_known_closed_record_returns_field_type()
    {
        // Given a parameter typed as a closed record { name : String, age : Int }, accessing
        // r.name should infer String.
        var recordType =
            new TypeInference.InferredType.RecordType(
                [("name", new TypeInference.InferredType.StringType()),
                 ("age", new TypeInference.InferredType.IntType())]);

        var expression =
            new SyntaxTypes.Expression.RecordAccess(Node(Param("r")), Node("name"));

        var inferredType =
            TypeInference.InferExpressionType(
                expression,
                parameterNames: new Dictionary<string, int> { ["r"] = 0 },
                parameterTypes: new Dictionary<string, TypeInference.InferredType> { ["r"] = recordType });

        inferredType.Should().BeOfType<TypeInference.InferredType.StringType>();
    }

    [Fact]
    public void Record_access_on_unknown_record_returns_unknown()
    {
        // Without a known type for the record, we cannot determine the field's type.
        var expression =
            new SyntaxTypes.Expression.RecordAccess(Node(Param("r")), Node("name"));

        var inferredType =
            TypeInference.InferExpressionType(
                expression,
                parameterNames: new Dictionary<string, int> { ["r"] = 0 },
                parameterTypes: new Dictionary<string, TypeInference.InferredType>());

        inferredType.Should().BeOfType<TypeInference.InferredType.UnknownType>();
    }
}
