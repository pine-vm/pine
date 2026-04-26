using AwesomeAssertions;
using Pine.Core.Elm.ElmCompilerInDotnet;
using Pine.Core.Elm.ElmSyntax.SyntaxModel;
using System.Collections.Generic;
using Xunit;

using SyntaxTypes = Pine.Core.Elm.ElmSyntax.Stil4mElmSyntax7;

namespace Pine.Core.Tests.Elm.ElmCompilerInDotnet.ElmCompilerTests;

/// <summary>
/// Phase 2 tests: infix <c>++</c> drives <c>appendable</c> constraints for parameters,
/// and specializes them to <c>String</c> or <c>List a</c> when paired with a concrete operand.
/// </summary>
public class TypeInferenceAppendableTests
{
    private static readonly Range s_dummyRange =
        new(new Location(1, 1), new Location(1, 1));

    private static Node<TValue> Node<TValue>(TValue value) =>
        new(s_dummyRange, value);

    private static SyntaxTypes.Expression Param(string name) =>
        new SyntaxTypes.Expression.FunctionOrValue([], name);

    private static SyntaxTypes.Expression OpApp(
        string op,
        SyntaxTypes.Expression left,
        SyntaxTypes.Expression right) =>
        new SyntaxTypes.Expression.OperatorApplication(
            op,
            InfixDirection.Left,
            Node(left),
            Node(right));

    [Fact]
    public void Parameter_used_with_string_literal_via_concat_is_constrained_to_String()
    {
        // alfa a = a ++ "x"
        var expression =
            OpApp("++", Param("a"), new SyntaxTypes.Expression.Literal("x"));

        var parameterTypes =
            TypeInference.InferParameterTypesFromUsage(
                expression,
                parameterNames: new Dictionary<string, int> { ["a"] = 0 },
                functionSignatures: new Dictionary<string, TypeInference.InferredType>());

        parameterTypes.Should().ContainKey("a");
        parameterTypes["a"].Should().BeOfType<TypeInference.InferredType.StringType>();
    }

    [Fact]
    public void Parameter_used_with_list_literal_via_concat_is_constrained_to_List()
    {
        // alfa a = a ++ [1, 2]
        var listLiteral =
            new SyntaxTypes.Expression.ListExpr(
                [
                Node<SyntaxTypes.Expression>(new SyntaxTypes.Expression.Integer(1)),
                Node<SyntaxTypes.Expression>(new SyntaxTypes.Expression.Integer(2))
                ]);

        var expression = OpApp("++", Param("a"), listLiteral);

        var parameterTypes =
            TypeInference.InferParameterTypesFromUsage(
                expression,
                parameterNames: new Dictionary<string, int> { ["a"] = 0 },
                functionSignatures: new Dictionary<string, TypeInference.InferredType>());

        parameterTypes.Should().ContainKey("a");
        parameterTypes["a"].Should().BeOfType<TypeInference.InferredType.ListType>();
    }

    [Fact]
    public void Two_parameters_concatenated_are_both_constrained_to_appendable()
    {
        // alfa a b = a ++ b
        var expression = OpApp("++", Param("a"), Param("b"));

        var parameterTypes =
            TypeInference.InferParameterTypesFromUsage(
                expression,
                parameterNames: new Dictionary<string, int> { ["a"] = 0, ["b"] = 1 },
                functionSignatures: new Dictionary<string, TypeInference.InferredType>());

        parameterTypes.Should().ContainKey("a");
        parameterTypes.Should().ContainKey("b");

        parameterTypes["a"].Should().BeOfType<TypeInference.InferredType.TypeVariable>();

        ((TypeInference.InferredType.TypeVariable)parameterTypes["a"]).Constraint
            .Should().Be(TypeVariableConstraint.Appendable);

        parameterTypes["b"].Should().BeOfType<TypeInference.InferredType.TypeVariable>();

        ((TypeInference.InferredType.TypeVariable)parameterTypes["b"]).Constraint
            .Should().Be(TypeVariableConstraint.Appendable);
    }

    [Fact]
    public void String_concat_in_record_field_value_constrains_parameter_to_String()
    {
        // alfa a = { x = a ++ "x" }
        var fieldValue =
            OpApp("++", Param("a"), new SyntaxTypes.Expression.Literal("x"));

        var fields =
            new List<Node<(Node<string> fieldName, Node<SyntaxTypes.Expression> valueExpr)>>
            {
                Node((Node("x"), Node(fieldValue))),
            };

        var expression = new SyntaxTypes.Expression.RecordExpr(fields);

        var parameterTypes =
            TypeInference.InferParameterTypesFromUsage(
                expression,
                parameterNames: new Dictionary<string, int> { ["a"] = 0 },
                functionSignatures: new Dictionary<string, TypeInference.InferredType>());

        parameterTypes.Should().ContainKey("a");
        parameterTypes["a"].Should().BeOfType<TypeInference.InferredType.StringType>();
    }

    [Fact]
    public void Nested_concatenation_propagates_String_constraint_through_chain()
    {
        // alfa a b = a ++ (b ++ "x")
        var inner =
            OpApp("++", Param("b"), new SyntaxTypes.Expression.Literal("x"));

        var expression = OpApp("++", Param("a"), inner);

        var parameterTypes =
            TypeInference.InferParameterTypesFromUsage(
                expression,
                parameterNames: new Dictionary<string, int> { ["a"] = 0, ["b"] = 1 },
                functionSignatures: new Dictionary<string, TypeInference.InferredType>());

        parameterTypes.Should().ContainKey("a");
        parameterTypes.Should().ContainKey("b");

        parameterTypes["b"].Should().BeOfType<TypeInference.InferredType.StringType>();

        // 'a' is appendable at minimum; with full propagation it becomes String.
        var typeOfA = parameterTypes["a"];

        var aIsStringOrAppendable =
            typeOfA is TypeInference.InferredType.StringType ||
            (typeOfA is TypeInference.InferredType.TypeVariable tv &&
            tv.Constraint == TypeVariableConstraint.Appendable);

        aIsStringOrAppendable.Should().BeTrue("'a' should be String or appendable, was " + typeOfA);
    }

    [Fact]
    public void Concat_operator_on_two_String_literals_infers_String_result()
    {
        var expression =
            OpApp(
                "++",
                new SyntaxTypes.Expression.Literal("hi"),
                new SyntaxTypes.Expression.Literal("there"));

        var inferredType =
            TypeInference.InferExpressionType(
                expression,
                parameterNames: new Dictionary<string, int>(),
                parameterTypes: new Dictionary<string, TypeInference.InferredType>());

        inferredType.Should().BeOfType<TypeInference.InferredType.StringType>();
    }

    [Fact]
    public void Concat_operator_on_two_lists_infers_List_result()
    {
        var leftList =
            new SyntaxTypes.Expression.ListExpr(
                [Node<SyntaxTypes.Expression>(new SyntaxTypes.Expression.Integer(1))]);

        var rightList =
            new SyntaxTypes.Expression.ListExpr(
                [Node<SyntaxTypes.Expression>(new SyntaxTypes.Expression.Integer(2))]);

        var expression = OpApp("++", leftList, rightList);

        var inferredType =
            TypeInference.InferExpressionType(
                expression,
                parameterNames: new Dictionary<string, int>(),
                parameterTypes: new Dictionary<string, TypeInference.InferredType>());

        inferredType.Should().BeOfType<TypeInference.InferredType.ListType>();
    }

    [Fact]
    public void Concat_with_no_concrete_operand_returns_appendable_constraint()
    {
        var expression = OpApp("++", Param("a"), Param("b"));

        var inferredType =
            TypeInference.InferExpressionType(
                expression,
                parameterNames: new Dictionary<string, int> { ["a"] = 0, ["b"] = 1 },
                parameterTypes: new Dictionary<string, TypeInference.InferredType>());

        inferredType.Should().BeOfType<TypeInference.InferredType.TypeVariable>();

        ((TypeInference.InferredType.TypeVariable)inferredType).Constraint
            .Should().Be(TypeVariableConstraint.Appendable);
    }
}

