using AwesomeAssertions;
using Pine.Core.Elm.ElmCompilerInDotnet;
using System.Collections.Generic;
using Xunit;

using SyntaxTypes = Pine.Core.Elm.ElmSyntax.Stil4mElmSyntax7;

namespace Pine.Core.Tests.Elm.ElmCompilerInDotnet.ElmCompilerTests;

/// <summary>
/// Tests for type inference of prefix operator expressions such as <c>(::)</c>, <c>(+)</c>, etc.
/// These tests verify that <see cref="TypeInference.InferExpressionType"/> correctly identifies
/// prefix operators as function types, which is required for the specialization pipeline to
/// recognize them as higher-order arguments.
/// </summary>
public class TypeInferencePrefixOperatorTests
{
    [Fact]
    public void Cons_operator_prefix_is_inferred_as_function_type()
    {
        // (::) has type `a -> List a -> List a` in Elm.
        // InferExpressionType should return a FunctionType for this expression.

        var expression =
            new SyntaxTypes.Expression.PrefixOperator("::");

        var inferredType =
            TypeInference.InferExpressionType(
                expression,
                parameterNames: new Dictionary<string, int>(),
                parameterTypes: new Dictionary<string, TypeInference.InferredType>());

        inferredType.Should().BeOfType<TypeInference.InferredType.FunctionType>();
    }

    [Fact]
    public void Add_operator_prefix_is_inferred_as_function_type()
    {
        // (+) has type `number -> number -> number` in Elm.

        var expression =
            new SyntaxTypes.Expression.PrefixOperator("+");

        var inferredType =
            TypeInference.InferExpressionType(
                expression,
                parameterNames: new Dictionary<string, int>(),
                parameterTypes: new Dictionary<string, TypeInference.InferredType>());

        inferredType.Should().BeOfType<TypeInference.InferredType.FunctionType>();
    }

    [Fact]
    public void Idiv_operator_prefix_is_inferred_as_function_type()
    {
        // (//) has type `Int -> Int -> Int` in Elm.

        var expression =
            new SyntaxTypes.Expression.PrefixOperator("//");

        var inferredType =
            TypeInference.InferExpressionType(
                expression,
                parameterNames: new Dictionary<string, int>(),
                parameterTypes: new Dictionary<string, TypeInference.InferredType>());

        inferredType.Should().BeOfType<TypeInference.InferredType.FunctionType>();
    }

    [Fact]
    public void Eq_operator_prefix_is_inferred_as_function_type()
    {
        // (==) has type `a -> a -> Bool` in Elm.

        var expression =
            new SyntaxTypes.Expression.PrefixOperator("==");

        var inferredType =
            TypeInference.InferExpressionType(
                expression,
                parameterNames: new Dictionary<string, int>(),
                parameterTypes: new Dictionary<string, TypeInference.InferredType>());

        inferredType.Should().BeOfType<TypeInference.InferredType.FunctionType>();
    }
}
