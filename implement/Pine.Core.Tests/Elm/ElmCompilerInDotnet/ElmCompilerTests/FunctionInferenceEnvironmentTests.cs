using AwesomeAssertions;
using Pine.Core.Elm.ElmCompilerInDotnet;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using Xunit;

using SyntaxTypes = Pine.Core.Elm.ElmSyntax.ElmSyntaxAbstract;

namespace Pine.Core.Tests.Elm.ElmCompilerInDotnet.ElmCompilerTests;

public class FunctionInferenceEnvironmentTests
{
    [Fact]
    public void Shared_environment_preserves_usage_inference()
    {
        var signatures =
            ImmutableDictionary<string, TypeInference.InferredType>.Empty.Add(
                "Library.consume",
                new TypeInference.InferredType.FunctionType(
                    new TypeInference.InferredType.IntType(),
                    new TypeInference.InferredType.StringType()));

        var expression =
            new SyntaxTypes.Expression.Application(
                SyntaxTypes.Expression.Identifier.Create(["Library"], "consume"),
                [SyntaxTypes.Expression.Identifier.Create([], "value")]);

        SyntaxTypes.Pattern[] arguments = [new SyntaxTypes.Pattern.VarPattern("value")];

        var environment = new TypeInference.FunctionInferenceEnvironment(signatures);

        var original =
            TypeInference.InferFunctionDeclarationType(expression, arguments, "Test", signatures);

        var shared =
            TypeInference.InferFunctionDeclarationType(expression, arguments, "Test", environment);

        shared.returnType.Should().Be(original.returnType);
        shared.returnType.Should().BeOfType<TypeInference.InferredType.StringType>();
        shared.parameterTypes["value"].Should().BeOfType<TypeInference.InferredType.IntType>();
        shared.parameterTypes.Should().BeEquivalentTo(original.parameterTypes);
    }

    [Fact]
    public void Changed_arguments_or_signature_environment_do_not_reuse_body_only_results()
    {
        var expression =
            new SyntaxTypes.Expression.Application(
                SyntaxTypes.Expression.Identifier.Create(["Library"], "consume"),
                [SyntaxTypes.Expression.Identifier.Create([], "value")]);

        SyntaxTypes.Pattern[] arguments = [new SyntaxTypes.Pattern.VarPattern("value")];

        var signatures =
            ImmutableDictionary<string, TypeInference.InferredType>.Empty.Add(
                "Library.consume",
                new TypeInference.InferredType.FunctionType(
                    new TypeInference.InferredType.IntType(),
                    new TypeInference.InferredType.StringType()));

        var environment = new TypeInference.FunctionInferenceEnvironment(signatures);

        var first =
            TypeInference.InferFunctionDeclarationType(expression, arguments, "Test", environment);

        var differentArguments =
            TypeInference.InferFunctionDeclarationType(
                expression,
                [new SyntaxTypes.Pattern.VarPattern("other")],
                "Test",
                environment);

        var changedEnvironment =
            new TypeInference.FunctionInferenceEnvironment(
                signatures.SetItem(
                    "Library.consume",
                    new TypeInference.InferredType.FunctionType(
                        new TypeInference.InferredType.FloatType(),
                        new TypeInference.InferredType.BoolType())));

        var differentSignatures =
            TypeInference.InferFunctionDeclarationType(expression, arguments, "Test", changedEnvironment);

        first.parameterTypes["value"].Should().BeOfType<TypeInference.InferredType.IntType>();
        differentArguments.parameterTypes.Should().NotContainKey("value");
        differentSignatures.parameterTypes["value"].Should().BeOfType<TypeInference.InferredType.FloatType>();
        differentSignatures.returnType.Should().BeOfType<TypeInference.InferredType.BoolType>();
    }

    [Fact]
    public void Current_module_is_part_of_inference_context()
    {
        var expression = SyntaxTypes.Expression.Identifier.Create([], "value");

        var signatures =
            ImmutableDictionary<string, TypeInference.InferredType>.Empty
            .Add("First.value", new TypeInference.InferredType.IntType())
            .Add("Second.value", new TypeInference.InferredType.FloatType());

        var environment = new TypeInference.FunctionInferenceEnvironment(signatures);

        TypeInference.InferFunctionDeclarationType(expression, [], "First", environment)
            .returnType.Should().BeOfType<TypeInference.InferredType.IntType>();

        TypeInference.InferFunctionDeclarationType(expression, [], "Second", environment)
            .returnType.Should().BeOfType<TypeInference.InferredType.FloatType>();
    }

    [Fact]
    public void Legacy_overload_preserves_signature_lookup_comparer()
    {
        var expression =
            new SyntaxTypes.Expression.Application(
                SyntaxTypes.Expression.Identifier.Create(["Library"], "CONSUME"),
                [SyntaxTypes.Expression.Identifier.Create([], "value")]);

        var signatures =
            new Dictionary<string, TypeInference.InferredType>(StringComparer.OrdinalIgnoreCase)
            {
                ["Library.consume"] =
                new TypeInference.InferredType.FunctionType(
                    new TypeInference.InferredType.IntType(),
                    new TypeInference.InferredType.StringType())
            };

        TypeInference.InferFunctionDeclarationType(
            expression, [new SyntaxTypes.Pattern.VarPattern("value")], "Test", signatures)
            .parameterTypes["value"].Should().BeOfType<TypeInference.InferredType.IntType>();
    }
}
