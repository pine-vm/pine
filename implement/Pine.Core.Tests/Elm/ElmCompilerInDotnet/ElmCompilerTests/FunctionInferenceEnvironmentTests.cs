using AwesomeAssertions;
using Pine.Core.Elm.ElmCompilerInDotnet;
using Pine.Core.Elm.Syntax.Abstract;
using System;
using System.Collections;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Threading.Tasks;
using Xunit;

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
            new Expression.Application(
                Expression.Identifier.Create(["Library"], "consume"),
                [Expression.Identifier.Create([], "value")]);

        Pattern[] arguments = [new Pattern.VarPattern("value")];

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
            new Expression.Application(
                Expression.Identifier.Create(["Library"], "consume"),
                [Expression.Identifier.Create([], "value")]);

        Pattern[] arguments = [new Pattern.VarPattern("value")];

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
                [new Pattern.VarPattern("other")],
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
        var expression = Expression.Identifier.Create([], "value");

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
            new Expression.Application(
                Expression.Identifier.Create(["Library"], "CONSUME"),
                [Expression.Identifier.Create([], "value")]);

        var signatures =
            new Dictionary<string, TypeInference.InferredType>(StringComparer.OrdinalIgnoreCase)
            {
                ["Library.consume"] =
                new TypeInference.InferredType.FunctionType(
                    new TypeInference.InferredType.IntType(),
                    new TypeInference.InferredType.StringType())
            };

        TypeInference.InferFunctionDeclarationType(
            expression, [new Pattern.VarPattern("value")], "Test", signatures)
            .parameterTypes["value"].Should().BeOfType<TypeInference.InferredType.IntType>();
    }

    [Theory]
    [InlineData("", "consume", typeof(TypeInference.InferredType.IntType))]
    [InlineData("Second", "consume", typeof(TypeInference.InferredType.FloatType))]
    [InlineData("", "length", typeof(TypeInference.InferredType.CharType))]
    [InlineData("List", "length", typeof(TypeInference.InferredType.CharType))]
    [InlineData("", "sleep", typeof(TypeInference.InferredType.FloatType))]
    [InlineData("Missing", "consume", null)]
    [InlineData("", "missing", null)]
    [InlineData("", "CONSUME", null)]
    public void Indexed_usage_inference_preserves_qualified_and_first_suffix_matches(
        string module, string name, Type? expectedParameterType)
    {
        var source =
            new Dictionary<string, TypeInference.InferredType>
            {
                ["First.consume"] = Signature(new TypeInference.InferredType.IntType()),
                ["Second.consume"] = Signature(new TypeInference.InferredType.FloatType()),
                ["List.length"] = Signature(new TypeInference.InferredType.CharType())
            };

        var environment = new TypeInference.FunctionInferenceEnvironment(source);

        var expression =
            new Expression.Application(
                Expression.Identifier.Create(module.Length == 0 ? [] : [module], name),
                [Expression.Identifier.Create([], "value")]);

        var parameters = new Dictionary<string, int> { ["value"] = 0 };

        var indexed =
            TypeInference.InferParameterTypesFromUsage(expression, parameters, environment.FunctionSignatures);

        var fallback =
            TypeInference.InferParameterTypesFromUsage(
                expression,
                parameters,
                new ObservedSignatures(environment.FunctionSignatures));

        indexed.Should().BeEquivalentTo(fallback);

        if (expectedParameterType is null)
            indexed.Should().NotContainKey("value");

        else
            indexed["value"].Should().BeOfType(expectedParameterType);
    }

    [Fact]
    public void Exact_unqualified_signature_takes_precedence_over_suffix_index()
    {
        var source =
            new Dictionary<string, TypeInference.InferredType>
            {
                ["First.consume"] = Signature(new TypeInference.InferredType.IntType()),
                ["consume"] = Signature(new TypeInference.InferredType.BoolType())
            };

        var environment = new TypeInference.FunctionInferenceEnvironment(source);

        var expression =
            new Expression.Application(
                Expression.Identifier.Create([], "consume"),
                [Expression.Identifier.Create([], "value")]);

        TypeInference.InferParameterTypesFromUsage(
            expression, new Dictionary<string, int> { ["value"] = 0 }, environment.FunctionSignatures)
            ["value"].Should().BeOfType<TypeInference.InferredType.BoolType>();
    }

    [Fact]
    public void Shared_index_avoids_reenumeration_for_concurrent_hits_and_misses()
    {
        var source =
            new ObservedSignatures(
                new Dictionary<string, TypeInference.InferredType>
                {
                    ["Library.consume"] = Signature(new TypeInference.InferredType.IntType())
                });

        var environment = new TypeInference.FunctionInferenceEnvironment(source);
        var enumerationsAfterPreparation = source.Enumerations;
        var parameters = new Dictionary<string, int> { ["value"] = 0 };

        Parallel.For(
            0,
            64,
            index =>
            {
                var hit = index % 2 == 0;

                var expression =
                    new Expression.Application(
                        Expression.Identifier.Create([], hit ? "consume" : "missing"),
                        [Expression.Identifier.Create([], "value")]);

                var result =
                    TypeInference.InferParameterTypesFromUsage(expression, parameters, environment.FunctionSignatures);

                if (hit)
                    result["value"].Should().BeOfType<TypeInference.InferredType.IntType>();

                else
                    result.Should().NotContainKey("value");
            });

        source.Enumerations.Should().Be(enumerationsAfterPreparation);
    }

    private static TypeInference.InferredType Signature(TypeInference.InferredType parameter) =>
        new TypeInference.InferredType.FunctionType(parameter, new TypeInference.InferredType.StringType());

    private sealed class ObservedSignatures(IReadOnlyDictionary<string, TypeInference.InferredType> source)
        : IReadOnlyDictionary<string, TypeInference.InferredType>
    {
        private int enumerations;

        public int Enumerations => System.Threading.Volatile.Read(ref enumerations);

        public TypeInference.InferredType this[string key] => source[key];

        public IEnumerable<string> Keys => source.Keys;

        public IEnumerable<TypeInference.InferredType> Values => source.Values;

        public int Count => source.Count;

        public bool ContainsKey(string key) => source.ContainsKey(key);

        public bool TryGetValue(string key, out TypeInference.InferredType value) =>
            source.TryGetValue(key, out value!);

        public IEnumerator<KeyValuePair<string, TypeInference.InferredType>> GetEnumerator()
        {
            System.Threading.Interlocked.Increment(ref enumerations);
            return source.GetEnumerator();
        }

        IEnumerator IEnumerable.GetEnumerator() => GetEnumerator();
    }
}
