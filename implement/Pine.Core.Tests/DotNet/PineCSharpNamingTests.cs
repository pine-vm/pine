using AwesomeAssertions;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Pine.Core.Addressing;
using Pine.Core.CodeAnalysis;
using Pine.Core.DotNet;
using Pine.Core.Encodings;
using Pine.Core.Functional;
using Pine.Core.Internal;
using Pine.Core.PineLanguage;
using Pine.Core.Runtime.IntermediateVM;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Numerics;
using System.Reflection;
using System.Threading;
using Xunit;

namespace Pine.Core.Tests.DotNet;

public class PineCSharpNamingTests
{
    [Theory]
    [InlineData(-16, "Value_int_neg_16_0x10")]
    [InlineData(-15, "Value_int_neg_15")]
    [InlineData(-1, "Value_int_neg_1")]
    [InlineData(0, "Value_int_pos_0")]
    [InlineData(1, "Value_int_pos_1")]
    [InlineData(15, "Value_int_pos_15")]
    [InlineData(16, "Value_int_pos_16_0x10")]
    public void Pine_CSharp_integer_literal_names_omit_hex_below_sixteen(int value, string expected)
    {
        var naming = new PineCSharpLiteralNaming(new());
        naming.ValueName(IntegerEncoding.EncodeSignedInteger(value)).Should().Be(expected);
    }

    [Theory]
    [InlineData("Map")]
    [InlineData("Switch")]
    public void Pine_CSharp_literal_table_names_use_ten_canonical_hash_digits(string prefix)
    {
        var contents = PineValue.List([PineValue.Blob([8, 9, 10])]);
        var expected = Convert.ToHexStringLower(PineValueHashTree.ComputeHash(contents).Span)[..10];
        var naming = new PineCSharpLiteralNaming(new());
        naming.TableName(prefix, contents).Should().Be(prefix + "_" + expected);
        naming.TableName(prefix, contents).Should().Be(prefix + "_" + expected);
    }

    [Fact]
    public void Pine_CSharp_literal_names_render_strict_integers_and_identifier_strings_and_preserve_bytes()
    {
        var big = BigInteger.One << 80;

        PineValue[] values =
            [
                IntegerEncoding.EncodeSignedInteger(0),
                IntegerEncoding.EncodeSignedInteger(128),
                IntegerEncoding.EncodeSignedInteger(-128),
                IntegerEncoding.EncodeSignedInteger(big),
                IntegerEncoding.EncodeSignedInteger(-big),
                StringEncoding.ValueFromString("case"),
                StringEncoding.ValueFromString("Elm_9"),
                StringEncoding.ValueFromString("élm_变量9"),
                StringEncoding.ValueFromString("123"),
                PineValue.EmptyBlob,
                PineValue.Blob([4, 0, 1]),
                PineValue.Blob([2, 0]),
                PineValue.Blob([0, 17, 0, 0]),
                PineValue.Blob([0, 0, 216, 0]),
                StringEncoding.ValueFromString("a-b"),
                StringEncoding.ValueFromString("a'b"),
                StringEncoding.ValueFromString("a b"),
                StringEncoding.ValueFromString("😀")
            ];

        var cache = new ConcurrentPineValueHashCache();

        var compilation =
            PineCSharpCompiler.Compile(
                Expression.LitralInst(PineValue.List(values)),
                options: new(EnableCancellation: false, ContainerNamespace: "LiteralNaming.Tests"),
                canonicalHashCache: cache).Extract(error => throw new Exception(error.Describe()));

        using var assembly = Load(compilation);
        var pool = assembly.Assembly.GetType(compilation.Options.LiteralsTypeName)!;

        var expectedNames =
            new[]
            {
                "Value_int_pos_0", "Value_int_pos_128_0x80", "Value_int_neg_128_0x80",
                "Value_int_pos_1208925819614629174706176_0x100000000000000000000",
                "Value_int_neg_1208925819614629174706176_0x100000000000000000000"
            };

        for (var index = 0; index < values.Length; index++)
        {
            var hash = Convert.ToHexStringLower(cache.GetHash(values[index]).Span[..5]);

            var name =
                index < 5
                ?
                expectedNames[index]
                :
                index < 10
                ?
                "Value_string_" + new[] { "case", "Elm_9", "élm_变量9", "123", "" }[index - 5] + "_" + hash
                :
                "Value_" + hash;

            var property = pool.GetProperty(name);
            property.Should().NotBeNull("literal {0} must have its readable or ten-digit fallback name", index);
            var literal = (PineValueInProcess)property!.GetValue(null)!;
            literal.Evaluate().Should().Be(values[index]);
            literal.Evaluate().Should().Be(pool.GetField("Materialized_" + name)!.GetValue(null));
        }

        assembly.Evaluate(PineValueInProcess.EmptyList)!.Evaluate().Should().Be(PineValue.List(values));
        var methods = SyntaxFactory.ParseCompilationUnit(compilation.MethodsSource);

        methods.Usings.Should().Contain(
            item => item.Alias!.Name.Identifier.ValueText == "PineLiterals" &&
                item.Name!.ToString() == "global::LiteralNaming.Tests.PineLiterals");

        methods.DescendantNodes().OfType<MemberAccessExpressionSyntax>().Should().NotContain(
            member =>
            member.Expression.ToString() == "global::LiteralNaming.Tests.PineLiterals");
    }

    [Theory]
    [InlineData(null, "PineLiterals")]
    [InlineData("Pgo.LiteralNaming", "PineLiterals")]
    [InlineData("Pgo.LiteralNaming", "environment")]
    [InlineData("Pgo.LiteralNaming", "state_0")]
    [InlineData("PineLiterals.Refinement", "PineLiterals")]
    public void Pine_CSharp_literal_module_alias_avoids_local_shadowing(string? containerNamespace, string poolName)
    {
        var scenario = PineCSharpScenarios.Load("guards");

        var variant =
            scenario.Variant("generic-and-specialized") with
            {
                Options =
                scenario.Variant("generic-and-specialized").Options with
                {
                    ContainerNamespace = containerNamespace,
                    LiteralsClassName = poolName
                }
            };

        var compilation = Compile(scenario, variant);
        using var assembly = Load(compilation);
        var syntax = SyntaxFactory.ParseCompilationUnit(compilation.MethodsSource);

        var alias =
            syntax.Usings.Single(item => item.Name!.ToString() == "global::" + compilation.Options.LiteralsTypeName);

        syntax.DescendantNodes().OfType<MemberAccessExpressionSyntax>().Should().Contain(
            member =>
            member.Expression.ToString() == alias.Alias!.Name.Identifier.ValueText);

        assembly.Evaluate(PineValueInProcess.Create(scenario.Evaluation("matching").Environment))!.Evaluate()
            .Should().Be(PineCSharpScenarios.ExpectedValue(scenario, variant.Name, "matching"));
    }

    [Fact]
    public void Pine_CSharp_literal_module_alias_cannot_shadow_a_custom_worker_name()
    {
        var scenario = PineCSharpScenarios.Load("literal-reuse");

        var compilation =
            Compile(
                scenario,
                scenario.Variant("without-cancellation"),
                new((_, _) => "PineLiterals", _ => "Dispatch"));

        using var assembly = Load(compilation);
        assembly.Evaluate(PineValueInProcess.EmptyList)!.Evaluate().Should().Be(scenario.Values["literal"]);
        compilation.MethodsSource.Should().Contain("using PineLiteralsModule =");
    }

    [Fact]
    public void Pine_CSharp_default_method_names_use_canonical_expression_and_constraint_hashes()
    {
        var scenario = PineCSharpScenarios.Load("guards");
        var variant = scenario.Variant("generic-and-specialized");
        var compilation = Compile(scenario, variant);

        var expressionHash =
            Convert.ToHexStringLower(
            PineValueHashTree.ComputeHash(ExpressionEncoding.EncodeExpressionAsValue(scenario.Entry)).Span)[..10];

        compilation.DispatchMethodName.Should().Be("Expr_" + expressionHash + "_dispatch");

        compilation.Methods.Select(method => method.Name).Should().Equal(
            "Expr_" + expressionHash + "_constraint_" + variant.Specializations.Single().HashBase16[..10],
            "Expr_" + expressionHash + "_constraint_generic");

        PineCSharpCompiler.Compile(scenario.Entry, null)
            .Extract(error => throw new Exception(error.Describe())).Methods.Single().Name
            .Should().Be("Expr_" + expressionHash + "_constraint_generic");

        using var loaded = Load(compilation);
        var type = loaded.Assembly.GetType(compilation.Options.MethodsTypeName)!;
        type.GetMethod(compilation.DispatchMethodName).Should().NotBeNull();

        foreach (var method in compilation.Methods)
            type.GetMethod(method.Name).Should().NotBeNull();
    }

    [Theory]
    [InlineData(true)]
    [InlineData(false)]
    public void Pine_CSharp_empty_environment_constraint_uses_a_single_generic_method(bool includeGeneric)
    {
        var scenario = PineCSharpScenarios.Load("identity");
        var cache = new ConcurrentPineValueHashCache();
        var naming = PineCSharpMethodNaming.Default(cache);

        naming.GetMethodName(scenario.Entry, PineValueClass.Empty).Should().Be(
            naming.GetMethodName(scenario.Entry, null));

        var compilation =
            PineCSharpCompiler.Compile(
                scenario.Entry,
                [PineValueClass.Empty],
                new(IncludeGenericEntry: includeGeneric),
                canonicalHashCache: cache).Extract(error => throw new Exception(error.Describe()));

        compilation.Methods.Should().ContainSingle();
        compilation.Methods.Single().Name.Should().EndWith("_constraint_generic");
        using var loaded = Load(compilation);

        loaded.Evaluate(PineValueInProcess.Create(scenario.Evaluation("small").Environment))!
            .Evaluate().Should().Be(PineCSharpScenarios.ExpectedValue(scenario, "without-cancellation", "small"));
    }

    [Theory]
    [InlineData("guards", "generic-and-specialized")]
    [InlineData("mutual-tail", "with-cancellation")]
    [InlineData("spread-call", "without-cancellation")]
    public void Pine_CSharp_callbacks_name_every_declaration_and_all_calls_use_those_names(
        string scenarioName, string variantName)
    {
        var scenario = PineCSharpScenarios.Load(scenarioName);
        var variant = scenario.Variant(variantName);
        var cache = new ConcurrentPineValueHashCache();
        var defaults = PineCSharpMethodNaming.Default(cache);
        var workerCalls = new List<(Expression Expression, PineValueClass? Constraint)>();
        var dispatchCalls = new List<Expression>();

        var naming =
            new PineCSharpMethodNaming(
                (expression, constraint) =>
                {
                    workerCalls.Add((expression, constraint));
                    return "Custom_" + defaults.GetMethodName(expression, constraint);
                },
                expression =>
                {
                    dispatchCalls.Add(expression);
                    return "Custom_" + defaults.GetDispatchMethodName(expression);
                });

        var compilation = Compile(scenario, variant, naming, cache);
        dispatchCalls.Should().Equal(scenario.Entry);
        workerCalls.Count.Should().Be(compilation.Methods.Count);

        for (var index = 0; index < workerCalls.Count; index++)
        {
            workerCalls[index].Expression.Should().Be(compilation.Methods[index].Function.Expression);
            workerCalls[index].Constraint.Should().Be(compilation.Methods[index].Function.EnvironmentConstraint);
        }

        compilation.Methods.Should().OnlyContain(method => method.Name.StartsWith("Custom_"));
        compilation.DispatchMethodName.Should().StartWith("Custom_");
        using var loaded = Load(compilation);

        loaded.Evaluate(PineValueInProcess.Create(scenario.Evaluations[0].Environment))!
            .Evaluate().Should().Be(
            PineCSharpScenarios.ExpectedValue(scenario, variantName, scenario.Evaluations[0].Name));
    }

    [Fact]
    public void Pine_CSharp_custom_method_names_can_match_local_names_without_shadowing_calls()
    {
        var scenario = PineCSharpScenarios.Load("spread-call");
        var variant = scenario.Variant("without-cancellation");
        var index = 0;
        var naming = new PineCSharpMethodNaming((_, _) => "local_0_" + index++, _ => "environment");
        var compilation = Compile(scenario, variant, naming);
        using var loaded = Load(compilation);

        loaded.Evaluate(PineValueInProcess.Create(scenario.Evaluation("small").Environment))!
            .Evaluate().Should().Be(PineCSharpScenarios.ExpectedValue(scenario, variant.Name, "small"));
    }

    [Theory]
    [InlineData("not-a-name")]
    [InlineData("class")]
    [InlineData("@escaped")]
    [InlineData("CompiledPine")]
    [InlineData("PineValueInProcess")]
    [InlineData("BlobMemory")]
    [InlineData("ListMemory")]
    [InlineData("BigInteger")]
    [InlineData("Loop_0")]
    public void Pine_CSharp_invalid_worker_or_dispatch_names_fail_before_assembly_compilation(string name)
    {
        var scenario = PineCSharpScenarios.Load("identity");
        var variant = scenario.Variant("without-cancellation");
        Action worker = () => Compile(scenario, variant, new((_, _) => name, _ => "Dispatch"));
        Action dispatch = () => Compile(scenario, variant, new((_, _) => "Worker", _ => name));
        worker.Should().Throw<ArgumentException>().WithMessage("*" + name + "*");
        dispatch.Should().Throw<ArgumentException>().WithMessage("*" + name + "*");
    }

    [Fact]
    public void Pine_CSharp_colliding_worker_and_dispatch_names_are_reported_explicitly()
    {
        var scenario = PineCSharpScenarios.Load("guards");
        var variant = scenario.Variant("generic-and-specialized");
        Action sameWorkers = () => Compile(scenario, variant, new((_, _) => "Worker", _ => "Dispatch"));
        sameWorkers.Should().Throw<ArgumentException>().WithMessage("*Duplicate generated method name: Worker");
        Action sameDispatch = () => Compile(scenario, variant, new((_, _) => "Dispatch", _ => "Dispatch"));
        sameDispatch.Should().Throw<ArgumentException>().WithMessage("*Duplicate generated method name: Dispatch");
    }

    [Fact]
    public void Pine_CSharp_shared_naming_policy_reuses_canonical_encodings_between_declarations_and_compilations()
    {
        var scenario = PineCSharpScenarios.Load("guards");
        var cache = new ConcurrentPineValueHashCache();
        var naming = PineCSharpMethodNaming.Default(cache);
        var expression = scenario.Entry;
        var constraint = scenario.Variant("specialized-only").Specializations.Single();
        var worker = naming.GetMethodName(expression, constraint);
        var dispatch = naming.GetDispatchMethodName(expression);
        using var profile = new PineVMWorkProfile(referenceCapacity: 0);

        using (profile.Enter())
        {
            naming.GetMethodName(expression, null).Should().EndWith("_constraint_generic");
            naming.GetMethodName(expression, constraint).Should().Be(worker);
            naming.GetDispatchMethodName(expression).Should().Be(dispatch);
        }

        profile[PineVMWorkProfile.Metric.ListValueConstructorAllocations].Should().Be(0);
        AssertCached(cache, ExpressionEncoding.EncodeExpressionAsValue(expression));
        var first = Compile(scenario, scenario.Variant("specialized-only"), naming, cache);
        var refined = Compile(scenario, scenario.Variant("generic-and-specialized"), naming, cache);
        first.Methods.Single().Name.Should().Be(refined.Methods.First().Name);
        first.DispatchMethodName.Should().Be(refined.DispatchMethodName);
    }

    [Fact]
    public void Pine_CSharp_shared_default_naming_and_hash_cache_support_concurrent_refinements()
    {
        var scenario = PineCSharpScenarios.Load("guards");
        var variant = scenario.Variant("generic-and-specialized");
        var cache = new ConcurrentPineValueHashCache();
        var naming = PineCSharpMethodNaming.Default(cache);
        var expected = Compile(scenario, variant, naming, cache);
        var results = new PineCSharpCompilation[4];

        System.Threading.Tasks.Parallel.For(
            0,
            results.Length,
            index =>
            {
                results[index] =
                    Compile(
                        scenario,
                        variant with
                        {
                            Options = variant.Options with { ContainerNamespace = "Pgo.Refinement" + index }
                        },
                        naming,
                        cache);
            });

        foreach (var result in results)
        {
            result.DispatchMethodName.Should().Be(expected.DispatchMethodName);
            result.Methods.Select(method => method.Name).Should().Equal(expected.Methods.Select(method => method.Name));
        }

        AssertCached(cache, ExpressionEncoding.EncodeExpressionAsValue(scenario.Entry));
    }

    [Fact]
    public void Pine_CSharp_compilation_uses_the_caller_hash_tree_cache_for_methods_and_literals()
    {
        var scenario = PineCSharpScenarios.Load("mutual-tail");
        var variant = scenario.Variant("without-cancellation");
        var cache = new ConcurrentPineValueHashCache();

        var compilation =
            PineCSharpCompiler.Compile(
                scenario.Entry,
                variant.Specializations,
                variant.Options,
                canonicalHashCache: cache).Extract(error => throw new Exception(error.Describe()));

        foreach (var method in compilation.Methods)
            AssertCached(cache, ExpressionEncoding.EncodeExpressionAsValue(method.Function.Expression));

        var literalScenario = PineCSharpScenarios.Load("literal-reuse");
        Compile(literalScenario, literalScenario.Variant("without-cancellation"), canonicalHashCache: cache);
        AssertCached(cache, literalScenario.Values["literal"]);
        Compile(scenario, variant, canonicalHashCache: cache);
        AssertCached(cache, ExpressionEncoding.EncodeExpressionAsValue(scenario.Entry));
    }

    [Fact]
    public void Pine_CSharp_rejected_graphs_do_not_invoke_naming_or_force_diagnostic_hashes()
    {
        var scenario = PineCSharpScenarios.Load("transitive-dynamic");

        var expression =
            Expression.ListInst(
                [scenario.Entry, Expression.LitralInst(PineValue.Blob(Guid.NewGuid().ToByteArray()))]);

        var cache = new ConcurrentPineValueHashCache();
        var callbacks = 0;

        var naming =
            new PineCSharpMethodNaming(
                (_, _) =>
                {
                    callbacks++;
                    return "Worker";
                },
                _ =>
                {
                    callbacks++;
                    return "Dispatch";
                });

        var rejection = PineCSharpCompiler.Compile(naming, expression, canonicalHashCache: cache).IsErrOrNull()!;
        callbacks.Should().Be(0);
        rejection.Describe().Should().Contain("DynamicEvaluation");
        var computed = 0;
        cache.GetHash(ExpressionEncoding.EncodeExpressionAsValue(expression), _ => { computed++; return true; });
        computed.Should().BeGreaterThan(0);
        rejection.Describe(includeExpressionHashes: true).Should().Contain(rejection.Path.Function.ExpressionHash);
        AssertCached(cache, ExpressionEncoding.EncodeExpressionAsValue(rejection.Path.Function.Expression));
    }

    [Theory]
    [InlineData("")]
    [InlineData("Pgo..Refinement")]
    [InlineData("Pgo.invalid-name")]
    [InlineData("Pgo.class")]
    [InlineData("Pgo.@escaped")]
    public void Pine_CSharp_invalid_container_namespaces_are_rejected(string containerNamespace)
    {
        Action compile =
            () => PineCSharpCompiler.Compile(
                Expression.EnvironmentInstance,
                options: new(ContainerNamespace: containerNamespace));

        compile.Should().Throw<ArgumentException>().WithMessage("*namespace segment*");
    }

    [Theory]
    [InlineData("Generated.System.Threading")]
    [InlineData("PineValueInProcess.Generated")]
    [InlineData("Kernel.PineValue.Refinement")]
    public void Pine_CSharp_namespace_qualification_cannot_shadow_framework_types_or_aliases(string containerNamespace)
    {
        var scenario = PineCSharpScenarios.Load("fixed-result");
        var variant = scenario.Variant("without-cancellation");

        var compilation =
            Compile(
                scenario,
                variant with
                {
                    Options =
                    variant.Options with
                    {
                        ContainerNamespace = containerNamespace,
                        EnableCancellation = true
                    }
                });

        using var loaded = Load(compilation);

        loaded.Evaluate(PineValueInProcess.Create(scenario.Evaluations[0].Environment))!
            .Evaluate().Should().Be(
            PineCSharpScenarios.ExpectedValue(scenario, variant.Name, scenario.Evaluations[0].Name));
    }

    [Fact]
    public void Pine_CSharp_namespaced_refinements_remain_independent_while_loaded_together()
    {
        var scenario = PineCSharpScenarios.Load("guards");
        var cache = new ConcurrentPineValueHashCache();
        var naming = PineCSharpMethodNaming.Default(cache);

        var firstVariant =
            scenario.Variant("specialized-only") with
            {
                Options =
                scenario.Variant("specialized-only").Options with
                {
                    ContainerNamespace = "Pine.Generated.Refinement1",
                    EnableCancellation = false
                }
            };

        var refinedVariant =
            scenario.Variant("generic-and-specialized") with
            {
                Options =
                scenario.Variant("generic-and-specialized").Options with
                {
                    ContainerNamespace = "Pine.Generated.Refinement2",
                    EnableCancellation = true
                }
            };

        var first = Compile(scenario, firstVariant, naming, cache);
        var refined = Compile(scenario, refinedVariant, naming, cache);
        using var firstAssembly = Load(first);
        using var refinedAssembly = Load(refined);
        var matching = PineValueInProcess.Create(scenario.Evaluation("matching").Environment);
        var mismatching = PineValueInProcess.Create(scenario.Evaluation("mismatching").Environment);
        var expected = PineCSharpScenarios.ExpectedValue(scenario, "generic-and-specialized", "matching");
        firstAssembly.Evaluate(matching)!.Evaluate().Should().Be(expected);
        refinedAssembly.Evaluate(matching)!.Evaluate().Should().Be(expected);
        firstAssembly.Evaluate(mismatching).Should().BeNull();

        refinedAssembly.Evaluate(mismatching)!.Evaluate().Should().Be(
            PineCSharpScenarios.ExpectedValue(scenario, "generic-and-specialized", "mismatching"));

        first.DispatchMethodName.Should().Be(refined.DispatchMethodName);
        firstAssembly.Assembly.GetType(first.Options.MethodsTypeName).Should().NotBeNull();
        firstAssembly.Assembly.GetType(refined.Options.MethodsTypeName).Should().BeNull();
        var firstPool = firstAssembly.Assembly.GetType(first.Options.LiteralsTypeName)!;
        var refinedPool = refinedAssembly.Assembly.GetType(refined.Options.LiteralsTypeName)!;
        firstPool.FullName.Should().StartWith("Pine.Generated.Refinement1.");
        refinedPool.FullName.Should().StartWith("Pine.Generated.Refinement2.");
        var field = firstPool.GetFields(BindingFlags.Static | BindingFlags.Public).First();
        firstPool.Should().NotBeSameAs(refinedPool);
        firstPool.GetField(field.Name)!.GetValue(null).Should().Be(refinedPool.GetField(field.Name)!.GetValue(null));
        using var cancelled = new CancellationTokenSource();
        cancelled.Cancel();
        Action run = () => refinedAssembly.Evaluate(matching, cancelled.Token);
        run.Should().Throw<OperationCanceledException>();
        firstAssembly.Evaluate(matching, cancelled.Token)!.Evaluate().Should().Be(expected);
    }

    private static void AssertCached(ConcurrentPineValueHashCache cache, PineValue value)
    {
        var computed = 0;
        cache.GetHash(value, _ => { computed++; return true; });
        computed.Should().Be(0, "canonical hash-tree nodes should already be retained in the caller's cache");
    }

    private static PineCSharpCompilation Compile(
        PineCSharpScenario scenario,
        PineCSharpScenarioVariant variant,
        PineCSharpMethodNaming? naming = null,
        ConcurrentPineValueHashCache? canonicalHashCache = null) =>
        (naming is null
        ?
        PineCSharpCompiler.Compile(
            scenario.Entry,
            variant.Specializations,
            variant.Options,
            canonicalHashCache: canonicalHashCache)
        :
        PineCSharpCompiler.Compile(
            naming,
            scenario.Entry,
            variant.Specializations,
            variant.Options,
            canonicalHashCache: canonicalHashCache))
        .Extract(error => throw new Exception(error.Describe()));

    private static PineCSharpAssembly Load(PineCSharpCompilation compilation) =>
        PineCSharpAssembly.Load(compilation).Extract(errors => throw new Exception(string.Join("\n", errors)));
}
