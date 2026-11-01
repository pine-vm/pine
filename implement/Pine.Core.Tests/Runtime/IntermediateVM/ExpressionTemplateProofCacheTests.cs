using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.CodeGen.Pine;
using Pine.Core.Encodings;
using Pine.Core.Functional;
using Pine.Core.Internal;
using Pine.Core.PineLanguage;
using Pine.Core.Runtime;
using Pine.Core.Runtime.IntermediateVM;
using System;
using System.Threading;
using System.Threading.Tasks;
using Xunit;

namespace Pine.Core.Tests.Runtime.IntermediateVM;

public class ExpressionTemplateProofCacheTests
{
    [Fact]
    public void Fixed_constructor_proofs_rebind_changing_captures_and_preserve_escaped_encodings()
    {
        var cache = new PineVMParseCache();
        using var profile = new PineVMWorkProfile(referenceCapacity: 0);
        using var scope = profile.Enter();
        var entry = FunctionValueBuilder.TryBuildCurriedFunctionValueAsTemplate(Expression.EnvironmentInstance, 3)!;
        var interpreter = DirectInterpreter.WithoutEvalCaching(cache);

        for (var index = 0; index < 20; ++index)
        {
            var captured = PineValue.List([IntegerEncoding.EncodeSignedInteger(index), PineValue.EmptyBlob]);
            var partial = Apply(interpreter, entry, captured);
            var plan = ExpressionTemplatePlan.TryParseTemplate(partial, cache)!;

            var ordinary =
                ExpressionTemplatePlan.TryParseTemplateUncached(
                partial, new TemplateValueRecipe.Constant(partial), new PineVMParseCache(), 0)!;

            plan.Should().NotBeNull();
            plan.TemplateValue.Should().BeSameAs(partial);
            plan.EnvironmentCount.Should().Be(ordinary.EnvironmentCount);
            plan.EncodedTerminalExpression.Should().Be(ordinary.EncodedTerminalExpression);
            plan.ElidesTerminalEval.Should().Be(ordinary.ElidesTerminalEval);

            PineValueInProcess[] environments =
                [PineValueInProcess.CreateInteger(index + 100), PineValueInProcess.EmptyList];

            var intermediate = Apply(interpreter, partial, environments[0].Evaluate());
            plan.Materialize([environments[0]], new DirectInterpreterCounters()).Should().Be(intermediate);

            plan.BuildTerminalEnvironment(environments).Evaluate().Should().Be(
                Apply(interpreter, intermediate, environments[1].Evaluate()));
        }

        profile[PineVMWorkProfile.Metric.TemplateShapeApplications].Should().Be(20);
        cache.TemplateConstructorProofCount.Should().Be(1);
        profile[PineVMWorkProfile.Metric.TemplateShapeFactories].Should().Be(1);
    }

    [Fact]
    public void Code_like_bound_data_and_different_terminal_code_are_never_frozen()
    {
        var cache = new PineVMParseCache();

        PineValue[] capturedValues =
            [
            PineValue.EmptyBlob,
            StringEncoding.ValueFromString("Environment"),
            ExpressionEncoding.EncodeExpressionAsValue(Expression.EnvironmentInstance),
            ExpressionEncoding.EncodeExpressionAsValue(Expression.LitralInst(PineValue.EmptyList)),
            PineValue.List([StringEncoding.ValueFromString("Litral")])
            ];

        foreach (var body in new Expression[]
        {
            Expression.EnvironmentInstance,
            Expression.LitralInst(IntegerEncoding.EncodeSignedInteger(31)),
            Expression.BuiltinInst("length", Expression.EnvironmentInstance)
        })
        {
            var entry = FunctionValueBuilder.TryBuildCurriedFunctionValueAsTemplate(body, 2)!;
            var interpreter = DirectInterpreter.WithoutEvalCaching(cache);

            foreach (var captured in capturedValues)
            {
                var partial = Apply(interpreter, entry, captured);
                var plan = ExpressionTemplatePlan.TryParseTemplate(partial, cache)!;
                plan.Should().NotBeNull();
                plan.TerminalExpression.Should().Be(body);
                PineValueInProcess[] arguments = [PineValueInProcess.EmptyBlob];

                interpreter.EvaluateExpressionDefault(
                    plan.TerminalExpression,
                    plan.BuildTerminalEnvironment(arguments).Evaluate())
                    .Should().Be(Apply(interpreter, partial, PineValue.EmptyBlob));
            }
        }
    }

    [Theory]
    [InlineData(1)]
    [InlineData(128)]
    [InlineData(129)]
    [InlineData(256)]
    public void Cached_bounded_proofs_preserve_depth_limits_and_remaining_budgets(int depth)
    {
        PineValue value = PineValue.EmptyBlob;

        for (var index = 0; index < depth; ++index)
            value = PineValue.List([value]);

        var cache = new PineVMParseCache();

        foreach (var consumed in new[] { 0, 16_200, 16_383 })
        {
            var fresh = new ExpressionTemplatePlan.RecognitionBudget();
            var cached = new ExpressionTemplatePlan.RecognitionBudget();

            for (var index = 0; index < consumed; ++index)
            {
                fresh.Visit(0);
                cached.Visit(0);
            }

            cache.TemplateProofs.CheckValue(value, cached).Should().Be(fresh.CheckValue(value));
            cached.Exhausted.Should().Be(fresh.Exhausted);

            if (!cached.Exhausted)
                cached.Consumed.Should().Be(fresh.Consumed);
        }

        cache.TemplateBoundedValueProofCount.Should().Be(1);
    }

    [Fact]
    public void Bounded_proofs_preserve_wide_list_and_expanded_DAG_limits()
    {
        var cache = new PineVMParseCache();
        var items = new PineValue[20_000];
        Array.Fill(items, PineValue.EmptyBlob);
        var wide = PineValue.List(items);
        PineValue dag = PineValue.EmptyBlob;

        for (var index = 0; index < 15; ++index)
            dag = PineValue.List([dag, dag]);

        foreach (var value in new[] { wide, dag, PineValue.List([wide]), PineValue.EmptyBlob })
        {
            for (var index = 0; index < 2; ++index)
            {
                var cached = new ExpressionTemplatePlan.RecognitionBudget();
                var fresh = new ExpressionTemplatePlan.RecognitionBudget();
                cache.TemplateProofs.CheckValue(value, cached).Should().Be(fresh.CheckValue(value));
            }
        }
    }

    [Fact]
    public void Unknown_dynamic_and_malformed_code_remain_recognition_misses()
    {
        var cache = new PineVMParseCache();

        var dynamicCode =
            ExpressionEncoding.EncodeExpressionAsValue(
                new Expression.Eval(Expression.EnvironmentInstance, Expression.EnvironmentInstance));

        var dynamicLiteral =
            ExpressionEncoding.EncodeExpressionAsValue(
                Expression.ListInst(
                    [Expression.LitralInst(StringEncoding.ValueFromString("Litral")), Expression.EnvironmentInstance]));

        var malformed =
            ExpressionEncoding.EncodeExpressionAsValue(
                Expression.LitralInst(PineValue.List([StringEncoding.ValueFromString("Litral")])));

        for (var index = 0; index < 2; ++index)
        {
            foreach (var value in new[] { dynamicCode, dynamicLiteral, malformed })
                ExpressionTemplatePlan.TryParseTemplate(value, cache).Should().BeNull();
        }
    }

    [Fact]
    public void Shared_proofs_bind_parallel_inputs_without_mutable_state_leaking_between_plans()
    {
        var cache = new PineVMParseCache();
        var entry = FunctionValueBuilder.TryBuildCurriedFunctionValueAsTemplate(Expression.EnvironmentInstance, 2)!;

        Parallel.For(
            0,
            32,
            index =>
            {
                var interpreter = DirectInterpreter.WithoutEvalCaching(cache);
                var captured = IntegerEncoding.EncodeSignedInteger(index);
                var partial = Apply(interpreter, entry, captured);
                var plan = ExpressionTemplatePlan.TryParseTemplate(partial, cache)!;
                PineValueInProcess[] arguments = [PineValueInProcess.EmptyBlob];
                plan.TemplateValue.Should().BeSameAs(partial);

                plan.BuildTerminalEnvironment(arguments).Evaluate().Should().Be(
                    Apply(interpreter, partial, PineValue.EmptyBlob));
            });

        cache.TemplateConstructorProofCount.Should().Be(1);
    }

    [Fact]
    public void Warm_proof_hits_still_observe_cancellation_without_publishing_new_results()
    {
        var cache = new PineVMParseCache();
        var entry = FunctionValueBuilder.TryBuildCurriedFunctionValueAsTemplate(Expression.EnvironmentInstance, 2)!;
        ExpressionTemplatePlan.TryParseTemplate(entry, cache).Should().NotBeNull();
        var code = ExpressionEncoding.EncodeExpressionAsValue(Expression.EnvironmentInstance);
        cache.TemplateProofs.CheckValue(code, new ExpressionTemplatePlan.RecognitionBudget()).Should().BeTrue();
        var constructors = cache.TemplateConstructorProofCount;
        var values = cache.TemplateBoundedValueProofCount;
        using var cancellation = new CancellationTokenSource();
        cancellation.Cancel();
        using var profile = new PineVMWorkProfile(referenceCapacity: 0);
        using var scope = profile.Enter(cancellation.Token);
        // The existing work-profile checkpoint observes cancellation every 4,096 visits.
        Action recognize =
            () =>
            {
                for (var index = 0; index < 4_096; ++index)
                    ExpressionTemplatePlan.TryParseTemplate(entry, cache);
            };

        recognize.Should().Throw<OperationCanceledException>();

        Action check =
            () =>
            {
                for (var index = 0; index < 4_096; ++index)
                    cache.TemplateProofs.CheckValue(code, new ExpressionTemplatePlan.RecognitionBudget());
            };

        check.Should().Throw<OperationCanceledException>();
        cache.TemplateConstructorProofCount.Should().Be(constructors);
        cache.TemplateBoundedValueProofCount.Should().Be(values);
    }

    private static PineValue Apply(DirectInterpreter interpreter, PineValue code, PineValue environment) =>
        interpreter.EvaluateExpressionDefault(
            ExpressionEncoding.ParseExpressionFromValue(code).IsOkOrNull()!,
            environment);
}
