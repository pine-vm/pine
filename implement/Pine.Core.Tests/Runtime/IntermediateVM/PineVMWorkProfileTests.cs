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
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Reflection;
using System.Text.Json;
using System.Threading;
using Xunit;

using ProfiledVM = Pine.Core.Runtime.IntermediateVM.PineVM;

namespace Pine.Core.Tests.Runtime.IntermediateVM;

public class PineVMWorkProfileTests
{
    // These tests measure the unoptimized observer and its counterfactual sharing estimates.
    private static (PineValueInProcess value, EvalWorkCounters evalWorkCounters)? BaselineDirect(
        Expression expression, PineValueInProcess environment) =>
        ProfiledVM.EvalDirect(expression, environment, preserveConstructorSharing: false);

    private static (Expression Expression, PineValueInProcess Environment) SharedConstructor()
    {
        var environment = PineValueInProcess.Create(PineValue.EmptyBlob);
        var literal = Expression.LitralInst(PineValue.Blob([40, 41]));
        var shared = Expression.ListInst([literal, Expression.EnvironmentInstance]);
        return (Expression.ListInst([shared, shared]), environment);
    }

    [Fact]
    public void Work_profile_exact_direct_constructor_and_materialization_counts()
    {
        var input = SharedConstructor();
        using var profile = new PineVMWorkProfile();
        PineValue actual;

        using (profile.Enter())
        {
            actual = BaselineDirect(input.Expression, input.Environment)!.Value.value.Evaluate();
        }

        profile[PineVMWorkProfile.Metric.EvalDirectCalls].Should().Be(1);
        profile[PineVMWorkProfile.Metric.EvalDirectEntries].Should().Be(7);
        profile[PineVMWorkProfile.Metric.EvalDirectRecursiveEntries].Should().Be(6);
        profile[PineVMWorkProfile.Metric.EvalDirectListArrays].Should().Be(3);
        profile[PineVMWorkProfile.Metric.EvalDirectListItems].Should().Be(6);
        profile[PineVMWorkProfile.Metric.LiteralWrapperCalls].Should().Be(2);
        profile[PineVMWorkProfile.Metric.ConstructorDistinctReferences].Should().Be(4);
        profile[PineVMWorkProfile.Metric.ConstructorRepeatedReferences].Should().Be(3);
        profile[PineVMWorkProfile.Metric.ConstructorRepeatedLists].Should().Be(1);
        profile[PineVMWorkProfile.Metric.ConstructorRepeatedListItems].Should().Be(2);
        profile[PineVMWorkProfile.Metric.ConstructorRepeatFrontiers].Should().Be(1);
        profile[PineVMWorkProfile.Metric.ConstructorFrontierAvoidableListArrays].Should().Be(1);
        profile[PineVMWorkProfile.Metric.ConstructorFrontierAvoidableListItems].Should().Be(2);
        profile[PineVMWorkProfile.Metric.ConstructorFrontierAvoidableWrappers].Should().Be(2);
        profile[PineVMWorkProfile.Metric.ConstructorFrontierAvoidableLiteralWrappers].Should().Be(1);
        profile[PineVMWorkProfile.Metric.ConstructorFrontierAvoidableListWrappers].Should().Be(1);
        profile[PineVMWorkProfile.Metric.ConstructorFrontierAvoidableNodeEntries].Should().Be(2);
        profile[PineVMWorkProfile.Metric.ConstructorCompletedRepeatFrontiers].Should().Be(1);
        profile[PineVMWorkProfile.Metric.ConstructorCompletedFrontierAvoidableNodeEntries].Should().Be(2);
        profile[PineVMWorkProfile.Metric.ConstructorCompletedFrontierAvoidableListArrays].Should().Be(1);
        profile[PineVMWorkProfile.Metric.ConstructorCompletedFrontierAvoidableListItems].Should().Be(2);
        profile[PineVMWorkProfile.Metric.ConstructorCompletedFrontierAvoidableWrappers].Should().Be(2);
        profile[PineVMWorkProfile.Metric.ConstructorCompletedFrontierAvoidableLiteralWrappers].Should().Be(1);
        profile[PineVMWorkProfile.Metric.ConstructorCompletedFrontierAvoidableListWrappers].Should().Be(1);
        profile[PineVMWorkProfile.Metric.CreateCalls].Should().Be(2);
        profile[PineVMWorkProfile.Metric.WrapperAllocations].Should().Be(5);
        profile[PineVMWorkProfile.Metric.EvaluateCalls].Should().Be(7);
        profile[PineVMWorkProfile.Metric.EvaluateCached].Should().Be(4);
        profile[PineVMWorkProfile.Metric.EvaluateUncached].Should().Be(3);
        profile[PineVMWorkProfile.Metric.ConcreteChildArrays].Should().Be(3);
        profile[PineVMWorkProfile.Metric.MaterializedListOther].Should().Be(3);
        profile[PineVMWorkProfile.Metric.ListValueConstructorAllocations].Should().Be(3);
        profile.KnownAllocationObjects.Should().Be(14);
        BaselineDirect(input.Expression, input.Environment)!.Value.value.Evaluate().Should().Be(actual);
        PineVMWorkProfile.Current.Should().BeNull();
    }

    [Fact]
    public void Work_profile_reference_cap_and_numeric_snapshot_do_not_capture_values()
    {
        var input = SharedConstructor();
        using var output = new StringWriter();
        using var profile = new PineVMWorkProfile(output, referenceCapacity: 1);

        using (profile.Enter())
            BaselineDirect(input.Expression, input.Environment).Should().NotBeNull();

        profile.WriteSnapshot("tested");
        using var json = JsonDocument.Parse(output.ToString().Split('\n', StringSplitOptions.RemoveEmptyEntries)[^1]);
        json.RootElement.GetProperty("ConstructorCoverageComplete").GetBoolean().Should().BeFalse();
        profile[PineVMWorkProfile.Metric.TrackingReferences].Should().Be(1);
        profile[PineVMWorkProfile.Metric.ConstructorCoverageCapEvents].Should().Be(6);
        output.ToString().Should().NotContain("BlobBase64").And.NotContain("\"Values\":");

        json.RootElement.GetProperty("Counters").EnumerateObject().Should().OnlyContain(
            item => item.Value.ValueKind == JsonValueKind.Number);
    }

    [Fact]
    public void Work_profile_snapshot_clock_anchors_bracket_snapshot_time()
    {
        using var output = new StringWriter();
        using var profile = new PineVMWorkProfile(output);
        var timestampBefore = Stopwatch.GetTimestamp();
        var unixMillisecondsBefore = DateTimeOffset.UtcNow.ToUnixTimeMilliseconds();
        profile.WriteSnapshot("clock-anchors");
        var unixMillisecondsAfter = DateTimeOffset.UtcNow.ToUnixTimeMilliseconds();
        var timestampAfter = Stopwatch.GetTimestamp();
        using var json = JsonDocument.Parse(output.ToString().Split('\n', StringSplitOptions.RemoveEmptyEntries)[^1]);

        json.RootElement.GetProperty("StopwatchTimestamp").GetInt64().Should().BeInRange(
            timestampBefore,
            timestampAfter);

        json.RootElement.GetProperty("StopwatchFrequency").GetInt64().Should().Be(Stopwatch.Frequency);

        json.RootElement.GetProperty("SnapshotUnixTimeMilliseconds").GetInt64().Should().BeInRange(
            unixMillisecondsBefore,
            unixMillisecondsAfter);

        json.RootElement.GetProperty("UnixTimestampMilliseconds").GetInt64().Should().Be(
            json.RootElement.GetProperty("SnapshotUnixTimeMilliseconds").GetInt64());

        json.RootElement.GetProperty("ConstructorInvocationsPending").GetInt64().Should().Be(0);
    }

    [Fact]
    public void Work_profile_nested_scopes_restore_on_error_and_cancellation()
    {
        var input = SharedConstructor();
        using var outer = new PineVMWorkProfile();
        using var inner = new PineVMWorkProfile();
        using var cancelled = new CancellationTokenSource();
        cancelled.Cancel();

        using (outer.Enter())
        {
            using (inner.Enter(cancelled.Token))
            {
                Action action = () => BaselineDirect(input.Expression, input.Environment);
                action.Should().Throw<OperationCanceledException>();
            }

            PineVMWorkProfile.Current.Should().BeSameAs(outer);

            try
            {
                using var scope = inner.Enter();
                throw new InvalidOperationException("test");
            }
            catch (InvalidOperationException) { }

            PineVMWorkProfile.Current.Should().BeSameAs(outer);
            PineValueInProcess.Create(PineValue.EmptyList);
        }

        PineVMWorkProfile.Current.Should().BeNull();
        outer[PineVMWorkProfile.Metric.CreateReuses].Should().Be(1);
        inner[PineVMWorkProfile.Metric.CreateReuses].Should().Be(0);
    }

    [Fact]
    public void Work_profile_deferred_partial_materialization_and_recognizer_budget()
    {
        _ = PineValueInProcess.EmptyList;
        using var profile = new PineVMWorkProfile();

        using (profile.Enter())
        {
            var template =
                FunctionValueBuilder.TryBuildCurriedFunctionValueAsTemplate(
                Expression.EnvironmentInstance, 3)!;

            var plan = ExpressionTemplatePlan.TryParseTemplate(template, new PineVMParseCache())!;
            var environments = CollectedTemplateEnvironments.Empty.Append(PineValueInProcess.EmptyBlob);

            var value =
                PineValueInProcess.CreateDeferredTemplateValue(
                    plan,
                    environments,
                    collected => plan.Materialize(collected, new DirectInterpreterCounters()));

            value.EvaluatedOrNull.Should().BeNull();
            value.Evaluate().Should().Be(plan.Materialize(environments, new DirectInterpreterCounters()));
            value.Evaluate();
            profile[PineVMWorkProfile.Metric.DeferredValuesCreated].Should().Be(1);
            profile[PineVMWorkProfile.Metric.DeferredMaterializations].Should().Be(1);
            profile[PineVMWorkProfile.Metric.DeferredMaterializedEnvironmentSlots].Should().Be(1);
            profile[PineVMWorkProfile.Metric.RecognizerVisits].Should().BeGreaterThan(0);
            var budget = new ExpressionTemplatePlan.RecognitionBudget();
            budget.Visit(129).Should().BeFalse();
            budget.Visit(130).Should().BeFalse();
            profile[PineVMWorkProfile.Metric.RecognizerBudgetFailures].Should().Be(1);
        }
    }

    [Fact]
    public void Work_profile_template_cache_positive_negative_and_warm_hits()
    {
        using var profile = new PineVMWorkProfile();

        var vm =
            Elm.ElmCompilerInDotnet.ElmCompilerTestHelper.PineVMForProfiling(
                _ => { },
                workProfile: profile);

        var template = FunctionValueBuilder.TryBuildCurriedFunctionValueAsTemplate(Expression.EnvironmentInstance, 3)!;
        var apply = new Expression.Eval(Expression.EnvironmentInstance, Expression.LitralInst(PineValue.EmptyBlob));
        var config = ProfiledVM.EvaluationConfig.Default with { MaterializeResult = true };
        vm.EvaluateExpressionOnCustomStack(apply, template, config).IsOkOrNull().Should().NotBeNull();
        var factories = profile[PineVMWorkProfile.Metric.TemplateCacheFactoryRuns];
        factories.Should().BeGreaterThan(0);
        vm.EvaluateExpressionOnCustomStack(apply, template, config).IsOkOrNull().Should().NotBeNull();
        profile[PineVMWorkProfile.Metric.TemplateCacheFactoryRuns].Should().Be(factories);
        profile[PineVMWorkProfile.Metric.TemplateCacheHits].Should().BeGreaterThan(0);

        var ordinary =
            ExpressionEncoding.EncodeExpressionAsValue(
                new Expression.Builtin(nameof(BuiltinFunction.int_add), Expression.EnvironmentInstance));

        vm.EvaluateExpressionOnCustomStack(apply, ordinary, config).IsOkOrNull().Should().NotBeNull();
        vm.EvaluateExpressionOnCustomStack(apply, ordinary, config).IsOkOrNull().Should().NotBeNull();
        profile[PineVMWorkProfile.Metric.TemplateCacheNullResults].Should().BeGreaterThan(0);
        profile[PineVMWorkProfile.Metric.RecognizerStages].Should().BeGreaterThan(0);
        profile[PineVMWorkProfile.Metric.RecognizerConstructorStages].Should().BeGreaterThan(0);
        profile[PineVMWorkProfile.Metric.RecognizerMalformedOrUnsupportedFailures].Should().BeGreaterThan(0);

        profile[PineVMWorkProfile.Metric.TemplateCacheProbes].Should().Be(
            profile[PineVMWorkProfile.Metric.TemplateCacheHits] + profile[PineVMWorkProfile.Metric.TemplateCacheMisses]);

        PineVMWorkProfile.Current.Should().BeNull();
    }

    [Fact]
    public void Work_profile_explicit_memory_guard_flushes_stop_and_restores_scope()
    {
        var input = SharedConstructor();
        using var output = new StringWriter();
        using var profile = new PineVMWorkProfile(output, workingSetLimitBytes: 1);

        Action action =
            () =>
            {
                using var scope = profile.Enter();
                BaselineDirect(input.Expression, input.Environment);
            };

        action.Should().Throw<OperationCanceledException>();
        output.ToString().Should().Contain("\"Outcome\":\"working-set-limit\"");
        PineVMWorkProfile.Current.Should().BeNull();
    }

    [Fact]
    public void Work_profile_uninstrumented_nested_vm_and_errors_do_not_record_or_leak()
    {
        var vm = Elm.ElmCompilerInDotnet.ElmCompilerTestHelper.PineVMForProfiling(_ => { });
        using var profile = new PineVMWorkProfile();

        using (profile.Enter())
        {
            vm.EvaluateExpression(Expression.EnvironmentInstance, PineValue.EmptyBlob).IsOkOrNull().Should().NotBeNull();

            vm.EvaluateExpression(
                new Expression.Eval(Expression.EnvironmentInstance, Expression.EnvironmentInstance),
                PineValue.EmptyBlob).IsErrOrNull().Should().NotBeNull();

            PineVMWorkProfile.Current.Should().BeSameAs(profile);
            profile[PineVMWorkProfile.Metric.WrapperAllocations].Should().Be(0);
        }

        PineVMWorkProfile.Current.Should().BeNull();
    }

    [Fact]
    public void Work_profile_direct_budget_stops_inside_constructor_at_exact_entry_and_flushes()
    {
        var input = SharedConstructor();
        var expression = input.Expression;

        for (var depth = 0; depth < 16; ++depth)
            expression = Expression.ListInst([expression, expression]);

        using var output = new StringWriter();
        using var profile = new PineVMWorkProfile(output, directVisitBudget: 1024);
        profile.SetScenario("preparation", "large-constructor");

        Action action =
            () =>
            {
                using var scope = profile.Enter();
                BaselineDirect(expression, input.Environment);
            };

        action.Should().Throw<OperationCanceledException>();
        profile[PineVMWorkProfile.Metric.EvalDirectEntries].Should().Be(1024);
        profile[PineVMWorkProfile.Metric.EvalDirectRecursiveEntries].Should().Be(1023);
        profile[PineVMWorkProfile.Metric.ConstructorInvocationsIncomplete].Should().Be(1);
        profile[PineVMWorkProfile.Metric.ConstructorCompletedFrontierAvoidableNodeEntries].Should().Be(0);
        profile[PineVMWorkProfile.Metric.ConstructorCompletedFrontierAvoidableWrappers].Should().Be(0);
        using var last = JsonDocument.Parse(output.ToString().Split('\n', StringSplitOptions.RemoveEmptyEntries)[^1]);
        last.RootElement.GetProperty("Outcome").GetString().Should().Be("direct-visit-limit");
        last.RootElement.GetProperty("Phase").GetString().Should().Be("preparation");
        last.RootElement.GetProperty("Scenario").GetString().Should().Be("large-constructor");
        last.RootElement.GetProperty("Counters").GetProperty("EvalDirectEntries").GetInt64().Should().Be(1024);
        last.RootElement.GetProperty("ConstructorCounterfactualCoverageComplete").GetBoolean().Should().BeFalse();
        last.RootElement.GetProperty("ConstructorInvocationsPending").GetInt64().Should().Be(1);
        profile.WriteSnapshot("unwound");

        using var unwound =
            JsonDocument.Parse(output.ToString().Split('\n', StringSplitOptions.RemoveEmptyEntries)[^1]);

        unwound.RootElement.GetProperty("ConstructorInvocationsPending").GetInt64().Should().Be(0);
        PineVMWorkProfile.Current.Should().BeNull();
    }

    [Fact]
    public void Work_profile_frontier_estimate_prunes_overlapping_descendant_repeats()
    {
        var environment = PineValueInProcess.EmptyBlob;
        var literal = Expression.LitralInst(PineValue.Blob([40, 41]));
        var inner = Expression.ListInst([literal]);
        var outer = Expression.ListInst([inner, inner]);
        var expression = Expression.ListInst([outer, outer]);
        using var output = new StringWriter();
        using var profile = new PineVMWorkProfile(output);

        using (profile.Enter())
            BaselineDirect(expression, environment).Should().NotBeNull();

        profile.WriteSnapshot("tested");
        profile[PineVMWorkProfile.Metric.ConstructorRepeatedReferences].Should().Be(7);
        profile[PineVMWorkProfile.Metric.ConstructorRepeatFrontiers].Should().Be(2);
        profile[PineVMWorkProfile.Metric.ConstructorFrontierAvoidableListArrays].Should().Be(4);
        profile[PineVMWorkProfile.Metric.ConstructorFrontierAvoidableListItems].Should().Be(5);
        profile[PineVMWorkProfile.Metric.ConstructorFrontierAvoidableWrappers].Should().Be(7);
        profile[PineVMWorkProfile.Metric.ConstructorFrontierAvoidableLiteralWrappers].Should().Be(3);
        profile[PineVMWorkProfile.Metric.ConstructorFrontierAvoidableListWrappers].Should().Be(4);
        profile[PineVMWorkProfile.Metric.ConstructorCompletedFrontierAvoidableListWrappers].Should().Be(4);
        profile[PineVMWorkProfile.Metric.ConstructorFrontierAvoidableNodeEntries].Should().Be(5);
        profile[PineVMWorkProfile.Metric.ConstructorCompletedFrontierAvoidableNodeEntries].Should().Be(5);
        using var last = JsonDocument.Parse(output.ToString().Split('\n', StringSplitOptions.RemoveEmptyEntries)[^1]);
        last.RootElement.GetProperty("ConstructorCounterfactualCoverageComplete").GetBoolean().Should().BeTrue();
    }

    [Fact]
    public void Work_profile_disabled_scopes_are_allocation_free_and_mask_outer_sink()
    {
        using var outer = new PineVMWorkProfile();

        using (outer.Enter())
        {
            using (PineVMWorkProfile.EnterEvaluation(null, default, null))
            {
            }

            var before = GC.GetAllocatedBytesForCurrentThread();

            for (var iteration = 0; iteration < 10000; ++iteration)
            {
                using var disabled = PineVMWorkProfile.EnterEvaluation(null, default, null);

                if (PineVMWorkProfile.Current is not null)
                    throw new InvalidOperationException("Disabled scope failed to mask the outer sink.");
            }

            var allocated = GC.GetAllocatedBytesForCurrentThread() - before;
            allocated.Should().Be(0);
            PineVMWorkProfile.Current.Should().BeSameAs(outer);
        }

        PineVMWorkProfile.Current.Should().BeNull();
    }

    [Fact]
    public void Work_profile_disabled_direct_entry_has_no_heavy_diagnostic_locals()
    {
        var direct =
            typeof(ProfiledVM).GetMethod(
            "EvalDirect", BindingFlags.NonPublic | BindingFlags.Static, null,
            [typeof(Expression), typeof(PineValueInProcess)], null)!;

        direct.GetMethodBody()!.LocalVariables.Should().NotContain(
            local =>
            local.LocalType == typeof(PineVMWorkProfile.ConstructorOpportunities) ||
            local.LocalType == typeof(PineVMWorkProfile.ConstructorBoundarySnapshot));

        var profiled =
            typeof(ProfiledVM).GetMethod(
            "EvalDirectProfiled", BindingFlags.NonPublic | BindingFlags.Static)!;

        profiled.MethodImplementationFlags.HasFlag(MethodImplAttributes.NoInlining).Should().BeTrue();
    }

    [Theory]
    [InlineData(0)]
    [InlineData(1)]
    [InlineData(4096)]
    public void Work_profile_preserves_direct_results_and_resource_counters(int capacity)
    {
        var input = SharedConstructor();

        var expressions =
            new Expression[]
            {
                input.Expression,
                new Expression.Label("resource-counts", input.Expression),
                new Expression.Conditional(
                    Expression.LitralInst(PineKernelValues.TrueValue),
                    input.Expression,
                    Expression.EnvironmentInstance),
                new Expression.Builtin(nameof(BuiltinFunction.length), input.Expression),
                new Expression.Eval(Expression.EnvironmentInstance, Expression.EnvironmentInstance)
            };

        using var profile = new PineVMWorkProfile(referenceCapacity: capacity);

        foreach (var expression in expressions)
        {
            var baseline = BaselineDirect(expression, input.Environment);
            (PineValueInProcess value, EvalWorkCounters evalWorkCounters)? actual;

            using (profile.Enter())
                actual = BaselineDirect(expression, input.Environment);

            actual.HasValue.Should().Be(baseline.HasValue);

            if (actual is { } completed && baseline is { } expected)
            {
                completed.evalWorkCounters.Should().Be(expected.evalWorkCounters);
                completed.value.Evaluate().Should().Be(expected.value.Evaluate());
            }
        }

        var expectedUnprofiled = BaselineDirect(input.Expression, input.Environment)!.Value.evalWorkCounters;
        var calls = profile[PineVMWorkProfile.Metric.EvalDirectCalls];

        using (profile.Enter())
        {
            using (PineVMWorkProfile.EnterEvaluation(null, default, null))
            {
                BaselineDirect(input.Expression, input.Environment)!.Value.evalWorkCounters.Should().Be(
                    expectedUnprofiled);
            }
        }

        profile[PineVMWorkProfile.Metric.EvalDirectCalls].Should().Be(calls);
        PineVMWorkProfile.Current.Should().BeNull();
    }

    [Fact]
    public void Work_profile_zero_reference_capacity_disables_hashsets_not_work_counters()
    {
        var input = SharedConstructor();
        using var output = new StringWriter();
        using var profile = new PineVMWorkProfile(output, referenceCapacity: 0);

        using (profile.Enter())
            BaselineDirect(input.Expression, input.Environment).Should().NotBeNull();

        profile.WriteSnapshot("tested");
        profile[PineVMWorkProfile.Metric.EvalDirectEntries].Should().Be(7);
        profile[PineVMWorkProfile.Metric.WrapperAllocations].Should().Be(5);
        profile[PineVMWorkProfile.Metric.ConstructorEligibleInvocations].Should().Be(1);
        profile[PineVMWorkProfile.Metric.ConstructorInvocationsStarted].Should().Be(1);
        profile[PineVMWorkProfile.Metric.ConstructorInvocationsCompleted].Should().Be(1);
        profile[PineVMWorkProfile.Metric.ConstructorInvocationsIncomplete].Should().Be(0);
        profile[PineVMWorkProfile.Metric.TrackingSets].Should().Be(0);
        profile[PineVMWorkProfile.Metric.TrackingReferences].Should().Be(0);
        profile[PineVMWorkProfile.Metric.TrackingAllocatedBytes].Should().Be(0);
        profile[PineVMWorkProfile.Metric.ConstructorFrontierAvoidableNodeEntries].Should().Be(0);
        profile[PineVMWorkProfile.Metric.ConstructorCompletedFrontierAvoidableWrappers].Should().Be(0);
        using var last = JsonDocument.Parse(output.ToString().Split('\n', StringSplitOptions.RemoveEmptyEntries)[^1]);
        last.RootElement.GetProperty("ConstructorReferenceTrackingEnabled").GetBoolean().Should().BeFalse();
        last.RootElement.GetProperty("ConstructorCoverageComplete").GetBoolean().Should().BeFalse();
        last.RootElement.GetProperty("ConstructorCounterfactualCoverageComplete").GetBoolean().Should().BeFalse();
    }

    [Fact]
    public void Work_profile_existing_recipe_instantiation_counts_memo_hits_and_arrays()
    {
        var environment = PineValueInProcess.EmptyBlob;
        var slot = TemplateValueRecipe.EnvironmentSlot(0);
        var shared = new TemplateValueRecipe.List([slot]);
        var root = new TemplateValueRecipe.List([shared, shared]);
        using var profile = new PineVMWorkProfile();

        using (profile.Enter())
        {
            var value = root.Instantiate([environment]);
            var items = value.ListItemsOrNull()!;
            ReferenceEquals(items[0], items[1]).Should().BeTrue();
        }

        profile[PineVMWorkProfile.Metric.RecipeInstantiateCalls].Should().Be(2);
        profile[PineVMWorkProfile.Metric.RecipeInstantiateSharedCalls].Should().Be(4);
        profile[PineVMWorkProfile.Metric.RecipeInstantiateSharedMemoHits].Should().Be(1);
        profile[PineVMWorkProfile.Metric.RecipeInstantiateSharedChildArrays].Should().Be(2);
        profile[PineVMWorkProfile.Metric.RecipeInstantiateSharedChildItems].Should().Be(3);
        profile[PineVMWorkProfile.Metric.WrapperAllocations].Should().Be(2);
        profile.KnownAllocationObjects.Should().Be(4);
    }

    [Fact]
    public void Work_profile_counterfactual_does_not_count_reused_literal_wrappers_as_allocations()
    {
        var environment = PineValueInProcess.EmptyBlob;
        var literal = Expression.LitralInst(PineValue.EmptyBlob);
        var expression = Expression.ListInst([literal, literal]);
        using var profile = new PineVMWorkProfile();

        using (profile.Enter())
            BaselineDirect(expression, environment).Should().NotBeNull();

        profile[PineVMWorkProfile.Metric.ConstructorRepeatFrontiers].Should().Be(1);
        profile[PineVMWorkProfile.Metric.CreateReuses].Should().Be(2);
        profile[PineVMWorkProfile.Metric.WrapperAllocations].Should().Be(1);
        profile[PineVMWorkProfile.Metric.ConstructorFrontierAvoidableWrappers].Should().Be(0);
        profile[PineVMWorkProfile.Metric.ConstructorFrontierAvoidableLiteralWrappers].Should().Be(0);
    }

    [Fact]
    public void Work_profile_constructor_frontiers_reset_for_each_environment()
    {
        var input = SharedConstructor();
        var secondEnvironment = PineValueInProcess.Create(PineValue.Blob([42, 43]));
        using var profile = new PineVMWorkProfile();

        using (profile.Enter())
        {
            foreach (var environment in new[] { input.Environment, secondEnvironment })
            {
                var result = BaselineDirect(input.Expression, environment)!.Value.value;
                var branches = result.ListItemsOrNull()!;
                branches[0].ListItemsOrNull()![1].Should().BeSameAs(environment);
                branches[1].ListItemsOrNull()![1].Should().BeSameAs(environment);
            }
        }

        profile[PineVMWorkProfile.Metric.ConstructorInvocationsStarted].Should().Be(2);
        profile[PineVMWorkProfile.Metric.ConstructorInvocationsCompleted].Should().Be(2);
        profile[PineVMWorkProfile.Metric.ConstructorDistinctReferences].Should().Be(8);
        profile[PineVMWorkProfile.Metric.ConstructorRepeatFrontiers].Should().Be(2);
        profile[PineVMWorkProfile.Metric.ConstructorFrontierAvoidableListArrays].Should().Be(2);
        profile[PineVMWorkProfile.Metric.ConstructorFrontierAvoidableListItems].Should().Be(4);
        profile[PineVMWorkProfile.Metric.ConstructorFrontierAvoidableWrappers].Should().Be(4);
    }

    [Fact]
    public void Work_profile_avoidable_descendant_entries_include_labels_not_frontier_roots()
    {
        var labeled = new Expression.Label("constructor", Expression.EnvironmentInstance);
        var shared = Expression.ListInst([labeled]);
        var expression = Expression.ListInst([shared, shared]);
        using var profile = new PineVMWorkProfile();

        using (profile.Enter())
            BaselineDirect(expression, PineValueInProcess.EmptyBlob).Should().NotBeNull();

        profile[PineVMWorkProfile.Metric.ConstructorRepeatedReferences].Should().Be(2);
        profile[PineVMWorkProfile.Metric.ConstructorRepeatFrontiers].Should().Be(1);
        profile[PineVMWorkProfile.Metric.ConstructorFrontierAvoidableNodeEntries].Should().Be(2);
        profile[PineVMWorkProfile.Metric.ConstructorCompletedFrontierAvoidableNodeEntries].Should().Be(2);
    }

    [Fact]
    public void Work_profile_completed_opportunities_exclude_interrupted_invocation()
    {
        var input = SharedConstructor();
        using var profile = new PineVMWorkProfile(directVisitBudget: 13);

        using (profile.Enter())
        {
            BaselineDirect(input.Expression, input.Environment).Should().NotBeNull();
            Action interrupted = () => BaselineDirect(input.Expression, input.Environment);
            interrupted.Should().Throw<OperationCanceledException>();
        }

        profile[PineVMWorkProfile.Metric.ConstructorInvocationsStarted].Should().Be(2);
        profile[PineVMWorkProfile.Metric.ConstructorInvocationsCompleted].Should().Be(1);
        profile[PineVMWorkProfile.Metric.ConstructorInvocationsIncomplete].Should().Be(1);
        profile[PineVMWorkProfile.Metric.ConstructorFrontierAvoidableNodeEntries].Should().Be(3);
        profile[PineVMWorkProfile.Metric.ConstructorFrontierAvoidableListArrays].Should().Be(2);
        profile[PineVMWorkProfile.Metric.ConstructorFrontierAvoidableWrappers].Should().Be(3);
        profile[PineVMWorkProfile.Metric.ConstructorFrontierAvoidableListWrappers].Should().Be(1);
        profile[PineVMWorkProfile.Metric.ConstructorCompletedRepeatFrontiers].Should().Be(1);
        profile[PineVMWorkProfile.Metric.ConstructorCompletedFrontierAvoidableNodeEntries].Should().Be(2);
        profile[PineVMWorkProfile.Metric.ConstructorCompletedFrontierAvoidableListArrays].Should().Be(1);
        profile[PineVMWorkProfile.Metric.ConstructorCompletedFrontierAvoidableListItems].Should().Be(2);
        profile[PineVMWorkProfile.Metric.ConstructorCompletedFrontierAvoidableWrappers].Should().Be(2);
        profile[PineVMWorkProfile.Metric.ConstructorCompletedFrontierAvoidableLiteralWrappers].Should().Be(1);
        profile[PineVMWorkProfile.Metric.ConstructorCompletedFrontierAvoidableListWrappers].Should().Be(1);
    }

    [Fact]
    public void Work_profile_zero_reference_capacity_preserves_interrupted_invocation_accounting()
    {
        var input = SharedConstructor();
        using var profile = new PineVMWorkProfile(referenceCapacity: 0, directVisitBudget: 4);

        using (profile.Enter())
        {
            Action interrupted = () => BaselineDirect(input.Expression, input.Environment);
            interrupted.Should().Throw<OperationCanceledException>();
        }

        profile[PineVMWorkProfile.Metric.ConstructorInvocationsStarted].Should().Be(1);
        profile[PineVMWorkProfile.Metric.ConstructorInvocationsCompleted].Should().Be(0);
        profile[PineVMWorkProfile.Metric.ConstructorInvocationsIncomplete].Should().Be(1);
        profile[PineVMWorkProfile.Metric.EvalDirectEntries].Should().Be(4);
        profile[PineVMWorkProfile.Metric.TrackingSets].Should().Be(0);
        profile[PineVMWorkProfile.Metric.TrackingReferences].Should().Be(0);
        profile[PineVMWorkProfile.Metric.TrackingAllocatedBytes].Should().Be(0);
        profile[PineVMWorkProfile.Metric.ConstructorFrontierAvoidableNodeEntries].Should().Be(0);
        profile[PineVMWorkProfile.Metric.ConstructorFrontierAvoidableListWrappers].Should().Be(0);
        profile[PineVMWorkProfile.Metric.ConstructorCompletedFrontierAvoidableWrappers].Should().Be(0);
    }

    [Fact]
    public void Work_profile_frontier_list_wrapper_allocations_exclude_empty_list_reuse()
    {
        var empty = Expression.ListInst([]);
        var expression = Expression.ListInst([empty, empty]);
        using var profile = new PineVMWorkProfile();

        using (profile.Enter())
            BaselineDirect(expression, PineValueInProcess.EmptyBlob).Should().NotBeNull();

        profile[PineVMWorkProfile.Metric.ConstructorRepeatFrontiers].Should().Be(1);
        profile[PineVMWorkProfile.Metric.ConstructorFrontierAvoidableListArrays].Should().Be(1);
        profile[PineVMWorkProfile.Metric.ConstructorFrontierAvoidableListWrappers].Should().Be(0);
        profile[PineVMWorkProfile.Metric.ConstructorCompletedFrontierAvoidableListWrappers].Should().Be(0);
        profile[PineVMWorkProfile.Metric.CreateListReuses].Should().Be(2);
        profile.KnownAllocationObjects.Should().Be(4);
    }

    [Fact]
    public void Work_profile_reference_census_does_not_change_covered_allocation_objects()
    {
        var input = SharedConstructor();
        using var tracked = new PineVMWorkProfile();
        using var untracked = new PineVMWorkProfile(referenceCapacity: 0);
        PineValue expected;

        using (tracked.Enter())
            expected = BaselineDirect(input.Expression, input.Environment)!.Value.value.Evaluate();

        using (untracked.Enter())
            BaselineDirect(input.Expression, input.Environment)!.Value.value.Evaluate().Should().Be(expected);

        tracked.KnownAllocationObjects.Should().Be(14);
        untracked.KnownAllocationObjects.Should().Be(tracked.KnownAllocationObjects);

        untracked[PineVMWorkProfile.Metric.ConstructorInvocationsStarted].Should().Be(
            tracked[PineVMWorkProfile.Metric.ConstructorInvocationsStarted]);

        untracked[PineVMWorkProfile.Metric.ConstructorInvocationsCompleted].Should().Be(
            tracked[PineVMWorkProfile.Metric.ConstructorInvocationsCompleted]);
    }

    [Fact]
    public void Work_profile_completed_opportunities_restore_nested_invocation_accumulator()
    {
        var input = SharedConstructor();
        using var output = new StringWriter();
        using var profile = new PineVMWorkProfile(output);
        PineVMWorkProfile.ConstructorBoundarySnapshot inner;

        using (profile.Enter())
        {
            var previous = profile.BeginConstructorInvocation();
            profile.BeginConstructorBody();
            profile.CountConstructorOpportunity(PineVMWorkProfile.Metric.ConstructorFrontierAvoidableNodeEntries, 10);
            profile.CountConstructorOpportunity(PineVMWorkProfile.Metric.ConstructorFrontierAvoidableListArrays, 3);
            BaselineDirect(input.Expression, input.Environment).Should().NotBeNull();
            inner = profile.LastConstructorInvocation!.Value;
            profile.EndConstructorBody();
            profile.EndConstructorInvocation(previous, completed: true);
        }

        profile[PineVMWorkProfile.Metric.ConstructorInvocationsCompleted].Should().Be(2);
        profile[PineVMWorkProfile.Metric.ConstructorFrontierAvoidableNodeEntries].Should().Be(12);
        profile[PineVMWorkProfile.Metric.ConstructorCompletedFrontierAvoidableNodeEntries].Should().Be(12);
        profile[PineVMWorkProfile.Metric.ConstructorFrontierAvoidableListArrays].Should().Be(4);
        profile[PineVMWorkProfile.Metric.ConstructorCompletedFrontierAvoidableListArrays].Should().Be(4);
        profile.LastConstructorInvocation!.Value.NoNestedConstructorInvocations.Should().BeFalse();
        profile.LastConstructorInvocation.Value.RootRetainedFreshObjects.Should().BeNull();
        profile.ConstructorCompletedPrivateCallWallMilliseconds.Should().Be(inner.WallMilliseconds);
        profile.ConstructorPrivateCallWindowCoverageComplete.Should().BeFalse();
        profile.WriteSnapshot("nested-window");
        using var json = JsonDocument.Parse(output.ToString().Split('\n', StringSplitOptions.RemoveEmptyEntries)[^1]);
        json.RootElement.GetProperty("ConstructorCompletedPrivateCallWindows").GetInt64().Should().Be(1);
        json.RootElement.GetProperty("ConstructorPrivateCallWindowsExcludedNested").GetInt64().Should().Be(1);

        json.RootElement.GetProperty("ConstructorCompletedPrivateCallGcPauseMilliseconds").GetDouble().Should().Be(
            inner.GcPauseMilliseconds);
    }

    [Fact]
    public void Work_profile_constructor_window_sums_cover_completed_and_interrupted_nonoverlapping_calls()
    {
        var input = SharedConstructor();
        using var output = new StringWriter();
        using var profile = new PineVMWorkProfile(output, directVisitBudget: 13);
        PineVMWorkProfile.ConstructorBoundarySnapshot completed;
        PineVMWorkProfile.ConstructorBoundarySnapshot incomplete;

        using (profile.Enter())
        {
            BaselineDirect(input.Expression, input.Environment).Should().NotBeNull();
            completed = profile.LastConstructorInvocation!.Value;
            profile.ConstructorPrivateCallWindowCoverageComplete.Should().BeTrue();
            Action interrupted = () => BaselineDirect(input.Expression, input.Environment);
            interrupted.Should().Throw<OperationCanceledException>();
            incomplete = profile.LastConstructorInvocation.Value;
        }

        incomplete.StartStopwatchTimestamp.Should().BeGreaterThanOrEqualTo(completed.EndStopwatchTimestamp);
        completed.PrivateCallWindowCaptured.Should().BeTrue();
        incomplete.PrivateCallWindowCaptured.Should().BeTrue();
        completed.NoNestedConstructorInvocations.Should().BeTrue();
        incomplete.NoNestedConstructorInvocations.Should().BeTrue();

        using (var pending = JsonDocument.Parse(output.ToString().Split('\n', StringSplitOptions.RemoveEmptyEntries)[^1]))
        {
            pending.RootElement.GetProperty("Outcome").GetString().Should().Be("direct-visit-limit");
            pending.RootElement.GetProperty("ConstructorCompletedPrivateCallWindows").GetInt64().Should().Be(1);
            pending.RootElement.GetProperty("ConstructorIncompletePrivateCallWindows").GetInt64().Should().Be(0);

            pending.RootElement
                .GetProperty("ConstructorPrivateCallWindowCoverageComplete").GetBoolean().Should().BeFalse();
        }

        profile.WriteSnapshot("unwound-window");
        using var json = JsonDocument.Parse(output.ToString().Split('\n', StringSplitOptions.RemoveEmptyEntries)[^1]);
        var root = json.RootElement;

        root.GetProperty("ConstructorCompletedPrivateCallStopwatchTicks").GetInt64().Should().Be(
            completed.EndStopwatchTimestamp - completed.StartStopwatchTimestamp);

        root.GetProperty("ConstructorIncompletePrivateCallStopwatchTicks").GetInt64().Should().Be(
            incomplete.EndStopwatchTimestamp - incomplete.StartStopwatchTimestamp);

        root.GetProperty("ConstructorCompletedPrivateCallWallMilliseconds").GetDouble().Should().Be(
            completed.WallMilliseconds);

        root.GetProperty("ConstructorIncompletePrivateCallWallMilliseconds").GetDouble().Should().Be(
            incomplete.WallMilliseconds);

        root.GetProperty("ConstructorCompletedPrivateCallGcPauseMilliseconds").GetDouble().Should().Be(
            completed.GcPauseMilliseconds);

        root.GetProperty("ConstructorIncompletePrivateCallGcPauseMilliseconds").GetDouble().Should().Be(
            incomplete.GcPauseMilliseconds);

        root.GetProperty("ConstructorCompletedPrivateCallWindows").GetInt64().Should().Be(1);
        root.GetProperty("ConstructorIncompletePrivateCallWindows").GetInt64().Should().Be(1);
        root.GetProperty("ConstructorPrivateCallWindowsExcludedNested").GetInt64().Should().Be(0);
        root.GetProperty("ConstructorPrivateCallWindowsExcludedMissing").GetInt64().Should().Be(0);
        root.GetProperty("ConstructorPrivateCallWindowCoverageComplete").GetBoolean().Should().BeTrue();
        root.GetProperty("ConstructorPrivateCallGcPauseWindowsAreProcessWide").GetBoolean().Should().BeTrue();
        root.GetProperty("ConstructorPrivateCallGcPauseWindowsProveCausality").GetBoolean().Should().BeFalse();
    }

    [Fact]
    public void Work_profile_constructor_window_sums_exclude_missing_private_call_windows()
    {
        using var output = new StringWriter();
        using var profile = new PineVMWorkProfile(output);
        var previous = profile.BeginConstructorInvocation();
        profile.EndConstructorInvocation(previous, completed: false);
        profile.WriteSnapshot("missing-window");
        using var json = JsonDocument.Parse(output.ToString().Split('\n', StringSplitOptions.RemoveEmptyEntries)[^1]);
        json.RootElement.GetProperty("ConstructorIncompletePrivateCallWindows").GetInt64().Should().Be(0);
        json.RootElement.GetProperty("ConstructorPrivateCallWindowsExcludedMissing").GetInt64().Should().Be(1);
        json.RootElement.GetProperty("ConstructorPrivateCallWindowCoverageComplete").GetBoolean().Should().BeFalse();
        json.RootElement.GetProperty("ConstructorIncompletePrivateCallWallMilliseconds").GetDouble().Should().Be(0);
    }

    [Fact]
    public void Work_profile_completed_constructor_boundary_proves_only_fresh_reachable_objects()
    {
        var input = SharedConstructor();
        using var profile = new PineVMWorkProfile();
        PineValueInProcess actual;

        using (profile.Enter())
            actual = BaselineDirect(input.Expression, input.Environment)!.Value.value;

        var boundary = profile.LastConstructorInvocation!.Value;
        boundary.Completed.Should().BeTrue();
        boundary.ResultAlreadyMaterialized.Should().BeFalse();
        boundary.PureListLiteralEnvironmentGrammar.Should().BeTrue();
        boundary.AllocationConservationVerified.Should().BeTrue();
        boundary.OpportunityCoverageComplete.Should().BeTrue();
        boundary.UniqueReferences.Should().Be(4);
        boundary.ReferenceCapacity.Should().Be(4096);
        boundary.RootRetainedFreshObjectsKnown.Should().BeTrue();
        boundary.PrivateCallWindowCaptured.Should().BeTrue();
        boundary.AcceptedBodyEntries.Should().Be(7);
        boundary.PrivateEvaluatorMethodInvocations.Should().Be(7);
        boundary.OuterEvalDirectCalls.Should().Be(1);
        boundary.LiteralWrapperCalls.Should().Be(2);
        boundary.ListArrays.Should().Be(3);
        boundary.ListReferenceSlots.Should().Be(6);
        boundary.FreshWrappers.Should().Be(5);
        boundary.FreshLiteralWrappers.Should().Be(2);
        boundary.FreshListWrappers.Should().Be(3);
        boundary.CreateListReuses.Should().Be(0);
        boundary.EvaluateCalls.Should().Be(0);
        boundary.RootRetainedFreshObjects.Should().Be(8);
        boundary.RootRetainedReferenceSlots.Should().Be(6);
        boundary.RootRetainedAvoidableObjects.Should().Be(3);
        boundary.RootOwnedFreshLiteralWrappers.Should().Be(2);
        boundary.RootOwnedFreshListWrappers.Should().Be(3);
        boundary.RootOwnedFreshListArrays.Should().Be(3);
        boundary.UnretainedEmptyListArrays.Should().Be(0);
        boundary.RequiredFirstUniqueWrappers.Should().Be(3);
        boundary.RequiredFirstUniqueLiteralWrappers.Should().Be(1);
        boundary.RequiredFirstUniqueListWrappers.Should().Be(2);
        boundary.RequiredFirstUniqueListArrays.Should().Be(2);
        boundary.RequiredFirstUniqueListReferenceSlots.Should().Be(4);
        boundary.RootOwnedObjectsBeforeMemo.Should().Be(8);
        boundary.CounterfactualRootOwnedObjectsAfterMemo.Should().Be(5);
        boundary.AvoidableDescendantBodyEntries.Should().Be(2);
        boundary.StartAcceptedBodyEntries.Should().Be(0);
        boundary.EndAcceptedBodyEntries.Should().Be(7);
        boundary.EndStopwatchTimestamp.Should().BeGreaterThanOrEqualTo(boundary.StartStopwatchTimestamp);
        boundary.ThreadAllocatedBytes.Should().BeGreaterThan(boundary.TrackingAllocatedBytes);

        (boundary.TrackingAllocatedBytes + boundary.SetupTrackingAllocatedBytes).Should().Be(
            profile[PineVMWorkProfile.Metric.TrackingAllocatedBytes]);

        FreshOutputObjects(actual, input.Environment).Should().Be(8);

        // Repeated AST references still produce distinct physical list/literal wrappers in the baseline.
        var branches = actual.ListItemsOrNull()!;
        branches[0].Should().NotBeSameAs(branches[1]);
        branches[0].ListItemsOrNull()![0].Should().NotBeSameAs(branches[1].ListItemsOrNull()![0]);
        branches[0].ListItemsOrNull()![1].Should().BeSameAs(input.Environment);
        branches[1].ListItemsOrNull()![1].Should().BeSameAs(input.Environment);
        actual.Evaluate().Should().Be(BaselineDirect(input.Expression, input.Environment)!.Value.value.Evaluate());
        profile.LargestCompletedConstructorByRootRetainedAvoidableObjects.Should().Be(boundary);
    }

    [Fact]
    public void Work_profile_completed_maxima_are_single_invocations_not_lifetime_sums()
    {
        var input = SharedConstructor();
        var larger = Expression.ListInst([input.Expression, input.Expression]);
        var secondEnvironment = PineValueInProcess.Create(PineValue.Blob([42, 43]));
        using var profile = new PineVMWorkProfile();

        using (profile.Enter())
        {
            BaselineDirect(input.Expression, input.Environment).Should().NotBeNull();
            BaselineDirect(larger, secondEnvironment).Should().NotBeNull();
            BaselineDirect(input.Expression, secondEnvironment).Should().NotBeNull();
        }

        var largest = profile.LargestCompletedConstructorByRootRetainedAvoidableObjects!.Value;
        largest.InvocationOrdinal.Should().Be(2);
        largest.AcceptedBodyEntries.Should().Be(15);
        largest.RootRetainedFreshObjects.Should().Be(18);
        largest.RootRetainedAvoidableObjects.Should().Be(11);
        largest.FreshWrappers.Should().Be(11);
        largest.ListArrays.Should().Be(7);
        largest.StartAcceptedBodyEntries.Should().Be(7);
        largest.EndAcceptedBodyEntries.Should().Be(22);
        profile.LargestCompletedConstructorByAcceptedBodyEntries.Should().Be(largest);
        profile.LargestCompletedConstructorByRootRetainedFreshObjects.Should().Be(largest);
        profile.LastConstructorInvocation!.Value.InvocationOrdinal.Should().Be(3);
        profile[PineVMWorkProfile.Metric.ConstructorCompletedRootRetentionProvenInvocations].Should().Be(3);
        profile[PineVMWorkProfile.Metric.ConstructorCompletedRootRetainedFreshObjectExposures].Should().Be(34);
        profile[PineVMWorkProfile.Metric.ConstructorCompletedRootRetainedAvoidableObjectExposures].Should().Be(17);
    }

    [Theory]
    [InlineData(0)]
    [InlineData(64)]
    public void Work_profile_private_call_allocation_window_excludes_tracking_set_setup(int capacity)
    {
        var input = SharedConstructor();
        using var warm = new PineVMWorkProfile(referenceCapacity: capacity);

        using (warm.Enter())
            BaselineDirect(input.Expression, input.Environment).Should().NotBeNull();

        using var profile = new PineVMWorkProfile(referenceCapacity: capacity);
        PineValueInProcess result;
        long allocated;

        using (profile.Enter())
        {
            var before = GC.GetAllocatedBytesForCurrentThread();
            result = BaselineDirect(input.Expression, input.Environment)!.Value.value;
            allocated = GC.GetAllocatedBytesForCurrentThread() - before;
        }

        var boundary = profile.LastConstructorInvocation!.Value;
        boundary.PrivateCallWindowCaptured.Should().BeTrue();
        allocated.Should().Be(boundary.ThreadAllocatedBytes + boundary.SetupTrackingAllocatedBytes);

        boundary.SetupTrackingAllocatedBytes.Should().Be(
            capacity is 0 ? 0 : warm.LastConstructorInvocation!.Value.SetupTrackingAllocatedBytes);

        FreshOutputObjects(result, input.Environment).Should().Be(8);
    }

    [Theory]
    [InlineData(0, 0)]
    [InlineData(1, 6)]
    [InlineData(2, 4)]
    public void Work_profile_retention_proof_is_independent_of_identity_census_coverage(int capacity, long capEvents)
    {
        var input = SharedConstructor();
        using var profile = new PineVMWorkProfile(referenceCapacity: capacity);

        using (profile.Enter())
            BaselineDirect(input.Expression, input.Environment).Should().NotBeNull();

        var boundary = profile.LastConstructorInvocation!.Value;
        boundary.OpportunityCoverageComplete.Should().BeFalse();
        boundary.RootRetainedFreshObjectsKnown.Should().BeTrue();
        boundary.RootRetainedFreshObjects.Should().Be(8);
        boundary.RootRetainedAvoidableObjects.Should().BeNull();
        boundary.RequiredFirstUniqueWrappers.Should().BeNull();
        boundary.RequiredFirstUniqueListArrays.Should().BeNull();
        boundary.CoverageCapEvents.Should().Be(capEvents);
        boundary.UniqueReferences.Should().Be(capacity);
        boundary.AvoidableWrappers.Should().Be(capacity is 2 ? 2 : 0);
        boundary.AvoidableListArrays.Should().Be(capacity is 2 ? 1 : 0);
        boundary.ReferenceCapacity.Should().Be(capacity);
        boundary.CounterfactualRootOwnedObjectsAfterMemo.Should().BeNull();
        profile.LargestCompletedConstructorByRootRetainedAvoidableObjects.Should().BeNull();
        profile.LargestCompletedConstructorByRootRetainedFreshObjects!.Value.RootRetainedFreshObjects.Should().Be(8);
        profile[PineVMWorkProfile.Metric.ConstructorCompletedRootRetainedFreshObjectExposures].Should().Be(8);
        profile[PineVMWorkProfile.Metric.ConstructorCompletedRootAvoidableOpportunityProvenInvocations].Should().Be(0);
        profile[PineVMWorkProfile.Metric.ConstructorCompletedRootAvoidableOpportunityUnknownInvocations].Should().Be(1);
    }

    [Fact]
    public void Work_profile_retention_proof_excludes_discarded_empty_arrays_and_preexisting_literal_wrappers()
    {
        var empty = Expression.ListInst([]);
        var emptyListConstructor = Expression.ListInst([empty, empty]);
        var literal = Expression.LitralInst(PineValue.EmptyBlob);
        var literalReuseConstructor = Expression.ListInst([literal, literal]);
        using var profile = new PineVMWorkProfile();

        using (profile.Enter())
        {
            var emptyReuseResult = BaselineDirect(emptyListConstructor, PineValueInProcess.EmptyBlob)!.Value.value;
            var discarded = profile.LastConstructorInvocation!.Value;
            discarded.Completed.Should().BeTrue();
            discarded.PureListLiteralEnvironmentGrammar.Should().BeTrue();
            discarded.AllocationConservationVerified.Should().BeTrue();
            discarded.CreateListReuses.Should().Be(2);
            discarded.ListArrays.Should().Be(3);
            discarded.FreshListWrappers.Should().Be(1);
            discarded.AvoidableListArrays.Should().Be(1);
            discarded.AvoidableListWrappers.Should().Be(0);
            discarded.RootRetainedFreshObjects.Should().BeNull();
            discarded.RootRetainedAvoidableObjects.Should().BeNull();
            discarded.RootOwnedFreshSubsetKnown.Should().BeTrue();
            discarded.RootOwnedFreshLiteralWrappers.Should().Be(0);
            discarded.RootOwnedFreshListWrappers.Should().Be(1);
            discarded.RootOwnedFreshListArrays.Should().Be(1);
            discarded.UnretainedEmptyListArrays.Should().Be(2);
            FreshOutputObjects(emptyReuseResult, PineValueInProcess.EmptyList).Should().Be(2);

            var reused = BaselineDirect(literalReuseConstructor, PineValueInProcess.EmptyBlob)!.Value.value;
            var boundary = profile.LastConstructorInvocation.Value;
            boundary.CreateReuses.Should().Be(2);
            boundary.FreshLiteralWrappers.Should().Be(0);
            boundary.FreshWrappers.Should().Be(1);
            boundary.RootRetainedFreshObjects.Should().Be(2);
            boundary.RootRetainedAvoidableObjects.Should().Be(0);
            FreshOutputObjects(reused, PineValueInProcess.EmptyBlob).Should().Be(2);
        }

        profile[PineVMWorkProfile.Metric.ConstructorCompletedRootRetentionUnknownInvocations].Should().Be(1);
        profile[PineVMWorkProfile.Metric.ConstructorCompletedRootRetentionProvenInvocations].Should().Be(1);
        profile[PineVMWorkProfile.Metric.ConstructorCompletedRootRetainedFreshObjectExposures].Should().Be(2);
    }

    [Fact]
    public void Work_profile_conditional_and_label_boundaries_do_not_claim_pure_grammar_retention()
    {
        var literal = Expression.LitralInst(PineValue.Blob([40, 41]));
        var list = Expression.ListInst([literal]);
        var conditional = new Expression.Conditional(list, list, list);
        using var profile = new PineVMWorkProfile();

        using (profile.Enter())
        {
            BaselineDirect(conditional, PineValueInProcess.EmptyBlob).Should().NotBeNull();
            var discardedCondition = profile.LastConstructorInvocation!.Value;
            discardedCondition.Completed.Should().BeTrue();
            discardedCondition.ConditionalEntries.Should().Be(1);
            discardedCondition.ListArrays.Should().Be(2);
            discardedCondition.FreshWrappers.Should().Be(4);
            discardedCondition.PureListLiteralEnvironmentGrammar.Should().BeFalse();
            discardedCondition.RootRetainedFreshObjects.Should().BeNull();
            discardedCondition.RootRetainedAvoidableObjects.Should().BeNull();
            BaselineDirect(new Expression.Label("not-pure", list), PineValueInProcess.EmptyBlob).Should().NotBeNull();
        }

        profile.LastConstructorInvocation!.Value.LabelEntries.Should().Be(1);
        profile.LastConstructorInvocation.Value.RootRetainedFreshObjectsKnown.Should().BeFalse();
        profile.LargestCompletedConstructorByRootRetainedFreshObjects.Should().BeNull();
        profile.LargestCompletedConstructorByRootRetainedAvoidableObjects.Should().BeNull();
        profile[PineVMWorkProfile.Metric.ConstructorCompletedRootRetentionUnknownInvocations].Should().Be(2);
        profile[PineVMWorkProfile.Metric.ConstructorCompletedRootRetainedAvoidableObjectExposures].Should().Be(0);
    }

    [Fact]
    public void Work_profile_ineligible_builtin_and_eval_have_no_constructor_retention_boundary()
    {
        using var profile = new PineVMWorkProfile();

        using (profile.Enter())
        {
            BaselineDirect(
                new Expression.Builtin(nameof(BuiltinFunction.length), Expression.EnvironmentInstance),
                PineValueInProcess.EmptyBlob).Should().NotBeNull();

            BaselineDirect(
                new Expression.Eval(Expression.EnvironmentInstance, Expression.EnvironmentInstance),
                PineValueInProcess.EmptyBlob).Should().BeNull();
        }

        profile.LastConstructorInvocation.Should().BeNull();
        profile.LargestCompletedConstructorByAcceptedBodyEntries.Should().BeNull();
        profile[PineVMWorkProfile.Metric.ConstructorIneligibleInvocations].Should().Be(2);
    }

    [Fact]
    public void Work_profile_interrupted_boundary_counts_rejected_call_but_no_completed_retention()
    {
        var input = SharedConstructor();
        using var profile = new PineVMWorkProfile(directVisitBudget: 13);

        using (profile.Enter())
        {
            BaselineDirect(input.Expression, input.Environment).Should().NotBeNull();
            Action interrupted = () => BaselineDirect(input.Expression, input.Environment);
            interrupted.Should().Throw<OperationCanceledException>();
        }

        var boundary = profile.LastConstructorInvocation!.Value;
        boundary.Completed.Should().BeFalse();
        boundary.PrivateCallWindowCaptured.Should().BeTrue();
        boundary.ResultAlreadyMaterialized.Should().BeNull();
        boundary.AcceptedBodyEntries.Should().Be(6);
        boundary.BudgetRejectedBodyEntries.Should().Be(1);
        boundary.PrivateEvaluatorMethodInvocations.Should().Be(7);
        boundary.OpportunityCoverageComplete.Should().BeFalse();
        boundary.RootRetainedFreshObjects.Should().BeNull();
        boundary.RootRetainedAvoidableObjects.Should().BeNull();
        profile.LargestCompletedConstructorByAcceptedBodyEntries!.Value.InvocationOrdinal.Should().Be(1);

        profile.LargestCompletedConstructorByRootRetainedAvoidableObjects!.Value
            .RootRetainedAvoidableObjects.Should().Be(3);

        profile[PineVMWorkProfile.Metric.ConstructorCompletedRootRetainedFreshObjectExposures].Should().Be(8);
        profile[PineVMWorkProfile.Metric.ConstructorCompletedRootRetainedAvoidableObjectExposures].Should().Be(3);
        profile[PineVMWorkProfile.Metric.EvalDirectBudgetRejectedEntries].Should().Be(1);
    }

    [Fact]
    public void Work_profile_large_completed_boundaries_emit_bounded_numeric_record_maxima()
    {
        Expression expression = Expression.LitralInst(PineValue.Blob([40, 41]));

        for (var depth = 0; depth < 12; ++depth)
            expression = Expression.ListInst([expression, expression]);

        using var output = new StringWriter();
        using var profile = new PineVMWorkProfile(output, referenceCapacity: 64);

        using (profile.Enter())
        {
            for (var depth = 12; depth <= 14; ++depth)
            {
                BaselineDirect(expression, PineValueInProcess.EmptyBlob).Should().NotBeNull();
                BaselineDirect(expression, PineValueInProcess.EmptyBlob).Should().NotBeNull();
                expression = Expression.ListInst([expression, expression]);
            }
        }

        var boundaryReports = 0;

        foreach (var line in output.ToString().Split('\n', StringSplitOptions.RemoveEmptyEntries))
        {
            using var json = JsonDocument.Parse(line);

            if (json.RootElement.GetProperty("Outcome").GetString() is not "constructor-completed-maximum")
                continue;

            ++boundaryReports;
            var maximum = json.RootElement.GetProperty("LargestCompletedConstructorByRootRetainedAvoidableObjects");
            maximum.GetProperty("Completed").GetBoolean().Should().BeTrue();
            maximum.GetProperty("RootRetainedFreshObjectsKnown").GetBoolean().Should().BeTrue();

            maximum.EnumerateObject().Should().OnlyContain(
                item =>
                item.Value.ValueKind == JsonValueKind.Number || item.Value.ValueKind == JsonValueKind.True ||
                item.Value.ValueKind == JsonValueKind.False || item.Value.ValueKind == JsonValueKind.Null);

            json.RootElement.GetProperty("ConstructorInvocationsPending").GetInt64().Should().Be(0);
            json.RootElement.GetProperty("ConstructorBoundarySnapshotCount").GetInt32().Should().Be(boundaryReports);
            json.RootElement.GetProperty("ConstructorBoundarySnapshotLimit").GetInt32().Should().Be(16);
            json.RootElement.GetProperty("SchemaVersion").GetInt32().Should().Be(2);
            json.RootElement.GetProperty("RetainedVmObjectCountKnown").GetBoolean().Should().BeFalse();
        }

        boundaryReports.Should().Be(3);
        profile.LargestCompletedConstructorByRootRetainedAvoidableObjects!.Value.InvocationOrdinal.Should().Be(5);
        output.ToString().Should().NotContain("BlobBase64").And.NotContain("not-pure");
    }

    [Fact]
    public void Work_profile_synthetic_invocation_local_memo_matches_completed_frontier_opportunity()
    {
        var environment = PineValueInProcess.Create(PineValue.List([PineValue.Blob([42, 43])]));
        var literal = Expression.LitralInst(PineValue.List([PineValue.Blob([40, 41])]));
        var inner = Expression.ListInst([literal, Expression.EnvironmentInstance]);
        var outer = Expression.ListInst([inner, inner]);
        var expression = Expression.ListInst([outer, outer]);
        using var profile = new PineVMWorkProfile();
        PineValueInProcess baseline;

        using (profile.Enter())
            baseline = BaselineDirect(expression, environment)!.Value.value;

        var boundary = profile.LastConstructorInvocation!.Value;
        var memo = new Dictionary<Expression, PineValueInProcess>(ReferenceEqualityComparer.Instance);
        var memoCalls = 0L;
        var shared = Memo(expression);
        var baselineObjects = FreshOutputObjects(baseline, environment);
        var sharedObjects = FreshOutputObjects(shared, environment);
        boundary.RootRetainedFreshObjects.Should().Be(baselineObjects);
        boundary.RootOwnedObjectsBeforeMemo.Should().Be(baselineObjects);
        boundary.CounterfactualRootOwnedObjectsAfterMemo.Should().Be(sharedObjects);
        boundary.UniqueReferences.Should().Be(5);
        boundary.RootRetainedAvoidableObjects.Should().Be(baselineObjects - sharedObjects);
        boundary.AvoidableDescendantBodyEntries.Should().Be(boundary.AcceptedBodyEntries - memoCalls);
        (boundary.RequiredFirstUniqueWrappers + boundary.RequiredFirstUniqueListArrays).Should().Be(sharedObjects);
        shared.ListItemsOrNull()![0].Should().BeSameAs(shared.ListItemsOrNull()![1]);
        shared.Evaluate().Should().Be(baseline.Evaluate());
        PineVMWorkProfile.Current.Should().BeNull();

        // Counterfactual only: this test-local interpreter does not change the VM or reuse across environments.
        PineValueInProcess Memo(Expression node)
        {
            ++memoCalls;

            if (memo.TryGetValue(node, out var cached))
                return cached;

            PineValueInProcess result;

            switch (node)
            {
                case Expression.Litral constant:
                    result = PineValueInProcess.Create(constant.Value);
                    break;

                case Expression.Environment:
                    result = environment;
                    break;

                case Expression.List list:
                    var items = new PineValueInProcess[list.Items.Count];

                    for (var index = 0; index < items.Length; ++index)
                        items[index] = Memo(list.Items[index]);

                    result = PineValueInProcess.CreateList(items);
                    break;

                case Expression.Label:
                case Expression.Conditional:
                case Expression.Builtin:
                case Expression.Eval:
                    throw new InvalidOperationException("The counterfactual requires pure constructor grammar.");

                default:
                    throw new NotImplementedException("Memo does not handle expression variant: " + node.GetType().Name);
            }

            memo.Add(node, result);
            return result;
        }
    }

    private static long FreshOutputObjects(PineValueInProcess root, PineValueInProcess preexisting)
    {
        var objects = new HashSet<object>(ReferenceEqualityComparer.Instance);
        var pending = new Stack<PineValueInProcess>();
        pending.Push(root);

        while (pending.TryPop(out var value))
        {
            if (ReferenceEquals(value, preexisting) || !objects.Add(value))
                continue;

            if (value.ListItemsOrNull() is { } items)
            {
                items.Should().BeOfType<PineValueInProcess[]>();
                objects.Add(items);

                foreach (var item in items)
                    pending.Push(item);
            }
        }

        return objects.Count;
    }

    [Fact]
    public void Work_profile_materialization_causes_partition_lists_and_deferred_values()
    {
        var template =
            FunctionValueBuilder.TryBuildCurriedFunctionValueAsTemplate(
            Expression.EnvironmentInstance, 3)!;

        var plan = ExpressionTemplatePlan.TryParseTemplate(template, new PineVMParseCache())!;
        var environments = CollectedTemplateEnvironments.Empty.Append(PineValueInProcess.EmptyBlob);
        using var profile = new PineVMWorkProfile();

        using (profile.Enter())
        {
            foreach (var cause in new[]
            {
                PineVMWorkProfile.MaterializationCause.RootReturn,
                PineVMWorkProfile.MaterializationCause.InvocationCache,
                PineVMWorkProfile.MaterializationCause.Recognition,
                PineVMWorkProfile.MaterializationCause.Other
            })
            {
                var value =
                    PineValueInProcess.CreateDeferredTemplateValue(
                        plan,
                        environments,
                        collected => plan.Materialize(collected, new DirectInterpreterCounters()));

                using (profile.WithCause(cause))
                {
                    value.Evaluate().Should().Be(plan.Materialize(environments, new DirectInterpreterCounters()));
                    value.Evaluate();
                }
            }
        }

        profile[PineVMWorkProfile.Metric.DeferredMaterializations].Should().Be(4);
        profile[PineVMWorkProfile.Metric.DeferredMaterializedEnvironmentSlots].Should().Be(4);
        profile[PineVMWorkProfile.Metric.DeferredMaterializedRootReturn].Should().Be(1);
        profile[PineVMWorkProfile.Metric.DeferredMaterializedInvocationCache].Should().Be(1);
        profile[PineVMWorkProfile.Metric.DeferredMaterializedRecognition].Should().Be(1);
        profile[PineVMWorkProfile.Metric.DeferredMaterializedOther].Should().Be(1);

        profile[PineVMWorkProfile.Metric.MaterializedListCalls].Should().Be(
            profile[PineVMWorkProfile.Metric.MaterializedListRootReturn] +
            profile[PineVMWorkProfile.Metric.MaterializedListInvocationCache] +
            profile[PineVMWorkProfile.Metric.MaterializedListRecognition] +
            profile[PineVMWorkProfile.Metric.MaterializedListOther]);

        profile[PineVMWorkProfile.Metric.MaterializedListRootReturn].Should().BeGreaterThan(0);
        profile[PineVMWorkProfile.Metric.MaterializedListInvocationCache].Should().BeGreaterThan(0);
        profile[PineVMWorkProfile.Metric.MaterializedListRecognition].Should().BeGreaterThan(0);
        profile[PineVMWorkProfile.Metric.MaterializedListOther].Should().BeGreaterThan(0);
    }

    [Fact]
    public void Work_profile_concrete_list_allocation_pre_reuse_and_post_reuse_are_distinct()
    {
        // A depth-four list with a large, nonpopular payload reaches the existing weak interner.
        var bytes = new byte[11000];
        Guid.NewGuid().TryWriteBytes(bytes);
        var largeBlob = PineValue.Blob(bytes);
        var depthOne = PineValue.List([largeBlob]);
        var depthTwo = PineValue.List([depthOne]);
        var depthThree = PineValue.List([depthTwo]);
        var items = new PineValue[] { depthThree };
        using var output = new StringWriter();
        using var profile = new PineVMWorkProfile(output);

        using (profile.Enter())
        {
            var first = PineValue.List(items.AsMemory());
            var second = PineValue.List(items.AsMemory());
            ReferenceEquals(first, second).Should().BeTrue();
            PineValue.List(ReadOnlyMemory<PineValue>.Empty).Should().BeSameAs(PineValue.EmptyList);
            _ = new PineValue.ListValue(items.AsMemory());
        }

        profile.WriteSnapshot("tested");
        profile[PineVMWorkProfile.Metric.PineListCalls].Should().Be(3);
        profile[PineVMWorkProfile.Metric.PineListPreallocationReuses].Should().Be(1);
        profile[PineVMWorkProfile.Metric.PineListPostallocationReuses].Should().Be(1);
        profile[PineVMWorkProfile.Metric.ListValueConstructorAllocations].Should().Be(3);
        profile.KnownAllocationObjects.Should().Be(3);
        using var last = JsonDocument.Parse(output.ToString().Split('\n', StringSplitOptions.RemoveEmptyEntries)[^1]);
        last.RootElement.GetProperty("KnownAllocationObjects").GetInt64().Should().Be(3);
        last.RootElement.GetProperty("KnownAllocationObjectsIsLowerBound").GetBoolean().Should().BeTrue();
    }
}
