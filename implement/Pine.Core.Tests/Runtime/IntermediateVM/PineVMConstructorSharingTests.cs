using AwesomeAssertions;
using Pine.Core.Internal;
using Pine.Core.PineLanguage;
using Pine.Core.Runtime.IntermediateVM;
using System;
using System.Collections.Generic;
using System.IO;
using System.Runtime.CompilerServices;
using System.Text.Json;
using System.Threading;
using Xunit;

using DirectVM = Pine.Core.Runtime.IntermediateVM.PineVM;

namespace Pine.Core.Tests.Runtime.IntermediateVM;

public class PineVMConstructorSharingTests
{
    private static Expression SharedConstructor()
    {
        var literal = new Expression.Litral(PineValue.Blob([40, 41]));
        var shared = new Expression.List([literal, Expression.EnvironmentInstance]);
        return new Expression.List([shared, shared]);
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void Direct_constructor_sharing_preserves_values_counters_and_reference_identity(bool profiled)
    {
        var expression = SharedConstructor();
        var environment = PineValueInProcess.Create(PineValue.Blob([42, 43]));
        var baseline = DirectVM.EvalDirect(expression, environment, preserveConstructorSharing: false)!.Value;
        using var profile = new PineVMWorkProfile();
        (PineValueInProcess value, EvalWorkCounters evalWorkCounters) actual;

        using (PineVMWorkProfile.EnterEvaluation(profiled ? profile : null, default, null))
            actual = DirectVM.EvalDirect(expression, environment)!.Value;

        var items = actual.value.ListItemsOrNull()!;
        items[0].Should().BeSameAs(items[1]);
        items[0].ListItemsOrNull()![1].Should().BeSameAs(environment);
        baseline.value.ListItemsOrNull()![0].Should().NotBeSameAs(baseline.value.ListItemsOrNull()![1]);
        actual.evalWorkCounters.Should().Be(baseline.evalWorkCounters);
        actual.evalWorkCounters.InstructionCount.Should().Be(7);
        actual.evalWorkCounters.BuildListCount.Should().Be(3);
        actual.evalWorkCounters.BuildListItemCount.Should().Be(6);
        FreshObjects(baseline.value, environment).Should().Be(8);
        FreshObjects(actual.value, environment).Should().Be(5);
        actual.value.Evaluate().Should().Be(baseline.value.Evaluate());

        if (profiled)
        {
            profile[PineVMWorkProfile.Metric.EvalDirectEntries].Should().Be(5);
            profile[PineVMWorkProfile.Metric.EvalDirectListArrays].Should().Be(2);
            profile[PineVMWorkProfile.Metric.WrapperAllocations].Should().Be(3);
            profile[PineVMWorkProfile.Metric.DirectConstructorMemoHits].Should().Be(1);
            profile[PineVMWorkProfile.Metric.ConstructorCompletedLogicalInstructions].Should().Be(7);
            profile.LastConstructorInvocation!.Value.LogicalInstructionCount.Should().Be(7);
            profile.LastConstructorInvocation.Value.LogicalBuildListCount.Should().Be(3);
            profile.LastConstructorInvocation.Value.LogicalBuildListItemCount.Should().Be(6);
            profile.LastConstructorInvocation.Value.AllocationConservationVerified.Should().BeTrue();
            profile.LastConstructorInvocation.Value.RootRetainedFreshObjects.Should().Be(5);
        }
    }

    [Fact]
    public void Direct_constructor_sharing_is_local_to_each_environment_and_invocation()
    {
        var expression = SharedConstructor();
        var firstEnvironment = PineValueInProcess.Create(PineValue.Blob([42, 43]));
        var secondEnvironment = PineValueInProcess.Create(PineValue.Blob([44, 45]));
        var first = DirectVM.EvalDirect(expression, firstEnvironment)!.Value.value;
        var second = DirectVM.EvalDirect(expression, secondEnvironment)!.Value.value;
        var repeated = DirectVM.EvalDirect(expression, firstEnvironment)!.Value.value;
        first.Should().NotBeSameAs(second).And.NotBeSameAs(repeated);
        first.ListItemsOrNull()![0].Should().NotBeSameAs(repeated.ListItemsOrNull()![0]);
        first.ListItemsOrNull()![0].ListItemsOrNull()![1].Should().BeSameAs(firstEnvironment);
        second.ListItemsOrNull()![0].ListItemsOrNull()![1].Should().BeSameAs(secondEnvironment);
        repeated.Evaluate().Should().Be(first.Evaluate());
        second.Evaluate().Should().NotBe(first.Evaluate());
    }

    [Fact]
    public void Direct_constructor_sharing_uses_identity_not_structural_equality()
    {
        var left = new Expression.List([new Expression.Litral(PineValue.Blob([40, 41]))]);
        var right = new Expression.List([new Expression.Litral(PineValue.Blob([40, 41]))]);
        left.Should().Be(right).And.NotBeSameAs(right);
        var expression = new Expression.List([left, right, left]);
        var actual = DirectVM.EvalDirect(expression, PineValueInProcess.EmptyBlob)!.Value;

        var baseline =
            DirectVM.EvalDirect(expression, PineValueInProcess.EmptyBlob, preserveConstructorSharing: false)!.Value;

        var items = actual.value.ListItemsOrNull()!;
        items[0].Should().BeSameAs(items[2]).And.NotBeSameAs(items[1]);
        items[0].ListItemsOrNull()![0].Should().NotBeSameAs(items[1].ListItemsOrNull()![0]);
        actual.value.Evaluate().Should().Be(baseline.value.Evaluate());
        actual.evalWorkCounters.Should().Be(baseline.evalWorkCounters);
    }

    [Fact]
    public void Direct_constructor_sharing_preserves_unshared_tree_and_small_constructor_allocation_cost()
    {
        var tree =
            new Expression.List(
                [
                new Expression.List([new Expression.Litral(PineValue.Blob([40, 41]))]),
                new Expression.List([new Expression.Litral(PineValue.Blob([42, 43]))])
                ]);

        var actual = DirectVM.EvalDirect(tree, PineValueInProcess.EmptyBlob)!.Value;

        var baseline =
            DirectVM.EvalDirect(tree, PineValueInProcess.EmptyBlob, preserveConstructorSharing: false)!.Value;

        actual.evalWorkCounters.Should().Be(baseline.evalWorkCounters);

        FreshObjects(actual.value, PineValueInProcess.EmptyBlob).Should().Be(
            FreshObjects(baseline.value, PineValueInProcess.EmptyBlob));

        actual.value.Evaluate().Should().Be(baseline.value.Evaluate());

        var small =
            new Expression.List(
                [
                new Expression.Litral(PineValue.Blob([40, 41])),
                Expression.EnvironmentInstance
                ]);

        _ = DirectVM.EvalDirect(small, PineValueInProcess.EmptyBlob);
        _ = DirectVM.EvalDirect(small, PineValueInProcess.EmptyBlob, preserveConstructorSharing: false);
        var before = GC.GetAllocatedBytesForCurrentThread();
        var smallActual = DirectVM.EvalDirect(small, PineValueInProcess.EmptyBlob)!.Value.value;
        var allocated = GC.GetAllocatedBytesForCurrentThread() - before;
        before = GC.GetAllocatedBytesForCurrentThread();

        var smallBaseline =
            DirectVM.EvalDirect(small, PineValueInProcess.EmptyBlob, preserveConstructorSharing: false)!.Value.value;

        var baselineAllocated = GC.GetAllocatedBytesForCurrentThread() - before;
        allocated.Should().Be(baselineAllocated);
        GC.KeepAlive(smallActual);
        GC.KeepAlive(smallBaseline);
    }

    [Fact]
    public void Direct_constructor_sharing_handles_empty_lists_and_repeated_literals()
    {
        var literal = new Expression.Litral(PineValue.Blob([40, 41]));
        var empty = new Expression.List([]);
        var expression = new Expression.List([literal, literal, empty, empty]);
        var actual = DirectVM.EvalDirect(expression, PineValueInProcess.EmptyBlob)!.Value;

        var baseline =
            DirectVM.EvalDirect(expression, PineValueInProcess.EmptyBlob, preserveConstructorSharing: false)!.Value;

        var items = actual.value.ListItemsOrNull()!;
        items[0].Should().BeSameAs(items[1]);
        items[2].Should().BeSameAs(PineValueInProcess.EmptyList);
        items[3].Should().BeSameAs(PineValueInProcess.EmptyList);
        actual.evalWorkCounters.Should().Be(baseline.evalWorkCounters);
        actual.value.Evaluate().Should().Be(baseline.value.Evaluate());
    }

    [Fact]
    public void Direct_constructor_sharing_excludes_labels_and_conditional_or_effectful_subtrees()
    {
        var shared = SharedConstructor();
        var label = new Expression.Label("constructor-sharing-test", shared);
        var labelParent = new Expression.List([label]);

        var expressions =
            new Expression[]
            {
                label,
                new Expression.List([labelParent, labelParent]),
                new Expression.Label(
                    "conditional",
                    new Expression.Conditional(
                        new Expression.Litral(PineKernelValues.TrueValue),
                        new Expression.Eval(Expression.EnvironmentInstance, Expression.EnvironmentInstance),
                        shared)),
                new Expression.List([new Expression.Builtin("unknown-builtin", shared), shared]),
                new Expression.List(
                    [
                    new Expression.Eval(Expression.EnvironmentInstance, Expression.EnvironmentInstance),
                    shared
                    ]),
                new Expression.List(
                    [new Expression.Builtin(nameof(BuiltinFunction.head), new Expression.List([])), shared])
            };

        foreach (var expression in expressions)
        {
            using var baselineProfile = new PineVMWorkProfile();
            using var actualProfile = new PineVMWorkProfile();
            (PineValueInProcess value, EvalWorkCounters evalWorkCounters)? baseline;
            (PineValueInProcess value, EvalWorkCounters evalWorkCounters)? actual;

            using (baselineProfile.Enter())
            {
                baseline =
                    DirectVM.EvalDirect(expression, PineValueInProcess.EmptyBlob, preserveConstructorSharing: false);
            }

            using (actualProfile.Enter())
                actual = DirectVM.EvalDirect(expression, PineValueInProcess.EmptyBlob);

            actual.HasValue.Should().Be(baseline.HasValue);

            if (actual is { } result)
            {
                result.evalWorkCounters.Should().Be(baseline!.Value.evalWorkCounters);
                result.value.Evaluate().Should().Be(baseline.Value.value.Evaluate());
            }

            actualProfile[PineVMWorkProfile.Metric.DirectConstructorMemoHits].Should().Be(0);

            actualProfile[PineVMWorkProfile.Metric.EvalDirectEntries].Should().Be(
                baselineProfile[PineVMWorkProfile.Metric.EvalDirectEntries]);
        }
    }

    [Theory]
    [InlineData(4, false)]
    [InlineData(5, true)]
    public void Direct_constructor_sharing_keeps_profile_budget_physical_and_checks_hits(long budget, bool completes)
    {
        using var profile = new PineVMWorkProfile(directVisitBudget: budget);

        using (profile.Enter())
        {
            if (completes)
            {
                var actual = DirectVM.EvalDirect(SharedConstructor(), PineValueInProcess.EmptyBlob)!.Value;
                actual.evalWorkCounters.InstructionCount.Should().Be(7);
                actual.evalWorkCounters.BuildListCount.Should().Be(3);
                actual.evalWorkCounters.BuildListItemCount.Should().Be(6);
            }
            else
            {
                Action run = () => DirectVM.EvalDirect(SharedConstructor(), PineValueInProcess.EmptyBlob);
                run.Should().Throw<OperationCanceledException>();
            }
        }

        profile[PineVMWorkProfile.Metric.EvalDirectEntries].Should().Be(budget);
        profile[PineVMWorkProfile.Metric.EvalDirectBudgetRejectedEntries].Should().Be(completes ? 0 : 1);
        profile.LastConstructorInvocation!.Value.Completed.Should().Be(completes);
        PineVMWorkProfile.Current.Should().BeNull();
    }

    [Fact]
    public void Direct_constructor_sharing_checks_cancellation_and_does_not_reuse_partial_invocations()
    {
        var expression = SharedConstructor();
        using var cancellation = new CancellationTokenSource();
        cancellation.Cancel();
        using var profile = new PineVMWorkProfile();

        using (profile.Enter(cancellation.Token))
        {
            Action run = () => DirectVM.EvalDirect(expression, PineValueInProcess.EmptyBlob);
            run.Should().Throw<OperationCanceledException>();
        }

        profile[PineVMWorkProfile.Metric.EvalDirectEntries].Should().Be(0);
        using var interrupted = new PineVMWorkProfile(directVisitBudget: 4);

        using (interrupted.Enter())
        {
            Action run = () => DirectVM.EvalDirect(expression, PineValueInProcess.EmptyBlob);
            run.Should().Throw<OperationCanceledException>();
        }

        interrupted.LastConstructorInvocation!.Value.LogicalInstructionCount.Should().BeNull();
        interrupted[PineVMWorkProfile.Metric.ConstructorCompletedLogicalInstructions].Should().Be(0);
        DirectVM.EvalDirect(expression, PineValueInProcess.EmptyBlob).Should().NotBeNull();
    }

    [Fact]
    public void Direct_constructor_sharing_does_not_retain_expressions_after_return()
    {
        var (value, expression) = EvaluateFreshExpression();
        GC.Collect();
        GC.WaitForPendingFinalizers();
        GC.Collect();
        expression.TryGetTarget(out _).Should().BeFalse();
        value.Evaluate().Should().BeOfType<PineValue.ListValue>();

        [MethodImpl(MethodImplOptions.NoInlining)]
        static (PineValueInProcess, WeakReference<Expression>) EvaluateFreshExpression()
        {
            var expression = SharedConstructor();

            return
                (DirectVM.EvalDirect(expression, PineValueInProcess.EmptyBlob)!.Value.value,
                new WeakReference<Expression>(expression));
        }
    }

    [Fact]
    public void Direct_constructor_sharing_reports_completed_logical_boundary_for_matched_work_profiles()
    {
        Expression expression = new Expression.Litral(PineValue.Blob([40, 41]));

        for (var depth = 0; depth < 12; ++depth)
            expression = new Expression.List([expression, expression]);

        using var output = new StringWriter();
        using var profile = new PineVMWorkProfile(output);

        using (profile.Enter())
            DirectVM.EvalDirect(expression, PineValueInProcess.EmptyBlob).Should().NotBeNull();

        profile.LastConstructorInvocation!.Value.AcceptedBodyEntries.Should().Be(25);
        profile.LastConstructorInvocation.Value.LogicalInstructionCount.Should().Be(8191);
        profile.LastConstructorInvocation.Value.RootRetainedFreshObjects.Should().Be(25);
        profile.LargestCompletedConstructorByLogicalInstructions.Should().Be(profile.LastConstructorInvocation);
        var found = false;

        foreach (var line in output.ToString().Split('\n', StringSplitOptions.RemoveEmptyEntries))
        {
            using var json = JsonDocument.Parse(line);

            if (json.RootElement.GetProperty("Outcome").GetString() is not "constructor-completed-maximum")
                continue;

            found = true;
            var boundary = json.RootElement.GetProperty("LargestCompletedConstructorByLogicalInstructions");
            boundary.GetProperty("LogicalInstructionCount").GetInt64().Should().Be(8191);
            boundary.GetProperty("AcceptedBodyEntries").GetInt64().Should().Be(25);
            boundary.GetProperty("InvocationOrdinal").GetInt64().Should().Be(1);
            boundary.GetProperty("RootRetainedFreshObjects").GetInt64().Should().Be(25);
            json.RootElement.GetProperty("DirectEntryCountersIncludeMemoHits").GetBoolean().Should().BeTrue();
            json.RootElement.GetProperty("ConstructorLogicalCountersIncludeMemoReplay").GetBoolean().Should().BeTrue();

            json.RootElement
                .GetProperty("ConstructorRetentionUsesMemoAwareAllocationConservation").GetBoolean().Should().BeTrue();

            json.RootElement.GetProperty("ConstructorCounterfactualMeasuresRemainingExecutionOpportunities")
                .GetBoolean().Should().BeTrue();
        }

        found.Should().BeTrue();
    }

    [Theory]
    [InlineData(0)]
    [InlineData(1)]
    [InlineData(64)]
    public void Direct_constructor_sharing_retention_proof_matches_object_census_for_all_memo_kinds(int capacity)
    {
        var literal = new Expression.Litral(PineValue.Blob([40, 41]));
        var inner = new Expression.List([literal, Expression.EnvironmentInstance]);
        var expression = new Expression.List([inner, inner, literal, Expression.EnvironmentInstance]);
        using var profile = new PineVMWorkProfile(referenceCapacity: capacity);
        PineValueInProcess value;

        using (profile.Enter())
            value = DirectVM.EvalDirect(expression, PineValueInProcess.EmptyBlob)!.Value.value;

        var boundary = profile.LargestCompletedConstructorByLogicalInstructions!.Value;
        boundary.LogicalInstructionCount.Should().Be(9);
        boundary.AcceptedBodyEntries.Should().Be(7);
        boundary.MemoHits.Should().Be(3);
        boundary.LiteralMemoHits.Should().Be(1);
        boundary.ListMemoHits.Should().Be(1);
        boundary.EnvironmentMemoHits.Should().Be(1);
        boundary.PureListLiteralEnvironmentGrammar.Should().BeTrue();
        boundary.AllocationConservationVerified.Should().BeTrue();
        boundary.RootRetainedFreshObjectsKnown.Should().BeTrue();
        boundary.RootRetainedFreshObjects.Should().Be(FreshObjects(value, PineValueInProcess.EmptyBlob)).And.Be(5);
        boundary.OpportunityCoverageComplete.Should().Be(capacity is 64);
        profile[PineVMWorkProfile.Metric.ConstructorCompletedRootRetainedFreshObjectExposures].Should().Be(5);
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void Work_profile_constructor_budget_matches_completed_work_with_sharing_on_or_off(bool sharing)
    {
        var expression = SharedConstructor();
        using var output = new StringWriter();

        using var profile =
            new PineVMWorkProfile(
                output,
                constructorInvocationBudget: 2,
                preserveConstructorSharing: sharing);

        using (profile.Enter())
        {
            for (var invocation = 0; invocation < 2; ++invocation)
            {
                DirectVM.EvalDirect(expression, PineValueInProcess.EmptyBlob)!.Value.evalWorkCounters.InstructionCount
                    .Should().Be(7);
            }

            Action rejected = () => DirectVM.EvalDirect(expression, PineValueInProcess.EmptyBlob);
            rejected.Should().Throw<OperationCanceledException>();
        }

        profile[PineVMWorkProfile.Metric.ConstructorInvocationsStarted].Should().Be(2);
        profile[PineVMWorkProfile.Metric.ConstructorInvocationsCompleted].Should().Be(2);
        profile[PineVMWorkProfile.Metric.ConstructorInvocationsIncomplete].Should().Be(0);
        profile[PineVMWorkProfile.Metric.ConstructorBudgetRejectedInvocations].Should().Be(1);
        profile[PineVMWorkProfile.Metric.ConstructorCompletedLogicalInstructions].Should().Be(14);
        profile[PineVMWorkProfile.Metric.EvalDirectEntries].Should().Be(sharing ? 10 : 14);
        profile.LastConstructorInvocation!.Value.InvocationOrdinal.Should().Be(2);
        profile.LastConstructorInvocation.Value.Completed.Should().BeTrue();
        profile.StopReason.Should().Be("Work-profile constructor invocation budget exhausted.");
        using var json = JsonDocument.Parse(output.ToString().Split('\n', StringSplitOptions.RemoveEmptyEntries)[^1]);
        json.RootElement.GetProperty("Outcome").GetString().Should().Be("constructor-invocation-limit");
        json.RootElement.GetProperty("ConstructorInvocationBudget").GetInt64().Should().Be(2);
        json.RootElement.GetProperty("PreserveConstructorSharing").GetBoolean().Should().Be(sharing);
        json.RootElement.GetProperty("ConstructorInvocationsPending").GetInt64().Should().Be(0);
        PineVMWorkProfile.Current.Should().BeNull();
    }

    [Fact]
    public void Work_profile_constructor_sharing_baseline_option_is_scope_local()
    {
        var expression = SharedConstructor();
        using var baseline = new PineVMWorkProfile(preserveConstructorSharing: false);

        using (baseline.Enter())
        {
            var unshared =
                DirectVM.EvalDirect(expression, PineValueInProcess.EmptyBlob)!.Value.value.ListItemsOrNull()!;

            unshared[0].Should().NotBeSameAs(unshared[1]);

            using (PineVMWorkProfile.EnterEvaluation(null, default, null))
            {
                var shared =
                    DirectVM.EvalDirect(expression, PineValueInProcess.EmptyBlob)!.Value.value.ListItemsOrNull()!;

                shared[0].Should().BeSameAs(shared[1]);
            }
        }

        using var enabled = new PineVMWorkProfile();

        using (enabled.Enter())
        {
            var shared = DirectVM.EvalDirect(expression, PineValueInProcess.EmptyBlob)!.Value.value.ListItemsOrNull()!;
            shared[0].Should().BeSameAs(shared[1]);
        }

        baseline[PineVMWorkProfile.Metric.DirectConstructorMemoHits].Should().Be(0);
        enabled[PineVMWorkProfile.Metric.DirectConstructorMemoHits].Should().Be(1);
    }

    [Theory]
    [InlineData(0)]
    [InlineData(-1)]
    public void Work_profile_constructor_budget_must_be_positive(long budget)
    {
        Action create = () => new PineVMWorkProfile(constructorInvocationBudget: budget);
        create.Should().Throw<ArgumentOutOfRangeException>();
    }

    [Fact]
    public void Direct_constructor_sharing_keeps_deep_exponential_DAG_work_and_output_linear()
    {
        const int depth = 45;
        Expression expression = new Expression.Litral(PineValue.Blob([40, 41]));

        for (var level = 0; level < depth; ++level)
            expression = new Expression.List([expression, expression]);

        using var profile = new PineVMWorkProfile();
        (PineValueInProcess value, EvalWorkCounters evalWorkCounters) actual;

        using (profile.Enter())
            actual = DirectVM.EvalDirect(expression, PineValueInProcess.EmptyBlob)!.Value;

        actual.evalWorkCounters.InstructionCount.Should().Be((1L << (depth + 1)) - 1);
        actual.evalWorkCounters.BuildListCount.Should().Be((1L << depth) - 1);
        actual.evalWorkCounters.BuildListItemCount.Should().Be((1L << (depth + 1)) - 2);
        profile[PineVMWorkProfile.Metric.EvalDirectEntries].Should().Be(2 * depth + 1);
        profile[PineVMWorkProfile.Metric.EvalDirectListArrays].Should().Be(depth);
        profile[PineVMWorkProfile.Metric.DirectConstructorMemoHits].Should().Be(depth);
        FreshObjects(actual.value, PineValueInProcess.EmptyBlob).Should().Be(2 * depth + 1);

        profile.LargestCompletedConstructorByLogicalInstructions!.Value.RootRetainedFreshObjects
            .Should().Be(2 * depth + 1);

        var current = actual.value;

        for (var level = 0; level < depth; ++level)
        {
            var items = current.ListItemsOrNull()!;
            items[0].Should().BeSameAs(items[1]);
            current = items[0];
        }

        current.Evaluate().Should().Be(PineValue.Blob([40, 41]));
        actual.value.Evaluate().Should().BeOfType<PineValue.ListValue>();
    }

    private static long FreshObjects(PineValueInProcess root, PineValueInProcess environment)
    {
        var objects = new HashSet<object>(ReferenceEqualityComparer.Instance);
        var pending = new Stack<PineValueInProcess>();
        pending.Push(root);

        while (pending.TryPop(out var value))
        {
            if (ReferenceEquals(value, environment) || !objects.Add(value))
                continue;

            if (value.ListItemsOrNull() is { } items)
            {
                objects.Add(items);

                foreach (var item in items)
                    pending.Push(item);
            }
        }

        return objects.Count;
    }
}
