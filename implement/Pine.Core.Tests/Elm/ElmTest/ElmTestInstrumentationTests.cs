using AwesomeAssertions;
using Pine.Core.Addressing;
using Pine.Core.CommonEncodings;
using Pine.Core.Elm.Testing;
using Pine.Core.Interpreter.IntermediateVM;
using System;
using System.Linq;
using System.Threading.Tasks;
using Xunit;

namespace Pine.Core.Tests.Elm.ElmTest;

public class ElmTestInstrumentationTests
{
    [Fact]
    public void Nonprofiling_evaluation_errors_do_not_cancel_other_tests_or_announce_a_budget_stop()
    {
        var stops = 0;

        using var budget =
            new ElmTestInstrumentation(new() { InvocationBudget = 100 }, recordDiagnostics: false)
            {
                OnStopped = _ => stops++,
            };

        var vm = budget.CreateVm(new ConcurrentInvocationCache(), new PineVMSharedCaches());
        var invalid = new Expression.Eval(Expression.EnvironmentInstance, Expression.EnvironmentInstance);
        vm.EvaluateExpression(invalid, PineValue.EmptyList).IsErrOrNull().Should().NotBeNull();
        stops.Should().Be(0);
        budget.CancellationToken.IsCancellationRequested.Should().BeFalse();
        budget.GetSummary().StopReason.Should().BeNull();

        vm.EvaluateExpression(Expression.LitralInst(IntegerEncoding.EncodeSignedInteger(42)), PineValue.EmptyList)
            .IsOkOrNull().Should().NotBeNull();
    }

    [Fact]
    public void Profile_reports_preserve_deep_value_graphs_without_recursive_serialization()
    {
        const int depth = 10_000;
        PineValue value = PineValue.EmptyList;

        for (var index = 0; index < depth; index++)
            value = PineValue.List([value]);

        using var profile = new ElmTestInstrumentation(new() { DisablePrecompiledLeaves = true });
        var vm = profile.CreateVm(new ConcurrentInvocationCache(), new PineVMSharedCaches());
        vm.EvaluateExpression(Expression.LitralInst(value), PineValue.EmptyList).IsOkOrNull().Should().NotBeNull();

        var report = profile.GetReport();
        var hash = Convert.ToHexStringLower(new ConcurrentPineValueHashCache().GetHash(value).Span);

        for (var index = 0; index < depth; index++)
        {
            report.Values[hash].Items.Should().ContainSingle();
            hash = report.Values[hash].Items!.Single();
        }

        report.Values[hash].Items.Should().BeEmpty();
        report.ToJson().Should().Contain(hash);
    }

    [Fact]
    public async Task Nonprofiling_budgets_are_shared_across_parallel_VMs_without_collecting_expression_data()
    {
        using var budget =
            new ElmTestInstrumentation(
                new()
                {
                    InvocationBudget = 4,
                    DisablePrecompiledLeaves = true,
                },
                recordDiagnostics: false);

        var recursive =
            Expression.ListInst(
                [
                new Expression.Eval(Expression.EnvironmentInstance, Expression.EnvironmentInstance),
                Expression.EmptyList
                ]);

        var encoded = ExpressionEncoding.EncodeExpressionAsValue(recursive);
        var caches = new PineVMSharedCaches();

        await Task.WhenAll(
            Enumerable.Range(0, 2).Select(
                _ => Task.Run(
                    () =>
                    {
                        Action evaluate =
                            () => budget.CreateVm(new ConcurrentInvocationCache(), caches)
                            .EvaluateExpression(recursive, encoded);

                        evaluate.Should().Throw<ElmTestInstrumentationStoppedException>();
                    })));

        var report = budget.GetReport();
        report.Summary.Counters.InvocationCount.Should().BeGreaterThan(4);
        report.Expressions.Should().BeEmpty();
        report.Samples.Should().BeEmpty();
    }

    [Fact]
    public void Invocation_budget_stops_nonterminating_eval_and_retains_partial_rankings()
    {
        using var profile =
            new ElmTestInstrumentation(
                new()
                {
                    InvocationBudget = 4,
                    DisablePrecompiledLeaves = true,
                    SnapshotInterval = TimeSpan.Zero,
                });

        using var scope = profile.EnterScope("execution", "recursive eval");
        var vm = profile.CreateVm(new ConcurrentInvocationCache(), new PineVMSharedCaches());

        var recursive =
            Expression.ListInst(
                [
                new Expression.Eval(Expression.EnvironmentInstance, Expression.EnvironmentInstance),
                Expression.EmptyList
                ]);

        var encoded = ExpressionEncoding.EncodeExpressionAsValue(recursive);
        Action evaluate = () => vm.EvaluateExpression(recursive, encoded);
        evaluate.Should().Throw<ElmTestInstrumentationStoppedException>();
        var report = profile.GetReport();
        report.Summary.Counters.InvocationCount.Should().Be(5);
        report.Summary.StopReason.Should().Contain("command limit 4");
        report.Expressions.Should().NotBeEmpty();
        report.Samples.Last().StackTrace.Should().NotBeEmpty();
    }

    [Fact]
    public void Wall_clock_budget_cooperatively_stops_recursive_eval()
    {
        using var profile =
            new ElmTestInstrumentation(
                new()
                {
                    Timeout = TimeSpan.FromMilliseconds(50),
                    DisablePrecompiledLeaves = true,
                });

        var recursive = new Expression.Eval(Expression.EnvironmentInstance, Expression.EnvironmentInstance);
        var encoded = ExpressionEncoding.EncodeExpressionAsValue(recursive);

        Action evaluate =
            () => profile.CreateVm(new ConcurrentInvocationCache(), new PineVMSharedCaches())
            .EvaluateExpression(recursive, encoded);

        evaluate.Should().Throw<ElmTestInstrumentationStoppedException>();
        profile.GetSummary().StopReason.Should().Contain("Time budget");
    }

    [Fact]
    public void Budgets_are_shared_across_evaluations_and_report_preserves_expression_graphs()
    {
        var announcedBeforeSerialization = false;

        using var profile =
            new ElmTestInstrumentation(
                new()
                {
                    InvocationBudget = 2,
                    DisablePrecompiledLeaves = true,
                    IncludeInputs = true,
                    IncludeLocals = true,
                })
            {
                OnStopped =
                summary =>
                {
                    announcedBeforeSerialization = true;
                    summary.Counters.InvocationCount.Should().Be(3);
                },
            };

        using var scope = profile.EnterScope("execution", "one test");
        var vm = profile.CreateVm(new ConcurrentInvocationCache(), new PineVMSharedCaches());
        var expression = new Expression.Eval(Expression.EnvironmentInstance, Expression.EmptyList);

        var body =
            ExpressionEncoding.EncodeExpressionAsValue(Expression.LitralInst(IntegerEncoding.EncodeSignedInteger(42)));

        vm.EvaluateExpression(expression, body).IsOkOrNull().Should().NotBeNull();
        Action next = () => vm.EvaluateExpression(expression, body);
        next.Should().Throw<ElmTestInstrumentationStoppedException>();
        announcedBeforeSerialization.Should().BeTrue();
        var report = profile.GetReport();
        report.Summary.Counters.InvocationCount.Should().Be(3);
        report.Summary.Counters.InstructionCount.Should().BeGreaterThan(0);
        report.Summary.Context.Should().Be("one test");
        report.Samples.Last().StackTrace.Should().NotBeEmpty();
        report.Expressions.Should().NotBeEmpty();

        foreach (var row in report.Expressions)
            report.Values.Should().ContainKey(row.Hash);

        foreach (var node in report.Values.Values.Where(node => node.Items is not null))
            foreach (var item in node.Items!)
                report.Values.Should().ContainKey(item);
    }

    [Fact]
    public void User_cancellation_is_explicit_and_does_not_start_evaluation()
    {
        using var profile = new ElmTestInstrumentation(new() { DisablePrecompiledLeaves = true });
        profile.Cancel();

        Action evaluate =
            () => profile.CreateVm(new ConcurrentInvocationCache(), new PineVMSharedCaches())
            .EvaluateExpression(Expression.EnvironmentInstance, PineValue.EmptyList);

        evaluate.Should().Throw<ElmTestInstrumentationStoppedException>().WithMessage("*cancellation*");
        profile.CancelledByUser.Should().BeTrue();
        profile.GetReport().Summary.Outcome.Should().Be("cancelled");
    }

    [Theory]
    [InlineData(0)]
    [InlineData(-1)]
    public void Invalid_budgets_are_rejected(int budget)
    {
        Action create = () => new ElmTestInstrumentation(new() { InvocationBudget = budget });
        create.Should().Throw<ArgumentException>();
    }
}
