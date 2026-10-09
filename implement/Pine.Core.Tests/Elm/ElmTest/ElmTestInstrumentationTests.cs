using AwesomeAssertions;
using Pine.Core.Addressing;
using Pine.Core.CodeAnalysis;
using Pine.Core.CodeGen;
using Pine.Core.CommonEncodings;
using Pine.Core.Elm.Testing;
using Pine.Core.Interpreter.IntermediateVM;
using Pine.Core.PineVM;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;
using Xunit;

namespace Pine.Core.Tests.Elm.ElmTest;

public class ElmTestInstrumentationTests
{
    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void Application_fast_paths_can_be_disabled_without_changing_nested_eval_results(bool disableFastPaths)
    {
        using var profile =
            new ElmTestInstrumentation(
                new()
                {
                    DisableApplicationFastPaths = disableFastPaths,
                    DisableInlining = true,
                    DisableReduction = true,
                    DisablePrecompiledLeaves = true,
                });

        var function =
            FunctionValueBuilder.TryBuildCurriedFunctionValueAsTemplate(
                ExpressionBuilder.BuildExpressionForPathInExpression([2], Expression.EnvironmentInstance),
                parameterCount: 2)!;

        var environment = PineValue.List([IntegerEncoding.EncodeSignedInteger(42)]);

        var expression =
            new Expression.Eval(
                new Expression.Eval(
                    Expression.LitralInst(function),
                    Expression.LitralInst(IntegerEncoding.EncodeSignedInteger(11))),
                Expression.LitralInst(environment));

        profile.CreateVm(new ConcurrentInvocationCache(), new PineVMSharedCaches())
            .EvaluateExpression(expression, PineValue.EmptyList).IsOkOrNull().Should().Be(environment);

        var report = profile.GetReport();
        report.Options.DisableApplicationFastPaths.Should().Be(disableFastPaths);

        if (disableFastPaths)
            report.Summary.Counters.DirectSaturatedApplicationCount.Should().Be(0);
    }

    [Fact]
    public void Nonreturning_loop_samples_identify_compiled_bodies_and_preserve_exact_exclusive_work()
    {
        ElmTestProfileSample? stoppedSample = null;

        using var profile =
            new ElmTestInstrumentation(
                new()
                {
                    LoopBudget = 4,
                    SnapshotInterval = TimeSpan.Zero,
                    DisablePrecompiledLeaves = true,
                    IncludeLocals = true,
                })
            {
                OnSample = sample => stoppedSample = sample,
            };

        var expression = Expression.EnvironmentInstance;

        profile.RegisterDeclarations(
            new ElmInteractiveEnvironment.ParsedInteractiveEnvironment(
                [
                ("Tests",
                PineValue.EmptyList,
                new(
                    new Dictionary<string, PineValue>
                    {
                        ["suite"] = ExpressionEncoding.EncodeExpressionAsValue(expression),
                    },
                    new Dictionary<string, PineValue>()))
                ]));

        var caches = LoopCaches(expression);

        Action evaluate =
            () => profile.CreateVm(new ConcurrentInvocationCache(), caches)
            .EvaluateExpression(expression, PineValue.EmptyList);

        evaluate.Should().Throw<ElmTestInstrumentationStoppedException>();
        stoppedSample.Should().NotBeNull();
        var frame = stoppedSample!.StackTrace.Single();
        frame.FrameIndex.Should().NotBeNull();
        frame.Declarations.Should().Equal("Tests.suite");
        frame.LoopIterationCount.Should().Be(5);
        frame.InstructionCount.Should().BeGreaterThan(0);
        frame.CompiledFrameId.Should().NotBeNull();

        var report = profile.GetReport();
        report.SchemaVersion.Should().Be(2);
        report.CompiledFrames[frame.CompiledFrameId!].Should().Contain("Jump_Const (-2, 0)");
        report.LoopSites.Should().ContainSingle();
        var loop = report.LoopSites.Single();
        loop.CompiledFrameId.Should().Be(frame.CompiledFrameId);
        loop.InstructionPointer.Should().Be(0, "the VM reports the destination after taking the backward jump");
        loop.Iterations.Should().Be(5);
        report.Expressions.Single().LoopIterations.Should().Be(5);
        report.Expressions.Single().Instructions.Should().Be(report.Summary.Counters.InstructionCount);
        report.Samples.Should().ContainSingle();
    }

    [Fact]
    public void Profile_sample_retention_is_bounded_without_losing_the_stop_or_aggregate_work()
    {
        var liveSamples = 0;
        var progressReports = 0;

        using var profile =
            new ElmTestInstrumentation(
                new()
                {
                    LoopBudget = 50,
                    MaxSamples = 2,
                    SnapshotInterval = TimeSpan.FromTicks(1),
                    DisablePrecompiledLeaves = true,
                })
            {
                OnSample = _ => liveSamples++,
                OnProgress = _ => progressReports++,
            };

        using var scope = profile.EnterScope("preparation", "loop");
        var expression = Expression.EnvironmentInstance;

        Action evaluate =
            () => profile.CreateVm(new ConcurrentInvocationCache(), LoopCaches(expression))
            .EvaluateExpression(expression, PineValue.EmptyList);

        evaluate.Should().Throw<ElmTestInstrumentationStoppedException>();

        var report = profile.GetReport();
        report.Samples.Should().HaveCount(2);
        report.DroppedSamples.Should().Be(liveSamples - 2).And.BeGreaterThan(0);
        report.Samples.Last().Summary.StopReason.Should().Contain("LoopIterationCount");
        report.Samples.Last().StackTrace.Single().LoopIterationCount.Should().Be(51);
        report.Expressions.Single().LoopIterations.Should().Be(51);
        report.Expressions.Single().Instructions.Should().Be(report.Summary.Counters.InstructionCount);
        progressReports.Should().Be(1, "live samples should not emit duplicate aggregate progress reports");
    }

    [Fact]
    public void Returning_frames_do_not_double_count_work_already_attributed_at_backward_jumps()
    {
        var expression = Expression.EnvironmentInstance;

        var instructions =
            new StackFrameInstructions(
                StaticFunctionInterface.Generic,
                [
                StackInstruction.Local_Set_Literal(1, PineKernelValues.FalseValue),
                StackInstruction.Local_Get(1),
                StackInstruction.Jump_If_True(3),
                StackInstruction.Local_Set_Literal(1, PineKernelValues.TrueValue),
                StackInstruction.Jump_Unconditional(-3),
                StackInstruction.Push_Literal(PineValue.EmptyList),
                StackInstruction.Return,
                ],
                null);

        var caches = new PineVMSharedCaches();

        caches.ExpressionCompilations.GetOrAdd(
            expression,
            () => new(new(instructions, []), new string('0', 64), null));

        using var profile = new ElmTestInstrumentation(new() { DisablePrecompiledLeaves = true });

        profile.CreateVm(new ConcurrentInvocationCache(), caches).EvaluateExpression(expression, PineValue.EmptyList)
            .IsOkOrNull().Should().NotBeNull();

        var report = profile.GetReport();
        report.Expressions.Single().LoopIterations.Should().Be(1);
        report.Expressions.Single().Instructions.Should().Be(report.Summary.Counters.InstructionCount);
    }

    private static PineVMSharedCaches LoopCaches(Expression expression)
    {
        var caches = new PineVMSharedCaches();
        var instructions = new UnhashableFrameInstructions();

        caches.ExpressionCompilations.GetOrAdd(
            expression,
            () => new(new(instructions, []), new string('0', 64), null));

        return caches;
    }

    private sealed record UnhashableFrameInstructions() : StackFrameInstructions(
        StaticFunctionInterface.Generic,
        [
        StackInstruction.Push_Literal(PineValue.EmptyList),
        StackInstruction.Pop,
        StackInstruction.Jump_Unconditional(-2),
        ],
        null)
    {
        public override int GetHashCode() =>
            throw new InvalidOperationException("Profiling must not structurally hash compiled bodies at loop boundaries.");
    }

    [Fact]
    public void Structural_value_previews_capture_children_without_materializing_the_parent()
    {
        var expression = Expression.EnvironmentInstance;

        var instructions =
            new StackFrameInstructions(
                StaticFunctionInterface.Generic,
                [
                StackInstruction.Push_Literal(IntegerEncoding.EncodeSignedInteger(42)),
                StackInstruction.Push_Literal(PineValue.EmptyList),
                StackInstruction.Build_List(2),
                StackInstruction.Local_Set([1], popCount: 1),
                StackInstruction.Push_Literal(PineValue.EmptyList),
                StackInstruction.Pop,
                StackInstruction.Jump_Unconditional(-2),
                ],
                null);

        var caches = new PineVMSharedCaches();

        caches.ExpressionCompilations.GetOrAdd(
            expression,
            () => new(new(instructions, []), new string('0', 64), null));

        using var profile =
            new ElmTestInstrumentation(
                new()
                {
                    LoopBudget = 1,
                    SnapshotInterval = TimeSpan.Zero,
                    IncludeLocals = true,
                    DisablePrecompiledLeaves = true,
                });

        Action evaluate =
            () => profile.CreateVm(new ConcurrentInvocationCache(), caches)
            .EvaluateExpression(expression, PineValue.EmptyList);

        evaluate.Should().Throw<ElmTestInstrumentationStoppedException>();

        var report = profile.GetReport();
        var value = report.Samples.Single().StackTrace.Single().Locals![1];
        value.ValueHash.Should().BeNull("capturing structural children must not materialize their parent");
        value.ItemCount.Should().Be(2);
        value.Items.Should().HaveCount(2);
        value.Items![0].Preview.Should().Be("integer 42");

        foreach (var child in value.Items)
            report.Values.Should().ContainKey(child.ValueHash!);
    }

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

    [Theory]
    [InlineData(0)]
    [InlineData(-1)]
    public void Invalid_sample_retention_limits_are_rejected(int maxSamples)
    {
        Action create = () => new ElmTestInstrumentation(new() { MaxSamples = maxSamples });
        create.Should().Throw<ArgumentException>();
    }
}
