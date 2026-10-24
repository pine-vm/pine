using AwesomeAssertions;
using Pine.Core.Elm.Testing;
using Pine.Core.Functional;
using Pine.Core.PineLanguage;
using Pine.Core.Runtime;
using Pine.Core.Runtime.IntermediateVM;
using Pine.Core.Tests.Runtime.IntermediateVM;
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Threading;
using System.Threading.Tasks;
using Xunit;

namespace Pine.Core.Tests.Elm.ElmTest;

public class ElmTestRunCacheSessionTests(ITestOutputHelper output)
{
    [Fact]
    public void Cache_session_defaults_to_legacy_unbounded_storage()
    {
        using var session = new ElmTestRunCacheSession();
        session.InvocationCache.Should().BeSameAs(session.InvocationResults);
        session.GenerationalInvocations.Should().BeNull();
        session.CacheLearning.Should().BeNull();
        session.InvocationCacheConfiguration.Should().BeNull();
        session.GetCacheStatisticsSnapshot().SharedCacheGenerations.Should().BeNull();
    }

    [Fact]
    public async Task Cache_session_root_boundaries_rotate_promote_and_retire_generations()
    {
        await using var session = new ElmTestRunCacheSession(generationOptions: new(2, 2));
        var shared = session.GenerationalInvocations!;
        session.InvocationCache.Should().BeSameAs(shared);
        var hot = GenerationalInvocationCacheTests.BuildKey(1);
        var cold = GenerationalInvocationCacheTests.BuildKey(2);
        var creations = 0;

        var context =
            session.CreateExecutionContext(
                cache =>
                {
                    ++creations;

                    return
                        new CallbackVm(
                            (_, environment) =>
                            {
                                var key =
                                    new EvalCacheEntryKey(
                                        environment,
                                        StackFrameInput.GenericFromEnvironmentValue(environment));

                                if (!cache.TryGet(key, out var value))
                                {
                                    value = environment;
                                    cache.TryAdd(key, value);
                                }

                                return Result<string, PineValue>.ok(value);
                            });
                });

        context.Run(
            vm =>
            {
                vm.EvaluateExpression(Expression.EnvironmentInstance, hot.ExprValue);
                vm.EvaluateExpression(Expression.EnvironmentInstance, cold.ExprValue);
                shared.PhysicalEntryCount.Should().Be(0, "completed frames stay private until the root exits");
                return 0;
            });

        await WaitUntil(() => shared.GetStatistics().Rotations == 1);
        shared.GetStatistics().EntriesPerGeneration.Should().Equal(0, 2);

        context.Run(
            vm =>
            {
                vm.EvaluateExpression(Expression.EnvironmentInstance, hot.ExprValue);

                vm.EvaluateExpression(
                    Expression.EnvironmentInstance,
                    GenerationalInvocationCacheTests.BuildKey(3).ExprValue);

                return 0;
            });

        await WaitUntil(() => shared.GetStatistics().Rotations == 2);
        shared.MayContainExpression(hot.ExprValue).Should().BeTrue();
        shared.MayContainExpression(cold.ExprValue).Should().BeFalse();
        shared.GetStatistics().Promotions.Should().Be(1);

        context.Run(
            vm =>
            {
                vm.EvaluateExpression(
                    Expression.EnvironmentInstance,
                    GenerationalInvocationCacheTests.BuildKey(4).ExprValue);

                vm.EvaluateExpression(
                    Expression.EnvironmentInstance,
                    GenerationalInvocationCacheTests.BuildKey(5).ExprValue);

                return 0;
            });

        await WaitUntil(() => shared.GetStatistics().Rotations == 3);
        shared.MayContainExpression(hot.ExprValue).Should().BeFalse();
        shared.GetStatistics().RetiredEntries.Should().Be(4);
        session.InvocationResults.Count.Should().Be(0);
        var snapshot = session.GetCacheStatisticsSnapshot();
        snapshot.SharedCacheGenerations!.UniqueEntryCount.Should().Be(2);
        snapshot.Totals.HitCount.Should().Be(1);
        creations.Should().Be(3);
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public async Task Cache_session_failed_or_cancelled_root_publishes_completed_frames(bool cancel)
    {
        await using var session = new ElmTestRunCacheSession(generationOptions: new(2, 1));
        var key = GenerationalInvocationCacheTests.BuildKey(1);

        var context =
            session.CreateExecutionContext(
                cache =>
                new CallbackVm(
                    (_, _) =>
                    {
                        cache.TryAdd(key, PineValue.EmptyList).Should().BeTrue();
                        session.GenerationalInvocations!.PhysicalEntryCount.Should().Be(0);
                        throw cancel ? new OperationCanceledException() : new InvalidOperationException("failed root");
                    }));

        Action run =
            () => context.Run(vm => vm.EvaluateExpression(Expression.EnvironmentInstance, PineValue.EmptyList));

        run.Should().Throw<Exception>();
        await WaitUntil(() => session.GenerationalInvocations!.GetStatistics().Rotations == 1);
        session.InvocationCache.TryGet(key, out _).Should().BeTrue();
        session.GetCacheStatisticsSnapshot().Totals.AdmissionCount.Should().Be(1);
    }

    [Fact]
    public async Task Cache_session_policy_switch_is_pinned_per_root_and_retains_denied_entries()
    {
        var key = GenerationalInvocationCacheTests.BuildKey(1);
        var enabled = new InvocationCachePolicy([key.ExprValue]);
        var nextPolicy = InvocationCachePolicy.Empty;

        var learning =
            new ContinuousInvocationCacheLearning(
                (state, _, _) => Task.FromResult(state with { Policy = Volatile.Read(ref nextPolicy) }),
                options: new(MinimumInterval: TimeSpan.FromMilliseconds(5)));

        await using var session = new ElmTestRunCacheSession(generationOptions: new(), cacheLearning: learning);
        session.InvocationCache.TryAdd(key, PineValue.EmptyList);
        IInvocationCacheAccess? currentCache = null;

        var context =
            session.CreateExecutionContext(
                cache =>
                {
                    currentCache = cache;
                    return new CallbackVm((_, _) => Result<string, PineValue>.ok(PineValue.EmptyList));
                });

        async Task SwitchTo(InvocationCachePolicy policy)
        {
            Volatile.Write(ref nextPolicy, policy);
            learning.Record(new(Expression.EnvironmentInstance, PineValue.EmptyList));
            learning.Record(new(Expression.EnvironmentInstance, PineValue.EmptyList));
            await WaitUntil(() => learning.CurrentPolicy == policy);
        }

        await context.RunAsync(
            async vm =>
            {
                currentCache!.TryGet(key, out _).Should().BeFalse();
                await SwitchTo(enabled);
                currentCache.TryGet(key, out _).Should().BeFalse("the root captured the empty policy");
                return 0;
            });

        context.Run(vm => currentCache!.TryGet(key, out _)).Should().BeTrue();
        await SwitchTo(InvocationCachePolicy.Empty);
        context.Run(vm => currentCache!.TryGet(key, out _)).Should().BeFalse();
        session.GenerationalInvocations!.GetStatistics().UniqueEntryCount.Should().Be(1);
        await SwitchTo(enabled);
        context.Run(vm => currentCache!.TryGet(key, out _)).Should().BeTrue();
        session.InvocationCacheConfiguration.Should().Be(new InvocationCacheConfiguration(0, 0, 0, 0));
        await session.DisposeAsync();
        learning.GetSnapshot().IsDisposed.Should().BeTrue();
    }

    [Fact]
    public async Task Cache_session_samples_preparation_and_workers_across_real_Elm_runs_without_sharing_budgets()
    {
        using var project =
            new ElmFuzzTests.Project(
                """
                total n accumulator =
                    if n == 0 then accumulator else total (n - 1) (accumulator + n)
                suite = Test.describe "learning"
                    [ Test.test "first" (\_ -> Expect.equal (total 100 0) 5050)
                    , Test.test "second" (\_ -> Expect.equal (total 101 0) 5151)
                    ]
                """);

        var learning =
            new ContinuousInvocationCacheLearning(
                (state, _, _) => Task.FromResult(state),
                options: new(MinimumInterval: TimeSpan.FromHours(1)));

        await using var session = new ElmTestRunCacheSession(generationOptions: new(), cacheLearning: learning);

        ElmTestRunner.CompileAndRunTests(session, project.Directory, 2, listTests: true)
            .Should().BeOfType<ElmTestRun.Listed>();

        var preparationSamples = learning.GetSnapshot().AcceptedCount;
        preparationSamples.Should().BeGreaterThanOrEqualTo(2);

        ElmTestRunner.CompileAndRunTests(session, project.Directory, 2)
            .Should().BeOfType<ElmTestRun.Completed>().Which.Tests
            .Should().HaveCount(2).And.OnlyContain(test => test.Kind == CompletedTestKind.Passed);

        learning.GetSnapshot().AcceptedCount.Should().BeGreaterThanOrEqualTo(preparationSamples * 2 + 2);
        var beforeBudget = learning.GetSnapshot().AcceptedCount;

        using var budget = new ElmTestInstrumentation(new(), recordDiagnostics: false);
        budget.GetSummary().Counters.Should().Be(default(EvalWorkCounters));

        ElmTestRunner.CompileAndRunTests(session, project.Directory, 2, instrumentation: budget)
            .Should().BeOfType<ElmTestRun.Completed>().Which.Tests
            .Should().HaveCount(2).And.OnlyContain(test => test.Kind == CompletedTestKind.Passed);

        learning.GetSnapshot().AcceptedCount.Should().BeGreaterThanOrEqualTo(beforeBudget + preparationSamples + 2);
        budget.GetSummary().CountersByPhase["preparation"].Total.InstructionCount.Should().BeGreaterThan(0);
        budget.GetSummary().CountersByPhase["execution"].Total.LoopIterationCount.Should().BeGreaterThanOrEqualTo(201);
        learning.State.Should().BeSameAs(InvocationCacheLearningState.Initial);

        session.GenerationalInvocations!.PhysicalEntryCount.Should().Be(
            0,
            "learning starts with an empty admission policy");

        session.InvocationResults.Count.Should().Be(0);
    }

    [Fact]
    public async Task Cache_session_disposes_and_awaits_active_learning()
    {
        var entered = new TaskCompletionSource(TaskCreationOptions.RunContinuationsAsynchronously);
        var stopped = new TaskCompletionSource(TaskCreationOptions.RunContinuationsAsynchronously);

        var learning =
            new ContinuousInvocationCacheLearning(
                async (state, _, token) =>
                {
                    entered.TrySetResult();

                    try
                    {
                        await Task.Delay(Timeout.InfiniteTimeSpan, token);
                        return state;
                    }
                    finally
                    {
                        stopped.TrySetResult();
                    }
                },
                options: new(MinimumInterval: TimeSpan.FromMilliseconds(5)));

        await using var session = new ElmTestRunCacheSession(cacheLearning: learning);

        var context =
            session.CreateExecutionContext(
                _ => new CallbackVm((_, _) => Result<string, PineValue>.ok(PineValue.EmptyList)));

        for (var index = 0; index < 2; ++index)
            context.Run(vm => vm.EvaluateExpression(Expression.EnvironmentInstance, PineValue.EmptyList));

        await entered.Task.WaitAsync(TimeSpan.FromSeconds(20));
        session.Dispose();
        await session.DisposeAsync();
        stopped.Task.IsCompletedSuccessfully.Should().BeTrue();
        learning.GetSnapshot().IsDisposed.Should().BeTrue();
        learning.GetSnapshot().IsLearning.Should().BeFalse();
        learning.Record(new(Expression.EnvironmentInstance, PineValue.EmptyList)).Should().BeFalse();
    }

    private static async Task WaitUntil(Func<bool> condition)
    {
        using var timeout = new CancellationTokenSource(TimeSpan.FromSeconds(20));

        while (!condition())
            await Task.Delay(5, timeout.Token);
    }

    private sealed class CallbackVm(
        Func<Expression, PineValue, Result<string, PineValue>> evaluate) : IPineVM
    {
        public Result<string, PineValue> EvaluateExpression(Expression expression, PineValue environment) =>
            evaluate(expression, environment);
    }

    [Fact]
    public void Cache_session_overload_preserves_old_null_VM_and_factory_calls()
    {
        var project = Path.Combine("artifacts", "cache-overload-tests", Guid.NewGuid().ToString("N"));
        Directory.CreateDirectory(project);

        try
        {
            ElmTestRunner.CompileAndRunTests(project, null).Should().BeOfType<ElmTestRun.NoTestModules>();
            ElmTestRunner.CompileAndRunTests(project, null, null).Should().BeOfType<ElmTestRun.NoTestModules>();

            ElmTestRunner.CompileAndRunTests(appDirectory: project, pineVm: null)
                .Should().BeOfType<ElmTestRun.NoTestModules>();

            Action nullFactory = () => ElmTestRunner.CompileAndRunTests(project, 1, null!);
            nullFactory.Should().Throw<ArgumentNullException>();
        }
        finally
        {
            Directory.Delete(project, recursive: true);
        }
    }

    [Fact]
    public void Diagnostic_profiles_keep_each_test_and_each_run_cold()
    {
        using var project =
            new ElmFuzzTests.Project(
                """
                total n accumulator =
                    if n == 0 then accumulator else total (n - 1) (accumulator + n)
                suite = Test.describe "isolated"
                    [ Test.test "first" (\_ -> Expect.equal (total 5000 0) 12502500)
                    , Test.test "second" (\_ -> Expect.equal (total 5000 0) 12502500)
                    ]
                """);

        long? previousInstructions = null;

        for (var attempt = 0; attempt < 2; ++attempt)
        {
            using var profile = new ElmTestInstrumentation(new() { SnapshotInterval = TimeSpan.Zero });

            ElmTestRunner.CompileAndRunTests(
                project.Directory,
                fuzzOptions: new() { Seed = 42, Runs = 1 },
                instrumentation: profile)
                .Should().BeOfType<ElmTestRun.Completed>().Which.Tests.Should().HaveCount(2)
                .And.OnlyContain(test => test.Kind == CompletedTestKind.Passed);

            var report = profile.GetReport();
            var counters = report.Summary.CountersByPhase["execution"].Total;
            counters.LoopIterationCount.Should().BeGreaterThanOrEqualTo(10000);

            if (previousInstructions is { } previous)
                counters.InstructionCount.Should().Be(previous);

            previousInstructions = counters.InstructionCount;
        }
    }

    [Theory]
    [InlineData(1)]
    [InlineData(3)]
    public void Cache_session_reuses_preparation_and_execution_work_with_fresh_accounting(int workers)
    {
        using var project =
            new ElmFuzzTests.Project(
                """
                total n accumulator =
                    if n == 0 then accumulator else total (n - 1) (accumulator + n)
                suite = Test.describe "cached"
                    [ Test.test "first" (\_ -> Expect.equal (total 5000 0) 12502500)
                    , Test.test "second" (\_ -> Expect.equal (total 5001 0) 12507501)
                    , Test.fuzz Fuzz.int "fuzzed" (\value -> Expect.equal value value)
                    ]
                """);

        var session = new ElmTestRunCacheSession();
        var caches = session.VmCaches;
        var invocations = session.InvocationResults;

        using var firstBudget = new ElmTestInstrumentation(new(), recordDiagnostics: false);

        var first =
            ElmTestRunner.CompileAndRunTests(
                session,
                project.Directory,
                workers,
                fuzzOptions: new() { Seed = 42, Runs = 3 },
                instrumentation: firstBudget)
            .Should().BeOfType<ElmTestRun.Completed>().Which;

        first.Tests.Should().HaveCount(3).And.OnlyContain(test => test.Kind == CompletedTestKind.Passed);
        var firstCounters = firstBudget.GetSummary();
        var encodedCount = caches.EncodedExpressions.Count;
        var reducedCount = caches.ReducedExpressions.Count;
        var parsedCount = caches.ParsedExpressions.Count;
        var compiledCount = caches.ExpressionCompilations.Count;
        var templateCount = caches.ExpressionTemplatePlans.Count;
        var sourceParsing = session.SourceParsing.ToArray();
        var canonicalization = session.DeclarationCanonicalization.Count;
        var invocationCount = invocations.Count;
        encodedCount.Should().BeGreaterThan(0);
        reducedCount.Should().BeGreaterThan(0);
        parsedCount.Should().BeGreaterThan(0);
        compiledCount.Should().BeGreaterThan(0);
        templateCount.Should().BeGreaterThan(0);
        sourceParsing.Should().NotBeEmpty();
        canonicalization.Should().BeGreaterThan(0);
        invocationCount.Should().BeGreaterThan(0);

        using var secondBudget = new ElmTestInstrumentation(new(), recordDiagnostics: false);
        secondBudget.GetSummary().Counters.Should().Be(default(EvalWorkCounters));

        var second =
            ElmTestRunner.CompileAndRunTests(
                session,
                project.Directory,
                workers,
                fuzzOptions: new() { Seed = 42, Runs = 3 },
                instrumentation: secondBudget)
            .Should().BeOfType<ElmTestRun.Completed>().Which;

        session.VmCaches.Should().BeSameAs(caches);
        session.InvocationResults.Should().BeSameAs(invocations);
        second.Should().NotBeSameAs(first);
        second.Tests.Should().BeEquivalentTo(first.Tests, options => options.WithStrictOrdering());
        caches.EncodedExpressions.Count.Should().Be(encodedCount);
        caches.ReducedExpressions.Count.Should().Be(reducedCount);
        caches.ParsedExpressions.Count.Should().Be(parsedCount);
        caches.ExpressionCompilations.Count.Should().Be(compiledCount);
        caches.ExpressionTemplatePlans.Count.Should().Be(templateCount);
        session.SourceParsing.Count.Should().Be(sourceParsing.Length);
        session.DeclarationCanonicalization.Count.Should().Be(canonicalization);

        foreach (var entry in sourceParsing)
            session.SourceParsing[entry.Key].Should().BeSameAs(entry.Value);

        // Cost-based admission can add different completed frames after inner calls become cache hits.
        invocations.Count.Should().BeGreaterThanOrEqualTo(invocationCount);
        secondBudget.GetSummary().Counters.InstructionCount.Should().BeLessThan(firstCounters.Counters.InstructionCount);

        secondBudget.GetSummary().CountersByPhase["preparation"].Total.InstructionCount
            .Should().BeLessThan(firstCounters.CountersByPhase["preparation"].Total.InstructionCount);

        secondBudget.GetSummary().CountersByPhase["execution"].Total.LoopIterationCount
            .Should().BeLessThan(firstCounters.CountersByPhase["execution"].Total.LoopIterationCount);

        secondBudget.GetSummary().CountersByPhase.Values.Sum(
            origins =>
            origins.CachedWorkCounters.ExpressionTemplatePlanParseCount).Should().Be(0);

        var secondCounters = secondBudget.GetSummary();

        output.WriteLine(
            $"Workers={workers}; instructions cold={firstCounters.Counters.InstructionCount}, warm={secondCounters.Counters.InstructionCount}; " +
            $"loops cold={firstCounters.Counters.LoopIterationCount}, warm={secondCounters.Counters.LoopIterationCount}; " +
            $"preparation instructions cold={firstCounters.CountersByPhase["preparation"].Total.InstructionCount}, " +
            $"warm={secondCounters.CountersByPhase["preparation"].Total.InstructionCount}; " +
            $"execution instructions cold={firstCounters.CountersByPhase["execution"].Total.InstructionCount}, " +
            $"warm={secondCounters.CountersByPhase["execution"].Total.InstructionCount}; " +
            $"total milliseconds cold={first.Duration.TotalMilliseconds:F2}, warm={second.Duration.TotalMilliseconds:F2}");

        // Normal execution and budget tracking compile compatible VMs into the same session.
        ElmTestRunner.CompileAndRunTests(
            session,
            project.Directory,
            workers,
            fuzzOptions: new() { Seed = 42, Runs = 3 }).Should().BeOfType<ElmTestRun.Completed>()
            .Which.Tests.Should().BeEquivalentTo(first.Tests, options => options.WithStrictOrdering());
    }

    [Fact]
    public void Cache_session_does_not_stale_source_callee_filter_seed_or_fuzz_changes()
    {
        using var project =
            new ElmFuzzTests.Project(
                """
                suite = Test.describe "changed"
                    [ Test.test "callee" (\_ -> Expect.equal (Helper.answer 0) 1)
                    , Test.fuzz Fuzz.int "random" (\value -> Expect.fail (String.fromInt value))
                    ]
                """);

        var path = Path.Combine(project.Directory, "tests", "Tests.elm");
        File.WriteAllText(path, File.ReadAllText(path).Replace("import Test\n", "import Test\nimport Helper\n"));
        var helper = Path.Combine(project.Directory, "tests", "Helper.elm");
        File.WriteAllText(helper, "module Helper exposing (answer)\nanswer ignored = 1");
        var session = new ElmTestRunCacheSession();

        ElmTestRun.Completed Run(uint seed = 42, uint fuzz = 1, string? filter = null) =>
            ElmTestRunner.CompileAndRunTests(
                session,
                project.Directory,
                1,
                filter: filter,
                fuzzOptions: new() { Seed = seed, Runs = fuzz }).Should().BeOfType<ElmTestRun.Completed>().Which;

        var initial = Run();
        initial.Tests[0].Kind.Should().Be(CompletedTestKind.Passed);
        var random = initial.Tests[1].Fuzz!;
        Run().Tests.Should().BeEquivalentTo(initial.Tests, options => options.WithStrictOrdering());
        var reseeded = Run(seed: 123);
        reseeded.Tests[1].Fuzz!.OriginalInput.Should().NotBe(random.OriginalInput);
        reseeded.ExecutionSettings!.Seed.Should().Be(123);
        var refuzzed = Run(fuzz: 7, filter: "random");
        refuzzed.Tests.Should().ContainSingle().Which.Fuzz!.RunsRequested.Should().Be(7);

        File.WriteAllText(helper, "module Helper exposing (answer)\nanswer ignored = 2");
        Run(filter: "callee").Tests.Should().ContainSingle().Which.Kind.Should().Be(CompletedTestKind.Failed);
        File.WriteAllText(path, File.ReadAllText(path).Replace("(Helper.answer 0) 1", "(Helper.answer 0) 2"));
        Run(filter: "callee").Tests.Should().ContainSingle().Which.Kind.Should().Be(CompletedTestKind.Passed);
        File.WriteAllText(helper, "module Helper exposing (answer)\nanswer ignored = 1");
        Run(filter: "callee").Tests.Should().ContainSingle().Which.Kind.Should().Be(CompletedTestKind.Failed);

        File.WriteAllText(helper, "module Helper exposing (other)\nother ignored = 0\nanswer ignored = 2");
        Action privateCallee = () => Run(filter: "callee");
        privateCallee.Should().Throw<Core.Elm.ElmCompilerInDotnet.ElmCompilationException>();
        File.WriteAllText(helper, "module Helper exposing (answer)\nanswer ignored = 2");
        Run(filter: "callee").Tests.Should().ContainSingle().Which.Kind.Should().Be(CompletedTestKind.Passed);
    }

    [Fact]
    public void Cache_session_stopped_execution_and_cancelled_preparation_can_retry()
    {
        using var project =
            new ElmFuzzTests.Project(
                """
                spin n = spin (n + 1)
                suite = Test.test "stopped" (\_ -> spin 0)
                """);

        var session = new ElmTestRunCacheSession();

        using var cancelled = new ElmTestInstrumentation(new(), recordDiagnostics: false);
        cancelled.Cancel();

        Action cancelledRun =
            () => ElmTestRunner.CompileAndRunTests(session, project.Directory, 1, instrumentation: cancelled);

        cancelledRun.Should().Throw<ElmTestInstrumentationStoppedException>();
        session.InvocationResults.Count.Should().Be(0);

        for (var attempt = 0; attempt < 2; ++attempt)
        {
            using var budget = new ElmTestInstrumentation(new() { LoopBudget = 100 }, recordDiagnostics: false);
            Action run = () => ElmTestRunner.CompileAndRunTests(session, project.Directory, 1, instrumentation: budget);
            run.Should().Throw<ElmTestInstrumentationStoppedException>();
            budget.GetSummary().Counters.LoopIterationCount.Should().BeGreaterThan(100);
            budget.GetSummary().StopReason.Should().Contain("Loop");
        }

        File.WriteAllText(
            Path.Combine(project.Directory, "tests", "Tests.elm"),
            "module Tests exposing (suite)\nimport Test\nimport Expect\nsuite = Test.test \"stopped\" (\\_ -> Expect.pass)");

        ElmTestRunner.CompileAndRunTests(session, project.Directory, 1).Should().BeOfType<ElmTestRun.Completed>()
            .Which.Tests.Should().ContainSingle().Which.Kind.Should().Be(CompletedTestKind.Passed);
    }

    [Theory]
    [InlineData("diagnostics")]
    [InlineData("reduction")]
    [InlineData("inlining")]
    [InlineData("tail")]
    [InlineData("fast-paths")]
    [InlineData("invocations")]
    [InlineData("leaves")]
    [InlineData("custom-leaves")]
    public void Cache_session_rejects_incompatible_diagnostic_and_compilation_controls(string control)
    {
        var options =
            new ElmTestInstrumentationOptions
            {
                DisableReduction = control == "reduction",
                DisableInlining = control == "inlining",
                DisableTailRecursion = control == "tail",
                DisableApplicationFastPaths = control == "fast-paths",
                DisableInvocationCache = control == "invocations",
                DisablePrecompiledLeaves = control == "leaves"
            };

        using var instrumentation =
            new ElmTestInstrumentation(
                options,
                precompiledLeaves: control == "custom-leaves" ? new Dictionary<PineValue, PrecompiledLeaf>() : null,
                recordDiagnostics: control == "diagnostics");

        var session = new ElmTestRunCacheSession();

        Action run =
            () => ElmTestRunner.CompileAndRunTests(session, "not-a-project", 1, instrumentation: instrumentation);

        run.Should().Throw<ArgumentException>().WithMessage("*cache session*");
        session.InvocationResults.Count.Should().Be(0);
        session.VmCaches.EncodedExpressions.Count.Should().Be(0);
    }
}
