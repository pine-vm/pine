using AwesomeAssertions;
using Pine.Core.Elm.Testing;
using Pine.Core.Interpreter.IntermediateVM;
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using Xunit;

namespace Pine.Core.Tests.Elm.ElmTest;

public class ElmTestRunCacheSessionTests(ITestOutputHelper output)
{
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
