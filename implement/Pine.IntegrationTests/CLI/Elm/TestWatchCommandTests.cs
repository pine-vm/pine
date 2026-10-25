using AwesomeAssertions;
using Pine.CLI;
using Pine.CLI.Elm;
using Pine.Core.CLI;
using Pine.Core.Elm.Testing;
using Pine.Core.PineLanguage;
using Pine.Core.Runtime;
using Pine.Core.Runtime.IntermediateVM;
using Spectre.Console;
using System;
using System.Collections.Generic;
using System.CommandLine;
using System.IO;
using System.Text;
using System.Text.RegularExpressions;
using System.Threading;
using System.Threading.Channels;
using System.Threading.Tasks;
using Xunit;

namespace Pine.IntegrationTests.CLI.Elm;

public class TestWatchCommandTests
{
    private static readonly TimeSpan s_wait = TimeSpan.FromSeconds(60);

    [Fact]
    public async Task Watch_factory_uses_generational_storage_and_starts_learning_with_empty_policy()
    {
        var configured = new TaskCompletionSource<string>(TaskCreationOptions.RunContinuationsAsynchronously);

        await using var session =
            TestCommand.CreateWatchCacheSession(
                message =>
                {
                    if (message.StartsWith("Invocation-cache learning configured:", StringComparison.Ordinal))
                        configured.TrySetResult(message);
                });

        session.GenerationalInvocations.Should().NotBeNull();
        session.InvocationCache.Should().BeSameAs(session.GenerationalInvocations);
        session.GenerationalInvocations!.Options.MaximumGenerationCount.Should().Be(3);
        session.GenerationalInvocations.Options.RotationEntryCount.Should().Be(5_000);
        session.InvocationResults.Count.Should().Be(0);
        session.CacheLearning.Should().NotBeNull();
        session.CacheLearning!.CurrentPolicy.Should().BeSameAs(InvocationCachePolicy.Empty);
        var snapshot = session.CacheLearning.GetSnapshot();
        snapshot.AcceptedCount.Should().Be(0);
        snapshot.RunCount.Should().Be(0);
        snapshot.SwitchCount.Should().Be(0);
        snapshot.IsDisposed.Should().BeFalse();

        (await configured.Task.WaitAsync(s_wait)).Should()
            .Contain("interval-ms=30000").And.Contain("sample-capacity=8").And.Contain("pending-capacity=16");
    }

    [Fact]
    public void One_shot_runs_keep_unbounded_cache_without_creating_watch_learning()
    {
        using var directory = new TestDirectory();
        Directory.CreateDirectory(Path.Combine(directory.Path, "tests"));
        File.WriteAllText(Path.Combine(directory.Path, "elm.json"), ElmJson);
        File.WriteAllText(Path.Combine(directory.Path, "tests", "Tests.elm"), FuzzTests);
        var output = new StringWriter();
        var console = CreateConsole(output);
        ElmTestRunCacheSession? captured = null;

        TestCommand.ExecuteCore(
            directory.Path,
            watch: false,
            console: console,
            errorConsole: console,
            offline: true,
            filter: "selected",
            seed: 42,
            fuzz: 3,
            workers: 1,
            onCacheSession: session => captured = session,
            createWatchCacheSession:
            () => throw new InvalidOperationException("One-shot must not create watch learning"))
            .Should().Be(0);

        output.ToString().Should().Contain("TEST RUN PASSED");
        captured.Should().NotBeNull();
        using var session = captured!;
        session.InvocationCache.Should().BeSameAs(session.InvocationResults);
        session.InvocationResults.Count.Should().BeGreaterThan(0);
        session.GenerationalInvocations.Should().BeNull();
        session.CacheLearning.Should().BeNull();
    }

    [Theory]
    [InlineData("cancel")]
    [InlineData("overflow")]
    [InlineData("start-failure")]
    public async Task Watch_disposes_its_owned_cache_session_on_cancellation_and_watcher_errors(string stop)
    {
        using var directory = new TestDirectory();
        Directory.CreateDirectory(Path.Combine(directory.Path, "tests"));
        File.WriteAllText(Path.Combine(directory.Path, "elm.json"), ElmJson);
        File.WriteAllText(Path.Combine(directory.Path, "tests", "Tests.elm"), "not a valid Elm module");
        using var session = CreateSmallWatchCacheSession();
        using var cancellation = new CancellationTokenSource(s_wait);
        var output = new NotifyingWriter();
        var console = CreateConsole(output);

        var watcher =
            new FakeTestWatcher(
                onStart: stop == "start-failure" ? () => throw new IOException("Simulated watcher start failure") : null);

        var factoryCalls = 0;

        var watching =
            Task.Run(
                () => TestCommand.ExecuteCore(
                    directory.Path,
                    watch: true,
                    console: console,
                    errorConsole: console,
                    offline: true,
                    cancellationToken: cancellation.Token,
                    createWatcher: _ => watcher,
                    createWatchCacheSession:
                    () =>
                    {
                        Interlocked.Increment(ref factoryCalls);
                        return session;
                    }));

        try
        {
            if (stop != "start-failure")
            {
                await output.NextContaining("Failed parsing Elm test module");
                session.CacheLearning!.GetSnapshot().IsDisposed.Should().BeFalse();

                if (stop == "cancel")
                    cancellation.Cancel();

                else
                    watcher.RaiseError(new InternalBufferOverflowException("Simulated watcher buffer overflow"));
            }

            (await watching.WaitAsync(s_wait)).Should().Be(stop == "cancel" ? 130 : 1);
            factoryCalls.Should().Be(1);
            watcher.IsDisposed.Should().BeTrue();
            session.CacheLearning!.GetSnapshot().IsDisposed.Should().BeTrue();
            session.CacheLearning.GetSnapshot().IsLearning.Should().BeFalse();
        }
        finally
        {
            cancellation.Cancel();
            await watching.WaitAsync(s_wait);
        }
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public async Task Watch_preserves_caller_owned_cache_session_after_shutdown(bool watcherError)
    {
        using var directory = new TestDirectory();
        Directory.CreateDirectory(Path.Combine(directory.Path, "tests"));
        File.WriteAllText(Path.Combine(directory.Path, "elm.json"), ElmJson);
        File.WriteAllText(Path.Combine(directory.Path, "tests", "Tests.elm"), "not a valid Elm module");
        await using var session = CreateSmallWatchCacheSession();
        using var cancellation = new CancellationTokenSource(s_wait);
        var output = new NotifyingWriter();
        var console = CreateConsole(output);
        var watcher = new FakeTestWatcher();

        var watching =
            Task.Run(
                () => TestCommand.ExecuteCore(
                    directory.Path,
                    watch: true,
                    console: console,
                    errorConsole: console,
                    offline: true,
                    cancellationToken: cancellation.Token,
                    cacheSession: session,
                    createWatcher: _ => watcher,
                    createWatchCacheSession: () => throw new InvalidOperationException("Caller supplied the session")));

        try
        {
            await output.NextContaining("Failed parsing Elm test module");

            if (watcherError)
                watcher.RaiseError(new IOException("Simulated watcher failure"));

            else
                cancellation.Cancel();

            (await watching.WaitAsync(s_wait)).Should().Be(watcherError ? 1 : 130);
            session.CacheLearning!.GetSnapshot().IsDisposed.Should().BeFalse();

            session.CacheLearning.Record(
                new(Expression.LitralInst(PineValue.EmptyList), PineValue.EmptyList)).Should().BeTrue();

            watcher.IsDisposed.Should().BeTrue();
        }
        finally
        {
            cancellation.Cancel();
            await watching.WaitAsync(s_wait);
        }
    }

    [Fact]
    public void Template_cache_factory_preserves_existing_explicit_null_cache_and_configuration_calls()
    {
        IntermediateVM.SetupVM.Create(null).Should().NotBeNull();
        IntermediateVM.SetupVM.Create(null, null).Should().NotBeNull();
        IntermediateVM.SetupVM.Create(evalCache: null, evaluationConfigDefault: null).Should().NotBeNull();
    }

    [Fact]
    public void Caller_supplied_instrumentation_keeps_custom_compilation_caches_isolated()
    {
        using var directory = new TestDirectory();
        Directory.CreateDirectory(Path.Combine(directory.Path, "tests"));
        File.WriteAllText(Path.Combine(directory.Path, "elm.json"), ElmJson);
        File.WriteAllText(Path.Combine(directory.Path, "tests", "Tests.elm"), FuzzTests);
        var output = new StringWriter();
        var console = CreateConsole(output);

        using var instrumentation =
            new ElmTestInstrumentation(
                new() { DisableReduction = true, DisableInlining = true },
                recordDiagnostics: false);

        TestCommand.Execute(
            directory.Path,
            console: console,
            errorConsole: console,
            offline: true,
            filter: "selected",
            seed: 42,
            fuzz: 3,
            instrumentation: instrumentation)
            .Should().Be(0);

        output.ToString().Should().Contain("TEST RUN PASSED");
    }

    [Theory]
    [InlineData("default")]
    [InlineData("reduction")]
    [InlineData("inlining")]
    [InlineData("tail")]
    [InlineData("fast-paths")]
    [InlineData("invocations")]
    [InlineData("leaves")]
    [InlineData("custom-leaves")]
    public void Caller_supplied_nondiagnostic_instrumentation_does_not_allocate_implicit_cache_session(string control)
    {
        using var directory = new TestDirectory();
        Directory.CreateDirectory(Path.Combine(directory.Path, "tests"));
        File.WriteAllText(Path.Combine(directory.Path, "elm.json"), ElmJson);
        File.WriteAllText(Path.Combine(directory.Path, "tests", "Tests.elm"), FuzzTests);
        var output = new StringWriter();
        var console = CreateConsole(output);

        using var instrumentation =
            new ElmTestInstrumentation(
                new()
                {
                    DisableReduction = control == "reduction",
                    DisableInlining = control == "inlining",
                    DisableTailRecursion = control == "tail",
                    DisableApplicationFastPaths = control == "fast-paths",
                    DisableInvocationCache = control == "invocations",
                    DisablePrecompiledLeaves = control == "leaves"
                },
                precompiledLeaves: control == "custom-leaves" ? new Dictionary<PineValue, PrecompiledLeaf>() : null,
                recordDiagnostics: false);

        var implicitSessionCreated = false;

        TestCommand.ExecuteCore(
            directory.Path,
            watch: false,
            console: console,
            errorConsole: console,
            offline: true,
            filter: "selected",
            seed: 42,
            fuzz: 3,
            workers: 1,
            instrumentation: instrumentation,
            onCacheSession: _ => implicitSessionCreated = true).Should().Be(0);

        output.ToString().Should().Contain("TEST RUN PASSED");
        implicitSessionCreated.Should().BeFalse();
        instrumentation.GetSummary().Counters.InstructionCount.Should().BeGreaterThan(0);
    }

    [Fact]
    public void Watch_help_describes_recursive_local_scope_and_cancellation()
    {
        var output = new StringWriter();

        TestCommand.Create().Parse(["--help"])
            .Invoke(new InvocationConfiguration { Output = output }).Should().Be(0);

        output.ToString().Should().Contain("--watch").And.Contain("recursively")
            .And.Contain("local project directory").And.Contain("Ctrl+C");
    }

    [Theory]
    [InlineData(true)]
    [InlineData(false)]
    public void Profile_rejects_watch_on_either_side_of_subcommand(bool before)
    {
        var errors = new StringWriter();

        TestCommand.Create().Parse(before ? ["--watch", "profile", "."] : ["profile", ".", "--watch"])
            .Invoke(new InvocationConfiguration { Error = errors }).Should().Be(1);

        errors.ToString().Should().Contain("--watch is not supported with elm test profile");
    }

    [Fact]
    public void Remote_watch_is_rejected_before_loading()
    {
        var output = new StringWriter();
        var console = CreateConsole(output);
        var loaded = false;

        TestCommand.Execute(
            "https://github.com/pine-vm/pine/tree/main",
            console: console,
            errorConsole: console,
            watch: true,
            remoteSourceLoader:
            _ =>
            {
                loaded = true;
                throw new InvalidOperationException("Must not load");
            }).Should().Be(1);

        loaded.Should().BeFalse();
        output.ToString().Should().Contain("--watch requires a local project directory");
    }

    [Fact]
    public void Dependency_report_watch_is_rejected_without_writing()
    {
        using var directory = new TestDirectory();
        var report = Path.Combine(directory.Path, "report.json");
        var output = new StringWriter();
        var console = CreateConsole(output);

        TestCommand.Execute(
            directory.Path,
            console: console,
            errorConsole: console,
            watch: true,
            dependencyReportPath: report).Should().Be(1);

        File.Exists(report).Should().BeFalse();
        output.ToString().Should().Contain("--watch cannot be combined with --dependency-report");
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void Invalid_watch_root_reports_error_without_running(bool file)
    {
        using var directory = new TestDirectory();
        var root = Path.Combine(directory.Path, "not-a-directory");

        if (file)
            File.WriteAllText(root, "file");

        var output = new StringWriter();
        var console = CreateConsole(output);
        TestCommand.Execute(root, console: console, errorConsole: console, watch: true).Should().Be(1);
        output.ToString().Should().Contain("Cannot watch project directory").And.NotContain("Running ");
    }

    [Fact]
    public async Task Watcher_is_active_before_initial_run_and_keeps_its_changes()
    {
        using var directory = new TestDirectory();
        using var cancellation = new CancellationTokenSource(s_wait);
        var runs = 0;

        var watching =
            Task.Run(
                () => FileWatchRunner.RunAsync(
                    directory.Path,
                    token =>
                    {
                        if (Interlocked.Increment(ref runs) == 1)
                            File.WriteAllText(Path.Combine(directory.Path, "arbitrary.txt"), "initial run edit");

                        else
                            cancellation.Cancel();

                        return 0;
                    },
                    () => { },
                    exception => throw new InvalidOperationException("Watcher failed", exception),
                    cancellation.Token));

        (await watching.WaitAsync(s_wait)).Should().Be(130);
        runs.Should().Be(2);
    }

    [Fact]
    public async Task Watcher_overflow_reports_error_and_disposes_instead_of_silently_staying_stale()
    {
        using var directory = new TestDirectory();
        using var cancellation = new CancellationTokenSource(s_wait);
        var watcher = new FakeTestWatcher();
        var overflow = new InternalBufferOverflowException("Simulated watcher buffer overflow");
        Exception? reported = null;
        var count = 0;

        (await FileWatchRunner.RunAsync(
            directory.Path,
            _ =>
            {
                watcher.IsStarted.Should().BeTrue();
                ++count;
                watcher.RaiseError(overflow);
                return 0;
            },
            () => { },
            exception => reported = exception,
            cancellation.Token,
            createWatcher: _ => watcher).WaitAsync(s_wait)).Should().Be(1);

        reported.Should().BeSameAs(overflow);
        watcher.IsDisposed.Should().BeTrue();
        watcher.IsStarted.Should().BeFalse();
        watcher.HasSubscriptions.Should().BeFalse();
        count.Should().Be(1);
    }

    [Fact]
    public async Task Watcher_subscriptions_and_start_precede_ready_and_initial_run()
    {
        using var directory = new TestDirectory();
        using var cancellation = new CancellationTokenSource(s_wait);
        var watcher = new FakeTestWatcher();
        var ready = false;
        var count = 0;

        (await FileWatchRunner.RunAsync(
            directory.Path,
            _ =>
            {
                ready.Should().BeTrue();
                watcher.IsStarted.Should().BeTrue();

                if (++count == 1)
                {
                    for (var index = 0; index < 20; ++index)
                        watcher.RaiseChange();
                }
                else
                {
                    cancellation.Cancel();
                }

                return 0;
            },
            () =>
            {
                watcher.IsStarted.Should().BeTrue();
                watcher.HasSubscriptions.Should().BeTrue();
                ready = true;
            },
            exception => throw new InvalidOperationException("Watcher failed", exception),
            cancellation.Token,
            createWatcher:
            path =>
            {
                path.Should().Be(Path.GetFullPath(directory.Path));
                return watcher;
            }).WaitAsync(s_wait)).Should().Be(130);

        count.Should().Be(2);
        watcher.IsDisposed.Should().BeTrue();
    }

    [Fact]
    public async Task Watcher_start_failure_reports_error_without_running_and_disposes()
    {
        using var directory = new TestDirectory();
        var failure = new IOException("Cannot start watcher");
        var watcher = new FakeTestWatcher(onStart: () => throw failure);
        Exception? reported = null;

        (await FileWatchRunner.RunAsync(
            directory.Path,
            _ => throw new InvalidOperationException("Must not run"),
            () => throw new InvalidOperationException("Must not announce readiness"),
            exception => reported = exception,
            CancellationToken.None,
            createWatcher: _ => watcher)).Should().Be(1);

        reported.Should().BeSameAs(failure);
        watcher.IsDisposed.Should().BeTrue();
    }

    [Theory]
    [InlineData("create")]
    [InlineData("change")]
    [InlineData("delete")]
    [InlineData("rename")]
    [InlineData("manifest")]
    [InlineData("directory-rename")]
    [InlineData("directory-delete")]
    public async Task Whole_project_arbitrary_nested_files_and_manifests_trigger_runs(string operation)
    {
        using var directory = new TestDirectory();
        var nested = Path.Combine(directory.Path, "not-a-source-directory", "deeper");
        Directory.CreateDirectory(nested);
        var file = Path.Combine(nested, "file.custom-extension");
        File.WriteAllText(file, "original");
        File.WriteAllText(Path.Combine(directory.Path, "elm.json"), "{}");

        using var cancellation = new CancellationTokenSource(s_wait);
        var runs = Channel.CreateUnbounded<int>();
        var count = 0;

        var watching =
            Start(
                directory.Path,
                cancellation.Token,
                _ =>
                {
                    runs.Writer.TryWrite(Interlocked.Increment(ref count));
                    return 0;
                });

        try
        {
            (await runs.Reader.ReadAsync().AsTask().WaitAsync(s_wait)).Should().Be(1);

            switch (operation)
            {
                case "create":
                    File.WriteAllText(Path.Combine(nested, "extensionless"), "created");
                    break;

                case "change":
                    File.WriteAllText(file, "changed");
                    break;

                case "delete":
                    File.Delete(file);
                    break;

                case "rename":
                    File.Move(file, file + ".renamed");
                    break;

                case "manifest":
                    File.WriteAllText(Path.Combine(directory.Path, "elm.json"), "{\"changed\":true}");
                    break;

                case "directory-rename":
                    Directory.Move(nested, nested + "-renamed");
                    break;

                case "directory-delete":
                    Directory.Delete(nested, recursive: true);
                    break;

                default:
                    throw new NotImplementedException(operation);
            }

            (await runs.Reader.ReadAsync().AsTask().WaitAsync(s_wait)).Should().Be(2);
        }
        finally
        {
            cancellation.Cancel();
            (await watching.WaitAsync(s_wait)).Should().Be(130);
        }
    }

    [Fact]
    public async Task Newly_created_subdirectories_are_watched_and_bursts_are_coalesced()
    {
        using var directory = new TestDirectory();
        using var cancellation = new CancellationTokenSource(s_wait);
        var runs = Channel.CreateUnbounded<int>();
        var count = 0;

        var watching =
            Start(
                directory.Path,
                cancellation.Token,
                _ =>
                {
                    runs.Writer.TryWrite(Interlocked.Increment(ref count));
                    return 0;
                });

        try
        {
            await runs.Reader.ReadAsync().AsTask().WaitAsync(s_wait);
            var nested = Path.Combine(directory.Path, "new", "deep");
            Directory.CreateDirectory(nested);
            (await runs.Reader.ReadAsync().AsTask().WaitAsync(s_wait)).Should().Be(2);

            for (var index = 0; index < 20; ++index)
                File.WriteAllText(Path.Combine(nested, "anything." + index), index.ToString());

            (await runs.Reader.ReadAsync().AsTask().WaitAsync(s_wait)).Should().Be(3);
            await Task.Delay(500);
            count.Should().Be(3);
        }
        finally
        {
            cancellation.Cancel();
            await watching.WaitAsync(s_wait);
        }
    }

    [Fact]
    public async Task Changes_during_each_run_queue_a_followup_without_parallel_execution()
    {
        using var directory = new TestDirectory();
        var watcher = new FakeTestWatcher();
        using var cancellation = new CancellationTokenSource(s_wait);
        using var firstRelease = new ManualResetEventSlim();
        using var secondRelease = new ManualResetEventSlim();
        var entered = Channel.CreateUnbounded<int>();
        var count = 0;
        var active = 0;
        var maximumActive = 0;

        var watching =
            Start(
                directory.Path,
                cancellation.Token,
                token =>
                {
                    maximumActive = Math.Max(maximumActive, Interlocked.Increment(ref active));
                    var current = Interlocked.Increment(ref count);
                    entered.Writer.TryWrite(current);

                    try
                    {
                        if (current == 1)
                            firstRelease.Wait(token);

                        else if (current == 2)
                            secondRelease.Wait(token);

                        else
                            cancellation.Cancel();

                        return current == 1 ? 1 : 0;
                    }
                    finally
                    {
                        Interlocked.Decrement(ref active);
                    }
                },
                watcher);

        try
        {
            (await entered.Reader.ReadAsync().AsTask().WaitAsync(s_wait)).Should().Be(1);
            File.WriteAllText(Path.Combine(directory.Path, "edit.txt"), "first");
            watcher.RaiseChange();
            await Task.Delay(300);
            count.Should().Be(1);
            firstRelease.Set();

            (await entered.Reader.ReadAsync().AsTask().WaitAsync(s_wait)).Should().Be(2);
            File.WriteAllText(Path.Combine(directory.Path, "edit.txt"), "second");
            watcher.RaiseChange();
            await Task.Delay(300);
            count.Should().Be(2);
            secondRelease.Set();

            (await watching.WaitAsync(s_wait)).Should().Be(130);
            count.Should().Be(3);
            maximumActive.Should().Be(1);
        }
        finally
        {
            cancellation.Cancel();
            await watching.WaitAsync(s_wait);
        }
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public async Task Cancellation_stops_idle_or_active_runner_and_disposes_watcher(bool activeRun)
    {
        using var directory = new TestDirectory();
        var watcher = new FakeTestWatcher();
        using var cancellation = new CancellationTokenSource(s_wait);
        var entered = new TaskCompletionSource(TaskCreationOptions.RunContinuationsAsynchronously);
        var count = 0;

        var watching =
            Start(
                directory.Path,
                cancellation.Token,
                token =>
                {
                    Interlocked.Increment(ref count);
                    entered.TrySetResult();

                    if (activeRun)
                    {
                        token.WaitHandle.WaitOne();
                        token.ThrowIfCancellationRequested();
                    }

                    return 0;
                },
                watcher);

        try
        {
            await entered.Task.WaitAsync(s_wait);
            cancellation.Cancel();
            (await watching.WaitAsync(s_wait)).Should().Be(130);
            watcher.IsDisposed.Should().BeTrue();
            watcher.IsStarted.Should().BeFalse();
            watcher.HasSubscriptions.Should().BeFalse();
            File.WriteAllText(Path.Combine(directory.Path, "after-cancellation.txt"), "no run");
            watcher.RaiseChange();
            watcher.RaiseError(new IOException("Ignored after disposal"));
            count.Should().Be(1);
        }
        finally
        {
            cancellation.Cancel();
            await watching.WaitAsync(s_wait);
        }
    }

    [Fact]
    public async Task Failed_runs_do_not_stop_watching()
    {
        using var directory = new TestDirectory();
        var watcher = new FakeTestWatcher();
        using var cancellation = new CancellationTokenSource(s_wait);
        var runs = Channel.CreateUnbounded<int>();
        var count = 0;

        var watching =
            Start(
                directory.Path,
                cancellation.Token,
                _ =>
                {
                    var current = Interlocked.Increment(ref count);
                    runs.Writer.TryWrite(current);
                    return current == 1 ? 1 : 2;
                },
                watcher);

        try
        {
            await runs.Reader.ReadAsync().AsTask().WaitAsync(s_wait);

            for (var expected = 2; expected <= 3; ++expected)
            {
                File.WriteAllText(Path.Combine(directory.Path, "fix.txt"), expected.ToString());
                watcher.RaiseChange();
                (await runs.Reader.ReadAsync().AsTask().WaitAsync(s_wait)).Should().Be(expected);
                watching.IsCompleted.Should().BeFalse();
            }
        }
        finally
        {
            cancellation.Cancel();
            await watching.WaitAsync(s_wait);
        }
    }

    [Fact]
    public async Task Watch_announces_initial_run_and_rerun_before_source_processing()
    {
        using var directory = new TestDirectory();
        Directory.CreateDirectory(Path.Combine(directory.Path, "tests"));
        File.WriteAllText(Path.Combine(directory.Path, "elm.json"), ElmJson);
        File.WriteAllText(Path.Combine(directory.Path, "tests", "Tests.elm"), "not a valid Elm module");
        using var cancellation = new CancellationTokenSource(s_wait);
        var output = new NotifyingWriter();
        var console = CreateConsole(output);
        var watcher = new FakeTestWatcher();

        var watching =
            Task.Run(
                () => TestCommand.ExecuteCore(
                    directory.Path,
                    colorMode: CommandLineColorMode.Never,
                    console: console,
                    errorConsole: console,
                    watch: true,
                    offline: true,
                    cancellationToken: cancellation.Token,
                    createWatcher: _ => watcher));

        try
        {
            await output.NextContaining("Starting test run...");
            (await output.NextContaining("Error:")).Should().Contain("Failed parsing Elm test module");

            File.WriteAllText(Path.Combine(directory.Path, "anything.txt"), "trigger another run");
            watcher.RaiseChange();
            await output.NextContaining("Starting test run...");
            (await output.NextContaining("Error:")).Should().Contain("Failed parsing Elm test module");
            watching.IsCompleted.Should().BeFalse();
        }
        finally
        {
            cancellation.Cancel();
            (await watching.WaitAsync(s_wait)).Should().Be(130);
        }
    }

    [Fact]
    public async Task Command_reports_watcher_error_and_disposes_while_waiting_for_changes()
    {
        using var directory = new TestDirectory();
        Directory.CreateDirectory(Path.Combine(directory.Path, "tests"));
        File.WriteAllText(Path.Combine(directory.Path, "elm.json"), ElmJson);
        File.WriteAllText(Path.Combine(directory.Path, "tests", "Tests.elm"), "not a valid Elm module");
        using var cancellation = new CancellationTokenSource(s_wait);
        var output = new NotifyingWriter();
        var console = CreateConsole(output);
        var watcher = new FakeTestWatcher();

        var watching =
            Task.Run(
                () => TestCommand.ExecuteCore(
                    directory.Path,
                    watch: true,
                    console: console,
                    errorConsole: console,
                    offline: true,
                    cancellationToken: cancellation.Token,
                    createWatcher: _ => watcher));

        try
        {
            await output.NextContaining("Failed parsing Elm test module");
            watcher.RaiseError(new InternalBufferOverflowException("Simulated watcher buffer overflow"));
            (await watching.WaitAsync(s_wait)).Should().Be(1);

            (await output.NextContaining("Cannot watch project directory")).Should()
                .Contain("Simulated watcher buffer overflow");

            watcher.IsDisposed.Should().BeTrue();
        }
        finally
        {
            cancellation.Cancel();
            await watching.WaitAsync(s_wait);
        }
    }

    [Fact]
    public async Task Command_recovers_from_source_errors_and_failures_with_latest_run_durations_and_settings()
    {
        using var directory = new TestDirectory();
        Directory.CreateDirectory(Path.Combine(directory.Path, "tests"));
        File.WriteAllText(Path.Combine(directory.Path, "elm.json"), ElmJson);
        var module = Path.Combine(directory.Path, "tests", "Tests.elm");
        File.WriteAllText(module, "not a valid Elm module");
        using var cancellation = new CancellationTokenSource(s_wait);
        var output = new NotifyingWriter();
        var console = CreateConsole(output);
        var watcher = new FakeTestWatcher();

        var watching =
            Task.Run(
                () => TestCommand.ExecuteCore(
                    directory.Path,
                    colorMode: CommandLineColorMode.Never,
                    console: console,
                    errorConsole: console,
                    watch: true,
                    offline: true,
                    reportDurations: true,
                    filter: "selected",
                    seed: 42,
                    fuzz: 3,
                    workers: 2,
                    cancellationToken: cancellation.Token,
                    createWatcher: _ => watcher));

        try
        {
            (await output.NextContaining("Error:")).Should().Contain("Failed parsing Elm test module");
            File.WriteAllText(module, FuzzTests.Replace("Expect.equal value value", "unknownDeclaration"));
            watcher.RaiseChange();
            await output.NextContaining("Failed compiling Elm tests");
            File.WriteAllText(module, FuzzTests.Replace("Expect.equal value value", "Expect.equal value (value + 1)"));
            watcher.RaiseChange();
            var failed = await output.NextRun();
            AssertSingleDuration(failed);

            failed.Should().Contain("TEST RUN FAILED").And.Contain("Passed:   0").And.Contain("Failed:   1")
                .And.Contain("--seed 42 --fuzz 3");

            File.WriteAllText(module, FuzzTests);
            watcher.RaiseChange();
            var passed = await output.NextRun();
            AssertSingleDuration(passed);

            passed.Should().Contain("TEST RUN PASSED").And.Contain("Passed:   1").And.Contain("Failed:   0")
                .And.NotContain("TEST RUN FAILED").And.Contain("--seed 42 --fuzz 3");

            File.WriteAllText(Path.Combine(directory.Path, "elm.json"), "{");
            watcher.RaiseChange();
            await output.NextContaining("Error:");
            File.WriteAllText(Path.Combine(directory.Path, "elm.json"), ElmJson);
            watcher.RaiseChange();
            (await output.NextRun()).Should().Contain("TEST RUN PASSED");
            watching.IsCompleted.Should().BeFalse();
        }
        finally
        {
            cancellation.Cancel();
            (await watching.WaitAsync(s_wait)).Should().Be(130);
        }
    }

    [Fact]
    public async Task Command_recovers_from_parse_errors_in_ordinary_source_directories()
    {
        using var directory = new TestDirectory();
        Directory.CreateDirectory(Path.Combine(directory.Path, "tests"));
        Directory.CreateDirectory(Path.Combine(directory.Path, "src"));
        File.WriteAllText(Path.Combine(directory.Path, "elm.json"), ElmJson);

        File.WriteAllText(
            Path.Combine(directory.Path, "tests", "Tests.elm"),
            FuzzTests.Replace("import Fuzz", "import Fuzz\nimport Source")
            .Replace("Expect.equal value value", "Expect.equal value (value + Source.zero)"));

        var source = Path.Combine(directory.Path, "src", "Source.elm");
        File.WriteAllText(source, "not a valid source module");
        using var cancellation = new CancellationTokenSource(s_wait);
        var output = new NotifyingWriter();
        var console = CreateConsole(output);
        var watcher = new FakeTestWatcher();

        var watching =
            Task.Run(
                () => TestCommand.ExecuteCore(
                    directory.Path,
                    colorMode: CommandLineColorMode.Never,
                    console: console,
                    errorConsole: console,
                    watch: true,
                    offline: true,
                    filter: "selected",
                    cancellationToken: cancellation.Token,
                    createWatcher: _ => watcher));

        try
        {
            (await output.NextContaining("Error:")).Should().Contain("Cannot parse Elm source");
            File.WriteAllText(source, "module Source exposing (zero)\nzero = 0");
            watcher.RaiseChange();
            (await output.NextRun()).Should().Contain("TEST RUN PASSED");
        }
        finally
        {
            cancellation.Cancel();
            (await watching.WaitAsync(s_wait)).Should().Be(130);
        }
    }

    [Fact]
    public async Task Command_watch_retains_the_same_warm_cache_session_across_file_triggered_runs()
    {
        using var directory = new TestDirectory();
        Directory.CreateDirectory(Path.Combine(directory.Path, "tests"));
        File.WriteAllText(Path.Combine(directory.Path, "elm.json"), ElmJson);
        File.WriteAllText(Path.Combine(directory.Path, "tests", "Tests.elm"), FuzzTests);
        using var cancellation = new CancellationTokenSource(s_wait);
        var output = new NotifyingWriter();
        var console = CreateConsole(output);
        var watcher = new FakeTestWatcher();

        var sessions =
            Channel.CreateUnbounded<(ElmTestRunCacheSession Session, int Encoded, int Reduced, int Parsed, int Compiled, int Templates, int Sources, int Canonical, ContinuousInvocationCacheLearningSnapshot Learning)>();

        var factoryCalls = 0;

        var watching =
            Task.Run(
                () => TestCommand.ExecuteCore(
                    directory.Path,
                    watch: true,
                    colorMode: CommandLineColorMode.Never,
                    console: console,
                    errorConsole: console,
                    offline: true,
                    filter: "selected",
                    seed: 42,
                    fuzz: 3,
                    workers: 2,
                    cancellationToken: cancellation.Token,
                    createWatcher: _ => watcher,
                    onCacheSession:
                    session => sessions.Writer.TryWrite(
                        (session,
                        session.VmCaches.EncodedExpressions.Count,
                        session.VmCaches.ReducedExpressions.Count,
                        session.VmCaches.ParsedExpressions.Count,
                        session.VmCaches.ExpressionCompilations.Count,
                        session.VmCaches.ExpressionTemplatePlans.Count,
                        session.SourceParsing.Count,
                        session.DeclarationCanonicalization.Count,
                        session.CacheLearning!.GetSnapshot())),
                    createWatchCacheSession:
                    () =>
                    {
                        Interlocked.Increment(ref factoryCalls);
                        return TestCommand.CreateWatchCacheSession();
                    }));

        try
        {
            var first = await sessions.Reader.ReadAsync().AsTask().WaitAsync(s_wait);
            (await output.NextRun()).Should().Contain("TEST RUN PASSED");
            first.Encoded.Should().BeGreaterThan(0);
            first.Reduced.Should().BeGreaterThan(0);
            first.Parsed.Should().BeGreaterThan(0);
            first.Compiled.Should().BeGreaterThan(0);
            first.Templates.Should().BeGreaterThan(0);
            first.Sources.Should().BeGreaterThan(0);
            first.Canonical.Should().BeGreaterThan(0);
            first.Session.GenerationalInvocations.Should().NotBeNull();
            first.Session.InvocationCache.Should().BeSameAs(first.Session.GenerationalInvocations);
            first.Session.InvocationResults.Count.Should().Be(0);
            first.Learning.AcceptedCount.Should().BeGreaterThan(0);
            first.Learning.PendingCount.Should().BeLessThanOrEqualTo(16);
            first.Learning.SampleCount.Should().BeLessThanOrEqualTo(8);
            first.Learning.IsDisposed.Should().BeFalse();
            File.WriteAllText(Path.Combine(directory.Path, "unrelated.txt"), "trigger without changing tests");
            watcher.RaiseChange();
            var second = await sessions.Reader.ReadAsync().AsTask().WaitAsync(s_wait);
            (await output.NextRun()).Should().Contain("TEST RUN PASSED");
            second.Session.Should().BeSameAs(first.Session);
            second.Encoded.Should().Be(first.Encoded);
            second.Reduced.Should().Be(first.Reduced);
            second.Parsed.Should().Be(first.Parsed);
            second.Compiled.Should().Be(first.Compiled);
            second.Templates.Should().Be(first.Templates);
            second.Sources.Should().Be(first.Sources);
            second.Canonical.Should().Be(first.Canonical);
            second.Session.InvocationResults.Count.Should().Be(0);
            second.Learning.AcceptedCount.Should().BeGreaterThanOrEqualTo(first.Learning.AcceptedCount);

            (second.Learning.AcceptedCount + second.Learning.DroppedCount).Should()
                .BeGreaterThan(first.Learning.AcceptedCount + first.Learning.DroppedCount);

            second.Learning.RunCount.Should().BeGreaterThanOrEqualTo(first.Learning.RunCount);
            second.Learning.IsDisposed.Should().BeFalse();
            factoryCalls.Should().Be(1);
        }
        finally
        {
            cancellation.Cancel();
            (await watching.WaitAsync(s_wait)).Should().Be(130);
        }
    }

    [Fact]
    public async Task Command_watch_learns_from_real_Elm_evaluations_and_reuses_learned_invocations()
    {
        using var directory = new TestDirectory();
        Directory.CreateDirectory(Path.Combine(directory.Path, "tests"));
        File.WriteAllText(Path.Combine(directory.Path, "elm.json"), ElmJson);
        File.WriteAllText(Path.Combine(directory.Path, "tests", "Tests.elm"), LearningTests);
        using var cancellation = new CancellationTokenSource(TimeSpan.FromMinutes(2));
        var output = new NotifyingWriter();
        var console = CreateConsole(output);
        var watcher = new FakeTestWatcher();
        var switched = new TaskCompletionSource<string>(TaskCreationOptions.RunContinuationsAsynchronously);

        await using var session =
            TestCommand.CreateWatchCacheSession(
                message =>
                {
                    if (message.StartsWith("Invocation-cache policy switch:", StringComparison.Ordinal))
                        switched.TrySetResult(message);
                },
                generationOptions: new(maximumGenerationCount: 2, rotationEntryCount: 64),
                learningOptions:
                new(
                    MinimumInterval: TimeSpan.FromMilliseconds(100),
                    SampleCapacity: 2,
                    PendingCapacity: 16),
                learningTimeout: TimeSpan.FromSeconds(30));

        var completed = Channel.CreateUnbounded<ElmTestRunCacheSession>();

        var watching =
            Task.Run(
                () => TestCommand.ExecuteCore(
                    directory.Path,
                    watch: true,
                    colorMode: CommandLineColorMode.Never,
                    console: console,
                    errorConsole: console,
                    offline: true,
                    seed: 42,
                    fuzz: 3,
                    workers: 1,
                    cancellationToken: cancellation.Token,
                    onCacheSession: current => completed.Writer.TryWrite(current),
                    createWatcher: _ => watcher,
                    createWatchCacheSession: () => session));

        try
        {
            (await completed.Reader.ReadAsync().AsTask().WaitAsync(s_wait)).Should().BeSameAs(session);
            (await output.NextRun()).Should().Contain("TEST RUN PASSED").And.Contain("Passed:   1");
            (await switched.Task.WaitAsync(s_wait)).Should().Contain("baseline-cost=").And.Contain("selected-cost=");
            var state = session.CacheLearning!.State;
            state.Policy.Expressions.Should().NotBeEmpty();
            state.LastRun.Should().NotBeNull();
            state.LastRun!.SelectedCost.Should().BeLessThanOrEqualTo(state.LastRun.BaselineCost);
            session.CacheLearning.GetSnapshot().SwitchCount.Should().BeGreaterThan(0);
            var hitsBeforeReruns = session.GetCacheStatisticsSnapshot().Totals.HitCount;

            for (var run = 0; run < 2; ++run)
            {
                watcher.RaiseChange();
                (await completed.Reader.ReadAsync().AsTask().WaitAsync(s_wait)).Should().BeSameAs(session);
                (await output.NextRun()).Should().Contain("TEST RUN PASSED").And.Contain("Passed:   1");
            }

            session.GetCacheStatisticsSnapshot().Totals.HitCount.Should().BeGreaterThan(hitsBeforeReruns);
            session.GenerationalInvocations!.GetStatistics().PhysicalEntryCount.Should().BeGreaterThan(0);
            session.InvocationResults.Count.Should().Be(0);
        }
        finally
        {
            cancellation.Cancel();
            (await watching.WaitAsync(s_wait)).Should().Be(130);
        }
    }

    [Fact]
    public async Task Command_creates_fresh_budget_tracker_for_each_run()
    {
        using var directory = new TestDirectory();
        Directory.CreateDirectory(Path.Combine(directory.Path, "tests"));
        File.WriteAllText(Path.Combine(directory.Path, "elm.json"), ElmJson);
        File.WriteAllText(Path.Combine(directory.Path, "tests", "Tests.elm"), LoopTests);
        using var cancellation = new CancellationTokenSource(s_wait);
        var output = new NotifyingWriter();
        var console = CreateConsole(output);
        var watcher = new FakeTestWatcher();

        var watching =
            Task.Run(
                () => TestCommand.ExecuteCore(
                    directory.Path,
                    colorMode: CommandLineColorMode.Never,
                    console: console,
                    errorConsole: console,
                    watch: true,
                    offline: true,
                    evaluationOptions: new() { LoopBudget = 100 },
                    cancellationToken: cancellation.Token,
                    createWatcher: _ => watcher));

        try
        {
            var first = await output.NextContaining("Invocations:");
            File.WriteAllText(Path.Combine(directory.Path, "anything.txt"), "rerun after exhausted budget");
            watcher.RaiseChange();
            var second = await output.NextContaining("Invocations:");

            Regex.Match(second, @"loops: [\d_]+").Value
                .Should().Be(Regex.Match(first, @"loops: [\d_]+").Value).And.NotBeEmpty();

            watching.IsCompleted.Should().BeFalse();
        }
        finally
        {
            cancellation.Cancel();
            (await watching.WaitAsync(s_wait)).Should().Be(130);
        }
    }

    [Fact]
    public async Task Command_cancels_active_VM_execution_without_explicit_budgets()
    {
        using var directory = new TestDirectory();
        Directory.CreateDirectory(Path.Combine(directory.Path, "tests"));
        File.WriteAllText(Path.Combine(directory.Path, "elm.json"), ElmJson);
        File.WriteAllText(Path.Combine(directory.Path, "tests", "Tests.elm"), LoopTests);
        using var cancellation = new CancellationTokenSource(s_wait);
        var output = new NotifyingWriter();
        var console = CreateConsole(output);
        var watcher = new FakeTestWatcher();

        var watching =
            Task.Run(
                () => TestCommand.ExecuteCore(
                    directory.Path,
                    colorMode: CommandLineColorMode.Never,
                    console: console,
                    errorConsole: console,
                    watch: true,
                    offline: true,
                    cancellationToken: cancellation.Token,
                    createWatcher: _ => watcher));

        try
        {
            await output.NextContaining("Running 1 test");
            cancellation.Cancel();
            (await watching.WaitAsync(s_wait)).Should().Be(130);
        }
        finally
        {
            cancellation.Cancel();
            await watching.WaitAsync(s_wait);
        }
    }

    private static void AssertSingleDuration(string rendered)
    {
        rendered.Split("Duration:").Should().HaveCount(2);
        rendered.Should().Contain("  compilation:").And.Contain("  preparation:").And.Contain("  execution:");
        rendered.Split("VM calls (sum):").Should().HaveCount(3);
    }

    private static ElmTestRunCacheSession CreateSmallWatchCacheSession() =>
        TestCommand.CreateWatchCacheSession(
            generationOptions: new(maximumGenerationCount: 2, rotationEntryCount: 1),
            learningOptions: new(MinimumInterval: TimeSpan.FromMilliseconds(10), SampleCapacity: 2, PendingCapacity: 2),
            learningTimeout: TimeSpan.FromSeconds(5));

    private static Task<int> Start(
        string directory, CancellationToken token, Func<CancellationToken, int> run, IFileWatcher? watcher = null) =>
        Task.Run(
            () => FileWatchRunner.RunAsync(
                directory,
                run,
                () => { },
                exception => throw new InvalidOperationException("Watcher failed", exception),
                token,
                createWatcher: watcher is null ? null : _ => watcher));

    private static IAnsiConsole CreateConsole(TextWriter writer) =>
        AnsiConsole.Create(
            new AnsiConsoleSettings
            {
                Ansi = AnsiSupport.No,
                Interactive = InteractionSupport.No,
                Out = new AnsiConsoleOutput(writer),
            });

    private sealed class TestDirectory : IDisposable
    {
        public string Path { get; } =
            System.IO.Path.GetFullPath(
                System.IO.Path.Combine(
                    Environment.GetFolderPath(Environment.SpecialFolder.LocalApplicationData),
                    "watch-command-tests",
                    Guid.NewGuid().ToString("N")));

        public TestDirectory() => Directory.CreateDirectory(Path);

        public void Dispose() => Directory.Delete(Path, recursive: true);
    }

    private sealed class FakeTestWatcher(Action? onStart = null) : IFileWatcher
    {
        public event Action? Changed;

        public event Action<Exception>? Error;

        public bool IsStarted { get; private set; }

        public bool IsDisposed { get; private set; }

        public bool HasSubscriptions => Changed is not null || Error is not null;

        public void RaiseChange()
        {
            if (!IsDisposed)
                IsStarted.Should().BeTrue();

            Changed?.Invoke();
        }

        public void RaiseError(Exception exception)
        {
            if (!IsDisposed)
                IsStarted.Should().BeTrue();

            Error?.Invoke(exception);
        }

        public void Start()
        {
            HasSubscriptions.Should().BeTrue();
            IsStarted = true;
            onStart?.Invoke();
        }

        public void Stop() => IsStarted = false;

        public void Dispose()
        {
            IsStarted.Should().BeFalse();
            HasSubscriptions.Should().BeFalse();
            IsDisposed = true;
        }
    }

    private sealed class NotifyingWriter : StringWriter
    {
        private readonly Channel<string> _writes = Channel.CreateUnbounded<string>();

        public override void Write(string? value)
        {
            if (value is not null)
                _writes.Writer.TryWrite(value);
        }

        public override void Write(char value) => Write(value.ToString());

        public override void WriteLine() => Write(Environment.NewLine);

        public override void WriteLine(string? value)
        {
            Write(value);
            WriteLine();
        }

        public async Task<string> NextRun()
        {
            using var timeout = new CancellationTokenSource(s_wait);
            var builder = new StringBuilder();

            while (true)
            {
                var value = await _writes.Reader.ReadAsync(timeout.Token);
                builder.Append(value);

                if (value.Contains("To reproduce", StringComparison.Ordinal))
                    return builder.ToString();
            }
        }

        public async Task<string> NextContaining(string fragment)
        {
            using var timeout = new CancellationTokenSource(s_wait);

            while (true)
            {
                var value = await _writes.Reader.ReadAsync(timeout.Token);

                if (value.Contains(fragment, StringComparison.Ordinal))
                    return value;
            }
        }
    }

    private const string ElmJson =
        """
        {
            "type": "application",
            "source-directories": ["src"],
            "elm-version": "0.19.1",
            "dependencies": {
                "direct": { "elm/core": "1.0.5" },
                "indirect": {}
            },
            "test-dependencies": {
                "direct": { "elm-explorations/test": "2.2.0" },
                "indirect": {
                    "elm/bytes": "1.0.8",
                    "elm/json": "1.1.4",
                    "elm/random": "1.0.0",
                    "elm/time": "1.0.0"
                }
            }
        }
        """;

    private const string FuzzTests =
        """
        module Tests exposing (suite)
        import Expect
        import Fuzz
        import Test
        suite =
            Test.describe "watch"
                [ Test.fuzz Fuzz.int "selected" (\value -> Expect.equal value value)
                , Test.test "excluded" (\_ -> Expect.fail "must be filtered out")
                ]
        """;

    private const string LoopTests =
        """
        module Tests exposing (suite)
        import Test
        spin n = spin (n + 1)
        suite = Test.test "looping test" (\_ -> spin 0)
        """;

    private const string LearningTests =
        """
        module Tests exposing (suite)
        import Expect
        import Fuzz
        import Test
        total count accumulated =
            if count <= 0 then
                accumulated
            else
                total (count - 1) (accumulated + count)
        suite =
            Test.fuzz (Fuzz.constant 100) "repeated work"
                (\value -> Expect.equal 5050 (total value 0))
        """;
}
