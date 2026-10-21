using AwesomeAssertions;
using Pine.Core.Addressing;
using Pine.Core.Elm.LanguageServer;
using Pine.Core.Interpreter.IntermediateVM;
using Pine.Core.LanguageServerProtocol;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading;
using System.Threading.Tasks;
using Xunit;

namespace Pine.Core.Tests.Elm.LanguageServer;

public class InvocationCacheStatisticsLogTests
{
    [Fact]
    public async Task Invocation_cache_statistics_log_runs_every_five_minutes_with_totals_before_expressions()
    {
        var statistics = new InvocationCacheStatistics();
        var cache = statistics.Observe(new ConcurrentInvocationCache());
        var expression = PineValue.Blob([1, 2, 3]);

        var key =
            new EvalCacheEntryKey(
                expression,
                StackFrameInput.GenericFromEnvironmentValue(PineValue.Blob([99])));

        cache.TryAdd(key, PineValue.EmptyList);
        cache.TryGet(key, out _);
        var logs = new List<string>();
        var clock = new RecordingTimeProvider();
        var session = new StatisticsSession(statistics);
        using var server = CreateServer(StubLanguageServiceSessionFactory.WithSession(session), logs.Add, clock);

        server.GetInvocationCacheStatistics().Should().BeNull();
        Initialize(server);
        await server.WorkspaceInitializationTask!;
        server.GetInvocationCacheStatistics()!.Totals.AdmissionCount.Should().Be(1);
        clock.DueTime.Should().Be(TimeSpan.FromMinutes(5));
        clock.Period.Should().Be(TimeSpan.FromMinutes(5));
        Reports(logs).Should().BeEmpty();

        clock.Fire();
        var report = Reports(logs).Should().ContainSingle().Which;
        var hash = Convert.ToHexStringLower(PineValueHashFlat.ComputeHashForValue(expression).hashBytes.Span);
        report.Should().Contain("Totals: admissions=1");
        report.Should().Contain("lookups=1, hits=1, misses=0, hit rate=100.00%");
        report.Should().Contain("Expression " + hash + ": admissions=1");

        report.IndexOf("Totals:", StringComparison.Ordinal)
            .Should().BeLessThan(report.IndexOf("Expression ", StringComparison.Ordinal));

        cache.TryGet(key, out _);
        clock.Fire();
        Reports(logs).Should().HaveCount(2);
        Reports(logs).Last().Should().Contain("lookups=2, hits=2");
        server.Dispose();
        server.Dispose();
        clock.Timer.Disposed.Should().BeTrue();
        clock.Fire();
        Reports(logs).Should().HaveCount(2);
    }

    [Fact]
    public async Task Invocation_cache_statistics_log_does_not_start_after_disposal_during_initialization()
    {
        var clock = new RecordingTimeProvider();
        var factory = new BlockingSessionFactory();
        using var server = CreateServer(factory, _ => { }, clock);
        Initialize(server);
        await factory.Started.Task;
        server.Dispose();

        factory.Completion.SetResult(
            Result<string, ILanguageServiceSession>.ok(new StatisticsSession(new())));

        await server.WorkspaceInitializationTask!;
        clock.Created.Should().BeFalse();
    }

    [Fact]
    public async Task Language_server_disposes_session_learning_on_shutdown_and_late_initialization()
    {
        var clock = new RecordingTimeProvider();
        var session = new DisposableSession();

        using (var server = CreateServer(StubLanguageServiceSessionFactory.WithSession(session), _ => { }, clock))
        {
            Initialize(server);
            await server.WorkspaceInitializationTask!;
            session.DisposeCount.Should().Be(0);
            server.Dispose();
            server.Dispose();
            session.DisposeCount.Should().Be(1);
        }

        var lateSession = new DisposableSession();
        var factory = new BlockingSessionFactory();
        using var lateServer = CreateServer(factory, _ => { }, new RecordingTimeProvider());
        Initialize(lateServer);
        await factory.Started.Task;
        lateServer.Dispose();
        factory.Completion.SetResult(Result<string, ILanguageServiceSession>.ok(lateSession));
        await lateServer.WorkspaceInitializationTask!;
        lateSession.DisposeCount.Should().Be(1);
    }

    [Fact]
    public void Invocation_cache_statistics_log_handles_unavailable_statistics_and_diagnostic_failures()
    {
        var clock = new RecordingTimeProvider();
        InvocationCacheStatisticsSnapshot? snapshot = null;
        var logs = new List<string>();
        var fail = false;
        var snapshotFailure = false;

        using var reporter =
            new InvocationCacheStatisticsLog(
                () => snapshotFailure ? throw new InvalidOperationException("snapshot unavailable") : snapshot,
                message =>
                {
                    if (fail)
                        throw new InvalidOperationException("sink unavailable");

                    logs.Add(message);
                },
                clock);

        clock.Fire();
        logs.Should().BeEmpty();
        snapshot = new InvocationCacheStatistics().GetSnapshot();
        fail = true;
        var fire = () => clock.Fire();
        fire.Should().NotThrow();
        fail = false;
        clock.Fire();
        logs.Should().ContainSingle().Which.Should().Contain("admissions=0");
        snapshotFailure = true;
        clock.Fire();
        logs.Last().Should().StartWith("Invocation-cache statistics report failed:");
        snapshotFailure = false;
        clock.Fire();
        logs.Last().Should().StartWith("Invocation-cache statistics (");
    }

    [Fact]
    public void Invocation_cache_statistics_log_includes_shared_storage_and_stable_expression_order()
    {
        var statistics = new InvocationCacheStatistics();
        var shared = new GenerationalInvocationCache();
        var cache = statistics.Observe(shared);

        var key =
            new EvalCacheEntryKey(
                PineValue.Blob([10]),
                StackFrameInput.GenericFromEnvironmentValue(PineValue.EmptyList));

        cache.TryAdd(key, PineValue.EmptyList);
        cache.TryGet(key, out _);
        cache.MayContainExpression(PineValue.Blob([20]));
        var snapshot = statistics.GetSnapshot() with { SharedCacheGenerations = shared.GetStatistics() };
        var report = InvocationCacheStatisticsLog.Format(snapshot);
        report.Should().Contain("Shared storage: unique entries=1, physical entries=1");
        var lines = report.Split(Environment.NewLine);
        lines[1].Should().StartWith("Totals: ");
        lines[2].Should().StartWith("Shared storage: ");
        lines[3].Should().EndWith("skipped lookups=0");
        lines[4].Should().EndWith("skipped lookups=1");
        InvocationCacheStatisticsLog.Format(snapshot).Should().Be(report);
    }

    private static IEnumerable<string> Reports(IEnumerable<string> logs) =>
        logs.Where(message => message.StartsWith("Invocation-cache statistics (", StringComparison.Ordinal));

    private static Pine.Core.Elm.LanguageServer.LanguageServer CreateServer(
        ILanguageServiceSessionFactory factory, Action<string> log, TimeProvider clock) =>
        new(
            factory,
            VirtualWorkspace.Create([]).Workspace,
            EmptyElmPackageSource.Instance,
            new StubDiagnosticsProvider(),
            new StubDocumentFormatter(text => text),
            new LanguageServerOptions("test"),
            logDelegate: log,
            timeProvider: clock);

    private static void Initialize(Pine.Core.Elm.LanguageServer.LanguageServer server) =>
        server.Initialize(
            new InitializeParams(
                ProcessId: null,
                Capabilities: new ClientCapabilities(Workspace: null, TextDocument: null),
                RootPath: null,
                RootUri: null,
                WorkspaceFolders: [],
                ClientInfo: null));

    private sealed class StatisticsSession(InvocationCacheStatistics statistics)
        : RecordingLanguageServiceSession, ILanguageServiceSession
    {
        public InvocationCacheStatisticsSnapshot GetInvocationCacheStatistics() => statistics.GetSnapshot();
    }

    private sealed class DisposableSession : RecordingLanguageServiceSession, ILanguageServiceSession
    {
        public int DisposeCount { get; private set; }

        public void Dispose() => ++DisposeCount;
    }

    private sealed class BlockingSessionFactory : ILanguageServiceSessionFactory
    {
        public TaskCompletionSource Started { get; } = new(TaskCreationOptions.RunContinuationsAsynchronously);

        public TaskCompletionSource<Result<string, ILanguageServiceSession>> Completion { get; } =
            new(TaskCreationOptions.RunContinuationsAsynchronously);

        public ValueTask<Result<string, ILanguageServiceSession>> CreateSessionAsync(
            LanguageServerOptions options, CancellationToken cancellationToken)
        {
            Started.SetResult();
            return new(Completion.Task);
        }
    }

    private sealed class RecordingTimeProvider : TimeProvider
    {
        private TimerCallback? _callback;

        private object? _state;

        public TimeSpan DueTime { get; private set; }

        public TimeSpan Period { get; private set; }

        public bool Created => _callback is not null;

        public RecordingTimer Timer { get; } = new();

        public override ITimer CreateTimer(TimerCallback callback, object? state, TimeSpan dueTime, TimeSpan period)
        {
            _callback = callback;
            _state = state;
            DueTime = dueTime;
            Period = period;
            return Timer;
        }

        public void Fire() => _callback!(_state);
    }

    private sealed class RecordingTimer : ITimer
    {
        public bool Disposed { get; private set; }

        public bool Change(TimeSpan dueTime, TimeSpan period) => !Disposed;

        public void Dispose() => Disposed = true;

        public ValueTask DisposeAsync()
        {
            Dispose();
            return ValueTask.CompletedTask;
        }
    }
}
