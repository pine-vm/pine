using AwesomeAssertions;
using Pine.Core.Elm.LanguageServer;
using Pine.Core.Elm.LanguageServer.LanguageServiceInterface;
using Pine.Core.LanguageServerProtocol;
using System;
using System.Collections.Generic;
using System.Threading;
using System.Threading.Tasks;
using Xunit;

using ElmLanguageServer = Pine.Core.Elm.LanguageServer.LanguageServer;

namespace Pine.Core.Tests.Elm.LanguageServer;

public class LanguageServerFormattingTests
{
    [Fact]
    public async Task Formatting_completes_from_latest_client_text_while_language_service_update_is_blocked()
    {
        var (workspace, _) = VirtualWorkspace.Create();
        var session = new ControlledLanguageServiceSession();
        var logs = new List<string>();
        var formattedSource = "";

        var server =
            CreateServer(
                workspace,
                StubLanguageServiceSessionFactory.WithSession(session),
                new DelegateDocumentFormatter(
                    (source, _) =>
                    {
                        formattedSource = source;
                        return ValueTask.FromResult(FormatResult(source.Replace("  0", "    0")));
                    }),
                logs.Add);

        await InitializeAsync(server);

        var documentUri = VirtualWorkspace.DocumentUri("Main.elm");
        const string original = "module Main exposing (..)\n\nvalue =\n  0\n";
        const string changed = "module Main exposing (..)\n\nvalue =\n  0\n\nother = 1\n";

        await server.TextDocument_didOpenAsync(
            new TextDocumentItem(documentUri, "elm", Version: 1, Text: original));

        session.BlockNextAsynchronousAdd();

        var update =
            server.TextDocument_didChangeAsync(
                new VersionedTextDocumentIdentifier(documentUri, Version: 2),
                [new TextDocumentContentChangeEvent(null, null, changed)]);

        await session.AsynchronousAddStarted.WaitAsync(TimeSpan.FromSeconds(5));

        try
        {
            var formatting =
                server.TextDocument_formattingAsync(
                    new TextDocumentIdentifier(documentUri),
                    new FormattingOptions());

            var edits = await formatting.WaitAsync(TimeSpan.FromSeconds(5));

            update.IsCompleted.Should().BeFalse();
            formattedSource.Should().Be(changed);

            ElmLanguageServer.ApplyTextEdits(changed, edits)
                .Should().Be(changed.Replace("  0", "    0"));

            server.TryGetDocumentText(documentUri).Should().Be(changed);

            logs.Should().Contain(log => log.Contains("Formatting request 1 captured client source"));
            logs.Should().Contain(log => log.Contains("client version: 2"));
            logs.Should().Contain(log => log.Contains("document generation: 2"));
            logs.Should().Contain(log => log.Contains("pending document update: 2"));
            logs.Should().Contain(log => log.Contains("language-service version: 1"));
            logs.Should().Contain(log => log.Contains("starting formatter"));
            logs.Should().Contain(log => log.Contains("returning 1 text edits"));
        }
        finally
        {
            session.ReleaseAsynchronousAdd();
        }

        await update.WaitAsync(TimeSpan.FromSeconds(5));
        session.TryGetFile(documentUri).Should().Be(changed);
    }

    [Fact]
    public async Task Formatting_open_document_does_not_wait_for_workspace_initialization()
    {
        var (workspace, _) = VirtualWorkspace.Create();
        var session = new ControlledLanguageServiceSession();
        var sessionFactory = new BlockingLanguageServiceSessionFactory(session);
        var formattedSource = "";

        var server =
            CreateServer(
                workspace,
                sessionFactory,
                new DelegateDocumentFormatter(
                    (source, _) =>
                    {
                        formattedSource = source;
                        return ValueTask.FromResult(FormatResult(source));
                    }));

        server.Initialize(InitializeParams());

        await sessionFactory.CreationStarted.WaitAsync(TimeSpan.FromSeconds(5));

        var documentUri = VirtualWorkspace.DocumentUri("Main.elm");
        const string content = "module Main exposing (..)\n";

        var opening =
            server.TextDocument_didOpenAsync(
                new TextDocumentItem(documentUri, "elm", Version: 1, Text: content));

        try
        {
            var edits =
                await server.TextDocument_formattingAsync(
                    new TextDocumentIdentifier(documentUri),
                    new FormattingOptions())
                .WaitAsync(TimeSpan.FromSeconds(5));

            opening.IsCompleted.Should().BeFalse();
            formattedSource.Should().Be(content);
            edits.Should().BeEmpty();
        }
        finally
        {
            sessionFactory.ReleaseCreation();
        }

        await opening.WaitAsync(TimeSpan.FromSeconds(5));
    }

    [Fact]
    public async Task Formatting_suppresses_result_when_document_identity_changes()
    {
        var (workspace, _) = VirtualWorkspace.Create();
        var session = new ControlledLanguageServiceSession();

        var formatterStarted =
            new TaskCompletionSource(TaskCreationOptions.RunContinuationsAsynchronously);

        var releaseFormatter =
            new TaskCompletionSource(TaskCreationOptions.RunContinuationsAsynchronously);

        var logs = new List<string>();

        var server =
            CreateServer(
                workspace,
                StubLanguageServiceSessionFactory.WithSession(session),
                new DelegateDocumentFormatter(
                    async (source, cancellationToken) =>
                    {
                        formatterStarted.TrySetResult();
                        await releaseFormatter.Task.WaitAsync(cancellationToken);
                        return FormatResult(source + "\n");
                    }),
                logs.Add);

        await InitializeAsync(server);

        var documentUri = VirtualWorkspace.DocumentUri("Main.elm");

        await server.TextDocument_didOpenAsync(
            new TextDocumentItem(documentUri, "elm", Version: 1, Text: "version 1"));

        var formatting =
            server.TextDocument_formattingAsync(
                new TextDocumentIdentifier(documentUri),
                new FormattingOptions());

        await formatterStarted.Task.WaitAsync(TimeSpan.FromSeconds(5));

        await server.TextDocument_didChangeAsync(
            new VersionedTextDocumentIdentifier(documentUri, Version: 2),
            [new TextDocumentContentChangeEvent(null, null, "version 2")]);

        releaseFormatter.TrySetResult();

        var edits = await formatting.WaitAsync(TimeSpan.FromSeconds(5));

        edits.Should().BeEmpty();
        server.TryGetDocumentText(documentUri).Should().Be("version 2");

        logs.Should().Contain(
            log =>
            log.Contains("suppressing stale result") &&
            log.Contains("version=1") &&
            log.Contains("version=2"));
    }

    [Fact]
    public async Task Formatting_honors_client_cancellation_when_formatter_cannot_stop()
    {
        var (workspace, _) = VirtualWorkspace.Create();
        var session = new ControlledLanguageServiceSession();

        var formatterStarted =
            new TaskCompletionSource(TaskCreationOptions.RunContinuationsAsynchronously);

        var releaseFormatter =
            new TaskCompletionSource(TaskCreationOptions.RunContinuationsAsynchronously);

        var logs = new List<string>();

        var server =
            CreateServer(
                workspace,
                StubLanguageServiceSessionFactory.WithSession(session),
                new DelegateDocumentFormatter(
                    async (_, _) =>
                    {
                        formatterStarted.TrySetResult();
                        await releaseFormatter.Task;
                        return FormatResult("");
                    }),
                logs.Add);

        await InitializeAsync(server);

        var documentUri = VirtualWorkspace.DocumentUri("Main.elm");

        await server.TextDocument_didOpenAsync(
            new TextDocumentItem(documentUri, "elm", Version: 1, Text: "source"));

        using var cancellation = new CancellationTokenSource();

        var formatting =
            server.TextDocument_formattingAsync(
                new TextDocumentIdentifier(documentUri),
                new FormattingOptions(),
                cancellation.Token);

        await formatterStarted.Task.WaitAsync(TimeSpan.FromSeconds(5));
        cancellation.Cancel();
        releaseFormatter.TrySetResult();

        Func<Task> awaitFormatting = async () => await formatting;

        await awaitFormatting.Should().ThrowAsync<OperationCanceledException>();

        logs.Should().Contain(
            log =>
            log.Contains("Formatting request 1") &&
            log.Contains("observed client cancellation"));
    }

    [Fact]
    public async Task Formatting_rejects_requests_beyond_bounded_capacity()
    {
        var (workspace, _) = VirtualWorkspace.Create();
        var session = new ControlledLanguageServiceSession();

        var formatterStarted =
            new TaskCompletionSource(TaskCreationOptions.RunContinuationsAsynchronously);

        var releaseFormatter =
            new TaskCompletionSource(TaskCreationOptions.RunContinuationsAsynchronously);

        var logs = new List<string>();

        var server =
            CreateServer(
                workspace,
                StubLanguageServiceSessionFactory.WithSession(session),
                new DelegateDocumentFormatter(
                    async (source, _) =>
                    {
                        formatterStarted.TrySetResult();
                        await releaseFormatter.Task;
                        return FormatResult(source + "\n");
                    }),
                logs.Add);

        await InitializeAsync(server);

        var documentUri = VirtualWorkspace.DocumentUri("Main.elm");

        await server.TextDocument_didOpenAsync(
            new TextDocumentItem(documentUri, "elm", Version: 1, Text: "source"));

        var formattingRequests =
            new List<Task<IReadOnlyList<Pine.Core.LanguageServerProtocol.TextEdit>>>();

        formattingRequests.Add(
            server.TextDocument_formattingAsync(
                new TextDocumentIdentifier(documentUri),
                new FormattingOptions()));

        await formatterStarted.Task.WaitAsync(TimeSpan.FromSeconds(5));

        for (var index = 1; index < 9; ++index)
        {
            formattingRequests.Add(
                server.TextDocument_formattingAsync(
                    new TextDocumentIdentifier(documentUri),
                    new FormattingOptions()));
        }

        try
        {
            var rejectedResult =
                await formattingRequests[^1].WaitAsync(TimeSpan.FromSeconds(5));

            rejectedResult.Should().BeEmpty();

            logs.Should().Contain(
                log =>
                log.Contains("Formatting request 9 rejected") &&
                log.Contains("capacity 8 is full"));
        }
        finally
        {
            releaseFormatter.TrySetResult();
        }

        await Task.WhenAll(formattingRequests).WaitAsync(TimeSpan.FromSeconds(5));

        for (var index = 0; index < 8; ++index)
        {
            formattingRequests[index].Result.Should().NotBeEmpty();
        }
    }

    [Fact]
    public async Task Formatting_failure_diagnostics_do_not_delay_response()
    {
        var (workspace, _) = VirtualWorkspace.Create();
        var session = new ControlledLanguageServiceSession();
        var diagnostics = new BlockingDiagnosticsProvider();

        var server =
            CreateServer(
                workspace,
                StubLanguageServiceSessionFactory.WithSession(session),
                new DelegateDocumentFormatter(
                    (_, _) =>
                    ValueTask.FromResult<Result<DocumentFormattingError, string>>(
                        new DocumentFormattingError(
                            DocumentFormattingErrorKind.SyntaxError,
                            "does not parse"))),
                formattingDiagnosticsProvider: diagnostics);

        await InitializeAsync(server);

        var documentUri = VirtualWorkspace.DocumentUri("Main.elm");

        await server.TextDocument_didOpenAsync(
            new TextDocumentItem(documentUri, "elm", Version: 1, Text: "not valid"));

        try
        {
            var edits =
                await server.TextDocument_formattingAsync(
                    new TextDocumentIdentifier(documentUri),
                    new FormattingOptions())
                .WaitAsync(TimeSpan.FromSeconds(5));

            edits.Should().BeEmpty();

            await diagnostics.Started.WaitAsync(TimeSpan.FromSeconds(5));
            diagnostics.Completed.IsCompleted.Should().BeFalse();
        }
        finally
        {
            diagnostics.Release();
        }

        await diagnostics.Completed.WaitAsync(TimeSpan.FromSeconds(5));
    }

    [Fact]
    public async Task Formatting_is_not_blocked_by_watched_file_language_service_mutation()
    {
        const string workspaceContent = "module Main exposing (..)\n\nvalue =\n  0\n";

        var (workspace, store) =
            VirtualWorkspace.Create([(["Main.elm"], workspaceContent)]);

        var session = new ControlledLanguageServiceSession();

        var server =
            CreateServer(
                workspace,
                StubLanguageServiceSessionFactory.WithSession(session),
                new DelegateDocumentFormatter(
                    (source, _) =>
                    ValueTask.FromResult(FormatResult(source.Replace("  0", "    0")))));

        await InitializeAsync(server);

        var documentUri = VirtualWorkspace.DocumentUri("Main.elm");
        const string changedWorkspaceContent = "module Main exposing (..)\n\nvalue =\n  0\n\nother = 1\n";

        VirtualWorkspace.SetFile(store, ["Main.elm"], changedWorkspaceContent);
        session.BlockNextSynchronousMutation();

        var watchedFileUpdate =
            Task.Run(
                () =>
                server.Workspace_didChangeWatchedFiles(
                    [new FileEvent(documentUri, FileChangeType.Changed)]));

        await session.SynchronousMutationStarted.WaitAsync(TimeSpan.FromSeconds(5));

        try
        {
            var edits =
                await server.TextDocument_formattingAsync(
                    new TextDocumentIdentifier(documentUri),
                    new FormattingOptions())
                .WaitAsync(TimeSpan.FromSeconds(5));

            watchedFileUpdate.IsCompleted.Should().BeFalse();

            ElmLanguageServer.ApplyTextEdits(changedWorkspaceContent, edits)
                .Should().Be(changedWorkspaceContent.Replace("  0", "    0"));
        }
        finally
        {
            session.ReleaseSynchronousMutation();
        }

        await watchedFileUpdate.WaitAsync(TimeSpan.FromSeconds(5));
    }

    private static ElmLanguageServer CreateServer(
        ILanguageServerWorkspace workspace,
        ILanguageServiceSessionFactory sessionFactory,
        IDocumentFormatter formatter,
        Action<string>? logDelegate = null,
        IDiagnosticsProvider? formattingDiagnosticsProvider = null) =>
        new(
            sessionFactory,
            workspace,
            EmptyElmPackageSource.Instance,
            new StubDiagnosticsProvider(),
            formatter,
            new LanguageServerOptions(ServerVersion: "test"),
            logDelegate,
            formattingDiagnosticsProvider);

    private static async Task InitializeAsync(ElmLanguageServer server)
    {
        server.Initialize(InitializeParams());
        await server.WorkspaceInitializationTask!;
    }

    private static InitializeParams InitializeParams() =>
        new(
            ProcessId: null,
            Capabilities: new ClientCapabilities(Workspace: null, TextDocument: null),
            RootPath: null,
            RootUri: VirtualWorkspace.RootUri,
            WorkspaceFolders: [],
            ClientInfo: null);

    private static Result<DocumentFormattingError, string> FormatResult(string content) =>
        Result<DocumentFormattingError, string>.ok(content);

    private sealed class DelegateDocumentFormatter(
        Func<string, CancellationToken, ValueTask<Result<DocumentFormattingError, string>>> format)
        : IDocumentFormatter
    {
        public ValueTask<Result<DocumentFormattingError, string>> FormatAsync(
            string documentUri,
            string sourceText,
            FormattingOptions options,
            CancellationToken cancellationToken) =>
            format(sourceText, cancellationToken);
    }

    private sealed class BlockingLanguageServiceSessionFactory(
        ILanguageServiceSession session) : ILanguageServiceSessionFactory
    {
        private readonly TaskCompletionSource _creationStarted =
            new(TaskCreationOptions.RunContinuationsAsynchronously);

        private readonly TaskCompletionSource _releaseCreation =
            new(TaskCreationOptions.RunContinuationsAsynchronously);

        public Task CreationStarted => _creationStarted.Task;

        public void ReleaseCreation() => _releaseCreation.TrySetResult();

        public async ValueTask<Result<string, ILanguageServiceSession>> CreateSessionAsync(
            LanguageServerOptions options,
            CancellationToken cancellationToken)
        {
            _creationStarted.TrySetResult();
            await _releaseCreation.Task.WaitAsync(cancellationToken);
            return Result<string, ILanguageServiceSession>.ok(session);
        }
    }

    private sealed class BlockingDiagnosticsProvider : IDiagnosticsProvider
    {
        private readonly TaskCompletionSource _started =
            new(TaskCreationOptions.RunContinuationsAsynchronously);

        private readonly TaskCompletionSource _release =
            new(TaskCreationOptions.RunContinuationsAsynchronously);

        private readonly TaskCompletionSource _completed =
            new(TaskCreationOptions.RunContinuationsAsynchronously);

        public Task Started => _started.Task;

        public Task Completed => _completed.Task;

        public void Release() => _release.TrySetResult();

        public async ValueTask<Result<DiagnosticsProviderError, IReadOnlyList<DocumentDiagnostics>>>
            GetDiagnosticsAsync(
            string entryPointDocumentUri,
            CancellationToken cancellationToken)
        {
            _started.TrySetResult();

            try
            {
                await _release.Task.WaitAsync(cancellationToken);

                return
                    Result<DiagnosticsProviderError, IReadOnlyList<DocumentDiagnostics>>.ok([]);
            }
            finally
            {
                _completed.TrySetResult();
            }
        }
    }

    private sealed class ControlledLanguageServiceSession : ILanguageServiceSession
    {
        private readonly Dictionary<string, string> _files = new(StringComparer.Ordinal);

        private readonly TaskCompletionSource _asynchronousAddStarted =
            new(TaskCreationOptions.RunContinuationsAsynchronously);

        private readonly TaskCompletionSource _releaseAsynchronousAdd =
            new(TaskCreationOptions.RunContinuationsAsynchronously);

        private readonly TaskCompletionSource _synchronousMutationStarted =
            new(TaskCreationOptions.RunContinuationsAsynchronously);

        private readonly TaskCompletionSource _releaseSynchronousMutation =
            new(TaskCreationOptions.RunContinuationsAsynchronously);

        private int _blockNextAsynchronousAdd;

        private int _blockNextSynchronousMutation;

        public Task AsynchronousAddStarted => _asynchronousAddStarted.Task;

        public Task SynchronousMutationStarted => _synchronousMutationStarted.Task;

        public void BlockNextAsynchronousAdd() =>
            Interlocked.Exchange(ref _blockNextAsynchronousAdd, 1);

        public void ReleaseAsynchronousAdd() => _releaseAsynchronousAdd.TrySetResult();

        public void BlockNextSynchronousMutation() =>
            Interlocked.Exchange(ref _blockNextSynchronousMutation, 1);

        public void ReleaseSynchronousMutation() => _releaseSynchronousMutation.TrySetResult();

        public string? TryGetFile(string documentUri)
        {
            lock (_files)
            {
                return _files.TryGetValue(documentUri, out var content) ? content : null;
            }
        }

        public Result<string, Response.WorkspaceSummaryResponse> AddFile(
            string fileUri,
            string fileContentAsText)
        {
            BlockSynchronousMutationIfRequested();

            lock (_files)
            {
                _files[fileUri] = fileContentAsText;
            }

            return new Response.WorkspaceSummaryResponse();
        }

        public async Task<Result<string, Response.WorkspaceSummaryResponse>> AddFileAsync(
            string fileUri,
            string fileContentAsText,
            CancellationToken cancellationToken = default)
        {
            if (Interlocked.Exchange(ref _blockNextAsynchronousAdd, 0) is 1)
            {
                _asynchronousAddStarted.TrySetResult();
                await _releaseAsynchronousAdd.Task.WaitAsync(cancellationToken);
            }

            lock (_files)
            {
                _files[fileUri] = fileContentAsText;
            }

            return new Response.WorkspaceSummaryResponse();
        }

        public Result<string, Response.WorkspaceSummaryResponse> DeleteFile(string fileUri)
        {
            BlockSynchronousMutationIfRequested();

            lock (_files)
            {
                _files.Remove(fileUri);
            }

            return new Response.WorkspaceSummaryResponse();
        }

        public Result<string, Response.WorkspaceSummaryResponse> AddElmPackage(
            ElmPackageVersion019Identifer packageVersionId,
            IReadOnlyList<KeyValuePair<IReadOnlyList<string>, string>> filesContentsAsText) =>
            new Response.WorkspaceSummaryResponse();

        public Result<string, Response> HandleRequest(Request request) =>
            "Not implemented in this test double";

        private void BlockSynchronousMutationIfRequested()
        {
            if (Interlocked.Exchange(ref _blockNextSynchronousMutation, 0) is not 1)
            {
                return;
            }

            _synchronousMutationStarted.TrySetResult();
            _releaseSynchronousMutation.Task.GetAwaiter().GetResult();
        }
    }
}
