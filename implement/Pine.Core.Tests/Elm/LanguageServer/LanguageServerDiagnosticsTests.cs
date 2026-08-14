using AwesomeAssertions;
using Pine.Core.Elm.LanguageServer;
using Pine.Core.LanguageServerProtocol;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.Linq;
using System.Threading;
using System.Threading.Tasks;
using Xunit;

using ElmLanguageServer = Pine.Core.Elm.LanguageServer.LanguageServer;

namespace Pine.Core.Tests.Elm.LanguageServer;

/// <summary>
/// Tests for the diagnostics engine and the formatting request, using in-memory sources and
/// providers under the control of the test.
/// </summary>
public class LanguageServerDiagnosticsTests
{
    private static Diagnostic DiagnosticWithMessage(string message, uint line = 0) =>
        new(
            Range: new Range(
                Start: new Position(Line: line, Character: 0),
                End: new Position(Line: line, Character: 1)),
            Severity: DiagnosticSeverity.Error,
            Code: null,
            CodeDescription: null,
            Source: "test",
            Message: message,
            Tags: null,
            RelatedInformation: null);

    private sealed class PublishedDiagnostics
    {
        private readonly ConcurrentQueue<PublishDiagnosticsParams> _published = new();

        public void Publish(PublishDiagnosticsParams publishParams) =>
            _published.Enqueue(publishParams);

        public IReadOnlyList<PublishDiagnosticsParams> All => [.. _published];

        public IReadOnlyList<string> Messages(string documentUri) =>
            [
            .._published
            .Where(item => item.Uri == documentUri)
            .LastOrDefault()
            ?.Diagnostics
            .Select(diagnostic => diagnostic.Message)
            ?? []
            ];

        public void Clear()
        {
            _published.Clear();
        }
    }

    private static (ElmLanguageServer Server, PublishedDiagnostics Published) CreateServer(
        IDiagnosticsProvider diagnosticsProvider,
        IReadOnlyList<(string[] Path, string Content)>? files = null,
        IDocumentFormatter? documentFormatter = null,
        IDiagnosticsProvider? formattingDiagnosticsProvider = null)
    {
        var (workspace, _) = VirtualWorkspace.Create(files);

        var published = new PublishedDiagnostics();

        var server =
            new ElmLanguageServer(
                StubLanguageServiceSessionFactory.WithSession(new RecordingLanguageServiceSession()),
                workspace,
                EmptyElmPackageSource.Instance,
                diagnosticsProvider,
                documentFormatter ?? new StubDocumentFormatter(text => text),
                new LanguageServerOptions(ServerVersion: "test"),
                logDelegate: null,
                formattingDiagnosticsProvider);

        server.SetDiagnosticsPublisher(published.Publish);

        server.Initialize(
            new InitializeParams(
                ProcessId: null,
                Capabilities: new ClientCapabilities(Workspace: null, TextDocument: null),
                RootPath: null,
                RootUri: VirtualWorkspace.RootUri,
                WorkspaceFolders: [],
                ClientInfo: null));

        server.WorkspaceInitializationTask!.Wait();

        return (server, published);
    }

    [Fact]
    public async Task DidSave_forwards_the_saved_document_as_entry_point()
    {
        var provider = new StubDiagnosticsProvider();

        var (server, published) =
            CreateServer(provider, [(["Main.elm"], "module Main exposing (..)\n")]);

        var documentUri = VirtualWorkspace.DocumentUri("Main.elm");

        provider.SetResult(
            documentUri,
            Result<DiagnosticsProviderError, IReadOnlyList<DocumentDiagnostics>>.ok(
                [new DocumentDiagnostics(documentUri, [DiagnosticWithMessage("first")])]));

        await server.TextDocument_didSaveAsync(
            new DidSaveTextDocumentParams(new TextDocumentIdentifier(documentUri), Text: null));

        provider.Requests.Should().Equal(documentUri);

        published.Messages(documentUri).Should().Equal("first");
    }

    [Fact]
    public async Task DidSave_with_text_for_a_document_which_is_not_open_does_not_shadow_the_workspace()
    {
        var provider = new StubDiagnosticsProvider();

        var (server, _) =
            CreateServer(provider, [(["Main.elm"], "module Main exposing (..)\n")]);

        var documentUri = VirtualWorkspace.DocumentUri("Main.elm");

        await server.TextDocument_didSaveAsync(
            new DidSaveTextDocumentParams(
                new TextDocumentIdentifier(documentUri),
                Text: "module Main exposing (..)\n\n\nfromSave = 1\n"));

        server.TryGetDocumentText(documentUri).Should().Be("module Main exposing (..)\n");
    }

    [Fact]
    public async Task Diagnostics_from_a_successful_run_without_findings_clear_previous_diagnostics()
    {
        var provider = new StubDiagnosticsProvider();

        var (server, published) = CreateServer(provider);

        var documentUri = VirtualWorkspace.DocumentUri("Main.elm");

        provider.SetResult(
            documentUri,
            Result<DiagnosticsProviderError, IReadOnlyList<DocumentDiagnostics>>.ok(
                [new DocumentDiagnostics(documentUri, [DiagnosticWithMessage("first")])]));

        await server.TextDocument_didSaveAsync(
            new DidSaveTextDocumentParams(new TextDocumentIdentifier(documentUri), Text: null));

        published.Messages(documentUri).Should().Equal("first");

        provider.SetResult(
            documentUri,
            Result<DiagnosticsProviderError, IReadOnlyList<DocumentDiagnostics>>.ok([]));

        await server.TextDocument_didSaveAsync(
            new DidSaveTextDocumentParams(new TextDocumentIdentifier(documentUri), Text: null));

        published.Messages(documentUri).Should().BeEmpty();
    }

    [Fact]
    public async Task Diagnostics_from_a_failed_run_retain_the_previous_diagnostics()
    {
        var provider = new StubDiagnosticsProvider();

        var (server, published) = CreateServer(provider);

        var documentUri = VirtualWorkspace.DocumentUri("Main.elm");

        provider.SetResult(
            documentUri,
            Result<DiagnosticsProviderError, IReadOnlyList<DocumentDiagnostics>>.ok(
                [new DocumentDiagnostics(documentUri, [DiagnosticWithMessage("first")])]));

        await server.TextDocument_didSaveAsync(
            new DidSaveTextDocumentParams(new TextDocumentIdentifier(documentUri), Text: null));

        published.Clear();

        provider.SetResult(
            documentUri,
            new DiagnosticsProviderError(
                DiagnosticsProviderErrorKind.ProviderFailure,
                "failed to run the compiler"));

        await server.TextDocument_didSaveAsync(
            new DidSaveTextDocumentParams(new TextDocumentIdentifier(documentUri), Text: null));

        published.All.Should().BeEmpty();

        /*
         * The retained diagnostics are still published when another entry point reports.
         * */
        var otherUri = VirtualWorkspace.DocumentUri("Other.elm");

        provider.SetResult(
            otherUri,
            Result<DiagnosticsProviderError, IReadOnlyList<DocumentDiagnostics>>.ok(
                [new DocumentDiagnostics(documentUri, [DiagnosticWithMessage("second")])]));

        await server.TextDocument_didSaveAsync(
            new DidSaveTextDocumentParams(new TextDocumentIdentifier(otherUri), Text: null));

        published.Messages(documentUri).Should().Equal("first", "second");
    }

    [Fact]
    public async Task Diagnostics_canceled_while_running_are_not_published()
    {
        var provider = new StubDiagnosticsProvider();

        var (server, published) = CreateServer(provider);

        var documentUri = VirtualWorkspace.DocumentUri("Main.elm");

        using var cancellation = new CancellationTokenSource();

        provider.SetHandler(
            documentUri,
            cancellationToken =>
            {
                cancellation.Cancel();

                cancellationToken.ThrowIfCancellationRequested();

                return
                    ValueTask.FromResult(
                        Result<DiagnosticsProviderError, IReadOnlyList<DocumentDiagnostics>>.ok(
                            (IReadOnlyList<DocumentDiagnostics>)
                            [new DocumentDiagnostics(documentUri, [DiagnosticWithMessage("canceled")])]));
            });

        await server.TextDocument_didSaveAsync(
            new DidSaveTextDocumentParams(new TextDocumentIdentifier(documentUri), Text: null),
            cancellation.Token);

        published.All.Should().BeEmpty();
    }

    [Fact]
    public async Task Diagnostics_completing_out_of_order_do_not_overwrite_the_newer_run()
    {
        var provider = new StubDiagnosticsProvider();

        var (server, published) = CreateServer(provider);

        var documentUri = VirtualWorkspace.DocumentUri("Main.elm");

        var firstRunStarted = new TaskCompletionSource();
        var firstRunContinue = new TaskCompletionSource();

        provider.SetHandler(
            documentUri,
            async _ =>
            {
                firstRunStarted.TrySetResult();

                await firstRunContinue.Task;

                return
                    Result<DiagnosticsProviderError, IReadOnlyList<DocumentDiagnostics>>.ok(
                        [new DocumentDiagnostics(documentUri, [DiagnosticWithMessage("from first run")])]);
            });

        var firstRun =
            server.TextDocument_didSaveAsync(
                new DidSaveTextDocumentParams(new TextDocumentIdentifier(documentUri), Text: null));

        await firstRunStarted.Task;

        provider.SetResult(
            documentUri,
            Result<DiagnosticsProviderError, IReadOnlyList<DocumentDiagnostics>>.ok(
                [new DocumentDiagnostics(documentUri, [DiagnosticWithMessage("from second run")])]));

        await server.TextDocument_didSaveAsync(
            new DidSaveTextDocumentParams(new TextDocumentIdentifier(documentUri), Text: null));

        published.Messages(documentUri).Should().Equal("from second run");

        firstRunContinue.TrySetResult();

        await firstRun;

        published.Messages(documentUri).Should().Equal("from second run");
    }

    [Fact]
    public async Task Diagnostics_computed_from_outdated_sources_are_discarded()
    {
        var provider = new StubDiagnosticsProvider();

        var (server, published) = CreateServer(provider);

        var documentUri = VirtualWorkspace.DocumentUri("Main.elm");

        var runStarted = new TaskCompletionSource();
        var runContinue = new TaskCompletionSource();

        provider.SetHandler(
            documentUri,
            async _ =>
            {
                runStarted.TrySetResult();

                await runContinue.Task;

                return
                    Result<DiagnosticsProviderError, IReadOnlyList<DocumentDiagnostics>>.ok(
                        [new DocumentDiagnostics(documentUri, [DiagnosticWithMessage("outdated")])]);
            });

        var run =
            server.TextDocument_didSaveAsync(
                new DidSaveTextDocumentParams(new TextDocumentIdentifier(documentUri), Text: null));

        await runStarted.Task;

        server.TextDocument_didOpen(
            new TextDocumentItem(documentUri, "elm", Version: 1, Text: "module Main exposing (..)\n"));

        runContinue.TrySetResult();

        await run;

        published.All.Should().BeEmpty();
    }

    [Fact]
    public async Task Diagnostics_aggregate_over_entry_points_deduplicated_and_ordered()
    {
        var provider = new StubDiagnosticsProvider();

        var (server, published) = CreateServer(provider);

        var mainUri = VirtualWorkspace.DocumentUri("Main.elm");
        var testsUri = VirtualWorkspace.DocumentUri("Tests.elm");
        var sharedUri = VirtualWorkspace.DocumentUri("Shared.elm");

        provider.SetResult(
            mainUri,
            Result<DiagnosticsProviderError, IReadOnlyList<DocumentDiagnostics>>.ok(
                [
                    new DocumentDiagnostics(
                        sharedUri,
                        [
                            DiagnosticWithMessage("shared problem", line: 3),
                            DiagnosticWithMessage("only from main", line: 1),
                        ]),
                    new DocumentDiagnostics(mainUri, [DiagnosticWithMessage("only in main")]),
                ]));

        provider.SetResult(
            testsUri,
            Result<DiagnosticsProviderError, IReadOnlyList<DocumentDiagnostics>>.ok(
                [
                    new DocumentDiagnostics(
                        sharedUri,
                        [
                            DiagnosticWithMessage("shared problem", line: 3),
                            DiagnosticWithMessage("only from tests", line: 2),
                        ]),
                ]));

        await server.TextDocument_didSaveAsync(
            new DidSaveTextDocumentParams(new TextDocumentIdentifier(mainUri), Text: null));

        await server.TextDocument_didSaveAsync(
            new DidSaveTextDocumentParams(new TextDocumentIdentifier(testsUri), Text: null));

        /*
         * The diagnostic reported by both entry points appears once, and the diagnostics are
         * ordered by location.
         * */
        published.Messages(sharedUri).Should().Equal(
            "only from main",
            "only from tests",
            "shared problem");

        published.Messages(mainUri).Should().Equal("only in main");

        /*
         * Dropping one entry point only removes the diagnostics contributed by it.
         * */
        server.Workspace_didChangeWatchedFiles(
            [new FileEvent(testsUri, FileChangeType.Deleted)]);

        published.Messages(sharedUri).Should().Equal(
            "only from main",
            "shared problem");
    }

    [Fact]
    public async Task Closing_a_deleted_document_removes_the_diagnostics_it_owned()
    {
        var provider = new StubDiagnosticsProvider();

        var (server, published) = CreateServer(provider);

        var documentUri = VirtualWorkspace.DocumentUri("Main.elm");
        var otherUri = VirtualWorkspace.DocumentUri("Other.elm");

        provider.SetResult(
            documentUri,
            Result<DiagnosticsProviderError, IReadOnlyList<DocumentDiagnostics>>.ok(
                [
                    new DocumentDiagnostics(documentUri, [DiagnosticWithMessage("in entry point")]),
                    new DocumentDiagnostics(otherUri, [DiagnosticWithMessage("in other document")]),
                ]));

        server.TextDocument_didOpen(
            new TextDocumentItem(documentUri, "elm", Version: 1, Text: "module Main exposing (..)\n"));

        await server.TextDocument_didSaveAsync(
            new DidSaveTextDocumentParams(new TextDocumentIdentifier(documentUri), Text: null));

        published.Messages(otherUri).Should().Equal("in other document");

        server.TextDocument_didClose(new TextDocumentIdentifier(documentUri));

        published.Messages(documentUri).Should().BeEmpty();
        published.Messages(otherUri).Should().BeEmpty();
    }

    [Fact]
    public async Task Formatting_returns_edits_and_updates_the_open_document()
    {
        var provider = new StubDiagnosticsProvider();

        var formattingProvider = new StubDiagnosticsProvider();

        var (server, published) =
            CreateServer(
                provider,
                files: [(["Main.elm"], "module Main exposing (..)\n")],
                documentFormatter: new StubDocumentFormatter(text => text.Replace("  ", " ")),
                formattingDiagnosticsProvider: formattingProvider);

        var documentUri = VirtualWorkspace.DocumentUri("Main.elm");

        server.TextDocument_didOpen(
            new TextDocumentItem(
                documentUri,
                "elm",
                Version: 1,
                Text: "module Main exposing (..)\n\n\nname =\n  0\n"));

        var edits =
            await server.TextDocument_formattingAsync(
                new TextDocumentIdentifier(documentUri),
                new FormattingOptions());

        edits.Should().NotBeEmpty();

        ElmLanguageServer.ApplyTextEdits("module Main exposing (..)\n\n\nname =\n  0\n", edits)
            .Should().Be("module Main exposing (..)\n\n\nname =\n 0\n");

        server.TryGetDocumentText(documentUri)
            .Should().Be("module Main exposing (..)\n\n\nname =\n 0\n");

        formattingProvider.Requests.Should().Equal(documentUri);

        published.All.Should().ContainSingle()
            .Which.Uri.Should().Be(documentUri);
    }

    [Fact]
    public async Task Formatting_a_document_which_cannot_be_formatted_publishes_diagnostics()
    {
        var formattingProvider = new StubDiagnosticsProvider();

        var documentUri = VirtualWorkspace.DocumentUri("Main.elm");

        formattingProvider.SetResult(
            documentUri,
            Result<DiagnosticsProviderError, IReadOnlyList<DocumentDiagnostics>>.ok(
                [new DocumentDiagnostics(documentUri, [DiagnosticWithMessage("syntax error")])]));

        var (server, published) =
            CreateServer(
                new StubDiagnosticsProvider(),
                files: [(["Main.elm"], "module Main exposing (..)\n")],
                documentFormatter:
                new StubDocumentFormatter(
                    _ =>
                    new DocumentFormattingError(
                        DocumentFormattingErrorKind.SyntaxError,
                        "does not parse")),
                formattingDiagnosticsProvider: formattingProvider);

        var edits =
            await server.TextDocument_formattingAsync(
                new TextDocumentIdentifier(documentUri),
                new FormattingOptions());

        edits.Should().BeEmpty();

        published.Messages(documentUri).Should().Equal("syntax error");
    }

    [Fact]
    public async Task Formatting_a_document_which_is_not_open_uses_the_workspace_contents()
    {
        var (server, _) =
            CreateServer(
                new StubDiagnosticsProvider(),
                files: [(["Main.elm"], "module Main exposing (..)\n\n\nname =\n  0\n")],
                documentFormatter: new StubDocumentFormatter(text => text.Replace("  ", " ")));

        var documentUri = VirtualWorkspace.DocumentUri("Main.elm");

        var edits =
            await server.TextDocument_formattingAsync(
                new TextDocumentIdentifier(documentUri),
                new FormattingOptions());

        ElmLanguageServer.ApplyTextEdits("module Main exposing (..)\n\n\nname =\n  0\n", edits)
            .Should().Be("module Main exposing (..)\n\n\nname =\n 0\n");

        /*
         * Formatting does not create an overlay for a document which is not open.
         * */
        server.TryGetDocumentText(documentUri)
            .Should().Be("module Main exposing (..)\n\n\nname =\n  0\n");
    }

    [Fact]
    public async Task Formatting_an_unknown_document_returns_no_edits()
    {
        var (server, published) = CreateServer(new StubDiagnosticsProvider());

        var edits =
            await server.TextDocument_formattingAsync(
                new TextDocumentIdentifier(VirtualWorkspace.DocumentUri("Missing.elm")),
                new FormattingOptions());

        edits.Should().BeEmpty();

        published.All.Should().BeEmpty();
    }
}
