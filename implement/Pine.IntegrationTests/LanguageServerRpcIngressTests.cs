using AwesomeAssertions;
using Nerdbank.Streams;
using Pine.Core;
using Pine.Core.Elm.LanguageServer;
using Pine.Core.Elm.LanguageServer.LanguageServiceInterface;
using Pine.Core.LanguageServerProtocol;
using Pine.Core.Tests.Elm.LanguageServer;
using Pine.Elm;
using StreamJsonRpc;
using System;
using System.Collections.Generic;
using System.Threading;
using System.Threading.Tasks;
using Xunit;

using ElmLanguageServer = Pine.Core.Elm.LanguageServer.LanguageServer;
using ProtocolTextEdit = Pine.Core.LanguageServerProtocol.TextEdit;

namespace Pine.IntegrationTests;

public class LanguageServerRpcIngressTests
{
    [Fact]
    public async Task Formatting_over_JSON_RPC_completes_while_earlier_hover_is_blocked()
    {
        var (workspace, _) = VirtualWorkspace.Create();
        var session = new IngressControlledLanguageServiceSession();
        var logs = new List<string>();

        const string initialContent = "module Main exposing (..)\n\nvalue =\n  0\n";
        var documentUri = VirtualWorkspace.DocumentUri("Main.elm");

        var server =
            new ElmLanguageServer(
                sessionFactory: StubLanguageServiceSessionFactory.WithSession(session),
                workspace: workspace,
                elmPackageSource: EmptyElmPackageSource.Instance,
                diagnosticsProvider: new StubDiagnosticsProvider(),
                documentFormatter: new DelegateDocumentFormatter(
                    (source, _) =>
                    ValueTask.FromResult(Result<DocumentFormattingError, string>.ok(source.Replace("  0", "    0")))),
                options: new LanguageServerOptions(ServerVersion: "test"),
                logDelegate: logs.Add,
                formattingDiagnosticsProvider: null);

        var (clientStream, serverStream) = FullDuplexStream.CreatePair();

        var serverHandler =
            new HeaderDelimitedMessageHandler(
                sendingStream: serverStream,
                receivingStream: serverStream,
                formatter: LanguageServerRpcTarget.JsonRpcMessageFormatterDefault(logs.Add));

        var target = new LanguageServerRpcTarget(server, LogDelegate: logs.Add);
        using var serverRpc = new JsonRpc(serverHandler, target);
        serverRpc.Disconnected += (s, e) => logs.Add($"Server RPC disconnected: {e.Reason}, exception: {e.Exception}");
        target.JsonRpc = serverRpc;
        serverRpc.StartListening();

        var clientHandler =
            new HeaderDelimitedMessageHandler(
                sendingStream: clientStream,
                receivingStream: clientStream,
                formatter: LanguageServerRpcTarget.JsonRpcMessageFormatterDefault());

        using var clientRpc = new JsonRpc(clientHandler);
        clientRpc.StartListening();

        await clientRpc.InvokeWithParameterObjectAsync<InitializeResult>(
            "initialize",
            new InitializeParams(
                ProcessId: Environment.ProcessId,
                Capabilities: new ClientCapabilities(Workspace: null, TextDocument: null),
                RootPath: null,
                RootUri: null,
                WorkspaceFolders: [],
                ClientInfo: null));

        await clientRpc.NotifyAsync(
            "textDocument/didOpen",
            new TextDocumentItem(documentUri, "elm", Version: 1, Text: initialContent));

        session.BlockNextHover();

        var hoverTask =
            clientRpc.InvokeWithParameterObjectAsync<Hover>(
                "textDocument/hover",
                new TextDocumentPositionParams(
                    new TextDocumentIdentifier(documentUri),
                    new Position(0, 0)));

        await session.HoverStarted.WaitAsync(TimeSpan.FromSeconds(5));

        try
        {
            var formattingTask =
                clientRpc.InvokeAsync<IReadOnlyList<ProtocolTextEdit>>(
                    "textDocument/formatting",
                    new TextDocumentIdentifier(documentUri),
                    new FormattingOptions());

            var edits = await formattingTask.WaitAsync(TimeSpan.FromSeconds(5));

            hoverTask.IsCompleted.Should().BeFalse();
            edits.Should().NotBeEmpty();

            logs.Should().Contain(log => log.Contains("RPC message received: textDocument/hover"));
            logs.Should().Contain(log => log.Contains("RPC method textDocument/hover") && log.Contains("invoked"));

            logs.Should().Contain(
                log => log.Contains("RPC method textDocument/hover") && log.Contains("yielding ingress lane"));

            logs.Should().Contain(
                log => log.Contains("RPC method textDocument/hover") && log.Contains("resumed after yield"));

            logs.Should().Contain(log => log.Contains("RPC message received: textDocument/formatting"));
            logs.Should().Contain(log => log.Contains("RPC method textDocument/formatting") && log.Contains("invoked"));

            logs.Should().Contain(
                log => log.Contains("RPC method textDocument/formatting") && log.Contains("completed"));
        }
        finally
        {
            session.ReleaseHover();
        }

        var hover = await hoverTask.WaitAsync(TimeSpan.FromSeconds(5));
        hover.Should().NotBeNull();
        logs.Should().Contain(log => log.Contains("RPC method textDocument/hover") && log.Contains("completed"));
    }

    [Fact]
    public async Task Formatting_over_JSON_RPC_sees_earlier_didChange_while_language_service_update_is_blocked()
    {
        var (workspace, _) = VirtualWorkspace.Create();
        var session = new IngressControlledLanguageServiceSession();
        var logs = new List<string>();

        const string initialContent = "module Main exposing (..)\n\nvalue =\n  0\n";
        const string changedContent = "module Main exposing (..)\n\nvalue =\n  0\n\nother = 1\n";
        var documentUri = VirtualWorkspace.DocumentUri("Main.elm");

        var server =
            new ElmLanguageServer(
                sessionFactory: StubLanguageServiceSessionFactory.WithSession(session),
                workspace: workspace,
                elmPackageSource: EmptyElmPackageSource.Instance,
                diagnosticsProvider: new StubDiagnosticsProvider(),
                documentFormatter: new DelegateDocumentFormatter(
                    (source, _) =>
                    ValueTask.FromResult(Result<DocumentFormattingError, string>.ok(source.Replace("  0", "    0")))),
                options: new LanguageServerOptions(ServerVersion: "test"),
                logDelegate: logs.Add,
                formattingDiagnosticsProvider: null);

        var (clientStream, serverStream) = FullDuplexStream.CreatePair();

        var serverHandler =
            new HeaderDelimitedMessageHandler(
                sendingStream: serverStream,
                receivingStream: serverStream,
                formatter: LanguageServerRpcTarget.JsonRpcMessageFormatterDefault(logs.Add));

        var target = new LanguageServerRpcTarget(server, LogDelegate: logs.Add);
        using var serverRpc = new JsonRpc(serverHandler, target);
        target.JsonRpc = serverRpc;
        serverRpc.StartListening();

        var clientHandler =
            new HeaderDelimitedMessageHandler(
                sendingStream: clientStream,
                receivingStream: clientStream,
                formatter: LanguageServerRpcTarget.JsonRpcMessageFormatterDefault());

        using var clientRpc = new JsonRpc(clientHandler);
        clientRpc.StartListening();

        await clientRpc.InvokeWithParameterObjectAsync<InitializeResult>(
            "initialize",
            new InitializeParams(
                ProcessId: Environment.ProcessId,
                Capabilities: new ClientCapabilities(Workspace: null, TextDocument: null),
                RootPath: null,
                RootUri: null,
                WorkspaceFolders: [],
                ClientInfo: null));

        await clientRpc.NotifyAsync(
            "textDocument/didOpen",
            new TextDocumentItem(documentUri, "elm", Version: 1, Text: initialContent));

        session.BlockNextAsynchronousAdd();

        await clientRpc.NotifyAsync(
            "textDocument/didChange",
            new VersionedTextDocumentIdentifier(documentUri, Version: 2),
            new TextDocumentContentChangeEvent[] { new(null, null, changedContent) });

        await session.AsynchronousAddStarted.WaitAsync(TimeSpan.FromSeconds(5));

        try
        {
            var formattingTask =
                clientRpc.InvokeAsync<IReadOnlyList<ProtocolTextEdit>>(
                    "textDocument/formatting",
                    new TextDocumentIdentifier(documentUri),
                    new FormattingOptions());

            var edits = await formattingTask.WaitAsync(TimeSpan.FromSeconds(5));

            edits.Should().NotBeEmpty();
            var applied = ElmLanguageServer.ApplyTextEdits(changedContent, edits);
            applied.Should().Be(changedContent.Replace("  0", "    0"));

            logs.Should().Contain(log => log.Contains("RPC message received: textDocument/didChange"));
            logs.Should().Contain(log => log.Contains("RPC method textDocument/didChange") && log.Contains("invoked"));

            logs.Should().Contain(
                log => log.Contains("RPC method textDocument/didChange") && log.Contains("yielded ingress lane"));

            logs.Should().Contain(log => log.Contains("RPC message received: textDocument/formatting"));
            logs.Should().Contain(log => log.Contains("RPC method textDocument/formatting") && log.Contains("invoked"));

            logs.Should().Contain(
                log => log.Contains("RPC method textDocument/formatting") && log.Contains("completed"));
        }
        finally
        {
            session.ReleaseAsynchronousAdd();
        }
    }

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

    private sealed class IngressControlledLanguageServiceSession : ILanguageServiceSession
    {
        private readonly Dictionary<string, string> _files = new(StringComparer.Ordinal);

        private readonly TaskCompletionSource _asynchronousAddStarted =
            new(TaskCreationOptions.RunContinuationsAsynchronously);

        private readonly TaskCompletionSource _releaseAsynchronousAdd =
            new(TaskCreationOptions.RunContinuationsAsynchronously);

        private readonly TaskCompletionSource _hoverStarted =
            new(TaskCreationOptions.RunContinuationsAsynchronously);

        private readonly TaskCompletionSource _releaseHover =
            new(TaskCreationOptions.RunContinuationsAsynchronously);

        private int _blockNextAsynchronousAdd;

        private int _blockNextHover;

        public Task AsynchronousAddStarted => _asynchronousAddStarted.Task;

        public Task HoverStarted => _hoverStarted.Task;

        public void BlockNextAsynchronousAdd() =>
            Interlocked.Exchange(ref _blockNextAsynchronousAdd, 1);

        public void ReleaseAsynchronousAdd() => _releaseAsynchronousAdd.TrySetResult();

        public void BlockNextHover() =>
            Interlocked.Exchange(ref _blockNextHover, 1);

        public void ReleaseHover() => _releaseHover.TrySetResult();

        public Result<string, Response.WorkspaceSummaryResponse> AddFile(
            string fileUri,
            string fileContentAsText)
        {
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

        public Result<string, Response> HandleRequest(Request request)
        {
            if (request is Request.ProvideHoverRequest)
            {
                if (Interlocked.Exchange(ref _blockNextHover, 0) is 1)
                {
                    _hoverStarted.TrySetResult();
                    _releaseHover.Task.GetAwaiter().GetResult();
                }

                return new Response.ProvideHoverResponse(["Hover content"]);
            }

            return "Not implemented in this test double";
        }
    }
}
