using AwesomeAssertions;
using Pine.Core.Elm.LanguageServer;
using Pine.Core.Elm.LanguageServer.LanguageServiceInterface;
using Pine.Core.Elm.LanguageServer.MonacoEditor;
using Pine.Core.LanguageServerProtocol;
using System.Collections.Generic;
using System.Linq;
using System.Text.Json;
using System.Threading;
using System.Threading.Tasks;
using Xunit;

using ElmLanguageServer = Pine.Core.Elm.LanguageServer.LanguageServer;
using Interface = Pine.Core.Elm.LanguageServer.LanguageServiceInterface;

namespace Pine.Core.Tests.Elm.LanguageServer;

public class LanguageServerCodeLensTests
{
    private const string DocumentUri = "memory://workspace/Main.elm";

    [Fact]
    public async Task Initialize_advertises_resolvable_CodeLens()
    {
        var (server, _) = CreateServer();

        var initializeResult = await InitializeAsync(server);

        initializeResult.Capabilities.CodeLensProvider.Should().Be(
            new CodeLensOptions(ResolveProvider: true));
    }

    [Fact]
    public async Task CodeLens_discovers_only_root_symbols_and_resolves_usage_count()
    {
        var (server, session) = CreateServer();
        await InitializeAsync(server);
        await OpenDocumentAsync(server);

        var lenses =
            server.TextDocument_codeLens(
                new CodeLensParams(new TextDocumentIdentifier(DocumentUri)));

        lenses.Should().ContainSingle();

        var lens = lenses.Single();

        lens.Range.Should().Be(
            new Range(
                new Position(Line: 2, Character: 0),
                new Position(Line: 2, Character: 0)));
        lens.Command.Should().BeNull();

        var data =
            lens.Data!.Value.Deserialize<CodeLensResolveData>(
                new JsonSerializerOptions
                {
                    PropertyNamingPolicy = JsonNamingPolicy.CamelCase,
                });

        data.Should().NotBeNull();
        data!.DocumentUri.Should().Be(DocumentUri);
        data.Position.Should().Be(new Position(Line: 3, Character: 0));
        data.ClientVersion.Should().Be(1);

        var resolved = server.CodeLens_resolve(lens);

        resolved.Command.Should().NotBeNull();
        resolved.Command!.Title.Should().Be("2 references");
        resolved.Command.Identifier.Should().Be("pine.client.peekReferences");
        resolved.Command.Arguments.Should().BeEquivalentTo(
            new object[] { DocumentUri, new Position(Line: 3, Character: 0) });

        session.ReferenceRequests.Should().ContainSingle();
        session.ReferenceRequests.Single().IncludeDeclaration.Should().BeFalse();
    }

    [Fact]
    public async Task CodeLens_resolve_leaves_lens_unresolved_after_document_changes()
    {
        var (server, session) = CreateServer();
        await InitializeAsync(server);
        await OpenDocumentAsync(server);

        var lens =
            server.TextDocument_codeLens(
                new CodeLensParams(new TextDocumentIdentifier(DocumentUri)))
            .Single();

        await server.TextDocument_didChangeAsync(
            new VersionedTextDocumentIdentifier(DocumentUri, Version: 2),
            [new TextDocumentContentChangeEvent(Range: null, RangeLength: null, Text: "changed")]);

        var resolved = server.CodeLens_resolve(lens);

        resolved.Command.Should().BeNull();
        session.ReferenceRequests.Should().BeEmpty();
    }

    [Fact]
    public async Task CodeLens_discovery_waits_for_pending_update_without_refresh_support()
    {
        var (server, session) = CreateServer();
        await InitializeAsync(server, codeLensRefreshSupport: false);
        session.BlockNextAdd();

        var openTask = OpenDocumentAsync(server);
        await session.AddStarted.Task.WaitAsync(System.TimeSpan.FromSeconds(2));

        var lensesTask =
            Task.Run(
                () =>
                server.TextDocument_codeLens(
                    new CodeLensParams(new TextDocumentIdentifier(DocumentUri))));

        await Task.Delay(100);
        lensesTask.IsCompleted.Should().BeFalse();

        session.AllowAddToComplete.TrySetResult();

        await openTask;
        (await lensesTask).Should().ContainSingle();
    }

    [Fact]
    public async Task CodeLens_refresh_is_negotiated_and_coalesces_accepted_updates()
    {
        var (server, _) = CreateServer();
        var refreshCount = 0;
        var firstRefresh = new TaskCompletionSource(TaskCreationOptions.RunContinuationsAsynchronously);

        server.SetCodeLensRefreshPublisher(
            () =>
            {
                Interlocked.Increment(ref refreshCount);
                firstRefresh.TrySetResult();
                return Task.CompletedTask;
            });

        await InitializeAsync(server, codeLensRefreshSupport: true);
        await OpenDocumentAsync(server);

        await server.TextDocument_didChangeAsync(
            new VersionedTextDocumentIdentifier(DocumentUri, Version: 2),
            [new TextDocumentContentChangeEvent(Range: null, RangeLength: null, Text: "version 2")]);

        await server.TextDocument_didChangeAsync(
            new VersionedTextDocumentIdentifier(DocumentUri, Version: 3),
            [new TextDocumentContentChangeEvent(Range: null, RangeLength: null, Text: "version 3")]);

        await firstRefresh.Task.WaitAsync(System.TimeSpan.FromSeconds(2));
        await Task.Delay(400);

        refreshCount.Should().Be(1);
    }

    [Fact]
    public async Task CodeLens_refresh_is_not_sent_without_client_support()
    {
        var (server, _) = CreateServer();
        var refreshCount = 0;

        server.SetCodeLensRefreshPublisher(
            () =>
            {
                Interlocked.Increment(ref refreshCount);
                return Task.CompletedTask;
            });

        await InitializeAsync(server, codeLensRefreshSupport: false);
        await OpenDocumentAsync(server);
        await Task.Delay(400);

        refreshCount.Should().Be(0);
    }

    private static (ElmLanguageServer Server, CodeLensLanguageServiceSession Session) CreateServer()
    {
        var (workspace, _) = VirtualWorkspace.Create();
        var session = new CodeLensLanguageServiceSession();

        var server =
            new ElmLanguageServer(
                StubLanguageServiceSessionFactory.WithSession(session),
                workspace,
                EmptyElmPackageSource.Instance,
                new StubDiagnosticsProvider(),
                new StubDocumentFormatter(text => text),
                new LanguageServerOptions(ServerVersion: "test"));

        return (server, session);
    }

    private static async Task<InitializeResult> InitializeAsync(
        ElmLanguageServer server,
        bool codeLensRefreshSupport = false)
    {
        var (result, _) =
            server.Initialize(
                new InitializeParams(
                    ProcessId: null,
                    Capabilities:
                    new ClientCapabilities(
                        Workspace:
                        new ClientCapabilitiesWorkspace(
                            DidChangeWatchedFiles: null,
                            WorkspaceFolders: null,
                            CodeLens:
                            new CodeLensWorkspaceClientCapabilities(
                                RefreshSupport: codeLensRefreshSupport)),
                        TextDocument: null),
                    RootPath: null,
                    RootUri: VirtualWorkspace.RootUri,
                    WorkspaceFolders: [],
                    ClientInfo: null));

        await server.WorkspaceInitializationTask!;

        return result;
    }

    private static Task OpenDocumentAsync(ElmLanguageServer server) =>
        server.TextDocument_didOpenAsync(
            new TextDocumentItem(
                Uri: DocumentUri,
                LanguageId: "elm",
                Version: 1,
                Text: "module Main exposing (..)\n\nvalue =\n    1\n"));

    private sealed class CodeLensLanguageServiceSession : ILanguageServiceSession
    {
        private bool _blockNextAdd;

        public TaskCompletionSource AddStarted { get; } =
            new(TaskCreationOptions.RunContinuationsAsynchronously);

        public TaskCompletionSource AllowAddToComplete { get; } =
            new(TaskCreationOptions.RunContinuationsAsynchronously);

        public List<ProvideReferencesRequestStruct> ReferenceRequests { get; } = [];

        public Result<string, Response.WorkspaceSummaryResponse> AddFile(
            string fileUri,
            string fileContentAsText)
        {
            if (_blockNextAdd)
            {
                _blockNextAdd = false;
                AddStarted.TrySetResult();
                AllowAddToComplete.Task.GetAwaiter().GetResult();
            }

            return new Response.WorkspaceSummaryResponse();
        }

        public void BlockNextAdd() => _blockNextAdd = true;

        public Result<string, Response.WorkspaceSummaryResponse> DeleteFile(string fileUri) =>
            new Response.WorkspaceSummaryResponse();

        public Result<string, Response.WorkspaceSummaryResponse> AddElmPackage(
            ElmPackageVersion019Identifer packageVersionId,
            IReadOnlyList<KeyValuePair<IReadOnlyList<string>, string>> filesContentsAsText) =>
            new Response.WorkspaceSummaryResponse();

        public Result<string, Response> HandleRequest(Request request)
        {
            if (request is Request.TextDocumentSymbolRequest)
            {
                return
                    new Response.TextDocumentSymbolResponse(
                        [
                            new Interface.DocumentSymbol(
                                new DocumentSymbolStruct(
                                    Name: "value",
                                    Kind: Interface.SymbolKind.Function,
                                    Range: new MonacoRange(3, 1, 4, 6),
                                    SelectionRange: new MonacoRange(4, 1, 4, 6),
                                    Children:
                                    [
                                        new Interface.DocumentSymbol(
                                            new DocumentSymbolStruct(
                                                Name: "local",
                                                Kind: Interface.SymbolKind.Constant,
                                                Range: new MonacoRange(4, 5, 4, 10),
                                                SelectionRange: new MonacoRange(4, 5, 4, 10),
                                                Children: []))
                                    ]))
                        ]);
            }

            if (request is Request.TextDocumentReferencesRequest referencesRequest)
            {
                ReferenceRequests.Add(referencesRequest.Request);

                return
                    new Response.TextDocumentReferencesResponse(
                        [
                            new LocationInFile(
                                new FileLocation.WorkspaceFileLocation("Main.elm"),
                                new MonacoRange(6, 1, 6, 6)),
                            new LocationInFile(
                                new FileLocation.WorkspaceFileLocation("Main.elm"),
                                new MonacoRange(7, 1, 7, 6)),
                        ]);
            }

            return "Unexpected request: " + request.GetType().Name;
        }
    }
}
