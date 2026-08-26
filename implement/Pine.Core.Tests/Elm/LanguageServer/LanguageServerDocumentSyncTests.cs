using AwesomeAssertions;
using Pine.Core.Elm.LanguageServer;
using Pine.Core.Elm.LanguageServer.LanguageServiceInterface;
using Pine.Core.LanguageServerProtocol;
using System.Collections.Generic;
using System.Linq;
using System.Threading;
using System.Threading.Tasks;
using Xunit;

using ElmLanguageServer = Pine.Core.Elm.LanguageServer.LanguageServer;
using Interface = Pine.Core.Elm.LanguageServer.LanguageServiceInterface;

namespace Pine.Core.Tests.Elm.LanguageServer;

/// <summary>
/// Tests for the synchronization between the documents managed by the client, the files in the
/// workspace and the state of the language-service session.
/// <para>
/// The workspace is backed by an in-memory file store addressed by virtual document URIs, so
/// these tests neither touch the file system nor compile the Elm language service.
/// </para>
/// </summary>
public class LanguageServerDocumentSyncTests
{
    private const string OriginalContent =
        """
        module Main exposing (init, name)


        name = init


        init : Int
        init =
            0

        """;

    private static ElmLanguageServer CreateServer(
        ILanguageServerWorkspace workspace,
        ILanguageServiceSession session,
        IElmPackageSource? elmPackageSource = null,
        IDiagnosticsProvider? diagnosticsProvider = null,
        IDocumentFormatter? documentFormatter = null,
        IDiagnosticsProvider? formattingDiagnosticsProvider = null,
        System.Action<string>? logDelegate = null) =>
        new(
            StubLanguageServiceSessionFactory.WithSession(session),
            workspace,
            elmPackageSource ?? EmptyElmPackageSource.Instance,
            diagnosticsProvider ?? new StubDiagnosticsProvider(),
            documentFormatter ?? new StubDocumentFormatter(text => text),
            new LanguageServerOptions(ServerVersion: "test"),
            logDelegate,
            formattingDiagnosticsProvider);

    private static async Task InitializeAsync(
        ElmLanguageServer server,
        string? rootUri = VirtualWorkspace.RootUri,
        IReadOnlyList<WorkspaceFolder>? workspaceFolders = null)
    {
        server.Initialize(
            new InitializeParams(
                ProcessId: null,
                Capabilities: new ClientCapabilities(Workspace: null, TextDocument: null),
                RootPath: null,
                RootUri: rootUri,
                WorkspaceFolders: workspaceFolders ?? [],
                ClientInfo: null));

        await server.WorkspaceInitializationTask!;
    }

    [Fact]
    public async Task Initialization_submits_workspace_files_concurrently()
    {
        var (workspace, _) =
            VirtualWorkspace.Create(
                [
                (["First.elm"], OriginalContent),
                (["Second.elm"], OriginalContent)
                ]);

        var session = new ConcurrentInitializationSession();
        var server = CreateServer(workspace, session);
        var initialization = InitializeAsync(server);

        try
        {
            var completed =
                await Task.WhenAny(
                    session.TwoFilesStarted,
                    Task.Delay(System.TimeSpan.FromSeconds(5)));

            completed.Should().Be(session.TwoFilesStarted);
            session.MaximumActiveRequests.Should().BeGreaterThan(1);
        }
        finally
        {
            session.Release();
        }

        await initialization;
    }

    [Fact]
    public async Task DidChange_updates_the_document_seen_by_the_language_service()
    {
        var (workspace, store) =
            VirtualWorkspace.Create([(["Main.elm"], OriginalContent)]);

        var session = new RecordingLanguageServiceSession();
        var logs = new List<string>();

        var server = CreateServer(workspace, session, logDelegate: logs.Add);

        await InitializeAsync(server);

        logs.Clear();

        var documentUri = VirtualWorkspace.DocumentUri("Main.elm");

        session.TryGetFile(documentUri).Should().Be(OriginalContent);

        server.TextDocument_didOpen(
            new TextDocumentItem(
                Uri: documentUri,
                LanguageId: "elm",
                Version: 1,
                Text: OriginalContent));

        session.TryGetFile(documentUri).Should().Be(OriginalContent);

        var changedContent = "\n" + OriginalContent;

        server.TextDocument_didChange(
            new VersionedTextDocumentIdentifier(documentUri, Version: 2),
            [new TextDocumentContentChangeEvent(Range: null, RangeLength: null, Text: changedContent)]);

        session.TryGetFile(documentUri).Should().Be(changedContent);

        logs.Should().ContainSingle(
            log =>
            log.StartsWith(
                "Processed file " + documentUri + " with " +
                CommandLineInterface.FormatIntegerForDisplay(changedContent.Length) +
                " chars in language service in ",
                System.StringComparison.Ordinal) &&
            log.EndsWith(" ms", System.StringComparison.Ordinal));

        /*
         * The contents from the client take precedence over the contents on the backing store.
         * */
        server.Workspace_didChangeWatchedFiles(
            [new FileEvent(documentUri, FileChangeType.Changed)]);

        session.TryGetFile(documentUri).Should().Be(changedContent);

        /*
         * Changes with an outdated version are ignored.
         * */
        server.TextDocument_didChange(
            new VersionedTextDocumentIdentifier(documentUri, Version: 1),
            [new TextDocumentContentChangeEvent(Range: null, RangeLength: null, Text: OriginalContent)]);

        session.TryGetFile(documentUri).Should().Be(changedContent);

        /*
         * Closing the document falls back to the contents on the backing store.
         * */
        VirtualWorkspace.SetFile(store, ["Main.elm"], OriginalContent);

        server.TextDocument_didClose(new TextDocumentIdentifier(documentUri));

        session.TryGetFile(documentUri).Should().Be(OriginalContent);
    }

    private sealed class ConcurrentInitializationSession : ILanguageServiceSession
    {
        private readonly TaskCompletionSource _twoFilesStarted =
            new(TaskCreationOptions.RunContinuationsAsynchronously);

        private readonly TaskCompletionSource _release =
            new(TaskCreationOptions.RunContinuationsAsynchronously);

        private int _startedRequests;

        private int _activeRequests;

        private int _maximumActiveRequests;

        public Task TwoFilesStarted => _twoFilesStarted.Task;

        public int MaximumActiveRequests => Volatile.Read(ref _maximumActiveRequests);

        public void Release() => _release.TrySetResult();

        public Result<string, Response.WorkspaceSummaryResponse> AddFile(
            string fileUri,
            string fileContentAsText) =>
            new Response.WorkspaceSummaryResponse();

        public async Task<Result<string, Response.WorkspaceSummaryResponse>> AddFileAsync(
            string fileUri,
            string fileContentAsText,
            CancellationToken cancellationToken = default)
        {
            var activeRequests = Interlocked.Increment(ref _activeRequests);

            UpdateMaximum(ref _maximumActiveRequests, activeRequests);

            if (Interlocked.Increment(ref _startedRequests) >= 2)
            {
                _twoFilesStarted.TrySetResult();
            }

            try
            {
                await _release.Task.WaitAsync(cancellationToken);
            }
            finally
            {
                Interlocked.Decrement(ref _activeRequests);
            }

            return new Response.WorkspaceSummaryResponse();
        }

        public Result<string, Response.WorkspaceSummaryResponse> DeleteFile(string fileUri) =>
            new Response.WorkspaceSummaryResponse();

        public Result<string, Response.WorkspaceSummaryResponse> AddElmPackage(
            ElmPackageVersion019Identifer packageVersionId,
            IReadOnlyList<KeyValuePair<IReadOnlyList<string>, string>> filesContentsAsText) =>
            new Response.WorkspaceSummaryResponse();

        public Result<string, Response> HandleRequest(Request request) =>
            "Not implemented in this test double";

        private static void UpdateMaximum(ref int maximum, int candidate)
        {
            var current = Volatile.Read(ref maximum);

            while (candidate > current)
            {
                var observed = Interlocked.CompareExchange(ref maximum, candidate, current);

                if (observed == current)
                {
                    return;
                }

                current = observed;
            }
        }
    }

    [Fact]
    public async Task DidClose_deletes_the_document_when_the_backing_file_disappeared()
    {
        var (workspace, store) =
            VirtualWorkspace.Create([(["Main.elm"], OriginalContent)]);

        var session = new RecordingLanguageServiceSession();

        var server = CreateServer(workspace, session);

        await InitializeAsync(server);

        var documentUri = VirtualWorkspace.DocumentUri("Main.elm");

        server.TextDocument_didOpen(
            new TextDocumentItem(documentUri, "elm", Version: 1, Text: "module Main exposing (..)\n"));

        VirtualWorkspace.DeleteFile(store, ["Main.elm"]);

        server.TextDocument_didClose(new TextDocumentIdentifier(documentUri));

        session.TryGetFile(documentUri).Should().BeNull();

        session.Mutations.Should().Contain("delete:" + documentUri);
    }

    [Fact]
    public async Task Watched_file_events_never_override_an_open_document()
    {
        var (workspace, store) =
            VirtualWorkspace.Create([(["Main.elm"], OriginalContent)]);

        var session = new RecordingLanguageServiceSession();

        var server = CreateServer(workspace, session);

        await InitializeAsync(server);

        var documentUri = VirtualWorkspace.DocumentUri("Main.elm");

        const string UnsavedContent = "module Main exposing (..)\n\n\nunsaved = 1\n";

        server.TextDocument_didOpen(
            new TextDocumentItem(documentUri, "elm", Version: 1, Text: UnsavedContent));

        VirtualWorkspace.SetFile(store, ["Main.elm"], "module Main exposing (..)\n\n\nfromDisk = 1\n");

        server.Workspace_didChangeWatchedFiles(
            [new FileEvent(documentUri, FileChangeType.Changed)]);

        session.TryGetFile(documentUri).Should().Be(UnsavedContent);

        /*
         * Even a delete event does not remove the contents of a document open in the client.
         * */
        VirtualWorkspace.DeleteFile(store, ["Main.elm"]);

        server.Workspace_didChangeWatchedFiles(
            [new FileEvent(documentUri, FileChangeType.Deleted)]);

        session.TryGetFile(documentUri).Should().Be(UnsavedContent);

        session.Mutations.Should().NotContain("delete:" + documentUri);
    }

    [Fact]
    public async Task Watched_file_events_apply_creations_and_deletions_for_documents_not_open()
    {
        var (workspace, store) = VirtualWorkspace.Create();

        var session = new RecordingLanguageServiceSession();

        var server = CreateServer(workspace, session);

        await InitializeAsync(server);

        var documentUri = VirtualWorkspace.DocumentUri("src", "Added.elm");

        VirtualWorkspace.SetFile(store, ["src", "Added.elm"], "module Added exposing (..)\n");

        server.Workspace_didChangeWatchedFiles(
            [new FileEvent(documentUri, FileChangeType.Created)]);

        session.TryGetFile(documentUri).Should().Be("module Added exposing (..)\n");

        VirtualWorkspace.DeleteFile(store, ["src", "Added.elm"]);

        server.Workspace_didChangeWatchedFiles(
            [new FileEvent(documentUri, FileChangeType.Deleted)]);

        session.TryGetFile(documentUri).Should().BeNull();
    }

    [Fact]
    public async Task DidOpen_ignores_stale_versions_and_didChange_ignores_equal_versions()
    {
        var (workspace, _) = VirtualWorkspace.Create();

        var session = new RecordingLanguageServiceSession();

        var server = CreateServer(workspace, session);

        await InitializeAsync(server);

        var documentUri = VirtualWorkspace.DocumentUri("Main.elm");

        server.TextDocument_didOpen(
            new TextDocumentItem(documentUri, "elm", Version: 5, Text: "version 5"));

        session.TryGetFile(documentUri).Should().Be("version 5");

        server.TextDocument_didOpen(
            new TextDocumentItem(documentUri, "elm", Version: 3, Text: "version 3"));

        session.TryGetFile(documentUri).Should().Be("version 5");

        server.TextDocument_didChange(
            new VersionedTextDocumentIdentifier(documentUri, Version: 5),
            [new TextDocumentContentChangeEvent(Range: null, RangeLength: null, Text: "again version 5")]);

        session.TryGetFile(documentUri).Should().Be("version 5");

        server.TextDocument_didChange(
            new VersionedTextDocumentIdentifier(documentUri, Version: 6),
            [new TextDocumentContentChangeEvent(Range: null, RangeLength: null, Text: "version 6")]);

        session.TryGetFile(documentUri).Should().Be("version 6");

        /*
         * Changes after closing the document are ignored: they would resurrect an overlay which
         * no longer has an owner in the client.
         * */
        server.TextDocument_didClose(new TextDocumentIdentifier(documentUri));

        server.TextDocument_didChange(
            new VersionedTextDocumentIdentifier(documentUri, Version: 7),
            [new TextDocumentContentChangeEvent(Range: null, RangeLength: null, Text: "version 7")]);

        session.TryGetFile(documentUri).Should().BeNull();
    }

    [Fact]
    public void ComposeWorkspaceRootUris_combines_rootUri_rootPath_and_folders_without_duplicates()
    {
        var initializeParams =
            new InitializeParams(
                ProcessId: null,
                Capabilities: new ClientCapabilities(Workspace: null, TextDocument: null),
                RootPath: "file:///deprecated/root",
                RootUri: "memory://workspace",
                WorkspaceFolders:
                [
                    new WorkspaceFolder("memory://workspace/", "duplicate of rootUri"),
                    new WorkspaceFolder("memory://other/", "other"),
                    new WorkspaceFolder("not an absolute uri", "ignored"),
                ],
                ClientInfo: null);

        ElmLanguageServer.ComposeWorkspaceRootUris(initializeParams)
            .Should().Equal(
            "memory://workspace/",
            "file:///deprecated/root/",
            "memory://other/");
    }

    [Fact]
    public async Task Initialize_loads_files_from_all_roots_starting_with_elm_json()
    {
        var storeWorkspace = new IO.FileStoreFromConcurrentDictionary();
        var storeOther = new IO.FileStoreFromConcurrentDictionary();

        VirtualWorkspace.SetFile(storeWorkspace, ["elm.json"], """{"type":"application"}""");
        VirtualWorkspace.SetFile(storeWorkspace, ["src", "Main.elm"], "module Main exposing (..)\n");
        VirtualWorkspace.SetFile(storeWorkspace, ["readme.md"], "not relevant");

        VirtualWorkspace.SetFile(storeOther, ["src", "Other.elm"], "module Other exposing (..)\n");

        var workspace =
            new WorkspaceFromFileStoreMounts(
                [
                    new FileStoreMount(new System.Uri("memory://workspace/"), storeWorkspace),
                    new FileStoreMount(new System.Uri("memory://other/"), storeOther),
                ]);

        var session = new RecordingLanguageServiceSession();

        var server = CreateServer(workspace, session);

        await InitializeAsync(
            server,
            rootUri: "memory://workspace/",
            workspaceFolders: [new WorkspaceFolder("memory://other/", "other")]);

        session.Mutations.Should().Equal(
            "add:memory://workspace/elm.json",
            "add:memory://workspace/src/Main.elm",
            "add:memory://other/src/Other.elm");
    }

    [Fact]
    public async Task Initialize_loads_the_exposed_modules_of_direct_dependencies()
    {
        var storeWorkspace = new IO.FileStoreFromConcurrentDictionary();
        var storePackages = new IO.FileStoreFromConcurrentDictionary();

        VirtualWorkspace.SetFile(
            storeWorkspace,
            ["elm.json"],
            """
            {
                "type": "application",
                "dependencies": {
                    "direct": { "author/pkg": "1.2.3" },
                    "indirect": { "author/other": "3.2.1" }
                }
            }
            """);

        VirtualWorkspace.SetFile(
            storePackages,
            ["author", "pkg", "1.2.3", "elm.json"],
            """
            {
                "type": "package",
                "name": "author/pkg",
                "version": "1.2.3",
                "exposed-modules": [ "Exposed" ]
            }
            """);

        VirtualWorkspace.SetFile(
            storePackages,
            ["author", "pkg", "1.2.3", "src", "Exposed.elm"],
            "module Exposed exposing (..)\n");

        VirtualWorkspace.SetFile(
            storePackages,
            ["author", "pkg", "1.2.3", "src", "Internal.elm"],
            "module Internal exposing (..)\n");

        var workspace =
            new WorkspaceFromFileStoreMounts(
                [
                    new FileStoreMount(new System.Uri("memory://workspace/"), storeWorkspace),
                    new FileStoreMount(new System.Uri("memory://packages/"), storePackages),
                ]);

        var packageSource =
            new ElmPackageSourceFromWorkspace(
                workspace,
                ["memory://packages/"]);

        var session = new RecordingLanguageServiceSession();

        var server = CreateServer(workspace, session, elmPackageSource: packageSource);

        await InitializeAsync(server, rootUri: "memory://workspace/");

        var package = session.Packages.Should().ContainSingle().Which;

        package.PackageVersionId.Should().Be(
            new Interface.ElmPackageVersion019Identifer("author/pkg", "1.2.3"));

        package.Modules.Select(module => string.Join("/", module.Key))
            .Should().Equal("src/Exposed.elm");

        var exposedModuleUri = "memory://packages/author/pkg/1.2.3/src/Exposed.elm";

        var fileLocation = server.InterfaceFileLocationFromUri(exposedModuleUri);

        var packageFileLocation =
            fileLocation.Should().BeOfType<FileLocation.ElmPackageFileLocation>().Which;

        packageFileLocation.ElmPackageVersionIdentifer.Should().Be(
            new Interface.ElmPackageVersion019Identifer("author/pkg", "1.2.3"));

        packageFileLocation.ModulePath.Should().Equal("src", "Exposed.elm");

        server.FindMatchingUri(packageFileLocation).Should().Be(exposedModuleUri);

        /*
         * A directory whose name only shares a prefix with the package directory is not part of
         * the package.
         * */
        server.InterfaceFileLocationFromUri("memory://packages/author/pkg/1.2.30/src/Exposed.elm")
            .Should().BeOfType<FileLocation.WorkspaceFileLocation>();

        server.InterfaceFileLocationFromUri("memory://workspace/src/Main.elm")
            .Should().BeOfType<FileLocation.WorkspaceFileLocation>()
            .Which.FilePath.Should().Be("memory://workspace/src/Main.elm");
    }
}
