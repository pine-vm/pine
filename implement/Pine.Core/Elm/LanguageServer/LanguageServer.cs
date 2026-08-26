using Pine.Core.Elm.Elm019;
using Pine.Core.Elm.ElmSyntax;
using Pine.Core.Elm.LanguageServer.LanguageServiceInterface;
using Pine.Core.Elm.LanguageServer.MonacoEditor;
using Pine.Core.Files;
using Pine.Core.LanguageServerProtocol;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Threading;
using System.Threading.Tasks;

using Interface = Pine.Core.Elm.LanguageServer.LanguageServiceInterface;
using Protocol = Pine.Core.LanguageServerProtocol;

namespace Pine.Core.Elm.LanguageServer;

/// <summary>
/// Implementation-neutral part of the Elm language server: maintains the documents seen from the
/// client, forwards requests to a language-service session and publishes diagnostics.
/// <para>
/// All access to sources outside the client-managed documents goes through
/// <see cref="ILanguageServerWorkspace"/> and <see cref="IElmPackageSource"/>; diagnostics and
/// formatting go through <see cref="IDiagnosticsProvider"/> and <see cref="IDocumentFormatter"/>.
/// </para>
/// </summary>
/// <param name="sessionFactory">Creates the language-service session backing this server.</param>
/// <param name="workspace">Read access to the sources referenced by document URIs.</param>
/// <param name="elmPackageSource">Source for the Elm packages referenced from <c>elm.json</c> files.</param>
/// <param name="diagnosticsProvider">Computes diagnostics after a document was saved.</param>
/// <param name="documentFormatter">Formats documents.</param>
/// <param name="options">Host-independent settings.</param>
/// <param name="logDelegate">Optional delegate receiving log messages.</param>
/// <param name="formattingDiagnosticsProvider">
/// Optional provider used after formatting a document. Production composition uses the syntax-only
/// provider here to avoid running a compiler on every formatting request.
/// </param>
public class LanguageServer(
    ILanguageServiceSessionFactory sessionFactory,
    ILanguageServerWorkspace workspace,
    IElmPackageSource elmPackageSource,
    IDiagnosticsProvider diagnosticsProvider,
    IDocumentFormatter documentFormatter,
    LanguageServerOptions options,
    System.Action<string>? logDelegate = null,
    IDiagnosticsProvider? formattingDiagnosticsProvider = null)
    : IDocumentTextSource
{
    private readonly ConcurrentDictionary<string, string> _allSeenDocumentUris = new();

    private readonly ConcurrentDictionary<string, string> _clientTextDocumentContents = new();

    private readonly ConcurrentDictionary<string, int> _clientTextDocumentVersions = new();

    private readonly ConcurrentDictionary<string, byte> _closedTextDocuments = new();

    private readonly Lock _documentStateLock = new();

    private IReadOnlyList<WorkspaceFolder> _workspaceFolders = [];

    private InitializeParams? _initializeParams;

    private readonly System.Action<string>? _logDelegate = logDelegate;

    private Task<Result<string, ILanguageServiceSession>>? _languageServiceStateTask;

    /*
     * TODO: Use the version identifier from elm.json as scope.
     * */
    private readonly ConcurrentDictionary<ElmPackageVersion019Identifer, string> _elmJsonDirectDependencies =
        new();

    private readonly ConcurrentDictionary<ElmPackageVersion019Identifer, string> _elmJsonDirectDependenciesLoaded =
        new();

    private readonly Lock _diagnosticsLock = new();

    /// <summary>
    /// Last diagnostics reported by a provider, keyed by the entry-point document URI which
    /// caused them. Publication aggregates the contributions of all entry points.
    /// </summary>
    private readonly Dictionary<string, IReadOnlyList<DocumentDiagnostics>> _diagnosticsByEntryPoint =
        new(System.StringComparer.Ordinal);

    /// <summary>
    /// Per-entry-point counter to discard results of superseded diagnostics runs.
    /// </summary>
    private readonly Dictionary<string, long> _diagnosticsGenerations =
        new(System.StringComparer.Ordinal);

    /// <summary>
    /// Incremented whenever a document content changes, to discard diagnostics computed from
    /// sources which have been replaced in the meantime.
    /// </summary>
    private long _sourceRevision;

    private System.Action<PublishDiagnosticsParams>? _publishDiagnostics;

    private void Log(string message)
    {
        _logDelegate?.Invoke(message);
    }

    /// <summary>
    /// Task tracking the workspace initialization started by <see cref="Initialize"/>, or
    /// <see langword="null"/> before initialization started. Requests from the client wait for
    /// this task to complete before using the language-service session.
    /// </summary>
    public Task? WorkspaceInitializationTask => _languageServiceStateTask;

    /// <summary>
    /// Sets the channel used to publish diagnostics to the client.
    /// </summary>
    public void SetDiagnosticsPublisher(System.Action<PublishDiagnosticsParams>? publishDiagnostics)
    {
        _publishDiagnostics = publishDiagnostics;
    }

    /// <summary>
    /// Returns the text currently known for a document: the client-managed content when the
    /// document is open, otherwise the content from the workspace.
    /// </summary>
    public string? TryGetDocumentText(string documentUri)
    {
        var documentUriCleaned = DocumentUriCleaned(documentUri);

        if (_clientTextDocumentContents.TryGetValue(documentUriCleaned, out var openContent))
        {
            return openContent;
        }

        var readResult = workspace.ReadFile(documentUriCleaned);

        if (readResult.IsErrOrNull() is { } err)
        {
            Log("Failed reading " + documentUriCleaned + " from workspace: " + err.Kind + ": " + err.Message);
            return null;
        }

        return OkFileOrNull(readResult)?.Text;
    }

    /// <summary>
    /// Initializes the language server.
    /// </summary>
    public (InitializeResult, IReadOnlyList<KeyValuePair<string, object>>) Initialize(
        InitializeParams initializeParams)
    {
        Log("Initialize: " + System.Text.Json.JsonSerializer.Serialize(initializeParams));

        this._initializeParams = initializeParams;

        _workspaceFolders = initializeParams.WorkspaceFolders ?? [];

        var requests = new List<KeyValuePair<string, object>>();

        if (initializeParams.Capabilities.Workspace?.DidChangeWatchedFiles?.DynamicRegistration ?? false)
        {
            requests.AddRange(RegisterFileWatchers());
        }

        var response =
            new InitializeResult(
                Capabilities: new ServerCapabilities
                {
                    TextDocumentSync =
                    new TextDocumentSyncOptions(
                        Change: TextDocumentSyncKind.Full,
                        WillSave: null,
                        WillSaveWaitUntil: null,
                        Save: new SaveOptions(IncludeText: true))
                    {
                        OpenClose = true,
                    },

                    DocumentFormattingProvider = true,
                    HoverProvider = true,

                    CompletionProvider =
                    new CompletionOptions(
                        TriggerCharacters: [".", " "],
                        AllCommitCharacters: null,
                        ResolveProvider: null),

                    DefinitionProvider = true,

                    DocumentSymbolProvider = true,

                    ReferencesProvider = true,

                    RenameProvider = true,

                    Workspace =
                    new ServerCapabilitiesWorkspace
                    {
                        WorkspaceFolders =
                        new WorkspaceFoldersServerCapabilities(Supported: true, ChangeNotifications: true),
                    }
                },
                ServerInfo: new ParticipentInfo(
                    Name: "Pine language server",
                    Version: options.ServerVersion));

        _languageServiceStateTask =
            Task.Run(() => InitializeWorkspaceState(initializeParams));

        return (response, requests);
    }

    private IReadOnlyList<KeyValuePair<string, object>> RegisterFileWatchers()
    {
        Log("Registering file watchers...");

        var registrationParams =
            new RegistrationParams(
                Registrations:
                [
                    new Registration(
                        Id : "workspace/didChangeWatchedFiles",
                        Method : "workspace/didChangeWatchedFiles",
                        RegisterOptions : new DidChangeWatchedFilesRegistrationOptions(
                            Watchers:
                            [
                                new FileSystemWatcher(
                                    GlobPattern : "**/*.elm",
                                    Kind : WatchKind.Create | WatchKind.Change | WatchKind.Delete)
                            ]))
                ]);

        return
            [
            new KeyValuePair<string, object>("client/registerCapability", registrationParams)
            ];
    }

    /// <summary>
    /// Composes the workspace root URIs from the initialize params, in order of precedence and
    /// without duplicates: <c>rootUri</c>, the deprecated <c>rootPath</c> and the workspace folders.
    /// </summary>
    public static IReadOnlyList<string> ComposeWorkspaceRootUris(
        InitializeParams initializeParams,
        System.Action<string>? logDelegate = null)
    {
        var roots = new List<string>();
        var seen = new HashSet<string>(System.StringComparer.Ordinal);

        void AddRootUri(string? rootUri, string origin)
        {
            if (rootUri is null)
            {
                return;
            }

            var cleaned = DocumentUriCleaned(rootUri);

            if (!System.Uri.TryCreate(cleaned, System.UriKind.Absolute, out var parsed))
            {
                logDelegate?.Invoke("Ignoring " + origin + " which is not an absolute URI: " + rootUri);
                return;
            }

            var normalized =
                parsed.AbsoluteUri.EndsWith('/')
                ?
                parsed.AbsoluteUri
                :
                parsed.AbsoluteUri + "/";

            if (seen.Add(normalized))
            {
                roots.Add(normalized);
            }
        }

        AddRootUri(initializeParams.RootUri, "rootUri");

        if (initializeParams.RootPath is { } rootPath)
        {
            if (System.Uri.TryCreate(rootPath, System.UriKind.Absolute, out var rootPathUri) &&
                rootPathUri.Scheme is "file")
            {
                AddRootUri(rootPathUri.AbsoluteUri, "rootPath");
            }
            else
            {
                logDelegate?.Invoke("Ignoring rootPath which is not an absolute local path: " + rootPath);
            }
        }

        if (initializeParams.WorkspaceFolders is { } workspaceFolders)
        {
            foreach (var workspaceFolder in workspaceFolders)
            {
                AddRootUri(workspaceFolder.Uri, "workspace folder");
            }
        }

        return roots;
    }

    /// <summary>
    /// File names loaded into the language service when enumerating a workspace root.
    /// </summary>
    public static bool IsRelevantWorkspaceFileName(string fileName) =>
        fileName.EndsWith(".elm", System.StringComparison.OrdinalIgnoreCase) ||
        string.Equals(fileName, "elm.json", System.StringComparison.OrdinalIgnoreCase);

    private async Task<Result<string, ILanguageServiceSession>> InitializeWorkspaceState(
        InitializeParams initializeParams)
    {
        var initResult =
            await sessionFactory.CreateSessionAsync(options, CancellationToken.None);

        if (initResult.IsErrOrNull() is { } err)
        {
            Log("Failed initializing language service state: " + err);
            return err;
        }

        if (initResult.IsOkOrNull() is not { } languageServiceState)
        {
            throw new System.NotImplementedException(
                "Unexpected language service state result type: " + initResult.GetType());
        }

        _elmJsonDirectDependencies.Clear();
        _elmJsonDirectDependenciesLoaded.Clear();

        var rootUris = ComposeWorkspaceRootUris(initializeParams, Log);

        Log("Starting to initialize files contents for " + rootUris.Count + " roots");

        var aggregateClock = System.Diagnostics.Stopwatch.StartNew();

        var aggregateElmModuleFiles = new HashSet<string>(System.StringComparer.Ordinal);
        var aggregateElmJsonFiles = new HashSet<string>(System.StringComparer.Ordinal);

        var pendingFiles =
            new List<(
                WorkspaceFile File,
                Task<int> ElapsedMilliseconds)>();

        async Task<int> AddFileAndMeasure(WorkspaceFile file)
        {
            var fileClock = System.Diagnostics.Stopwatch.StartNew();

            _ =
                await languageServiceState.AddFileAsync(
                    file.DocumentUri,
                    file.Text,
                    CancellationToken.None);

            return (int)fileClock.Elapsed.TotalMilliseconds;
        }

        foreach (var rootUri in rootUris)
        {
            var enumerateResult =
                workspace.EnumerateFiles(rootUri, IsRelevantWorkspaceFileName);

            if (enumerateResult.IsErrOrNull() is { } enumerateError)
            {
                Log(
                    "Failed enumerating files in " + rootUri + ": " +
                    enumerateError.Kind + ": " + enumerateError.Message);

                continue;
            }

            if (enumerateResult.IsOkOrNull() is not { } files)
            {
                throw new System.NotImplementedException(
                    "Unexpected enumeration result type: " + enumerateResult.GetType());
            }

            var elmJsonFiles =
                files
                .Where(file => IsElmJsonDocumentUri(file.DocumentUri))
                .ToList();

            var elmModuleFiles =
                files
                .Where(file => IsElmModuleDocumentUri(file.DocumentUri))
                .ToList();

            Log(
                "Found " + elmModuleFiles.Count + " Elm module files and " +
                elmJsonFiles.Count + " elm.json files in " + rootUri);

            foreach (var file in elmJsonFiles.Concat(elmModuleFiles))
            {
                pendingFiles.Add(
                    (file,
                    AddFileAndMeasure(file)));

                if (IsElmModuleDocumentUri(file.DocumentUri))
                {
                    aggregateElmModuleFiles.Add(file.DocumentUri);
                }

                if (IsElmJsonDocumentUri(file.DocumentUri))
                {
                    aggregateElmJsonFiles.Add(file.DocumentUri);

                    CollectDirectDependenciesFromElmJsonFile(file.Text);
                }
            }
        }

        await Task.WhenAll(pendingFiles.Select(pending => pending.ElapsedMilliseconds));

        foreach (var pending in pendingFiles)
        {
            Log(
                "Processed file " + pending.File.DocumentUri + " with " +
                CommandLineInterface.FormatIntegerForDisplay(pending.File.Text.Length) +
                " chars in language service in " +
                CommandLineInterface.FormatIntegerForDisplay(pending.ElapsedMilliseconds.Result) +
                " ms");
        }

        Log(
            "Finished initializing contents for " + aggregateElmModuleFiles.Count +
            " Elm modules and " +
            aggregateElmJsonFiles.Count +
            " elm.json files in " +
            CommandLineInterface.FormatIntegerForDisplay((int)aggregateClock.Elapsed.TotalMilliseconds) + " ms");

        LoadDirectDependenciesFromElmJsonFiles(languageServiceState);

        /*
         * Do not take the document-state lock here: notification handlers hold that lock while
         * waiting for this task to complete, so taking it here would deadlock. Enumerating the
         * concurrent dictionary is safe without the lock.
         * */
        foreach (var (documentUri, content) in _clientTextDocumentContents.ToArray())
        {
            languageServiceState.AddFile(documentUri, content);
        }

        return Result<string, ILanguageServiceSession>.ok(languageServiceState);
    }

    /// <summary>
    /// Applies a workspace folder change notification.
    /// </summary>
    public void Workspace_didChangeWorkspaceFolders(WorkspaceFoldersChangeEvent workspaceFoldersChangeEvent)
    {
        Log(
            "Workspace_didChangeWorkspaceFolders (added " +
            workspaceFoldersChangeEvent.Added.Count +
            " and removed " +
            workspaceFoldersChangeEvent.Removed.Count +
            ")");

        IReadOnlyList<WorkspaceFolder> newWorkspaceFolders =
            [
            .._workspaceFolders
            .Where(
                prevFolder =>
                !workspaceFoldersChangeEvent.Removed.Any(removedFolder => removedFolder.Uri == prevFolder.Uri)),
            ..workspaceFoldersChangeEvent.Added
            ];

        Log(
            "Workspace_didChangeWorkspaceFolders: new workspace folders count: " +
            newWorkspaceFolders.Count + " (" +
            string.Join(", ", newWorkspaceFolders.Select(wf => wf.Uri)));

        _workspaceFolders = newWorkspaceFolders;

        if (_initializeParams is not { } previousInitializeParams)
        {
            Log("Cannot reinitialize workspace state before initialization");
            return;
        }

        var currentInitializeParams =
            previousInitializeParams with
            {
                RootPath = null,
                RootUri = null,
                WorkspaceFolders = newWorkspaceFolders,
            };

        _initializeParams = currentInitializeParams;

        var previousLanguageServiceStateTask = _languageServiceStateTask;

        _languageServiceStateTask =
            Task.Run(
                async () =>
                {
                    if (previousLanguageServiceStateTask is not null)
                    {
                        _ = await previousLanguageServiceStateTask;
                    }

                    return await InitializeWorkspaceState(currentInitializeParams);
                });
    }

    /// <summary>
    /// Applies a text document open notification.
    /// </summary>
    public void TextDocument_didOpen(TextDocumentItem textDocument)
    {
        lock (_documentStateLock)
        {
            TextDocument_didOpenSynchronized(textDocument);
        }
    }

    private void TextDocument_didOpenSynchronized(TextDocumentItem textDocument)
    {
        var decodedUri = DocumentUriCleaned(textDocument.Uri);

        Log("TextDocument_didOpen: " + decodedUri);

        _allSeenDocumentUris[decodedUri] = decodedUri;
        _closedTextDocuments.TryRemove(decodedUri, out var _);

        lock (_documentStateLock)
        {
            if (_clientTextDocumentVersions.TryGetValue(decodedUri, out var currentVersion) &&
                currentVersion > textDocument.Version)
            {
                Log(
                    "Ignoring stale open document version " + textDocument.Version +
                    " because current version is " + currentVersion);

                return;
            }

            _clientTextDocumentContents[decodedUri] = textDocument.Text;
            _clientTextDocumentVersions[decodedUri] = textDocument.Version;
        }

        BumpSourceRevision();

        if (GetLanguageServiceState("opening document") is not { } languageServiceState)
        {
            return;
        }

        lock (_documentStateLock)
        {
            if (_clientTextDocumentVersions.TryGetValue(decodedUri, out var currentVersion) &&
                currentVersion == textDocument.Version &&
                _clientTextDocumentContents.TryGetValue(decodedUri, out var currentContent) &&
                currentContent == textDocument.Text)
            {
                languageServiceState.AddFile(decodedUri, textDocument.Text);
            }
        }
    }

    /// <summary>
    /// Applies a text document change notification.
    /// </summary>
    public void TextDocument_didChange(
        VersionedTextDocumentIdentifier textDocument,
        IReadOnlyList<TextDocumentContentChangeEvent> contentChanges)
    {
        lock (_documentStateLock)
        {
            TextDocument_didChangeSynchronized(textDocument, contentChanges);
        }
    }

    private void TextDocument_didChangeSynchronized(
        VersionedTextDocumentIdentifier textDocument,
        IReadOnlyList<TextDocumentContentChangeEvent> contentChanges)
    {
        var textDocumentUri = DocumentUriCleaned(textDocument.Uri);

        _allSeenDocumentUris[textDocumentUri] = textDocumentUri;

        Log(
            "TextDocument_didChange: " + textDocumentUri +
            " (" + contentChanges.Count + " content changes)");

        if (contentChanges.Count is 0)
        {
            return;
        }

        if (_closedTextDocuments.ContainsKey(textDocumentUri))
        {
            Log("Ignoring change for closed document " + textDocumentUri);
            return;
        }

        for (var i = 0; i < contentChanges.Count; ++i)
        {
            Log("Content change " + i + ".range: " + contentChanges[i].Range?.ToString() ?? "null");

            if (contentChanges[i].Range is not null)
            {
                Log("Failed to apply changes: not implemented");
                return;
            }
        }

        var newContent = contentChanges[^1].Text;

        lock (_documentStateLock)
        {
            if (_clientTextDocumentVersions.TryGetValue(textDocumentUri, out var currentVersion) &&
                textDocument.Version <= currentVersion)
            {
                Log(
                    "Ignoring stale document version " + textDocument.Version +
                    " because current version is " + currentVersion);

                return;
            }

            _clientTextDocumentContents[textDocumentUri] = newContent;
            _clientTextDocumentVersions[textDocumentUri] = textDocument.Version;
        }

        BumpSourceRevision();

        var linesCount = ElmModule.ModuleLines(newContent).Count();

        Log(
            "Replaced all of " + textDocumentUri + " with " +
            CommandLineInterface.FormatIntegerForDisplay(newContent.Length) +
            " chars distributed over " +
            CommandLineInterface.FormatIntegerForDisplay(linesCount) + " lines");

        if (GetLanguageServiceState("changing document") is not { } languageServiceState)
        {
            return;
        }

        lock (_documentStateLock)
        {
            if (_clientTextDocumentVersions.TryGetValue(textDocumentUri, out var currentVersion) &&
                currentVersion == textDocument.Version)
            {
                var clock = System.Diagnostics.Stopwatch.StartNew();

                languageServiceState.AddFile(textDocumentUri, newContent);

                Log(
                    "Processed file " + textDocumentUri + " with " +
                    CommandLineInterface.FormatIntegerForDisplay(newContent.Length) +
                    " chars in language service in " +
                    CommandLineInterface.FormatIntegerForDisplay((int)clock.Elapsed.TotalMilliseconds) +
                    " ms");
            }
        }
    }

    /// <summary>
    /// Applies a text document close notification.
    /// </summary>
    public void TextDocument_didClose(TextDocumentIdentifier textDocument)
    {
        lock (_documentStateLock)
        {
            TextDocument_didCloseSynchronized(textDocument);
        }
    }

    private void TextDocument_didCloseSynchronized(TextDocumentIdentifier textDocument)
    {
        var decodedUri = DocumentUriCleaned(textDocument.Uri);

        lock (_documentStateLock)
        {
            _clientTextDocumentContents.TryRemove(decodedUri, out var _);
            _clientTextDocumentVersions.TryRemove(decodedUri, out var _);
            _closedTextDocuments[decodedUri] = 0;
        }

        BumpSourceRevision();

        Log(
            "TextDocument_didClose: " + decodedUri +
            " (" + _clientTextDocumentContents.Count + " open remaining)");

        if (GetLanguageServiceState("closing document") is not { } languageServiceState)
        {
            return;
        }

        /*
         * Read the backing content outside the document-state lock: reading can be slow and the
         * lock only protects the client-managed overlay.
         * */
        var readResult = workspace.ReadFile(decodedUri);

        string? backingContent = null;

        if (readResult.IsErrOrNull() is { } readError)
        {
            Log(
                "Failed reading " + decodedUri + " while closing document: " +
                readError.Kind + ": " + readError.Message);
        }
        else
        {
            backingContent = OkFileOrNull(readResult)?.Text;
        }

        lock (_documentStateLock)
        {
            if (_clientTextDocumentContents.ContainsKey(decodedUri))
            {
                return;
            }

            if (backingContent is not null)
            {
                languageServiceState.AddFile(decodedUri, backingContent);
            }
            else
            {
                languageServiceState.DeleteFile(decodedUri);
            }
        }

        if (backingContent is null)
        {
            RemoveDiagnosticsEntryPoint(decodedUri);
        }
    }

    /// <summary>
    /// Applies watched workspace file changes.
    /// </summary>
    public void Workspace_didChangeWatchedFiles(IReadOnlyList<FileEvent> changesBeforeDecode)
    {
        var changes =
            changesBeforeDecode
            .Select(
                change =>
                change with
                {
                    Uri = DocumentUriCleaned(change.Uri)
                })
            .ToList();

        Log(
            "Workspace_didChangeWatchedFiles: " + changes.Count + " changes: " +
            string.Join(", ", changes.Select(change => change.Uri)));

        ProcessFileChanges(changes);
    }

    private void ProcessFileChanges(IReadOnlyList<FileEvent> changes)
    {
        if (GetLanguageServiceState("processing file changes") is not { } languageServiceState)
        {
            return;
        }

        var anyChangeApplied = false;

        foreach (var change in changes)
        {
            /*
             * Contents managed by the client take precedence over the contents seen on the
             * backing store: the client may have unsaved changes for that document.
             * */
            lock (_documentStateLock)
            {
                if (_clientTextDocumentContents.TryGetValue(change.Uri, out var openDocumentContent))
                {
                    languageServiceState.AddFile(change.Uri, openDocumentContent);
                    continue;
                }
            }

            if (change.Type is FileChangeType.Deleted)
            {
                lock (_documentStateLock)
                {
                    if (_clientTextDocumentContents.TryGetValue(change.Uri, out var openDocumentContent))
                    {
                        languageServiceState.AddFile(change.Uri, openDocumentContent);
                        continue;
                    }

                    languageServiceState.DeleteFile(change.Uri);
                }

                anyChangeApplied = true;

                RemoveDiagnosticsEntryPoint(change.Uri);

                continue;
            }

            if (change.Type is not FileChangeType.Created and not FileChangeType.Changed)
            {
                Log("Ignoring file change: " + change.Type + ": " + change.Uri);
                continue;
            }

            var clock = System.Diagnostics.Stopwatch.StartNew();

            var readResult = workspace.ReadFile(change.Uri);

            if (readResult.IsErrOrNull() is { } readError)
            {
                Log(
                    "Failed reading " + change.Uri + ": " +
                    readError.Kind + ": " + readError.Message);

                continue;
            }

            if (OkFileOrNull(readResult) is not { } file)
            {
                Log("File reported as " + change.Type + " does not exist: " + change.Uri);
                continue;
            }

            Log(
                "Read file " + change.Uri + " with " +
                CommandLineInterface.FormatIntegerForDisplay(file.Text.Length) +
                " chars in " +
                CommandLineInterface.FormatIntegerForDisplay((int)clock.Elapsed.TotalMilliseconds) +
                " ms");

            clock.Restart();

            lock (_documentStateLock)
            {
                if (_clientTextDocumentContents.TryGetValue(change.Uri, out var openDocumentContent))
                {
                    languageServiceState.AddFile(change.Uri, openDocumentContent);
                }
                else
                {
                    languageServiceState.AddFile(change.Uri, file.Text);
                    anyChangeApplied = true;
                }
            }

            Log(
                "Processed file " + change.Uri + " in language service in " +
                CommandLineInterface.FormatIntegerForDisplay((int)clock.Elapsed.TotalMilliseconds) +
                " ms");

            if (IsElmJsonDocumentUri(change.Uri))
            {
                CollectDirectDependenciesFromElmJsonFile(file.Text);
            }
        }

        if (anyChangeApplied)
        {
            BumpSourceRevision();
        }

        LoadDirectDependenciesFromElmJsonFiles(languageServiceState);
    }

    private void CollectDirectDependenciesFromElmJsonFile(string elmJson)
    {
        try
        {
            var elmJsonFileParsed =
                System.Text.Json.JsonSerializer.Deserialize<ElmJsonStructure>(elmJson);

            if (elmJsonFileParsed?.Dependencies.Direct is { } directDependencies)
            {
                foreach (var (packageName, packageVersion) in directDependencies)
                {
                    var packageNameItems = packageName.Split('/');

                    if (packageNameItems.Length is not 2)
                    {
                        Log("Ignoring invalid package name: " + packageName);
                        continue;
                    }

                    var packageVersionId =
                        new ElmPackageVersion019Identifer(
                            PackageName: packageName,
                            VersionTag: packageVersion);

                    if (_elmJsonDirectDependencies.ContainsKey(packageVersionId))
                    {
                        continue;
                    }

                    _elmJsonDirectDependencies[packageVersionId] = packageVersion;

                    Log("Registered direct dependency: " + packageName + " " + packageVersion);
                }
            }
        }
        catch (System.Exception e)
        {
            Log("Failed reading elm.json file: " + e);
        }
    }

    private void LoadDirectDependenciesFromElmJsonFiles(
        ILanguageServiceSession session)
    {
        foreach (var dependency in _elmJsonDirectDependencies)
        {
            if (_elmJsonDirectDependenciesLoaded.ContainsKey(dependency.Key))
            {
                continue;
            }

            var clock = System.Diagnostics.Stopwatch.StartNew();

            var loadResult = elmPackageSource.LoadPackage(dependency.Key);

            if (loadResult.IsErrOrNull() is { } loadError)
            {
                Log(
                    "Failed loading package " + dependency.Key.PackageName + " " +
                    dependency.Key.VersionTag + ": " + loadError.Kind + ": " + loadError.Message);

                continue;
            }

            if (OkPackageOrNull(loadResult) is not { } packageContent)
            {
                Log("Did not find package: " + dependency.Key + " " + dependency.Value);
                continue;
            }

            var addPackageResponse =
                session.AddElmPackage(
                    dependency.Key,
                    packageContent.Modules);

            if (addPackageResponse.IsErrOrNull() is { } addErr)
            {
                Log("Failed adding package: " + addErr);
                continue;
            }

            _elmJsonDirectDependenciesLoaded[dependency.Key] =
                NormalizeDirectoryUri(packageContent.RootUri);

            Log(
                "Loaded package: " + dependency.Key.PackageName + " " + dependency.Key.VersionTag +
                ": Added " + packageContent.Modules.Count + " Elm modules in " +
                CommandLineInterface.FormatIntegerForDisplay((int)clock.Elapsed.TotalMilliseconds) + " ms");
        }
    }

    /// <summary>
    /// Formats a document using the configured <see cref="IDocumentFormatter"/> and returns the
    /// text edits transforming the current content into the formatted content.
    /// </summary>
    public async Task<IReadOnlyList<Protocol.TextEdit>> TextDocument_formattingAsync(
        TextDocumentIdentifier textDocument,
        FormattingOptions options,
        CancellationToken cancellationToken = default)
    {
        var textDocumentUri = DocumentUriCleaned(textDocument.Uri);

        Log("TextDocument_formatting: " + textDocumentUri);

        string? textDocumentContentBefore;

        lock (_documentStateLock)
        {
            _clientTextDocumentContents.TryGetValue(textDocumentUri, out textDocumentContentBefore);
        }

        if (textDocumentContentBefore is not null)
        {
            Log("Found document " + textDocumentUri + " in client-managed state");
        }
        else
        {
            var readResult = workspace.ReadFile(textDocumentUri);

            if (readResult.IsErrOrNull() is { } readError)
            {
                Log(
                    "Failed reading " + textDocumentUri + " for formatting: " +
                    readError.Kind + ": " + readError.Message);
            }
            else
            {
                textDocumentContentBefore = OkFileOrNull(readResult)?.Text;
            }
        }

        if (textDocumentContentBefore is null)
        {
            return [];
        }

        IReadOnlyList<string> linesBefore =
            [.. textDocumentContentBefore.ModuleLines()];

        Log(
            "Document " + textDocumentUri + " had " +
            CommandLineInterface.FormatIntegerForDisplay(linesBefore.Count) +
            " lines and " +
            CommandLineInterface.FormatIntegerForDisplay(textDocumentContentBefore.Length) +
            " chars before");

        var formatClock = System.Diagnostics.Stopwatch.StartNew();

        Result<DocumentFormattingError, string> formatResult;

        try
        {
            formatResult =
                await documentFormatter.FormatAsync(
                    textDocumentUri,
                    textDocumentContentBefore,
                    options,
                    cancellationToken);
        }
        catch (System.OperationCanceledException)
        {
            Log("Formatting " + textDocumentUri + " was canceled");
            return [];
        }
        catch (System.Exception e)
        {
            Log("Error: Failed formatting document " + textDocumentUri + ": " + e);
            return [];
        }

        if (formatResult.IsErrOrNull() is { } formatError)
        {
            Log(
                "Exiting because formatting " + textDocumentUri + " failed: " +
                formatError.Kind + ": " + formatError.Message);

            /*
             * Even when formatting could not produce new content (for example because of
             * syntax errors), publish diagnostics so the user can see them right away.
             * Locations refer to the unchanged document content.
             * */
            await PublishFormattingDiagnosticsAsync(textDocumentUri, cancellationToken);

            return [];
        }

        if (formatResult.IsOkOrNull() is not { } newContent)
        {
            throw new System.NotImplementedException(
                "Unexpected formatting result type: " + formatResult.GetType());
        }

        Log(
            "Completed formatting " + textDocumentUri + " in " +
            CommandLineInterface.FormatIntegerForDisplay((int)formatClock.Elapsed.TotalMilliseconds) +
            " ms");

        lock (_documentStateLock)
        {
            if (_clientTextDocumentContents.TryGetValue(textDocumentUri, out var currentContent) &&
                currentContent == textDocumentContentBefore)
            {
                _clientTextDocumentContents[textDocumentUri] = newContent;
            }
        }

        var textEdits =
            ComputeTextEditsForDocumentFormat(textDocumentContentBefore, newContent);

        Log(
            "Formatting document " + textDocumentUri + ": Computed " +
            textEdits.Count + " text edits with " +
            textEdits.Sum(te => te.NewText.Length) + " aggregate chars replaced or added");

        /*
         * Publish diagnostics for the formatted document. The client will apply the returned
         * edits, so locations refer to the formatted content. Publishing (even an empty list)
         * ensures stale diagnostics are removed on formatting.
         * */
        await PublishFormattingDiagnosticsAsync(textDocumentUri, cancellationToken);

        return textEdits;
    }

    private ValueTask PublishFormattingDiagnosticsAsync(
        string textDocumentUri,
        CancellationToken cancellationToken)
    {
        if (formattingDiagnosticsProvider is not { } provider)
        {
            return ValueTask.CompletedTask;
        }

        return
            RunDiagnosticsAsync(
                provider,
                textDocumentUri,
                cancellationToken);
    }

    /// <summary>
    /// Provides hover information for a text document position.
    /// </summary>
    public Hover? TextDocument_hover(
        TextDocumentPositionParams positionParams)
    {
        var textDocumentUri = DocumentUriCleaned(positionParams.TextDocument.Uri);

        var clock = System.Diagnostics.Stopwatch.StartNew();

        Log("TextDocument_hover: " + textDocumentUri + " at " + positionParams.Position);

        var hoverStrings =
            ProvideHover(
                new ProvideHoverRequestStruct(
                    InterfaceFileLocationFromUri(textDocumentUri),
                    /*
                     * The language service currently uses the 1-based line and column numbers
                     * inherited from the Monaco editor API.
                     * */
                    (int)positionParams.Position.Line + 1,
                    (int)positionParams.Position.Character + 1));

        {
            if (hoverStrings.IsErrOrNull() is { } err)
            {
                Log("Failed to provide hover: " + err);
                return null;
            }
        }

        if (hoverStrings.IsOkOrNull() is not { } hoverStringsOk)
        {
            throw new System.NotImplementedException(
                "Unexpected result type: " + hoverStrings.GetType());
        }

        Log(
            "Completed hover in " +
            CommandLineInterface.FormatIntegerForDisplay(clock.ElapsedMilliseconds) + " ms, returning " +
            hoverStringsOk.Count + " items");

        return
            new Hover(
                Contents: hoverStringsOk,
                Range: null);
    }

    /// <summary>
    /// Provides completion items for a text document position.
    /// </summary>
    public CompletionItem[] TextDocument_completion(
        TextDocumentPositionParams positionParams)
    {
        var textDocumentUri = DocumentUriCleaned(positionParams.TextDocument.Uri);

        var clock = System.Diagnostics.Stopwatch.StartNew();

        Log("TextDocument_completion: " + textDocumentUri + " at " + positionParams.Position);

        var completionItems =
            ProvideCompletionItems(
                new ProvideCompletionItemsRequestStruct(
                    textDocumentUri,
                    /*
                     * The language service currently uses the 1-based line and column numbers
                     * inherited from the Monaco editor API.
                     * */
                    CursorLineNumber:
                    (int)positionParams.Position.Line + 1,
                    CursorColumn:
                    (int)positionParams.Position.Character + 1));

        {
            if (completionItems.IsErrOrNull() is { } err)
            {
                Log("Failed to provide completion items: " + err);
                return [];
            }
        }

        if (completionItems.IsOkOrNull() is not { } completionItemsOk)
        {
            throw new System.NotImplementedException(
                "Unexpected result type: " + completionItems.GetType());
        }

        Log(
            "Completed completion in " +
            CommandLineInterface.FormatIntegerForDisplay(clock.ElapsedMilliseconds) + " ms, returning " +
            completionItemsOk.Count + " items");

        return
            [
            ..completionItemsOk
            .Select(
                monacoCompletionItem =>
                new CompletionItem(
                    Label: monacoCompletionItem.Label,
                    SortText: null,
                    FilterText: null,
                    InsertText: monacoCompletionItem.InsertText,
                    TextEditText: null,
                    Detail: null,
                    Documentation: monacoCompletionItem.Documentation,
                    Preselect: null,
                    Deprecated: null,
                    CommitCharacters: null))
            ];
    }

    /// <summary>
    /// Provides definition locations for a text document position.
    /// </summary>
    public IReadOnlyList<Location> TextDocument_definition(
        TextDocumentPositionParams positionParams)
    {
        var textDocumentUri = DocumentUriCleaned(positionParams.TextDocument.Uri);

        var clock = System.Diagnostics.Stopwatch.StartNew();

        Log("TextDocument_definition: " + textDocumentUri + " at " + positionParams.Position);

        var provideDefinitionResult =
            ProvideDefinition(
                new ProvideHoverRequestStruct(
                    InterfaceFileLocationFromUri(textDocumentUri),
                    PositionLineNumber: (int)positionParams.Position.Line + 1,
                    PositionColumn: (int)positionParams.Position.Character + 1));

        {
            if (provideDefinitionResult.IsErrOrNull() is { } err)
            {
                Log("Failed to provide definition: " + err);
                return [];
            }
        }

        if (provideDefinitionResult.IsOkOrNull() is not { } provideDefinitionOk)
        {
            throw new System.NotImplementedException(
                "Unexpected result type: " + provideDefinitionResult.GetType());
        }

        Log(
            "Completed provide definition in " +
            CommandLineInterface.FormatIntegerForDisplay(clock.ElapsedMilliseconds) + " ms, returning " +
            provideDefinitionOk.Count + " items");

        var locations =
            MapLocations(
                locations: provideDefinitionOk,
                noMatchingUri:
                fileLocation =>
                {
                    Log("No corresponding URI for " + fileLocation);

                    return [];
                })
            .ToImmutableArray();

        Log(
            "Returning " + locations.Length + " locations: " +
            string.Join(
                ", ",
                locations
                .Select(l => l.Uri + ": " + l.Range.Start.Line)));

        return locations;
    }

    /// <summary>
    /// Provides symbols for a text document.
    /// </summary>
    public IReadOnlyList<Protocol.DocumentSymbol> TextDocument_documentSymbol(
        TextDocumentIdentifier textDocument)
    {
        var textDocumentUri = DocumentUriCleaned(textDocument.Uri);

        var clock = System.Diagnostics.Stopwatch.StartNew();

        Log("textDocument/documentSymbol: " + textDocumentUri);

        var documentSymbols =
            TextDocumentSymbolRequest(textDocumentUri);

        {
            if (documentSymbols.IsErrOrNull() is { } err)
            {
                Log("Failed to provide document symbols: " + err);
                return [];
            }
        }

        if (documentSymbols.IsOkOrNull() is not { } documentSymbolsOk)
        {
            throw new System.NotImplementedException(
                "Unexpected result type: " + documentSymbols.GetType());
        }

        Log(
            "Completed document symbols in " +
            CommandLineInterface.FormatIntegerForDisplay(clock.ElapsedMilliseconds) + " ms, returning " +
            documentSymbolsOk.Count + " items");

        static Protocol.SymbolKind MapSymbolKind(Interface.SymbolKind symbolKind)
        {
            return symbolKind switch
            {
                Interface.SymbolKind.File => Protocol.SymbolKind.File,
                Interface.SymbolKind.Module => Protocol.SymbolKind.Module,
                Interface.SymbolKind.Namespace => Protocol.SymbolKind.Namespace,
                Interface.SymbolKind.Package => Protocol.SymbolKind.Package,
                Interface.SymbolKind.Class => Protocol.SymbolKind.Class,
                Interface.SymbolKind.Enum => Protocol.SymbolKind.Enum,
                Interface.SymbolKind.Interface => Protocol.SymbolKind.Interface,
                Interface.SymbolKind.Function => Protocol.SymbolKind.Function,
                Interface.SymbolKind.Constant => Protocol.SymbolKind.Constant,
                Interface.SymbolKind.String => Protocol.SymbolKind.String,
                Interface.SymbolKind.Number => Protocol.SymbolKind.Number,
                Interface.SymbolKind.Boolean => Protocol.SymbolKind.Boolean,
                Interface.SymbolKind.Array => Protocol.SymbolKind.Array,
                Interface.SymbolKind.EnumMember => Protocol.SymbolKind.EnumMember,
                Interface.SymbolKind.Struct => Protocol.SymbolKind.Struct,

                _ =>
                throw new System.NotImplementedException("Unexpected symbol kind: " + symbolKind)
            };
        }

        static Protocol.DocumentSymbol MapDocumentSymbol(DocumentSymbolStruct documentSymbol)
        {
            return
                new Protocol.DocumentSymbol(
                    Name: documentSymbol.Name,
                    Detail: null,
                    Kind: MapSymbolKind(documentSymbol.Kind),
                    Range: new Range(
                        Start: new Position(
                            Line: (uint)documentSymbol.Range.StartLineNumber - 1,
                            Character: (uint)documentSymbol.Range.StartColumn - 1),
                        End: new Position(
                            Line: (uint)documentSymbol.Range.EndLineNumber - 1,
                            Character: (uint)documentSymbol.Range.EndColumn - 1)),
                    SelectionRange: new Range(
                        Start: new Position(
                            Line: (uint)documentSymbol.SelectionRange.StartLineNumber - 1,
                            Character: (uint)documentSymbol.SelectionRange.StartColumn - 1),
                        End: new Position(
                            Line: (uint)documentSymbol.SelectionRange.EndLineNumber - 1,
                            Character: (uint)documentSymbol.SelectionRange.EndColumn - 1)),
                    Children:
                    [
                    ..documentSymbol.Children
                    .Select(cn => MapDocumentSymbol(cn.Struct))
                    ]);
        }

        return
            [
            ..documentSymbolsOk
            .Select(ds => MapDocumentSymbol(ds.Struct))
            ];
    }

    /// <summary>
    /// Provides reference locations for a text document position.
    /// </summary>
    public IReadOnlyList<Location> TextDocument_references(
        TextDocumentPositionParams positionParams)
    {
        var textDocumentUri = DocumentUriCleaned(positionParams.TextDocument.Uri);

        var clock = System.Diagnostics.Stopwatch.StartNew();

        Log("TextDocument_references: " + textDocumentUri + " at " + positionParams.Position);

        var provideReferenceResult =
            TextDocumentReferencesRequest(
                new ProvideHoverRequestStruct(
                    InterfaceFileLocationFromUri(textDocumentUri),
                    PositionLineNumber: (int)positionParams.Position.Line + 1,
                    PositionColumn: (int)positionParams.Position.Character + 1));

        {
            if (provideReferenceResult.IsErrOrNull() is { } err)
            {
                Log("Failed to provide references: " + err);
                return [];
            }
        }

        if (provideReferenceResult.IsOkOrNull() is not { } provideReferenceOk)
        {
            throw new System.NotImplementedException(
                "Unexpected result type: " + provideReferenceResult.GetType());
        }

        Log(
            "Completed provide references in " +
            CommandLineInterface.FormatIntegerForDisplay(clock.ElapsedMilliseconds) + " ms, returning " +
            provideReferenceOk.Count + " items");

        var locations =
            MapLocations(
                locations: provideReferenceOk,
                noMatchingUri:
                fileLocation =>
                {
                    Log("No corresponding URI for " + fileLocation);
                    return [];
                })
            .ToImmutableArray();

        Log(
            "Returning " + locations.Length + " locations: " +
            string.Join(
                ", ",
                locations
                .Select(l => l.Uri + ": " + l.Range.Start.Line)));

        return locations;
    }

    /// <summary>
    /// Provides edits to rename a symbol.
    /// </summary>
    public Result<string, Protocol.WorkspaceEdit?> TextDocument_rename(
        Protocol.RenameParams renameParams)
    {
        var textDocumentUri = DocumentUriCleaned(renameParams.TextDocument.Uri);

        var clock = System.Diagnostics.Stopwatch.StartNew();

        Log("TextDocument_rename: " + textDocumentUri + " at " + renameParams.Position);

        var provideRenameResult =
            TextDocumentRenameRequest(
                new Interface.RenameParams(
                    textDocumentUri,
                    /*
                     * The language service currently uses the 1-based line and column numbers
                     * inherited from the Monaco editor API.
                     * */
                    PositionLineNumber: (int)renameParams.Position.Line + 1,
                    PositionColumn: (int)renameParams.Position.Character + 1,
                    NewName: renameParams.NewName));

        {
            if (provideRenameResult.IsErrOrNull() is { } err)
            {
                Log("Failed to provide rename: " + err);

                return "Failed to provide rename: " + err;
            }
        }

        if (provideRenameResult.IsOkOrNull() is not { } provideRenameOk)
        {
            throw new System.NotImplementedException(
                "Unexpected result type: " + provideRenameResult.GetType());
        }

        Log(
            "Completed rename in " +
            CommandLineInterface.FormatIntegerForDisplay(clock.ElapsedMilliseconds) + " ms, returning " +
            provideRenameOk.Edits.Sum(fe => fe.Edits.Count) + " edits across " +
            provideRenameOk.Edits.Count + " files");

        var documentChanges =
            provideRenameOk.Edits
            .Select(
                documentEdit =>
                {
                    var editsInDocument =
                        documentEdit.Edits
                        .Select(
                            edit =>
                            new Protocol.TextEdit(
                                Range: new Range(
                                    Start: new Position(
                                        Line: (uint)edit.Range.StartLineNumber - 1,
                                        Character: (uint)edit.Range.StartColumn - 1),
                                    End: new Position(
                                        Line: (uint)edit.Range.EndLineNumber - 1,
                                        Character: (uint)edit.Range.EndColumn - 1)),
                                NewText: edit.NewText));

                    return
                        new Protocol.TextDocumentEdit(
                            new OptionalVersionedTextDocumentIdentifier(documentEdit.FilePath, Version: null),
                            [.. editsInDocument]);
                })
            .ToImmutableArray();

        return new Protocol.WorkspaceEdit(documentChanges);
    }

    /// <summary>
    /// Applies the saved document content and refreshes the diagnostics owned by that document.
    /// </summary>
    public async Task TextDocument_didSaveAsync(
        DidSaveTextDocumentParams didSaveParams,
        CancellationToken cancellationToken = default)
    {
        var textDocumentUri = DocumentUriCleaned(didSaveParams.TextDocument.Uri);

        _allSeenDocumentUris[textDocumentUri] = textDocumentUri;

        Log("TextDocument_didSave: " + textDocumentUri);

        if (didSaveParams.Text is { } text)
        {
            var appliedToOpenDocument = false;

            lock (_documentStateLock)
            {
                /*
                 * Only update the client-managed contents for documents which are open: adding an
                 * entry for a document which is not open would shadow the contents from the
                 * workspace for the remaining lifetime of the server.
                 * */
                if (_clientTextDocumentContents.ContainsKey(textDocumentUri))
                {
                    _clientTextDocumentContents[textDocumentUri] = text;
                    appliedToOpenDocument = true;
                }
            }

            if (appliedToOpenDocument)
            {
                if (GetLanguageServiceState("saving document") is { } languageServiceState)
                {
                    lock (_documentStateLock)
                    {
                        if (_clientTextDocumentContents.TryGetValue(textDocumentUri, out var currentContent) &&
                            currentContent == text)
                        {
                            languageServiceState.AddFile(textDocumentUri, text);
                        }
                    }
                }

                BumpSourceRevision();
            }
            else
            {
                Log(
                    "Ignoring text from save notification for document which is not open: " +
                    textDocumentUri);
            }
        }

        await RunDiagnosticsAsync(diagnosticsProvider, textDocumentUri, cancellationToken);
    }

    /// <summary>
    /// Runs a diagnostics provider for an entry-point document and publishes the aggregate
    /// diagnostics of all entry points for each affected document.
    /// </summary>
    private async ValueTask RunDiagnosticsAsync(
        IDiagnosticsProvider provider,
        string entryPointDocumentUri,
        CancellationToken cancellationToken)
    {
        long generation;

        lock (_diagnosticsLock)
        {
            _diagnosticsGenerations.TryGetValue(entryPointDocumentUri, out var previousGeneration);

            generation = previousGeneration + 1;

            _diagnosticsGenerations[entryPointDocumentUri] = generation;
        }

        var revision = Interlocked.Read(ref _sourceRevision);

        var clock = System.Diagnostics.Stopwatch.StartNew();

        Result<DiagnosticsProviderError, IReadOnlyList<DocumentDiagnostics>> providerResult;

        try
        {
            providerResult =
                await provider.GetDiagnosticsAsync(entryPointDocumentUri, cancellationToken);
        }
        catch (System.OperationCanceledException)
        {
            Log("Diagnostics for " + entryPointDocumentUri + " were canceled");
            return;
        }
        catch (System.Exception e)
        {
            Log("Diagnostics provider failed for " + entryPointDocumentUri + ": " + e);
            return;
        }

        if (cancellationToken.IsCancellationRequested)
        {
            Log("Discarding canceled diagnostics for " + entryPointDocumentUri);
            return;
        }

        if (providerResult.IsErrOrNull() is { } providerError)
        {
            /*
             * Keep the diagnostics from the last successful run: a failure to compute new
             * diagnostics is not evidence that the previous ones disappeared.
             * */
            Log(
                "Diagnostics provider reported a failure for " + entryPointDocumentUri + ": " +
                providerError.Kind + ": " + providerError.Message);

            return;
        }

        if (providerResult.IsOkOrNull() is not { } providerDiagnostics)
        {
            throw new System.NotImplementedException(
                "Unexpected diagnostics result type: " + providerResult.GetType());
        }

        Log(
            "Completed diagnostics for " + entryPointDocumentUri + " in " +
            CommandLineInterface.FormatIntegerForDisplay((int)clock.Elapsed.TotalMilliseconds) +
            " ms, reporting on " + providerDiagnostics.Count + " documents");

        IReadOnlyList<PublishDiagnosticsParams> toPublish;

        lock (_diagnosticsLock)
        {
            if (_diagnosticsGenerations.TryGetValue(entryPointDocumentUri, out var currentGeneration) &&
                currentGeneration != generation)
            {
                Log(
                    "Discarding superseded diagnostics for " + entryPointDocumentUri +
                    " (generation " + generation + " of " + currentGeneration + ")");

                return;
            }

            if (Interlocked.Read(ref _sourceRevision) != revision)
            {
                Log(
                    "Discarding diagnostics for " + entryPointDocumentUri +
                    " computed from outdated sources");

                return;
            }

            var affectedUris =
                new HashSet<string>(System.StringComparer.Ordinal)
                {
                    entryPointDocumentUri
                };

            if (_diagnosticsByEntryPoint.TryGetValue(entryPointDocumentUri, out var previousDiagnostics))
            {
                foreach (var previous in previousDiagnostics)
                {
                    affectedUris.Add(previous.DocumentUri);
                }
            }

            IReadOnlyList<DocumentDiagnostics> normalized =
                [
                .. providerDiagnostics
                .Select(
                    documentDiagnostics =>
                    new DocumentDiagnostics(
                        DocumentUriCleaned(documentDiagnostics.DocumentUri),
                        documentDiagnostics.Diagnostics))
                ];

            foreach (var documentDiagnostics in normalized)
            {
                affectedUris.Add(documentDiagnostics.DocumentUri);
            }

            _diagnosticsByEntryPoint[entryPointDocumentUri] = normalized;

            toPublish = AggregateDiagnostics(affectedUris);
        }

        PublishDiagnostics(toPublish);
    }

    /// <summary>
    /// Drops the diagnostics owned by an entry-point document and publishes the aggregates for
    /// the documents which were covered by it.
    /// </summary>
    private void RemoveDiagnosticsEntryPoint(string entryPointDocumentUri)
    {
        IReadOnlyList<PublishDiagnosticsParams> toPublish;

        lock (_diagnosticsLock)
        {
            if (!_diagnosticsByEntryPoint.TryGetValue(entryPointDocumentUri, out var previousDiagnostics))
            {
                return;
            }

            _diagnosticsByEntryPoint.Remove(entryPointDocumentUri);

            /*
             * Invalidate diagnostics runs in flight for the removed entry point.
             * */
            _diagnosticsGenerations.TryGetValue(entryPointDocumentUri, out var generation);
            _diagnosticsGenerations[entryPointDocumentUri] = generation + 1;

            var affectedUris =
                new HashSet<string>(System.StringComparer.Ordinal)
                {
                    entryPointDocumentUri
                };

            foreach (var previous in previousDiagnostics)
            {
                affectedUris.Add(previous.DocumentUri);
            }

            toPublish = AggregateDiagnostics(affectedUris);
        }

        PublishDiagnostics(toPublish);
    }

    /// <summary>
    /// Aggregates the contributions of all entry points for the given documents.
    /// Must be called while holding the diagnostics lock.
    /// </summary>
    private IReadOnlyList<PublishDiagnosticsParams> AggregateDiagnostics(
        IReadOnlyCollection<string> documentUris)
    {
        var aggregate = new List<PublishDiagnosticsParams>(documentUris.Count);

        foreach (var documentUri in documentUris.OrderBy(uri => uri, System.StringComparer.Ordinal))
        {
            var diagnostics = new List<Diagnostic>();
            var seen = new HashSet<string>(System.StringComparer.Ordinal);

            foreach (var entryPoint in
                _diagnosticsByEntryPoint.OrderBy(entry => entry.Key, System.StringComparer.Ordinal))
            {
                foreach (var documentDiagnostics in entryPoint.Value)
                {
                    if (!string.Equals(documentDiagnostics.DocumentUri, documentUri, System.StringComparison.Ordinal))
                    {
                        continue;
                    }

                    foreach (var diagnostic in documentDiagnostics.Diagnostics)
                    {
                        if (seen.Add(DiagnosticIdentity(diagnostic)))
                        {
                            diagnostics.Add(diagnostic);
                        }
                    }
                }
            }

            int? version = null;

            /*
             * Read the version without taking the document-state lock: handlers holding that lock
             * can end up here via entry-point removal, so taking it here would invert the lock
             * order between the document state and the diagnostics state.
             * */
            if (_clientTextDocumentVersions.TryGetValue(documentUri, out var documentVersion))
            {
                version = documentVersion;
            }

            aggregate.Add(
                new PublishDiagnosticsParams(
                    documentUri,
                    [
                    .. diagnostics
                    .OrderBy(DiagnosticIdentity, System.StringComparer.Ordinal)
                    ],
                    version));
        }

        return aggregate;
    }

    private void PublishDiagnostics(IReadOnlyList<PublishDiagnosticsParams> toPublish)
    {
        if (_publishDiagnostics is not { } publish)
        {
            if (0 < toPublish.Count)
            {
                Log("Cannot publish diagnostics: no publisher configured");
            }

            return;
        }

        foreach (var publishParams in toPublish)
        {
            Log(
                "Publishing " + publishParams.Diagnostics.Count + " diagnostics for " +
                publishParams.Uri);

            publish(publishParams);
        }
    }

    /// <summary>
    /// Text used to order and deduplicate diagnostics from multiple entry points.
    /// </summary>
    private static string DiagnosticIdentity(Diagnostic diagnostic) =>
        string.Join(
            "\u0000",
            diagnostic.Range.Start.Line.ToString("D9"),
            diagnostic.Range.Start.Character.ToString("D9"),
            diagnostic.Range.End.Line.ToString("D9"),
            diagnostic.Range.End.Character.ToString("D9"),
            ((int?)diagnostic.Severity)?.ToString() ?? "",
            diagnostic.Source ?? "",
            diagnostic.Code ?? "",
            diagnostic.Message);

    private void BumpSourceRevision()
    {
        Interlocked.Increment(ref _sourceRevision);
    }

    /// <summary>
    /// Returns the last path component of a document URI, unescaped.
    /// </summary>
    public static string? DocumentUriFileName(string documentUri)
    {
        var withoutQuery = documentUri.Split('?', '#')[0];

        var lastSlashIndex = withoutQuery.LastIndexOf('/');

        var lastComponent =
            lastSlashIndex < 0
            ?
            withoutQuery
            :
            withoutQuery[(lastSlashIndex + 1)..];

        if (lastComponent.Length is 0)
        {
            return null;
        }

        return System.Uri.UnescapeDataString(lastComponent);
    }

    private static bool IsElmModuleDocumentUri(string documentUri) =>
        DocumentUriFileName(documentUri) is { } fileName &&
        fileName.EndsWith(".elm", System.StringComparison.OrdinalIgnoreCase);

    private static bool IsElmJsonDocumentUri(string documentUri) =>
        string.Equals(
            DocumentUriFileName(documentUri),
            "elm.json",
            System.StringComparison.OrdinalIgnoreCase);

    private static string NormalizeDirectoryUri(string directoryUri) =>
        directoryUri.EndsWith('/')
        ?
        directoryUri
        :
        directoryUri + "/";

    /// <summary>
    /// Computes language-server diagnostics for syntax errors in an Elm module, reusing the
    /// locations and messages reported by the Elm syntax parser. Each diagnostic has severity
    /// 'error' and source "elm syntax".
    /// <para>
    /// Returns an empty list when the module parses cleanly (it may still need formatting).
    /// When the module cannot be parsed at all, a single diagnostic is reported at the start
    /// of the document carrying the parser's error message.
    /// </para>
    /// </summary>
    public static IReadOnlyList<Diagnostic> ComputeSyntaxErrorDiagnostics(string moduleText)
    {
        const string DiagnosticSource = "elm syntax";

        var formatResult =
            ElmFormat.FormatModuleTextReportingSyntaxErrors(moduleText);

        if (formatResult.IsErrOrNullable() is { } parseErr)
        {
            var locationMapped =
                new Position(
                    Line: (uint)parseErr.Location.Row - 1,
                    Character: (uint)parseErr.Location.Column - 1);

            // The module could not be parsed at all (e.g. malformed module header).
            // Report a single diagnostic at the start of the document.
            return
                [
                new Diagnostic(
                    Range: new Range(
                        Start: locationMapped,
                        End: locationMapped),
                    Severity: DiagnosticSeverity.Error,
                    Code: null,
                    Source: DiagnosticSource,
                    Message: parseErr.ToString(),
                    CodeDescription: null,
                    Tags: null,
                    RelatedInformation: null)
                ];
        }

        if (formatResult.IsOkOrNull() is not { } formatOk)
        {
            throw new System.NotImplementedException(
                "Unexpected ElmFormat.FormatModuleTextReportingSyntaxErrors result: " + formatResult.GetType());
        }

        return
            [
            ..formatOk.SyntaxErrors
            .Select(
                syntaxError =>
                {
                    // The Elm syntax parser uses 1-based rows/columns; LSP uses 0-based positions.
                    var startLine = System.Math.Max(0, syntaxError.Location.Row - 1);
                    var startChar = System.Math.Max(0, syntaxError.Location.Column - 1);

                    // Highlight from the error location to the end of the incomplete declaration.
                    var endLine = System.Math.Max(0, syntaxError.Range.End.Row - 1);
                    var endChar = System.Math.Max(0, syntaxError.Range.End.Column - 1);

                    // Guard against an end that precedes the start.
                    if (endLine < startLine || (endLine == startLine && endChar < startChar))
                    {
                        endLine = startLine;
                        endChar = startChar + 1;
                    }

                    return
                        new Diagnostic(
                            Range: new Range(
                                Start: new Position(Line: (uint)startLine, Character: (uint)startChar),
                                End: new Position(Line: (uint)endLine, Character: (uint)endChar)),
                            Severity: DiagnosticSeverity.Error,
                            Code: null,
                            Source: DiagnosticSource,
                            Message: syntaxError.Message,
                            CodeDescription: null,
                            Tags: null,
                            RelatedInformation: null);
                })
            ];
    }

    /// <summary>
    /// Merges files into a file tree.
    /// </summary>
    public static FileTree MergeIntoFileTree(
        FileTree seed,
        IReadOnlyDictionary<IReadOnlyList<string>, System.ReadOnlyMemory<byte>> dictionary)
    {
        return
            dictionary
            .Aggregate(
                seed:
                seed,
                (aggregate, nextFile) =>
                {
                    return
                        aggregate.SetNodeAtPathSorted(
                            nextFile.Key,
                            new FileTree.FileNode(nextFile.Value));
                });
    }

    /// <summary>
    /// Requests hover information from the language service.
    /// </summary>
    public Result<string, IReadOnlyList<string>> ProvideHover(
        ProvideHoverRequestStruct provideHoverRequest)
    {
        var genericRequestResult =
            HandleRequest(
                new Request.ProvideHoverRequest(provideHoverRequest));

        if (genericRequestResult.IsErrOrNull() is { } err)
        {
            return err;
        }

        if (genericRequestResult.IsOkOrNull() is not { } requestOk)
        {
            throw new System.NotImplementedException(
                "Unexpected request result type: " + genericRequestResult.GetType());
        }

        if (requestOk is not Response.ProvideHoverResponse provideHoverResponse)
        {
            throw new System.NotImplementedException(
                "Unexpected request result type: " + requestOk.GetType());
        }

        return Result<string, IReadOnlyList<string>>.ok(provideHoverResponse.Strings);
    }

    /// <summary>
    /// Requests completion items from the language service.
    /// </summary>
    public Result<string, IReadOnlyList<MonacoCompletionItem>>
        ProvideCompletionItems(
        ProvideCompletionItemsRequestStruct provideCompletionItemsRequest)
    {
        var genericRequestResult =
            HandleRequest(
                new Request.ProvideCompletionItemsRequest(provideCompletionItemsRequest));

        if (genericRequestResult.IsErrOrNull() is { } err)
        {
            return err;
        }

        if (genericRequestResult.IsOkOrNull() is not { } requestOk)
        {
            throw new System.NotImplementedException(
                "Unexpected request result type: " + genericRequestResult.GetType());
        }

        if (requestOk is not Response.ProvideCompletionItemsResponse provideCompletionItemsResponse)
        {
            throw new System.NotImplementedException(
                "Unexpected request result type: " + requestOk.GetType());
        }

        return
            Result<string, IReadOnlyList<MonacoCompletionItem>>.ok(
                provideCompletionItemsResponse.CompletionItems);
    }

    /// <summary>
    /// Requests definition locations from the language service.
    /// </summary>
    public Result<string, IReadOnlyList<LocationInFile>>
        ProvideDefinition(
        ProvideHoverRequestStruct provideDefinitionRequest)
    {
        var genericRequestResult =
            HandleRequest(
                new Request.ProvideDefinitionRequest(provideDefinitionRequest));

        if (genericRequestResult.IsErrOrNull() is { } err)
        {
            return err;
        }

        if (genericRequestResult.IsOkOrNull() is not { } requestOk)
        {
            throw new System.NotImplementedException(
                "Unexpected request result type: " + genericRequestResult.GetType());
        }

        if (requestOk is not Response.ProvideDefinitionResponse provideDefinitionResponse)
        {
            throw new System.NotImplementedException(
                "Unexpected request result type: " + requestOk.GetType());
        }

        return
            Result<string, IReadOnlyList<LocationInFile>>.ok(
                provideDefinitionResponse.Locations);
    }

    /// <summary>
    /// Requests document symbols from the language service.
    /// </summary>
    public Result<string, IReadOnlyList<Interface.DocumentSymbol>> TextDocumentSymbolRequest(
        string fileUri)
    {
        var genericRequestResult =
            HandleRequest(
                new Request.TextDocumentSymbolRequest(fileUri));

        if (genericRequestResult.IsErrOrNull() is { } err)
        {
            return err;
        }

        if (genericRequestResult.IsOkOrNull() is not { } requestOk)
        {
            throw new System.NotImplementedException(
                "Unexpected request result type: " + genericRequestResult.GetType());
        }

        if (requestOk is not Response.TextDocumentSymbolResponse documentSymbolResponse)
        {
            throw new System.NotImplementedException(
                "Unexpected request result type: " + requestOk.GetType());
        }

        return
            Result<string, IReadOnlyList<Interface.DocumentSymbol>>.ok(
                documentSymbolResponse.Symbols);
    }

    /// <summary>
    /// Requests reference locations from the language service.
    /// </summary>
    public Result<string, IReadOnlyList<LocationInFile>> TextDocumentReferencesRequest(
        ProvideHoverRequestStruct referenceRequest)
    {
        var genericRequestResult =
            HandleRequest(
                new Request.TextDocumentReferencesRequest(referenceRequest));

        if (genericRequestResult.IsErrOrNull() is { } err)
        {
            return err;
        }

        if (genericRequestResult.IsOkOrNull() is not { } requestOk)
        {
            throw new System.NotImplementedException(
                "Unexpected request result type: " + genericRequestResult.GetType());
        }

        if (requestOk is not Response.TextDocumentReferencesResponse provideReferenceResponse)
        {
            throw new System.NotImplementedException(
                "Unexpected request result type: " + requestOk.GetType());
        }

        return
            Result<string, IReadOnlyList<LocationInFile>>.ok(
                provideReferenceResponse.Locations);
    }

    /// <summary>
    /// Requests symbol rename edits from the language service.
    /// </summary>
    public Result<string, Interface.WorkspaceEdit> TextDocumentRenameRequest(
        Interface.RenameParams renameParams)
    {
        var genericRequestResult =
            HandleRequest(
                new Request.TextDocumentRenameRequest(renameParams));

        if (genericRequestResult.IsErrOrNull() is { } err)
        {
            return err;
        }

        if (genericRequestResult.IsOkOrNull() is not { } requestOk)
        {
            throw new System.NotImplementedException(
                "Unexpected request result type: " + genericRequestResult.GetType());
        }

        if (requestOk is not Response.TextDocumentRenameResponse renameResponse)
        {
            throw new System.NotImplementedException(
                "Unexpected request result type: " + requestOk.GetType());
        }

        return
            Result<string, Interface.WorkspaceEdit>.ok(
                renameResponse.WorkspaceEdit);
    }

    /// <summary>
    /// Handles a request in the current language service workspace.
    /// </summary>
    public Result<string, Response> HandleRequest(
        Request request)
    {
        if (_languageServiceStateTask is not { } languageServiceStateTask)
        {
            return "Language service state not initialized";
        }

        if (languageServiceStateTask.Result.IsErrOrNull() is { } err)
        {
            return err;
        }

        if (languageServiceStateTask.Result.IsOkOrNull() is not { } languageServiceState)
        {
            throw new System.NotImplementedException(
                "Unexpected language service state result type: " + languageServiceStateTask.Result.GetType());
        }

        return
            languageServiceState.HandleRequest(request);
    }

    private ILanguageServiceSession? GetLanguageServiceState(string operation)
    {
        if (_languageServiceStateTask is not { } languageServiceStateTask)
        {
            Log("Cannot update language service while " + operation + ": state not initialized");
            return null;
        }

        var taskResult = languageServiceStateTask.Result;

        if (taskResult.IsErrOrNull() is { } err)
        {
            Log("Cannot update language service while " + operation + ": " + err);
            return null;
        }

        if (taskResult.IsOkOrNull() is not { } languageServiceState)
        {
            throw new System.NotImplementedException(
                "Unexpected language service state result type: " + taskResult.GetType());
        }

        return languageServiceState;
    }

    /// <summary>
    /// Maps language service locations to protocol locations.
    /// </summary>
    public IEnumerable<Location> MapLocations(
        IEnumerable<LocationInFile> locations,
        System.Func<FileLocation, IEnumerable<Location>> noMatchingUri)
    {
        return
            locations
            .SelectMany(
                location =>
                {
                    var uri = FindMatchingUri(location.FileLocation);

                    if (uri is null)
                    {
                        return noMatchingUri(location.FileLocation);
                    }

                    return
                        [
                        new Location(
                            uri,
                            new Range(
                                Start: new Position(
                                    Line: (uint)location.Range.StartLineNumber - 1,
                                    Character: (uint)location.Range.StartColumn - 1),
                                End: new Position(
                                    Line: (uint)location.Range.EndLineNumber - 1,
                                    Character: (uint)location.Range.EndColumn - 1)))
                        ];
                });
    }

    /// <summary>
    /// Maps a document URI to a language-service file location, recognizing documents from
    /// loaded Elm packages.
    /// </summary>
    public FileLocation InterfaceFileLocationFromUri(string documentUri)
    {
        var documentUriNormalized = DocumentUriCleaned(documentUri);

        ElmPackageVersion019Identifer? bestMatchPackage = null;
        var bestMatchRootUri = "";

        foreach (var (elmPackageVersionIdentifer, packageRootUri) in _elmJsonDirectDependenciesLoaded)
        {
            /*
             * The package root URI ends with a slash, so this comparison cannot match a sibling
             * directory sharing a name prefix with the package directory.
             * */
            var packageRootUriNormalized = NormalizeDirectoryUri(DocumentUriCleaned(packageRootUri));

            if (!documentUriNormalized.StartsWith(packageRootUriNormalized, System.StringComparison.Ordinal))
            {
                continue;
            }

            if (bestMatchRootUri.Length < packageRootUriNormalized.Length)
            {
                bestMatchPackage = elmPackageVersionIdentifer;
                bestMatchRootUri = packageRootUriNormalized;
            }
        }

        if (bestMatchPackage is not null)
        {
            var modulePathFlat = documentUriNormalized[bestMatchRootUri.Length..];

            IReadOnlyList<string> modulePathItems =
                [.. modulePathFlat.Split('/').Select(System.Uri.UnescapeDataString)];

            return
                new FileLocation.ElmPackageFileLocation(
                    bestMatchPackage,
                    ModulePath: modulePathItems);
        }

        return new FileLocation.WorkspaceFileLocation(documentUriNormalized);
    }

    /// <summary>
    /// Maps a language-service file location back to a document URI.
    /// </summary>
    public string? FindMatchingUri(FileLocation fileLocation)
    {
        if (fileLocation is FileLocation.WorkspaceFileLocation workspaceFileLocation)
        {
            return workspaceFileLocation.FilePath;
        }

        if (fileLocation is FileLocation.ElmPackageFileLocation elmPackageFileLocation)
        {
            if (_elmJsonDirectDependenciesLoaded.TryGetValue(
                elmPackageFileLocation.ElmPackageVersionIdentifer,
                out var packageRootUri))
            {
                return
                    NormalizeDirectoryUri(packageRootUri) +
                    string.Join(
                        "/",
                        elmPackageFileLocation.ModulePath.Select(System.Uri.EscapeDataString));
            }
        }

        return null;
    }

    private static WorkspaceFile? OkFileOrNull(
        Result<WorkspaceAccessError, WorkspaceFile?> result) =>
        result is Result<WorkspaceAccessError, WorkspaceFile?>.Ok ok ? ok.Value : null;

    private static ElmPackageContent? OkPackageOrNull(
        Result<PackageLoadError, ElmPackageContent?> result) =>
        result is Result<PackageLoadError, ElmPackageContent?>.Ok ok ? ok.Value : null;

    /// <summary>
    /// Normalizes an escaped document URI.
    /// </summary>
    public static string DocumentUriCleaned(string documentUri)
    {
        /*
         * The client in VSCode appears to send document URIs like this:
         * file:///k%3A/Source/Repos/
         * Therefore we need to decode before handing to System.Uri
         * */

        var unescaped = System.Uri.UnescapeDataString(documentUri);

        return unescaped.Replace("\\", "/");
    }

    /// <summary>
    /// Converts a document URI to a local file path.
    /// </summary>
    public static Result<string, string> DocumentUriAsLocalPath(string documentUri)
    {
        /*
         * The client in VSCode appears to send document URIs like this:
         * file:///k%3A/Source/Repos/
         * Therefore we need to decode before handing to System.Uri
         * */
        if (System.Uri.TryCreate(
            System.Uri.UnescapeDataString(documentUri),
            System.UriKind.Absolute,
            out var uriAbsolute))
        {
            if (uriAbsolute.Scheme is not "file")
            {
                return Result<string, string>.err("non-file URI");
            }

            return Result<string, string>.ok(uriAbsolute.LocalPath);
        }

        return Result<string, string>.err("Not an absolute URI");
    }

    /// <summary>
    /// Apply a list of text edits following the specification from
    /// https://microsoft.github.io/language-server-protocol/specifications/lsp/3.18/specification/#textEditArray
    /// </summary>
    public static string ApplyTextEdits(string originalText, IReadOnlyList<Protocol.TextEdit> edits)
    {
        if (edits.Count is 0)
            return originalText;

        // Convert text to lines for easier position calculation
        var lines = originalText.ModuleLines().ToList();

        // Sort edits by start position in reverse order (end to start)
        // This ensures that applying edits doesn't invalidate positions of subsequent edits
        var sortedEdits =
            edits
            .OrderByDescending(edit => edit.Range.Start.Line)
            .ThenByDescending(edit => edit.Range.Start.Character)
            .ToList();

        // Apply each edit
        foreach (var edit in sortedEdits)
        {
            var startLine = (int)edit.Range.Start.Line;
            var startChar = (int)edit.Range.Start.Character;
            var endLine = (int)edit.Range.End.Line;
            var endChar = (int)edit.Range.End.Character;

            // Validate range bounds
            if (startLine < 0 || startLine >= lines.Count)
                continue; // Skip invalid edits

            if (endLine < 0 || endLine >= lines.Count)
                continue; // Skip invalid edits

            if (startLine == endLine)
            {
                // Single line edit
                var line = lines[startLine];

                if (startChar < 0 || startChar > line.Length || endChar < startChar || endChar > line.Length)
                    continue; // Skip invalid edits

                var before = line[..startChar];
                var after = line[endChar..];

                lines[startLine] = before + edit.NewText + after;
            }
            else
            {
                // Multi-line edit
                var firstLine = lines[startLine];
                var lastLine = lines[endLine];

                if (startChar < 0 || startChar > firstLine.Length || endChar < 0 || endChar > lastLine.Length)
                    continue; // Skip invalid edits

                var before = firstLine[..startChar];
                var after = lastLine[endChar..];

                // Replace the range with new text
                var newContent = before + edit.NewText + after;

                // Remove the lines in the range
                for (var i = endLine; i >= startLine; i--)
                {
                    lines.RemoveAt(i);
                }

                // Split new content into lines and insert
                var newLines = newContent.ModuleLines().ToList();

                for (var i = 0; i < newLines.Count; i++)
                {
                    lines.Insert(startLine + i, newLines[i]);
                }
            }
        }

        // Reconstruct the text
        return string.Join("\n", lines);
    }

    /// <summary>
    /// Compute text edits to transform original text to new text using a line-based algorithm.
    /// Finds common prefix and suffix, then creates a single edit for the middle differences.
    /// </summary>
    public static IReadOnlyList<Protocol.TextEdit> ComputeTextEditsForDocumentFormat(
        string originalText, string newText)
    {
        if (originalText == newText)
            return [];

        var originalLines =
            originalText.ModuleLines().ToList();

        var newLines =
            newText.ModuleLines().ToList();

        // Find common prefix (lines that are the same at the beginning)
        var commonPrefixLength = 0;

        var minLength =
            originalLines.Count < newLines.Count
            ?
            originalLines.Count
            :
            newLines.Count;

        while (commonPrefixLength < minLength &&
            originalLines[commonPrefixLength] == newLines[commonPrefixLength])
        {
            commonPrefixLength++;
        }

        // Find common suffix (lines that are the same at the end)
        var commonSuffixLength = 0;
        var remainingOriginal = originalLines.Count - commonPrefixLength;
        var remainingNew = newLines.Count - commonPrefixLength;

        var maxSuffixLength =
            remainingOriginal < remainingNew
            ?
            remainingOriginal
            :
            remainingNew;

        while (commonSuffixLength < maxSuffixLength &&
            originalLines[originalLines.Count - 1 - commonSuffixLength] ==
            newLines[newLines.Count - 1 - commonSuffixLength])
        {
            commonSuffixLength++;
        }

        // Calculate what needs to be replaced
        var firstChangedLine = commonPrefixLength;
        var lastChangedLineInOriginal = originalLines.Count - commonSuffixLength - 1;

        // Get the replacement text (the lines from new text that differ)
        var replacementLines =
            newLines.Slice(start: commonPrefixLength, length: newLines.Count - commonPrefixLength - commonSuffixLength);

        if (firstChangedLine >= originalLines.Count)
        {
            // Insertion at the end of the document
            var lastExistingLine = originalLines.Count - 1;
            var lastExistingLineLength = lastExistingLine >= 0 ? originalLines[lastExistingLine].Length : 0;

            var range =
                new Range(
                    Start: new Position(Line: (uint)lastExistingLine, Character: (uint)lastExistingLineLength),
                    End: new Position(Line: (uint)lastExistingLine, Character: (uint)lastExistingLineLength));

            var replacementText = "\n" + string.Join("\n", replacementLines);

            return [new Protocol.TextEdit(Range: range, NewText: replacementText)];
        }

        if (newLines.Count < originalLines.Count && firstChangedLine >= newLines.Count)
        {
            // Deletion from the end - delete extra lines from original
            var lastKeptLine = newLines.Count - 1;
            var lastKeptLineLength = lastKeptLine >= 0 ? originalLines[lastKeptLine].Length : 0;
            var lastDeletedLine = originalLines.Count - 1;
            var lastDeletedLineLength = originalLines[lastDeletedLine].Length;

            var range =
                new Range(
                    Start: new Position(Line: (uint)lastKeptLine, Character: (uint)lastKeptLineLength),
                    End: new Position(Line: (uint)lastDeletedLine, Character: (uint)lastDeletedLineLength));

            return [new Protocol.TextEdit(Range: range, NewText: "")];
        }

        // Handle edge case where lastChangedLineInOriginal might be invalid
        if (lastChangedLineInOriginal < 0 || lastChangedLineInOriginal < firstChangedLine)
        {
            // This can happen when we have an insertion in the middle
            // In this case, we need to insert at the position between common prefix and suffix

            if (firstChangedLine > 0 && replacementLines.Count > 0)
            {
                // Insert after the last line of common prefix
                var insertLine = firstChangedLine - 1;
                var insertChar = originalLines[insertLine].Length;

                var range =
                    new Range(
                        Start: new Position(Line: (uint)insertLine, Character: (uint)insertChar),
                        End: new Position(Line: (uint)insertLine, Character: (uint)insertChar));

                var replacementText = "\n" + string.Join("\n", replacementLines);

                return [new Protocol.TextEdit(Range: range, NewText: replacementText)];
            }
            else if (firstChangedLine == 0 && replacementLines.Count > 0)
            {
                // Insert at the beginning
                var range =
                    new Range(
                        Start: new Position(Line: 0, Character: 0),
                        End: new Position(Line: 0, Character: 0));

                var replacementText = string.Join("\n", replacementLines) + "\n";

                return [new Protocol.TextEdit(Range: range, NewText: replacementText)];
            }

            // If no replacement lines, this might be a degenerate case - return no edits
            return [];
        }

        if (firstChangedLine < originalLines.Count && newLines.Count < originalLines.Count &&
            replacementLines.Count is 0)
        {
            // Pure deletion in the middle - we need to delete some lines without replacement
            // The range should include the newline that creates the line to be deleted
            var startLine = firstChangedLine > 0 ? firstChangedLine - 1 : 0;
            var startChar = firstChangedLine > 0 ? originalLines[startLine].Length : 0;
            var endLine = lastChangedLineInOriginal;
            var endChar = originalLines[endLine].Length;

            var range =
                new Range(
                    Start: new Position(Line: (uint)startLine, Character: (uint)startChar),
                    End: new Position(Line: (uint)endLine, Character: (uint)endChar));

            var replacementText = string.Join("\n", replacementLines);

            return [new Protocol.TextEdit(Range: range, NewText: replacementText)];
        }

        {
            // Normal replacement case
            var range =
                new Range(
                    Start: new Position(Line: (uint)firstChangedLine, Character: 0),
                    End: new Position(
                        Line: (uint)lastChangedLineInOriginal,
                        Character: (uint)originalLines[lastChangedLineInOriginal].Length));

            var replacementText = string.Join("\n", replacementLines);

            return [new Protocol.TextEdit(Range: range, NewText: replacementText)];
        }
    }
}
