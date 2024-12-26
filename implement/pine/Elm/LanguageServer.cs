using ElmTime.ElmSyntax;
using Interface = Pine.Elm.LanguageServiceInterface;
using Pine.Core;
using Pine.Core.LanguageServerProtocol;
using Pine.Core.PineVM;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.Linq;
using Pine.PineVM;
using Pine.Elm019;
using System.Collections.Immutable;

namespace Pine.Elm;

public class LanguageServer(
    System.Action<string>? logDelegate,
    IReadOnlyList<string> elmPackagesSearchDirectories)
{
    private readonly ConcurrentDictionary<string, string> allSeenDocumentUris = new();

    private readonly ConcurrentDictionary<string, string> clientTextDocumentContents = new();

    private IReadOnlyList<WorkspaceFolder> workspaceFolders = [];

    private InitializeParams? initializeParams;

    private readonly System.Action<string>? logDelegate = logDelegate;

    private System.Threading.Tasks.Task<Result<string, WorkspaceState>>? languageServiceStateTask;

    /*
     * TODO: Use the version identifier from elm.json as scope.
     * */
    private readonly ConcurrentDictionary<string, string> elmJsonDirectDependencies = new();

    private readonly ConcurrentDictionary<string, string> elmJsonDirectDependenciesLoaded = new();

    private class WorkspaceState(
        IReadOnlyList<WorkspaceFolder> workspaceFolders,
        LanguageServiceState languageServiceState)
    {
        public static Result<string, WorkspaceState> Init(
            IReadOnlyList<WorkspaceFolder> workspaceFolders,
            IPineVM pineVM)
        {
            var initServiceStateResult = LanguageServiceState.InitLanguageServiceState(pineVM);

            if (initServiceStateResult.IsErrOrNull() is { } err)
            {
                return err;
            }

            if (initServiceStateResult.IsOkOrNull() is not { } languageServiceState)
            {
                throw new System.NotImplementedException(
                    "Unexpected language service state result type: " + initServiceStateResult.GetType());
            }

            return new WorkspaceState(workspaceFolders, languageServiceState);
        }

        public Result<string, Interface.Response.WorkspaceSummaryResponse> AddFile(
            string fileUri,
            string fileContent)
        {
            return languageServiceState.AddFile(fileUri, fileContent);
        }

        public Result<string, Interface.Response.WorkspaceSummaryResponse> DeleteFile(
            string fileUri)
        {
            return languageServiceState.DeleteFile(fileUri);
        }

        public Result<string, Interface.Response> HandleRequest(
            Interface.Request request)
        {
            return languageServiceState.HandleRequest(request);
        }
    }

    private void Log(string message)
    {
        logDelegate?.Invoke(message);
    }

    public (InitializeResult, IReadOnlyList<KeyValuePair<string, object>>) Initialize(
        InitializeParams initializeParams)
    {
        Log("Initialize: " + System.Text.Json.JsonSerializer.Serialize(initializeParams));

        this.initializeParams = initializeParams;

        workspaceFolders = initializeParams.WorkspaceFolders ?? [];

        var requests = new List<KeyValuePair<string, object>>();

        if (initializeParams.Capabilities.Workspace?.DidChangeWatchedFiles?.DynamicRegistration ?? false)
        {
            requests.AddRange(RegisterFileWatchers());
        }

        var response = new InitializeResult
        (
            Capabilities: new ServerCapabilities
            {
                TextDocumentSync =
                new TextDocumentSyncOptions(
                    Change: TextDocumentSyncKind.Full,
                    WillSave: null,
                    WillSaveWaitUntil: null,
                    Save: new SaveOptions(IncludeText: true)),

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

                Workspace = new ServerCapabilitiesWorkspace
                {
                    WorkspaceFolders = new WorkspaceFoldersServerCapabilities(Supported: true, ChangeNotifications: true),
                }
            },
            ServerInfo: new ParticipentInfo(
                Name: "Pine language server",
                Version: ElmTime.Program.AppVersionId)
        );

        System.Threading.Tasks.Task.Run(() => InitializeWorkspaceState(initializeParams));

        return (response, requests);
    }

    private IReadOnlyList<KeyValuePair<string, object>> RegisterFileWatchers()
    {
        Log("Registering file watchers...");

        var registrationParams = new RegistrationParams
        (
            Registrations:
            [
                new Registration
                (
                    Id : "workspace/didChangeWatchedFiles",
                    Method : "workspace/didChangeWatchedFiles",
                    RegisterOptions : new DidChangeWatchedFilesRegistrationOptions
                    (
                        Watchers:
                        [
                            new FileSystemWatcher
                            (
                                GlobPattern : "**/*.elm",
                                Kind : WatchKind.Create | WatchKind.Change | WatchKind.Delete
                            )
                        ]
                    )
                )
            ]
        );

        return
            [
            new KeyValuePair<string, object>("client/registerCapability", registrationParams)
            ];
    }

    void InitializeWorkspaceState(InitializeParams initializeParams)
    {
        IEnumerable<string> composeDirectories()
        {
            if (initializeParams.RootPath is { } rootPath)
            {
                yield return rootPath;
            }

            if (initializeParams.WorkspaceFolders is { } workspaceFolders)
            {
                foreach (var workspaceFolder in workspaceFolders)
                {
                    if (workspaceFolder.Uri is { } uri)
                    {
                        var localPathResult = DocumentUriAsLocalPath(uri);

                        if (localPathResult.IsErrOrNull() is { } err)
                        {
                            Log("Ignoring URI: " + err + ": " + uri);
                            continue;
                        }

                        if (localPathResult.IsOkOrNull() is not { } localPath)
                        {
                            throw new System.NotImplementedException(
                                "Unexpected result type: " + localPathResult.GetType());
                        }

                        yield return localPath;
                    }
                }
            }
        }

        languageServiceStateTask = System.Threading.Tasks.Task.Run(() =>
        {
            var vmCache = new PineVMCache();

            var pineVM = new PineVM.PineVM(evalCache: vmCache.EvalCache);

            var initResult = WorkspaceState.Init(workspaceFolders, pineVM);

            if (initResult.IsErrOrNull() is { } err)
            {
                Log("Failed initializing language service state: " + err);
            }

            return initResult;
        });

        var taskResult = languageServiceStateTask.Result;

        if (taskResult.IsErrOrNull() is { } err)
        {
            Log("Failed initializing language service state: " + err);
            return;
        }

        if (taskResult.IsOkOrNull() is not { } languageServiceState)
        {
            throw new System.NotImplementedException(
                "Unexpected language service state result type: " + taskResult.GetType());
        }

        IReadOnlyList<string> sourceDirectories = [.. composeDirectories().Distinct()];

        Log("Starting to initialize files contents for " + sourceDirectories.Count + " directories");

        var aggregateClock = System.Diagnostics.Stopwatch.StartNew();

        var aggregateElmModuleFiles = new HashSet<string>();
        var aggregateElmJsonFiles = new HashSet<string>();

        foreach (var sourceDirectory in sourceDirectories)
        {
            try
            {
                var elmModuleFiles =
                    System.IO.Directory.GetFiles(sourceDirectory, "*.elm", System.IO.SearchOption.AllDirectories);

                Log("Found " + elmModuleFiles.Length + " Elm module files in " + sourceDirectory);

                var elmJsonFiles =
                    System.IO.Directory.GetFiles(sourceDirectory, "elm.json", System.IO.SearchOption.AllDirectories);

                Log("Found " + elmJsonFiles.Length + " elm.json files in " + sourceDirectory);

                foreach (var filePath in elmJsonFiles.Concat(elmModuleFiles))
                {
                    try
                    {
                        var fileName = System.IO.Path.GetFileName(filePath);

                        var fileClock = System.Diagnostics.Stopwatch.StartNew();

                        var fileContent = System.IO.File.ReadAllText(filePath);

                        Log(
                            "Read file " + filePath + " with " +
                            CommandLineInterface.FormatIntegerForDisplay(fileContent.Length) +
                            " chars in " +
                            CommandLineInterface.FormatIntegerForDisplay((int)fileClock.Elapsed.TotalMilliseconds) + " ms");

                        fileClock.Restart();

                        if (System.Uri.TryCreate(filePath, System.UriKind.Absolute, out var uri) is false)
                        {
                            Log("Failed to create URI: " + filePath);
                            continue;
                        }

                        languageServiceState.AddFile(
                            uri.AbsoluteUri,
                            fileContent);

                        Log(
                            "Processed file " + filePath + " in language service in " +
                            CommandLineInterface.FormatIntegerForDisplay((int)fileClock.Elapsed.TotalMilliseconds) + " ms");

                        if (fileName.EndsWith(".elm", System.StringComparison.InvariantCultureIgnoreCase))
                        {
                            aggregateElmModuleFiles.Add(filePath);
                        }

                        if (string.Equals(fileName, "elm.json", System.StringComparison.InvariantCultureIgnoreCase))
                        {
                            aggregateElmJsonFiles.Add(filePath);

                            CollectDirectDependenciesFromElmJsonFile(fileContent);
                        }
                    }
                    catch (System.Exception e)
                    {
                        Log("Failed reading file: " + e);
                    }
                }
            }
            catch (System.Exception e)
            {
                Log("Failed reading directory: " + e);
            }
        }

        Log(
            "Finished initializing contents for " + aggregateElmModuleFiles.Count +
            " Elm modules and " +
            aggregateElmJsonFiles.Count +
            " elm.json files in " +
            CommandLineInterface.FormatIntegerForDisplay((int)aggregateClock.Elapsed.TotalMilliseconds) + " ms");

        LoadDirectDependenciesFromElmJsonFiles(languageServiceState);
    }

    public void Workspace_didChangeWorkspaceFolders(WorkspaceFoldersChangeEvent workspaceFoldersChangeEvent)
    {
        Log("Workspace_didChangeWorkspaceFolders (added " +
            workspaceFoldersChangeEvent.Added.Count + " and removed " + workspaceFoldersChangeEvent.Removed.Count + ")");

        IReadOnlyList<WorkspaceFolder> newWorkspaceFolders =
            [..workspaceFolders
            .Where(prevFolder => !workspaceFoldersChangeEvent.Removed.Any(removedFolder => removedFolder.Uri == prevFolder.Uri)),
            ..workspaceFoldersChangeEvent.Added
            ];

        Log("Workspace_didChangeWorkspaceFolders: new workspace folders count: " +
            newWorkspaceFolders.Count + " (" +
            string.Join(", ", newWorkspaceFolders.Select(wf => wf.Uri)));

        workspaceFolders = newWorkspaceFolders;

        System.Threading.Tasks.Task.Run(() => InitializeWorkspaceState(initializeParams));
    }

    public void TextDocument_didOpen(TextDocumentItem textDocument)
    {
        var decodedUri = DocumentUriCleaned(textDocument.Uri);

        Log("TextDocument_didOpen: " + decodedUri);

        clientTextDocumentContents[decodedUri] = textDocument.Text;

        allSeenDocumentUris[decodedUri] = decodedUri;
    }

    public void TextDocument_didChange(
        VersionedTextDocumentIdentifier textDocument,
        IReadOnlyList<TextDocumentContentChangeEvent> contentChanges)
    {
        var textDocumentUri = DocumentUriCleaned(textDocument.Uri);

        allSeenDocumentUris[textDocumentUri] = textDocumentUri;

        Log(
            "TextDocument_didChange: " + textDocumentUri +
            " (" + contentChanges.Count + " content changes)");

        for (var i = 0; i < contentChanges.Count; ++i)
        {
            var contentChange = contentChanges[i];

            Log("Content change " + i + ".range: " + contentChange.Range?.ToString() ?? "null");

            if (contentChange.Range is null)
            {
                clientTextDocumentContents[textDocumentUri] = contentChange.Text;

                var linesCount = ElmModule.ModuleLines(contentChange.Text).Count();

                Log(
                    "Replaced all of " + textDocumentUri + " with " +
                    CommandLineInterface.FormatIntegerForDisplay(contentChange.Text.Length) +
                    " chars distributed over " +
                    CommandLineInterface.FormatIntegerForDisplay(linesCount) + " lines");
            }
            else
            {
                // TODO

                Log("Failed to apply changes: not implemented");
            }
        }
    }

    public void TextDocument_didClose(TextDocumentIdentifier textDocument)
    {
        var decodedUri = DocumentUriCleaned(textDocument.Uri);

        clientTextDocumentContents.TryRemove(decodedUri, out var _);

        Log("TextDocument_didClose: " + decodedUri +
            " (" + clientTextDocumentContents.Count + " open remaining)");
    }

    public void Workspace_didChangeWatchedFiles(IReadOnlyList<FileEvent> changesBeforeDecode)
    {
        var changes =
            changesBeforeDecode
            .Select(change =>
                change
                with
                {
                    Uri = DocumentUriCleaned(change.Uri)
                })
            .ToList();

        Log(
            "Workspace_didChangeWatchedFiles: " + changes.Count + " changes: " +
            string.Join(", ", changes.Select(change => change.Uri)));

        if (this.languageServiceStateTask is not { } languageServiceStateTask)
        {
            Log("Error processing file changes: language service state not initialized");
            return;
        }

        {
            if (languageServiceStateTask.Result.IsErrOrNull() is { } err)
            {
                Log("Error processing file changes: language service state not initialized: " + err);
                return;
            }
        }

        if (languageServiceStateTask.Result.IsOkOrNull() is not { } languageServiceState)
        {
            throw new System.NotImplementedException(
                "Unexpected language service state result type: " + languageServiceStateTask.Result.GetType());
        }

        foreach (var change in changes)
        {
            var localPathResult = DocumentUriAsLocalPath(change.Uri);

            if (localPathResult.IsErrOrNull() is { } err)
            {
                Log("Ignoring URI: " + err + ": " + change.Uri);
                continue;
            }

            if (localPathResult.IsOkOrNull() is not { } localPath)
            {
                throw new System.NotImplementedException(
                    "Unexpected result type: " + localPathResult.GetType());
            }

            if (change.Type is FileChangeType.Deleted)
            {
                languageServiceState.DeleteFile(change.Uri);
                continue;
            }

            if (change.Type is not FileChangeType.Created &&
                change.Type is not FileChangeType.Changed)
            {
                Log("Ignoring file change: " + change.Type + ": " + change.Uri);
                continue;
            }

            try
            {
                var clock = System.Diagnostics.Stopwatch.StartNew();

                var fileContent = System.IO.File.ReadAllText(localPath);

                var fileName = System.IO.Path.GetFileName(localPath);

                Log(
                    "Read file " + change.Uri + " with " +
                    CommandLineInterface.FormatIntegerForDisplay(fileContent.Length) +
                    " chars in " +
                    CommandLineInterface.FormatIntegerForDisplay((int)clock.Elapsed.TotalMilliseconds) +
                    " ms");

                clock.Restart();

                languageServiceState.AddFile(
                    change.Uri,
                    fileContent);

                Log(
                    "Processed file " + change.Uri + " in language service in " +
                    CommandLineInterface.FormatIntegerForDisplay((int)clock.Elapsed.TotalMilliseconds) +
                    " ms");

                if (string.Equals(fileName, "elm.json", System.StringComparison.InvariantCultureIgnoreCase))
                {
                    CollectDirectDependenciesFromElmJsonFile(fileContent);
                }
            }
            catch (System.Exception e)
            {
                Log("Failed reading file: " + e);
            }
        }

        LoadDirectDependenciesFromElmJsonFiles(languageServiceState);
    }

    private void CollectDirectDependenciesFromElmJsonFile(string elmJson)
    {
        try
        {
            var elmJsonFileParsed =
                System.Text.Json.JsonSerializer.Deserialize<ElmJsonStructure>(elmJson);

            foreach (var (packageName, packageVersion) in elmJsonFileParsed?.Dependencies.Direct ?? [])
            {
                elmJsonDirectDependencies.TryGetValue(packageName, out var existingVersion);

                elmJsonDirectDependencies[packageName] = packageVersion;

                if (existingVersion is null)
                {
                    Log("Registered direct dependency: " + packageName + " " + packageVersion);
                }
                else if (existingVersion != packageVersion)
                {
                    Log("Updated direct dependency: " + packageName + " " + packageVersion);
                }
            }
        }
        catch (System.Exception e)
        {
            Log("Failed reading elm.json file: " + e);
        }
    }

    private void LoadDirectDependenciesFromElmJsonFiles(
        WorkspaceState workspaceState)
    {
        int? searchAndLoadPackage(string packageName, string version)
        {
            foreach (var searchDirectory in elmPackagesSearchDirectories)
            {
                var packageDirectory = System.IO.Path.Combine(searchDirectory, packageName);

                var versionDirectory = System.IO.Path.Combine(packageDirectory, version);

                if (!System.IO.Directory.Exists(versionDirectory))
                {
                    continue;
                }

                var elmModuleFiles =
                    System.IO.Directory.GetFiles(versionDirectory, "*.elm", System.IO.SearchOption.AllDirectories);

                var elmJsonFiles =
                    System.IO.Directory.GetFiles(versionDirectory, "elm.json", System.IO.SearchOption.AllDirectories);

                Log(
                    "Package: " + packageName + " version " + version +
                    ": Found " + elmModuleFiles.Length +
                    " Elm module files and " +
                    elmJsonFiles.Length +
                    " elm.json file(s) in " + versionDirectory);

                var exposedModuleNames = new HashSet<string>();

                foreach (var elmJsonFile in elmJsonFiles)
                {
                    try
                    {
                        var fileContent = System.IO.File.ReadAllText(elmJsonFile);

                        var elmJsonFileParsed =
                            System.Text.Json.JsonSerializer.Deserialize<ElmJsonStructure>(fileContent);

                        if (elmJsonFileParsed is null)
                        {
                            Log("Failed parsing elm.json file: " + elmJsonFile);
                            continue;
                        }

                        foreach (var exposedModuleName in elmJsonFileParsed.ExposedModules)
                        {
                            exposedModuleNames.Add(exposedModuleName);
                        }
                    }
                    catch (System.Exception e)
                    {
                        Log("Failed reading elm.json file: " + e);
                    }
                }

                var elmModuleNamesAdded = new HashSet<string>();

                foreach (var filePath in elmModuleFiles)
                {
                    try
                    {
                        var fileName = System.IO.Path.GetFileName(filePath);
                        var fileContent = System.IO.File.ReadAllText(filePath);

                        Log(
                            "Read file " + filePath + " with " +
                            CommandLineInterface.FormatIntegerForDisplay(fileContent.Length) +
                            " chars");

                        var parseModuleNameResult = ElmModule.ParseModuleName(fileContent);

                        if (parseModuleNameResult.IsErrOrNull() is { } err)
                        {
                            Log("Failed parsing module name: " + err);
                            continue;
                        }

                        if (parseModuleNameResult.IsOkOrNull() is not { } moduleName)
                        {
                            throw new System.NotImplementedException(
                                "Unexpected result type: " + parseModuleNameResult.GetType());
                        }

                        var moduleNameFlat = string.Join('.', moduleName);

                        if (0 < exposedModuleNames.Count && !exposedModuleNames.Contains(moduleNameFlat))
                        {
                            Log("Ignoring non-exposed module: " + moduleNameFlat);
                            continue;
                        }

                        if (System.Uri.TryCreate(filePath, System.UriKind.Absolute, out var uri) is false)
                        {
                            Log("Failed to create URI: " + filePath);
                            continue;
                        }

                        workspaceState.AddFile(
                            uri.AbsoluteUri,
                            fileContent);

                        elmModuleNamesAdded.Add(moduleNameFlat);
                    }
                    catch (System.Exception e)
                    {
                        Log("Failed reading file: " + e);
                    }
                }

                Log(
                    "Loaded package: " + packageName + " " + version +
                    ": Added " + elmModuleNamesAdded.Count + " exposed Elm modules");

                return elmModuleNamesAdded.Count;
            }

            return null;
        }

        foreach (var dependency in elmJsonDirectDependencies)
        {
            elmJsonDirectDependenciesLoaded.TryGetValue(dependency.Key, out var loadedVersion);

            if (loadedVersion == dependency.Value)
            {
                continue;
            }

            var clock = System.Diagnostics.Stopwatch.StartNew();

            if (searchAndLoadPackage(dependency.Key, dependency.Value) is { } packageElmModuleCount)
            {
                elmJsonDirectDependenciesLoaded[dependency.Key] = dependency.Value;

                Log(
                    "Loaded package: " + dependency.Key + " " + dependency.Value +
                    " in " + CommandLineInterface.FormatIntegerForDisplay((int)clock.Elapsed.TotalMilliseconds) + " ms");
            }
            else
            {
                Log("Did not find package: " + dependency.Key + " " + dependency.Value);
            }
        }
    }

    public IReadOnlyList<TextEdit> TextDocument_formatting(
        TextDocumentIdentifier textDocument,
        FormattingOptions options)
    {
        var textDocumentUri = DocumentUriCleaned(textDocument.Uri);

        Log("TextDocument_formatting: " + textDocumentUri);

        clientTextDocumentContents.TryGetValue(textDocumentUri, out var textDocumentContentBefore);

        var localPathResult = DocumentUriAsLocalPath(textDocumentUri);

        {
            if (localPathResult.IsErrOrNull() is { } err)
            {
                Log("Ignoring URI: " + err + ": " + textDocumentUri);
                return [];
            }
        }

        if (localPathResult.IsOkOrNull() is not { } localPath)
        {
            throw new System.NotImplementedException(
                "Unexpected result type: " + localPathResult.GetType());
        }

        if (textDocumentContentBefore is not null)
        {
            Log("Found document " + textDocumentUri + " in client-managed state");
        }
        else
        {
            try
            {
                textDocumentContentBefore ??= System.IO.File.ReadAllText(localPath);
            }
            catch (System.Exception e)
            {
                Log("Failed reading file: " + e);
            }
        }

        if (textDocumentContentBefore is null)
        {
            return [];
        }

        IReadOnlyList<string> linesBefore =
            [.. textDocumentContentBefore.ModuleLines()];

        static TextEdit replaceWholeDocument(string newContent)
        {
            return new TextEdit(
                Range: new Range(
                    Start: new Position(Line: 0, Character: 0),
                    End: new Position(Line: 999_999_999, Character: 999_999_999)),
                NewText: newContent);
        }

        Log(
            "Document " + textDocumentUri + " had " +
            CommandLineInterface.FormatIntegerForDisplay(linesBefore.Count) +
            " lines and " +
            CommandLineInterface.FormatIntegerForDisplay(textDocumentContentBefore.Length) +
            " chars before");

        var newContent =
            TextDocument_formatting_lessStore(
                textDocument,
                textDocumentContentBefore);

        if (newContent is null)
            return [];

        if (clientTextDocumentContents.ContainsKey(textDocumentUri))
        {
            clientTextDocumentContents[textDocumentUri] = newContent;
        }

        return
            [replaceWholeDocument(newContent)
            ];
    }

    public string? TextDocument_formatting_lessStore(
        TextDocumentIdentifier textDocument,
        string textDocumentContentBefore)
    {
        if (textDocument.Uri.EndsWith(".elm"))
        {
            try
            {
                var binaryClock = System.Diagnostics.Stopwatch.StartNew();

                var elmFormatted =
                    AVH4ElmFormatBinaries.RunElmFormat(textDocumentContentBefore);

                binaryClock.Stop();

                Log("Completed elm-format on " + textDocument.Uri + " in " +
                    CommandLineInterface.FormatIntegerForDisplay((int)binaryClock.Elapsed.TotalMilliseconds)
                    + " ms");

                return elmFormatted;
            }
            catch (System.Exception e)
            {
                Log("Error: Failed running elm-format: " + e);
            }
        }

        return null;
    }

    public Hover? TextDocument_hover(
        TextDocumentPositionParams positionParams)
    {
        var textDocumentUri = DocumentUriCleaned(positionParams.TextDocument.Uri);

        var clock = System.Diagnostics.Stopwatch.StartNew();

        Log("TextDocument_hover: " + textDocumentUri + " at " + positionParams.Position);

        var hoverStrings =
            ProvideHover(
                new Interface.ProvideHoverRequestStruct(
                    textDocumentUri,
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

        return new Hover
        (
            Contents: hoverStringsOk,
            Range: null
        );
    }

    public CompletionItem[] TextDocument_completion(
        TextDocumentPositionParams positionParams)
    {
        var textDocumentUri = DocumentUriCleaned(positionParams.TextDocument.Uri);

        var clock = System.Diagnostics.Stopwatch.StartNew();

        Log("TextDocument_completion: " + textDocumentUri + " at " + positionParams.Position);

        var completionItems =
            ProvideCompletionItems(
                new Interface.ProvideCompletionItemsRequestStruct(
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
            [..completionItemsOk
            .Select(monacoCompletionItem =>
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

    public IReadOnlyList<Location> TextDocument_definition(
        TextDocumentPositionParams positionParams)
    {
        var textDocumentUri = DocumentUriCleaned(positionParams.TextDocument.Uri);

        var clock = System.Diagnostics.Stopwatch.StartNew();

        Log("TextDocument_definition: " + textDocumentUri + " at " + positionParams.Position);

        var provideDefinitionResult =
            ProvideDefinition(
                new Interface.ProvideHoverRequestStruct(
                    textDocumentUri,
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
                filePath =>
                {
                    Log("No corresponding URI for " + string.Join('/', filePath));

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

    public IReadOnlyList<DocumentSymbol> TextDocument_documentSymbol(
        TextDocumentIdentifier textDocument)
    {
        var textDocumentUri = DocumentUriCleaned(textDocument.Uri);

        var clock = System.Diagnostics.Stopwatch.StartNew();

        Log("textDocument/documentSymbol: " + textDocumentUri);

        var localPathResult = DocumentUriAsLocalPath(textDocumentUri);

        {
            if (localPathResult.IsErrOrNull() is { } err)
            {
                Log("Ignoring URI: " + err + ": " + textDocumentUri);
                return [];
            }
        }

        if (localPathResult.IsOkOrNull() is not { } localPath)
        {
            throw new System.NotImplementedException(
                "Unexpected result type: " + localPathResult.GetType());
        }

        var fileContent = System.IO.File.ReadAllText(localPath);

        Log(
            "Read file " + textDocumentUri + " with " +
            CommandLineInterface.FormatIntegerForDisplay(fileContent.Length) +
            " chars in " +
            CommandLineInterface.FormatIntegerForDisplay((int)clock.Elapsed.TotalMilliseconds) +
            " ms");

        clock.Restart();

        if (languageServiceStateTask.Result.IsOkOrNull() is not { } languageServiceState)
        {
            throw new System.NotImplementedException(
                "Unexpected language service state result type: " + languageServiceStateTask.Result.GetType());
        }

        languageServiceState.AddFile(
            textDocumentUri,
            fileContent);

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

        static SymbolKind mapSymbolKind(Interface.SymbolKind symbolKind)
        {
            return symbolKind switch
            {
                Interface.SymbolKind.File => SymbolKind.File,
                Interface.SymbolKind.Module => SymbolKind.Module,
                Interface.SymbolKind.Namespace => SymbolKind.Namespace,
                Interface.SymbolKind.Package => SymbolKind.Package,
                Interface.SymbolKind.Class => SymbolKind.Class,
                Interface.SymbolKind.Enum => SymbolKind.Enum,
                Interface.SymbolKind.Interface => SymbolKind.Interface,
                Interface.SymbolKind.Function => SymbolKind.Function,
                Interface.SymbolKind.Constant => SymbolKind.Constant,
                Interface.SymbolKind.String => SymbolKind.String,
                Interface.SymbolKind.Number => SymbolKind.Number,
                Interface.SymbolKind.Boolean => SymbolKind.Boolean,
                Interface.SymbolKind.Array => SymbolKind.Array,
                Interface.SymbolKind.EnumMember => SymbolKind.EnumMember,
                Interface.SymbolKind.Struct => SymbolKind.Struct,

                _ =>
                throw new System.NotImplementedException("Unexpected symbol kind: " + symbolKind)
            };
        }

        static DocumentSymbol mapDocumentSymbol(Interface.DocumentSymbolStruct documentSymbol)
        {
            return new DocumentSymbol(
                Name: documentSymbol.Name,
                Detail: null,
                Kind: mapSymbolKind(documentSymbol.Kind),
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
                [..documentSymbol.Children
                    .Select(cn => mapDocumentSymbol(cn.Struct))]);
        }

        return
            [..documentSymbolsOk
            .Select(ds => mapDocumentSymbol(ds.Struct))];
    }

    public IReadOnlyList<Location> TextDocument_references(
        TextDocumentPositionParams positionParams)
    {
        var textDocumentUri = DocumentUriCleaned(positionParams.TextDocument.Uri);

        var clock = System.Diagnostics.Stopwatch.StartNew();

        Log("TextDocument_references: " + textDocumentUri + " at " + positionParams.Position);

        var provideReferenceResult =
            TextDocumentReferencesRequest(
                new Interface.ProvideHoverRequestStruct(
                    textDocumentUri,
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
                filePath =>
                {
                    Log("No corresponding URI for " + string.Join('/', filePath));
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

    public Result<string, WorkspaceEdit?> TextDocument_rename(
        RenameParams renameParams)
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
            .Select(documentEdit =>
            {
                var editsInDocument =
                    documentEdit.Edits
                    .Select(edit =>
                        new TextEdit(
                            Range: new Range(
                                Start: new Position(
                                    Line: (uint)edit.Range.StartLineNumber - 1,
                                    Character: (uint)edit.Range.StartColumn - 1),
                                End: new Position(
                                    Line: (uint)edit.Range.EndLineNumber - 1,
                                    Character: (uint)edit.Range.EndColumn - 1)),
                            NewText: edit.NewText));

                return
                new TextDocumentEdit(
                    new OptionalVersionedTextDocumentIdentifier(documentEdit.FilePath, Version: null),
                    [.. editsInDocument]);
            })
            .ToImmutableArray();

        return new WorkspaceEdit(documentChanges);
    }

    public void TextDocument_didSave(
        DidSaveTextDocumentParams didSaveParams,
        System.Action<TextDocumentIdentifier, IReadOnlyList<Diagnostic>> publishDiagnostics)
    {
        var textDocumentUri = DocumentUriCleaned(didSaveParams.TextDocument.Uri);

        allSeenDocumentUris[textDocumentUri] = textDocumentUri;

        Log("TextDocument_didSave: " + textDocumentUri);

        if (didSaveParams.Text is { } text)
        {
            clientTextDocumentContents[textDocumentUri] = text;
        }

        var localPathResult = DocumentUriAsLocalPath(textDocumentUri);
        {
            if (localPathResult.IsErrOrNull() is { } err)
            {
                Log("Ignoring URI: " + err + ": " + textDocumentUri);
                return;
            }
        }

        if (localPathResult.IsOkOrNull() is not { } localPath)
        {
            throw new System.NotImplementedException(
                "Unexpected result type: " + localPathResult.GetType());
        }

        var elmJsonFilePath =
            FindElmJsonFile(localPath) ?? initializeParams?.RootPath;

        if (elmJsonFilePath is null)
        {
            Log("Failed to find elm.json file for " + textDocumentUri);
            return;
        }

        var workingDirectoryAbsolute =
            System.IO.Path.GetDirectoryName(elmJsonFilePath);

        if (workingDirectoryAbsolute is null)
        {
            Log("Failed to get elm.json directory for " + textDocumentUri);
            return;
        }

        var clock = System.Diagnostics.Stopwatch.StartNew();

        Log("Begin elm make for " + textDocumentUri);

        var elmMakeOutput =
            ElmMakeRunner.ElmMakeAsync(
                workingDirectoryAbsolute: workingDirectoryAbsolute,
                pathToFileWithElmEntryPoint: localPath)
            .Result;

        Log("Completed elm make for " + textDocumentUri + " in " +
            CommandLineInterface.FormatIntegerForDisplay(clock.ElapsedMilliseconds) + " ms");

        Log("elm make exit code: " + elmMakeOutput.ExitCode);
        Log("elm make output: " + elmMakeOutput.StandardOutput);
        Log("elm make error: " + elmMakeOutput.StandardError);

        if (elmMakeOutput.ExitCode is 0)
        {
            publishDiagnostics(
                new TextDocumentIdentifier(didSaveParams.TextDocument.Uri),
                []);

            return;
        }

        var parseReportResult =
            ParseReportFromElmMake(elmMakeOutput.StandardError);

        {
            if (parseReportResult.IsErrOrNull() is { } err)
            {
                Log("Failed to parse elm make report: " + err);
                return;
            }
        }

        if (parseReportResult.IsOkOrNull() is not { } parseReportOk)
        {
            throw new System.NotImplementedException(
                "Unexpected result type: " + parseReportResult.GetType());
        }

        if (parseReportOk is ElmMakeReport.ElmMakeReportError generalError)
        {
            Log("Elm make general error: " + generalError.Message);

            return;
        }

        if (parseReportOk is ElmMakeReport.ElmMakeReportCompileErrors compileErrors)
        {
            var errorsByPath =
                compileErrors.Errors
                .GroupBy(error => error.Path)
                .ToDictionary(group => group.Key, group => group.ToList());

            foreach (var (path, errors) in errorsByPath)
            {
                var pathErrors =
                    errors
                    .SelectMany(error => error.Problems)
                    .ToImmutableArray();

                Log("Elm make errors for " + path + ": " + pathErrors.Length);

                IReadOnlyList<Diagnostic> diagnostics =
                    [..pathErrors
                    .Select(problem =>
                        new Diagnostic(
                            Range: new Range(
                                Start: new Position(
                                    Line: (uint)problem.Region.Start.Line - 1,
                                    Character: (uint)problem.Region.Start.Column - 1),
                                End: new Position(
                                    Line: (uint)problem.Region.End.Line - 1,
                                    Character: (uint)problem.Region.End.Column - 1)),
                            Severity: DiagnosticSeverity.Error,
                            Code: null,
                            Source: "elm make",
                            Message: string.Join("", problem.Message.Select(MessageItemToString)),
                            CodeDescription: null,
                            Tags: null,
                            RelatedInformation: null))
                    ];

                publishDiagnostics(
                    new TextDocumentIdentifier(didSaveParams.TextDocument.Uri),
                    diagnostics);
            }
        }
    }

    public static string MessageItemToString(MessageItem messageItem)
    {
        if (messageItem is MessageItem.StringMessage stringMessage)
        {
            return stringMessage.Value;
        }

        if (messageItem is MessageItem.StyledMessage styledMessage)
        {
            return styledMessage.String;
        }

        throw new System.NotImplementedException(
            "Unexpected message item type: " + messageItem.GetType());
    }

    public static Result<string, ElmMakeReport> ParseReportFromElmMake(string elmMakeOutput)
    {
        try
        {
            var elmMakeReport =
                ElmMakeReportConverter.Deserialize(elmMakeOutput);

            return elmMakeReport;
        }
        catch (System.Exception e)
        {
            return "Failed to parse elm make report: " + e;
        }
    }

    public static string? FindElmJsonFile(string elmModuleFilePath)
    {
        var directoryName = System.IO.Path.GetDirectoryName(elmModuleFilePath);

        while (directoryName is not null)
        {
            var elmJsonFilePath = System.IO.Path.Combine(directoryName, "elm.json");

            if (System.IO.File.Exists(elmJsonFilePath))
            {
                return elmJsonFilePath;
            }

            directoryName = System.IO.Path.GetDirectoryName(directoryName);
        }

        return null;
    }

    public static TreeNodeWithStringPath MergeIntoFileTree(
        TreeNodeWithStringPath seed,
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
                            new TreeNodeWithStringPath.BlobNode(nextFile.Value));
                });
    }

    public Result<string, IReadOnlyList<string>> ProvideHover(
        Interface.ProvideHoverRequestStruct provideHoverRequest)
    {
        var genericRequestResult =
            HandleRequest(
                new Interface.Request.ProvideHoverRequest(provideHoverRequest));

        if (genericRequestResult.IsErrOrNull() is { } err)
        {
            return err;
        }

        if (genericRequestResult.IsOkOrNull() is not { } requestOk)
        {
            throw new System.NotImplementedException(
                "Unexpected request result type: " + genericRequestResult.GetType());
        }

        if (requestOk is not Interface.Response.ProvideHoverResponse provideHoverResponse)
        {
            throw new System.NotImplementedException(
                "Unexpected request result type: " + requestOk.GetType());
        }

        return Result<string, IReadOnlyList<string>>.ok(provideHoverResponse.Strings);
    }

    public Result<string, IReadOnlyList<MonacoEditor.MonacoCompletionItem>>
        ProvideCompletionItems(
            Interface.ProvideCompletionItemsRequestStruct provideCompletionItemsRequest)
    {
        var genericRequestResult =
            HandleRequest(
                new Interface.Request.ProvideCompletionItemsRequest(provideCompletionItemsRequest));

        if (genericRequestResult.IsErrOrNull() is { } err)
        {
            return err;
        }

        if (genericRequestResult.IsOkOrNull() is not { } requestOk)
        {
            throw new System.NotImplementedException(
                "Unexpected request result type: " + genericRequestResult.GetType());
        }

        if (requestOk is not Interface.Response.ProvideCompletionItemsResponse provideCompletionItemsResponse)
        {
            throw new System.NotImplementedException(
                "Unexpected request result type: " + requestOk.GetType());
        }

        return Result<string, IReadOnlyList<MonacoEditor.MonacoCompletionItem>>.ok(
            provideCompletionItemsResponse.CompletionItems);
    }

    public Result<string, IReadOnlyList<Interface.LocationUnderFilePath>>
        ProvideDefinition(
            Interface.ProvideHoverRequestStruct provideDefinitionRequest)
    {
        var genericRequestResult =
            HandleRequest(
                new Interface.Request.ProvideDefinitionRequest(provideDefinitionRequest));

        if (genericRequestResult.IsErrOrNull() is { } err)
        {
            return err;
        }

        if (genericRequestResult.IsOkOrNull() is not { } requestOk)
        {
            throw new System.NotImplementedException(
                "Unexpected request result type: " + genericRequestResult.GetType());
        }

        if (requestOk is not Interface.Response.ProvideDefinitionResponse provideDefinitionResponse)
        {
            throw new System.NotImplementedException(
                "Unexpected request result type: " + requestOk.GetType());
        }

        return
            Result<string, IReadOnlyList<Interface.LocationUnderFilePath>>.ok(
                provideDefinitionResponse.Locations);
    }

    public Result<string, IReadOnlyList<Interface.DocumentSymbol>> TextDocumentSymbolRequest(
        string fileUri)
    {
        var genericRequestResult =
            HandleRequest(
                new Interface.Request.TextDocumentSymbolRequest(fileUri));

        if (genericRequestResult.IsErrOrNull() is { } err)
        {
            return err;
        }

        if (genericRequestResult.IsOkOrNull() is not { } requestOk)
        {
            throw new System.NotImplementedException(
                "Unexpected request result type: " + genericRequestResult.GetType());
        }

        if (requestOk is not Interface.Response.TextDocumentSymbolResponse documentSymbolResponse)
        {
            throw new System.NotImplementedException(
                "Unexpected request result type: " + requestOk.GetType());
        }

        return
            Result<string, IReadOnlyList<Interface.DocumentSymbol>>.ok(
                documentSymbolResponse.Symbols);
    }

    public Result<string, IReadOnlyList<Interface.LocationUnderFilePath>> TextDocumentReferencesRequest(
        Interface.ProvideHoverRequestStruct referenceRequest)
    {
        var genericRequestResult =
            HandleRequest(
                new Interface.Request.TextDocumentReferencesRequest(referenceRequest));

        if (genericRequestResult.IsErrOrNull() is { } err)
        {
            return err;
        }

        if (genericRequestResult.IsOkOrNull() is not { } requestOk)
        {
            throw new System.NotImplementedException(
                "Unexpected request result type: " + genericRequestResult.GetType());
        }

        if (requestOk is not Interface.Response.TextDocumentReferencesResponse provideReferenceResponse)
        {
            throw new System.NotImplementedException(
                "Unexpected request result type: " + requestOk.GetType());
        }

        return Result<string, IReadOnlyList<Interface.LocationUnderFilePath>>.ok(
            provideReferenceResponse.Locations);
    }

    public Result<string, Interface.WorkspaceEdit> TextDocumentRenameRequest(
        Interface.RenameParams renameParams)
    {
        var genericRequestResult =
            HandleRequest(
                new Interface.Request.TextDocumentRenameRequest(renameParams));

        if (genericRequestResult.IsErrOrNull() is { } err)
        {
            return err;
        }

        if (genericRequestResult.IsOkOrNull() is not { } requestOk)
        {
            throw new System.NotImplementedException(
                "Unexpected request result type: " + genericRequestResult.GetType());
        }

        if (requestOk is not Interface.Response.TextDocumentRenameResponse renameResponse)
        {
            throw new System.NotImplementedException(
                "Unexpected request result type: " + requestOk.GetType());
        }

        return Result<string, Interface.WorkspaceEdit>.ok(
            renameResponse.WorkspaceEdit);
    }

    public Result<string, Interface.Response> HandleRequest(
        Interface.Request request)
    {
        if (this.languageServiceStateTask is not { } languageServiceStateTask)
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

    public IEnumerable<Location> MapLocations(
        IEnumerable<Interface.LocationUnderFilePath> locations,
        System.Func<IReadOnlyList<string>, IEnumerable<Location>> noMatchingUri)
    {
        return
            locations
            .Select(location =>
            {
                return
                    new Location(
                        location.FilePath,
                        new Range(
                            Start: new Position(
                                Line: (uint)location.Range.StartLineNumber - 1,
                                Character: (uint)location.Range.StartColumn - 1),
                            End: new Position(
                                Line: (uint)location.Range.EndLineNumber - 1,
                                Character: (uint)location.Range.EndColumn - 1)));
            });
    }

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

    public static Result<string, string> DocumentUriAsLocalPath(string documentUri)
    {
        /*
         * The client in VSCode appears to send document URIs like this:
         * file:///k%3A/Source/Repos/
         * Therefore we need to decode before handing to System.Uri
         * */
        if (System.Uri.TryCreate(
            System.Uri.UnescapeDataString(documentUri), System.UriKind.Absolute, out var uriAbsolute))
        {
            if (uriAbsolute.Scheme is not "file")
            {
                return Result<string, string>.err("non-file URI");
            }

            return Result<string, string>.ok(uriAbsolute.LocalPath);
        }

        return Result<string, string>.err("Not an absolute URI");
    }
}
