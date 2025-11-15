using Interface = Pine.Elm.LanguageServiceInterface;
using Pine.Core;
using Pine.Core.Elm.ElmSyntax;
using Pine.Core.Elm.Elm019;
using Pine.Core.LanguageServerProtocol;
using Pine.Elm019;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.Linq;
using System.Collections.Immutable;
using Pine.Core.Files;

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

    private System.Threading.Tasks.Task<Result<string, WorkspaceState>>? _languageServiceStateTask;

    /*
     * TODO: Use the version identifier from elm.json as scope.
     * */
    private readonly ConcurrentDictionary<Interface.ElmPackageVersion019Identifer, string> elmJsonDirectDependencies = new();

    private readonly ConcurrentDictionary<Interface.ElmPackageVersion019Identifer, string> elmJsonDirectDependenciesLoaded = new();

    private class WorkspaceState(
        IReadOnlyList<WorkspaceFolder> workspaceFolders,
        LanguageServiceState languageServiceState)
    {
        public static Result<string, WorkspaceState> Init(
            IReadOnlyList<WorkspaceFolder> workspaceFolders)
        {
            var initServiceStateResult =
                LanguageServiceState.InitLanguageServiceState();

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

        public Result<string, Interface.Response.WorkspaceSummaryResponse>
            AddElmPackage(
            Interface.ElmPackageVersion019Identifer packageVersionId,
            IReadOnlyList<KeyValuePair<IReadOnlyList<string>, string>> filesContentsAsText)
        {
            return languageServiceState.AddElmPackage(packageVersionId, filesContentsAsText);
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
        IEnumerable<string> ComposeDirectories()
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

        _languageServiceStateTask = System.Threading.Tasks.Task.Run(() =>
        {
            var initResult = WorkspaceState.Init(workspaceFolders);

            if (initResult.IsErrOrNull() is { } err)
            {
                Log("Failed initializing language service state: " + err);
            }

            return initResult;
        });

        var taskResult = _languageServiceStateTask.Result;

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

        IReadOnlyList<string> sourceDirectories = [.. ComposeDirectories().Distinct()];

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

        if (_languageServiceStateTask is not { } languageServiceStateTask)
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
                        new Interface.ElmPackageVersion019Identifer(
                            PackageName: packageName,
                            VersionTag: packageVersion);

                    if (elmJsonDirectDependencies.ContainsKey(packageVersionId))
                    {
                        continue;
                    }

                    elmJsonDirectDependencies[packageVersionId] = packageVersion;

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
        WorkspaceState workspaceState)
    {
        int? searchAndLoadPackage(Interface.ElmPackageVersion019Identifer packageVersionIdentifer)
        {
            var packageName = packageVersionIdentifer.PackageName;
            var packageVersion = packageVersionIdentifer.VersionTag;

            foreach (var searchDirectory in elmPackagesSearchDirectories)
            {
                var packageDirectory = System.IO.Path.Combine(searchDirectory, packageName);

                var packageVersionDirectory =
                    System.IO.Path.Combine(packageDirectory, packageVersion)
                    .Replace('\\', '/');

                if (!System.IO.Directory.Exists(packageVersionDirectory))
                {
                    continue;
                }

                var elmModuleFiles =
                    System.IO.Directory.GetFiles(packageVersionDirectory, "*.elm", System.IO.SearchOption.AllDirectories);

                var elmJsonFiles =
                    System.IO.Directory.GetFiles(packageVersionDirectory, "elm.json", System.IO.SearchOption.AllDirectories);

                Log(
                    "Package: " + packageName + " version " + packageVersion +
                    ": Found " + elmModuleFiles.Length +
                    " Elm module files and " +
                    elmJsonFiles.Length +
                    " elm.json file(s) in " + packageVersionDirectory);

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

                var elmModulesToAdd =
                    new Dictionary<IReadOnlyList<string>, string>(
                        comparer: EnumerableExtensions.EqualityComparer<IReadOnlyList<string>>());

                foreach (var filePath in elmModuleFiles)
                {
                    try
                    {
                        var fileName = System.IO.Path.GetFileName(filePath);
                        var fileContent = System.IO.File.ReadAllText(filePath);

                        var filePathRelative =
                            filePath[(packageVersionDirectory.Length + 1)..]
                            .Replace('\\', '/');

                        var filePathRelativeItems = filePathRelative.Split('/');

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

                        elmModulesToAdd.TryAdd(filePathRelativeItems, fileContent);
                    }
                    catch (System.Exception e)
                    {
                        Log("Failed reading file: " + e);
                    }
                }

                Log(
                    "Package: " + packageName + " version " + packageVersion +
                    ": Found " + elmModulesToAdd.Count + " Elm modules to add");

                var clock = System.Diagnostics.Stopwatch.StartNew();

                var addPackageResponse =
                    workspaceState.AddElmPackage(
                        packageVersionIdentifer,
                        [.. elmModulesToAdd]);

                {
                    if (addPackageResponse.IsErrOrNull() is { } err)
                    {
                        Log("Failed adding package: " + err);
                        return null;
                    }
                }

                Log(
                    "Loaded package: " + packageName + " " + packageVersion +
                    ": Added " + elmModulesToAdd.Count + " exposed Elm modules in " +
                    CommandLineInterface.FormatIntegerForDisplay((int)clock.Elapsed.TotalMilliseconds) + " ms");

                elmJsonDirectDependenciesLoaded[packageVersionIdentifer] = packageVersionDirectory;

                return elmModulesToAdd.Count;
            }

            return null;
        }

        foreach (var dependency in elmJsonDirectDependencies)
        {
            if (elmJsonDirectDependenciesLoaded.ContainsKey(dependency.Key))
            {
                continue;
            }

            var clock = System.Diagnostics.Stopwatch.StartNew();

            if (searchAndLoadPackage(dependency.Key) is { } packageElmModuleCount)
            {
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
        {
            Log("Exiting because new content for document " + textDocumentUri + " is null");
            return [];
        }

        if (clientTextDocumentContents.ContainsKey(textDocumentUri))
        {
            clientTextDocumentContents[textDocumentUri] = newContent;
        }

        var textEdits =
            ComputeTextEditsForDocumentFormat(textDocumentContentBefore, newContent);

        Log(
            "Formatting document " + textDocumentUri + ": Computed " +
            textEdits.Count + " text edits with " +
            textEdits.Sum(te => te.NewText.Length) + " aggregate chars replaced or added");

        return textEdits;
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

        {
            if (_languageServiceStateTask?.Result.IsErrOrNull() is { } err)
            {
                throw new System.NotImplementedException(
                    "Failed initializing language service: " + err);
            }
        }

        if (_languageServiceStateTask?.Result.IsOkOrNull() is not { } languageServiceState)
        {
            throw new System.NotImplementedException(
                "Unexpected language service state result type: " + _languageServiceStateTask?.Result.GetType());
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

        static SymbolKind MapSymbolKind(Interface.SymbolKind symbolKind)
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

        static DocumentSymbol MapDocumentSymbol(Interface.DocumentSymbolStruct documentSymbol)
        {
            return new DocumentSymbol(
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
                [..documentSymbol.Children
                    .Select(cn => MapDocumentSymbol(cn.Struct))]);
        }

        return
            [..documentSymbolsOk
            .Select(ds => MapDocumentSymbol(ds.Struct))];
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

    public Result<string, IReadOnlyList<Interface.LocationInFile>>
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
            Result<string, IReadOnlyList<Interface.LocationInFile>>.ok(
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

    public Result<string, IReadOnlyList<Interface.LocationInFile>> TextDocumentReferencesRequest(
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

        return Result<string, IReadOnlyList<Interface.LocationInFile>>.ok(
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

    public IEnumerable<Location> MapLocations(
        IEnumerable<Interface.LocationInFile> locations,
        System.Func<Interface.FileLocation, IEnumerable<Location>> noMatchingUri)
    {
        return
            locations
            .SelectMany(location =>
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

    public Interface.FileLocation InterfaceFileLocationFromUri(string documentUri)
    {
        var documentUriNormalized = DocumentUriCleaned(documentUri);

        foreach (var (elmPackageVersionIdentifer, packageDirectory) in elmJsonDirectDependenciesLoaded)
        {
            var packageDirectoryNormalized = DocumentUriCleaned(packageDirectory);

            if (documentUriNormalized.StartsWith(packageDirectoryNormalized))
            {
                var modulePathFlat = documentUriNormalized[packageDirectoryNormalized.Length..];

                var modulePathItems = modulePathFlat.Split('/');

                return new Interface.FileLocation.ElmPackageFileLocation(
                    elmPackageVersionIdentifer,
                    ModulePath: modulePathItems);
            }
        }

        return new Interface.FileLocation.WorkspaceFileLocation(documentUriNormalized);
    }

    public string? FindMatchingUri(Interface.FileLocation fileLocation)
    {
        if (fileLocation is Interface.FileLocation.WorkspaceFileLocation workspaceFileLocation)
        {
            return workspaceFileLocation.FilePath;
        }

        if (fileLocation is Interface.FileLocation.ElmPackageFileLocation elmPackageFileLocation)
        {
            if (elmJsonDirectDependenciesLoaded.TryGetValue(
                elmPackageFileLocation.ElmPackageVersionIdentifer,
                out var packageDirectory))
            {
                var filePath =
                    System.IO.Path.Combine([packageDirectory, .. elmPackageFileLocation.ModulePath])
                    .Replace('\\', '/');

                if (System.Uri.TryCreate(filePath, System.UriKind.Absolute, out var uri) is false)
                {
                    Log("Failed to create URI: " + filePath);
                    return null;
                }

                return uri.AbsoluteUri;
            }
        }

        return null;
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

    /// <summary>
    /// Apply a list of text edits following the specification from
    /// https://microsoft.github.io/language-server-protocol/specifications/lsp/3.18/specification/#textEditArray
    /// </summary>
    public static string ApplyTextEdits(string originalText, IReadOnlyList<TextEdit> edits)
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
    public static IReadOnlyList<TextEdit> ComputeTextEditsForDocumentFormat(
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
            ? originalLines.Count
            : newLines.Count;

        while (
            commonPrefixLength < minLength &&
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
            ? remainingOriginal
            : remainingNew;

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

            var range = new Range(
                Start: new Position(Line: (uint)lastExistingLine, Character: (uint)lastExistingLineLength),
                End: new Position(Line: (uint)lastExistingLine, Character: (uint)lastExistingLineLength));

            var replacementText = "\n" + string.Join("\n", replacementLines);

            return [new TextEdit(Range: range, NewText: replacementText)];
        }

        if (newLines.Count < originalLines.Count && firstChangedLine >= newLines.Count)
        {
            // Deletion from the end - delete extra lines from original
            var lastKeptLine = newLines.Count - 1;
            var lastKeptLineLength = lastKeptLine >= 0 ? originalLines[lastKeptLine].Length : 0;
            var lastDeletedLine = originalLines.Count - 1;
            var lastDeletedLineLength = originalLines[lastDeletedLine].Length;

            var range = new Range(
                Start: new Position(Line: (uint)lastKeptLine, Character: (uint)lastKeptLineLength),
                End: new Position(Line: (uint)lastDeletedLine, Character: (uint)lastDeletedLineLength));

            return [new TextEdit(Range: range, NewText: "")];
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

                var range = new Range(
                    Start: new Position(Line: (uint)insertLine, Character: (uint)insertChar),
                    End: new Position(Line: (uint)insertLine, Character: (uint)insertChar));

                var replacementText = "\n" + string.Join("\n", replacementLines);

                return [new TextEdit(Range: range, NewText: replacementText)];
            }
            else if (firstChangedLine == 0 && replacementLines.Count > 0)
            {
                // Insert at the beginning
                var range = new Range(
                    Start: new Position(Line: 0, Character: 0),
                    End: new Position(Line: 0, Character: 0));

                var replacementText = string.Join("\n", replacementLines) + "\n";

                return [new TextEdit(Range: range, NewText: replacementText)];
            }

            // If no replacement lines, this might be a degenerate case - return no edits
            return [];
        }

        if (firstChangedLine < originalLines.Count && newLines.Count < originalLines.Count && replacementLines.Count is 0)
        {
            // Pure deletion in the middle - we need to delete some lines without replacement
            // The range should include the newline that creates the line to be deleted
            var startLine = firstChangedLine > 0 ? firstChangedLine - 1 : 0;
            var startChar = firstChangedLine > 0 ? originalLines[startLine].Length : 0;
            var endLine = lastChangedLineInOriginal;
            var endChar = originalLines[endLine].Length;

            var range = new Range(
                Start: new Position(Line: (uint)startLine, Character: (uint)startChar),
                End: new Position(Line: (uint)endLine, Character: (uint)endChar));

            var replacementText = string.Join("\n", replacementLines);

            return [new TextEdit(Range: range, NewText: replacementText)];
        }

        {
            // Normal replacement case
            var range = new Range(
                Start: new Position(Line: (uint)firstChangedLine, Character: 0),
                End: new Position(Line: (uint)lastChangedLineInOriginal, Character: (uint)originalLines[lastChangedLineInOriginal].Length));

            var replacementText = string.Join("\n", replacementLines);

            return [new TextEdit(Range: range, NewText: replacementText)];
        }
    }
}
