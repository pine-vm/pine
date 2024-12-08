using ElmTime.ElmSyntax;
using Interface = Pine.Elm.LanguageServiceInterface;
using Pine.Core;
using Pine.Core.LanguageServerProtocol;
using Pine.Core.PineVM;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.Linq;

namespace Pine.Elm;

public class LanguageServer(
    System.Action<string>? logDelegate,
    IPineVM pineVM)
{
    private readonly ConcurrentDictionary<string, string> clientTextDocumentContents = new();

    private IReadOnlyList<WorkspaceFolder> workspaceFolders = [];

    private InitializeParams? initializeParams;

    private readonly System.Action<string>? logDelegate = logDelegate;

    private System.Threading.Tasks.Task<Result<string, WorkspaceState>>? languageServiceStateTask;

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
            IReadOnlyList<string> filePath,
            string fileContent,
            IPineVM pineVM)
        {
            return languageServiceState.AddFile(filePath, fileContent, pineVM);
        }

        public Result<string, Interface.Response.WorkspaceSummaryResponse> DeleteFile(
            IReadOnlyList<string> filePath,
            IPineVM pineVM)
        {
            return languageServiceState.DeleteFile(filePath, pineVM);
        }

        public Result<string, Interface.Response> HandleRequest(
            Interface.Request request,
            IPineVM pineVM)
        {
            return languageServiceState.HandleRequest(request, pineVM);
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
                TextDocumentSync = TextDocumentSyncKind.Full,

                DocumentFormattingProvider = true,
                HoverProvider = true,

                CompletionProvider =
                new CompletionOptions(
                    TriggerCharacters: [".", " "],
                    AllCommitCharacters: null,
                    ResolveProvider: null),

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

        IReadOnlyList<string> sourceDirectories = [.. composeDirectories().Distinct()];

        Log("Starting to initialize files contents for " + sourceDirectories.Count + " directories");

        languageServiceStateTask = System.Threading.Tasks.Task.Run(() =>
        {
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

        var filesContents = new Dictionary<string, string>();

        foreach (var sourceDirectory in sourceDirectories)
        {
            try
            {
                var elmFiles =
                    System.IO.Directory.GetFiles(sourceDirectory, "*.elm", System.IO.SearchOption.AllDirectories);

                Log("Found " + elmFiles.Length + " Elm files in " + sourceDirectory);

                foreach (var elmFile in elmFiles)
                {
                    try
                    {
                        var clock = System.Diagnostics.Stopwatch.StartNew();

                        var fileContent = System.IO.File.ReadAllText(elmFile);

                        filesContents[elmFile] = fileContent;

                        Log(
                            "Read file " + elmFile + " with " +
                            CommandLineInterface.FormatIntegerForDisplay(fileContent.Length) +
                            " chars in " +
                            CommandLineInterface.FormatIntegerForDisplay((int)clock.Elapsed.TotalMilliseconds) + " ms");

                        clock.Restart();

                        languageServiceState.AddFile(
                            PathItemsFromFlatPath(elmFile),
                            fileContent,
                            pineVM);

                        Log(
                            "Processed file " + elmFile + " in language service in " +
                            CommandLineInterface.FormatIntegerForDisplay((int)clock.Elapsed.TotalMilliseconds) + " ms");
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

        Log("Finished initializing contents for " + filesContents.Count + " files");
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
        var decodedUri = System.Uri.UnescapeDataString(textDocument.Uri);

        Log("TextDocument_didOpen: " + decodedUri);

        clientTextDocumentContents[decodedUri] = textDocument.Text;
    }

    public void TextDocument_didChange(
        VersionedTextDocumentIdentifier textDocument,
        IReadOnlyList<TextDocumentContentChangeEvent> contentChanges)
    {
        var textDocumentUri = System.Uri.UnescapeDataString(textDocument.Uri);

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
        var decodedUri = System.Uri.UnescapeDataString(textDocument.Uri);

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
                    Uri = System.Uri.UnescapeDataString(change.Uri)
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
                languageServiceState.DeleteFile(
                    PathItemsFromFlatPath(localPath),
                    pineVM);
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

                Log(
                    "Read file " + change.Uri + " with " +
                    CommandLineInterface.FormatIntegerForDisplay(fileContent.Length) +
                    " chars in " +
                    CommandLineInterface.FormatIntegerForDisplay((int)clock.Elapsed.TotalMilliseconds) +
                    " ms");

                clock.Restart();

                languageServiceState.AddFile(
                    PathItemsFromFlatPath(localPath),
                    fileContent,
                    pineVM);

                Log(
                    "Processed file " + change.Uri + " in language service in " +
                    CommandLineInterface.FormatIntegerForDisplay((int)clock.Elapsed.TotalMilliseconds) +
                    " ms");
            }
            catch (System.Exception e)
            {
                Log("Failed reading file: " + e);
            }
        }
    }

    public IReadOnlyList<TextEdit> TextDocument_formatting(
        TextDocumentIdentifier textDocument,
        FormattingOptions options)
    {
        var textDocumentUri = System.Uri.UnescapeDataString(textDocument.Uri);

        Log("TextDocument_formatting: " + textDocumentUri);

        clientTextDocumentContents.TryGetValue(textDocumentUri, out var textDocumentContentBefore);

        if (textDocumentContentBefore is not null)
        {
            Log("Found document " + textDocumentUri + " in client-managed state");
        }
        else
        {
            try
            {
                textDocumentContentBefore ??= System.IO.File.ReadAllText(textDocumentUri);
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
        var textDocumentUri = System.Uri.UnescapeDataString(positionParams.TextDocument.Uri);

        var clock = System.Diagnostics.Stopwatch.StartNew();

        Log("TextDocument_hover: " + textDocumentUri + " at " + positionParams.Position);

        var localPathResult = DocumentUriAsLocalPath(textDocumentUri);

        {
            if (localPathResult.IsErrOrNull() is { } err)
            {
                Log("Ignoring URI: " + err + ": " + textDocumentUri);

                return null;
            }
        }

        if (localPathResult.IsOkOrNull() is not { } localPath)
        {
            throw new System.NotImplementedException(
                "Unexpected result type: " + localPathResult.GetType());
        }

        var filePathOpenedInEditor = PathItemsFromFlatPath(localPath);

        var hoverStrings =
            ProvideHover(
                new Interface.ProvideHoverRequestStruct(
                    filePathOpenedInEditor,
                    /*
                     * The language service currently uses the 1-based line and column numbers
                     * inherited from the Monaco editor API.
                     * */
                    (int)positionParams.Position.Line + 1,
                    (int)positionParams.Position.Character + 1),
                pineVM);

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
        var textDocumentUri = System.Uri.UnescapeDataString(positionParams.TextDocument.Uri);

        var clock = System.Diagnostics.Stopwatch.StartNew();

        Log("TextDocument_completion: " + textDocumentUri + " at " + positionParams.Position);

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

        var filePathOpenedInEditor = PathItemsFromFlatPath(localPath);

        var completionItems =
            ProvideCompletionItems(
                new Interface.ProvideCompletionItemsRequestStruct(
                    filePathOpenedInEditor,
                    /*
                     * The language service currently uses the 1-based line and column numbers
                     * inherited from the Monaco editor API.
                     * */
                    CursorLineNumber:
                    (int)positionParams.Position.Line + 1,
                    CursorColumn:
                    (int)positionParams.Position.Character + 1),
                pineVM);

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

    static IReadOnlyList<string> PathItemsFromFlatPath(string path)
    {
        return path.Split(['\\', '/']);
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
        Interface.ProvideHoverRequestStruct provideHoverRequest,
        IPineVM pineVM)
    {
        var genericRequestResult =
            HandleRequest(
                new Interface.Request.ProvideHoverRequest(provideHoverRequest),
                pineVM);

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
            Interface.ProvideCompletionItemsRequestStruct provideCompletionItemsRequest,
            IPineVM pineVM)
    {
        var genericRequestResult =
            HandleRequest(
                new Interface.Request.ProvideCompletionItemsRequest(provideCompletionItemsRequest),
                pineVM);

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

    public Result<string, Interface.Response> HandleRequest(
        Interface.Request request,
        IPineVM pineVM)
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
            languageServiceState.HandleRequest(request, pineVM);
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
