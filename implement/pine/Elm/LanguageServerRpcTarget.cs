using Pine.Core;
using Pine.Core.LanguageServerProtocol;
using Pine.LanguageServer;
using StreamJsonRpc;
using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Threading;
using System.Threading.Tasks;

namespace Pine.Elm;

public record LanguageServerRpcTarget(
    Core.Elm.LanguageServer.LanguageServer Server,
    Action<string>? LogDelegate)
{
    private JsonRpc? _jsonRpc;

    private long _rpcCallSequence;

    /// <summary>
    /// Connection used to send notifications and requests to the client.
    /// Setting this also connects the diagnostics channel of the language server.
    /// </summary>
    public JsonRpc? JsonRpc
    {
        get => _jsonRpc;

        set
        {
            _jsonRpc = value;

            Server.SetDiagnosticsPublisher(
                value is null ? null : PublishDiagnostics);
        }
    }

    private bool shutdown;

    private void Log(string message)
    {
        LogDelegate?.Invoke(message);
    }

    public static IJsonRpcMessageFormatter JsonRpcMessageFormatterDefault(Action<string>? logDelegate = null)
    {
        var inner =
            new SystemTextJsonFormatter()
            {
                JsonSerializerOptions =
                new System.Text.Json.JsonSerializerOptions
                {
                    PropertyNamingPolicy = System.Text.Json.JsonNamingPolicy.CamelCase,

                    /*
                     * 2024-12-17: Sending null instead of omitting the property caused the VSCode client to fail parsing
                     * responses to `textDocument/documentSymbol` with errors like this:
                     * ----
                        [Error - 7:59:39 PM] Request textDocument/documentSymbol failed.
                        TypeError: Cannot read properties of undefined (reading 'range')
                            at asSymbolInformation (c:\Users\winfail\.vscode\extensions\pine.pine-0.2.1\client\node_modules\vscode-languageclient\lib\common\protocolConverter.js:591:33)

                    The null value confused the client parsing into thinking the entry was a `SymbolInformation` instead of a `DocumentSymbol`.
                     * */
                    DefaultIgnoreCondition = System.Text.Json.Serialization.JsonIgnoreCondition.WhenWritingNull,
                }
            };

        if (logDelegate is null)
        {
            return inner;
        }

        return new DelegatingJsonRpcMessageFormatter(inner, logDelegate);
    }

    /// <summary>
    /// https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#initialize
    /// </summary>
    [JsonRpcMethod("initialize", UseSingleObjectParameterDeserialization = true)]
    public InitializeResult Initialize(InitializeParams initializeParams)
    {
        var callSequence = Interlocked.Increment(ref _rpcCallSequence);
        var clock = Stopwatch.StartNew();

        Log($"RPC method initialize {callSequence} invoked");

        var (response, requests) = Server.Initialize(initializeParams);

        if (requests.Count > 0)
        {
            if (JsonRpc is not { } jsonRpc)
            {
                Log("Failed dynamic registration on Initialize: sendRequest is null");
            }
            else
            {
                var task =
                    Task.Delay(TimeSpan.FromSeconds(1)).ContinueWith(
                        _ =>
                        {
                            Log("Sending requests on initialize after delay");

                            foreach (var request in requests)
                            {
                                Log($"Sending request on initialize: {request.Key}");

                                jsonRpc.InvokeWithParameterObjectAsync(request.Key, request.Value);
                            }
                        });
            }
        }

        Log($"RPC method initialize {callSequence} completed in {clock.ElapsedMilliseconds} ms");

        return response;
    }

    /// <summary>
    /// https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#workspace_didChangeWorkspaceFolders
    /// </summary>
    [JsonRpcMethod("workspace/didChangeWorkspaceFolders", UseSingleObjectParameterDeserialization = true)]
    public void Workspace_didChangeWorkspaceFolders(WorkspaceFoldersChangeEvent workspaceFoldersChangeEvent)
    {
        var callSequence = Interlocked.Increment(ref _rpcCallSequence);
        var clock = Stopwatch.StartNew();

        Log($"RPC method workspace/didChangeWorkspaceFolders {callSequence} invoked");

        Server.Workspace_didChangeWorkspaceFolders(workspaceFoldersChangeEvent);

        Log($"RPC method workspace/didChangeWorkspaceFolders {callSequence} completed in {clock.ElapsedMilliseconds} ms");
    }

    /// <summary>
    /// https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_didOpen
    /// </summary>
    [JsonRpcMethod("textDocument/didOpen")]
    public async Task TextDocument_didOpen(TextDocumentItem textDocument)
    {
        var callSequence = Interlocked.Increment(ref _rpcCallSequence);
        var clock = Stopwatch.StartNew();

        Log($"RPC method textDocument/didOpen {callSequence} invoked for {textDocument.Uri} version {textDocument.Version}");

        var task = Server.TextDocument_didOpenAsync(textDocument);

        Log($"RPC method textDocument/didOpen {callSequence} yielded ingress lane");

        try
        {
            await task;
            Log($"RPC method textDocument/didOpen {callSequence} completed in {clock.ElapsedMilliseconds} ms");
        }
        catch (Exception ex)
        {
            Log($"RPC method textDocument/didOpen {callSequence} failed after {clock.ElapsedMilliseconds} ms: {ex}");
            throw;
        }
    }

    /// <summary>
    /// https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_didChange
    /// </summary>
    [JsonRpcMethod("textDocument/didChange")]
    public async Task TextDocument_didChange(
        VersionedTextDocumentIdentifier textDocument,
        IReadOnlyList<TextDocumentContentChangeEvent> contentChanges)
    {
        var callSequence = Interlocked.Increment(ref _rpcCallSequence);
        var clock = Stopwatch.StartNew();

        Log($"RPC method textDocument/didChange {callSequence} invoked for {textDocument.Uri} version {textDocument.Version}");

        var task = Server.TextDocument_didChangeAsync(textDocument, contentChanges);

        Log($"RPC method textDocument/didChange {callSequence} yielded ingress lane");

        try
        {
            await task;
            Log($"RPC method textDocument/didChange {callSequence} completed in {clock.ElapsedMilliseconds} ms");
        }
        catch (Exception ex)
        {
            Log($"RPC method textDocument/didChange {callSequence} failed after {clock.ElapsedMilliseconds} ms: {ex}");
            throw;
        }
    }

    /// <summary>
    /// https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_didClose
    /// </summary>
    [JsonRpcMethod("textDocument/didClose")]
    public async Task TextDocument_didClose(TextDocumentIdentifier textDocument)
    {
        var callSequence = Interlocked.Increment(ref _rpcCallSequence);
        var clock = Stopwatch.StartNew();

        Log($"RPC method textDocument/didClose {callSequence} invoked for {textDocument.Uri}");

        var task = Server.TextDocument_didCloseAsync(textDocument);

        Log($"RPC method textDocument/didClose {callSequence} yielded ingress lane");

        try
        {
            await task;
            Log($"RPC method textDocument/didClose {callSequence} completed in {clock.ElapsedMilliseconds} ms");
        }
        catch (Exception ex)
        {
            Log($"RPC method textDocument/didClose {callSequence} failed after {clock.ElapsedMilliseconds} ms: {ex}");
            throw;
        }
    }

    /// <summary>
    /// https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#workspace_didChangeWatchedFiles
    /// </summary>
    [JsonRpcMethod("workspace/didChangeWatchedFiles", UseSingleObjectParameterDeserialization = false)]
    public async Task Workspace_didChangeWatchedFiles(IReadOnlyList<FileEvent> changes)
    {
        var callSequence = Interlocked.Increment(ref _rpcCallSequence);
        var clock = Stopwatch.StartNew();

        Log($"RPC method workspace/didChangeWatchedFiles {callSequence} invoked for {changes.Count} changes");

        var task = Server.Workspace_didChangeWatchedFilesAsync(changes);

        Log($"RPC method workspace/didChangeWatchedFiles {callSequence} yielded ingress lane");

        try
        {
            await task;
            Log($"RPC method workspace/didChangeWatchedFiles {callSequence} completed in {clock.ElapsedMilliseconds} ms");
        }
        catch (Exception ex)
        {
            Log($"RPC method workspace/didChangeWatchedFiles {callSequence} failed after {clock.ElapsedMilliseconds} ms: {ex}");
            throw;
        }
    }

    /// <summary>
    /// https://microsoft.github.io/language-server-protocol/specifications/lsp/3.18/specification/#textDocument_formatting
    /// </summary>
    [JsonRpcMethod("textDocument/formatting")]
    public async Task<IReadOnlyList<TextEdit>> TextDocument_formatting(
        TextDocumentIdentifier textDocument,
        FormattingOptions options,
        CancellationToken cancellationToken)
    {
        var callSequence = Interlocked.Increment(ref _rpcCallSequence);
        var clock = Stopwatch.StartNew();

        Log($"RPC method textDocument/formatting {callSequence} invoked for {textDocument.Uri}");

        var task =
            Server.TextDocument_formattingAsync(
                textDocument,
                options,
                cancellationToken);

        Log($"RPC method textDocument/formatting {callSequence} yielded ingress lane");

        try
        {
            var result = await task;
            Log($"RPC method textDocument/formatting {callSequence} completed in {clock.ElapsedMilliseconds} ms, returning {result.Count} edits");
            return result;
        }
        catch (OperationCanceledException) when (cancellationToken.IsCancellationRequested)
        {
            Log($"RPC method textDocument/formatting {callSequence} canceled after {clock.ElapsedMilliseconds} ms");
            throw;
        }
        catch (Exception ex)
        {
            Log($"RPC method textDocument/formatting {callSequence} failed after {clock.ElapsedMilliseconds} ms: {ex}");
            throw;
        }
    }

    /// <summary>
    /// https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_hover
    /// </summary>
    [JsonRpcMethod("textDocument/hover", UseSingleObjectParameterDeserialization = true)]
    public async Task<Hover?> TextDocument_hover(
        TextDocumentPositionParams positionParams,
        CancellationToken cancellationToken)
    {
        var callSequence = Interlocked.Increment(ref _rpcCallSequence);
        var clock = Stopwatch.StartNew();

        Log($"RPC method textDocument/hover {callSequence} invoked for {positionParams.TextDocument.Uri} at {positionParams.Position}");

        Log($"RPC method textDocument/hover {callSequence} yielding ingress lane");
        await Task.Yield();
        Log($"RPC method textDocument/hover {callSequence} resumed after yield");

        try
        {
            var result = Server.TextDocument_hover(positionParams, cancellationToken);
            Log($"RPC method textDocument/hover {callSequence} completed in {clock.ElapsedMilliseconds} ms");
            return result;
        }
        catch (OperationCanceledException) when (cancellationToken.IsCancellationRequested)
        {
            Log($"RPC method textDocument/hover {callSequence} canceled after {clock.ElapsedMilliseconds} ms");
            throw;
        }
        catch (Exception ex)
        {
            Log($"RPC method textDocument/hover {callSequence} failed after {clock.ElapsedMilliseconds} ms: {ex}");
            throw;
        }
    }

    /// <summary>
    /// https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_completion
    /// </summary>
    [JsonRpcMethod("textDocument/completion", UseSingleObjectParameterDeserialization = true)]
    public async Task<CompletionItem[]> TextDocument_completion(
        TextDocumentPositionParams positionParams,
        CancellationToken cancellationToken)
    {
        var callSequence = Interlocked.Increment(ref _rpcCallSequence);
        var clock = Stopwatch.StartNew();

        Log($"RPC method textDocument/completion {callSequence} invoked for {positionParams.TextDocument.Uri} at {positionParams.Position}");

        Log($"RPC method textDocument/completion {callSequence} yielding ingress lane");
        await Task.Yield();
        Log($"RPC method textDocument/completion {callSequence} resumed after yield");

        try
        {
            var result = Server.TextDocument_completion(positionParams, cancellationToken);
            Log($"RPC method textDocument/completion {callSequence} completed in {clock.ElapsedMilliseconds} ms, returning {result.Length} items");
            return result;
        }
        catch (OperationCanceledException) when (cancellationToken.IsCancellationRequested)
        {
            Log($"RPC method textDocument/completion {callSequence} canceled after {clock.ElapsedMilliseconds} ms");
            throw;
        }
        catch (Exception ex)
        {
            Log($"RPC method textDocument/completion {callSequence} failed after {clock.ElapsedMilliseconds} ms: {ex}");
            throw;
        }
    }

    /// <summary>
    /// https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_definition
    /// </summary>
    [JsonRpcMethod("textDocument/definition", UseSingleObjectParameterDeserialization = true)]
    public async Task<IReadOnlyList<Location>> TextDocument_definition(
        TextDocumentPositionParams positionParams,
        CancellationToken cancellationToken)
    {
        var callSequence = Interlocked.Increment(ref _rpcCallSequence);
        var clock = Stopwatch.StartNew();

        Log($"RPC method textDocument/definition {callSequence} invoked for {positionParams.TextDocument.Uri} at {positionParams.Position}");

        Log($"RPC method textDocument/definition {callSequence} yielding ingress lane");
        await Task.Yield();
        Log($"RPC method textDocument/definition {callSequence} resumed after yield");

        try
        {
            var result = Server.TextDocument_definition(positionParams, cancellationToken);
            Log($"RPC method textDocument/definition {callSequence} completed in {clock.ElapsedMilliseconds} ms, returning {result.Count} locations");
            return result;
        }
        catch (OperationCanceledException) when (cancellationToken.IsCancellationRequested)
        {
            Log($"RPC method textDocument/definition {callSequence} canceled after {clock.ElapsedMilliseconds} ms");
            throw;
        }
        catch (Exception ex)
        {
            Log($"RPC method textDocument/definition {callSequence} failed after {clock.ElapsedMilliseconds} ms: {ex}");
            throw;
        }
    }

    /// <summary>
    /// https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_documentSymbol
    /// </summary>
    [JsonRpcMethod("textDocument/documentSymbol")]
    public async Task<IReadOnlyList<DocumentSymbol>> TextDocument_documentSymbol(
        TextDocumentIdentifier textDocument,
        CancellationToken cancellationToken)
    {
        var callSequence = Interlocked.Increment(ref _rpcCallSequence);
        var clock = Stopwatch.StartNew();

        Log($"RPC method textDocument/documentSymbol {callSequence} invoked for {textDocument.Uri}");

        Log($"RPC method textDocument/documentSymbol {callSequence} yielding ingress lane");
        await Task.Yield();
        Log($"RPC method textDocument/documentSymbol {callSequence} resumed after yield");

        try
        {
            var result = Server.TextDocument_documentSymbol(textDocument, cancellationToken);
            Log($"RPC method textDocument/documentSymbol {callSequence} completed in {clock.ElapsedMilliseconds} ms, returning {result.Count} symbols");
            return result;
        }
        catch (OperationCanceledException) when (cancellationToken.IsCancellationRequested)
        {
            Log($"RPC method textDocument/documentSymbol {callSequence} canceled after {clock.ElapsedMilliseconds} ms");
            throw;
        }
        catch (Exception ex)
        {
            Log($"RPC method textDocument/documentSymbol {callSequence} failed after {clock.ElapsedMilliseconds} ms: {ex}");
            throw;
        }
    }

    /// <summary>
    /// https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_references
    /// </summary>
    [JsonRpcMethod("textDocument/references", UseSingleObjectParameterDeserialization = true)]
    public async Task<IReadOnlyList<Location>> TextDocument_references(
        TextDocumentPositionParams referenceParams,
        CancellationToken cancellationToken)
    {
        var callSequence = Interlocked.Increment(ref _rpcCallSequence);
        var clock = Stopwatch.StartNew();

        Log($"RPC method textDocument/references {callSequence} invoked for {referenceParams.TextDocument.Uri} at {referenceParams.Position}");

        Log($"RPC method textDocument/references {callSequence} yielding ingress lane");
        await Task.Yield();
        Log($"RPC method textDocument/references {callSequence} resumed after yield");

        try
        {
            var result = Server.TextDocument_references(referenceParams, cancellationToken);
            Log($"RPC method textDocument/references {callSequence} completed in {clock.ElapsedMilliseconds} ms, returning {result.Count} locations");
            return result;
        }
        catch (OperationCanceledException) when (cancellationToken.IsCancellationRequested)
        {
            Log($"RPC method textDocument/references {callSequence} canceled after {clock.ElapsedMilliseconds} ms");
            throw;
        }
        catch (Exception ex)
        {
            Log($"RPC method textDocument/references {callSequence} failed after {clock.ElapsedMilliseconds} ms: {ex}");
            throw;
        }
    }

    [JsonRpcMethod("textDocument/rename", UseSingleObjectParameterDeserialization = true)]
    public async Task<WorkspaceEdit?> TextDocument_rename(
        RenameParams renameParams,
        CancellationToken cancellationToken)
    {
        var callSequence = Interlocked.Increment(ref _rpcCallSequence);
        var clock = Stopwatch.StartNew();

        Log($"RPC method textDocument/rename {callSequence} invoked for {renameParams.TextDocument.Uri} at {renameParams.Position}");

        Log($"RPC method textDocument/rename {callSequence} yielding ingress lane");
        await Task.Yield();
        Log($"RPC method textDocument/rename {callSequence} resumed after yield");

        try
        {
            var renameResult = Server.TextDocument_rename(renameParams, cancellationToken);

            if (renameResult.IsErrOrNull() is { } err)
            {
                Log($"Rename failed: {err}");
                Log($"RPC method textDocument/rename {callSequence} completed in {clock.ElapsedMilliseconds} ms with failure");
                return null;
            }

            if (renameResult is not Result<string, WorkspaceEdit?>.Ok workspaceEditOk)
            {
                throw new InvalidOperationException(
                    "Unexpected result type: " + renameResult.GetType().FullName);
            }

            Log($"RPC method textDocument/rename {callSequence} completed in {clock.ElapsedMilliseconds} ms");
            return workspaceEditOk.Value;
        }
        catch (OperationCanceledException) when (cancellationToken.IsCancellationRequested)
        {
            Log($"RPC method textDocument/rename {callSequence} canceled after {clock.ElapsedMilliseconds} ms");
            throw;
        }
        catch (Exception ex)
        {
            Log($"RPC method textDocument/rename {callSequence} failed after {clock.ElapsedMilliseconds} ms: {ex}");
            throw;
        }
    }

    /// <summary>
    /// https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_didSave
    /// </summary>
    [JsonRpcMethod("textDocument/didSave", UseSingleObjectParameterDeserialization = true)]
    public async Task TextDocument_didSave(DidSaveTextDocumentParams didSaveParams)
    {
        var callSequence = Interlocked.Increment(ref _rpcCallSequence);
        var clock = Stopwatch.StartNew();

        Log($"RPC method textDocument/didSave {callSequence} invoked for {didSaveParams.TextDocument.Uri}");

        var task = Server.TextDocument_didSaveAsync(didSaveParams);

        Log($"RPC method textDocument/didSave {callSequence} yielded ingress lane");

        try
        {
            await task;
            Log($"RPC method textDocument/didSave {callSequence} completed in {clock.ElapsedMilliseconds} ms");
        }
        catch (Exception ex)
        {
            Log($"RPC method textDocument/didSave {callSequence} failed after {clock.ElapsedMilliseconds} ms: {ex}");
            throw;
        }
    }

    /// <summary>
    /// Sends a <c>textDocument/publishDiagnostics</c> notification to the client.
    /// </summary>
    public Task PublishDiagnosticsAsync(string documentUri, IReadOnlyList<Diagnostic> diagnostics) =>
        PublishDiagnosticsAsync(
            new PublishDiagnosticsParams(
                Uri: documentUri,
                Diagnostics: diagnostics,
                Version: null));

    /// <summary>
    /// Sends a <c>textDocument/publishDiagnostics</c> notification to the client.
    /// </summary>
    public async Task PublishDiagnosticsAsync(PublishDiagnosticsParams parameters)
    {
        if (JsonRpc is not { } jsonRpc)
        {
            Log("Failed to publish diagnostics: JsonRpc is null");
            return;
        }

        Log($"Publishing {parameters.Diagnostics.Count} diagnostics for {parameters.Uri}");

        await jsonRpc.NotifyAsync("textDocument/publishDiagnostics", parameters);
    }

    private void PublishDiagnostics(PublishDiagnosticsParams parameters)
    {
        _ =
            PublishDiagnosticsAsync(parameters)
            .ContinueWith(
                task =>
                {
                    if (task.Exception is { } exception)
                    {
                        Log("Failed publishing diagnostics: " + exception.Message);
                    }
                },
                TaskScheduler.Default);
    }

    /// <summary>
    /// https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#shutdown
    /// </summary>
    [JsonRpcMethod("shutdown")]
    public void Shutdown()
    {
        shutdown = true;

        Log("Shutdown");
    }

    /// <summary>
    /// https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#exit
    /// </summary>
    [JsonRpcMethod("exit")]
    public void Exit()
    {
        if (shutdown)
        {
            Log("Exiting");

            Environment.Exit(0);
        }
        else
        {
            Log("Shutdown not called before exit");

            Environment.Exit(1);
        }
    }
}
