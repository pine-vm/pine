using Pine.Core;
using Pine.Core.LanguageServerProtocol;
using StreamJsonRpc;
using System;
using System.Collections.Generic;
using System.Threading.Tasks;

namespace Pine.Elm;

public record LanguageServerRpcTarget(
    LanguageServer Server,
    Action<string>? LogDelegate)
{
    public JsonRpc? JsonRpc { get; set; } = null;

    private bool shutdown;

    private void Log(string message)
    {
        LogDelegate?.Invoke(message);
    }

    public static IJsonRpcMessageFormatter JsonRpcMessageFormatterDefault() =>
        new SystemTextJsonFormatter()
        {
            JsonSerializerOptions = new System.Text.Json.JsonSerializerOptions
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

    /// <summary>
    /// https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#initialize
    /// </summary>
    [JsonRpcMethod("initialize", UseSingleObjectParameterDeserialization = true)]
    public InitializeResult Initialize(InitializeParams initializeParams)
    {
        var (response, requests) = Server.Initialize(initializeParams);

        if (requests.Count > 0)
        {
            if (JsonRpc is not { } jsonRpc)
            {
                Log("Failed dynamic registration on Initialize: sendRequest is null");
            }
            else
            {
                var task = Task.Delay(TimeSpan.FromSeconds(1)).ContinueWith(_ =>
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

        return response;
    }

    /// <summary>
    /// https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#workspace_didChangeWorkspaceFolders
    /// </summary>
    [JsonRpcMethod("workspace/didChangeWorkspaceFolders", UseSingleObjectParameterDeserialization = true)]
    public void Workspace_didChangeWorkspaceFolders(WorkspaceFoldersChangeEvent workspaceFoldersChangeEvent)
    {
        Server.Workspace_didChangeWorkspaceFolders(workspaceFoldersChangeEvent);
    }

    /// <summary>
    /// https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_didOpen
    /// </summary>
    [JsonRpcMethod("textDocument/didOpen")]
    public void TextDocument_didOpen(TextDocumentItem textDocument)
    {
        Server.TextDocument_didOpen(textDocument);
    }

    /// <summary>
    /// https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_didChange
    /// </summary>
    [JsonRpcMethod("textDocument/didChange")]
    public void TextDocument_didChange(
        VersionedTextDocumentIdentifier textDocument,
        IReadOnlyList<TextDocumentContentChangeEvent> contentChanges)
    {
        Server.TextDocument_didChange(textDocument, contentChanges);
    }

    /// <summary>
    /// https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_didClose
    /// </summary>
    [JsonRpcMethod("textDocument/didClose")]
    public void TextDocument_didClose(TextDocumentIdentifier textDocument)
    {
        Server.TextDocument_didClose(textDocument);
    }

    /// <summary>
    /// https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#workspace_didChangeWatchedFiles
    /// </summary>
    [JsonRpcMethod("workspace/didChangeWatchedFiles", UseSingleObjectParameterDeserialization = false)]
    public void Workspace_didChangeWatchedFiles(IReadOnlyList<FileEvent> changes)
    {
        Server.Workspace_didChangeWatchedFiles(changes);
    }

    /// <summary>
    /// https://microsoft.github.io/language-server-protocol/specifications/lsp/3.18/specification/#textDocument_formatting
    /// </summary>
    [JsonRpcMethod("textDocument/formatting")]
    public IReadOnlyList<TextEdit> TextDocument_formatting(
        TextDocumentIdentifier textDocument,
        FormattingOptions options)
    {
        return Server.TextDocument_formatting(textDocument, options);
    }

    /// <summary>
    /// https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_hover
    /// </summary>
    [JsonRpcMethod("textDocument/hover", UseSingleObjectParameterDeserialization = true)]
    public Hover? TextDocument_hover(
        TextDocumentPositionParams positionParams)
    {
        return Server.TextDocument_hover(positionParams);
    }

    /// <summary>
    /// https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_completion
    /// </summary>
    [JsonRpcMethod("textDocument/completion", UseSingleObjectParameterDeserialization = true)]
    public CompletionItem[] TextDocument_completion(
        TextDocumentPositionParams positionParams)
    {
        return Server.TextDocument_completion(positionParams);
    }

    /// <summary>
    /// https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_definition
    /// </summary>
    [JsonRpcMethod("textDocument/definition", UseSingleObjectParameterDeserialization = true)]
    public IReadOnlyList<Location> TextDocument_definition(
        TextDocumentPositionParams positionParams)
    {
        return Server.TextDocument_definition(positionParams);
    }

    /// <summary>
    /// https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_documentSymbol
    /// </summary>
    [JsonRpcMethod("textDocument/documentSymbol")]
    public IReadOnlyList<DocumentSymbol> TextDocument_documentSymbol(TextDocumentIdentifier textDocument)
    {
        var documentSymbols = Server.TextDocument_documentSymbol(textDocument);

        return documentSymbols;
    }

    /// <summary>
    /// https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_references
    /// </summary>
    [JsonRpcMethod("textDocument/references", UseSingleObjectParameterDeserialization = true)]
    public IReadOnlyList<Location> TextDocument_references(
        TextDocumentPositionParams referenceParams)
    {
        return Server.TextDocument_references(referenceParams);
    }

    [JsonRpcMethod("textDocument/rename", UseSingleObjectParameterDeserialization = true)]
    public WorkspaceEdit? TextDocument_rename(RenameParams renameParams)
    {
        var renameResult = Server.TextDocument_rename(renameParams);

        if (renameResult.IsErrOrNull() is { } err)
        {
            Log($"Rename failed: {err}");
            return null;
        }

        if (renameResult is not Result<string, WorkspaceEdit?>.Ok workspaceEditOk)
        {
            throw new InvalidOperationException(
                "Unexpected result type: " + renameResult.GetType().FullName);
        }

        return workspaceEditOk.Value;
    }

    /// <summary>
    /// https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_didSave
    /// </summary>
    [JsonRpcMethod("textDocument/didSave", UseSingleObjectParameterDeserialization = true)]
    public void TextDocument_didSave(DidSaveTextDocumentParams didSaveParams)
    {
        Server.TextDocument_didSave(
            didSaveParams,
            publishDiagnostics:
            (documentId, diagnostics) => PublishDiagnosticsAsync(documentId.Uri, diagnostics));
    }

    public async Task PublishDiagnosticsAsync(string documentUri, IReadOnlyList<Diagnostic> diagnostics)
    {
        if (JsonRpc is not { } jsonRpc)
        {
            Log("Failed to publish diagnostics: JsonRpc is null");
            return;
        }

        var parameters = new PublishDiagnosticsParams
        (
            Uri: documentUri,
            Diagnostics: diagnostics,
            Version: null
        );

        Log($"Publishing {diagnostics.Count} diagnostics for {documentUri}");

        await jsonRpc.NotifyAsync("textDocument/publishDiagnostics", parameters);
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
