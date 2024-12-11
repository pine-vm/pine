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
                Task task = Task.Delay(TimeSpan.FromSeconds(1)).ContinueWith(_ =>
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
}
