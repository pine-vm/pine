using Pine.Core.LanguageServerProtocol;
using StreamJsonRpc;
using System.Collections.Generic;

namespace Pine.Elm;

public record LanguageServerRpcTarget(LanguageServer Server)
{
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
        return Server.Initialize(initializeParams);
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
    /// https://microsoft.github.io/language-server-protocol/specifications/lsp/3.18/specification/#textDocument_formatting
    /// </summary>
    [JsonRpcMethod("textDocument/formatting")]
    public IReadOnlyList<TextEdit> TextDocument_formatting(
        TextDocumentIdentifier textDocument,
        FormattingOptions options)
    {
        return Server.TextDocument_formatting(textDocument, options);
    }
}
