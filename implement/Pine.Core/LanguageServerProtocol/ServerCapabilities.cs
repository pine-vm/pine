namespace Pine.Core.LanguageServerProtocol;

/// <summary>
/// https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#serverCapabilities
/// </summary>
public record ServerCapabilities(
    bool? DocumentFormattingProvider = null,
    TextDocumentSyncKind? TextDocumentSync = null);

/// <summary>
/// https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocumentSyncKind
/// </summary>
public enum TextDocumentSyncKind
{
    /// <summary>
    /// Documents should not be synced at all.
    /// </summary>
    None = 0,

    /// <summary>
    /// Documents are synced by always sending the full content of the document.
    /// </summary>
    Full = 1,

    /// <summary>
    /// Documents are synced by sending the full content on open.
    /// After that only incremental updates to the document are sent.
    /// </summary>
    Incremental = 2
}
