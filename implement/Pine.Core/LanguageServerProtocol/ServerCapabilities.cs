using System.Collections.Generic;

namespace Pine.Core.LanguageServerProtocol;

/// <summary>
/// https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#serverCapabilities
/// </summary>
public record ServerCapabilities(
    bool? DocumentFormattingProvider = null,
    TextDocumentSyncOptions? TextDocumentSync = null,
    ServerCapabilitiesWorkspace? Workspace = null,
    bool? HoverProvider = null,
    CompletionOptions? CompletionProvider = null);

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

/// <summary>
/// https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#serverCapabilities
/// </summary>
public record ServerCapabilitiesWorkspace(
    WorkspaceFoldersServerCapabilities? WorkspaceFolders = null);

/// <summary>
/// https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#workspaceFoldersServerCapabilities
/// </summary>
public record WorkspaceFoldersServerCapabilities(
    bool? Supported,
    bool? ChangeNotifications);

/// <summary>
/// https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#completionOptions
/// </summary>
public record CompletionOptions(
    IReadOnlyList<string>? TriggerCharacters,
    IReadOnlyList<string>? AllCommitCharacters,
    bool? ResolveProvider);


/// <summary>
/// https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocumentSyncOptions
/// </summary>
public record TextDocumentSyncOptions(
    TextDocumentSyncKind? Change,
    bool? WillSave,
    bool? WillSaveWaitUntil,
    SaveOptions? Save);

/// <summary>
/// https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#saveOptions
/// </summary>
public record SaveOptions(
    bool? IncludeText);

