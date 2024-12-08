namespace Pine.Core.LanguageServerProtocol;

/// <summary>
/// https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#clientCapabilities
/// </summary>
public record ClientCapabilities(
    ClientCapabilitiesWorkspace? Workspace,
    TextDocumentClientCapabilities? TextDocument);

/// <summary>
/// https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#clientCapabilities
/// </summary>
public record ClientCapabilitiesWorkspace(
    DidChangeWatchedFilesClientCapabilities? DidChangeWatchedFiles,
    bool? WorkspaceFolders);

/// <summary>
/// https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#didChangeWatchedFilesClientCapabilities
/// </summary>
public record DidChangeWatchedFilesClientCapabilities(
    bool? DynamicRegistration,
    bool? RelativePatternSupport);

/// <summary>
/// https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocumentClientCapabilities
/// </summary>
public record TextDocumentClientCapabilities(
    CompletionClientCapabilities? Completion);

/// <summary>
/// https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#completionClientCapabilities
/// </summary>
public record CompletionClientCapabilities(
    bool? DynamicRegistration,
    CompletionClientCapabilitiesCompletionItem? CompletionItem);


/// <summary>
/// https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#completionClientCapabilities
/// </summary>
public record CompletionClientCapabilitiesCompletionItem(
    bool? SnippetSupport,
    bool? CommitCharactersSupport,
    bool? DeprecatedSupport,
    bool? PreselectSupport);

