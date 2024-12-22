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
    CompletionClientCapabilities? Completion,
    PublishDiagnosticsClientCapabilities? PublishDiagnostics,
    DocumentSymbolClientCapabilities? DocumentSymbol,
    RenameClientCapabilities? Rename);


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


/// <summary>
/// https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#publishDiagnosticsClientCapabilities
/// </summary>
public record PublishDiagnosticsClientCapabilities(
    bool? RelatedInformation,
    bool? VersionSupport,
    bool? CodeDescriptionSupport,
    bool? DataSupport);

/// <summary>
/// https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#renameClientCapabilities
/// </summary>
public record RenameClientCapabilities(
    bool? DynamicRegistration,
    bool? PrepareSupport,
    bool? HonorsChangeAnnotations);

