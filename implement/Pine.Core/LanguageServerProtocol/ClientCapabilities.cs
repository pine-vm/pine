namespace Pine.Core.LanguageServerProtocol;

/// <summary>
/// https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#clientCapabilities
/// </summary>
public record ClientCapabilities(
    ClientCapabilitiesWorkspace? Workspace);

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
