namespace Pine.Core.LanguageServerProtocol;

/// <summary>
/// https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#initializeResult
/// </summary>
public record InitializeResult(
    ServerCapabilities Capabilities);
