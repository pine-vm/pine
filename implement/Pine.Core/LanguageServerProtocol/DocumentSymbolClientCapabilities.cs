namespace Pine.Core.LanguageServerProtocol;

/// <summary>
/// https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#documentSymbolClientCapabilities
/// </summary>
public record DocumentSymbolClientCapabilities(
    bool? HierarchicalDocumentSymbolSupport);
