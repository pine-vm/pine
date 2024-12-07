namespace Pine.Core.LanguageServerProtocol;

/// <summary>
/// https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocumentPositionParams
/// </summary>
public record TextDocumentPositionParams(
    TextDocumentIdentifier TextDocument,
    Position Position);
