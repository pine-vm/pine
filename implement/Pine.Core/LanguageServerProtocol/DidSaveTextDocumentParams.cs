namespace Pine.Core.LanguageServerProtocol;

/// <summary>
/// https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#didSaveTextDocumentParams
/// </summary>
public record DidSaveTextDocumentParams(
    TextDocumentIdentifier TextDocument,
    string? Text);
