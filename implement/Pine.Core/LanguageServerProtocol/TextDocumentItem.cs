namespace Pine.Core.LanguageServerProtocol;

/// <summary>
/// https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocumentItem
/// </summary>
public record TextDocumentItem(
    string Uri,
    string LanguageId,
    int Version,
    string Text);
