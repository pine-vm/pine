namespace Pine.Core.LanguageServerProtocol;

/// <summary>
/// https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocumentContentChangeEvent
/// 
/// An event describing a change to a text document. If only a text is provided it
/// is considered to be the full content of the document.
/// </summary>
public record TextDocumentContentChangeEvent(
    Range? Range,
    uint? RangeLength,
    string Text);
