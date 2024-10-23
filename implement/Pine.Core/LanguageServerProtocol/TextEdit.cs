namespace Pine.Core.LanguageServerProtocol;

/// <summary>
/// https://microsoft.github.io/language-server-protocol/specifications/lsp/3.18/specification/#textEdit
/// </summary>
public record TextEdit(
    Range Range,
    string NewText);
