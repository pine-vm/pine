namespace Pine.Core.LanguageServerProtocol;

/// <summary>
/// https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#referenceParams
/// </summary>
public record ReferenceParams(
    TextDocumentIdentifier TextDocument,
    Position Position,
    ReferenceContext Context);

/// <summary>
/// https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#referenceContext
/// </summary>
public record ReferenceContext(
    bool IncludeDeclaration);
