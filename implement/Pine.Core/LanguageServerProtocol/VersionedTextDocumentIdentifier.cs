namespace Pine.Core.LanguageServerProtocol;

/// <summary>
/// https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#versionedTextDocumentIdentifier
/// </summary>
public record VersionedTextDocumentIdentifier(
    string Uri,
    int Version);
