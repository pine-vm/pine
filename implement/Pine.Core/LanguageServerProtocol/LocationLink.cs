namespace Pine.Core.LanguageServerProtocol;

/// <summary>
/// https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#locationLink
/// </summary>
public record LocationLink(
    Range? OriginSelectionRange,
    string TargetUri,
    Range TargetRange,
    Range TargetSelectionRange);
