namespace Pine.Core.LanguageServerProtocol;

/// <summary>
/// https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#range
/// </summary>
public record Range(
    Position Start,
    Position End);

/// <summary>
/// https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#position
/// </summary>
public record Position(
    uint Line,
    uint Character);

