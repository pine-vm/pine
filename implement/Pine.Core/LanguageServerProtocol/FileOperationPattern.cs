namespace Pine.Core.LanguageServerProtocol;

/// <summary>
/// https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#fileOperationPattern
/// </summary>
public record FileOperationPattern(
    string Glob,
    string? Matches,
    FileOperationPatternOptions? Options);

/// <summary>
/// https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#fileOperationPatternOptions
/// </summary>
public record FileOperationPatternOptions(
    bool? IgnoreCase);

