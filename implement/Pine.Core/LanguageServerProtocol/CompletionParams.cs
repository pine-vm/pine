using System.Collections.Generic;

namespace Pine.Core.LanguageServerProtocol;

/// <summary>
/// https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#completionParams
/// </summary>
public record CompletionParams(
    TextDocumentIdentifier TextDocument,
    Position Position,
    CompletionContext? Context);

/// <summary>
/// https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#completionContext
/// </summary>
public record CompletionContext();

/// <summary>
/// https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#completionItem
/// </summary>
public record CompletionItem(
    string Label,
    string? SortText,
    string? FilterText,
    string? InsertText,
    string? TextEditText,
    string? Detail,
    string? Documentation,
    bool? Preselect,
    bool? Deprecated,
    IReadOnlyList<string>? CommitCharacters);

