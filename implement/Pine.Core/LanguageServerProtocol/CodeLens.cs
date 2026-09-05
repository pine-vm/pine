using System.Collections.Generic;
using System.Text.Json;

namespace Pine.Core.LanguageServerProtocol;

/// <summary>
/// https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#codeLens
/// </summary>
public record CodeLens(
    Range Range,
    Command? Command,
    JsonElement? Data);

/// <summary>
/// https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#codeLensOptions
/// </summary>
public record CodeLensOptions(
    bool? ResolveProvider);

/// <summary>
/// https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#codeLensParams
/// </summary>
public record CodeLensParams(
    TextDocumentIdentifier TextDocument);

/// <summary>
/// https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#command
/// </summary>
public record Command(
    string Title,
    [property: System.Text.Json.Serialization.JsonPropertyName("command")]
    string Identifier,
    IReadOnlyList<object>? Arguments);
