using System.Collections.Generic;

namespace Pine.Core.LanguageServerProtocol;

/// <summary>
/// https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#hover
/// </summary>
public record Hover(
    IReadOnlyList<string> Contents,
    Range? Range);


