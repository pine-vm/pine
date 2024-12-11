using System.Collections.Generic;

namespace Pine.Core.LanguageServerProtocol;

/// <summary>
/// https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#publishDiagnosticsParams
/// </summary>
public record PublishDiagnosticsParams(
    string Uri,
    IReadOnlyList<Diagnostic> Diagnostics,
    int? Version);
