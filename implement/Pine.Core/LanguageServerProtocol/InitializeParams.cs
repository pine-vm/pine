using System.Collections.Generic;

namespace Pine.Core.LanguageServerProtocol;

/// <summary>
/// https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#initializeParams
/// </summary>
public record InitializeParams(
    int? ProcessId,
    ClientCapabilities Capabilities,
    string? RootPath,
    string? RootUri,
    IReadOnlyList<WorkspaceFolder>? WorkspaceFolders,
    ParticipentInfo? ClientInfo);
