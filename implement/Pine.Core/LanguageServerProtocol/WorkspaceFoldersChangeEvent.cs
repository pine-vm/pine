using System.Collections.Generic;

namespace Pine.Core.LanguageServerProtocol;

/// <summary>
/// https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#workspaceFoldersChangeEvent
/// </summary>
public record WorkspaceFoldersChangeEvent(
    IReadOnlyList<WorkspaceFolder> Added,
    IReadOnlyList<WorkspaceFolder> Removed);
