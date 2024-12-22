using System.Collections.Generic;

namespace Pine.Core.LanguageServerProtocol;

/// <summary>
/// https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#workspaceEdit
/// </summary>
public record WorkspaceEdit(
    IReadOnlyList<TextDocumentEdit> DocumentChanges);
