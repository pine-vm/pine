using System.Collections.Generic;

namespace Pine.Core.LanguageServerProtocol;

/// <summary>
/// https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocumentEdit
/// </summary>
public record TextDocumentEdit(
    OptionalVersionedTextDocumentIdentifier TextDocument,
    IReadOnlyList<TextEdit> Edits);

