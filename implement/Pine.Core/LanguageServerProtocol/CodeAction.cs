using System.Collections.Generic;

namespace Pine.Core.LanguageServerProtocol;

/// <summary>Request for actions applicable to a range of a document.</summary>
public record CodeActionParams(
    TextDocumentIdentifier TextDocument,
    Range Range,
    CodeActionContext Context);

/// <summary>Optional filter for the requested action kinds.</summary>
public record CodeActionContext(IReadOnlyList<string>? Only = null);

/// <summary>An editor action backed by a workspace edit.</summary>
public record CodeAction(string Title, string Kind, WorkspaceEdit Edit);
