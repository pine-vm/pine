namespace Pine.Core.LanguageServerProtocol;

/**
 * An event describing a file change.
interface FileEvent
{
    uri: DocumentUri;
    type: FileChangeType;
}
 */

/// <summary>
/// https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#fileEvent
/// </summary>
public record FileEvent(
    string Uri,
    FileChangeType Type);

/// <summary>
/// https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#fileChangeType
/// </summary>
public enum FileChangeType
{
    /// <summary>
    /// The file got created.
    /// </summary>
    Created = 1,

    /// <summary>
    /// The file got changed.
    /// </summary>
    Changed = 2,

    /// <summary>
    /// The file got deleted.
    /// </summary>
    Deleted = 3
}

