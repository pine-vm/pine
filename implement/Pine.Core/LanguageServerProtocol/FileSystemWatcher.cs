namespace Pine.Core.LanguageServerProtocol;

/// <summary>
/// https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#fileSystemWatcher
/// </summary>
public record FileSystemWatcher(
    string GlobPattern,
    WatchKind? Kind);

/// <summary>
/// https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#watchKind
/// </summary>
public enum WatchKind
{
    /// <summary>
    /// Interested in create events.
    /// </summary>
    Create = 1,

    /// <summary>
    /// Interested in change events
    /// </summary>
    Change = 2,

    /// <summary>
    /// Interested in delete events
    /// </summary>
    Delete = 4,
}

