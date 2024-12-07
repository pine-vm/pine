using System.Collections.Generic;

namespace Pine.Core.LanguageServerProtocol;

/// <summary>
/// https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#registrationParams
/// </summary>
public record RegistrationParams(
    IReadOnlyList<Registration> Registrations);

/// <summary>
/// https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#registration
/// </summary>
public record Registration(
    string Id,
    string Method,
    object? RegisterOptions);

/// <summary>
/// https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#didChangeWatchedFilesRegistrationOptions
/// </summary>
public record DidChangeWatchedFilesRegistrationOptions(
    IReadOnlyList<FileSystemWatcher> Watchers);

