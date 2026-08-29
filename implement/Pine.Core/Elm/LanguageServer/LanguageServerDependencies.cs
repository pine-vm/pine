using Pine.Core.LanguageServerProtocol;
using System.Collections.Generic;
using System.Threading;
using System.Threading.Tasks;

namespace Pine.Core.Elm.LanguageServer;

/// <summary>
/// Computes diagnostics for an entry-point document without exposing a particular compiler implementation.
/// </summary>
public interface IDiagnosticsProvider
{
    /// <summary>
    /// Gets diagnostics for an entry-point document.
    /// </summary>
    ValueTask<Result<DiagnosticsProviderError, IReadOnlyList<DocumentDiagnostics>>> GetDiagnosticsAsync(
        string entryPointDocumentUri,
        CancellationToken cancellationToken);
}

/// <summary>
/// Diagnostics produced for one target document.
/// </summary>
public sealed record DocumentDiagnostics(
    string DocumentUri,
    IReadOnlyList<Diagnostic> Diagnostics);

/// <summary>
/// Identifies the kind of diagnostics provider error.
/// </summary>
public enum DiagnosticsProviderErrorKind
{
    /// <summary>
    /// Indicates an invalid request.
    /// </summary>
    InvalidRequest,

    /// <summary>
    /// Indicates unavailable source content.
    /// </summary>
    SourceUnavailable,

    /// <summary>
    /// Indicates a provider failure.
    /// </summary>
    ProviderFailure,

    /// <summary>
    /// Indicates an invalid provider response.
    /// </summary>
    InvalidResponse,
}

/// <summary>
/// Describes a diagnostics provider error.
/// </summary>
public sealed record DiagnosticsProviderError(
    DiagnosticsProviderErrorKind Kind,
    string Message);

/// <summary>
/// Formats complete document text without exposing the formatter implementation.
/// </summary>
public interface IDocumentFormatter
{
    /// <summary>
    /// Formats a document.
    /// </summary>
    ValueTask<Result<DocumentFormattingError, string>> FormatAsync(
        string documentUri,
        string sourceText,
        FormattingOptions options,
        CancellationToken cancellationToken);
}

/// <summary>
/// Identifies the kind of document formatting error.
/// </summary>
public enum DocumentFormattingErrorKind
{
    /// <summary>
    /// Indicates an unsupported document.
    /// </summary>
    UnsupportedDocument,

    /// <summary>
    /// Indicates a syntax error.
    /// </summary>
    SyntaxError,

    /// <summary>
    /// Indicates a provider failure.
    /// </summary>
    ProviderFailure,
}

/// <summary>
/// Describes a document formatting error.
/// </summary>
public sealed record DocumentFormattingError(
    DocumentFormattingErrorKind Kind,
    string Message);

/// <summary>
/// Read-only, URI-aware source content supplied to a language server.
/// </summary>
public interface ILanguageServerWorkspace
{
    /// <summary>
    /// Enumerates the files below <paramref name="rootDocumentUri"/>.
    /// </summary>
    /// <param name="rootDocumentUri">Directory URI to enumerate recursively.</param>
    /// <param name="fileNameFilter">
    /// Optional predicate on the last path component. Implementations only read the contents of
    /// files accepted by the predicate. Passing <see langword="null"/> includes every file.
    /// </param>
    Result<WorkspaceAccessError, IReadOnlyList<WorkspaceFile>> EnumerateFiles(
        string rootDocumentUri,
        System.Func<string, bool>? fileNameFilter = null);

    /// <summary>
    /// Reads a workspace file.
    /// </summary>
    Result<WorkspaceAccessError, WorkspaceFile?> ReadFile(string documentUri);

    /// <summary>
    /// Finds the nearest Elm project containing a document.
    /// </summary>
    Result<WorkspaceAccessError, ElmProjectLocation?> FindNearestElmProject(string documentUri);
}

/// <summary>
/// Contains the URI and text of a workspace file.
/// </summary>
public sealed record WorkspaceFile(
    string DocumentUri,
    string Text);

/// <summary>
/// Identifies an Elm project configuration.
/// </summary>
public sealed record ElmProjectLocation(
    string ElmJsonDocumentUri);

/// <summary>
/// Identifies the kind of workspace access error.
/// </summary>
public enum WorkspaceAccessErrorKind
{
    /// <summary>
    /// Indicates an invalid URI.
    /// </summary>
    InvalidUri,

    /// <summary>
    /// Indicates a URI outside the configured mounts.
    /// </summary>
    OutsideMount,

    /// <summary>
    /// Indicates an invalid path.
    /// </summary>
    InvalidPath,

    /// <summary>
    /// Indicates invalid text content.
    /// </summary>
    InvalidText,

    /// <summary>
    /// Indicates a workspace backend failure.
    /// </summary>
    BackendFailure,
}

/// <summary>
/// Describes a workspace access error.
/// </summary>
public sealed record WorkspaceAccessError(
    WorkspaceAccessErrorKind Kind,
    string Message);

/// <summary>
/// Overlay-aware read access to document texts, as maintained by a language server.
/// </summary>
public interface IDocumentTextSource
{
    /// <summary>
    /// Returns the current text for the given document URI, or <see langword="null"/> when unknown.
    /// </summary>
    string? TryGetDocumentText(string documentUri);
}

/// <summary>
/// Late-bound <see cref="IDocumentTextSource"/> for composition roots which need to construct
/// providers before the language server that owns the document overlay exists.
/// </summary>
public sealed class MutableDocumentTextSource : IDocumentTextSource
{
    /// <summary>
    /// The underlying source. Reads return <see langword="null"/> while this is not set.
    /// </summary>
    public IDocumentTextSource? Inner { get; set; }

    /// <inheritdoc/>
    public string? TryGetDocumentText(string documentUri) =>
        Inner?.TryGetDocumentText(documentUri);
}

/// <summary>
/// Host-independent settings of a language server.
/// </summary>
/// <param name="ServerVersion">
/// Version reported to the client in the <c>initialize</c> response.
/// </param>
/// <param name="MaxConcurrencyCount">
/// Maximum number of exclusively leased language-service workers.
/// </param>
public sealed record LanguageServerOptions
{
    /// <summary>
    /// Initializes language server options and enforces that at least one worker can be leased concurrently.
    /// </summary>
    public LanguageServerOptions(
        string ServerVersion,
        int MaxConcurrencyCount = 4)
    {
        System.ArgumentOutOfRangeException.ThrowIfLessThan(MaxConcurrencyCount, 1);

        this.ServerVersion = ServerVersion;
        this.MaxConcurrencyCount = MaxConcurrencyCount;
    }

    /// <summary>
    /// Gets the version string reported to clients during language server initialization.
    /// </summary>
    public string ServerVersion { get; }

    /// <summary>
    /// Gets the maximum number of language-service workers that may be leased at the same time.
    /// </summary>
    public int MaxConcurrencyCount { get; }
}
