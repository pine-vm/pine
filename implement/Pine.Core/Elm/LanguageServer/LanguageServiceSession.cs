using Pine.Core.Elm.LanguageServer.LanguageServiceInterface;
using System.Collections.Generic;
using System.Threading;
using System.Threading.Tasks;

namespace Pine.Core.Elm.LanguageServer;

/// <summary>
/// Stateful language-service session, holding the workspace known to the language service.
/// </summary>
public interface ILanguageServiceSession
{
    /// <summary>
    /// Adds or replaces the contents of a workspace file.
    /// </summary>
    Result<string, Response.WorkspaceSummaryResponse> AddFile(
        string fileUri,
        string fileContentAsText);

    /// <summary>
    /// Removes a workspace file.
    /// </summary>
    Result<string, Response.WorkspaceSummaryResponse> DeleteFile(
        string fileUri);

    /// <summary>
    /// Adds the modules of an Elm package version to the session.
    /// </summary>
    Result<string, Response.WorkspaceSummaryResponse> AddElmPackage(
        ElmPackageVersion019Identifer packageVersionId,
        IReadOnlyList<KeyValuePair<IReadOnlyList<string>, string>> filesContentsAsText);

    /// <summary>
    /// Answers a language-service request from the current workspace.
    /// </summary>
    Result<string, Response> HandleRequest(
        Request request);
}

/// <summary>
/// Creates language-service sessions, hiding how the underlying program is compiled and cached.
/// </summary>
public interface ILanguageServiceSessionFactory
{
    /// <summary>
    /// Creates a new session. Implementations may need substantial time on the first call.
    /// </summary>
    ValueTask<Result<string, ILanguageServiceSession>> CreateSessionAsync(
        CancellationToken cancellationToken);
}

/// <summary>
/// Supplies the modules of Elm package versions referenced from an <c>elm.json</c> file.
/// </summary>
public interface IElmPackageSource
{
    /// <summary>
    /// Loads the modules of the given package version, or returns <see langword="null"/> when this
    /// source does not contain that package version.
    /// </summary>
    Result<PackageLoadError, ElmPackageContent?> LoadPackage(
        ElmPackageVersion019Identifer packageVersionId);
}

/// <summary>
/// Modules of one Elm package version.
/// </summary>
/// <param name="RootUri">
/// URI of the directory containing the package modules, ending with a slash.
/// Used to map between package file locations and document URIs.
/// </param>
/// <param name="Modules">
/// Module file contents, keyed by the file path relative to <paramref name="RootUri"/>.
/// </param>
public sealed record ElmPackageContent(
    string RootUri,
    IReadOnlyList<KeyValuePair<IReadOnlyList<string>, string>> Modules);

/// <summary>
/// Classification of failures loading an Elm package version.
/// </summary>
public enum PackageLoadErrorKind
{
    /// <summary>
    /// The package identifier could not be mapped to a location.
    /// </summary>
    InvalidPackageIdentifier,

    /// <summary>
    /// The underlying source could not be read.
    /// </summary>
    SourceUnavailable,

    /// <summary>
    /// The package was found but its contents could not be interpreted.
    /// </summary>
    InvalidPackageContent,
}

/// <summary>
/// Failure loading an Elm package version.
/// </summary>
public sealed record PackageLoadError(
    PackageLoadErrorKind Kind,
    string Message);

/// <summary>
/// Package source without any packages.
/// </summary>
public sealed class EmptyElmPackageSource : IElmPackageSource
{
    /// <summary>
    /// Shared instance.
    /// </summary>
    public static readonly EmptyElmPackageSource Instance = new();

    /// <inheritdoc/>
    public Result<PackageLoadError, ElmPackageContent?> LoadPackage(
        ElmPackageVersion019Identifer packageVersionId) =>
        Result<PackageLoadError, ElmPackageContent?>.ok(null);
}
