using Pine.Core.IO;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Text;

namespace Pine.Core.Elm.LanguageServer;

/// <summary>
/// Associates a document-URI directory with a componentized file-store root.
/// </summary>
public sealed record FileStoreMount
{
    /// <summary>
    /// Initializes a file-store mount.
    /// </summary>
    public FileStoreMount(Uri rootUri, IFileStoreReader reader)
    {
        ArgumentNullException.ThrowIfNull(rootUri);
        ArgumentNullException.ThrowIfNull(reader);

        if (!rootUri.IsAbsoluteUri)
            throw new ArgumentException("The mount root must be an absolute URI.", nameof(rootUri));

        if (!string.IsNullOrEmpty(rootUri.Query) || !string.IsNullOrEmpty(rootUri.Fragment))
            throw new ArgumentException("The mount root must not contain a query or fragment.", nameof(rootUri));

        RootUri =
            rootUri.AbsoluteUri.EndsWith("/", StringComparison.Ordinal)
            ?
            rootUri
            :
            new Uri(rootUri.AbsoluteUri + "/", UriKind.Absolute);

        Reader = reader;
    }

    /// <summary>
    /// Gets the root URI of the mount.
    /// </summary>
    public Uri RootUri { get; }

    /// <summary>
    /// Gets the mounted file-store reader.
    /// </summary>
    public IFileStoreReader Reader { get; }
}

/// <summary>
/// URI-aware workspace backed by one or more file-store mounts.
/// </summary>
public sealed class WorkspaceFromFileStoreMounts : ILanguageServerWorkspace
{
    private static readonly Encoding StrictUtf8 =
        new UTF8Encoding(
            encoderShouldEmitUTF8Identifier: false,
            throwOnInvalidBytes: true);

    private readonly IReadOnlyList<ResolvedMount> _mounts;

    /// <summary>
    /// Initializes a workspace backed by file-store mounts.
    /// </summary>
    public WorkspaceFromFileStoreMounts(IReadOnlyList<FileStoreMount> mounts)
    {
        ArgumentNullException.ThrowIfNull(mounts);

        _mounts =
            [
            .. mounts.Select(
                mount =>
                {
                    ArgumentNullException.ThrowIfNull(mount);

                    var rootParts = ParseAbsoluteUri(mount.RootUri.AbsoluteUri);

                    if (rootParts.IsErrOrNull() is { } error)
                        throw new ArgumentException("Invalid mount root: " + error.Message, nameof(mounts));

                    return
                        new ResolvedMount(
                            mount,
                            rootParts.IsOkOrNull()
                            ??
                            throw new InvalidOperationException("Unexpected URI parse result."));
                })
            ];
    }

    /// <inheritdoc/>
    public Result<WorkspaceAccessError, IReadOnlyList<WorkspaceFile>> EnumerateFiles(
        string rootDocumentUri,
        Func<string, bool>? fileNameFilter = null)
    {
        var resolution = ResolveUri(rootDocumentUri);

        if (resolution.IsErrOrNull() is { } resolutionError)
            return resolutionError;

        var resolved =
            resolution.IsOkOrNull()
            ??
            throw new InvalidOperationException("Unexpected URI resolution result.");

        try
        {
            var filesByUri = new Dictionary<string, WorkspaceFile>(StringComparer.Ordinal);

            var requestedPath =
                resolved.Mount.UriParts.PathComponents
                .AddRange(resolved.RelativePath);

            var mountDirectories =
                new[] { (resolved.Mount, resolved.RelativePath) }
                .Concat(
                    _mounts
                    .Where(
                        mount =>
                        !ReferenceEquals(mount, resolved.Mount) &&
                        string.Equals(
                            mount.UriParts.Origin,
                            resolved.Mount.UriParts.Origin,
                            StringComparison.OrdinalIgnoreCase) &&
                        IsPathPrefix(requestedPath, mount.UriParts.PathComponents))
                    .Select(
                        mount =>
                        (mount, directoryPath: (IImmutableList<string>)ImmutableArray<string>.Empty)));

            foreach (var (mount, directoryPath) in mountDirectories)
            {
                foreach (var listedRelativePath in
                    mount.Mount.Reader.ListFilesInDirectory(directoryPath))
                {
                    var validatedPath = ValidateStorePath(listedRelativePath);

                    if (validatedPath.IsErrOrNull() is { } pathError)
                        return pathError;

                    var listedPath =
                        validatedPath.IsOkOrNull()
                        ??
                        throw new InvalidOperationException("Unexpected path validation result.");

                    var storePath = directoryPath.AddRange(listedPath);

                    if (fileNameFilter is not null &&
                        0 < storePath.Count &&
                        !fileNameFilter(storePath[^1]))
                    {
                        continue;
                    }

                    var documentUri = BuildDocumentUri(mount, storePath);
                    var winningResolution = ResolveUri(documentUri);

                    if (winningResolution.IsErrOrNull() is { } winningError)
                        return winningError;

                    var winningDocument =
                        winningResolution.IsOkOrNull()
                        ??
                        throw new InvalidOperationException("Unexpected URI resolution result.");

                    if (!ReferenceEquals(winningDocument.Mount, mount))
                        continue;

                    var content = mount.Mount.Reader.GetFileContent(storePath);

                    if (content is null)
                    {
                        return
                            new WorkspaceAccessError(
                                WorkspaceAccessErrorKind.BackendFailure,
                                "The file store listed a file that could not be read: " +
                                string.Join("/", storePath));
                    }

                    var textResult = DecodeText(content.Value, storePath);

                    if (textResult.IsErrOrNull() is { } textError)
                        return textError;

                    filesByUri[documentUri] =
                        new WorkspaceFile(
                            documentUri,
                            textResult.IsOkOrNull()
                            ??
                            throw new InvalidOperationException("Unexpected text decoding result."));
                }
            }

            return
                filesByUri.Values
                .OrderBy(file => file.DocumentUri, StringComparer.Ordinal)
                .ToImmutableArray();
        }
        catch (Exception exception)
        {
            return BackendFailure("Failed to enumerate workspace files.", exception);
        }
    }

    /// <summary>
    /// Reads a file from the mounted workspace.
    /// </summary>
    public Result<WorkspaceAccessError, WorkspaceFile?> ReadFile(string documentUri)
    {
        var resolution = ResolveUri(documentUri);

        if (resolution.IsErrOrNull() is { } resolutionError)
            return resolutionError;

        var resolved =
            resolution.IsOkOrNull()
            ??
            throw new InvalidOperationException("Unexpected URI resolution result.");

        try
        {
            var content = resolved.Mount.Mount.Reader.GetFileContent(resolved.RelativePath);

            if (content is null)
                return Result<WorkspaceAccessError, WorkspaceFile?>.ok(null);

            var textResult = DecodeText(content.Value, resolved.RelativePath);

            if (textResult.IsErrOrNull() is { } textError)
                return textError;

            return
                new WorkspaceFile(
                    BuildDocumentUri(resolved.Mount, resolved.RelativePath),
                    textResult.IsOkOrNull()
                    ??
                    throw new InvalidOperationException("Unexpected text decoding result."));
        }
        catch (Exception exception)
        {
            return BackendFailure("Failed to read workspace file.", exception);
        }
    }

    /// <summary>
    /// Finds the nearest Elm project containing a document.
    /// </summary>
    public Result<WorkspaceAccessError, ElmProjectLocation?> FindNearestElmProject(string documentUri)
    {
        var resolution = ResolveUri(documentUri);

        if (resolution.IsErrOrNull() is { } resolutionError)
            return resolutionError;

        var resolved =
            resolution.IsOkOrNull()
            ??
            throw new InvalidOperationException("Unexpected URI resolution result.");

        if (resolved.RelativePath.Count is 0)
            return Result<WorkspaceAccessError, ElmProjectLocation?>.ok(null);

        try
        {
            var directoryPath = resolved.RelativePath.RemoveAt(resolved.RelativePath.Count - 1);

            for (var componentCount = directoryPath.Count; componentCount >= 0; componentCount--)
            {
                var elmJsonPath =
                    directoryPath.Take(componentCount)
                    .Append("elm.json")
                    .ToImmutableArray();

                if (resolved.Mount.Mount.Reader.GetFileContent(elmJsonPath) is not null)
                {
                    return
                        new ElmProjectLocation(
                            BuildDocumentUri(resolved.Mount, elmJsonPath));
                }
            }

            return Result<WorkspaceAccessError, ElmProjectLocation?>.ok(null);
        }
        catch (Exception exception)
        {
            return BackendFailure("Failed to locate the nearest Elm project.", exception);
        }
    }

    private Result<WorkspaceAccessError, ResolvedDocumentUri> ResolveUri(string documentUri)
    {
        var parsed = ParseAbsoluteUri(documentUri);

        if (parsed.IsErrOrNull() is { } parseError)
            return parseError;

        var uriParts =
            parsed.IsOkOrNull()
            ??
            throw new InvalidOperationException("Unexpected URI parse result.");

        var matchingMount =
            _mounts
            .Where(
                mount =>
                string.Equals(mount.UriParts.Origin, uriParts.Origin, StringComparison.OrdinalIgnoreCase) &&
                IsPathPrefix(mount.UriParts.PathComponents, uriParts.PathComponents))
            .OrderByDescending(mount => mount.UriParts.PathComponents.Count)
            .FirstOrDefault();

        if (matchingMount is null)
        {
            return
                new WorkspaceAccessError(
                    WorkspaceAccessErrorKind.OutsideMount,
                    "The document URI is outside all configured workspace mounts: " + documentUri);
        }

        return
            new ResolvedDocumentUri(
                matchingMount,
                uriParts.PathComponents
                .Skip(matchingMount.UriParts.PathComponents.Count)
                .ToImmutableArray());
    }

    private static Result<WorkspaceAccessError, ParsedUri> ParseAbsoluteUri(string documentUri)
    {
        if (!Uri.TryCreate(documentUri, UriKind.Absolute, out var uri))
        {
            return
                new WorkspaceAccessError(
                    WorkspaceAccessErrorKind.InvalidUri,
                    "The document URI is not absolute: " + documentUri);
        }

        if (!string.IsNullOrEmpty(uri.Query) || !string.IsNullOrEmpty(uri.Fragment))
        {
            return
                new WorkspaceAccessError(
                    WorkspaceAccessErrorKind.InvalidUri,
                    "Document URIs with a query or fragment are not supported: " + documentUri);
        }

        try
        {
            var pathComponents =
                uri.GetComponents(UriComponents.Path, UriFormat.UriEscaped)
                .Split('/', StringSplitOptions.RemoveEmptyEntries)
                .Select(Uri.UnescapeDataString)
                .ToImmutableArray();

            var validatedPath = ValidateStorePath(pathComponents);

            if (validatedPath.IsErrOrNull() is { } pathError)
                return pathError;

            return
                new ParsedUri(
                    uri.GetComponents(UriComponents.SchemeAndServer, UriFormat.UriEscaped),
                    validatedPath.IsOkOrNull()
                    ??
                    throw new InvalidOperationException("Unexpected path validation result."));
        }
        catch (Exception exception)
        {
            return
                new WorkspaceAccessError(
                    WorkspaceAccessErrorKind.InvalidUri,
                    "Failed to parse document URI '" + documentUri + "': " + exception.Message);
        }
    }

    private static Result<WorkspaceAccessError, IImmutableList<string>> ValidateStorePath(
        IEnumerable<string> path)
    {
        var components = path.ToImmutableArray();

        foreach (var component in components)
        {
            if (string.IsNullOrEmpty(component) ||
                component is "." or ".." ||
                component.Contains('/') ||
                component.Contains('\\'))
            {
                return
                    new WorkspaceAccessError(
                        WorkspaceAccessErrorKind.InvalidPath,
                        "Invalid file-store path component: '" + component + "'.");
            }
        }

        return Result<WorkspaceAccessError, IImmutableList<string>>.ok(components);
    }

    private static Result<WorkspaceAccessError, string> DecodeText(
        ReadOnlyMemory<byte> content,
        IImmutableList<string> path)
    {
        try
        {
            return StrictUtf8.GetString(content.Span);
        }
        catch (DecoderFallbackException exception)
        {
            return
                new WorkspaceAccessError(
                    WorkspaceAccessErrorKind.InvalidText,
                    "File is not valid UTF-8 at '" + string.Join("/", path) + "': " + exception.Message);
        }
    }

    private static string BuildDocumentUri(
        ResolvedMount mount,
        IImmutableList<string> storePath)
    {
        var relativeUri =
            string.Join(
                "/",
                storePath.Select(Uri.EscapeDataString));

        return new Uri(mount.Mount.RootUri, relativeUri).AbsoluteUri;
    }

    private static bool IsPathPrefix(
        IImmutableList<string> prefix,
        IImmutableList<string> path)
    {
        if (prefix.Count > path.Count)
            return false;

        for (var index = 0; index < prefix.Count; index++)
        {
            if (!string.Equals(prefix[index], path[index], StringComparison.Ordinal))
                return false;
        }

        return true;
    }

    private static WorkspaceAccessError BackendFailure(string message, Exception exception) =>
        new(
            WorkspaceAccessErrorKind.BackendFailure,
            message + " " + exception.Message);

    private sealed record ParsedUri(
        string Origin,
        IImmutableList<string> PathComponents);

    private sealed record ResolvedMount(
        FileStoreMount Mount,
        ParsedUri UriParts);

    private sealed record ResolvedDocumentUri(
        ResolvedMount Mount,
        IImmutableList<string> RelativePath);
}
