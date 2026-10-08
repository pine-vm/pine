using Pine.Core.Elm.Elm019;
using Pine.Core.Elm.ElmCompilerInDotnet;
using Pine.Core.Elm.ElmSyntax;
using Pine.Core.Files;
using Pine.Core.LanguageServerProtocol;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Text.Json;
using System.Threading;
using System.Threading.Tasks;

using Syntax = Pine.Core.Elm.ElmSyntax.SyntaxModel;

namespace Pine.Core.Elm.LanguageServer;

/// <summary>Analyzes all application sources, including editor overlays, without compiling entry points.</summary>
public sealed class ElmCompilerDiagnosticsProvider(
    ILanguageServerWorkspace workspace,
    IDocumentTextSource documentTexts,
    Func<FileTree, IReadOnlyList<string>, CancellationToken, Task<ElmResolvedBuild>> prepareApplication,
    bool includeTests = false) : IApplicationDiagnosticsProvider
{
    private readonly SemaphoreSlim _analysisConcurrency = new(1, 1);

    private readonly HashSet<string> _knownDocumentUris = new(StringComparer.Ordinal);

    private readonly Dictionary<string, Result<ElmSyntaxParseError, Syntax.File>> _sourceParseCache = [];

    private readonly Dictionary<(Syntax.Node<Syntax.Declaration> Declaration, string Scope),
        DeclarationCanonicalizationCacheEntry>
        _canonicalizationCache = [];

    /// <inheritdoc/>
    public Result<DiagnosticsProviderError, string> GetApplicationUri(string documentUri)
    {
        var project = workspace.FindNearestElmProject(documentUri);

        if (project.IsErrOrNull() is { } error)
            return new DiagnosticsProviderError(DiagnosticsProviderErrorKind.SourceUnavailable, error.Message);

        if (project is not Result<WorkspaceAccessError, ElmProjectLocation?>.Ok { Value: { } location })
        {
            return
                new DiagnosticsProviderError(
                    DiagnosticsProviderErrorKind.SourceUnavailable,
                    "Could not find elm.json for " + documentUri);
        }

        return location.ElmJsonDocumentUri;
    }

    /// <inheritdoc/>
    public async ValueTask<Result<DiagnosticsProviderError, IReadOnlyList<DocumentDiagnostics>>> GetDiagnosticsAsync(
        string entryPointDocumentUri,
        CancellationToken cancellationToken)
    {
        var application = GetApplicationUri(entryPointDocumentUri);

        if (application.IsErrOrNull() is { } applicationError)
            return applicationError;

        var manifestUri = application.Extract(error => throw new InvalidOperationException(error.Message));
        var manifestResult = workspace.ReadFile(manifestUri);

        if (manifestResult.IsErrOrNull() is { } accessError)
            return new DiagnosticsProviderError(DiagnosticsProviderErrorKind.SourceUnavailable, accessError.Message);

        var manifestText =
            documentTexts.TryGetDocumentText(manifestUri) ??
            (manifestResult is Result<WorkspaceAccessError, WorkspaceFile?>.Ok { Value: { } manifestFile }
            ?
            manifestFile.Text
            :
            null);

        if (manifestText is null)
        {
            return
                new DiagnosticsProviderError(
                    DiagnosticsProviderErrorKind.SourceUnavailable,
                    "No source available for " + manifestUri);
        }

        await _analysisConcurrency.WaitAsync(cancellationToken);

        try
        {
            _knownDocumentUris.Add(entryPointDocumentUri);

            var manifestTree =
                FileTree.FromSetOfFilesWithStringPath(
                    [(new[] { "elm.json" }, (ReadOnlyMemory<byte>)Encoding.UTF8.GetBytes(manifestText))]);

            var manifest = ElmDependencyResolver.ReadManifest(manifestTree, ["elm.json"]);
            var projectUri = new Uri(new Uri(manifestUri), ".");

            var directories =
                manifest.Type is "package"
                ?
                new[] { new ElmJsonStructure.RelativeDirectory(0, ["src"]) }
                :
                [.. manifest.ParsedSourceDirectories];

            var parentLevels = directories.Select(directory => directory.ParentLevel).DefaultIfEmpty().Max();
            var treeUri = projectUri;

            for (var level = 0; level < parentLevels; level++)
                treeUri = new Uri(treeUri, "../");

            var sourceUris =
                directories.Select(
                    directory => new Uri(
                        projectUri,
                        string.Concat(Enumerable.Repeat("../", directory.ParentLevel)) +
                        (directory.Subdirectories.Count is 0
                        ?
                        "./"
                        :
                        string.Join("/", directory.Subdirectories.Select(Uri.EscapeDataString)) + "/")))
                .ToList();

            if (includeTests)
                sourceUris.Add(new Uri(projectUri, "tests/"));

            var files =
                new Dictionary<string, string>(StringComparer.Ordinal)
                {
                    [RelativePath(treeUri, new Uri(manifestUri))] = manifestText
                };

            foreach (var directory in sourceUris.Distinct())
            {
                var enumeration =
                    workspace.EnumerateFiles(
                        directory.AbsoluteUri,
                        name =>
                        name is "elm.json" || name.EndsWith(".elm", StringComparison.OrdinalIgnoreCase));

                if (enumeration.IsErrOrNull() is { } enumerationError)
                {
                    return
                        new DiagnosticsProviderError(
                            DiagnosticsProviderErrorKind.SourceUnavailable,
                            enumerationError.Message);
                }

                foreach (var source in enumeration.Extract(error => throw new InvalidOperationException(error.Message)))
                {
                    var sourceUri = new Uri(source.DocumentUri);

                    if (!treeUri.IsBaseOf(sourceUri) || !directory.IsBaseOf(sourceUri))
                    {
                        return
                            new DiagnosticsProviderError(
                                DiagnosticsProviderErrorKind.InvalidResponse,
                                "Workspace enumeration returned a source outside the selected directories: " +
                                source.DocumentUri);
                    }

                    files[RelativePath(treeUri, sourceUri)] =
                        documentTexts.TryGetDocumentText(source.DocumentUri) ?? source.Text;
                }
            }

            foreach (var uri in _knownDocumentUris.ToArray())
            {
                var text = documentTexts.TryGetDocumentText(uri);

                if (text is null)
                {
                    _knownDocumentUris.Remove(uri);
                    continue;
                }

                var sourceUri = new Uri(uri);

                if (treeUri.IsBaseOf(sourceUri) && sourceUris.Any(directory => directory.IsBaseOf(sourceUri)))
                    files[RelativePath(treeUri, sourceUri)] = text;
            }

            var tree =
                FileTree.FromSetOfFilesWithStringPath(
                    files.Select(
                        file =>
                        ((IReadOnlyList<string>)file.Key.Split('/'), (ReadOnlyMemory<byte>)Encoding.UTF8.GetBytes(file.Value))));

            var build =
                await prepareApplication(
                    tree,
                    RelativePath(treeUri, new Uri(manifestUri)).Split('/'),
                    cancellationToken);

            cancellationToken.ThrowIfCancellationRequested();
            var diagnostics = ElmCompiler.AnalyzeApplication(build, _sourceParseCache, _canonicalizationCache);
            cancellationToken.ThrowIfCancellationRequested();

            var projectPaths =
                build.ProjectSources.EnumerateFilesTransitive()
                .Select(source => string.Join("/", source.path)).ToHashSet(StringComparer.Ordinal);

            var result =
                build.ProjectSources.EnumerateFilesTransitive()
                .Where(source => source.path[^1].EndsWith(".elm", StringComparison.OrdinalIgnoreCase))
                .Select(
                    source =>
                    {
                        var path = string.Join("/", source.path);

                        var uri =
                            new Uri(treeUri, string.Join("/", source.path.Select(Uri.EscapeDataString))).AbsoluteUri;

                        return
                            new DocumentDiagnostics(
                                uri,
                                [
                                .. diagnostics.Where(diagnostic => diagnostic.FilePath == path)
                                .Select(diagnostic => ToDiagnostic(diagnostic, treeUri, projectPaths))
                                ]);
                    }).ToArray();

            if (_sourceParseCache.Count > 512)
                _sourceParseCache.Clear();

            if (_canonicalizationCache.Count > 10_000)
                _canonicalizationCache.Clear();

            return result;
        }
        catch (ElmDependencyResolutionException error)
        {
            return new DiagnosticsProviderError(DiagnosticsProviderErrorKind.ProviderFailure, error.Message);
        }
        catch (Exception error) when (error is JsonException or FormatException or ArgumentException)
        {
            return new DiagnosticsProviderError(DiagnosticsProviderErrorKind.InvalidRequest, error.Message);
        }
        finally
        {
            _analysisConcurrency.Release();
        }
    }

    private static string RelativePath(Uri root, Uri source) =>
        Uri.UnescapeDataString(root.MakeRelativeUri(source).ToString());

    private static Diagnostic ToDiagnostic(
        ElmCompilerDiagnostic diagnostic,
        Uri treeUri,
        IReadOnlySet<string> projectPaths) =>
        new(
            new(
                new(
                    checked((uint)Math.Max(0, diagnostic.Range.Start.Row - 1)),
                    checked((uint)Math.Max(0, diagnostic.Range.Start.Column - 1))),
                new(
                    checked((uint)Math.Max(0, diagnostic.Range.End.Row - 1)),
                    checked((uint)Math.Max(0, diagnostic.Range.End.Column - 1)))),
            DiagnosticSeverity.Error,
            diagnostic.Code,
            null,
            "pine-elm",
            diagnostic.Message,
            null,
            diagnostic.RelatedLocations.Count is 0
            ?
            null
            :
            [
            .. diagnostic.RelatedLocations.Where(location => projectPaths.Contains(location.FilePath))
            .Select(
                location => new DiagnosticRelatedInformation(
                    new Location(
                        new Uri(
                            treeUri,
                            string.Join("/", location.FilePath.Split('/').Select(Uri.EscapeDataString))).AbsoluteUri,
                        new(
                            new(
                                checked((uint)Math.Max(0, location.Range.Start.Row - 1)),
                                checked((uint)Math.Max(0, location.Range.Start.Column - 1))),
                            new(
                                checked((uint)Math.Max(0, location.Range.End.Row - 1)),
                                checked((uint)Math.Max(0, location.Range.End.Column - 1))))),
                    location.Message))
            ]);
}
