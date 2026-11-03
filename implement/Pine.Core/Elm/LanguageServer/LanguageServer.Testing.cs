using Pine.Core.Files;
using Pine.Core.Functional;
using Pine.Core.LanguageServerProtocol;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Text.Json;
using System.Threading;
using System.Threading.Tasks;

namespace Pine.Core.Elm.LanguageServer;

public partial class LanguageServer
{
    private readonly Lock _testingServiceLock = new();

    private LanguageServerTestingService? _testingService;

    private Func<PineTestsEvent, Task>? _publishTestingEvent;

    private Action<PineTestsChangedParams>? _publishTestingChanged;

    private readonly Dictionary<string, string> _testingDocumentUris = new(StringComparer.Ordinal);

    /// <summary>Connects version-correlated testing events and cheap workspace invalidations.</summary>
    public void SetTestingPublishers(
        Func<PineTestsEvent, Task>? publishEvent,
        Action<PineTestsChangedParams>? publishChanged)
    {
        _publishTestingEvent = publishEvent;
        _publishTestingChanged = publishChanged;
    }

    /// <summary>Discovers one project at a current or explicitly identified immutable source version.</summary>
    public Task<PineTestsDiscoverResult> TestsDiscoverAsync(
        PineTestsDiscoverParams parameters,
        CancellationToken cancellationToken = default) =>
        TestingService().DiscoverAsync(parameters, cancellationToken);

    /// <summary>Runs an explicit immutable version, independently of later document changes.</summary>
    public Task<PineTestsRunResult> TestsRunAsync(
        PineTestsRunParams parameters,
        CancellationToken cancellationToken = default) =>
        TestingService().RunAsync(parameters, cancellationToken);

    /// <summary>Reads source from an immutable test version rather than the mutable workspace.</summary>
    public PineTestsSourceResult TestsSource(PineTestsSourceParams parameters) =>
        TestingService().GetSource(parameters);

    private LanguageServerTestingService TestingService()
    {
        if (_initializeParams?.Capabilities.Experimental?.PineTests?.ProtocolVersion != 1)
        {
            throw new PineTestingProtocolException(
                "unsupportedProtocol",
                "Negotiate experimental.pineTests.protocolVersion = 1 first.");
        }

        lock (_testingServiceLock)
        {
            ObjectDisposedException.ThrowIf(_disposed, this);

            return
                _testingService ??=
                    new(
                        CaptureTestingWorkspaceAsync,
                        testCommitSource,
                        publishEvent: parameters => _publishTestingEvent?.Invoke(parameters) ?? Task.CompletedTask,
                        log: Log);
        }
    }

    private Task<ElmTestSourceSnapshot> CaptureTestingWorkspaceAsync(string projectUri, CancellationToken token)
    {
        var root =
            _workspaceFolders.Select(folder => LanguageServerTestingService.DirectoryUri(folder.Uri))
            .Where(folder => new Uri(folder).IsBaseOf(new Uri(projectUri)))
            .OrderByDescending(folder => folder.Length).FirstOrDefault()
            ?? (_initializeParams?.RootUri is { } rootUri &&
                new Uri(LanguageServerTestingService.DirectoryUri(rootUri)).IsBaseOf(new Uri(projectUri))
            ?
            LanguageServerTestingService.DirectoryUri(rootUri)
            :
            projectUri);

        Dictionary<string, string> openDocuments;
        Dictionary<string, int> versions;
        Dictionary<string, string> originalUris;
        long revision;

        lock (_documentStateLock)
        {
            revision = Interlocked.Read(ref _sourceRevision);

            openDocuments =
                _clientTextDocumentContents.ToDictionary(item => item.Key, item => item.Value, StringComparer.Ordinal);

            versions =
                _clientTextDocumentVersions.ToDictionary(item => item.Key, item => item.Value, StringComparer.Ordinal);

            originalUris = new(_testingDocumentUris, StringComparer.Ordinal);
        }

        var openDocumentsByUri = new Dictionary<string, string>(StringComparer.Ordinal);

        foreach (var document in openDocuments)
        {
            var uri = originalUris.GetValueOrDefault(document.Key, document.Key);
            openDocumentsByUri[WorkspaceDocumentUri.Normalize(new Uri(uri)).AbsoluteUri] = document.Value;
        }

        return
            Task.Run(
                () =>
                {
                    token.ThrowIfCancellationRequested();
                    root = TestingSourceRoot(projectUri, root, openDocumentsByUri);

                    var enumerated =
                        workspace.EnumerateFiles(
                            root,
                            name => name == "elm.json" || name.EndsWith(".elm", StringComparison.OrdinalIgnoreCase));

                    if (enumerated.IsErrOrNull() is { } error)
                        throw new PineTestingProtocolException("workspaceUnavailable", error.Kind + ": " + error.Message);

                    var files =
                        enumerated.IsOkOrNull()
                        ?? throw new InvalidOperationException("Unexpected workspace enumeration result.");

                    var contents = new Dictionary<string, string>(StringComparer.Ordinal);
                    var documentVersions = new Dictionary<string, int>(StringComparer.Ordinal);

                    foreach (var file in files)
                    {
                        token.ThrowIfCancellationRequested();
                        AddFile(file.DocumentUri, file.Text);
                    }

                    foreach (var document in openDocumentsByUri)
                    {
                        token.ThrowIfCancellationRequested();
                        var uri = document.Key;

                        if (new Uri(root).IsBaseOf(new Uri(uri)))
                            AddFile(uri, document.Value);
                    }

                    foreach (var document in openDocuments)
                    {
                        var uri = originalUris.GetValueOrDefault(document.Key, document.Key);

                        if (contents.ContainsKey(WorkspaceDocumentUri.Normalize(new Uri(uri)).AbsoluteUri) &&
                            versions.TryGetValue(document.Key, out var version))
                        {
                            documentVersions.Add(uri, version);
                        }
                    }

                    var tree =
                        FileTree.FromSetOfFilesWithStringPath(
                            contents.Select(
                                file =>
                                (LanguageServerTestingService.RelativePath(root, file.Key),
                                (ReadOnlyMemory<byte>)Encoding.UTF8.GetBytes(file.Value))));

                    if (Interlocked.Read(ref _sourceRevision) != revision)
                    {
                        throw new PineTestingProtocolException(
                            "workspaceChanged",
                            "Workspace changed during snapshot capture; request discovery again.");
                    }

                    return new ElmTestSourceSnapshot(root, tree, documentVersions);

                    void AddFile(string uri, string text)
                    {
                        var path = LanguageServerTestingService.RelativePath(root, uri);

                        if (path.Length == 0 || path.Any(segment => segment is ".git" or "elm-stuff" or "node_modules") ||
                            !(path[^1] == "elm.json" || path[^1].EndsWith(".elm", StringComparison.OrdinalIgnoreCase)))
                        {
                            return;
                        }

                        contents[WorkspaceDocumentUri.Normalize(new Uri(uri)).AbsoluteUri] = text;
                    }
                },
                token);
    }

    private string TestingSourceRoot(
        string projectUri,
        string root,
        IReadOnlyDictionary<string, string> openDocuments)
    {
        var manifestUri = new Uri(new Uri(projectUri), "elm.json").AbsoluteUri;
        var text = openDocuments.GetValueOrDefault(manifestUri);

        if (text is null)
        {
            var read = workspace.ReadFile(manifestUri);

            if (read.IsErrOrNull() is { } error)
                throw new PineTestingProtocolException("workspaceUnavailable", error.Message);

            text = OkFileOrNull(read)?.Text;
        }

        if (text is null)
            return root;

        try
        {
            var manifest =
                ElmDependencyResolver.ReadManifest(
                    FileTree.FromSetOfFilesWithStringPath(
                        new (IReadOnlyList<string>, ReadOnlyMemory<byte>)[]
                        {
                            (["elm.json"], Encoding.UTF8.GetBytes(text))
                        }),
                    ["elm.json"]);

            ElmDependencyResolver.ValidateManifestShape(manifest, manifestUri);

            foreach (var sourceDirectory in manifest.ParsedSourceDirectories)
            {
                var parent = new Uri(projectUri);

                for (var level = 0; level < sourceDirectory.ParentLevel; ++level)
                {
                    var next = new Uri(parent, "../");

                    if (next == parent)
                    {
                        throw new PineTestingProtocolException(
                            "invalidSource",
                            "A declared source directory traverses above its URI root.");
                    }

                    parent = next;
                }

                var sourceUri =
                    sourceDirectory.Subdirectories.Count == 0
                    ?
                    parent
                    :
                    new Uri(
                        parent,
                        string.Join('/', sourceDirectory.Subdirectories.Select(Uri.EscapeDataString)) + "/");

                while (!new Uri(root).IsBaseOf(sourceUri))
                {
                    var next = new Uri(new Uri(root), "../").AbsoluteUri;

                    if (next == root)
                    {
                        throw new PineTestingProtocolException(
                            "invalidSource",
                            "A declared source directory is outside the accessible URI root.");
                    }

                    root = next;
                }
            }
        }
        catch (Exception exception) when (exception is JsonException or FormatException or ArgumentException)
        {
            throw new PineTestingProtocolException("invalidManifest", exception.Message);
        }

        return root;
    }
}
