using Pine.Core.Addressing;
using Pine.Core.Elm.ElmCompilerInDotnet;
using Pine.Core.Elm.Testing;
using Pine.Core.Encodings;
using Pine.Core.Files;
using Pine.Core.LanguageServerProtocol;
using System;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Security.Cryptography;
using System.Text;
using System.Threading;
using System.Threading.Tasks;

using SyntaxLocation = Pine.Core.Elm.Syntax.Concrete.Location;
using ProtocolRange = Pine.Core.LanguageServerProtocol.Range;

namespace Pine.Core.Elm.LanguageServer;

/// <summary>Immutable input bytes at a logical source root, with accepted document versions when applicable.</summary>
public sealed record ElmTestSourceSnapshot(
    string RootUri,
    FileTree Files,
    IReadOnlyDictionary<string, int> DocumentVersions);

/// <summary>Host boundary for resolving exact local Git commits without checking out or changing workspace files.</summary>
public interface IElmTestCommitSource
{
    /// <summary>Loads the specified full commit ID, not a branch, abbreviated hash, or working tree.</summary>
    Task<ElmTestSourceSnapshot> LoadCommitAsync(
        string projectUri,
        string commit,
        CancellationToken cancellationToken);
}

/// <summary>A rejected testing request with a machine-readable protocol error code.</summary>
public sealed class PineTestingProtocolException(string code, string message) : Exception(message)
{
    /// <summary>Stable error category defined by the testing protocol.</summary>
    public string Code { get; } = code;
}

/// <summary>
/// Version-scoped test discovery and execution. Mutable workspace state is captured only when explicitly
/// requested; all subsequent work uses retained immutable bytes, never a path reread from disk.
/// </summary>
public sealed class LanguageServerTestingService(
    Func<string, CancellationToken, Task<ElmTestSourceSnapshot>> captureWorkspace,
    IElmTestCommitSource? commitSource = null,
    IElmPackageProvider? packageProvider = null,
    Func<PineTestsEvent, Task>? publishEvent = null,
    Action<string>? log = null,
    SourceCompilationCaches? compilationCaches = null,
    ElmTestDiscoveryCaches? discoveryCaches = null) : IDisposable
{
    /// <summary>Maximum number of immutable versions retained per source root by one server connection.</summary>
    public const int RetainedVersions = 16;

    /// <summary>Maximum combined file bytes accepted in a single input snapshot.</summary>
    public const long MaximumSnapshotBytes = 64 * 1024 * 1024;

    private static readonly UTF8Encoding s_utf8 = new(false, true);

    private readonly Lock _snapshotLock = new();

    private readonly Dictionary<(string Project, string Version), Snapshot> _snapshots = [];

    private readonly ConcurrentDictionary<string, byte> _activeRuns = new(StringComparer.Ordinal);

    private readonly SemaphoreSlim _workCapacity = new(4, 4);

    private readonly CancellationTokenSource _lifetime = new();

    private readonly Lazy<ElmTestRunCacheSession> _cache =
        new(() => new(generationOptions: new(), compilationCaches: compilationCaches));

    private readonly Lazy<ElmTestDiscoveryCaches> _discoveryCaches =
        new(() => discoveryCaches ?? ElmTestDiscoveryCaches.CreateInMemory());

    private long _accessSequence;

    private sealed class Snapshot(string projectUri, string version, ElmTestSourceSnapshot source)
    {
        public string ProjectUri { get; } = projectUri;

        public string Version { get; } = version;

        public ElmTestSourceSnapshot Source { get; } = source;

        public IReadOnlyDictionary<string, int> DocumentVersions = source.DocumentVersions;

        public string TreeHash { get; } = TreeHashOf(source.Files);

        public long AccessSequence { get; set; }

        public int ActiveOperations { get; set; }

        public FileTree NavigationSources = source.Files;

        public SemaphoreSlim RunGate { get; } = new(1, 1);

        public SemaphoreSlim DiscoveryGate { get; } = new(1, 1);

        public SnapshotDiscovery? Discovery;
    }

    private sealed record SnapshotDiscovery(
        ElmTestDiscoveryResult Inventory, string Id, IReadOnlyDictionary<string, Location> Locations);

    /// <summary>Hash of the canonical sorted Pine FileTree encoding, not Git's tree hash or a JSON digest.</summary>
    public static string TreeHashOf(FileTree tree) =>
        Convert.ToHexStringLower(PineValueHashTree.ComputeHash(FileTreeEncoding2026.Encode(FileTree.Sort(tree))).Span);

    /// <summary>A version-qualified source URI that a client can resolve using pine/tests/source.</summary>
    public static string SourceUri(string projectUri, string version, string path) =>
        "pine-test-source:/" + Uri.EscapeDataString(version) + "/" +
        string.Join('/', path.Split('/').Select(Uri.EscapeDataString)) +
        "?project=" + Uri.EscapeDataString(projectUri);

    /// <summary>Discovers a complete inventory for exactly the requested or newly captured version.</summary>
    public async Task<PineTestsDiscoverResult> DiscoverAsync(
        PineTestsDiscoverParams parameters,
        CancellationToken cancellationToken = default)
    {
        using var linked = CancellationTokenSource.CreateLinkedTokenSource(cancellationToken, _lifetime.Token);
        var token = linked.Token;

        var snapshot =
            await Task.Run(() => ResolveSnapshotAsync(parameters, token, pin: true), token).ConfigureAwait(false);

        var capacityHeld = false;

        try
        {
            await _workCapacity.WaitAsync(token).ConfigureAwait(false);
            capacityHeld = true;
            var retained = await DiscoveryAsync(snapshot, token).ConfigureAwait(false);
            var discovery = retained.Inventory;

            var items = discovery.Items.Select(item => ToProtocolItem(retained, item)).ToArray();

            return
                new(
                    snapshot.ProjectUri,
                    snapshot.Version,
                    snapshot.Source.RootUri,
                    snapshot.TreeHash,
                    Complete: true,
                    items,
                    discovery.HasOnly,
                    discovery.HasSkipped,
                    Volatile.Read(ref snapshot.DocumentVersions),
                    discovery.ResolutionFingerprint,
                    DiscoveryId: retained.Id);
        }
        catch (Exception exception) when (IsPreparationError(exception))
        {
            log?.Invoke("Elm test discovery failed for " + snapshot.Version + ": " + exception);

            return
                new(
                    snapshot.ProjectUri,
                    snapshot.Version,
                    snapshot.Source.RootUri,
                    snapshot.TreeHash,
                    Complete: false,
                    Items: null,
                    HasOnly: false,
                    HasSkipped: false,
                    Volatile.Read(ref snapshot.DocumentVersions),
                    ResolutionFingerprint: null,
                    new("discoveryFailed", exception.Message));
        }
        finally
        {
            if (capacityHeld)
                _workCapacity.Release();

            Unpin(snapshot);
        }
    }

    /// <summary>Runs a version-qualified selection and streams ordered lifecycle and test-state events.</summary>
    public async Task<PineTestsRunResult> RunAsync(
        PineTestsRunParams parameters,
        CancellationToken cancellationToken = default)
    {
        ValidateVersion(parameters.Version);

        if (string.IsNullOrWhiteSpace(parameters.RunId) || parameters.RunId.Length > 200)
            throw new PineTestingProtocolException("invalidRunId", "runId must contain between 1 and 200 characters.");

        if (parameters.FuzzRuns == 0)
            throw new PineTestingProtocolException("invalidSettings", "fuzzRuns must be positive.");

        using var linked = CancellationTokenSource.CreateLinkedTokenSource(cancellationToken, _lifetime.Token);
        var token = linked.Token;

        var snapshot =
            await Task.Run(
                () => ResolveSnapshotAsync(new(parameters.ProjectUri, parameters.Version), token, pin: true),
                token).ConfigureAwait(false);

        if (!_activeRuns.TryAdd(parameters.RunId, 0))
        {
            Unpin(snapshot);
            throw new PineTestingProtocolException("duplicateRunId", "runId is already active: " + parameters.RunId);
        }

        var settings = new ElmFuzzOptions { Seed = parameters.Seed, Runs = parameters.FuzzRuns }.Resolve();
        var results = new List<PineTestResult>();
        var selected = Array.Empty<ElmTestDiscoveryItem>();
        var status = "errored";
        var incomplete = false;
        PineTestingError? error = null;
        string? resolutionFingerprint = null;
        string? discoveryId = null;
        long sequence = 0;
        var gateHeld = false;
        var capacityHeld = false;

        Task Send(
            string kind,
            string? id = null,
            string? state = null,
            PineTestResult? result = null,
            string? message = null) =>
            publishEvent?.Invoke(
                new(snapshot.ProjectUri, snapshot.Version, parameters.RunId, ++sequence, kind, id, state, result, message))
            ?? Task.CompletedTask;

        try
        {
            await Send("phase", state: "waiting").ConfigureAwait(false);
            await snapshot.RunGate.WaitAsync(token).ConfigureAwait(false);
            gateHeld = true;
            await _workCapacity.WaitAsync(token).ConfigureAwait(false);
            capacityHeld = true;

            await Send("phase", state: "discovery").ConfigureAwait(false);
            var retained = await DiscoveryAsync(snapshot, token).ConfigureAwait(false);
            var discovery = retained.Inventory;
            resolutionFingerprint = discovery.ResolutionFingerprint;
            discoveryId = retained.Id;

            if (parameters.DiscoveryId is { } expected && expected != discoveryId)
            {
                throw new PineTestingProtocolException(
                    "staleDiscovery",
                    "The inventory's dependency/compiler identity changed. Discover this version again before selecting tests.");
            }

            if (parameters.DiscoveryId is null && (parameters.Include is not null || parameters.Exclude?.Count > 0))
            {
                throw new PineTestingProtocolException(
                    "discoveryIdRequired",
                    "Selected runs require the discoveryId returned by discovery.");
            }

            incomplete = discovery.HasOnly || discovery.HasSkipped;
            selected = Select(discovery, parameters.Include, parameters.Exclude);

            foreach (var item in selected)
                await Send("test", item.Id, "queued").ConfigureAwait(false);

            foreach (var item in selected.Where(
                item =>
                item.Selection is not ElmTestDiscoverySelection.Included ||
                item.Kind is ElmTestDiscoveryItemKind.Todo || item.IsEmptyGroup))
            {
                PineTestResult result;

                if (item.Selection is ElmTestDiscoverySelection.Included && item.IsEmptyGroup)
                {
                    result =
                        ToProtocolResult(
                            retained,
                            item,
                            new CompletedTest([], CompletedTestKind.FailedEmptyGroup, null),
                            TimeSpan.Zero,
                            settings);
                }
                else
                {
                    var message =
                        item.Selection switch
                        {
                            ElmTestDiscoverySelection.Skipped => "Skipped by Test.skip.",

                            ElmTestDiscoverySelection.ExcludedByOnly =>
                            "Excluded by Test.only elsewhere in this suite.",

                            _ =>
                            "TODO: " + item.ListedTest.Name
                        };

                    result = new(item.Id, "skipped", 0, [new(message)]);
                    incomplete = true;
                }

                results.Add(result);
                await Send("test", item.Id, result.State, result).ConfigureAwait(false);
            }

            await Send("phase", state: "preparation").ConfigureAwait(false);
            token.ThrowIfCancellationRequested();

            var executable =
                selected.Where(
                    item => item.Selection is ElmTestDiscoverySelection.Included &&
                        item.Kind is ElmTestDiscoveryItemKind.Unit or ElmTestDiscoveryItemKind.Fuzz)
                .ToArray();

            var entries =
                executable.Length == 0
                ?
                ImmutableDictionary<string, ElmTestExecutionEntry>.Empty
                :
                await Task.Run(
                    () =>
                    {
                        var cache = _cache.Value;
                        using var invocation = cache.BeginCompilationInvocation();

                        var prepared =
                            discovery.PrepareExecution(
                                executable.Select(item => item.Id),
                                settings,
                                token,
                                cache.CompilationCaches);

                        token.ThrowIfCancellationRequested();
                        return prepared;
                    },
                    token).ConfigureAwait(false);

            await Send("phase", state: "running").ConfigureAwait(false);
            token.ThrowIfCancellationRequested();

            if (executable.Length > 0)
            {
                await Task.Run(
                    async () =>
                    {
                        foreach (var item in executable)
                        {
                            token.ThrowIfCancellationRequested();
                            await Send("test", item.Id, "started").ConfigureAwait(false);
                            var watch = Stopwatch.StartNew();

                            var completed =
                                ElmTestRunner.ExecuteDiscoveredTest(item, entries[item.Id], _cache.Value, token);

                            var result = ToProtocolResult(retained, item, completed, watch.Elapsed, settings);
                            results.Add(result);
                            await Send("test", item.Id, result.State, result).ConfigureAwait(false);
                        }
                    },
                    token).ConfigureAwait(false);
            }

            status = "completed";
        }
        catch (OperationCanceledException) when (token.IsCancellationRequested)
        {
            status = "cancelled";
            incomplete = true;
        }
        catch (PineTestingProtocolException exception)
        {
            error = new(exception.Code, exception.Message);
            incomplete = true;
            log?.Invoke("Elm test run rejected: " + exception.Message);
        }
        catch (Exception exception) when (IsPreparationError(exception))
        {
            error = new("executionFailed", exception.Message);
            incomplete = true;
            log?.Invoke("Elm test run failed for " + snapshot.Version + ": " + exception);
        }
        finally
        {
            try
            {
                var finished = results.Select(result => result.TestId).ToHashSet(StringComparer.Ordinal);

                foreach (var item in selected.Where(item => !finished.Contains(item.Id)))
                {
                    var result =
                        new PineTestResult(
                            item.Id,
                            status == "cancelled" ? "skipped" : "errored",
                            0,
                            [new(error?.Message ?? "Run ended before this test completed.")]);

                    results.Add(result);
                    await Send("test", item.Id, result.State, result).ConfigureAwait(false);
                }

                await Send("end", state: status, message: error?.Message).ConfigureAwait(false);
            }
            finally
            {
                if (capacityHeld)
                    _workCapacity.Release();

                if (gateHeld)
                    snapshot.RunGate.Release();

                _activeRuns.TryRemove(parameters.RunId, out _);
                Unpin(snapshot);
            }
        }

        return
            new(
                snapshot.ProjectUri,
                snapshot.Version,
                parameters.RunId,
                status,
                settings,
                incomplete,
                results,
                error,
                resolutionFingerprint,
                discoveryId);
    }

    /// <summary>Returns retained original text, never the current content of a historical source path.</summary>
    public PineTestsSourceResult GetSource(PineTestsSourceParams parameters)
    {
        var project = DirectoryUri(parameters.ProjectUri);
        ValidateVersion(parameters.Version);

        if (string.IsNullOrEmpty(parameters.Path))
            throw new PineTestingProtocolException("invalidSource", "A source path is required.");

        ValidatePath(parameters.Path.Split('/'));
        var snapshot = Find(project, parameters.Version) ?? throw VersionUnavailable(parameters.Version);
        var path = parameters.Path.Split('/');

        var node =
            snapshot.Source.Files.GetNodeAtPath(path) ??
            Volatile.Read(ref snapshot.NavigationSources).GetNodeAtPath(path);

        if (node is not FileTree.FileNode file)
        {
            throw new PineTestingProtocolException(
                "sourceUnavailable",
                "Source is not part of this test version: " + parameters.Path);
        }

        try
        {
            return new(project, parameters.Version, parameters.Path, s_utf8.GetString(file.Bytes.Span));
        }
        catch (DecoderFallbackException)
        {
            throw new PineTestingProtocolException("invalidSource", "Requested source is not valid UTF-8: " + parameters.Path);
        }
    }

    private async Task<SnapshotDiscovery> DiscoveryAsync(Snapshot snapshot, CancellationToken token)
    {
        await snapshot.DiscoveryGate.WaitAsync(token).ConfigureAwait(false);

        try
        {
            if (snapshot.Discovery is { } existing)
                return existing;

            var manifestPath = RelativePath(snapshot.Source.RootUri, snapshot.ProjectUri).Append("elm.json").ToArray();

            var discovery =
                await Task.Run(
                    () =>
                    {
                        var inventory =
                            ElmTestDiscovery.DiscoverTests(
                                snapshot.Source.Files,
                                packageProvider: packageProvider,
                                cancellationToken: token,
                                manifestPath: manifestPath,
                                caches: _discoveryCaches.Value);

                        token.ThrowIfCancellationRequested();

                        return
                            new SnapshotDiscovery(
                                inventory,
                                DiscoveryId(snapshot, inventory),
                                BuildLocations(snapshot, inventory, token));
                    },
                    token).ConfigureAwait(false);

            token.ThrowIfCancellationRequested();
            Volatile.Write(ref snapshot.NavigationSources, discovery.Inventory.SourceFiles);
            return snapshot.Discovery = discovery;
        }
        finally
        {
            snapshot.DiscoveryGate.Release();
        }
    }

    private static string DiscoveryId(Snapshot snapshot, ElmTestDiscoveryResult discovery) =>
        "discovery-" +
        Convert.ToHexStringLower(
            SHA256.HashData(
                Encoding.UTF8.GetBytes(
                    System.Text.Json.JsonSerializer.Serialize(
                        new
                        {
                            snapshot.Version,
                            discovery.ResolutionFingerprint,
                            Items =
                            discovery.Items.Select(
                                item => new { item.Id, item.ParentId, item.Kind, item.Selection, item.ListedTest })
                        }))));

    private async Task<Snapshot> ResolveSnapshotAsync(
        PineTestsDiscoverParams parameters,
        CancellationToken token,
        bool pin = false)
    {
        var project = DirectoryUri(parameters.ProjectUri);
        ElmTestSourceSnapshot source;

        if (parameters.Version is { } version)
        {
            ValidateVersion(version);

            if (parameters.Files is null)
            {
                if (parameters.SourceRootUri is not null)
                    throw new PineTestingProtocolException("invalidSource", "sourceRootUri requires explicit files.");

                if (Find(project, version, pin) is { } retained)
                    return retained;

                if (version.StartsWith("file-tree-", StringComparison.Ordinal))
                {
                    lock (_snapshotLock)
                    {
                        source =
                            _snapshots.Values
                            .Where(
                                snapshot =>
                                snapshot.Version == version &&
                                new Uri(snapshot.Source.RootUri).IsBaseOf(new Uri(project)))
                            .OrderByDescending(snapshot => snapshot.Source.RootUri.Length)
                            .FirstOrDefault()?.Source
                            ?? throw VersionUnavailable(version);
                    }
                }
                else
                {
                    if (commitSource is null)
                        throw VersionUnavailable(version);

                    source =
                        await commitSource.LoadCommitAsync(project, version["commit-".Length..], token).ConfigureAwait(false);
                }
            }
            else
            {
                if (!version.StartsWith("file-tree-", StringComparison.Ordinal))
                    throw new PineTestingProtocolException("invalidSource", "Explicit files require a file-tree version.");

                var paths = new HashSet<string>(StringComparer.Ordinal);
                var directories = new HashSet<string>(StringComparer.Ordinal);
                var files = new List<(IReadOnlyList<string>, ReadOnlyMemory<byte>)>();
                long size = 0;

                foreach (var file in parameters.Files)
                {
                    token.ThrowIfCancellationRequested();

                    if (file is null)
                        throw new PineTestingProtocolException("invalidSource", "Source entries must not be null.");

                    ValidatePath(file.Path);
                    var path = string.Join('/', file.Path);

                    if (!paths.Add(path))
                    {
                        throw new PineTestingProtocolException(
                            "invalidSource",
                            "Duplicate source path: " + string.Join('/', file.Path));
                    }

                    if (directories.Contains(path))
                    {
                        throw new PineTestingProtocolException(
                            "invalidSource",
                            "A source path is both a file and directory: " + path);
                    }

                    for (var index = 1; index < file.Path.Count; ++index)
                    {
                        var parent = string.Join('/', file.Path.Take(index));

                        if (paths.Contains(parent))
                        {
                            throw new PineTestingProtocolException(
                                "invalidSource",
                                "A source path is both a file and directory: " + parent);
                        }

                        directories.Add(parent);
                    }

                    byte[] bytes;

                    try
                    {
                        bytes = Convert.FromBase64String(file.ContentBase64);
                    }
                    catch (FormatException exception)
                    {
                        throw new PineTestingProtocolException(
                            "invalidSource",
                            "Invalid base64 source content: " + exception.Message);
                    }
                    catch (ArgumentNullException)
                    {
                        throw new PineTestingProtocolException("invalidSource", "Source contentBase64 must not be null.");
                    }

                    size += bytes.Length;

                    if (size > MaximumSnapshotBytes)
                        throw new PineTestingProtocolException("snapshotTooLarge", "Snapshot exceeds 64 MiB.");

                    files.Add((file.Path, bytes));
                }

                source =
                    new(
                        DirectoryUri(parameters.SourceRootUri ?? project),
                        FileTree.FromSetOfFilesWithStringPath(files),
                        new Dictionary<string, int>());

                if (version != "file-tree-" + TreeHashOf(source.Files))
                {
                    throw new PineTestingProtocolException(
                        "versionMismatch",
                        "Supplied files do not match the declared Pine FileTree hash.");
                }
            }
        }
        else
        {
            if (parameters.Files is not null || parameters.SourceRootUri is not null)
                throw new PineTestingProtocolException("invalidSource", "Explicit source trees require a file-tree version.");

            source = await captureWorkspace(project, token).ConfigureAwait(false);
        }

        token.ThrowIfCancellationRequested();
        var root = DirectoryUri(source.RootUri);
        var manifestPath = RelativePath(root, project).Append("elm.json").ToArray();

        if (source.Files.GetNodeAtPath(manifestPath) is not FileTree.FileNode)
            throw new PineTestingProtocolException("projectUnavailable", "Version does not contain the project's elm.json.");

        long totalBytes = 0;

        foreach (var file in source.Files.EnumerateFilesTransitive())
        {
            ValidatePath(file.path);
            totalBytes += file.fileContent.Length;

            if (totalBytes > MaximumSnapshotBytes)
                throw new PineTestingProtocolException("snapshotTooLarge", "Snapshot exceeds 64 MiB.");

            if (file.path[^1] == "elm.json" || file.path[^1].EndsWith(".elm", StringComparison.OrdinalIgnoreCase))
            {
                try
                {
                    _ = s_utf8.GetString(file.fileContent.Span);
                }
                catch (DecoderFallbackException)
                {
                    throw new PineTestingProtocolException(
                        "invalidSource",
                        "Elm input is not valid UTF-8: " + string.Join('/', file.path));
                }
            }
        }

        var resolvedVersion = parameters.Version ?? "file-tree-" + TreeHashOf(source.Files);
        var snapshot = new Snapshot(project, resolvedVersion, source with { RootUri = root });

        lock (_snapshotLock)
        {
            if (_snapshots.TryGetValue((project, resolvedVersion), out var existing))
            {
                if (existing.TreeHash != snapshot.TreeHash || existing.Source.RootUri != root)
                {
                    throw new PineTestingProtocolException(
                        "versionMismatch",
                        "This version was already bound to different input bytes or a source root.");
                }

                existing.AccessSequence = ++_accessSequence;

                if (pin)
                    ++existing.ActiveOperations;

                if (parameters.Version is null)
                    Volatile.Write(ref existing.DocumentVersions, source.DocumentVersions);

                return existing;
            }

            var rootVersions =
                _snapshots.Values.Where(item => item.Source.RootUri == root).GroupBy(item => item.Version).ToArray();

            var sameSource = rootVersions.SingleOrDefault(group => group.Key == resolvedVersion)?.FirstOrDefault();

            if (sameSource is not null)
            {
                if (sameSource.TreeHash != snapshot.TreeHash)
                {
                    throw new PineTestingProtocolException(
                        "versionMismatch",
                        "This source version was already bound to different bytes.");
                }

                snapshot =
                    new(
                        project,
                        resolvedVersion,
                        source with { RootUri = root, Files = sameSource.Source.Files });
            }
            else if (rootVersions.Length >= RetainedVersions)
            {
                var candidates = rootVersions.Where(group => group.All(item => item.ActiveOperations == 0)).ToArray();

                if (candidates.Length == 0)
                {
                    throw new PineTestingProtocolException(
                        "snapshotCapacity",
                        "All retained versions have active requests. Retry after a request completes.");
                }

                var oldest = candidates.MinBy(group => group.Max(item => item.AccessSequence))!;

                foreach (var removed in oldest)
                    _snapshots.Remove((removed.ProjectUri, removed.Version));
            }

            snapshot.AccessSequence = ++_accessSequence;

            if (pin)
                ++snapshot.ActiveOperations;

            _snapshots.Add((project, resolvedVersion), snapshot);
        }

        return snapshot;
    }

    private Snapshot? Find(string project, string version, bool pin = false)
    {
        lock (_snapshotLock)
        {
            if (!_snapshots.TryGetValue((project, version), out var snapshot))
                return null;

            snapshot.AccessSequence = ++_accessSequence;

            if (pin)
                ++snapshot.ActiveOperations;

            return snapshot;
        }
    }

    private void Unpin(Snapshot snapshot)
    {
        lock (_snapshotLock)
        {
            --snapshot.ActiveOperations;
        }
    }

    private static ElmTestDiscoveryItem[] Select(
        ElmTestDiscoveryResult discovery,
        IReadOnlyList<string>? include,
        IReadOnlyList<string>? exclude)
    {
        var byId = discovery.ItemsById;
        var included = include?.ToHashSet(StringComparer.Ordinal);
        var excluded = (exclude ?? []).ToHashSet(StringComparer.Ordinal);

        foreach (var id in (include ?? []).Concat(exclude ?? []))
        {
            if (!byId.ContainsKey(id))
                throw new PineTestingProtocolException("unknownTest", "Unknown test ID in the requested version: " + id);
        }

        if (included is { Count: 0 })
            return [];

        return
            discovery.Items.Where(
                item => (item.Kind is not ElmTestDiscoveryItemKind.Group || item.IsEmptyGroup) &&
                    (included is null || HasAncestor(item, included)) &&
                    !HasAncestor(item, excluded)).ToArray();

        bool HasAncestor(ElmTestDiscoveryItem item, IReadOnlySet<string> ids)
        {
            while (true)
            {
                if (ids.Contains(item.Id))
                    return true;

                if (item.ParentId is not { } parent)
                    return false;

                item = byId[parent];
            }
        }
    }

    private static PineTestItem ToProtocolItem(
        SnapshotDiscovery discovery,
        ElmTestDiscoveryItem item) =>
        new(
            item.Id,
            item.ParentId,
            item.ListedTest.Name,
            item.ListedTest.FilePath,
            item.RootModuleName,
            item.RootDeclarationName,
            item.ListedTest.DescriptionPath,
            item.Kind,
            item.Selection,
            item.IsOnly,
            item.IsEmptyGroup,
            item.FuzzRuns,
            item.SourceLocation.FilePath,
            discovery.Locations[item.Id]);

    private static IReadOnlyDictionary<string, Location> BuildLocations(
        Snapshot snapshot, ElmTestDiscoveryResult discovery, CancellationToken token)
    {
        var locations = new Dictionary<string, Location>(StringComparer.Ordinal);

        foreach (var group in discovery.Items.GroupBy(item => item.SourceLocation.FilePath, StringComparer.Ordinal))
        {
            token.ThrowIfCancellationRequested();
            var path = group.Key.Split('/');

            var node =
                discovery.SourceFiles.GetNodeAtPath(path) as FileTree.FileNode
                ?? throw new InvalidOperationException("Missing original source for discovered test: " + group.Key);

            var lines = s_utf8.GetString(node.Bytes.Span).Split('\n');

            var uri =
                snapshot.Source.Files.GetNodeAtPath(path) is FileTree.FileNode
                ?
                new Uri(
                    new Uri(snapshot.Source.RootUri),
                    string.Join('/', path.Select(Uri.EscapeDataString))).AbsoluteUri
                :
                SourceUri(snapshot.ProjectUri, snapshot.Version, group.Key);

            foreach (var item in group)
            {
                token.ThrowIfCancellationRequested();
                var range = item.SourceLocation.Range;

                locations.Add(
                    item.Id,
                    new(
                        uri,
                        new ProtocolRange(ConvertPosition(range.Start), ConvertPosition(range.End))));
            }

            Position ConvertPosition(SyntaxLocation location)
            {
                if (location.Row < 1 || location.Row > lines.Length || location.Column < 1)
                    throw new InvalidOperationException("Invalid Elm test source position in " + group.Key);

                var line = lines[location.Row - 1];
                var column = 0;
                var utf16 = 0;

                foreach (var rune in line.EnumerateRunes())
                {
                    if (column == location.Column - 1)
                        break;

                    ++column;
                    utf16 += rune.Utf16SequenceLength;
                }

                if (column != location.Column - 1)
                    throw new InvalidOperationException("Elm test source position exceeds line length in " + group.Key);

                return new(checked((uint)location.Row - 1), checked((uint)utf16));
            }
        }

        return locations;
    }

    private static PineTestResult ToProtocolResult(
        SnapshotDiscovery discovery,
        ElmTestDiscoveryItem item,
        CompletedTest completed,
        TimeSpan duration,
        ElmTestExecutionSettings settings)
    {
        var messages = new List<PineTestMessage>();
        var location = discovery.Locations[item.Id];

        switch (completed.Failure)
        {
            case EqualityFailure equality:
                messages.Add(new(equality.Description, equality.Expected, equality.Actual, location));
                break;

            case MessageFailure message:
                messages.Add(new(message.Message, Location: location));
                break;

            case null:
                if (completed.Kind is CompletedTestKind.FailedEmptyGroup)
                    messages.Add(new("This describe group contains no tests.", Location: location));

                break;

            default:
                throw new NotImplementedException(
                    nameof(ToProtocolResult) + " does not handle test failure: " + completed.Failure.GetType().Name);
        }

        var state =
            completed.EvaluationError is not null
            ?
            "errored"
            :
            completed.Kind switch
            {
                CompletedTestKind.Passed => "passed",
                CompletedTestKind.Failed or CompletedTestKind.FailedEmptyGroup => "failed",
                CompletedTestKind.Todo => "skipped",

                _ =>
                throw new NotImplementedException(nameof(ToProtocolResult) + " does not handle " + completed.Kind)
            };

        var reproduction =
            completed.Fuzz is null
            ?
            null
            :
            "pine elm test --seed " + settings.Seed + " --fuzz " + settings.FuzzRuns +
            " --filter '" + item.ExecutionFilter!.Replace("'", "'\\''", StringComparison.Ordinal) + "'";

        return new(item.Id, state, duration.TotalMilliseconds, messages, completed.Fuzz, reproduction);
    }

    internal static string DirectoryUri(string uri)
    {
        if (!Uri.TryCreate(uri, UriKind.Absolute, out var parsed) || parsed.Query.Length > 0 ||
            parsed.Fragment.Length > 0)
        {
            throw new PineTestingProtocolException(
                "invalidUri",
                "Expected an absolute directory URI without query or fragment: " + uri);
        }

        var normalized = WorkspaceDocumentUri.Normalize(parsed).AbsoluteUri;
        return normalized.EndsWith('/') ? normalized : normalized + "/";
    }

    internal static string[] RelativePath(string root, string uri)
    {
        var rootUri = WorkspaceDocumentUri.Normalize(new Uri(root));
        var documentUri = WorkspaceDocumentUri.Normalize(new Uri(uri));

        if (!rootUri.IsBaseOf(documentUri))
            throw new PineTestingProtocolException("invalidSource", "Project or document is outside its source root: " + uri);

        var path =
            rootUri.MakeRelativeUri(documentUri).OriginalString.Split('/', StringSplitOptions.RemoveEmptyEntries)
            .Select(Uri.UnescapeDataString).ToArray();

        if (path.Length > 0)
            ValidatePath(path);

        return path;
    }

    private static void ValidatePath(IReadOnlyList<string> path)
    {
        if (path is null || path.Count == 0 ||
            path.Any(
                segment =>
                string.IsNullOrEmpty(segment) || segment is "." or ".." || segment.IndexOfAny(['/', '\\', '\0']) >= 0))
        {
            throw new PineTestingProtocolException(
                "invalidSource",
                "Source paths must contain nonempty relative path segments, without separators or traversal.");
        }
    }

    private static void ValidateVersion(string version)
    {
        if (string.IsNullOrEmpty(version))
            throw new PineTestingProtocolException("invalidVersion", "An explicit immutable version is required.");

        var prefix =
            version.StartsWith("commit-", StringComparison.Ordinal)
            ?
            "commit-"
            :
            version.StartsWith("file-tree-", StringComparison.Ordinal)
            ?
            "file-tree-"
            :
            throw new PineTestingProtocolException("invalidVersion", "Expected commit-<sha> or file-tree-<sha>.");

        var hash = version.AsSpan(prefix.Length);

        if ((prefix == "file-tree-" ? hash.Length != 64 : hash.Length is not (40 or 64)) ||
            hash.ContainsAnyExcept("0123456789abcdef"))
        {
            throw new PineTestingProtocolException(
                "invalidVersion",
                "Version hashes must be full lowercase hexadecimal hashes.");
        }
    }

    private static PineTestingProtocolException VersionUnavailable(string version) =>
        new("versionUnavailable", "Version is not retained. Rediscover a commit or supply its file tree: " + version);

    private static bool IsPreparationError(Exception exception) =>
        exception is ElmDependencyResolutionException or ElmCompilationException or
            InvalidOperationException or ArgumentException or IOException or System.Text.Json.JsonException;

    /// <summary>Cancels active work and releases session-owned cache learning.</summary>
    public void Dispose()
    {
        _lifetime.Cancel();

        if (_cache.IsValueCreated)
            _cache.Value.Dispose();
        if (_discoveryCaches.IsValueCreated)
            _discoveryCaches.Value.WaitForMaintenanceAsync().GetAwaiter().GetResult();

        lock (_snapshotLock)
        {
            _snapshots.Clear();
        }
    }
}
