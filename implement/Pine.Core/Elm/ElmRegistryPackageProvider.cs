using Pine.Core.Files;
using Pine.Core.IO;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.IO;
using System.Linq;
using System.Net;
using System.Net.Http;
using System.Text;
using System.Text.Json;
using System.Threading;
using System.Threading.Tasks;

namespace Pine.Core.Elm;

/// <summary>
/// Discovers published versions and package manifests independently of source archives. Offline mode
/// performs no HTTP requests and never treats an incomplete local registry as proof of unsatisfiability.
/// </summary>
public sealed class ElmRegistryPackageProvider : IElmPackageProvider
{
    private static readonly HttpClient s_httpClient = new();

    private readonly bool _offline;

    private readonly string _cacheDirectory;

    private readonly IReadOnlyList<string> _sourceCacheDirectories;

    private readonly string _elmHome;

    private readonly string _compilerVersion;

    private Dictionary<string, string[]>? _registry;

    /// <summary>Configures registry/cache access, including the compiler-specific ELM_HOME package directory.</summary>
    public ElmRegistryPackageProvider(
        bool offline = false,
        string? cacheDirectory = null,
        IReadOnlyList<string>? sourceCacheDirectories = null,
        string? elmHome = null,
        ElmPackageVersion? compilerVersion = null)
    {
        _offline = offline;
        _cacheDirectory = cacheDirectory ?? Path.Combine(Filesystem.CacheDirectory, "elm-package-metadata");
        _sourceCacheDirectories = sourceCacheDirectories ?? ElmPackageSource.LocalCacheDirectoriesDefault;

        _elmHome =
            elmHome ??
            Environment.GetEnvironmentVariable("ELM_HOME") ??
            Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.ApplicationData), "elm");

        _compilerVersion = (compilerVersion ?? ElmPackageVersion.Parse("0.19.1")).ToString();
    }

    /// <inheritdoc/>
    public async Task<ElmPackageVersionListing> GetVersionsAsync(
        string packageName,
        CancellationToken cancellationToken)
    {
        ElmDependencyResolver.ValidatePackageName(packageName);
        var registryPath = Path.Combine(_cacheDirectory, "all-packages.json");
        const string url = "https://package.elm-lang.org/all-packages";

        if (_registry is null)
        {
            var bytes =
                _offline
                ?
                File.Exists(registryPath) ? await ReadAsync(registryPath, cancellationToken) : "{}"u8.ToArray()
                :
                await DownloadAsync(url, cancellationToken);

            try
            {
                _registry =
                    JsonSerializer.Deserialize<Dictionary<string, string[]>>(bytes)
                    ?? throw new JsonException("Registry is JSON null.");
            }
            catch (JsonException exception)
            {
                throw new ElmPackageProviderException(
                    ElmResolutionFailureKind.ProviderFailure,
                    _offline ? registryPath : url,
                    "Invalid Elm package registry: " + exception.Message,
                    exception);
            }

            if (!_offline)
                await WriteAsync(registryPath, bytes, cancellationToken);
        }

        try
        {
            var versions = (_registry.GetValueOrDefault(packageName) ?? []).Select(ElmPackageVersion.Parse).ToList();
            var metadataDirectory = PackageDirectory(_cacheDirectory, packageName);
            var elmDirectory = PackageDirectory(Path.Combine(_elmHome, _compilerVersion, "packages"), packageName);

            foreach (var directory in new[] { metadataDirectory, elmDirectory })
            {
                if (Directory.Exists(directory))
                {
                    foreach (var path in Directory.EnumerateDirectories(directory))
                        if (File.Exists(Path.Combine(path, "elm.json")))
                            versions.Add(ElmPackageVersion.Parse(Path.GetFileName(path)));
                }
            }

            foreach (var directory in _sourceCacheDirectories)
            {
                var zipDirectory =
                    Path.GetDirectoryName(ElmPackageSource.GetLocalZipPath(directory, packageName, "0.0.0"))!;

                var filePrefix = packageName.Split('/')[1] + "@";

                if (Directory.Exists(zipDirectory))
                {
                    foreach (var file in Directory.EnumerateFiles(zipDirectory, filePrefix + "*.zip"))
                        versions.Add(ElmPackageVersion.Parse(Path.GetFileName(file)[filePrefix.Length..^4]));
                }
            }

            return new([.. versions.Distinct().OrderBy(version => version)], _offline ? registryPath : url, !_offline);
        }
        catch (Exception exception) when (exception is FormatException or ArgumentException or IOException or UnauthorizedAccessException)
        {
            throw new ElmPackageProviderException(
                ElmResolutionFailureKind.ProviderFailure,
                _offline ? registryPath : url,
                $"Cannot enumerate versions of '{packageName}': {exception.Message}",
                exception);
        }
    }

    /// <inheritdoc/>
    public async Task<ElmPackageMetadata> GetMetadataAsync(
        ElmPackageIdentity identity,
        CancellationToken cancellationToken)
    {
        ElmDependencyResolver.ValidatePackageName(identity.Name);

        var cachePath =
            Path.Combine(PackageDirectory(_cacheDirectory, identity.Name), identity.Version.ToString(), "elm.json");

        var elmPath =
            Path.Combine(
                PackageDirectory(Path.Combine(_elmHome, _compilerVersion, "packages"), identity.Name),
                identity.Version.ToString(),
                "elm.json");

        byte[] bytes;
        string origin;

        if (File.Exists(cachePath) || File.Exists(elmPath))
        {
            origin = File.Exists(cachePath) ? cachePath : elmPath;
            bytes = await ReadAsync(origin, cancellationToken);
        }
        else if (_sourceCacheDirectories.Any(
            directory =>
            File.Exists(
                ElmPackageSource.GetLocalZipPath(directory, identity.Name, identity.Version.ToString()))))
        {
            var files =
                await ElmPackageSource.LoadElmPackageAsync(
                    identity.Name,
                    identity.Version.ToString(),
                    _sourceCacheDirectories,
                    offline: true,
                    cancellationToken);

            if (!files.TryGetValue(["elm.json"], out var file))
            {
                throw new ElmPackageProviderException(
                    ElmResolutionFailureKind.ProviderFailure,
                    identity.ToString(),
                    $"Cached source archive for '{identity}' contains no elm.json.");
            }

            bytes = file.ToArray();
            origin = "cached source archive:" + identity;
        }
        else
        {
            origin = $"https://package.elm-lang.org/packages/{identity.Name}/{identity.Version}/elm.json";

            if (_offline)
            {
                throw new ElmPackageProviderException(
                    ElmResolutionFailureKind.OfflineUnavailable,
                    cachePath,
                    $"Metadata for '{identity}' is not cached. Retry online before using offline mode.");
            }

            bytes = await DownloadAsync(origin, cancellationToken, ElmResolutionFailureKind.NoMatchingVersion);
            await WriteAsync(cachePath, bytes, cancellationToken);
        }

        try
        {
            var manifest =
                ElmDependencyResolver.ReadManifest(
                    FileTree.FromSetOfFilesWithStringPath([(new[] { "elm.json" }, (ReadOnlyMemory<byte>)bytes)]),
                    ["elm.json"]);

            return new(identity, manifest, origin) { ManifestText = Encoding.UTF8.GetString(bytes) };
        }
        catch (JsonException exception)
        {
            throw new ElmPackageProviderException(
                ElmResolutionFailureKind.InvalidManifest,
                origin,
                $"Cannot parse metadata for '{identity}': {exception.Message}",
                exception);
        }
    }

    /// <inheritdoc/>
    public async Task<FileTree> GetSourcesAsync(ElmPackageIdentity identity, CancellationToken cancellationToken)
    {
        var elmDirectory =
            Path.Combine(
                PackageDirectory(Path.Combine(_elmHome, _compilerVersion, "packages"), identity.Name),
                identity.Version.ToString());

        try
        {
            if (File.Exists(Path.Combine(elmDirectory, "elm.json")) &&
                Directory.Exists(Path.Combine(elmDirectory, "src")))
            {
                cancellationToken.ThrowIfCancellationRequested();

                return
                    FileTree.FromSetOfFilesWithStringPath(
                        Filesystem.GetFilesFromDirectory(elmDirectory, _ => true)
                        .Select(file => (file.path, file.content)));
            }

            return
                FileTree.FromSetOfFilesWithStringPath(
                    await ElmPackageSource.LoadElmPackageAsync(
                        identity.Name,
                        identity.Version.ToString(),
                        _sourceCacheDirectories,
                        _offline,
                        cancellationToken));
        }
        catch (Exception exception) when (exception is IOException or HttpRequestException or UnauthorizedAccessException ||
            exception is TaskCanceledException && !cancellationToken.IsCancellationRequested)
        {
            throw new ElmPackageProviderException(
                _offline && exception is FileNotFoundException
                ?
                ElmResolutionFailureKind.OfflineUnavailable
                :
                ElmResolutionFailureKind.ProviderFailure,
                identity.ToString(),
                $"Cannot load sources for '{identity}': {exception.Message}",
                exception);
        }
    }

    private static string PackageDirectory(string directory, string name) =>
        Path.Combine(directory, name.Split('/')[0], name.Split('/')[1]);

    private static async Task<byte[]> DownloadAsync(
        string url,
        CancellationToken cancellationToken,
        ElmResolutionFailureKind notFoundKind = ElmResolutionFailureKind.ProviderFailure)
    {
        try
        {
            return await s_httpClient.GetByteArrayAsync(url, cancellationToken);
        }
        catch (HttpRequestException exception) when (exception.StatusCode is HttpStatusCode.NotFound &&
            notFoundKind is ElmResolutionFailureKind.NoMatchingVersion)
        {
            throw new ElmPackageProviderException(
                notFoundKind,
                url,
                $"Requested Elm package release does not exist at '{url}' (HTTP 404). Check the package name and exact version.",
                exception);
        }
        catch (HttpRequestException exception)
        {
            throw new ElmPackageProviderException(
                ElmResolutionFailureKind.ProviderFailure,
                url,
                $"Failed retrieving Elm package data: {exception.Message}. This is a network/registry error, not a version conflict.",
                exception);
        }
        catch (TaskCanceledException exception) when (!cancellationToken.IsCancellationRequested)
        {
            throw new ElmPackageProviderException(
                ElmResolutionFailureKind.ProviderFailure,
                url,
                "Timed out retrieving Elm package data. This is a network/registry error, not a version conflict.",
                exception);
        }
    }

    private static async Task<byte[]> ReadAsync(string path, CancellationToken cancellationToken)
    {
        try
        {
            return await File.ReadAllBytesAsync(path, cancellationToken);
        }
        catch (Exception exception) when (exception is IOException or UnauthorizedAccessException)
        {
            throw new ElmPackageProviderException(
                ElmResolutionFailureKind.ProviderFailure,
                path,
                "Cannot read Elm package cache: " + exception.Message,
                exception);
        }
    }

    private static async Task WriteAsync(string path, byte[] bytes, CancellationToken cancellationToken)
    {
        var temporary = path + "." + Guid.NewGuid().ToString("N") + ".tmp";

        try
        {
            Directory.CreateDirectory(Path.GetDirectoryName(path)!);
            await File.WriteAllBytesAsync(temporary, bytes, cancellationToken);
            File.Move(temporary, path, overwrite: true);
        }
        catch (Exception exception) when (exception is IOException or UnauthorizedAccessException)
        {
            throw new ElmPackageProviderException(
                ElmResolutionFailureKind.ProviderFailure,
                path,
                "Cannot write Elm package cache: " + exception.Message,
                exception);
        }
        finally
        {
            if (File.Exists(temporary))
                File.Delete(temporary);
        }
    }
}
