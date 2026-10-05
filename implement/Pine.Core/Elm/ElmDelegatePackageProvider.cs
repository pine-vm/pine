using Pine.Core.Files;
using System;
using System.Collections.Generic;
using System.Text;
using System.Threading;
using System.Threading.Tasks;

namespace Pine.Core.Elm;

/// <summary>Adapts existing exact-version source loaders without losing manifest validation.</summary>
public sealed class ElmDelegatePackageProvider(
    Func<string, string, IReadOnlyDictionary<IReadOnlyList<string>, ReadOnlyMemory<byte>>> loadPackage,
    Func<string, ElmPackageVersionListing>? listVersions = null) : IElmPackageProvider
{
    private readonly Dictionary<ElmPackageIdentity, FileTree> _sources = [];

    /// <inheritdoc/>
    public Task<ElmPackageVersionListing> GetVersionsAsync(string packageName, CancellationToken cancellationToken)
    {
        cancellationToken.ThrowIfCancellationRequested();

        if (listVersions is null)
        {
            throw new ElmPackageProviderException(
                ElmResolutionFailureKind.ProviderFailure,
                "delegate package loader",
                $"Resolving a range for '{packageName}' requires a version-list provider. " +
                "Supply listVersions or use ElmRegistryPackageProvider; a range is not a downloadable version tag.");
        }

        return Task.FromResult(listVersions(packageName));
    }

    /// <inheritdoc/>
    public async Task<ElmPackageMetadata> GetMetadataAsync(
        ElmPackageIdentity identity,
        CancellationToken cancellationToken)
    {
        var sources = await GetSourcesAsync(identity, cancellationToken);

        return
            new(identity, ElmDependencyResolver.ReadManifest(sources, ["elm.json"]), "source-loader:" + identity)
            {
                ManifestText =
                sources.GetNodeAtPath(["elm.json"]) is FileTree.FileNode node
                ?
                Encoding.UTF8.GetString(node.Bytes.Span)
                :
                null,
            };
    }

    /// <inheritdoc/>
    public Task<FileTree> GetSourcesAsync(ElmPackageIdentity identity, CancellationToken cancellationToken)
    {
        cancellationToken.ThrowIfCancellationRequested();

        if (!_sources.TryGetValue(identity, out var sources))
        {
            sources = FileTree.FromSetOfFilesWithStringPath(loadPackage(identity.Name, identity.Version.ToString()));
            _sources.Add(identity, sources);
        }

        return Task.FromResult(sources);
    }
}
