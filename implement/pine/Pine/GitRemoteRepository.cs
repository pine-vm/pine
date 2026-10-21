using Pine.Core;
using Pine.Core.Files;
using Pine.Core.IO;
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Security.Cryptography;
using System.Text;
using System.Threading;

namespace Pine;

internal sealed class GitRemoteRepository
{
    internal sealed record Node(string Hash, bool IsDirectory);

    private readonly Func<IReadOnlyList<string>, IReadOnlyDictionary<string, ReadOnlyMemory<byte>>> _fetchBlobs;

    private readonly Dictionary<string, ReadOnlyMemory<byte>> _blobs = new(StringComparer.Ordinal);

    private readonly Dictionary<string, ReadOnlyMemory<byte>> _cachedFiles;

    private readonly Dictionary<string, IReadOnlyList<GitCore.GitObjects.TreeEntry>> _trees =
        new(StringComparer.Ordinal);

    private readonly string? _blobCacheDirectory;

    public GitCore.Repository Objects { get; }

    public GitCore.GitObjects.CommitObject Commit { get; }

    internal GitRemoteRepository(
        GitCore.Repository objects,
        string commit,
        Func<IReadOnlyList<string>, IReadOnlyDictionary<string, ReadOnlyMemory<byte>>> fetchBlobs,
        IReadOnlyDictionary<IReadOnlyList<string>, ReadOnlyMemory<byte>>? cachedFiles = null,
        string? blobCacheDirectory = null)
    {
        Objects = objects;
        var root = objects.GetObject(commit);

        if (root?.Type is not GitCore.PackFile.ObjectType.Commit)
            throw new IOException("Git commit not found: " + commit);

        Commit = GitCore.GitObjects.ParseCommit(root.Data);
        _fetchBlobs = fetchBlobs;

        _cachedFiles =
            cachedFiles?.ToDictionary(file => string.Join("/", file.Key), file => file.Value, StringComparer.Ordinal) ??
            [];

        _blobCacheDirectory = blobCacheDirectory;
    }

    internal static GitRemoteRepository Open(
        string cloneUrl,
        string commit,
        int? depth,
        CancellationToken token = default,
        IReadOnlyDictionary<IReadOnlyList<string>, ReadOnlyMemory<byte>>? cachedFiles = null,
        string? blobCacheDirectory = null)
    {
        token.ThrowIfCancellationRequested();
        var objects = GitCore.LoadFromUrl.FetchBloblessClone(cloneUrl, commit, depth);
        token.ThrowIfCancellationRequested();

        return
            new(
                objects,
                commit,
                hashes =>
                {
                    var pack =
                        GitCore.GitSmartHttp.FetchSpecificObjectsAsync(cloneUrl, hashes).GetAwaiter().GetResult();

                    var index = GitCore.PackIndex.GeneratePackIndexV2(pack);

                    return
                        GitCore.PackFile.ParseAllObjects(pack, GitCore.PackIndex.ParsePackIndexV2(index.IndexData))
                        .Where(item => item.Type is GitCore.PackFile.ObjectType.Blob)
                        .ToDictionary(item => item.SHA1base16, item => item.Data, StringComparer.Ordinal);
                },
                cachedFiles,
                blobCacheDirectory ?? Path.Combine(Filesystem.CacheDirectory, "git", "blobs"));
    }

    public Node? Find(IReadOnlyList<string> path)
    {
        var node = new Node(Commit.TreeHash, true);

        foreach (var segment in path)
        {
            if (!node.IsDirectory)
                return null;

            var entry = Entries(node.Hash).SingleOrDefault(item => item.Name == segment);

            if (entry is null)
                return null;

            node = new(entry.HashBase16, entry.Mode is "40000" or "040000");
        }

        return node;
    }

    public IReadOnlyList<(IReadOnlyList<string> Path, string Hash)> ListFiles(
        IReadOnlyList<string> path, CancellationToken token = default)
    {
        var node = Find(path) ?? throw new FileNotFoundException("Git path not found: " + string.Join("/", path));
        var files = new List<(IReadOnlyList<string>, string)>();

        void Visit(Node current, IReadOnlyList<string> currentPath)
        {
            token.ThrowIfCancellationRequested();

            if (!current.IsDirectory)
            {
                files.Add((currentPath, current.Hash));
                return;
            }

            foreach (var entry in Entries(current.Hash))
            {
                if (entry.Mode is not "160000")
                    Visit(new(entry.HashBase16, entry.Mode is "40000" or "040000"), [.. currentPath, entry.Name]);
            }
        }

        Visit(node, path);
        return files;
    }

    public FileTree ReadPath(IReadOnlyList<string> path, CancellationToken token = default)
    {
        var node = Find(path) ?? throw new FileNotFoundException("Git path not found: " + string.Join("/", path));
        var files = ReadFiles(ListFiles(path, token).Select(file => file.Path), token);

        return
            node.IsDirectory
            ?
            FileTree.FromSetOfFilesWithStringPath(
                files.Select(
                    file => ((IReadOnlyList<string>)[.. file.Key.Skip(path.Count)], file.Value)))
            :
            FileTree.File(files.Single().Value);
    }

    public IReadOnlyDictionary<IReadOnlyList<string>, ReadOnlyMemory<byte>> ReadFiles(
        IEnumerable<IReadOnlyList<string>> paths, CancellationToken token = default)
    {
        var selected =
            paths.Distinct(EnumerableExtensions.EqualityComparer<IReadOnlyList<string>>())
            .Select(path => (Path: path, Node: Find(path))).Where(item => item.Node is not null).ToArray();

        foreach (var (path, node) in selected)
        {
            token.ThrowIfCancellationRequested();

            if (node!.IsDirectory)
                throw new IOException("Expected a Git file: " + string.Join("/", path));

            if (_blobs.ContainsKey(node.Hash))
                continue;

            ReadOnlyMemory<byte>? bytes = null;

            if (_cachedFiles.TryGetValue(string.Join("/", path), out var cached))
                bytes = cached;

            else if (Objects.GetObject(node.Hash) is { Type: GitCore.PackFile.ObjectType.Blob } blob)
                bytes = blob.Data;

            var cachedOnDisk = false;

            if (bytes is null && _blobCacheDirectory is not null)
            {
                var cacheFile = Path.Combine(_blobCacheDirectory, node.Hash);

                if (File.Exists(cacheFile))
                {
                    using var file =
                        new FileStream(cacheFile, FileMode.Open, FileAccess.Read, FileShare.Read | FileShare.Delete);

                    using var buffer = new MemoryStream();
                    file.CopyTo(buffer);
                    bytes = buffer.ToArray();
                    cachedOnDisk = true;
                }
            }

            if (bytes is { } available)
            {
                RememberBlob(node.Hash, available, token, publish: !cachedOnDisk);
            }
        }

        var missing =
            selected.Select(item => item.Node!.Hash).Distinct(StringComparer.Ordinal)
            .Where(hash => !_blobs.ContainsKey(hash)).ToArray();

        if (missing.Length > 0)
        {
            var fetched = _fetchBlobs(missing);
            token.ThrowIfCancellationRequested();

            foreach (var hash in missing)
            {
                if (!fetched.TryGetValue(hash, out var bytes))
                    throw new IOException("Git server did not supply blob " + hash);

                RememberBlob(hash, bytes, token, publish: true);
            }
        }

        token.ThrowIfCancellationRequested();

        return
            selected.ToDictionary(
                item => item.Path,
                item => _blobs[item.Node!.Hash],
                EnumerableExtensions.EqualityComparer<IReadOnlyList<string>>());
    }

    private IReadOnlyList<GitCore.GitObjects.TreeEntry> Entries(string hash)
    {
        if (_trees.TryGetValue(hash, out var entries))
            return entries;

        var tree = Objects.GetObject(hash);

        if (tree?.Type is not GitCore.PackFile.ObjectType.Tree)
            throw new IOException("Git tree not found: " + hash);

        entries = GitCore.GitObjects.ParseTree(tree.Data).Entries;
        _trees.Add(hash, entries);
        return entries;
    }

    private void RememberBlob(string hash, ReadOnlyMemory<byte> bytes, CancellationToken token, bool publish)
    {
        VerifyBlob(hash, bytes);

        if (publish && _blobCacheDirectory is not null)
        {
            Directory.CreateDirectory(_blobCacheDirectory);
            AtomicFile.WriteAllBytes(Path.Combine(_blobCacheDirectory, hash), bytes, token);
        }

        _blobs.Add(hash, bytes);
    }

    internal static string BlobHash(ReadOnlyMemory<byte> bytes)
    {
        using var hash = IncrementalHash.CreateHash(HashAlgorithmName.SHA1);
        hash.AppendData(Encoding.ASCII.GetBytes("blob " + bytes.Length + "\0"));
        hash.AppendData(bytes.Span);
        return Convert.ToHexStringLower(hash.GetHashAndReset());
    }

    private static void VerifyBlob(string expected, ReadOnlyMemory<byte> bytes)
    {
        var actual = BlobHash(bytes);

        if (!string.Equals(expected, actual, StringComparison.OrdinalIgnoreCase))
            throw new InvalidDataException("Git blob integrity check failed: expected " + expected + ", got " + actual);
    }
}
