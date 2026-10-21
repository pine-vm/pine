using Pine.Core.Files;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.IO;
using System.Linq;
using System.Security.Cryptography;
using System.Text;

namespace Pine.IntegrationTests;

internal sealed class GitRepositoryFixture
{
    private readonly Dictionary<string, GitCore.PackFile.PackObject> _objects = [];

    private readonly Dictionary<string, ReadOnlyMemory<byte>> _blobs = [];

    private readonly Dictionary<string, string> _paths = [];

    public string Commit { get; }

    public GitCore.Repository Metadata { get; }

    public List<string> RequestedPaths { get; } = [];

    public List<string[]> Requests { get; } = [];

    public long DownloadedBytes { get; private set; }

    public Action? BeforeFetch { get; set; }

    public int OpenCount { get; private set; }

    public GitRepositoryFixture(FileTree tree)
    {
        var root = AddTree(tree, []);

        Commit =
            AddObject(
                GitCore.PackFile.ObjectType.Commit,
                Encoding.UTF8.GetBytes(
                    "tree " + root + "\nauthor Fixture <fixture@example.com> 1760000000 +0000\n" +
                    "committer Fixture <fixture@example.com> 1760000000 +0000\n\nFixture\n"));

        Metadata = new(_objects.ToImmutableDictionary());
    }

    public GitRemoteRepository Open(
        IReadOnlyDictionary<IReadOnlyList<string>, ReadOnlyMemory<byte>>? cachedFiles = null,
        string? blobCacheDirectory = null)
    {
        ++OpenCount;

        return
            new(
                Metadata,
                Commit,
                hashes =>
                {
                    BeforeFetch?.Invoke();
                    Requests.Add([.. hashes]);

                    foreach (var hash in hashes)
                    {
                        RequestedPaths.Add(_paths[hash]);
                        DownloadedBytes += _blobs[hash].Length;
                    }

                    return hashes.ToDictionary(hash => hash, hash => _blobs[hash]);
                },
                cachedFiles,
                blobCacheDirectory);
    }

    private string AddTree(FileTree node, IReadOnlyList<string> path)
    {
        switch (node)
        {
            case FileTree.FileNode file:
                var hash = GitRemoteRepository.BlobHash(file.Bytes);
                _blobs.TryAdd(hash, file.Bytes);
                _paths.TryAdd(hash, string.Join("/", path));
                return hash;

            case FileTree.DirectoryNode directory:
                using (var bytes = new MemoryStream())
                {
                    foreach (var (name, child) in directory.Items)
                    {
                        var childHash = AddTree(child, [.. path, name]);

                        bytes.Write(
                            Encoding.UTF8.GetBytes(
                                (child is FileTree.DirectoryNode ? "40000 " : "100644 ") + name + "\0"));

                        bytes.Write(Convert.FromHexString(childHash));
                    }

                    return AddObject(GitCore.PackFile.ObjectType.Tree, bytes.ToArray());
                }

            default:
                throw new NotImplementedException(
                    nameof(AddTree) + " does not handle file-tree variant: " + node.GetType().Name);
        }
    }

    private string AddObject(GitCore.PackFile.ObjectType type, byte[] data)
    {
        var name = type is GitCore.PackFile.ObjectType.Tree ? "tree" : "commit";

        var hash =
            Convert.ToHexStringLower(
                SHA1.HashData([.. Encoding.ASCII.GetBytes(name + " " + data.Length + "\0"), .. data]));

        _objects.TryAdd(hash, new(type, data.Length, data, hash));
        return hash;
    }
}
