using System.Collections.Generic;
using System;
using System.IO;
using System.Linq;
using Pine.Core;

namespace Pine;

public static class LoadFromLocalFilesystem
{
    public static BlobTreeWithStringPath? LoadSortedTreeFromPath(
        string path,
        Func<IReadOnlyList<string>, IOException, bool>? ignoreFileOnIOException = null)
    {
        if (File.Exists(path))
            return BlobTreeWithStringPath.Blob(blobContent: File.ReadAllBytes(path));

        if (!Directory.Exists(path))
            return null;

        var blobs = Filesystem.GetAllFilesFromDirectory(
            path,
            ignoreFileOnIOException: ignoreFileOnIOException);

        return PineValueComposition.SortedTreeFromSetOfBlobsWithStringPath(blobs);
    }

    public static BlobTreeWithStringPath RemoveNoiseFromTree(
        BlobTreeWithStringPath originalTree,
        bool discardGitDirectory)
    {
        if (originalTree is not BlobTreeWithStringPath.TreeNode tree)
            return originalTree;

        BlobTreeWithStringPath? getValueFromStringName(string name) =>
            tree.Items.FirstOrDefault(c => c.name == name).component;

        var elmJson = getValueFromStringName("elm.json");

        bool keepNode((string name, BlobTreeWithStringPath component) node)
        {
            if (elmJson != null && node.name is "elm-stuff")
                return false;

            if (discardGitDirectory && node.component is BlobTreeWithStringPath.TreeNode && node.name is ".git")
                return false;

            return true;
        }

        return BlobTreeWithStringPath.SortedTree(
            treeContent:
                [.. tree.Items
                .Where(keepNode)
                .Select(child => (child.name, RemoveNoiseFromTree(child.component, discardGitDirectory)))
                ]);
    }
}
