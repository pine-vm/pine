using System.Collections.Generic;
using System;
using System.IO;
using System.Linq;
using Pine.Core;

namespace Pine;

public static class LoadFromLocalFilesystem
{
    public static TreeNodeWithStringPath? LoadSortedTreeFromPath(
        string path,
        Func<IReadOnlyList<string>, IOException, bool>? ignoreFileOnIOException = null)
    {
        if (File.Exists(path))
            return TreeNodeWithStringPath.Blob(blobContent: File.ReadAllBytes(path));

        if (!Directory.Exists(path))
            return null;

        var blobs = Filesystem.GetAllFilesFromDirectory(
            path,
            ignoreFileOnIOException: ignoreFileOnIOException);

        return PineValueComposition.SortedTreeFromSetOfBlobsWithStringPath(blobs);
    }

    public static TreeNodeWithStringPath RemoveNoiseFromTree(
        TreeNodeWithStringPath originalTree,
        bool discardGitDirectory)
    {
        if (originalTree is not TreeNodeWithStringPath.TreeNode tree)
            return originalTree;

        TreeNodeWithStringPath? getValueFromStringName(string name) =>
            tree.Elements.FirstOrDefault(c => c.name == name).component;

        var elmJson = getValueFromStringName("elm.json");

        bool keepNode((string name, TreeNodeWithStringPath component) node)
        {
            if (elmJson != null && node.name is "elm-stuff")
                return false;

            if (discardGitDirectory && node.component is TreeNodeWithStringPath.TreeNode && node.name is ".git")
                return false;

            return true;
        }

        return TreeNodeWithStringPath.SortedTree(
            treeContent:
                [.. tree.Elements
                .Where(keepNode)
                .Select(child => (child.name, RemoveNoiseFromTree(child.component, discardGitDirectory)))
                ]);
    }
}
