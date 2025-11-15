using System.Collections.Generic;
using System;
using System.IO;
using System.Linq;
using Pine.Core.IO;
using Pine.Core.Files;

namespace Pine;

public static class LoadFromLocalFilesystem
{
    public static FileTree? LoadSortedTreeFromPath(
        string path,
        Func<IReadOnlyList<string>, IOException, bool>? ignoreFileOnIOException = null)
    {
        if (File.Exists(path))
            return FileTree.File(content: File.ReadAllBytes(path));

        if (!Directory.Exists(path))
            return null;

        var blobs = Filesystem.GetAllFilesFromDirectory(
            path,
            ignoreFileOnIOException: ignoreFileOnIOException);

        return FileTree.FromSetOfFilesWithStringPath(blobs);
    }

    public static FileTree RemoveNoiseFromTree(
        FileTree originalTree,
        bool discardGitDirectory)
    {
        if (originalTree is not FileTree.DirectoryNode tree)
            return originalTree;

        FileTree? getValueFromStringName(string name) =>
            tree.Items.FirstOrDefault(c => c.name == name).component;

        var elmJson = getValueFromStringName("elm.json");

        bool keepNode((string name, FileTree component) node)
        {
            if (elmJson != null && node.name is "elm-stuff")
                return false;

            if (discardGitDirectory && node.component is FileTree.DirectoryNode && node.name is ".git")
                return false;

            return true;
        }

        return FileTree.SortedDirectory(
            directoryContent:
                [.. tree.Items
                .Where(keepNode)
                .Select(child => (child.name, RemoveNoiseFromTree(child.component, discardGitDirectory)))
                ]);
    }
}
