using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

namespace Pine;

public record TreeNodeWithStringPath : IEquatable<TreeNodeWithStringPath>
{
    public ReadOnlyMemory<byte>? BlobContent { private init; get; }

    public IImmutableList<(string name, TreeNodeWithStringPath component)>? TreeContent { private init; get; }

    static public readonly IComparer<string> TreeEntryNameComparer = StringComparer.Ordinal;

    static public TreeNodeWithStringPath Blob(ReadOnlyMemory<byte> blobContent) =>
        new() { BlobContent = blobContent };

    static public TreeNodeWithStringPath SortedTree(IImmutableList<(string name, TreeNodeWithStringPath component)> treeContent) =>
        Sort(new() { TreeContent = treeContent });

    static public TreeNodeWithStringPath NonSortedTree(IImmutableList<(string name, TreeNodeWithStringPath component)> treeContent) =>
        new() { TreeContent = treeContent };

    static public TreeNodeWithStringPath EmptyTree => SortedTree(ImmutableList<(string name, TreeNodeWithStringPath component)>.Empty);


    public IImmutableList<(IImmutableList<string> path, ReadOnlyMemory<byte> blobContent)> EnumerateBlobsTransitive()
    {
        if (BlobContent != null)
        {
            return ImmutableList.Create<(IImmutableList<string> path, ReadOnlyMemory<byte> blobContent)>(
                (ImmutableList<string>.Empty, BlobContent.Value));
        }

        return
            TreeContent!.SelectMany(treeEntry =>
                treeEntry.component.EnumerateBlobsTransitive()
                .Select(child => (child.path.Insert(0, treeEntry.name), child.blobContent)))
            .ToImmutableList();
    }

    public ReadOnlyMemory<byte>? GetBlobAtPath(IReadOnlyList<string> path) =>
        GetNodeAtPath(path)?.BlobContent;

    public TreeNodeWithStringPath? GetNodeAtPath(IReadOnlyList<string> path)
    {
        if (path.Count == 0)
            return this;

        if (TreeContent == null)
            return null;

        var pathFirstElement = path[0];

        return
            TreeContent
            .Where(treeNode => treeNode.name == pathFirstElement)
            .Select(treeNode => treeNode.component.GetNodeAtPath(path.Skip(1).ToImmutableList()))
            .FirstOrDefault();
    }

    public TreeNodeWithStringPath? RemoveNodeAtPath(IReadOnlyList<string> path)
    {
        if (path.Count == 0)
            return null;

        if (TreeContent == null)
            return null;

        var pathFirstElement = path[0];

        var treeContent =
            TreeContent.SelectMany(treeNode =>
            {
                if (treeNode.name != pathFirstElement)
                    return ImmutableList.Create(treeNode);

                if (path.Count == 1)
                    return ImmutableList<(string name, TreeNodeWithStringPath component)>.Empty;

                var componentAfterRemoval =
                    treeNode.component.RemoveNodeAtPath(path.Skip(1).ToImmutableArray());

                if (componentAfterRemoval == null)
                    return ImmutableList<(string name, TreeNodeWithStringPath component)>.Empty;

                return ImmutableList.Create((treeNode.name, componentAfterRemoval));
            }).ToImmutableList();

        return SortedTree(treeContent);
    }

    public TreeNodeWithStringPath SetNodeAtPathSorted(IReadOnlyList<string> path, TreeNodeWithStringPath node)
    {
        if (path.Count == 0)
            return node;

        var pathFirstElement = path[0];

        var childNodeBefore = GetNodeAtPath(new[] { pathFirstElement });

        var childNode =
            (childNodeBefore ?? EmptyTree).SetNodeAtPathSorted(path.Skip(1).ToImmutableList(), node);

        var treeEntries =
            (TreeContent?.Where(treeNode => treeNode.name != pathFirstElement) ?? ImmutableList<(string name, TreeNodeWithStringPath component)>.Empty)
            .Concat(new[] { (pathFirstElement, childNode) })
            .OrderBy(treeEntry => treeEntry.Item1, TreeEntryNameComparer)
            .ToImmutableList();

        return SortedTree(treeEntries);
    }

    static TreeNodeWithStringPath Sort(TreeNodeWithStringPath node) =>
        node.TreeContent == null
        ?
        node
        :
        new TreeNodeWithStringPath
        {
            TreeContent = node.TreeContent.OrderBy(child => child.name).Select(child => (child.name, Sort(child.component))).ToImmutableList()
        };

    public virtual bool Equals(TreeNodeWithStringPath? other)
    {
        if (other is null)
            return false;

        if (BlobContent != null || other.BlobContent != null)
        {
            if (BlobContent == null || other.BlobContent == null)
                return false;

            return BlobContent.Value.Span.SequenceEqual(other.BlobContent.Value.Span);
        }

        if (TreeContent == null || other.TreeContent == null)
            return false;

        if (TreeContent.Count != other.TreeContent.Count)
            return false;

        return
            Enumerable.Range(0, TreeContent.Count)
            .All(i =>
            {
                var thisElement = TreeContent.ElementAt(i);
                var otherElement = other.TreeContent.ElementAt(i);

                return thisElement.name == otherElement.name &&
                    thisElement.component.Equals(otherElement.component);
            });
    }

    public override int GetHashCode()
    {
        return HashCode.Combine(BlobContent, TreeContent);
    }
}

