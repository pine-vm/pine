using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

namespace Pine;

public abstract record TreeNodeWithStringPath : IEquatable<TreeNodeWithStringPath>
{
    public record BlobNode : TreeNodeWithStringPath
    {
        readonly int slimHashCode;

        public ReadOnlyMemory<byte> Bytes { private init; get; }

        public BlobNode(ReadOnlyMemory<byte> bytes)
        {
            Bytes = bytes;

            var hash = new HashCode();

            hash.AddBytes(bytes.Span);

            slimHashCode = hash.ToHashCode();
        }

        public virtual bool Equals(BlobNode? other)
        {
            if (other is null)
                return false;

            return
                slimHashCode == other.slimHashCode &&
                Bytes.Span.SequenceEqual(other.Bytes.Span);
        }

        public override int GetHashCode() => slimHashCode;
    }

    public record TreeNode : TreeNodeWithStringPath
    {
        readonly int slimHashCode;

        public IReadOnlyList<(string name, TreeNodeWithStringPath component)> Elements { private init; get; }

        public TreeNode(IReadOnlyList<(string name, TreeNodeWithStringPath component)> elements)
        {
            Elements = elements;

            var hash = new HashCode();

            foreach (var item in elements)
            {
                hash.Add(item.GetHashCode());
            }

            slimHashCode = hash.ToHashCode();
        }

        public virtual bool Equals(TreeNode? other)
        {
            if (other is null)
                return false;

            return
                slimHashCode == other.slimHashCode &&
                Elements.Count == other.Elements.Count &&
                Elements.SequenceEqual(other.Elements);
        }

        public override int GetHashCode() => slimHashCode;
    }

    static public readonly IComparer<(string name, TreeNodeWithStringPath component)> TreeEntryDefaultComparer = new TreeEntryDefaultComparerClass();

    static public TreeNodeWithStringPath Blob(ReadOnlyMemory<byte> blobContent) =>
        new BlobNode(blobContent);

    static public TreeNodeWithStringPath SortedTree(IImmutableList<(string name, TreeNodeWithStringPath component)> treeContent) =>
        Sort(NonSortedTree(treeContent));

    static public TreeNodeWithStringPath NonSortedTree(IImmutableList<(string name, TreeNodeWithStringPath component)> treeContent) =>
        new TreeNode(treeContent);

    static public readonly TreeNodeWithStringPath EmptyTree = new TreeNode(ImmutableList<(string name, TreeNodeWithStringPath component)>.Empty);


    public IImmutableList<(IImmutableList<string> path, ReadOnlyMemory<byte> blobContent)> EnumerateBlobsTransitive() =>
        this switch
        {
            BlobNode blob => ImmutableList.Create<(IImmutableList<string> path, ReadOnlyMemory<byte> blobContent)>(
                (ImmutableList<string>.Empty, blob.Bytes)),

            TreeNode tree => tree.Elements.SelectMany(treeEntry =>
            treeEntry.component.EnumerateBlobsTransitive()
            .Select(child => (child.path.Insert(0, treeEntry.name), child.blobContent)))
            .ToImmutableList(),

            _ => throw new NotImplementedException()
        };

    public TreeNodeWithStringPath? GetNodeAtPath(IReadOnlyList<string> path)
    {
        if (path.Count == 0)
            return this;

        var pathFirstElement = path[0];

        return
            this switch
            {
                TreeNode tree => tree.Elements
                .Where(treeNode => treeNode.name == pathFirstElement)
                .Select(treeNode => treeNode.component.GetNodeAtPath(path.Skip(1).ToImmutableList()))
                .FirstOrDefault(),

                _ => null
            };
    }

    public TreeNodeWithStringPath? RemoveNodeAtPath(IReadOnlyList<string> path)
    {
        if (path.Count == 0)
            return null;

        if (this is not TreeNode tree)
            return null;

        var pathFirstElement = path[0];

        var treeContent =
            tree.Elements.SelectMany(treeNode =>
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
            (this switch
            {
                TreeNode tree => tree.Elements,
                _ => ImmutableList<(string name, TreeNodeWithStringPath component)>.Empty
            })
            .Where(treeNode => treeNode.name != pathFirstElement)
            .Concat(new[] { (pathFirstElement, childNode) })
            .Order(TreeEntryDefaultComparer)
            .ToImmutableList();

        return SortedTree(treeEntries);
    }

    static public TreeNodeWithStringPath Sort(TreeNodeWithStringPath node) =>
        node switch
        {
            BlobNode _ => node,
            TreeNode tree =>
            new TreeNode(tree.Elements.Order(TreeEntryDefaultComparer).Select(child => (child.name, Sort(child.component))).ToImmutableList()),

            _ => throw new NotImplementedException()
        };

    public T Map<T>(
        Func<ReadOnlyMemory<byte>, T> fromBlob,
        Func<IReadOnlyList<(string itemName, TreeNodeWithStringPath itemValue)>, T> fromTree) =>
        this switch
        {
            TreeNode tree => fromTree(tree.Elements),

            BlobNode blob => fromBlob(blob.Bytes),

            _ => throw new NotImplementedException()
        };

    class TreeEntryDefaultComparerClass : IComparer<(string name, TreeNodeWithStringPath component)>
    {
        public int Compare((string name, TreeNodeWithStringPath component) x, (string name, TreeNodeWithStringPath component) y)
        {
            return string.CompareOrdinal(x.name, y.name);
        }
    }
}

