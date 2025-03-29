using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

namespace Pine.Core;

public abstract record TreeNodeWithStringPath : IEquatable<TreeNodeWithStringPath>
{
    public record BlobNode : TreeNodeWithStringPath
    {
        private readonly int slimHashCode;

        public ReadOnlyMemory<byte> Bytes { get; }

        public BlobNode(ReadOnlyMemory<byte> bytes)
        {
            Bytes = bytes;

            var hash = new HashCode();

            hash.AddBytes(bytes.Span);

            slimHashCode = hash.ToHashCode();
        }

        /// <inheritdoc/>
        public virtual bool Equals(BlobNode? other)
        {
            if (other is null)
                return false;

            return
                slimHashCode == other.slimHashCode &&
                Bytes.Span.SequenceEqual(other.Bytes.Span);
        }

        /// <inheritdoc/>
        public override int GetHashCode() => slimHashCode;
    }

    public record TreeNode : TreeNodeWithStringPath
    {
        private readonly int slimHashCode;

        public IReadOnlyList<(string name, TreeNodeWithStringPath component)> Elements { get; }

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

        /// <inheritdoc/>
        public virtual bool Equals(TreeNode? other)
        {
            if (other is null)
                return false;

            return
                slimHashCode == other.slimHashCode &&
                Elements.Count == other.Elements.Count &&
                Elements.SequenceEqual(other.Elements);
        }

        /// <inheritdoc/>
        public override int GetHashCode() => slimHashCode;
    }

    public static readonly IComparer<(string name, TreeNodeWithStringPath component)> TreeEntryDefaultComparer =
        new TreeEntryDefaultComparerClass();

    public static TreeNodeWithStringPath Blob(ReadOnlyMemory<byte> blobContent) =>
        new BlobNode(blobContent);

    public static TreeNodeWithStringPath SortedTree(IReadOnlyList<(string name, TreeNodeWithStringPath component)> treeContent) =>
        Sort(NonSortedTree(treeContent));

    public static TreeNodeWithStringPath NonSortedTree(IReadOnlyList<(string name, TreeNodeWithStringPath component)> treeContent) =>
        new TreeNode(treeContent);

    public static readonly TreeNodeWithStringPath EmptyTree = new TreeNode([]);


    public IEnumerable<(IImmutableList<string> path, ReadOnlyMemory<byte> blobContent)> EnumerateBlobsTransitive()
    {
        var stack = new Stack<(IImmutableList<string> path, TreeNodeWithStringPath node)>();

        stack.Push((ImmutableList<string>.Empty, this));

        while (stack.Count > 0)
        {
            var (path, node) = stack.Pop();

            switch (node)
            {
                case BlobNode blob:
                    yield return (path, blob.Bytes);
                    break;

                case TreeNode tree:
                    foreach (var (name, component) in tree.Elements)
                    {
                        stack.Push((path.Add(name), component));
                    }
                    break;

                default:
                    throw new NotImplementedException(
                        "Unexpected node type: " + node.GetType());
            }
        }
    }

    public TreeNodeWithStringPath? GetNodeAtPath(IReadOnlyList<string> path) =>
        GetNodeAtPath([.. path]);

    public TreeNodeWithStringPath? GetNodeAtPath(ReadOnlySpan<string> path)
    {
        if (path.Length is 0)
            return this;

        var pathFirstElement = path[0];

        if (this is TreeNode treeNode)
        {
            for (var i = 0; i < treeNode.Elements.Count; i++)
            {
                var (name, component) = treeNode.Elements[i];

                if (name == pathFirstElement)
                {
                    return component.GetNodeAtPath(path.Slice(1));
                }
            }
        }

        return null;
    }

    public TreeNodeWithStringPath? RemoveNodeAtPath(IReadOnlyList<string> path) =>
        RemoveNodeAtPath([.. path]);

    public TreeNodeWithStringPath? RemoveNodeAtPath(ReadOnlySpan<string> path)
    {
        if (path.Length is 0)
            return null;

        if (this is not TreeNode tree)
            return this;

        var pathFirstElement = path[0];

        if (tree.Elements.FirstOrDefault(treeNode => treeNode.name == pathFirstElement) is { } childNodeBefore &&
            childNodeBefore.component is not null)
        {
            var childNodeAfterRemoval =
                childNodeBefore.component.RemoveNodeAtPath(path[1..]);

            if (childNodeAfterRemoval is null)
            {
                var items =
                    new (string name, TreeNodeWithStringPath component)[tree.Elements.Count - 1];

                bool removed = false;

                for (var i = 0; i < tree.Elements.Count; i++)
                {
                    var prevPositionItem = tree.Elements[i];

                    if (prevPositionItem.name == pathFirstElement)
                    {
                        removed = true;
                        continue;
                    }
                    else
                    {
                        items[removed ? i - 1 : i] = prevPositionItem;
                    }
                }

                return new TreeNode(items);
            }
            else
            {
                var items =
                    new (string name, TreeNodeWithStringPath component)[tree.Elements.Count];

                for (var i = 0; i < tree.Elements.Count; i++)
                {
                    items[i] =
                        tree.Elements[i].name == pathFirstElement
                        ?
                        (pathFirstElement, childNodeAfterRemoval)
                        :
                        tree.Elements[i];
                }

                return new TreeNode(items);
            }
        }

        return this;
    }

    public TreeNodeWithStringPath SetNodeAtPathSorted(
        IReadOnlyList<string> path,
        TreeNodeWithStringPath node) =>
        SetNodeAtPathSorted([.. path], node);

    public TreeNodeWithStringPath SetNodeAtPathSorted(
        ReadOnlySpan<string> path,
        TreeNodeWithStringPath node)
    {
        if (path.Length is 0)
            return node;

        var pathFirstElement = path[0];

        if (this is BlobNode)
        {
            var childNode =
                EmptyTree.SetNodeAtPathSorted(path[1..], node);

            return
                new TreeNode(
                    ImmutableList.Create((pathFirstElement, childNode)));
        }

        if (this is TreeNode treeNode)
        {
            if (treeNode.Elements.FirstOrDefault(treeNode => treeNode.name == pathFirstElement) is { } childNodeBefore &&
                childNodeBefore.component is not null)
            {
                var childNode =
                    childNodeBefore.component.SetNodeAtPathSorted(path[1..], node);

                var items =
                    new (string name, TreeNodeWithStringPath component)[treeNode.Elements.Count];

                for (var i = 0; i < treeNode.Elements.Count; i++)
                {
                    items[i] =
                        treeNode.Elements[i].name == pathFirstElement
                        ?
                        (pathFirstElement, childNode)
                        :
                        treeNode.Elements[i];
                }

                return new TreeNode(items);
            }

            {
                var childNode =
                    EmptyTree.SetNodeAtPathSorted(path[1..], node);

                var items =
                    new (string name, TreeNodeWithStringPath component)[treeNode.Elements.Count + 1];

                bool inserted = false;

                for (var i = 0; i < treeNode.Elements.Count; i++)
                {
                    if (inserted)
                    {
                        items[i] = treeNode.Elements[i - 1];
                        continue;
                    }

                    var prevPositionItem = treeNode.Elements[i];

                    if (string.CompareOrdinal(prevPositionItem.name, pathFirstElement) > 0)
                    {
                        items[i] = (pathFirstElement, childNode);
                        inserted = true;
                    }
                    else
                    {
                        items[i] = prevPositionItem;
                    }
                }

                if (inserted)
                {
                    items[^1] = treeNode.Elements[^1];
                }
                else
                {
                    items[^1] = (pathFirstElement, childNode);
                }

                return new TreeNode(items);
            }
        }

        throw new NotImplementedException(
            "Unexpected node type: " + GetType());
    }

    public static TreeNodeWithStringPath MergeBlobs(
        TreeNodeWithStringPath left,
        TreeNodeWithStringPath right) =>
        right.EnumerateBlobsTransitive()
        .Aggregate(left, (acc, blob) => acc.SetNodeAtPathSorted(blob.path, Blob(blob.blobContent)));


    public static TreeNodeWithStringPath FilterNodesByPath(
        TreeNodeWithStringPath node,
        Func<IReadOnlyList<string>, bool> pathFilter,
        IReadOnlyList<string>? currentPrefix = null) =>
        node switch
        {
            BlobNode blob => blob,

            TreeNode tree =>
            new TreeNode(
                tree.Elements
                .Where(treeNode => pathFilter([.. (currentPrefix ?? []), treeNode.name]))
                .Select(treeNode => (treeNode.name,
                FilterNodesByPath(
                    treeNode.component,
                    pathFilter,
                    currentPrefix: [.. currentPrefix ?? [], treeNode.name])))
                .ToImmutableList()),

            _ =>
            throw new NotImplementedException(
                "Unexpected node type: " + node.GetType())
        };

    public static TreeNodeWithStringPath? RemoveEmptyNodes(TreeNodeWithStringPath node)
    {
        if (node is BlobNode)
            return node;

        if (node is TreeNode tree)
        {
            IReadOnlyList<(string name, TreeNodeWithStringPath component)> newElements =
                [..tree.Elements
                .Select(e => (e.name, component: RemoveEmptyNodes(e.component)))
                .Where(e => e.component is not null)
                ];

            if (newElements.Count is 0)
                return null;

            return new TreeNode(newElements);
        }

        throw new NotImplementedException(
            "Unexpected node type: " + node.GetType());
    }

    public static TreeNodeWithStringPath Sort(TreeNodeWithStringPath node) =>
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

    private class TreeEntryDefaultComparerClass : IComparer<(string name, TreeNodeWithStringPath component)>
    {
        public int Compare((string name, TreeNodeWithStringPath component) x, (string name, TreeNodeWithStringPath component) y)
        {
            return string.CompareOrdinal(x.name, y.name);
        }
    }
}

