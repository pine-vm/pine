using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

namespace Pine.Core.Files;

/// <summary>
/// Represents an immutable tree of files and directories. A <see cref="FileTree"/> is either a <see cref="FileNode"/> holding file content
/// or a <see cref="DirectoryNode"/> holding a list of named child nodes. All update operations return new tree instances.
/// </summary>
public abstract record FileTree : IEquatable<FileTree>
{
    /// <summary>
    /// Represents a leaf node containing the binary content of a single file.
    /// </summary>
    public record FileNode : FileTree
    {
        private readonly int _slimHashCode;

        /// <summary>
        /// Gets the file content as a read-only memory of bytes.
        /// </summary>
        public ReadOnlyMemory<byte> Bytes { get; }

        /// <summary>
        /// Initializes a new instance of the <see cref="FileNode"/> class with the specified file content.
        /// </summary>
        /// <param name="bytes">The file content as a read-only memory of bytes.</param>
        public FileNode(ReadOnlyMemory<byte> bytes)
        {
            Bytes = bytes;

            var hash = new HashCode();

            hash.AddBytes(bytes.Span);

            _slimHashCode = hash.ToHashCode();
        }

        /// <summary>
        /// Determines whether this file node contains the same content as the specified other file node.
        /// </summary>
        /// <param name="other">The other file node to compare with.</param>
        /// <returns><c>true</c> if both nodes represent identical file content; otherwise, <c>false</c>.</returns>
        public virtual bool Equals(FileNode? other)
        {
            if (other is null)
                return false;

            return
                _slimHashCode == other._slimHashCode &&
                Bytes.Span.SequenceEqual(other.Bytes.Span);
        }

        /// <summary>
        /// Returns a hash code for this file node.
        /// </summary>
        /// <returns>A hash code representing this file node.</returns>
        public override int GetHashCode() => _slimHashCode;
    }

    /// <summary>
    /// Represents a directory node containing a list of named child nodes.
    /// </summary>
    public record DirectoryNode : FileTree
    {
        private readonly int _slimHashCode;

        /// <summary>
        /// Gets the list of child nodes for this tree node. Each child is represented as a tuple containing the name of the edge and the corresponding <see cref="FileTree"/> component.
        /// </summary>
        public IReadOnlyList<(string name, FileTree component)> Items { get; }

        /// <summary>
        /// Initializes a new instance of the <see cref="DirectoryNode"/> class with the specified items.
        /// </summary>
        /// <param name="items">
        /// The list of child nodes, each represented as a tuple containing the name of the edge and the corresponding <see cref="FileTree"/> component.
        /// </param>
        public DirectoryNode(IReadOnlyList<(string name, FileTree component)> items)
        {
            Items = items;

            var hash = new HashCode();

            foreach (var item in items)
            {
                hash.Add(item.GetHashCode());
            }

            _slimHashCode = hash.ToHashCode();
        }

        /// <summary>
        /// Determines whether this directory node has the same structure and child nodes as the specified other directory node.
        /// </summary>
        /// <param name="other">The other directory node to compare with.</param>
        /// <returns><c>true</c> if both directories have identical ordered children and each child is equal; otherwise, <c>false</c>.</returns>
        public virtual bool Equals(DirectoryNode? other)
        {
            if (other is null)
                return false;

            return
                _slimHashCode == other._slimHashCode &&
                Items.Count == other.Items.Count &&
                Items.SequenceEqual(other.Items);
        }

        /// <summary>
        /// Returns a hash code for this directory node.
        /// </summary>
        /// <returns>A hash code representing this directory node.</returns>
        public override int GetHashCode() => _slimHashCode;
    }

    /// <summary>
    /// Gets the default comparer for tree entries, which compares entries by their name using ordinal string comparison.
    /// </summary>
    public static readonly IComparer<(string name, FileTree component)> TreeEntryDefaultComparer =
        new TreeEntryDefaultComparerClass();

    /// <summary>
    /// Creates a new <see cref="FileNode"/> with the specified content.
    /// </summary>
    /// <param name="content">The file content as a read-only memory of bytes.</param>
    public static FileTree File(ReadOnlyMemory<byte> content) =>
        new FileNode(content);

    /// <summary>
    /// Creates a new <see cref="DirectoryNode"/> with the specified content, recursively sorting all sibling entries
    /// using ordinal string comparison. This ensures a canonical order for equality and hashing.
    /// </summary>
    /// <param name="directoryContent">
    /// The list of child nodes, each represented as a tuple containing the name of the edge and the corresponding <see cref="FileTree"/> component.
    /// </param>
    /// <returns>
    /// A <see cref="DirectoryNode"/> with the specified content, with all siblings sorted recursively.
    /// </returns>
    public static FileTree SortedDirectory(IReadOnlyList<(string name, FileTree component)> directoryContent) =>
        Sort(NonSortedDirectory(directoryContent));

    /// <summary>
    /// Creates a new <see cref="DirectoryNode"/> with the specified content, preserving the order of the provided items.
    /// Sibling order is not sorted; use <see cref="SortedDirectory"/> for canonical ordering.
    /// </summary>
    /// <param name="treeContent">
    /// The list of child nodes, each represented as a tuple containing the name of the edge and the corresponding <see cref="FileTree"/> component.
    /// </param>
    /// <returns>
    /// A <see cref="DirectoryNode"/> with the specified content, preserving the order of the provided items.
    /// </returns>
    public static FileTree NonSortedDirectory(IReadOnlyList<(string name, FileTree component)> treeContent) =>
        new DirectoryNode(treeContent);

    /// <summary>
    /// Gets an empty tree node (a directory with no children).
    /// </summary>
    public static readonly FileTree EmptyTree = new DirectoryNode([]);


    /// <summary>
    /// Enumerates all files (leaf nodes) in the tree, yielding each file's path and content.
    /// Traverses the tree in depth-first order, returning a sequence of tuples where each tuple contains
    /// the path from the root to the file and the file's content as <see cref="ReadOnlyMemory{Byte}"/>.
    /// </summary>
    public IEnumerable<(IImmutableList<string> path, ReadOnlyMemory<byte> fileContent)> EnumerateFilesTransitive()
    {
        var stack = new Stack<(IImmutableList<string> path, FileTree node)>();

        stack.Push((ImmutableList<string>.Empty, this));

        while (stack.Count > 0)
        {
            var (path, node) = stack.Pop();

            switch (node)
            {
                case FileNode file:
                    yield return (path, file.Bytes);
                    break;

                case DirectoryNode directory:
                    foreach (var (name, component) in directory.Items)
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

    /// <summary>
    /// Returns the node at the specified path, or <c>null</c> if the path does not exist.
    /// An empty path returns the current node.
    /// </summary>
    /// <param name="path">The path to the node as a list of strings.</param>
    /// <returns>
    /// The <see cref="FileTree"/> at the specified path, or <c>null</c> if not found.
    /// </returns>
    public FileTree? GetNodeAtPath(IReadOnlyList<string> path) =>
        GetNodeAtPath([.. path]);

    /// <summary>
    /// Returns the node at the specified path, or <c>null</c> if the path does not exist.
    /// An empty path returns the current node.
    /// </summary>
    /// <param name="path">The path to the node as a list of strings.</param>
    /// <returns>
    /// The <see cref="FileTree"/> at the specified path, or <c>null</c> if not found.
    /// </returns>
    public FileTree? GetNodeAtPath(ReadOnlySpan<string> path)
    {
        if (path.Length is 0)
            return this;

        var pathFirstItem = path[0];

        if (this is DirectoryNode directory)
        {
            for (var i = 0; i < directory.Items.Count; i++)
            {
                var (name, component) = directory.Items[i];

                if (name == pathFirstItem)
                {
                    return component.GetNodeAtPath(path[1..]);
                }
            }
        }

        return null;
    }

    /// <summary>
    /// Returns a new tree with the node at the specified path removed.
    /// An empty path returns <c>null</c> (i.e., removes the current root).
    /// Ancestors are retained even if empty; combine with <see cref="RemoveEmptyNodes(FileTree)"/> to prune empty directories.
    /// </summary>
    /// <param name="path">The path to the node as a list of strings.</param>
    /// <returns>
    /// A new <see cref="FileTree"/> with the node at the specified path removed, or <c>null</c> if the path is empty.
    /// </returns>
    public FileTree? RemoveNodeAtPath(IReadOnlyList<string> path) =>
        RemoveNodeAtPath([.. path]);

    /// <summary>
    /// Returns a new tree with the node at the specified path removed.
    /// An empty path returns <c>null</c> (i.e., removes the current root).
    /// Ancestors are retained even if empty; combine with <see cref="RemoveEmptyNodes(FileTree)"/> to prune empty directories.
    /// </summary>
    /// <param name="path">The path to the node as a list of strings.</param>
    /// <returns>
    /// A new <see cref="FileTree"/> with the node at the specified path removed, or <c>null</c> if the path is empty.
    /// </returns>
    public FileTree? RemoveNodeAtPath(ReadOnlySpan<string> path)
    {
        if (path.Length is 0)
            return null;

        if (this is not DirectoryNode directory)
            return this;

        var pathFirstItem = path[0];

        if (directory.Items.FirstOrDefault(treeNode => treeNode.name == pathFirstItem) is { } childNodeBefore &&
            childNodeBefore.component is not null)
        {
            var childNodeAfterRemoval =
                childNodeBefore.component.RemoveNodeAtPath(path[1..]);

            if (childNodeAfterRemoval is null)
            {
                var items =
                    new (string name, FileTree component)[directory.Items.Count - 1];

                var removed = false;

                for (var i = 0; i < directory.Items.Count; i++)
                {
                    var prevPositionItem = directory.Items[i];

                    if (prevPositionItem.name == pathFirstItem)
                    {
                        removed = true;
                        continue;
                    }
                    else
                    {
                        items[removed ? i - 1 : i] = prevPositionItem;
                    }
                }

                return new DirectoryNode(items);
            }
            else
            {
                var items =
                    new (string name, FileTree component)[directory.Items.Count];

                for (var i = 0; i < directory.Items.Count; i++)
                {
                    items[i] =
                        directory.Items[i].name == pathFirstItem
                        ?
                        (pathFirstItem, childNodeAfterRemoval)
                        :
                        directory.Items[i];
                }

                return new DirectoryNode(items);
            }
        }

        return this;
    }

    /// <summary>
    /// Inserts or replaces a node at the specified path, maintaining sorted sibling order.
    /// If the path traverses a <see cref="FileNode"/>, that file is replaced by a directory to hold the new child.
    /// </summary>
    /// <param name="path">The path at which to insert or replace the node.</param>
    /// <param name="node">The node to insert or replace at the specified path.</param>
    /// <returns>
    /// A new <see cref="FileTree"/> with the node inserted or replaced at the specified path.
    /// </returns>
    public FileTree SetNodeAtPathSorted(
        IReadOnlyList<string> path,
        FileTree node) =>
        SetNodeAtPathSorted([.. path], node);

    /// <summary>
    /// Inserts or replaces a node at the specified path, maintaining sorted sibling order.
    /// If the path traverses a <see cref="FileNode"/>, that file is replaced by a directory to hold the new child.
    /// </summary>
    /// <param name="path">The path at which to insert or replace the node, as a <c>ReadOnlySpan&lt;string&gt;</c>.</param>
    /// <param name="node">The node to insert or replace at the specified path.</param>
    /// <returns>
    /// A new <see cref="FileTree"/> with the node inserted or replaced at the specified path.
    /// </returns>
    public FileTree SetNodeAtPathSorted(
        ReadOnlySpan<string> path,
        FileTree node)
    {
        if (path.Length is 0)
            return node;

        var pathFirstItem = path[0];

        if (this is FileNode)
        {
            var childNode =
                EmptyTree.SetNodeAtPathSorted(path[1..], node);

            return
                new DirectoryNode(
                    ImmutableList.Create((pathFirstItem, childNode)));
        }

        if (this is DirectoryNode treeNode)
        {
            if (treeNode.Items.FirstOrDefault(treeNode => treeNode.name == pathFirstItem) is { } childNodeBefore &&
                childNodeBefore.component is not null)
            {
                var childNode =
                    childNodeBefore.component.SetNodeAtPathSorted(path[1..], node);

                var items =
                    new (string name, FileTree component)[treeNode.Items.Count];

                for (var i = 0; i < treeNode.Items.Count; i++)
                {
                    items[i] =
                        treeNode.Items[i].name == pathFirstItem
                        ?
                        (pathFirstItem, childNode)
                        :
                        treeNode.Items[i];
                }

                return new DirectoryNode(items);
            }

            {
                var childNode =
                    EmptyTree.SetNodeAtPathSorted(path[1..], node);

                var items =
                    new (string name, FileTree component)[treeNode.Items.Count + 1];

                var inserted = false;

                for (var i = 0; i < treeNode.Items.Count; i++)
                {
                    if (inserted)
                    {
                        items[i] = treeNode.Items[i - 1];
                        continue;
                    }

                    var prevPositionItem = treeNode.Items[i];

                    if (string.CompareOrdinal(prevPositionItem.name, pathFirstItem) > 0)
                    {
                        items[i] = (pathFirstItem, childNode);
                        inserted = true;
                    }
                    else
                    {
                        items[i] = prevPositionItem;
                    }
                }

                if (inserted)
                {
                    items[^1] = treeNode.Items[^1];
                }
                else
                {
                    items[^1] = (pathFirstItem, childNode);
                }

                return new DirectoryNode(items);
            }
        }

        throw new NotImplementedException(
            "Unexpected node type: " + GetType());
    }

    /// <summary>
    /// Merges all files from the <paramref name="right"/> tree into the <paramref name="left"/> tree.
    /// For each file in <paramref name="right"/>, inserts or replaces the corresponding file in <paramref name="left"/> at the same path.
    /// If a file exists at the same path in both trees, the file from <paramref name="right"/> overwrites the one in <paramref name="left"/> (last-write-wins).
    /// The merge is performed using sorted upserts to maintain canonical order.
    /// </summary>
    /// <param name="left">The left tree to merge into.</param>
    /// <param name="right">The right tree whose files will be merged into the left tree.</param>
    public static FileTree MergeFiles(
        FileTree left,
        FileTree right) =>
        right.EnumerateFilesTransitive()
        .Aggregate(left, (acc, file) => acc.SetNodeAtPathSorted(file.path, File(file.fileContent)));


    /// <summary>
    /// Returns a new tree containing only the nodes whose immediate edge path satisfies the given predicate.
    /// The filter is applied to each child node's path (relative to the provided <paramref name="currentPrefix"/>).
    /// May yield empty directories; use <see cref="RemoveEmptyNodes(FileTree)"/> to prune them if desired.
    /// </summary>
    /// <param name="node">The root node to filter.</param>
    /// <param name="pathFilter">A predicate that receives the path (as a list of strings) to each child node and returns true to keep the node, or false to exclude it.</param>
    /// <param name="currentPrefix">The current path prefix (used for recursion; callers can usually omit this).</param>
    /// <returns>
    /// A new <see cref="FileTree"/> containing only the nodes whose immediate edge path satisfies the predicate.
    /// </returns>
    public static FileTree FilterNodesByPath(
        FileTree node,
        Func<IReadOnlyList<string>, bool> pathFilter,
        IReadOnlyList<string>? currentPrefix = null) =>
        node switch
        {
            FileNode file => file,

            DirectoryNode tree =>
            new DirectoryNode(
                tree.Items
                .Where(treeNode => pathFilter([.. currentPrefix ?? [], treeNode.name]))
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

    /// <summary>
    /// Removes all empty directory nodes from the tree, returning a new tree with only non-empty nodes.
    /// File nodes are always retained. If the root node becomes empty as a result, returns <c>null</c>.
    /// </summary>
    /// <param name="node">The root node to process for empty directories.</param>
    /// <returns>
    /// A new <see cref="FileTree"/> with all empty directories removed, or <c>null</c> if the entire tree is empty.
    /// </returns>
    public static FileTree? RemoveEmptyNodes(FileTree node)
    {
        if (node is FileNode)
            return node;

        if (node is DirectoryNode directory)
        {
            if (directory.Items.Count is 0)
                return null;

            var newItemsBuilder =
                ImmutableList.CreateBuilder<(string name, FileTree component)>();

            foreach (var (name, component) in directory.Items)
            {
                if (RemoveEmptyNodes(component) is { } newComponent)
                {
                    newItemsBuilder.Add((name, newComponent));
                }
            }

            var newItems = newItemsBuilder.ToImmutable();

            if (newItems.Count is 0)
                return null;

            return new DirectoryNode(newItems);
        }

        throw new NotImplementedException(
            "Unexpected node type: " + node.GetType());
    }

    /// <summary>
    /// Recursively sorts all sibling lists in the tree using ordinal string comparison.
    /// This ensures a canonical order for equality and hashing. File nodes are returned as-is.
    /// </summary>
    /// <param name="node">The root node to sort.</param>
    /// <returns>
    /// A new <see cref="FileTree"/> with all siblings sorted recursively.
    /// </returns>
    public static FileTree Sort(FileTree node) =>
        node switch
        {
            FileNode _ => node,

            DirectoryNode tree =>
            new DirectoryNode(
                tree.Items.Order(TreeEntryDefaultComparer).Select(child => (child.name, Sort(child.component))).ToImmutableList()),

            _ =>
            throw new NotImplementedException(
                "Unexpected node type: " + node.GetType())
        };

    /// <summary>
    /// Folds the tree structure by applying the provided handlers to each node type.
    /// </summary>
    /// <typeparam name="T">The result type returned by the handlers.</typeparam>
    /// <param name="fromFile">A function to handle file nodes, receiving the file's content as <see cref="ReadOnlyMemory{Byte}"/>.</param>
    /// <param name="fromDirectory">A function to handle tree nodes, receiving the list of child items as a list of (item name, item value) pairs.</param>
    /// <returns>
    /// The result of folding the tree using the provided handlers.
    /// </returns>
    public T Map<T>(
        Func<ReadOnlyMemory<byte>, T> fromFile,
        Func<IReadOnlyList<(string itemName, FileTree itemValue)>, T> fromDirectory) =>
        this switch
        {
            DirectoryNode tree =>
            fromDirectory(tree.Items),

            FileNode file =>
            fromFile(file.Bytes),

            _ =>
            throw new NotImplementedException(
                "Unexpected node type: " + GetType())
        };

    private class TreeEntryDefaultComparerClass : IComparer<(string name, FileTree component)>
    {
        public int Compare((string name, FileTree component) x, (string name, FileTree component) y)
        {
            return string.CompareOrdinal(x.name, y.name);
        }
    }
}

