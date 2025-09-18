using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

namespace Pine.Core;

/// <summary>
/// Immutable, value-semantics tree keyed by string path segments, where leaves are byte blobs.
/// </summary>
/// <remarks>
/// <para><b>Overview</b></para>
/// <para>
/// This type models a tiny in-memory filesystem: each edge is named by a <see cref="string"/>, internal
/// nodes are directories (<see cref="TreeNode"/>), and leaves are files (<see cref="BlobNode"/>) holding
/// <see cref="ReadOnlyMemory{Byte}"/> content. All operations are <i>immutable</i>: methods return new trees
/// and never mutate existing instances. Structural equality and hashing are supported and deterministic.
/// </para>
///
/// <para><b>Value semantics &amp; equality</b></para>
/// <list type="bullet">
///   <item>
///     <description>
///       Both <see cref="TreeNode"/> and <see cref="BlobNode"/> are records with precomputed "slim" hash codes.
///       Equality short-circuits on the slim hash and then verifies structure/content:
///       <see cref="BlobNode"/> compares bytes; <see cref="TreeNode"/> compares item count and sequence equality.
///     </description>
///   </item>
///   <item>
///     <description>
///       <b>Order matters</b> for equality and hashing: two trees with the same children in different sibling orders
///       are not equal. Use <see cref="Sort(BlobTreeWithStringPath)"/> or construct via
///       <see cref="SortedTree"/>
///       to normalize ordering for canonical comparisons.
///     </description>
///   </item>
/// </list>
///
/// <para><b>Ordering</b></para>
/// <list type="bullet">
///   <item>
///     <description>
///       Sibling ordering is ordinal and case-sensitive (uses <see cref="string.CompareOrdinal(string, string)"/>).
///       The default comparer is <see cref="TreeEntryDefaultComparer"/>.
///     </description>
///   </item>
///   <item>
///     <description>
///       <see cref="NonSortedTree"/>
///       preserves the order you pass in; <see cref="SortedTree"/>
///       sorts recursively; <see cref="SetNodeAtPathSorted(ReadOnlySpan{string}, BlobTreeWithStringPath)"/> maintains sorted sibling order on upsert.
///     </description>
///   </item>
/// </list>
///
/// <para><b>Path operations (all immutable)</b></para>
/// <list type="bullet">
///   <item>
///     <description>
///       <see cref="GetNodeAtPath(ReadOnlySpan{string})"/>: returns the node at the path or <c>null</c>.
///       An empty path returns the current node.
///     </description>
///   </item>
///   <item>
///     <description>
///       <see cref="SetNodeAtPathSorted(ReadOnlySpan{string}, BlobTreeWithStringPath)"/>: inserts or replaces a node
///       while keeping siblings sorted. If the path traverses a <see cref="BlobNode"/>, that blob is <b>promoted</b> to a directory
///       to hold the new child.
///     </description>
///   </item>
///   <item>
///     <description>
///       <see cref="RemoveNodeAtPath(ReadOnlySpan{string})"/>: returns a new tree with that node removed.
///       Removing with an empty path returns <c>null</c> (i.e., remove the current root). Ancestors are retained even if empty;
///       combine with <see cref="RemoveEmptyNodes(BlobTreeWithStringPath)"/> to prune empty directories.
///     </description>
///   </item>
///   <item>
///     <description>
///       <see cref="EnumerateBlobsTransitive"/>: depth-first traversal yielding <c>(path, bytes)</c> for each blob under the current node.
///     </description>
///   </item>
///   <item>
///     <description>
///       <see cref="MergeBlobs(BlobTreeWithStringPath, BlobTreeWithStringPath)"/>: folds all blobs from <c>right</c> into <c>left</c>
///       via sorted upserts; on conflicts, <c>right</c> overwrites <c>left</c> (last-write-wins).
///     </description>
///   </item>
///   <item>
///     <description>
///       <see cref="FilterNodesByPath(BlobTreeWithStringPath, Func{IReadOnlyList{string}, bool}, System.Collections.Generic.IReadOnlyList{string}?)"/>:
///       keeps only children whose immediate edge satisfies the predicate; may yield empty directories — use
///       <see cref="RemoveEmptyNodes(BlobTreeWithStringPath)"/> afterward if desired.
///     </description>
///   </item>
///   <item>
///     <description>
///       <see cref="Sort(BlobTreeWithStringPath)"/>: recursively sorts all sibling lists using ordinal comparison.
///     </description>
///   </item>
///   <item>
///     <description>
///       <see cref="Map"/>:
///       folds the structure by providing handlers for blobs vs. trees (useful for serialization, metrics, pretty-printing, etc.).
///     </description>
///   </item>
/// </list>
///
/// <para><b>Performance characteristics</b></para>
/// <list type="bullet">
///   <item><description>Sibling lookups/insertions are linear in sibling count (list scan); no hash map or binary search is used.</description></item>
///   <item><description>Operations allocate only along the edited path; untouched subtrees are shared by reference.</description></item>
///   <item><description>Traversal is O(nodes) time and O(depth) extra space (explicit stack).</description></item>
///   <item><description>Hashes are computed once per node; equality benefits from hash short-circuiting before deep comparison.</description></item>
/// </list>
///
/// <para><b>Thread safety</b></para>
/// <para>
/// Instances are effectively immutable and therefore thread-safe to share. However, <see cref="TreeNode.Items"/> is not defensively copied:
/// pass immutable collections (e.g., <see cref="ImmutableList{T}"/>) or private arrays to avoid external mutation after construction.
/// </para>
///
/// <para><b>Common edge cases</b></para>
/// <list type="bullet">
///   <item><description>Empty path: <see cref="GetNodeAtPath(ReadOnlySpan{string})"/> returns <c>this</c>; <see cref="SetNodeAtPathSorted(ReadOnlySpan{string}, BlobTreeWithStringPath)"/> returns the provided node; <see cref="RemoveNodeAtPath(ReadOnlySpan{string})"/> returns <c>null</c>.</description></item>
///   <item><description>Order-sensitive equality: call <see cref="Sort(BlobTreeWithStringPath)"/> (or build via <see cref="SortedTree"/>) before comparing trees from different sources.</description></item>
///   <item><description>Case sensitivity: ordering uses ordinal, case-sensitive comparison.</description></item>
///   <item><description>Blob promotion: inserting under a blob converts it into a tree holding the child path.</description></item>
/// </list>
///
/// <para><b>Example</b></para>
/// <example>
/// <code>
/// using Pine.Core;
/// using System.Linq;
///
/// // Build a canonical tree
/// var root = TreeNodeWithStringPath.SortedTree(new[]
/// {
///     ("readme.md", TreeNodeWithStringPath.Blob("hello"u8.ToArray())),
///     ("src", TreeNodeWithStringPath.EmptyTree),
/// });
///
/// // Upsert a file: src/Program.cs
/// root = root.SetNodeAtPathSorted(new[] { "src", "Program.cs" },
///         TreeNodeWithStringPath.Blob("// entry point"u8.ToArray()));
///
/// // Lookup
/// var node = root.GetNodeAtPath(new[] { "src", "Program.cs" }); // BlobNode
///
/// // Enumerate all blobs
/// foreach (var (path, bytes) in root.EnumerateBlobsTransitive())
///     System.Console.WriteLine($"{string.Join('/', path)} -> {bytes.Length} bytes");
///
/// // Keep only items under "src" and prune empties
/// var filtered = TreeNodeWithStringPath.RemoveEmptyNodes(
///     TreeNodeWithStringPath.FilterNodesByPath(root, p => p.Count >= 1 &amp;&amp; p[0] == "src"));
///
/// // Merge with overwrite semantics (right wins)
/// var right = TreeNodeWithStringPath.SortedTree(new[]
/// {
///     ("readme.md", TreeNodeWithStringPath.Blob("overwritten"u8.ToArray()))
/// });
/// var merged = TreeNodeWithStringPath.MergeBlobs(root, right);
///
/// // Normalize order before equality checks
/// var canonical = TreeNodeWithStringPath.Sort(merged);
/// </code>
/// </example>
///
/// <para><b>When to use</b></para>
/// <para>
/// Ideal for representing content trees (project files, assets, configs) where you need immutable updates,
/// deterministic ordering, structural equality, and straightforward path-based editing.
/// </para>
/// </remarks>
/// <seealso cref="BlobNode"/>
/// <seealso cref="TreeNode"/>
public abstract record BlobTreeWithStringPath : IEquatable<BlobTreeWithStringPath>
{
    /// <summary>
    /// Leaf node representing a binary blob within a <see cref="BlobTreeWithStringPath"/> tree.
    /// </summary>
    /// <remarks>
    /// <para>
    /// A <see cref="BlobNode"/> corresponds to a “file” in a filesystem analogy. Its content is held in
    /// <see cref="Bytes"/> as <see cref="ReadOnlyMemory{Byte}"/> and is treated as immutable by convention:
    /// this type does not copy the buffer you pass in. To ensure immutability, pass a buffer that you will not mutate,
    /// or a slice over a buffer you control exclusively.
    /// </para>
    /// <para><b>Equality</b></para>
    /// <para>
    /// Equality is structural: two <see cref="BlobNode"/> instances are equal when their bytes are sequence-equal.
    /// </para>
    /// <para><b>Usage notes</b></para>
    /// <list type="bullet">
    ///   <item><description>Create instances via <see cref="Blob(ReadOnlyMemory{byte})"/> for clarity.</description></item>
    ///   <item><description>
    ///     When inserting a child under a path that currently resolves to a <see cref="BlobNode"/>,
    ///     APIs such as <see cref="SetNodeAtPathSorted(ReadOnlySpan{string}, BlobTreeWithStringPath)"/>
    ///     will <b>promote</b> the blob to a directory (a <see cref="TreeNode"/>) automatically to hold the new child.
    ///   </description></item>
    ///   <item><description>
    ///     Blobs participate in traversals (e.g., <see cref="EnumerateBlobsTransitive"/>), filters, and merges
    ///     exactly as leaves; they have no children.
    ///   </description></item>
    /// </list>
    /// <para><b>Thread-safety</b></para>
    /// <para>
    /// Instances are safe to share between threads as long as the underlying memory passed to <see cref="Bytes"/>
    /// is not mutated externally after construction.
    /// </para>
    /// </remarks>
    /// <example>
    /// <code>
    /// // Construct via factory:
    /// var blob = TreeNodeWithStringPath.Blob("hello"u8.ToArray());
    ///
    /// // Use in a tree:
    /// var root = TreeNodeWithStringPath.SortedTree(new[] { ("readme.md", blob) });
    /// </code>
    /// </example>
    /// <seealso cref="TreeNode"/>
    /// <seealso cref="BlobTreeWithStringPath"/>
    public record BlobNode : BlobTreeWithStringPath
    {
        private readonly int _slimHashCode;

        /// <summary>
        /// Gets the content of the blob as a read-only memory of bytes.
        /// </summary>
        public ReadOnlyMemory<byte> Bytes { get; }

        /// <summary>
        /// Initializes a new instance of the <see cref="BlobNode"/> class with the specified blob content.
        /// </summary>
        /// <param name="bytes">The content of the blob as a read-only memory of bytes.</param>
        public BlobNode(ReadOnlyMemory<byte> bytes)
        {
            Bytes = bytes;

            var hash = new HashCode();

            hash.AddBytes(bytes.Span);

            _slimHashCode = hash.ToHashCode();
        }

        /// <inheritdoc/>
        public virtual bool Equals(BlobNode? other)
        {
            if (other is null)
                return false;

            return
                _slimHashCode == other._slimHashCode &&
                Bytes.Span.SequenceEqual(other.Bytes.Span);
        }

        /// <inheritdoc/>
        public override int GetHashCode() => _slimHashCode;
    }

    /// <summary>
    /// Internal (directory-like) node containing a list of named children within a <see cref="BlobTreeWithStringPath"/> tree.
    /// </summary>
    /// <remarks>
    /// <para>
    /// A <see cref="TreeNode"/> holds an ordered list of children in <see cref="Items"/> where each child is a pair
    /// <c>(name, component)</c>. Sibling <b>order matters</b> for hashing and equality.
    /// </para>
    /// <para><b>Equality</b></para>
    /// <para>
    /// Equality is structural and order-sensitive: two <see cref="TreeNode"/> instances are equal when their slim hashes match,
    /// their <see cref="Items"/> counts match, and the sequences of items are element-wise equal (including each child’s name and subtree).
    /// To obtain canonical, order-insensitive comparisons across heterogeneous inputs, use <see cref="Sort(BlobTreeWithStringPath)"/>
    /// (or build via <see cref="SortedTree"/>)
    /// to normalize sibling ordering first.
    /// </para>
    /// <para><b>Ordering</b></para>
    /// <list type="bullet">
    ///   <item><description>Default sibling ordering is ordinal and case-sensitive (see <see cref="string.CompareOrdinal(string, string)"/>).</description></item>
    ///   <item><description>
    ///     <see cref="NonSortedTree"/>
    ///     preserves the order you supply; <see cref="SortedTree"/>
    ///     recursively sorts; <see cref="SetNodeAtPathSorted(ReadOnlySpan{string}, BlobTreeWithStringPath)"/> maintains sorted order on upsert.
    ///   </description></item>
    /// </list>
    /// <para><b>Usage notes</b></para>
    /// <list type="bullet">
    ///   <item><description>
    ///     Prefer to construct trees via the factory helpers on <see cref="BlobTreeWithStringPath"/> to make intent explicit
    ///     (e.g., <see cref="SortedTree"/>).
    ///   </description></item>
    ///   <item><description>
    ///     Common operations that rebuild a <see cref="TreeNode"/> while sharing unchanged subtrees include:
    ///     <see cref="SetNodeAtPathSorted(ReadOnlySpan{string}, BlobTreeWithStringPath)"/>,
    ///     <see cref="RemoveNodeAtPath(ReadOnlySpan{string})"/>,
    ///     <see cref="FilterNodesByPath(BlobTreeWithStringPath, Func{IReadOnlyList{string}, bool}, IReadOnlyList{string}?)"/>,
    ///     <see cref="RemoveEmptyNodes(BlobTreeWithStringPath)"/>, and
    ///     <see cref="Sort(BlobTreeWithStringPath)"/>.
    ///   </description></item>
    ///   <item><description>
    ///     Lookups and insertions scan siblings linearly. If you expect many siblings per directory and frequent updates,
    ///     consider batching edits and building via <see cref="SortedTree"/>
    ///     or adding a variant that uses binary search for insertion.
    ///   </description></item>
    /// </list>
    /// <para><b>Thread-safety</b></para>
    /// <para>
    /// Instances are effectively immutable and safe to share across threads provided the <see cref="Items"/> collection is not
    /// mutated externally after construction.
    /// </para>
    /// </remarks>
    /// <example>
    /// <code>
    /// // Build a directory node with two children
    /// var dir = new TreeNodeWithStringPath.TreeNode(new[]
    /// {
    ///     ("readme.md", TreeNodeWithStringPath.Blob("hello"u8.ToArray())),
    ///     ("src", TreeNodeWithStringPath.EmptyTree),
    /// });
    ///
    /// // Canonicalize ordering (if needed)
    /// var canonical = (TreeNodeWithStringPath.TreeNode)TreeNodeWithStringPath.Sort(dir);
    /// </code>
    /// </example>
    /// <seealso cref="BlobNode"/>
    /// <seealso cref="BlobTreeWithStringPath"/>
    public record TreeNode : BlobTreeWithStringPath
    {
        private readonly int _slimHashCode;

        /// <summary>
        /// Gets the list of child nodes for this tree node. Each child is represented as a tuple containing the name of the edge and the corresponding <see cref="BlobTreeWithStringPath"/> component.
        /// </summary>
        public IReadOnlyList<(string name, BlobTreeWithStringPath component)> Items { get; }

        /// <summary>
        /// Initializes a new instance of the <see cref="TreeNode"/> class with the specified items.
        /// </summary>
        /// <param name="items">
        /// The list of child nodes, each represented as a tuple containing the name of the edge and the corresponding <see cref="BlobTreeWithStringPath"/> component.
        /// </param>
        public TreeNode(IReadOnlyList<(string name, BlobTreeWithStringPath component)> items)
        {
            Items = items;

            var hash = new HashCode();

            foreach (var item in items)
            {
                hash.Add(item.GetHashCode());
            }

            _slimHashCode = hash.ToHashCode();
        }

        /// <inheritdoc/>
        public virtual bool Equals(TreeNode? other)
        {
            if (other is null)
                return false;

            return
                _slimHashCode == other._slimHashCode &&
                Items.Count == other.Items.Count &&
                Items.SequenceEqual(other.Items);
        }

        /// <inheritdoc/>
        public override int GetHashCode() => _slimHashCode;
    }

    /// <summary>
    /// Gets the default comparer for tree entries, which compares entries by their name using ordinal string comparison.
    /// </summary>
    public static readonly IComparer<(string name, BlobTreeWithStringPath component)> TreeEntryDefaultComparer =
        new TreeEntryDefaultComparerClass();

    /// <summary>
    /// Creates a new <see cref="BlobNode"/> with the specified blob content.
    /// </summary>
    /// <param name="blobContent">The content of the blob as a read-only memory of bytes.</param>
    /// <returns>A <see cref="BlobNode"/> containing the specified blob content.</returns>
    public static BlobTreeWithStringPath Blob(ReadOnlyMemory<byte> blobContent) =>
        new BlobNode(blobContent);

    /// <summary>
    /// Creates a new <see cref="TreeNode"/> with the specified content, recursively sorting all sibling entries
    /// using ordinal string comparison. This ensures a canonical order for equality and hashing.
    /// </summary>
    /// <param name="treeContent">
    /// The list of child nodes, each represented as a tuple containing the name of the edge and the corresponding <see cref="BlobTreeWithStringPath"/> component.
    /// </param>
    /// <returns>
    /// A <see cref="TreeNode"/> with the specified content, with all siblings sorted recursively.
    /// </returns>
    public static BlobTreeWithStringPath SortedTree(IReadOnlyList<(string name, BlobTreeWithStringPath component)> treeContent) =>
        Sort(NonSortedTree(treeContent));

    /// <summary>
    /// Creates a new <see cref="TreeNode"/> with the specified content, preserving the order of the provided items.
    /// Sibling order is not sorted; use <see cref="SortedTree"/> for canonical ordering.
    /// </summary>
    /// <param name="treeContent">
    /// The list of child nodes, each represented as a tuple containing the name of the edge and the corresponding <see cref="BlobTreeWithStringPath"/> component.
    /// </param>
    /// <returns>
    /// A <see cref="TreeNode"/> with the specified content, preserving the order of the provided items.
    /// </returns>
    public static BlobTreeWithStringPath NonSortedTree(IReadOnlyList<(string name, BlobTreeWithStringPath component)> treeContent) =>
        new TreeNode(treeContent);

    /// <summary>
    /// Gets an empty tree node (a directory with no children).
    /// </summary>
    public static readonly BlobTreeWithStringPath EmptyTree = new TreeNode([]);


    /// <summary>
    /// Enumerates all blobs (leaf nodes) in the tree, yielding each blob's path and content.
    /// Traverses the tree in depth-first order, returning a sequence of tuples where each tuple contains
    /// the path from the root to the blob and the blob's content as <see cref="ReadOnlyMemory{Byte}"/>.
    /// </summary>
    /// <returns>
    /// An <see cref="IEnumerable{T}"/> of tuples, where each tuple contains the path as an <see cref="IImmutableList{String}"/>
    /// and the blob content as <see cref="ReadOnlyMemory{Byte}"/>.
    /// </returns>
    public IEnumerable<(IImmutableList<string> path, ReadOnlyMemory<byte> blobContent)> EnumerateBlobsTransitive()
    {
        var stack = new Stack<(IImmutableList<string> path, BlobTreeWithStringPath node)>();

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
                    foreach (var (name, component) in tree.Items)
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
    /// The <see cref="BlobTreeWithStringPath"/> at the specified path, or <c>null</c> if not found.
    /// </returns>
    public BlobTreeWithStringPath? GetNodeAtPath(IReadOnlyList<string> path) =>
        GetNodeAtPath([.. path]);

    /// <summary>
    /// Returns the node at the specified path, or <c>null</c> if the path does not exist.
    /// An empty path returns the current node.
    /// </summary>
    /// <param name="path">The path to the node as a list of strings.</param>
    /// <returns>
    /// The <see cref="BlobTreeWithStringPath"/> at the specified path, or <c>null</c> if not found.
    /// </returns>
    public BlobTreeWithStringPath? GetNodeAtPath(ReadOnlySpan<string> path)
    {
        if (path.Length is 0)
            return this;

        var pathFirstItem = path[0];

        if (this is TreeNode treeNode)
        {
            for (var i = 0; i < treeNode.Items.Count; i++)
            {
                var (name, component) = treeNode.Items[i];

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
    /// Ancestors are retained even if empty; combine with <see cref="RemoveEmptyNodes(BlobTreeWithStringPath)"/> to prune empty directories.
    /// </summary>
    /// <param name="path">The path to the node as a list of strings.</param>
    /// <returns>
    /// A new <see cref="BlobTreeWithStringPath"/> with the node at the specified path removed, or <c>null</c> if the path is empty.
    /// </returns>
    public BlobTreeWithStringPath? RemoveNodeAtPath(IReadOnlyList<string> path) =>
        RemoveNodeAtPath([.. path]);

    /// <summary>
    /// Returns a new tree with the node at the specified path removed.
    /// An empty path returns <c>null</c> (i.e., removes the current root).
    /// Ancestors are retained even if empty; combine with <see cref="RemoveEmptyNodes(BlobTreeWithStringPath)"/> to prune empty directories.
    /// </summary>
    /// <param name="path">The path to the node as a list of strings.</param>
    /// <returns>
    /// A new <see cref="BlobTreeWithStringPath"/> with the node at the specified path removed, or <c>null</c> if the path is empty.
    /// </returns>
    public BlobTreeWithStringPath? RemoveNodeAtPath(ReadOnlySpan<string> path)
    {
        if (path.Length is 0)
            return null;

        if (this is not TreeNode tree)
            return this;

        var pathFirstItem = path[0];

        if (tree.Items.FirstOrDefault(treeNode => treeNode.name == pathFirstItem) is { } childNodeBefore &&
            childNodeBefore.component is not null)
        {
            var childNodeAfterRemoval =
                childNodeBefore.component.RemoveNodeAtPath(path[1..]);

            if (childNodeAfterRemoval is null)
            {
                var items =
                    new (string name, BlobTreeWithStringPath component)[tree.Items.Count - 1];

                var removed = false;

                for (var i = 0; i < tree.Items.Count; i++)
                {
                    var prevPositionItem = tree.Items[i];

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

                return new TreeNode(items);
            }
            else
            {
                var items =
                    new (string name, BlobTreeWithStringPath component)[tree.Items.Count];

                for (var i = 0; i < tree.Items.Count; i++)
                {
                    items[i] =
                        tree.Items[i].name == pathFirstItem
                        ?
                        (pathFirstItem, childNodeAfterRemoval)
                        :
                        tree.Items[i];
                }

                return new TreeNode(items);
            }
        }

        return this;
    }

    /// <summary>
    /// Inserts or replaces a node at the specified path, maintaining sorted sibling order.
    /// If the path traverses a <see cref="BlobNode"/>, that blob is promoted to a directory to hold the new child.
    /// </summary>
    /// <param name="path">The path at which to insert or replace the node.</param>
    /// <param name="node">The node to insert or replace at the specified path.</param>
    /// <returns>
    /// A new <see cref="BlobTreeWithStringPath"/> with the node inserted or replaced at the specified path.
    /// </returns>
    public BlobTreeWithStringPath SetNodeAtPathSorted(
        IReadOnlyList<string> path,
        BlobTreeWithStringPath node) =>
        SetNodeAtPathSorted([.. path], node);

    /// <summary>
    /// Inserts or replaces a node at the specified path, maintaining sorted sibling order.
    /// If the path traverses a <see cref="BlobNode"/>, that blob is promoted to a directory to hold the new child.
    /// </summary>
    /// <param name="path">The path at which to insert or replace the node, as a <c>ReadOnlySpan&lt;string&gt;</c>.</param>
    /// <param name="node">The node to insert or replace at the specified path.</param>
    /// <returns>
    /// A new <see cref="BlobTreeWithStringPath"/> with the node inserted or replaced at the specified path.
    /// </returns>
    public BlobTreeWithStringPath SetNodeAtPathSorted(
        ReadOnlySpan<string> path,
        BlobTreeWithStringPath node)
    {
        if (path.Length is 0)
            return node;

        var pathFirstItem = path[0];

        if (this is BlobNode)
        {
            var childNode =
                EmptyTree.SetNodeAtPathSorted(path[1..], node);

            return
                new TreeNode(
                    ImmutableList.Create((pathFirstItem, childNode)));
        }

        if (this is TreeNode treeNode)
        {
            if (treeNode.Items.FirstOrDefault(treeNode => treeNode.name == pathFirstItem) is { } childNodeBefore &&
                childNodeBefore.component is not null)
            {
                var childNode =
                    childNodeBefore.component.SetNodeAtPathSorted(path[1..], node);

                var items =
                    new (string name, BlobTreeWithStringPath component)[treeNode.Items.Count];

                for (var i = 0; i < treeNode.Items.Count; i++)
                {
                    items[i] =
                        treeNode.Items[i].name == pathFirstItem
                        ?
                        (pathFirstItem, childNode)
                        :
                        treeNode.Items[i];
                }

                return new TreeNode(items);
            }

            {
                var childNode =
                    EmptyTree.SetNodeAtPathSorted(path[1..], node);

                var items =
                    new (string name, BlobTreeWithStringPath component)[treeNode.Items.Count + 1];

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

                return new TreeNode(items);
            }
        }

        throw new NotImplementedException(
            "Unexpected node type: " + GetType());
    }

    /// <summary>
    /// Merges all blobs from the <paramref name="right"/> tree into the <paramref name="left"/> tree.
    /// For each blob in <paramref name="right"/>, inserts or replaces the corresponding blob in <paramref name="left"/> at the same path.
    /// If a blob exists at the same path in both trees, the blob from <paramref name="right"/> overwrites the one in <paramref name="left"/> (last-write-wins).
    /// The merge is performed using sorted upserts to maintain canonical order.
    /// </summary>
    /// <param name="left">The left tree to merge into.</param>
    /// <param name="right">The right tree whose blobs will be merged into the left tree.</param>
    /// <returns>
    /// A new <see cref="BlobTreeWithStringPath"/> representing the merged tree, with blobs from <paramref name="right"/> overwriting those in <paramref name="left"/> on conflicts.
    /// </returns>
    public static BlobTreeWithStringPath MergeBlobs(
        BlobTreeWithStringPath left,
        BlobTreeWithStringPath right) =>
        right.EnumerateBlobsTransitive()
        .Aggregate(left, (acc, blob) => acc.SetNodeAtPathSorted(blob.path, Blob(blob.blobContent)));


    /// <summary>
    /// Returns a new tree containing only the nodes whose immediate edge path satisfies the given predicate.
    /// The filter is applied to each child node's path (relative to the provided <paramref name="currentPrefix"/>).
    /// Blob nodes are always retained. May yield empty directories; use <see cref="RemoveEmptyNodes(BlobTreeWithStringPath)"/> to prune them if desired.
    /// </summary>
    /// <param name="node">The root node to filter.</param>
    /// <param name="pathFilter">A predicate that receives the path (as a list of strings) to each child node and returns true to keep the node, or false to exclude it.</param>
    /// <param name="currentPrefix">The current path prefix (used for recursion; callers can usually omit this).</param>
    /// <returns>
    /// A new <see cref="BlobTreeWithStringPath"/> containing only the nodes whose immediate edge path satisfies the predicate.
    /// </returns>
    public static BlobTreeWithStringPath FilterNodesByPath(
        BlobTreeWithStringPath node,
        Func<IReadOnlyList<string>, bool> pathFilter,
        IReadOnlyList<string>? currentPrefix = null) =>
        node switch
        {
            BlobNode blob => blob,

            TreeNode tree =>
            new TreeNode(
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
    /// Blob nodes are always retained. If the root node becomes empty as a result, returns <c>null</c>.
    /// </summary>
    /// <param name="node">The root node to process for empty directories.</param>
    /// <returns>
    /// A new <see cref="BlobTreeWithStringPath"/> with all empty directories removed, or <c>null</c> if the entire tree is empty.
    /// </returns>
    public static BlobTreeWithStringPath? RemoveEmptyNodes(BlobTreeWithStringPath node)
    {
        if (node is BlobNode)
            return node;

        if (node is TreeNode tree)
        {
            if (tree.Items.Count is 0)
                return null;

            var newItemsBuilder =
                ImmutableList.CreateBuilder<(string name, BlobTreeWithStringPath component)>();

            foreach (var (name, component) in tree.Items)
            {
                if (RemoveEmptyNodes(component) is { } newComponent)
                {
                    newItemsBuilder.Add((name, newComponent));
                }
            }

            var newItems = newItemsBuilder.ToImmutable();

            if (newItems.Count is 0)
                return null;

            return new TreeNode(newItems);
        }

        throw new NotImplementedException(
            "Unexpected node type: " + node.GetType());
    }

    /// <summary>
    /// Recursively sorts all sibling lists in the tree using ordinal string comparison.
    /// This ensures a canonical order for equality and hashing. Blob nodes are returned as-is.
    /// </summary>
    /// <param name="node">The root node to sort.</param>
    /// <returns>
    /// A new <see cref="BlobTreeWithStringPath"/> with all siblings sorted recursively.
    /// </returns>
    public static BlobTreeWithStringPath Sort(BlobTreeWithStringPath node) =>
        node switch
        {
            BlobNode _ => node,
            TreeNode tree =>
            new TreeNode(tree.Items.Order(TreeEntryDefaultComparer).Select(child => (child.name, Sort(child.component))).ToImmutableList()),

            _ => throw new NotImplementedException()
        };

    /// <summary>
    /// Folds the tree structure by applying the provided handlers to each node type.
    /// </summary>
    /// <typeparam name="T">The result type returned by the handlers.</typeparam>
    /// <param name="fromBlob">A function to handle blob nodes, receiving the blob's content as <see cref="ReadOnlyMemory{Byte}"/>.</param>
    /// <param name="fromTree">A function to handle tree nodes, receiving the list of child items as a list of (item name, item value) pairs.</param>
    /// <returns>
    /// The result of folding the tree using the provided handlers.
    /// </returns>
    public T Map<T>(
        Func<ReadOnlyMemory<byte>, T> fromBlob,
        Func<IReadOnlyList<(string itemName, BlobTreeWithStringPath itemValue)>, T> fromTree) =>
        this switch
        {
            TreeNode tree =>
            fromTree(tree.Items),

            BlobNode blob =>
            fromBlob(blob.Bytes),

            _ =>
            throw new NotImplementedException()
        };

    private class TreeEntryDefaultComparerClass : IComparer<(string name, BlobTreeWithStringPath component)>
    {
        public int Compare((string name, BlobTreeWithStringPath component) x, (string name, BlobTreeWithStringPath component) y)
        {
            return string.CompareOrdinal(x.name, y.name);
        }
    }
}

