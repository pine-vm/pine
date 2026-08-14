using System.Collections.Generic;

namespace Pine.Core.Elm.LanguageServer.LanguageServiceInterface;


/// <summary>
/// Represents a node in a file tree.
/// </summary>
public abstract record FileTreeNode<BlobT>
{
    /// <summary>
    /// Represents a blob in a file tree.
    /// </summary>
    public record BlobNode(BlobT Blob)
        : FileTreeNode<BlobT>;

    /// <summary>
    /// Represents a directory in a file tree.
    /// </summary>
    public record TreeNode(
        IReadOnlyList<(string name, FileTreeNode<BlobT> node)> Children)
        : FileTreeNode<BlobT>;
}
