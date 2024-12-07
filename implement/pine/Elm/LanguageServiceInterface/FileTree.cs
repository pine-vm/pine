using System.Collections.Generic;

namespace Pine.Elm.LanguageServiceInterface;


public abstract record FileTreeNode<BlobT>
{
    public record BlobNode(BlobT Blob)
        : FileTreeNode<BlobT>;

    public record TreeNode(
        IReadOnlyList<(string name, FileTreeNode<BlobT> node)> Children)
        : FileTreeNode<BlobT>;
}
