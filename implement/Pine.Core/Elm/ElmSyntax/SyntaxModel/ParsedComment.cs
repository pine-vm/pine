namespace Pine.Core.Elm.ElmSyntax.SyntaxModel;


/// <summary>
/// Represents a parsed Elm comment with type-safe category information.
/// This eliminates the need for repeated string inspection to determine comment type.
/// </summary>
/// <param name="Text">The raw comment text including delimiters.</param>
/// <param name="Category">The syntactic category of the comment.</param>
public record ParsedComment(string Text, CommentCategory Category)
{
    /// <summary>
    /// Parses a comment string to determine its category.
    /// </summary>
    /// <param name="commentText">The raw comment text.</param>
    /// <returns>A ParsedComment with the appropriate category.</returns>
    /// <exception cref="System.NotImplementedException">Thrown when the comment text does not match any known Elm comment format.</exception>
    public static ParsedComment FromText(string commentText)
    {
        var trimmed = commentText.TrimStart();

        if (trimmed.StartsWith("{-|"))
        {
            return new ParsedComment(commentText, CommentCategory.DocComment);
        }

        if (trimmed.StartsWith("{-"))
        {
            var isMultiLine = commentText.Contains('\n');
            return new ParsedComment(commentText,
                isMultiLine ? CommentCategory.BlockComment_MultiLine : CommentCategory.BlockComment_SingleLine);
        }

        if (trimmed.StartsWith("--"))
        {
            return new ParsedComment(commentText, CommentCategory.LineComment);
        }

        throw new System.NotImplementedException(
            $"Unrecognized comment format. Expected comment to start with '{{-|', '{{-', or '--', but got: '" +
            $"{commentText[..System.Math.Min(20, commentText.Length)]}'...");
    }

    /// <summary>
    /// Returns true if this is a documentation comment.
    /// </summary>
    public bool IsDocComment => Category is CommentCategory.DocComment;
}
