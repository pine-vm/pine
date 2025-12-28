namespace Pine.Core.Elm.ElmSyntax.Stil4mConcretized;


/// <summary>
/// Categorizes an Elm comment by its syntactic type.
/// Elm supports line comments (--) and block comments ({- -}), with doc comments ({-|) as a special case.
/// </summary>
public enum CommentCategory
{
    /// <summary>
    /// A single-line comment starting with --.
    /// Example: -- this is a line comment
    /// </summary>
    LineComment,

    /// <summary>
    /// A single-line block comment delimited by {- and -} on the same line.
    /// Example: {- this is a block comment -}
    /// </summary>
    BlockComment_SingleLine,

    /// <summary>
    /// A multi-line block comment delimited by {- and -} spanning multiple lines.
    /// Example:
    /// {- this is a
    ///    multi-line block comment -}
    /// </summary>
    BlockComment_MultiLine,

    /// <summary>
    /// A documentation comment starting with {-|.
    /// Doc comments are attached to the following declaration.
    /// Example: {-| This documents a function -}
    /// </summary>
    DocComment
}

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
