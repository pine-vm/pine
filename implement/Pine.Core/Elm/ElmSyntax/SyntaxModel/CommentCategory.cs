namespace Pine.Core.Elm.ElmSyntax.SyntaxModel;

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
