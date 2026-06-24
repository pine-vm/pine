namespace Pine.Core.Elm.ElmSyntax;

/// <summary>
/// Represents an error that occurred during parsing of Elm syntax.
/// </summary>
/// <param name="Location">The location where the parsing error occurred.</param>
/// <param name="Message">The error message describing why parsing failed.</param>
public record struct ElmSyntaxParseError(
    SyntaxModel.Location Location,
    string Message)
{
    /// <inheritdoc/>
    public override readonly string ToString()
    {
        return RenderDisplayString(this);
    }

    /// <summary>
    /// Renders the given Elm syntax parse error as a human-readable display string.
    /// </summary>
    public static string RenderDisplayString(ElmSyntaxParseError elmSyntaxParseError) =>
        $"{elmSyntaxParseError.Location.Row}:{elmSyntaxParseError.Location.Column}: {elmSyntaxParseError.Message}";
}
