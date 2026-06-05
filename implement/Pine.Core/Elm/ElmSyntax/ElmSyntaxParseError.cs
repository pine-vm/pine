namespace Pine.Core.Elm.ElmSyntax;

/// <summary>
/// Represents an error that occurred during parsing of Elm syntax.
/// </summary>
/// <param name="Location">The location where the parsing error occurred.</param>
/// <param name="Message">The error message describing why parsing failed.</param>
public record struct ElmSyntaxParseError(
    SyntaxModel.Location Location,
    string Message);
