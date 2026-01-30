namespace Pine.Core.Elm.ElmSyntax.SyntaxModel;

/// <summary>
/// Inclusive start / exclusive end positions spanning a source fragment.
/// </summary>
public record Range(
    Location Start,
    Location End);
