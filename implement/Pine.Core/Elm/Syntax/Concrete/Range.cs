namespace Pine.Core.Elm.Syntax.Concrete;

/// <summary>
/// Inclusive start / exclusive end positions spanning a source fragment.
/// </summary>
public record Range(
    Location Start,
    Location End);
