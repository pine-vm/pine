namespace Pine.Core.Elm.Syntax.Concrete;

/// <summary>
/// One-based line and column within source text.
/// </summary>
public readonly record struct Location(
    int Row,
    int Column);
