namespace Pine.Core.Elm.ElmSyntax.SyntaxModel;

/// <summary>
/// One-based line and column within source text.
/// </summary>
public readonly record struct Location(
    int Row,
    int Column);
