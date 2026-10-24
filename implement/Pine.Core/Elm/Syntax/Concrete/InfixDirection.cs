namespace Pine.Core.Elm.Syntax.Concrete;

/// <summary>
/// Direction of infix operator association.
/// </summary>
public enum InfixDirection
{
    /// <summary>Left-associative operator.</summary>
    Left,
    /// <summary>Right-associative operator.</summary>
    Right,
    /// <summary>Non-associative operator.</summary>
    Non,
}
