using System.Collections.Generic;

namespace Pine.Core.CodeAnalysis;

/// <summary>
/// Represents the fully qualified name of a declaration consisting of a (possibly empty) sequence
/// of namespace identifiers and the simple declaration name.
/// </summary>
/// <param name="Namespaces">Ordered list of namespace identifiers from outermost to innermost. Can be empty for global namespace.</param>
/// <param name="DeclName">The simple (unqualified) name of the declaration.</param>
public record DeclQualifiedName(
    IReadOnlyList<string> Namespaces,
    string DeclName)
{
    /// <summary>
    /// Gets the full name composed from <see cref="Namespaces"/> (if any) and <see cref="DeclName"/>, separated by dots.
    /// </summary>
    public string FullName =>
        Namespaces.Count is 0
        ?
        DeclName
        :
        string.Join(".", Namespaces) + "." + DeclName;


    /// <inheritdoc/>
    public override string ToString()
    {
        return FullName;
    }

    /// <summary>
    /// Parses a string representation of a fully qualified declaration name.
    /// </summary>
    /// <param name="fullName">The dot separated full name (e.g. "System.Collections.Generic.List").
    /// A name without dots is interpreted as <see cref="DeclName"/> in the global namespace.</param>
    /// <returns>A <see cref="DeclQualifiedName"/> instance representing the supplied <paramref name="fullName"/>.</returns>
    /// <exception cref="System.ArgumentException">Thrown when <paramref name="fullName"/> is empty.</exception>
    public static DeclQualifiedName FromString(string fullName)
    {
        var parts = fullName.Split('.');

        if (parts.Length is 0)
            throw new System.ArgumentException("Full name cannot be empty.", nameof(fullName));

        if (parts.Length is 1)
            return new DeclQualifiedName(Namespaces: [], DeclName: parts[0]);

        return new DeclQualifiedName(
            Namespaces: parts[..^1],
            DeclName: parts[^1]);
    }
}

