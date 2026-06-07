using System.Collections.Generic;

namespace Pine.Core.CodeAnalysis;

/// <summary>
/// Represents the fully qualified name of a declaration consisting of a (possibly empty) sequence
/// of namespace identifiers and the simple declaration name.
/// </summary>
public record DeclQualifiedName
    : System.IComparable<DeclQualifiedName>, System.IEquatable<DeclQualifiedName>
{
    /// <summary>
    /// Ordered list of namespace identifiers from outermost to innermost. Can be empty for global namespace.
    /// </summary>
    public IReadOnlyList<string> Namespaces { get; init; }

    /// <summary>
    /// The simple (unqualified) name of the declaration.
    /// </summary>
    public string DeclName { get; init; }

    /// <summary>
    /// Gets the full name composed from <see cref="Namespaces"/> (if any) and <see cref="DeclName"/>, separated by dots.
    /// </summary>
    public string FullName { get; init; }

    private DeclQualifiedName(
        IReadOnlyList<string> namespaces,
        string declName,
        string fullName)
    {
        Namespaces = namespaces;
        DeclName = declName;
        FullName = fullName;
    }

    /// <summary>
    /// Creates a new <see cref="DeclQualifiedName"/> from the given namespace and declaration name components.
    /// </summary>
    static public DeclQualifiedName Create(
        IReadOnlyList<string> namespaces,
        string declName)
    {
        var fullName =
            namespaces.Count > 0
            ?
            string.Join(".", namespaces) + "." + declName
            :
            declName;

        return new DeclQualifiedName(namespaces, declName, fullName);
    }

    /// <inheritdoc/>
    public override string ToString()
    {
        return FullName;
    }

    /// <inheritdoc/>
    public virtual bool Equals(DeclQualifiedName? other)
    {
        if (other is null)
            return false;

        if (ReferenceEquals(this, other))
            return true;

        return FullName == other.FullName;
    }

    /// <inheritdoc/>
    public override int GetHashCode()
    {
        var hash = new System.HashCode();

        hash.Add(DeclName);

        foreach (var ns in Namespaces)
            hash.Add(ns);

        return hash.ToHashCode();
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

        return Create(parts[..^1], parts[^1]);
    }

    /// <summary>
    /// Returns a new <see cref="DeclQualifiedName"/> representing a declaration named <paramref name="containedDeclName"/> 
    /// contained within the current declaration. The new qualified name appends the current <see cref="DeclName"/> 
    /// to the <see cref="Namespaces"/> and sets <see cref="DeclName"/> to <paramref name="containedDeclName"/>.
    /// </summary>
    /// <param name="containedDeclName">The name of the contained declaration.</param>
    /// <returns>
    /// A <see cref="DeclQualifiedName"/> with namespaces extended by the current <see cref="DeclName"/>, 
    /// and <see cref="DeclName"/> set to <paramref name="containedDeclName"/>.
    /// </returns>
    public DeclQualifiedName ContainedDeclName(string containedDeclName)
    {
        return Create([.. Namespaces, DeclName], containedDeclName);
    }

    /// <inheritdoc/>
    public int CompareTo(DeclQualifiedName? other)
    {
        if (ReferenceEquals(this, other))
            return 0;

        if (other is null)
            return 1;

        return string.Compare(FullName, other.FullName, System.StringComparison.Ordinal);
    }
}

