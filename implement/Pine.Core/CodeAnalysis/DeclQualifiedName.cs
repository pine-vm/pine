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

    /// <inheritdoc/>
    public virtual bool Equals(DeclQualifiedName? other)
    {
        if (other is null)
            return false;

        if (ReferenceEquals(this, other))
            return true;

        if (DeclName != other.DeclName)
            return false;

        if (Namespaces.Count != other.Namespaces.Count)
            return false;

        for (var i = 0; i < Namespaces.Count; i++)
        {
            if (Namespaces[i] != other.Namespaces[i])
                return false;
        }

        return true;
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

        if (parts.Length is 1)
            return new DeclQualifiedName(Namespaces: [], DeclName: parts[0]);

        return new DeclQualifiedName(
            Namespaces: parts[..^1],
            DeclName: parts[^1]);
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
    public DeclQualifiedName ContainedDeclName(string containedDeclName) =>
        new(
            Namespaces: [.. Namespaces, DeclName],
            DeclName: containedDeclName);
}

