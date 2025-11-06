using System.Collections.Immutable;

namespace GitCore;

/// <summary>
/// Represents an immutable Git repository containing fetched objects (commits, trees, and blobs).
/// </summary>
public record Repository(
    ImmutableDictionary<string, PackFile.PackObject> Objects)
{
    /// <summary>
    /// Creates an empty repository.
    /// </summary>
    public static Repository Empty { get; } =
        new Repository(ImmutableDictionary<string, PackFile.PackObject>.Empty);

    /// <summary>
    /// Creates a new repository with additional objects merged in.
    /// </summary>
    /// <param name="additionalObjects">Additional objects to add to the repository</param>
    /// <returns>A new repository containing all objects from this repository plus the additional objects</returns>
    public Repository WithObjects(ImmutableDictionary<string, PackFile.PackObject> additionalObjects)
    {
        return new Repository(Objects.AddRange(additionalObjects));
    }

    /// <summary>
    /// Gets an object by its hash, or null if not found.
    /// </summary>
    public PackFile.PackObject? GetObject(string hash)
    {
        return Objects.TryGetValue(hash, out var obj) ? obj : null;
    }

    /// <summary>
    /// Checks if the repository contains an object with the given hash.
    /// </summary>
    public bool ContainsObject(string hash)
    {
        return Objects.ContainsKey(hash);
    }
}
