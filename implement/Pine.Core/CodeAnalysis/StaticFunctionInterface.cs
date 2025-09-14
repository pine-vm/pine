using System.Collections.Generic;
using System.Linq;

namespace Pine.Core.CodeAnalysis;

/// <summary>
/// The interface to call a function.
/// </summary>
/// <param name="ParamsPaths">
/// The list of parameter reference paths. Each path is a sequence of integers describing how to
/// locate the parameter relative to the root environment.
/// The list MUST be sorted using <c>IntPathComparer</c> for deterministic ordering; prefer
/// <see cref="FromPathsSorted(IEnumerable{IReadOnlyList{int}})"/> when the input ordering is unknown.
/// </param>
public record StaticFunctionInterface(
    IReadOnlyList<IReadOnlyList<int>> ParamsPaths)
{
    private readonly string _hashString = ComputeHashString(ParamsPaths);

    /// <summary>
    /// Creates a <see cref="StaticFunctionInterface"/> instance from an arbitrary enumeration of
    /// parameter paths, sorting them deterministically using <c>IntPathComparer</c>.
    /// </summary>
    /// <param name="paramsPaths">Enumeration of parameter reference paths (unsorted).</param>
    /// <returns>A <see cref="StaticFunctionInterface"/> instance with paths sorted.</returns>
    public static StaticFunctionInterface FromPathsSorted(
        IEnumerable<IReadOnlyList<int>> paramsPaths)
    {
        return
            new StaticFunctionInterface([.. paramsPaths.Order(IntPathComparer.Instance)]);
    }

    /// <inheritdoc/>
    public override int GetHashCode()
    {
        return _hashString.GetHashCode();
    }

    /// <inheritdoc/>
    public virtual bool Equals(StaticFunctionInterface? other)
    {
        return other is not null && _hashString == other._hashString;
    }

    /// <summary>
    /// Builds the canonical hash string representation for a collection of parameter paths.
    /// </summary>
    /// <param name="paramsPaths">Sorted list of paths.</param>
    /// <returns>Canonical string encoding used for equality and hashing.</returns>
    private static string ComputeHashString(IReadOnlyList<IReadOnlyList<int>> paramsPaths)
    {
        var builder = new System.Text.StringBuilder();

        builder.Append(paramsPaths.Count.ToString().PadLeft(3, '0'));

        for (var i = 0; i < paramsPaths.Count; i++)
        {
            if (i > 0)
            {
                builder.Append('|');
            }

            var path = paramsPaths[i];

            for (var j = 0; j < path.Count; j++)
            {
                if (j > 0)
                {
                    builder.Append(',');
                }

                builder.Append(path[j]);
            }
        }

        return builder.ToString();
    }
}
