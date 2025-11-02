using System;
using System.Collections.Generic;

namespace Pine.Core;

/// <summary>
/// Extension methods for navigating and querying <see cref="PineValue"/> trees.
/// </summary>
public static class PineValueExtension
{
    /// <summary>
    /// Navigates a nested <see cref="PineValue"/> list structure by following the given sequence of indices.
    /// </summary>
    /// <param name="environment">The root <see cref="PineValue"/> to start navigation from.</param>
    /// <param name="path">A sequence of zero-based indices indicating which item to select at each list level. Negative indices are treated as 0.</param>
    /// <returns>
    /// The <see cref="PineValue"/> found at the specified path, or <see cref="PineValue.EmptyList"/> if any step
    /// encounters a non-list value or an out-of-range index.
    /// </returns>
    public static PineValue ValueFromPathOrEmptyList(
        this PineValue environment,
        ReadOnlySpan<int> path)
    {
        var currentNode = environment;

        for (var i = 0; i < path.Length; i++)
        {
            if (currentNode is not PineValue.ListValue listValue)
                return PineValue.EmptyList;

            var skipCount = path[i];

            if (skipCount >= listValue.Items.Length)
                return PineValue.EmptyList;

            currentNode = listValue.Items.Span[skipCount < 0 ? 0 : skipCount];
        }

        return currentNode;
    }

    /// <summary>
    /// Navigates a nested <see cref="PineValue"/> list structure by following the given sequence of indices.
    /// </summary>
    /// <param name="environment">The root <see cref="PineValue"/> to start navigation from.</param>
    /// <param name="path">A sequence of zero-based indices indicating which item to select at each list level. Negative indices are treated as 0.</param>
    /// <param name="startOffset">The index in the path to start from.</param>
    /// <returns>
    /// The <see cref="PineValue"/> found at the specified path, or <see cref="PineValue.EmptyList"/> if any step
    /// encounters a non-list value or an out-of-range index.
    /// </returns>
    public static PineValue ValueFromPathOrEmptyList(
        this PineValue environment,
        IReadOnlyList<int> path,
        int startOffset = 0)
    {
        return
            ValueFromPathOrNull(
                environment,
                path,
                startOffset) ?? PineValue.EmptyList;
    }

    /// <summary>
    /// Navigates a nested <see cref="PineValue"/> list structure by following the given sequence of indices.
    /// </summary>
    /// <param name="environment">The root <see cref="PineValue"/> to start navigation from.</param>
    /// <param name="path">A sequence of zero-based indices indicating which item to select at each list level. Negative indices are treated as 0.</param>
    /// <param name="startOffset">The index in the path to start from.</param>
    /// <returns>
    /// The <see cref="PineValue"/> found at the specified path, or <langword>null</langword> if any step
    /// encounters a non-list value or an out-of-range index.
    /// </returns>
    public static PineValue? ValueFromPathOrNull(
        this PineValue environment,
        IReadOnlyList<int> path,
        int startOffset = 0)
    {
        var currentNode = environment;

        for (var i = startOffset; i < path.Count; i++)
        {
            if (currentNode is not PineValue.ListValue listValue)
                return PineValue.EmptyList;

            var skipCount = path[i];

            if (skipCount >= listValue.Items.Length)
                return PineValue.EmptyList;

            currentNode = listValue.Items.Span[skipCount < 0 ? 0 : skipCount];
        }

        return currentNode;
    }

    public static PineValue? ValueFromPathOrNull(
        this PineValue environment,
        ReadOnlySpan<int> path)
    {
        var currentNode = environment;

        for (var i = 0; i < path.Length; i++)
        {
            if (currentNode is not PineValue.ListValue listValue)
                return null;

            var skipCount = path[i];

            if (skipCount >= listValue.Items.Length)
                return null;

            currentNode = listValue.Items.Span[skipCount < 0 ? 0 : skipCount];
        }

        return currentNode;
    }
}
