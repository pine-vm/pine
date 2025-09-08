using System;
using System.Collections.Generic;
using System.Linq;

namespace Pine.Core.CodeAnalysis;

/// <summary>
/// Represents the mapping of a Pine <see cref="Expression"/> to a path in the parent environment.
/// </summary>
public abstract record ExprMappedToParentEnv
{
    /// <summary>
    /// Expression maps to a path (list indices) in the parent environment.
    /// </summary>
    /// <param name="Path">Zero-based indices from the root environment to the target item.</param>
    public record PathInParentEnv(IReadOnlyList<int> Path)
        : ExprMappedToParentEnv
    {
        /// <inheritdoc />
        public virtual bool Equals(PathInParentEnv? other)
        {
            if (ReferenceEquals(this, other))
                return true;

            if (other is null)
                return false;

            return Path.SequenceEqual(other.Path);
        }

        /// <inheritdoc />
        public override int GetHashCode()
        {
            return base.GetHashCode();
        }
    }

    /*
     * TODO: Consider removing variant LiteralInParentEnv entirely:
     * Where we use this so far, instead use a method to attempt to interpret as literal and branch on that.
     * */

    /// <summary>
    /// Expression maps to a literal value available directly in the parent environment.
    /// </summary>
    /// <param name="Value">The literal Pine value.</param>
    public record LiteralInParentEnv(PineValue Value)
        : ExprMappedToParentEnv;
}
