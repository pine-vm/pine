using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

namespace Pine.Core.CodeAnalysis;

/// <summary>
/// Describes the set of environment paths an expression depends.
/// </summary>
public abstract record ExpressionEnvClass
{
    /// <summary>
    /// Represents the case where no constraints could be determined for the environment of an expression.
    /// </summary>
    public record UnconstrainedEnv
        : ExpressionEnvClass;

    /// <summary>
    /// Represents a constrained environment containing the set of paths observed by the expression.
    /// </summary>
    public record ConstrainedEnv
        : ExpressionEnvClass
    {
        /// <summary>
        /// The set of integer paths into the environment that the expression (directly or indirectly) reads.
        /// </summary>
        public ImmutableHashSet<IReadOnlyList<int>> ParsedEnvItems { get; }

        /// <summary>
        /// Creates a new constrained environment from the given collection of observed paths.
        /// </summary>
        /// <param name="parsedEnvItems">Paths observed in the environment. Duplicates are removed using <see cref="IntPathEqualityComparer"/>.</param>
        public ConstrainedEnv(
            IEnumerable<IReadOnlyList<int>> parsedEnvItems)
        {
            ParsedEnvItems =
                parsedEnvItems.ToImmutableHashSet(equalityComparer: IntPathEqualityComparer.Instance);
        }

        /// <summary>
        /// Computes a hash code from the observed paths.
        /// </summary>
        /// <returns>A hash code that aggregates the hash codes of all paths in <see cref="ParsedEnvItems"/>.</returns>
        public override int GetHashCode() =>
            ParsedEnvItems.Aggregate(0, (acc, next) => acc ^ next.GetHashCode());

        /// <summary>
        /// Determines equality by comparing the sets of observed paths.
        /// </summary>
        /// <param name="other">The other constrained environment.</param>
        /// <returns>True if both contain exactly the same paths; otherwise false.</returns>
        public virtual bool Equals(ConstrainedEnv? other) =>
            other is not null &&
            other.ParsedEnvItems.SetEquals(ParsedEnvItems);
    }

    /// <summary>
    /// Compares two environment classes for equality.
    /// </summary>
    /// <param name="env1">First environment class.</param>
    /// <param name="env2">Second environment class.</param>
    /// <returns>
    /// True if both references are the same, or both are constrained with equal path sets, or either is <see cref="UnconstrainedEnv"/>;
    /// otherwise false.
    /// </returns>
    public static bool Equal(ExpressionEnvClass? env1, ExpressionEnvClass? env2)
    {
        if (ReferenceEquals(env1, env2))
            return true;

        if (env1 is null || env2 is null)
            return false;

        if (env1 is UnconstrainedEnv || env2 is UnconstrainedEnv)
            return true;

        if (env1 is ConstrainedEnv constrainedEnv1 && env2 is ConstrainedEnv constrainedEnv2)
            return constrainedEnv1.Equals(constrainedEnv2);

        throw new NotImplementedException();
    }

    /// <summary>
    /// Attempts to map a path within a child environment-building expression back to the parent environment.
    /// </summary>
    /// <param name="envExpr">The expression that constructs the child environment.</param>
    /// <param name="path">The path to resolve within <paramref name="envExpr"/>.</param>
    /// <returns>
    /// A mapping descriptor to the parent environment when possible; otherwise null. If <paramref name="path"/> is empty,
    /// returns the mapping for <paramref name="envExpr"/> itself.
    /// </returns>
    public static IReadOnlyList<int>? TryMapPathToParentEnvironment(
        Expression envExpr,
        IReadOnlyList<int> path)
    {
        if (path.Count is 0)
            return CodeAnalysis.TryParseExprAsPathInEnv(envExpr);

        var currentIndex = path[0];

        if (envExpr is not Expression.List listExpr)
            return null;

        if (currentIndex < 0)
            return null;

        if (currentIndex >= listExpr.Items.Count)
            return null;

        return TryMapPathToParentEnvironment(listExpr.Items[currentIndex], [.. path.Skip(1)]);
    }
}
