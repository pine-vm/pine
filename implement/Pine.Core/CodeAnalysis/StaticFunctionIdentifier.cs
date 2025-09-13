using System;

namespace Pine.Core.CodeAnalysis;

/// <summary>
/// Identifies a static function description resulting from code analysis.
/// </summary>
/// <remarks>
/// In code analysis, a static function is initially identified by:
/// <list type="bullet">
/// <item><description><see cref="EncodedExpr"/>: The canonical Pine value encoding of the function body expression.</description></item>
/// <item><description><see cref="EnvClass"/>: A <see cref="PineValueClass"/> describing the (observed / constrained) shape and concrete items of the evaluation environment that the function interprets as further program code.</description></item>
/// </list>
/// Two <see cref="StaticFunctionIdentifier"/> instances are equal iff both their <see cref="EncodedExpr"/> and <see cref="EnvClass"/> are equal (reference or structural equality as implemented on those types).
/// This identifier is used by code analysis stages to cache derivations (e.g. inferred types, compiled artifacts) and avoid recomputation.
/// </remarks>
/// <param name="EncodedExpr">Canonical Pine value encoding of the function body.</param>
/// <param name="EnvClass">Constraint describing required environment structure/items for evaluation.</param>
public record StaticFunctionIdentifier(
    PineValue EncodedExpr,
    PineValueClass EnvClass)
{
    /// <summary>
    /// Canonical Pine value encoding of the function body expression.
    /// </summary>
    public PineValue EncodedExpr { get; } = EncodedExpr;

    /// <summary>
    /// Constrained environment class describing which environment items (paths and their values) the function depends on.
    /// </summary>
    public PineValueClass EnvClass { get; } = EnvClass;

    /// <inheritdoc />
    public virtual bool Equals(StaticFunctionIdentifier? other)
    {
        if (other is null)
        {
            return false;
        }

        return EncodedExpr == other.EncodedExpr && EnvClass == other.EnvClass;
    }

    /// <inheritdoc />
    public override int GetHashCode()
    {
        return HashCode.Combine(EncodedExpr, EnvClass);
    }
}
