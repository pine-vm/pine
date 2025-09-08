using System.Collections.Generic;

namespace Pine.Core.CodeAnalysis;


/// <summary>
/// Comparer that orders <see cref="PineValueClass"/> instances by how specific their constraints are.
/// More general classes (constraints satisfied by more classes) sort before more specific ones.
/// </summary>
/// <remarks>
/// Ordering rules:
/// <list type="number">
/// <item>
/// <description>
/// If <c>x.SatisfiedByConstraint(y)</c> and not <c>y.SatisfiedByConstraint(x)</c>, then x is more general and sorts before y.
/// </description>
/// </item>
/// <item>
/// <description>
/// If <c>y.SatisfiedByConstraint(x)</c> and not <c>x.SatisfiedByConstraint(y)</c>, then y is more general and sorts before x.
/// </description>
/// </item>
/// <item>
/// <description>
/// Otherwise, ties are broken by <see cref="PineValueClass.ParsedItems"/> count (fewer parsed items sorts first).
/// </description>
/// </item>
/// </list>
/// Note that this comparer can return 0 for distinct instances (e.g., incomparable classes with the same
/// number of <see cref="PineValueClass.ParsedItems"/>), so it does not define a strict total order. Be careful when using it
/// with data structures that assume a strict-weak ordering (like <see cref="SortedSet{T}"/>).
/// </remarks>
public class PineValueClassSpecificityComparer : IComparer<PineValueClass>
{
    /// <summary>
    /// A reusable singleton instance of the comparer.
    /// </summary>
    public readonly static PineValueClassSpecificityComparer Instance = new();

    /// <summary>
    /// Compares two <see cref="PineValueClass"/> instances by generality/specificity.
    /// </summary>
    /// <param name="x">Left operand.</param>
    /// <param name="y">Right operand.</param>
    /// <returns>
    /// A negative number if <paramref name="x"/> sorts before <paramref name="y"/>,
    /// zero if they are considered equivalent by this comparer, or a positive number otherwise.
    /// </returns>
    /// <remarks>
    /// Null handling: <c>null</c> sorts before non-null.
    /// Generality: If only one side's constraints are satisfied by the other (<see cref="PineValueClass.SatisfiedByConstraint(PineValueClass)"/>),
    /// the more general (subset of constraints) sorts first. As a final tie-breaker, the number of parsed items is compared
    /// (fewer items = more general = sorts first).
    /// </remarks>
    public int Compare(PineValueClass? x, PineValueClass? y)
    {
        if (x is null && y is null)
            return 0;

        if (x is null)
            return -1;

        if (y is null)
            return 1;

        if (x.SatisfiedByConstraint(y))
        {
            if (!y.SatisfiedByConstraint(x))
            {
                return -1;
            }
        }

        if (y.SatisfiedByConstraint(x))
        {
            if (!x.SatisfiedByConstraint(y))
            {
                return 1;
            }
        }

        return x.ParsedItems.Count - y.ParsedItems.Count;
    }
}
