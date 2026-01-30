using System.Collections.Generic;

using ModuleName = System.Collections.Generic.IReadOnlyList<string>;

namespace Pine.Core.Elm.ElmSyntax.SyntaxModel;

/// <summary>
/// Wrapper pairing a value with its source range in the parsed text.
/// </summary>
public record Node<T>(
    Range Range,
    T Value)
{
    /// <summary>
    /// Casts the contained value to another type without changing range.
    /// </summary>
    public Node<TOther> Cast<TOther>()
        where TOther : notnull
    {
        return new Node<TOther>(Range, (TOther)(object)Value!);
    }

    /// <summary>
    /// Creates a new node with the specified start and end locations, updating the range accordingly.
    /// </summary>
    public Node<T> WithRange(
        Location newStart,
        Location newEnd) =>
        this
        with
        {
            Range = new Range(newStart, newEnd)
        };

    /// <inheritdoc/>
    public virtual bool Equals(Node<T>? other)
    {
        if (ReferenceEquals(this, other))
            return true;

        if (other is null)
            return false;

        if (Range != other.Range)
            return false;

        if (Value is ModuleName list && other.Value is ModuleName otherList)
        {
            return System.Linq.Enumerable.SequenceEqual(list, otherList);
        }

        // Handle tuples containing ModuleName (e.g., (ModuleName ModuleName, string Name))
        if (Value is System.ValueTuple<ModuleName, string> tuple &&
            other.Value is System.ValueTuple<ModuleName, string> otherTuple)
        {
            return System.Linq.Enumerable.SequenceEqual(tuple.Item1, otherTuple.Item1) &&
                   tuple.Item2 == otherTuple.Item2;
        }

        return EqualityComparer<T>.Default.Equals(Value, other.Value);
    }

    /// <inheritdoc/>
    public override int GetHashCode()
    {
        var hashCode = new System.HashCode();

        hashCode.Add(Range);

        if (Value is ModuleName list)
        {
            foreach (var item in list)
            {
                hashCode.Add(item);
            }
        }
        else if (Value is System.ValueTuple<ModuleName, string> tuple)
        {
            foreach (var item in tuple.Item1)
            {
                hashCode.Add(item);
            }
            hashCode.Add(tuple.Item2);
        }
        else
        {
            hashCode.Add(Value);
        }

        return hashCode.ToHashCode();
    }
}
