module ElmSyntax.Concrete.SeparatedSyntaxList exposing (..)

import ElmSyntax.Concrete.Range exposing (Location)



{-


/// <summary>
/// A list of syntax nodes separated by delimiters, such as commas in a parameter list.
/// This model stores the locations of delimiter tokens alongside the nodes they separate.
/// It does not store the actual delimiter tokens, therefore only fit for contexts where the delimiter type is known and fixed.
/// </summary>
public abstract record SeparatedSyntaxList<TNode>
    : IEnumerable<TNode>
{
    /// <summary>
    /// The case of an empty list.
    /// </summary>
    public sealed record Empty
        : SeparatedSyntaxList<TNode>;

    /// <summary>
    /// Represents a separated syntax list that is guaranteed to contain at least one node.
    /// </summary>
    public sealed record NonEmpty(
        TNode First,
        IReadOnlyList<(Location SeparatorLocation, TNode Node)> Rest)
        : SeparatedSyntaxList<TNode>
    {
        /// <inheritdoc/>
        public bool Equals(NonEmpty? other)
        {
            if (ReferenceEquals(this, other))
                return true;

            if (other is null)
                return false;

            return
                EqualityComparer<TNode>.Default.Equals(First, other.First) &&
                Enumerable.SequenceEqual(Rest, other.Rest);
        }

        /// <inheritdoc/>
        public override int GetHashCode()
        {
            var hashCode = new System.HashCode();

            hashCode.Add(First);

            foreach (var item in Rest)
                hashCode.Add(item);

            return hashCode.ToHashCode();
        }
    }

    /// <summary>
    /// Gets the count of elements in the list.
    /// </summary>
    public int Count =>
        this switch
        {
            Empty =>
            0,

            NonEmpty nonEmpty =>
            1 + nonEmpty.Rest.Count,

            _ =>
            throw new System.NotImplementedException(
                "Unexpected type: " + GetType().FullName)
        };

    /// <summary>
    /// Gets all nodes in the list as an enumerable.
    /// </summary>
    public IEnumerable<TNode> Nodes =>
        this switch
        {
            Empty =>
            [],

            NonEmpty nonEmpty =>
            new[] { nonEmpty.First }.Concat(nonEmpty.Rest.Select(r => r.Node)),

            _ =>
            throw new System.NotImplementedException(
                "Unexpected type: " + GetType().FullName)
        };

    /// <summary>
    /// Gets the element at the specified index.
    /// </summary>
    public TNode this[int index] => this switch
    {
        NonEmpty nonEmpty when index is 0 =>
        nonEmpty.First,

        NonEmpty nonEmpty when index > 0 && index <= nonEmpty.Rest.Count =>
        nonEmpty.Rest[index - 1].Node,

        _ =>
        throw new System.ArgumentOutOfRangeException(nameof(index))
    };

    /// <inheritdoc/>
    public IEnumerator<TNode> GetEnumerator()
    {
        switch (this)
        {
            case Empty:
                yield break;

            case NonEmpty nonEmpty:

                yield return nonEmpty.First;

                foreach (var item in nonEmpty.Rest)
                {
                    yield return item.Node;
                }

                break;

            default:
                throw new System.NotImplementedException(
                    "Unexpected type: " + this.GetType().FullName);
        }
    }

    IEnumerator IEnumerable.GetEnumerator()
    {
        return GetEnumerator();
    }
}

-}


type SeparatedSyntaxList a
    = Empty
    | NonEmpty a (List ( Location, a ))
