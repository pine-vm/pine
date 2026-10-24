using System;
using System.Collections;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Runtime.CompilerServices;
using System.Text.Json.Serialization;

namespace Pine.Core.Elm.ElmCompilerInDotnet.CompilerModel;

/// <summary>An immutable sequence with element-wise equality, suitable for compiler inputs and identities.</summary>
[CollectionBuilder(typeof(ValueList), nameof(ValueList.Create))]
[JsonConverter(typeof(ValueListJsonConverterFactory))]
public sealed class ValueList<T> : IReadOnlyList<T>, IEquatable<ValueList<T>>
{
    private readonly ImmutableArray<T> _items;

    private readonly int _hashCode;

    /// <summary>Copies a sequence into an immutable value.</summary>
    public ValueList(IEnumerable<T> items)
    {
        ArgumentNullException.ThrowIfNull(items);
        _items = [.. items];
        var hash = new HashCode();

        foreach (var item in _items)
        {
            if (item is null)
                throw new ArgumentException("Compiler sequences cannot contain null elements.", nameof(items));

            hash.Add(item);
        }

        _hashCode = hash.ToHashCode();
    }

    /// <inheritdoc/>
    public int Count => _items.Length;

    /// <inheritdoc/>
    public T this[int index] => _items[index];

    /// <inheritdoc/>
    public bool Equals(ValueList<T>? other) =>
        ReferenceEquals(this, other) ||
        other is not null && _hashCode == other._hashCode && _items.SequenceEqual(other._items);

    /// <inheritdoc/>
    public override bool Equals(object? obj) => obj is ValueList<T> other && Equals(other);

    /// <inheritdoc/>
    public override int GetHashCode() => _hashCode;

    /// <inheritdoc/>
    public IEnumerator<T> GetEnumerator() => ((IEnumerable<T>)_items).GetEnumerator();

    IEnumerator IEnumerable.GetEnumerator() => GetEnumerator();
}

/// <summary>Collection-expression support for immutable compiler sequences.</summary>
public static class ValueList
{
    /// <summary>Copies collection-expression elements.</summary>
    public static ValueList<T> Create<T>(ReadOnlySpan<T> items) => new(items.ToArray());
}
