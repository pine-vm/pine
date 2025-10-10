using System.Collections.Generic;

namespace Pine.Core.DotNet.Builtins;

/// <summary>
/// Tracks a sequence of <see cref="KernelFunction.concat"/> applications to enable evaluation at a later time.
/// This mutating variant offers cheaper appending on average, compared to <see cref="ImmutableConcatBuilder"/>.
/// Use this variant only when you can guarantee single-owner append-only usage until evaluation.
/// </summary>
public class MutatingConcatBuilder
{
    readonly List<PineValue> _values = [];

    private MutatingConcatBuilder(
        IEnumerable<PineValue> values)
    {
        _values.AddRange(values);
    }

    /// <summary>
    /// Creates a new builder from the provided initial <paramref name="values"/>.
    /// </summary>
    /// <param name="values">The initial sequence of values to seed the builder with.</param>
    /// <returns>A new <see cref="MutatingConcatBuilder"/> instance.</returns>
    public static MutatingConcatBuilder Create(
        IEnumerable<PineValue> values)
    {
        return new MutatingConcatBuilder(values);
    }

    /// <summary>
    /// Appends a single <paramref name="value"/> to the end of the builder.
    /// </summary>
    /// <param name="value">The value to append.</param>
    public void AppendItem(PineValue value)
    {
        _values.Add(value);
    }

    /// <summary>
    /// Appends the sequence of <paramref name="values"/> to the end of the builder.
    /// </summary>
    /// <param name="values">The values to append.</param>
    public void AppendItems(IEnumerable<PineValue> values)
    {
        _values.AddRange(values);
    }

    /// <summary>
    /// Inserts the specified item at the beginning of the collection.
    /// </summary>
    public void PrependItem(PineValue value)
    {
        _values.Insert(0, value);
    }

    /// <summary>
    /// Inserts the specified collection of values at the beginning of the current list.
    /// </summary>
    public void PrependItems(IEnumerable<PineValue> values)
    {
        _values.InsertRange(0, values);
    }

    /// <summary>
    /// Evaluates this builder into a single <see cref="PineValue"/> by applying all <see cref="KernelFunction.concat"/> operations."/>
    /// </summary>
    public PineValue Evaluate()
    {
        return Internal.KernelFunctionSpecialized.concat(_values.ToArray());
    }

    /// <inheritdoc/>
    public override bool Equals(object? obj)
    {
        throw new System.InvalidOperationException(
            "Equals should not be called directly, instead explicitly evaluate first.");
    }

    /// <inheritdoc/>
    public override int GetHashCode()
    {
        throw new System.InvalidOperationException(
            "GetHashCode should not be called directly, instead explicitly evaluate first.");
    }
}
