using System;
using System.Collections.Generic;
using System.Collections.Immutable;

// Alias to avoid ambiguity with System.Range
using Range = Pine.Core.Elm.ElmSyntax.SyntaxModel.Range;

using SyntaxTypes = Pine.Core.Elm.ElmSyntax.Stil4mElmSyntax7;
using ModuleName = System.Collections.Generic.IReadOnlyList<string>;

namespace Pine.Core.Elm.ElmCompilerInDotnet;

/// <summary>
/// Represents an error encountered during canonicalization, such as an undefined reference.
/// </summary>
/// <param name="Range">The source location where the error occurred.</param>
/// <param name="ReferencedName">The name that was referenced but could not be resolved.</param>
public record CanonicalizationError(
    Range Range,
    string ReferencedName);

/// <summary>
/// Describes the location of a shadowing declaration within a module.
/// </summary>
/// <param name="Range">The source range of the shadowing declaration.</param>
/// <param name="DeclarationPath">
/// The path containing the shadowing declaration within the module.
/// An empty list means the shadowing declaration is a module-level declaration.
/// A list with one item contains the name of the containing module-level declaration.
/// </param>
public record ShadowingLocation(
    Range Range,
    ImmutableList<string> DeclarationPath)
{
    /// <summary>
    /// Determines whether the specified <see cref="ShadowingLocation"/> is equal to this instance,
    /// comparing <see cref="DeclarationPath"/> by sequence rather than by reference.
    /// </summary>
    public virtual bool Equals(ShadowingLocation? other)
    {
        if (ReferenceEquals(this, other))
            return true;

        if (other is null)
            return false;

        return
            Range.Equals(other.Range) &&
            System.Linq.Enumerable.SequenceEqual(DeclarationPath, other.DeclarationPath);
    }

    /// <summary>
    /// Returns a hash code computed from <see cref="Range"/> and all elements
    /// in <see cref="DeclarationPath"/> to support value equality semantics.
    /// </summary>
    public override int GetHashCode()
    {
        var hashCode = new HashCode();

        hashCode.Add(Range);

        foreach (var item in DeclarationPath)
            hashCode.Add(item);

        return hashCode.ToHashCode();
    }
}

/// <summary>
/// The result of canonicalizing a single module, containing the canonicalized file, any errors,
/// and all detected shadowings.
/// </summary>
/// <param name="File">The canonicalized module file.</param>
/// <param name="Errors">List of errors encountered during canonicalization of this module.</param>
/// <param name="Shadowings">
/// Dictionary of all detected shadowings, keyed by the shadowed name.
/// Each value describes the location of the shadowing declaration.
/// </param>
public record ModuleCanonicalizationResult(
    SyntaxTypes.File File,
    IReadOnlyList<CanonicalizationError> Errors,
    ImmutableDictionary<string, ShadowingLocation> Shadowings);

/// <summary>
/// The result of canonicalizing multiple modules.
/// </summary>
/// <param name="Modules">Dictionary mapping module names to their canonicalization results.</param>
public record CanonicalizationResultWithErrors(
    IReadOnlyDictionary<ModuleName, ModuleCanonicalizationResult> Modules);

/// <summary>
/// Wraps a canonicalization result value along with any errors and shadowings encountered during canonicalization.
/// Enables error and shadowing aggregation throughout the canonicalization process.
/// </summary>
/// <typeparam name="T">The type of the canonicalized value.</typeparam>
/// <param name="Value">The canonicalized value.</param>
/// <param name="Errors">List of errors encountered during canonicalization of this value and its children.</param>
/// <param name="Shadowings">Dictionary of detected shadowings, keyed by the shadowed name.</param>
public record CanonicalizationResult<T>(
    T Value,
    IReadOnlyList<CanonicalizationError> Errors,
    ImmutableDictionary<string, ShadowingLocation> Shadowings)
{
    /// <summary>
    /// Creates a result with no shadowings. Used by sites that do not produce shadowing information.
    /// </summary>
    public CanonicalizationResult(T value, IReadOnlyList<CanonicalizationError> errors)
        : this(value, errors, [])
    {
    }

    /// <summary>
    /// Enables deconstruction into (Value, Errors) for backward compatibility with code
    /// that only needs the value and errors.
    /// </summary>
    public void Deconstruct(out T value, out IReadOnlyList<CanonicalizationError> errors)
    {
        value = Value;
        errors = Errors;
    }

    /// <summary>
    /// Transforms the value while preserving the errors and shadowings.
    /// </summary>
    /// <typeparam name="TResult">The type of the transformed value.</typeparam>
    /// <param name="mapper">Function to transform the value.</param>
    /// <returns>A new result with the transformed value and the same errors and shadowings.</returns>
    public CanonicalizationResult<TResult> MapValue<TResult>(Func<T, TResult> mapper)
    {
        return new CanonicalizationResult<TResult>(mapper(Value), Errors, Shadowings);
    }

    /// <summary>
    /// Aggregates multiple canonicalization results into a single result, combining all errors and shadowings.
    /// </summary>
    /// <typeparam name="AggregateT">The type of the aggregated value.</typeparam>
    /// <param name="combine">Function to combine the individual values into an aggregate.</param>
    /// <param name="itemsResults">The results to aggregate.</param>
    /// <returns>A result containing the combined value and all aggregated errors and shadowings.</returns>
    public static CanonicalizationResult<AggregateT> Aggregate<AggregateT>(
        Func<IReadOnlyList<T>, AggregateT> combine,
        IEnumerable<CanonicalizationResult<T>> itemsResults)
    {
        var errors = new List<CanonicalizationError>();
        var values = new List<T>();
        var shadowings = ImmutableDictionary<string, ShadowingLocation>.Empty;

        foreach (var itemResult in itemsResults)
        {
            values.Add(itemResult.Value);
            errors.AddRange(itemResult.Errors);
            shadowings = MergeShadowings(shadowings, itemResult.Shadowings);
        }

        var aggregatedValue = combine(values);

        return new CanonicalizationResult<AggregateT>(aggregatedValue, errors, shadowings);
    }

    /// <summary>
    /// Merges two shadowing dictionaries. The first dictionary's entries take precedence
    /// when the same name appears in both.
    /// </summary>
    public static ImmutableDictionary<string, ShadowingLocation> MergeShadowings(
        ImmutableDictionary<string, ShadowingLocation> first,
        ImmutableDictionary<string, ShadowingLocation> second)
    {
        if (second.IsEmpty)
            return first;

        if (first.IsEmpty)
            return second;

        var result = first;

        foreach (var kvp in second)
        {
            if (!result.ContainsKey(kvp.Key))
            {
                result = result.Add(kvp.Key, kvp.Value);
            }
        }

        return result;
    }
}
