using System;
using System.Collections.Generic;

using SyntaxTypes = Pine.Core.Elm.ElmSyntax.Stil4mElmSyntax7;

namespace Pine.Core.Elm.ElmCompilerInDotnet;

/// <summary>
/// Represents an error encountered during canonicalization, such as an undefined reference.
/// </summary>
/// <param name="Range">The source location where the error occurred.</param>
/// <param name="ReferencedName">The name that was referenced but could not be resolved.</param>
public record CanonicalizationError(
    SyntaxTypes.Range Range,
    string ReferencedName);

/// <summary>
/// Wraps a canonicalization result value along with any errors encountered during canonicalization.
/// Enables error aggregation throughout the canonicalization process.
/// </summary>
/// <typeparam name="T">The type of the canonicalized value.</typeparam>
/// <param name="Value">The canonicalized value.</param>
/// <param name="Errors">List of errors encountered during canonicalization of this value and its children.</param>
public record CanonicalizationResult<T>(
    T Value,
    IReadOnlyList<CanonicalizationError> Errors)
{
    /// <summary>
    /// Transforms the value while preserving the errors.
    /// </summary>
    /// <typeparam name="TResult">The type of the transformed value.</typeparam>
    /// <param name="mapper">Function to transform the value.</param>
    /// <returns>A new result with the transformed value and the same errors.</returns>
    public CanonicalizationResult<TResult> MapValue<TResult>(Func<T, TResult> mapper)
    {
        return new CanonicalizationResult<TResult>(mapper(Value), Errors);
    }

    /// <summary>
    /// Aggregates multiple canonicalization results into a single result, combining all errors.
    /// </summary>
    /// <typeparam name="AggregateT">The type of the aggregated value.</typeparam>
    /// <param name="combine">Function to combine the individual values into an aggregate.</param>
    /// <param name="itemsResults">The results to aggregate.</param>
    /// <returns>A result containing the combined value and all aggregated errors.</returns>
    public static CanonicalizationResult<AggregateT> Aggregate<AggregateT>(
        Func<IReadOnlyList<T>, AggregateT> combine,
        IEnumerable<CanonicalizationResult<T>> itemsResults)
    {
        var errors = new List<CanonicalizationError>();
        var values = new List<T>();

        foreach (var itemResult in itemsResults)
        {
            values.Add(itemResult.Value);
            errors.AddRange(itemResult.Errors);
        }

        var aggregatedValue = combine(values);

        return new CanonicalizationResult<AggregateT>(aggregatedValue, errors);
    }
}
