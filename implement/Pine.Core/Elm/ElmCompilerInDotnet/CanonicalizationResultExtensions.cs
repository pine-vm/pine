using System;
using System.Collections.Generic;
using System.Linq;

namespace Pine.Core.Elm.ElmCompilerInDotnet;

/// <summary>
/// Provides extension methods for aggregating and combining canonicalization results, enabling the merging of values
/// and errors from multiple sources.
/// </summary>
/// <remarks>These methods facilitate the composition of canonicalization operations by allowing results to be
/// combined while preserving all associated errors. This is useful when validating or transforming multiple inputs and
/// collecting all issues encountered. The class is intended for use with the CanonicalizationResult type to streamline
/// error handling in multi-step or multi-input scenarios.</remarks>
public class CanonicalizationResultExtensions
{
    /// <summary>
    /// Aggregates multiple canonicalization results into a single result with combined errors.
    /// </summary>
    /// <typeparam name="TValue">The type of values being aggregated.</typeparam>
    /// <param name="results">The results to aggregate.</param>
    /// <returns>A result containing all values and all combined errors.</returns>
    public static CanonicalizationResult<IReadOnlyList<TValue>> Concat<TValue>(
        IEnumerable<CanonicalizationResult<TValue>> results)
    {
        var values = new List<TValue>();
        var errors = new List<CanonicalizationError>();

        foreach (var item in results)
        {
            values.Add(item.Value);
            errors.AddRange(item.Errors);
        }

        return new CanonicalizationResult<IReadOnlyList<TValue>>(values, errors);
    }

    /// <summary>
    /// Projects each element of the input sequence using the specified function and aggregates the results into a
    /// single canonicalization result containing all projected values and their associated errors.
    /// </summary>
    /// <remarks>The returned canonicalization result aggregates the values and errors from each individual
    /// projection. The order of the output values matches the order of the input sequence. If no errors are
    /// encountered, the Errors collection in the result will be empty.</remarks>
    /// <typeparam name="ItemIn">The type of elements in the input sequence.</typeparam>
    /// <typeparam name="ItemOut">The type of elements produced by the projector function.</typeparam>
    /// <param name="items">The sequence of input elements to be projected. Cannot be null.</param>
    /// <param name="projector">A function that transforms each input element into a canonicalization result. Cannot be null.</param>
    /// <returns>A canonicalization result containing a read-only list of projected values and a combined list of all errors
    /// encountered during projection.</returns>
    public static CanonicalizationResult<IReadOnlyList<ItemOut>> ConcatMap<ItemIn, ItemOut>(
        IEnumerable<ItemIn> items,
        Func<ItemIn, CanonicalizationResult<ItemOut>> projector)
    {
        var values = new List<ItemOut>();

        var errors = new List<CanonicalizationError>();

        foreach (var item in items)
        {
            var itemResult = projector(item);

            values.Add(itemResult.Value);
            errors.AddRange(itemResult.Errors);
        }

        return new CanonicalizationResult<IReadOnlyList<ItemOut>>(values, errors);
    }

    /// <summary>
    /// Combines two canonicalization results using a combiner function.
    /// </summary>
    /// <typeparam name="T1">The type of the first result.</typeparam>
    /// <typeparam name="T2">The type of the second result.</typeparam>
    /// <typeparam name="TResult">The type of the combined result.</typeparam>
    /// <param name="result1">The first result.</param>
    /// <param name="result2">The second result.</param>
    /// <param name="combiner">Function to combine the two values.</param>
    /// <returns>A result with the combined value and merged errors from both inputs.</returns>
    public static CanonicalizationResult<TResult> Map2<T1, T2, TResult>(
        CanonicalizationResult<T1> result1,
        CanonicalizationResult<T2> result2,
        Func<T1, T2, TResult> combiner)
    {
        var combinedValue =
            combiner(result1.Value, result2.Value);

        var combinedErrors =
            result1.Errors.Concat(result2.Errors).ToList();

        return new CanonicalizationResult<TResult>(combinedValue, combinedErrors);
    }

    /// <summary>
    /// Combines three canonicalization results using a combiner function.
    /// </summary>
    /// <typeparam name="T1">The type of the first result.</typeparam>
    /// <typeparam name="T2">The type of the second result.</typeparam>
    /// <typeparam name="T3">The type of the third result.</typeparam>
    /// <typeparam name="TResult">The type of the combined result.</typeparam>
    /// <param name="result1">The first result.</param>
    /// <param name="result2">The second result.</param>
    /// <param name="result3">The third result.</param>
    /// <param name="combiner">Function to combine the three values.</param>
    /// <returns>A result with the combined value and merged errors from all inputs.</returns>
    public static CanonicalizationResult<TResult> Map3<T1, T2, T3, TResult>(
        CanonicalizationResult<T1> result1,
        CanonicalizationResult<T2> result2,
        CanonicalizationResult<T3> result3,
        Func<T1, T2, T3, TResult> combiner)
    {
        var combinedValue =
            combiner(result1.Value, result2.Value, result3.Value);

        var combinedErrors =
            result1.Errors
            .Concat(result2.Errors)
            .Concat(result3.Errors)
            .ToList();

        return new CanonicalizationResult<TResult>(combinedValue, combinedErrors);
    }
}
