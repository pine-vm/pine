using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

namespace Pine.Core.Elm.ElmCompilerInDotnet;

/// <summary>
/// Provides extension methods for aggregating and combining canonicalization results, enabling the merging of values,
/// errors, and shadowings from multiple sources.
/// </summary>
/// <remarks>These methods facilitate the composition of canonicalization operations by allowing results to be
/// combined while preserving all associated errors and shadowings. This is useful when validating or transforming
/// multiple inputs and collecting all issues encountered. The class is intended for use with the
/// CanonicalizationResult type to streamline error handling in multi-step or multi-input scenarios.</remarks>
public class CanonicalizationResultExtensions
{
    /// <summary>
    /// Aggregates multiple canonicalization results into a single result with combined errors and shadowings.
    /// </summary>
    /// <typeparam name="TValue">The type of values being aggregated.</typeparam>
    /// <param name="results">The results to aggregate.</param>
    /// <returns>A result containing all values and all combined errors and shadowings.</returns>
    public static CanonicalizationResult<IReadOnlyList<TValue>> Concat<TValue>(
        IEnumerable<CanonicalizationResult<TValue>> results)
    {
        var values = new List<TValue>();
        var errors = new List<CanonicalizationError>();
        var shadowings = ImmutableDictionary<string, ShadowingLocation>.Empty;

        foreach (var item in results)
        {
            values.Add(item.Value);
            errors.AddRange(item.Errors);
            shadowings = CanonicalizationResult<TValue>.MergeShadowings(shadowings, item.Shadowings);
        }

        return new CanonicalizationResult<IReadOnlyList<TValue>>(values, errors, shadowings);
    }

    /// <summary>
    /// Projects each element of the input sequence using the specified function and aggregates the results into a
    /// single canonicalization result containing all projected values and their associated errors and shadowings.
    /// </summary>
    /// <remarks>The returned canonicalization result aggregates the values, errors, and shadowings from each individual
    /// projection. The order of the output values matches the order of the input sequence. If no errors are
    /// encountered, the Errors collection in the result will be empty.</remarks>
    public static CanonicalizationResult<IReadOnlyList<ItemOut>> ConcatMap<ItemIn, ItemOut>(
        IEnumerable<ItemIn> items,
        Func<ItemIn, CanonicalizationResult<ItemOut>> projector)
    {
        var values = new List<ItemOut>();
        var errors = new List<CanonicalizationError>();
        var shadowings = ImmutableDictionary<string, ShadowingLocation>.Empty;

        foreach (var item in items)
        {
            var itemResult = projector(item);

            values.Add(itemResult.Value);
            errors.AddRange(itemResult.Errors);
            shadowings = CanonicalizationResult<ItemOut>.MergeShadowings(shadowings, itemResult.Shadowings);
        }

        return new CanonicalizationResult<IReadOnlyList<ItemOut>>(values, errors, shadowings);
    }

    /// <summary>
    /// Combines two canonicalization results using a combiner function.
    /// </summary>
    public static CanonicalizationResult<TResult> Map2<T1, T2, TResult>(
        CanonicalizationResult<T1> result1,
        CanonicalizationResult<T2> result2,
        Func<T1, T2, TResult> combiner)
    {
        var combinedValue =
            combiner(result1.Value, result2.Value);

        var combinedErrors =
            result1.Errors.Concat(result2.Errors).ToList();

        var combinedShadowings =
            CanonicalizationResult<T1>.MergeShadowings(result1.Shadowings, result2.Shadowings);

        return new CanonicalizationResult<TResult>(combinedValue, combinedErrors, combinedShadowings);
    }

    /// <summary>
    /// Combines three canonicalization results using a combiner function.
    /// </summary>
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

        var combinedShadowings =
            CanonicalizationResult<T1>.MergeShadowings(
                CanonicalizationResult<T1>.MergeShadowings(result1.Shadowings, result2.Shadowings),
                result3.Shadowings);

        return new CanonicalizationResult<TResult>(combinedValue, combinedErrors, combinedShadowings);
    }
}
