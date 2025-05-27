using System;
using System.Collections.Generic;
using System.Collections.Immutable;

namespace Pine.Core;

/// <summary>
/// Extension methods for the <see cref="Result{ErrT, OkT}"/> type.
/// </summary>
public static class ResultExtension
{
    /// <summary>
    /// Converts a <see cref="Result{ErrT, OkT}"/> to a <see cref="Maybe{OkT}"/>.
    /// If the result is an <see cref="Result{ErrT, OkT}.Err"/>, it returns <see cref="Maybe{OkT}.nothing()"/>.
    /// If the result is an <see cref="Result{ErrT, OkT}.Ok"/>, it returns <see cref="Maybe{OkT}.just(OkT)"/> with the Ok value.
    /// </summary>
    /// <typeparam name="ErrT">The type of the error value.</typeparam>
    /// <typeparam name="OkT">The type of the success value.</typeparam>
    /// <param name="result">The result to convert.</param>
    /// <returns>A <see cref="Maybe{OkT}"/> representing the Ok value if present, otherwise Nothing.</returns>
    public static Maybe<OkT> ToMaybe<ErrT, OkT>(this Result<ErrT, OkT> result) =>
        result.Unpack(
            fromErr: _ => Maybe<OkT>.nothing(),
            fromOk: Maybe<OkT>.just);

    /// <summary>
    /// Checks if the result is an <see cref="Result{ErrT, OkT}.Err"/> and returns its value, or null if it's an <see cref="Result{ErrT, OkT}.Ok"/>.
    /// This method is for reference types.
    /// </summary>
    /// <typeparam name="ErrT">The type of the error value, constrained to be a class.</typeparam>
    /// <typeparam name="OkT">The type of the success value.</typeparam>
    /// <param name="result">The result to check.</param>
    /// <returns>The error value if the result is an error, otherwise null.</returns>
    public static ErrT? IsErrOrNull<ErrT, OkT>(this Result<ErrT, OkT> result)
        where ErrT : class
    {
        if (result is Result<ErrT, OkT>.Err error)
            return error.Value;

        return null;
    }

    /// <summary>
    /// Checks if the result is an <see cref="Result{ErrT, OkT}.Err"/> and returns its value, or null if it's an <see cref="Result{ErrT, OkT}.Ok"/>.
    /// This method is for value types.
    /// </summary>
    /// <typeparam name="ErrT">The type of the error value, constrained to be a struct.</typeparam>
    /// <typeparam name="OkT">The type of the success value.</typeparam>
    /// <param name="result">The result to check.</param>
    /// <returns>The error value if the result is an error, otherwise null.</returns>
    public static ErrT? IsErrOrNullable<ErrT, OkT>(this Result<ErrT, OkT> result)
        where ErrT : struct
    {
        if (result is Result<ErrT, OkT>.Err error)
            return error.Value;

        return null;
    }

    /// <summary>
    /// Checks if the result is an <see cref="Result{ErrT, OkT}.Ok"/> and returns its value, or null if it's an <see cref="Result{ErrT, OkT}.Err"/>.
    /// This method is for reference types.
    /// </summary>
    /// <typeparam name="ErrT">The type of the error value.</typeparam>
    /// <typeparam name="OkT">The type of the success value, constrained to be a class.</typeparam>
    /// <param name="result">The result to check.</param>
    /// <returns>The success value if the result is a success, otherwise null.</returns>
    public static OkT? IsOkOrNull<ErrT, OkT>(this Result<ErrT, OkT> result)
        where OkT : class
    {
        if (result is Result<ErrT, OkT>.Ok ok)
            return ok.Value;

        return null;
    }

    /// <summary>
    /// Checks if the result is an <see cref="Result{ErrT, OkT}.Ok"/> and returns its value, or null if it's an <see cref="Result{ErrT, OkT}.Err"/>.
    /// This method is for value types.
    /// </summary>
    /// <typeparam name="ErrT">The type of the error value.</typeparam>
    /// <typeparam name="OkT">The type of the success value, constrained to be a struct.</typeparam>
    /// <param name="result">The result to check.</param>
    /// <returns>The success value if the result is a success, otherwise null.</returns>
    public static OkT? IsOkOrNullable<ErrT, OkT>(this Result<ErrT, OkT> result)
        where OkT : struct
    {
        if (result is Result<ErrT, OkT>.Ok ok)
            return ok.Value;

        return null;
    }

    /// <summary>
    /// Combines a list of <see cref="Result{ErrT, OkT}"/> into a single Result&lt;ErrT, IReadOnlyList&lt;OkT&gt;&gt;.
    /// If any result in the list is an error, it returns the first error encountered.
    /// Otherwise, it returns a list of all the success values.
    /// </summary>
    /// <typeparam name="ErrT">The type of the error value.</typeparam>
    /// <typeparam name="OkT">The type of the success value.</typeparam>
    /// <param name="list">The list of results to combine.</param>
    /// <returns>A combined result.</returns>
    public static Result<ErrT, IReadOnlyList<OkT>> ListCombine<ErrT, OkT>(this IEnumerable<Result<ErrT, OkT>> list)
    {
        var okList = new List<OkT>();

        foreach (var item in list)
        {
            if (item is Result<ErrT, OkT>.Err error)
                return new Result<ErrT, IReadOnlyList<OkT>>.Err(error.Value);

            if (item is not Result<ErrT, OkT>.Ok ok)
                throw new NotImplementedException();

            okList.Add(ok.Value);
        }

        return new Result<ErrT, IReadOnlyList<OkT>>.Ok(okList);
    }

    /// <summary>
    /// Tries to get the first successful result from a sequence of result-producing functions.
    /// If all functions return errors, it aggregates all the errors into a single result.
    /// </summary>
    /// <typeparam name="ErrT">The type of the error value.</typeparam>
    /// <typeparam name="OkT">The type of the success value.</typeparam>
    /// <param name="source">The sequence of result-producing functions.</param>
    /// <returns>A result containing either the first success or all errors.</returns>
    public static Result<IImmutableList<ErrT>, OkT> FirstOkOrAllErrors<ErrT, OkT>(
        this IEnumerable<Func<Result<ErrT, OkT>>> source)
    {
        var errors = ImmutableList<ErrT>.Empty;

        foreach (var candidateFunc in source)
        {
            var candidateResult = candidateFunc();

            if (candidateResult is Result<ErrT, OkT>.Ok okResult)
            {
                return Result<IImmutableList<ErrT>, OkT>.ok(okResult.Value);
            }

            if (candidateResult is Result<ErrT, OkT>.Err errResult)
            {
                errors = errors.Add(errResult.Value);
            }

            throw new NotImplementedException(
                $"Unexpected result type: {candidateResult.GetType()}");
        }

        return Result<IImmutableList<ErrT>, OkT>.err(errors);
    }

    /// <summary>
    /// Aggregates a sequence of items using a specified aggregation function, exiting on the first error encountered.
    /// </summary>
    /// <typeparam name="ItemT">The type of the items in the sequence.</typeparam>
    /// <typeparam name="ErrT">The type of the error value.</typeparam>
    /// <typeparam name="OkT">The type of the success value.</typeparam>
    /// <param name="sequence">The sequence of items to aggregate.</param>
    /// <param name="aggregateFunc">The aggregation function.</param>
    /// <param name="aggregateSeed">The initial value for the aggregation.</param>
    /// <returns>A result containing either the aggregated value or the first error encountered.</returns>
    public static Result<ErrT, OkT> AggregateExitingOnFirstError<ItemT, ErrT, OkT>(
        IEnumerable<ItemT> sequence,
        Func<OkT, ItemT, Result<ErrT, OkT>> aggregateFunc,
        OkT aggregateSeed)
    {
        var currentResult = Result<ErrT, OkT>.ok(aggregateSeed);

        foreach (var item in sequence)
        {
            if (currentResult is Result<ErrT, OkT>.Err err)
            {
                return err;
            }

            // currentResult is Ok at this point.
            var okValue = ((Result<ErrT, OkT>.Ok)currentResult).Value;

            currentResult = aggregateFunc(okValue, item);
        }

        return currentResult;
    }
}
