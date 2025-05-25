using System;
using System.Text.Json.Serialization;

namespace Pine.Core;

/// <summary>
/// Generic choice type to describe the outcome of an operation with an overall classification into either failure ('Err') or success ('Ok').
/// </summary>
[JsonConverter(typeof(Json.JsonConverterForChoiceType))]
public abstract record Result<ErrT, OkT>
{
    /// <summary>
    /// Explicit constructor for the 'Err' case.
    /// </summary>
    public static Result<ErrT, OkT> err(ErrT err) => new Err(err);

    /// <summary>
    /// Explicit constructor for the 'Ok' case.
    /// </summary>
    public static Result<ErrT, OkT> ok(OkT ok) => new Ok(ok);

    /// <summary>
    /// Implicit constructor for the 'Err' case.
    /// </summary>
    public static implicit operator Result<ErrT, OkT>(ErrT err) =>
        Result<ErrT, OkT>.err(err);

    /// <summary>
    /// Implicit constructor for the 'Ok' case.
    /// </summary>
    public static implicit operator Result<ErrT, OkT>(OkT ok) =>
        Result<ErrT, OkT>.ok(ok);

    /// <summary>
    /// Returns whether this value represents an error case.
    /// </summary>
    public bool IsErr() =>
        this switch
        {
            Err _ => true,
            _ => false
        };

    /// <summary>
    /// Returns whether this value represents a success case.
    /// </summary>
    public bool IsOk() =>
        this switch
        {
            Ok _ => true,
            _ => false
        };

    /// <summary>
    /// A <see cref="Result{ErrT, OkT}"/> that is an error.
    /// </summary>
    public record Err(ErrT Value) : Result<ErrT, OkT>;

    /// <summary>
    /// A <see cref="Result{ErrT, OkT}"/> that is a success.
    /// </summary>
    public record Ok(OkT Value) : Result<ErrT, OkT>;

    /// <summary>
    /// Maps the value of the 'Ok' case.
    /// </summary>
    public Result<ErrT, MappedOkT> Map<MappedOkT>(Func<OkT, MappedOkT> okMap) =>
        this switch
        {
            Ok ok =>
            new Result<ErrT, MappedOkT>.Ok(okMap(ok.Value)),

            Err err =>
            new Result<ErrT, MappedOkT>.Err(err.Value),

            _ => throw new NotImplementedException()
        };

    /// <summary>
    /// Maps the value of the 'Err' case.
    /// </summary>
    public Result<MappedErrT, OkT> MapError<MappedErrT>(Func<ErrT, MappedErrT> errMap) =>
        this switch
        {
            Ok ok => new Result<MappedErrT, OkT>.Ok(ok.Value),
            Err err => new Result<MappedErrT, OkT>.Err(errMap(err.Value)),
            _ => throw new NotImplementedException()
        };

    /// <summary>
    /// Map for the 'Ok' case that may fail.
    /// Used for chaining together a sequence of computations that may fail.
    /// </summary>
    public Result<ErrT, MappedOkT> AndThen<MappedOkT>(Func<OkT, Result<ErrT, MappedOkT>> okMap) =>
        this switch
        {
            Ok ok =>
            okMap(ok.Value),

            Err err =>
            new Result<ErrT, MappedOkT>.Err(err.Value),

            _ =>
            throw new NotImplementedException()
        };

    /// <summary>
    /// If the result is <typeparamref name="OkT"/> return the value,
    /// but if the result is an <see cref="Err"/> then return <paramref name="defaultIfErr"/>.
    /// </summary>
    public OkT WithDefault(OkT defaultIfErr) =>
        this switch
        {
            Ok ok =>
            ok.Value,

            Err =>
            defaultIfErr,

            _ =>
            throw new NotImplementedException()
        };

    /// <summary>
    /// If the result is Ok return the value,
    /// but if the result is an Err then invoke <paramref name="getDefault"/> to get a default value.
    /// </summary>
    public OkT WithDefaultBuilder(Func<OkT> getDefault) =>
        this switch
        {
            Ok ok =>
            ok.Value,

            Err =>
            getDefault(),

            _ =>
            throw new NotImplementedException()
        };

    /// <summary>
    /// Convert a <see cref="Result{ErrT, OkT}"/> into an <typeparamref name="OkT"/>
    /// by applying the conversion function <paramref name="fromErr"/> if the <see cref="Result{ErrT, OkT}"/> is an <see cref="Err"/>.
    /// </summary>
    public OkT Extract(Func<ErrT, OkT> fromErr) =>
        this switch
        {
            Ok ok =>
            ok.Value,

            Err err =>
            fromErr(err.Value),

            _ =>
            throw new NotImplementedException()
        };

    /// <summary>
    /// Converts a <see cref="Result{ErrT, OkT}"/> to a value of type <typeparamref name="T"/>,
    /// by applying either the first function if the <see cref="Result{ErrT, OkT}"/> is an <see cref="Err"/>,
    /// or the second function if the <see cref="Result{ErrT, OkT}"/> is an <see cref="Ok"/>.
    /// Both of these functions must return the same type.
    /// </summary>
    public T Unpack<T>(Func<ErrT, T> fromErr, Func<OkT, T> fromOk) =>
        this switch
        {
            Ok ok =>
            fromOk(ok.Value),

            Err err =>
            fromErr(err.Value),

            _ =>
            throw new NotImplementedException()
        };
}
