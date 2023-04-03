using System;
using System.Text.Json.Serialization;

namespace Pine;

[JsonConverter(typeof(Json.JsonConverterForChoiceType))]
/// <summary>
/// Generic choice type to describe the outcome of an operation with an overall classification into either failure ('Err') or success ('Ok').
/// </summary>
public abstract record Result<ErrT, OkT>
{
    /// <summary>
    /// Constructor for the 'Err' case.
    /// </summary>
    public static Result<ErrT, OkT> err(ErrT err) => new Err(err);

    /// <summary>
    /// Constructor for the 'Ok' case.
    /// </summary>
    public static Result<ErrT, OkT> ok(OkT ok) => new Ok(ok);

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
            Ok ok => new Result<ErrT, MappedOkT>.Ok(okMap(ok.Value)),
            Err err => new Result<ErrT, MappedOkT>.Err(err.Value),
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
            Ok ok => okMap(ok.Value),
            Err err => new Result<ErrT, MappedOkT>.Err(err.Value),
            _ => throw new NotImplementedException()
        };

    public OkT WithDefault(OkT defaultIfErr) =>
        Unpack(fromErr: _ => defaultIfErr, fromOk: ok => ok);

    public OkT WithDefaultBuilder(Func<OkT> getDefault) =>
        Unpack(fromErr: _ => getDefault(), fromOk: ok => ok);

    public OkT Extract(Func<ErrT, OkT> fromErr) =>
        Unpack(fromErr: fromErr, fromOk: ok => ok);

    public T Unpack<T>(Func<ErrT, T> fromErr, Func<OkT, T> fromOk) =>
        this switch
        {
            Ok ok => fromOk(ok.Value),
            Err err => fromErr(err.Value),
            _ => throw new NotImplementedException()
        };
}
