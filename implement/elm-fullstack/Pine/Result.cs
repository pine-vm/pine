using System;

namespace Pine;

/// <summary>
/// Generic DU type to describe the outcome of an operation with an overall classification into either failure ('Err') or success ('Ok').
/// Supports only nullable types (class or Nullable<T>) as type arguments.
/// </summary>
public record Result<ErrT, OkT>
{
    /// <summary>
    /// A non-null value represents an error.
    /// </summary>
    public ErrT? Err { private init; get; }

    /// <summary>
    /// A non-null value represents a success.
    /// </summary>
    public OkT? Ok { private init; get; }

    /// <summary>
    /// Constructor for the 'Err' case.
    /// </summary>
    static public Result<ErrT, OkT> err(ErrT err) =>
        new(Err: err);

    /// <summary>
    /// Constructor for the 'Ok' case.
    /// </summary>
    static public Result<ErrT, OkT> ok(OkT ok) =>
        new(Ok: ok);

    /// <summary>
    /// Maps the value of the 'Ok' case.
    /// </summary>
    public Result<ErrT, MappedOkT> map<MappedOkT>(Func<OkT, MappedOkT> okMap)
    {
        if (Ok == null)
            return Result<ErrT, MappedOkT>.err(Err!);

        return Result<ErrT, MappedOkT>.ok(okMap(Ok));
    }

    /// <summary>
    /// Maps the value of the 'Err' case.
    /// </summary>
    public Result<MappedErrT, OkT> mapError<MappedErrT>(Func<ErrT, MappedErrT> errMap)
    {
        if (Ok == null)
            return Result<MappedErrT, OkT>.err(errMap(Err!));

        return Result<MappedErrT, OkT>.ok(Ok);
    }

    /// <summary>
    /// Map for the 'Ok' case that may fail.
    /// Used for chaining together a sequence of computations that may fail.
    /// </summary>
    public Result<ErrT, MappedOkT> andThen<MappedOkT>(Func<OkT, Result<ErrT, MappedOkT>> okMap)
    {
        if (Ok == null)
            return Result<ErrT, MappedOkT>.err(Err!);

        return okMap(Ok);
    }

    /// <summary>
    /// Prevent construction of invalid values: Make the default constructor private to force using the explicit construction methods.
    /// </summary>
    protected Result(ErrT? Err = default, OkT? Ok = default)
    {
        if (!(default(ErrT) == null))
            throw new InvalidOperationException(
              "Only use types with a default value of null for the type argument " + nameof(ErrT) +
              ". For value types, use Nullable<T> for wrapping.");

        if (!(default(OkT) == null))
            throw new InvalidOperationException(
              "Only use types with a default value of null for the type argument " + nameof(OkT) +
              ". For value types, use Nullable<T> for wrapping.");

        if (Err != null && Ok != null)
            throw new ArgumentException("Both Err and Ok are not null.");

        if (Err == null && Ok == null)
            throw new ArgumentException("Both Err and Ok are null.");

        this.Err = Err;
        this.Ok = Ok;
    }
}
