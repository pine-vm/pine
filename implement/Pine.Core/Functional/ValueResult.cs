using System;

namespace Pine.Core.Functional;

/// <summary>
/// A success value or an error, stored without allocating a result wrapper.
/// The default instance is uninitialized; inspecting it throws <see cref="InvalidOperationException"/>.
/// </summary>
/// <typeparam name="ErrT">The error payload type.</typeparam>
/// <typeparam name="OkT">The success payload type.</typeparam>
public readonly struct ValueResult<ErrT, OkT>
{
    private readonly ResultState _state;

    private readonly ErrT _error;

    private readonly OkT _value;

    private ValueResult(ResultState state, ErrT error, OkT value)
    {
        _state = state;
        _error = error;
        _value = value;
    }

    /// <summary>Whether this result contains a success value.</summary>
    public bool IsOk =>
        _state switch
        {
            ResultState.Ok => true,
            ResultState.Err => false,

            _ =>
            throw new InvalidOperationException("ValueResult is uninitialized.")
        };

    /// <summary>Whether this result contains an error.</summary>
    public bool IsErr => !IsOk;

    /// <summary>Gets the success payload, throwing if this is not a success.</summary>
    public OkT Value =>
        IsOk
        ?
        _value
        :
        throw new InvalidOperationException("ValueResult does not contain a success value.");

    /// <summary>Gets the error payload, throwing if this is not an error.</summary>
    public ErrT Error =>
        IsErr
        ?
        _error
        :
        throw new InvalidOperationException("ValueResult does not contain an error.");

#pragma warning disable IDE1006
    /// <summary>Constructs an error result without allocating a wrapper.</summary>
    public static ValueResult<ErrT, OkT> err(ErrT error) =>
        new(ResultState.Err, error, default!);

    /// <summary>Constructs a success result without allocating a wrapper.</summary>
    public static ValueResult<ErrT, OkT> ok(OkT value) =>
        new(ResultState.Ok, default!, value);
#pragma warning restore IDE1006

    /// <summary>Constructs an error result from its payload.</summary>
    public static implicit operator ValueResult<ErrT, OkT>(ErrT error) => err(error);

    /// <summary>Constructs a success result from its payload.</summary>
    public static implicit operator ValueResult<ErrT, OkT>(OkT value) => ok(value);

    private enum ResultState : byte
    {
        Uninitialized,
        Ok,
        Err
    }
}
