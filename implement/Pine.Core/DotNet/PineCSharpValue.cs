using Pine.Core.Internal;
using System;
using System.Numerics;

namespace Pine.Core.DotNet;

/// <summary>
/// A non-allocating internal value view used by generated methods. Original Pine representations
/// remain available until an operation produces a new scalar; scalar results materialize on demand.
/// </summary>
public struct PineCSharpValue
{
    private enum Representation : byte
    {
        Pine, SmallInteger, Integer, Boolean, Invalid
    }

    private Representation _representation;

    private PineValueInProcess? _pine;

    private BigInteger _integer;

    private long _smallInteger;

    private bool _boolean;

    /// <summary>Retains an original value without decoding or materializing it.</summary>
    public static PineCSharpValue FromValue(PineValueInProcess value) => new() { _pine = value };

    /// <summary>Creates an unwrapped machine integer.</summary>
    public static PineCSharpValue FromInt64(long value) =>
        new() { _representation = Representation.SmallInteger, _smallInteger = value };

    /// <summary>Creates an unwrapped integer, including the kernel's invalid-number result.</summary>
    public static PineCSharpValue FromInteger(BigInteger? value) =>
        value is not { } integer
        ?
        new() { _representation = Representation.Invalid }
        :
        integer >= long.MinValue && integer <= long.MaxValue
        ?
        FromInt64((long)integer)
        :
        new() { _representation = Representation.Integer, _integer = integer };

    /// <summary>Creates an unwrapped kernel boolean, or its invalid-input result.</summary>
    public static PineCSharpValue FromBoolean(bool? value) =>
        value is { } boolean
        ?
        new() { _representation = Representation.Boolean, _boolean = boolean }
        :
        new() { _representation = Representation.Invalid };

    /// <summary>Returns the original representation or materializes a scalar once in this view.</summary>
    public PineValueInProcess ToPineValue() =>
        _pine ??=
            _representation switch
            {
                Representation.Pine or Representation.Invalid => PineValueInProcess.EmptyList,
                Representation.SmallInteger => PineValueInProcess.CreateInteger(_smallInteger),
                Representation.Integer => PineValueInProcess.CreateInteger(_integer),
                Representation.Boolean => PineValueInProcess.CreateBool(_boolean),

                _ =>
                throw new NotImplementedException("ToPineValue does not handle representation: " + _representation)
            };

    /// <summary>Obtains a numeric interpretation without losing an original blob representation.</summary>
    public BigInteger? AsInteger()
    {
        if (_representation is Representation.SmallInteger)
            return new BigInteger(_smallInteger);

        if (_representation is Representation.Integer)
            return _integer;

        if (_representation is Representation.Invalid)
            return null;

        var integer = ToPineValue().AsInteger();

        if (integer is { } value)
        {
            var pine = _pine;
            this = FromInteger(value);
            _pine = pine;
        }
        else
            _representation = Representation.Invalid;

        return integer;
    }

    /// <summary>Tests exact kernel truth without constructing a Pine boolean.</summary>
    public bool IsTrue() =>
        _representation is Representation.Boolean
        ?
        _boolean
        :
        _representation is Representation.Invalid
        ?
        false
        :
        PineValueInProcess.AreEqual(ToPineValue(), PineValueInProcess.KernelTrueValue);

    private bool TryInt64(out long value)
    {
        if (_representation is Representation.SmallInteger)
        {
            value = _smallInteger;
            return true;
        }

        var integer = AsInteger();

        if (integer is { } number && number >= long.MinValue && number <= long.MaxValue)
        {
            value = (long)number;
            return true;
        }

        value = 0;
        return false;
    }

    /// <summary>Adds numeric views with a machine-integer fast path and arbitrary-precision overflow handling.</summary>
    public static PineCSharpValue Add(PineCSharpValue left, PineCSharpValue right)
    {
        if (left.TryInt64(out var a) && right.TryInt64(out var b) &&
            (b <= 0 || a <= long.MaxValue - b) && (b >= 0 || a >= long.MinValue - b))
        {
            return FromInt64(a + b);
        }

        return
            left.AsInteger() is { } x && right.AsInteger() is { } y
            ?
            FromInteger(x + y)
            :
            FromInteger(null);
    }

    /// <summary>Multiplies numeric views without constructing intermediate Pine values.</summary>
    public static PineCSharpValue Multiply(PineCSharpValue left, PineCSharpValue right)
    {
        if (left.TryInt64(out var a) && right.TryInt64(out var b))
        {
            var magnitudeA = a < 0 ? (ulong)(-(a + 1)) + 1 : (ulong)a;
            var magnitudeB = b < 0 ? (ulong)(-(b + 1)) + 1 : (ulong)b;
            var limit = (a < 0) != (b < 0) ? 1UL << 63 : (ulong)long.MaxValue;

            if (magnitudeB == 0 || magnitudeA <= limit / magnitudeB)
                return FromInt64(unchecked(a * b));
        }

        return
            left.AsInteger() is { } x && right.AsInteger() is { } y
            ?
            FromInteger(x * y)
            :
            FromInteger(null);
    }

    /// <summary>Preserves left-to-right decoding for a fused multiply/add operation.</summary>
    public static PineCSharpValue MultiplyAdd(
        PineCSharpValue left, PineCSharpValue right, BigInteger multiplier) =>
        left.AsInteger() is { } x && right.AsInteger() is { } y
        ?
        FromInteger(x + multiplier * y)
        :
        FromInteger(null);

    /// <summary>Compares numeric views, returning null when either operand is not an integer.</summary>
    public static bool? LessThanOrEqual(PineCSharpValue left, PineCSharpValue right)
    {
        if (left.TryInt64(out var a) && right.TryInt64(out var b))
            return a <= b;

        return left.AsInteger() is { } x && right.AsInteger() is { } y ? x <= y : null;
    }

    /// <summary>Compares numeric views, returning null when either operand is not an integer.</summary>
    public static bool? LessThan(PineCSharpValue left, PineCSharpValue right)
    {
        if (left.TryInt64(out var a) && right.TryInt64(out var b))
            return a < b;

        return left.AsInteger() is { } x && right.AsInteger() is { } y ? x < y : null;
    }
}
