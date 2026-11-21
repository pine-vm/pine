using System.Numerics;

namespace Pine.Core.DotNet;

/// <summary>Shares a derived slice offset across native decisions without speculating a conversion.</summary>
public struct PineCSharpSliceOffset
{
    private bool _hasValue;

    private BigInteger _count;

    private BigInteger _multiplier;

    private int _offset;

    /// <summary>Derives the clamped byte offset on first demand; invalid counts remain invalid.</summary>
    public int? Get(PineCSharpValue count, BigInteger multiplier)
    {
        if (count.AsInteger() is not { } integer)
            return null;

        if (!_hasValue || _count != integer || _multiplier != multiplier)
        {
            var multiplied = integer * multiplier;
            var offset = multiplied < 0 ? 0 : (int)multiplied;
            _count = integer;
            _multiplier = multiplier;
            _offset = offset;
            _hasValue = true;
        }

        return _offset;
    }
}
