using Pine.Core.Internal;
using System;
using System.Collections.Generic;
using System.Numerics;

namespace Pine.Core.Elm.Syntax.Interpreter;

/// <summary>
/// JavaScript-compatible 32-bit operations, matching the bundled Bitwise.elm implementation.
/// </summary>
public partial class ElmSyntaxInterpreter
{
    private static int BitwiseInt32(BigInteger value) =>
        unchecked((int)(uint)(value & uint.MaxValue));

    private static PineValueInProcess? ResolveBitwiseAnd(IReadOnlyList<PineValueInProcess> arguments) =>
        ResolveBitwiseBinary(arguments, (left, right) => left & right);

    private static PineValueInProcess? ResolveBitwiseOr(IReadOnlyList<PineValueInProcess> arguments) =>
        ResolveBitwiseBinary(arguments, (left, right) => left | right);

    private static PineValueInProcess? ResolveBitwiseXor(IReadOnlyList<PineValueInProcess> arguments) =>
        ResolveBitwiseBinary(arguments, (left, right) => left ^ right);

    private static PineValueInProcess? ResolveBitwiseShiftLeftBy(IReadOnlyList<PineValueInProcess> arguments) =>
        ResolveBitwiseBinary(arguments, (offset, value) => value << (offset & 31));

    private static PineValueInProcess? ResolveBitwiseShiftRightBy(IReadOnlyList<PineValueInProcess> arguments) =>
        ResolveBitwiseBinary(arguments, (offset, value) => value >> (offset & 31));

    private static PineValueInProcess? ResolveBitwiseShiftRightZfBy(IReadOnlyList<PineValueInProcess> arguments) =>
        ResolveBitwiseBinary(arguments, (offset, value) => unchecked((uint)value) >> (offset & 31));

    private static PineValueInProcess? ResolveBitwiseBinary(
        IReadOnlyList<PineValueInProcess> arguments,
        Func<int, int, long> combine)
    {
        if (arguments.Count is not 2 ||
            AsPlainInteger(arguments[0]) is not { } left ||
            AsPlainInteger(arguments[1]) is not { } right)
        {
            return null;
        }

        return PineValueInProcess.CreateInteger(combine(BitwiseInt32(left), BitwiseInt32(right)));
    }

    private static PineValueInProcess? ResolveBitwiseComplement(IReadOnlyList<PineValueInProcess> arguments) =>
        arguments.Count is 1 && AsPlainInteger(arguments[0]) is { } integer
        ?
        PineValueInProcess.CreateInteger(~BitwiseInt32(integer))
        :
        null;
}
