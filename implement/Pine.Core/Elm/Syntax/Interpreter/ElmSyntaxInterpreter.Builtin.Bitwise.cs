using Pine.Core.Encodings;
using Pine.Core.Internal;
using Pine.Core.PineLanguage;
using System;
using System.Collections.Generic;
using System.Numerics;

namespace Pine.Core.Elm.Syntax.Interpreter;

/// <summary>
/// Whole-function shortcuts for <c>elm-kernel-modules/Bitwise.elm</c>. These compose the same
/// blob transformations as the Elm source, including its nonstandard padding, truncation,
/// and shift behavior, rather than substituting host-language 32-bit bitwise semantics.
/// </summary>
public partial class ElmSyntaxInterpreter
{
    private static readonly PineValue s_bitwiseMask32 = PineValue.Blob([255, 255, 255, 255]);

    private static readonly PineValue s_bitwiseMask31 = PineValue.Blob([127, 255, 255, 255]);

    private static readonly PineValue s_bitwiseSignBit = PineValue.Blob([128, 0, 0, 0]);

    private static readonly PineValue s_bitwisePadding = PineValue.Blob([0, 0, 0, 0]);

    private static PineValueInProcess? ResolveBitwiseAnd(IReadOnlyList<PineValueInProcess> arguments) =>
        ResolveBitwiseBinary(arguments, BuiltinFunctionSpecialized.bit_and);

    private static PineValueInProcess? ResolveBitwiseOr(IReadOnlyList<PineValueInProcess> arguments) =>
        ResolveBitwiseBinary(arguments, BuiltinFunctionSpecialized.bit_or);

    private static PineValueInProcess? ResolveBitwiseXor(IReadOnlyList<PineValueInProcess> arguments) =>
        ResolveBitwiseBinary(arguments, BuiltinFunctionSpecialized.bit_xor);

    private static PineValueInProcess? ResolveBitwiseBinary(
        IReadOnlyList<PineValueInProcess> arguments,
        Func<PineValue, PineValue, PineValue> combine)
    {
        if (arguments.Count is not 2 ||
            AsPlainInteger(arguments[0]) is not { } left ||
            AsPlainInteger(arguments[1]) is not { } right)
        {
            return null;
        }

        var leftBytes = BitwiseOperandBytes(arguments[0].Evaluate(), left);
        var rightBytes = BitwiseOperandBytes(arguments[1].Evaluate(), right);

        return PineValueInProcess.Create(BitwiseSignedResult(combine(leftBytes, rightBytes)));
    }

    private static PineValue BitwiseOperandBytes(PineValue value, BigInteger integer)
    {
        if (integer.Sign >= 0)
        {
            return
                BuiltinFunctionSpecialized.bit_and(
                    BuiltinFunctionSpecialized.skip(1, value),
                    s_bitwiseMask32);
        }

        var incremented = IntegerEncoding.EncodeSignedInteger(integer + 1);

        var beforeComplement =
            integer >= int.MinValue
            ?
            BuiltinFunctionSpecialized.bit_or(
                BuiltinFunctionSpecialized.skip(1, incremented),
                s_bitwisePadding)
            :
            BuiltinFunctionSpecialized.bit_and(incremented, s_bitwiseMask32);

        return BuiltinFunction.bit_not(beforeComplement);
    }

    private static bool BitwiseHasSignBit(PineValue bytes) =>
        BuiltinFunctionSpecialized.bit_and(bytes, s_bitwiseSignBit) == s_bitwiseSignBit;

    private static PineValue BitwiseSignedResult(PineValue bytes)
    {
        var magnitude = BuiltinFunctionSpecialized.bit_and(bytes, s_bitwiseMask31);

        return
            BitwiseHasSignBit(bytes)
            ?
            BuiltinFunctionSpecialized.int_add(
                int.MinValue,
                BuiltinFunctionFused.BlobPrependByte(4, magnitude))
            :
            BuiltinFunctionFused.CanonicalIntegerFromUnsigned(signIsPositive: true, magnitude);
    }

    private static PineValueInProcess? ResolveBitwiseComplement(IReadOnlyList<PineValueInProcess> arguments)
    {
        if (arguments.Count is not 1 ||
            AsPlainInteger(arguments[0]) is not { } integer)
        {
            return null;
        }

        if (integer >= int.MinValue && integer <= int.MaxValue)
        {
            return PineValueInProcess.CreateInteger(-integer - 1);
        }

        var value = arguments[0].Evaluate();
        var magnitude = BuiltinFunctionSpecialized.bit_and(value, s_bitwiseMask31);

        var result =
            BitwiseHasSignBit(value)
            ?
            BuiltinFunctionSpecialized.int_add(
                int.MaxValue,
                BuiltinFunctionFused.BlobPrependByte(2, magnitude))
            :
            BuiltinFunctionSpecialized.int_add(
                -1,
                BuiltinFunctionFused.BlobPrependByte(4, magnitude));

        return PineValueInProcess.Create(result);
    }

    private static PineValueInProcess? ResolveBitwiseShiftLeftBy(IReadOnlyList<PineValueInProcess> arguments)
    {
        if (arguments.Count is not 2 ||
            AsPlainInteger(arguments[0]) is not { } offset ||
            AsPlainInteger(arguments[1]) is not { } integer)
        {
            return null;
        }

        // The negative branch deliberately retains the sign byte of int_add's result.
        var withPadding =
            integer >= int.MinValue && integer <= -1
            ?
            BuiltinFunctionSpecialized.bit_or(
                IntegerEncoding.EncodeSignedInteger(integer - int.MinValue),
                s_bitwiseSignBit)
            :
            BuiltinFunctionSpecialized.bit_or(
                BuiltinFunctionSpecialized.skip(1, arguments[1].Evaluate()),
                s_bitwisePadding);

        var shifted = BuiltinFunctionSpecialized.bit_shift_left(offset, withPadding);

        return PineValueInProcess.Create(BitwiseSignedResult(shifted));
    }

    private static PineValueInProcess? ResolveBitwiseShiftRightBy(IReadOnlyList<PineValueInProcess> arguments)
    {
        if (arguments.Count is not 2 ||
            AsPlainInteger(arguments[0]) is not { } offset ||
            AsPlainInteger(arguments[1]) is not { } integer)
        {
            return null;
        }

        if (integer < int.MinValue || integer > int.MaxValue)
        {
            var value = arguments[1].Evaluate();
            var magnitude = BuiltinFunctionSpecialized.bit_and(value, s_bitwiseMask31);

            var asInt32 =
                BitwiseHasSignBit(value)
                ?
                BuiltinFunctionSpecialized.int_add(
                    int.MinValue,
                    BuiltinFunctionFused.BlobPrependByte(4, magnitude))
                :
                BuiltinFunctionSpecialized.int_add(
                    -1,
                    BuiltinFunctionFused.BlobPrependByte(2, magnitude));

            integer =
                BuiltinFunction.SignedIntegerFromValueRelaxed(asInt32)
                ?? throw new InvalidOperationException("Bitwise.shiftRightBy: expected a normalized integer.");
        }

        var signIsPositive = integer.Sign >= 0;
        var beforeShift = IntegerEncoding.EncodeSignedInteger(signIsPositive ? integer : integer - 1);
        var magnitudeToShift = BuiltinFunctionSpecialized.skip(1, beforeShift);
        var shifted = BuiltinFunctionSpecialized.bit_shift_right(offset, magnitudeToShift);

        return
            PineValueInProcess.Create(
                BuiltinFunctionFused.CanonicalIntegerFromUnsigned(signIsPositive, shifted));
    }

    private static PineValueInProcess? ResolveBitwiseShiftRightZfBy(IReadOnlyList<PineValueInProcess> arguments)
    {
        if (arguments.Count is not 2 ||
            AsPlainInteger(arguments[0]) is not { } offset ||
            AsPlainInteger(arguments[1]) is not { } integer)
        {
            return null;
        }

        var value = arguments[1].Evaluate();

        PineValue shifted;

        if (BuiltinFunctionSpecialized.take(1, value) == PineValue.BlobSingleByte(4))
        {
            var magnitude = BuiltinFunctionSpecialized.skip(1, value);
            var beforeTruncate = BuiltinFunctionSpecialized.bit_shift_right(offset, magnitude);

            shifted =
                BuiltinFunctionSpecialized.skip(
                    BigInteger.Max(0, BuiltinFunctionSpecialized.length_as_int(beforeTruncate) - 4),
                    beforeTruncate);
        }
        else
        {
            var incremented = IntegerEncoding.EncodeSignedInteger(integer + 1);
            var magnitude = BuiltinFunctionSpecialized.skip(1, incremented);

            var padded =
                BuiltinFunction.reverse(
                    BuiltinFunctionSpecialized.take(
                        4,
                        BuiltinFunctionSpecialized.concat(
                            BuiltinFunction.reverse(magnitude),
                            s_bitwisePadding)));

            var fromTwosComplement32 = BuiltinFunction.bit_not(padded);

            shifted = BuiltinFunctionSpecialized.bit_shift_right(offset, fromTwosComplement32);
        }

        return
            PineValueInProcess.Create(
                BuiltinFunctionFused.CanonicalIntegerFromUnsigned(signIsPositive: true, shifted));
    }
}
