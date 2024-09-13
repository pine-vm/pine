using System;
using System.Collections.Generic;
using System.Linq;

namespace Pine.Core;

/// <summary>
/// Standard encoding of integers in Pine.
/// This integer encoding corresponds to kernel functions like <see cref="KernelFunction.add_int(PineValue)"/> and <see cref="KernelFunction.skip(PineValue)"/>.
/// </summary>
public static class PineValueAsInteger
{
    /// <summary>
    /// Converts an integer into a blob value with an unsigned representation of that integer.
    /// Returns an error if the input integer is less than zero.
    /// </summary>
    public static Result<string, PineValue> ValueFromUnsignedInteger(System.Numerics.BigInteger integer) =>
        ReusedValueFromUnsignedInteger is { } reused && integer < reused.Count && 0 <= integer ?
        reused[(int)integer]
        :
        BlobValueFromUnsignedInteger(integer)
        .Map(PineValue.Blob);

    private static readonly IReadOnlyList<PineValue> ReusedValueFromUnsignedInteger =
        [..Enumerable.Range(0, 10_000)
        .Select(Range => ValueFromUnsignedInteger(Range).Extract(err => throw new Exception(err)))];

    public static Result<string, ReadOnlyMemory<byte>> BlobValueFromUnsignedInteger(System.Numerics.BigInteger integer)
    {
        if (integer < 0)
            return "Argument is a negative integer.";

        var array = integer.ToByteArray(isUnsigned: true, isBigEndian: true);

        return Result<string, ReadOnlyMemory<byte>>.ok(array);
    }

    public static PineValue ValueFromSignedInteger(System.Numerics.BigInteger integer) =>
        PineValue.Blob(BlobValueFromSignedInteger(integer));

    public static ReadOnlyMemory<byte> BlobValueFromSignedInteger(System.Numerics.BigInteger integer)
    {
        var absoluteValue = System.Numerics.BigInteger.Abs(integer);

        var signByte =
            (byte)(absoluteValue == integer ? 4 : 2);

        var absoluteArray = absoluteValue.ToByteArray(isUnsigned: true, isBigEndian: true);

        var memory = new byte[1 + absoluteArray.Length];

        memory[0] = signByte;
        absoluteArray.CopyTo(memory, 1);

        return memory;
    }

    public static Result<string, System.Numerics.BigInteger> SignedIntegerFromValueStrict(PineValue value) =>
        SignedIntegerFromValue(value, rejectLeadingZero: true, rejectNegativeZero: true);

    public static Result<string, System.Numerics.BigInteger> SignedIntegerFromValueRelaxed(PineValue value) =>
        SignedIntegerFromValue(value, rejectLeadingZero: false, rejectNegativeZero: false);

    public static Result<string, System.Numerics.BigInteger> SignedIntegerFromValue(
        PineValue value,
        bool rejectLeadingZero,
        bool rejectNegativeZero)
    {
        if (value is not PineValue.BlobValue blob)
            return "Not a blob.";

        return SignedIntegerFromBlobValue(
            blob.Bytes.Span,
            rejectLeadingZero: rejectLeadingZero,
            rejectNegativeZero: rejectNegativeZero);
    }

    public static Result<string, System.Numerics.BigInteger> SignedIntegerFromBlobValueStrict(ReadOnlySpan<byte> blobValue) =>
        SignedIntegerFromBlobValue(blobValue, rejectLeadingZero: true, rejectNegativeZero: true);

    public static Result<string, System.Numerics.BigInteger> SignedIntegerFromBlobValueRelaxed(ReadOnlySpan<byte> blobValue) =>
        SignedIntegerFromBlobValue(blobValue, rejectLeadingZero: false, rejectNegativeZero: false);

    public static Result<string, System.Numerics.BigInteger> SignedIntegerFromBlobValue(
        ReadOnlySpan<byte> blobValue,
        bool rejectLeadingZero,
        bool rejectNegativeZero)
    {
        if (blobValue.Length < 2)
            return "Not a valid integer because it is shorter than 2 bytes. Did you mean to use an unsigned integer?";

        var signByte = blobValue[0];

        if (signByte is not 4 && signByte is not 2)
            return "Unexpected value for sign byte of integer: " + signByte;

        if (rejectLeadingZero && 2 < blobValue.Length && blobValue[1] is 0)
            return "Avoid ambiguous leading zero.";

        var absValue = UnsignedIntegerFromBlobValue(blobValue[1..]);

        return
            signByte is 4
            ?
            absValue
            :
            rejectNegativeZero && absValue == 0
            ?
            "Avoid ambiguous negative zero."
            :
            absValue * System.Numerics.BigInteger.MinusOne;
    }

    public static Result<string, System.Numerics.BigInteger> UnsignedIntegerFromValue(PineValue value) =>
        value switch
        {
            PineValue.BlobValue blob =>
            UnsignedIntegerFromBlobValue(blob.Bytes.Span),

            _ =>
            "Only a BlobValue can represent an integer."
        };

    public static System.Numerics.BigInteger UnsignedIntegerFromBlobValue(ReadOnlySpan<byte> blobValue) =>
        new(blobValue, isUnsigned: true, isBigEndian: true);
}
