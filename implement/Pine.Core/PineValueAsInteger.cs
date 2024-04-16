using System;

namespace Pine;

public static class PineValueAsInteger
{
    /// <summary>
    /// Converts an integer into a blob value with an unsigned representation of that integer.
    /// Returns an error if the input integer is less than zero.
    /// </summary>
    public static Result<string, PineValue> ValueFromUnsignedInteger(System.Numerics.BigInteger integer) =>
        BlobValueFromUnsignedInteger(integer)
        .Map(PineValue.Blob);

    public static Result<string, ReadOnlyMemory<byte>> BlobValueFromUnsignedInteger(System.Numerics.BigInteger integer)
    {
        var signedBlobValue = BlobValueFromSignedInteger(integer);

        if (signedBlobValue.Span[0] != 4)
            return "Argument is a negative integer.";

        return signedBlobValue[1..];
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
