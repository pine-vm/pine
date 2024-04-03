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

    public static Result<string, System.Numerics.BigInteger> SignedIntegerFromValue(PineValue value)
    {
        if (value is not PineValue.BlobValue blob)
            return "Only a BlobValue can represent an integer.";

        return SignedIntegerFromBlobValue(blob.Bytes.Span);
    }

    public static Result<string, System.Numerics.BigInteger> SignedIntegerFromBlobValue(ReadOnlySpan<byte> blobValue)
    {
        if (blobValue.Length < 2)
            return "Empty blob is not a valid integer because it is shorter than 2 bytes. Did you mean to use an unsigned integer?";

        var signByte = blobValue[0];

        if (signByte is not 4 && signByte is not 2)
            return "Unexpected value for sign byte of integer: " + signByte;

        var absValue = UnsignedIntegerFromBlobValue(blobValue[1..]);

        return
            signByte is 4
            ?
            absValue
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
