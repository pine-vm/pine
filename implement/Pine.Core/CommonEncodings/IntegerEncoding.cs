using System;
using System.Collections.Generic;
using System.Linq;

namespace Pine.Core.CommonEncodings;

/// <summary>
/// Standard encoding of integers in Pine.
/// <para>
/// This integer encoding corresponds to kernel functions like <see cref="KernelFunction.int_add(PineValue)"/> and <see cref="KernelFunction.skip(PineValue)"/>.
/// </para>
/// <para>
/// When parsing, the 'strict' variant means we only accept the canonical encodings as produced by the encoding functions.
/// In 'relaxed' parsing, we accept non-canonical encodings like negative zero or zeros bytes directly after the sign byte.
/// </para>
/// </summary>
public static class IntegerEncoding
{
    /// <summary>
    /// Converts an integer into a blob value with an unsigned representation of that integer.
    /// Returns an error if the input integer is less than zero.
    /// </summary>
    public static Result<string, PineValue> EncodeUnsignedInteger(System.Numerics.BigInteger integer)
    {
        if (s_reusedValueFromUnsignedInteger is { } reused && integer < reused.Count && 0 <= integer)
        {
            return reused[(int)integer];
        }

        if (integer < 0)
            return "Argument is a negative integer.";

        return PineValue.Blob(EncodeUnsignedIntegerBlobThrowing(integer));
    }

    private static readonly IReadOnlyList<PineValue> s_reusedValueFromUnsignedInteger =
        [..Enumerable.Range(0, 10_000)
        .Select(range => EncodeUnsignedInteger(range).Extract(err => throw new Exception(err)))];

    private static readonly IReadOnlyList<PineValue> s_reusedValueFromSignedIntegerPositive =
        [.. Enumerable.Range(0, 10_000).Select(range => EncodeSignedInteger(range))];

    /// <summary>
    /// Returns a reused instance of a <see cref="PineValue"/> encoding <paramref name="integer"/> in unsigned form.
    /// <para>
    /// Returns null if the set of unsigned reused instances does not cover the given integer.
    /// </para>
    /// </summary>
    public static PineValue? ReusedInstanceForUnsignedInteger(int integer)
    {
        if (integer < 0 || s_reusedValueFromUnsignedInteger is not { } reused)
            return null;

        if (integer < reused.Count)
            return reused[integer];

        return null;
    }

    /// <summary>
    /// Blob bytes encoding the given integer in unsigned form.
    /// <para>
    /// Throws an exception if the given integer is negative.
    /// </para>
    /// </summary>
    public static ReadOnlyMemory<byte> EncodeUnsignedIntegerBlobThrowing(System.Numerics.BigInteger integer)
    {
        if (integer < 0)
        {
            throw new ArgumentOutOfRangeException(nameof(integer), "Argument is a negative integer.");
        }

        if (s_reusedValueFromUnsignedInteger is { } reusedUnsigned &&
            integer < reusedUnsigned.Count)
        {
            if (reusedUnsigned[(int)integer] is PineValue.BlobValue blob)
                return blob.Bytes;
        }

        var array = integer.ToByteArray(isUnsigned: true, isBigEndian: true);

        return array;
    }

    /// <summary>
    /// Encode the given integer in signed form.
    /// </summary>
    public static PineValue EncodeSignedInteger(System.Numerics.BigInteger integer)
    {
        if (integer > long.MinValue && integer < long.MaxValue)
        {
            var asLong = (long)integer;

            if (s_reusedValueFromSignedIntegerPositive is { } reusedPositive &&
                0 <= asLong &&
                asLong < reusedPositive.Count)
            {
                return reusedPositive[(int)asLong];
            }
        }

        var absoluteValue = System.Numerics.BigInteger.Abs(integer);

        var signByte =
            (byte)(absoluteValue == integer ? 4 : 2);

        Span<byte> absoluteBuffer = stackalloc byte[64];

        absoluteBuffer[0] = signByte;

        if (absoluteValue.TryWriteBytes(absoluteBuffer[1..], out var bytesWritten, isUnsigned: true, isBigEndian: true))
        {
            if (bytesWritten is 1)
            {
                return PineValue.ReusedBlobTupleFromBytes(signByte, absoluteBuffer[1]);
            }

            if (bytesWritten is 2)
            {
                if (signByte is 2)
                {
                    return PineValue.ReusedBlobInteger3ByteNegativeFromBytes(absoluteBuffer[1], absoluteBuffer[2]);
                }
                else
                {
                    return PineValue.ReusedBlobInteger3BytePositiveFromBytes(absoluteBuffer[1], absoluteBuffer[2]);
                }
            }

            return PineValue.Blob(absoluteBuffer[..(1 + bytesWritten)].ToArray());
        }

        return PineValue.Blob(EncodeSignedIntegerBlob(integer));
    }

    /// <summary>
    /// Blob bytes encoding the given integer in signed form.
    /// </summary>
    public static ReadOnlyMemory<byte> EncodeSignedIntegerBlob(System.Numerics.BigInteger integer)
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

    /// <summary>
    /// Parse a signed integer from a Pine blob using strict validation rules.
    /// <para>
    /// Strict parsing rejects ambiguous encodings (leading zero after the sign byte and negative zero).
    /// </para>
    /// </summary>
    /// <param name="value">The <see cref="PineValue"/> expected to be a <see cref="PineValue.BlobValue"/> holding the integer bytes.</param>
    /// <returns>
    /// An <see cref="Result{T1, T2}"/> with the parsed <see cref="System.Numerics.BigInteger"/> on success, or an error message on failure.
    /// </returns>
    public static Result<string, System.Numerics.BigInteger> ParseSignedIntegerStrict(PineValue value) =>
        ParseSignedInteger(value, rejectLeadingZero: true, rejectNegativeZero: true);

    /// <summary>
    /// Parse a signed integer from a Pine blob using relaxed validation rules.
    /// <para>
    /// Relaxed parsing accepts non-canonical encodings (such as a leading zero after the sign byte or negative zero).
    /// </para>
    /// </summary>
    /// <param name="value">The <see cref="PineValue"/> expected to be a <see cref="PineValue.BlobValue"/> holding the integer bytes.</param>
    /// <returns>
    /// An <see cref="Result{T1, T2}"/> with the parsed <see cref="System.Numerics.BigInteger"/> on success, or an error message on failure.
    /// </returns>
    public static Result<string, System.Numerics.BigInteger> ParseSignedIntegerRelaxed(PineValue value) =>
        ParseSignedInteger(value, rejectLeadingZero: false, rejectNegativeZero: false);

    /// <summary>
    /// Try to parse the given <see cref="PineValue"/> as a signed integer with configurable validation.
    /// </summary>
    /// <param name="value">The value to parse; must be a <see cref="PineValue.BlobValue"/>.</param>
    /// <param name="rejectLeadingZero">If true, reject encodings where the first magnitude byte is zero (when length &gt; 2).</param>
    /// <param name="rejectNegativeZero">If true, reject encodings of negative zero (sign byte 2 with all following bytes zero).</param>
    /// <returns>
    /// An <see cref="Result{T1, T2}"/> with the parsed <see cref="System.Numerics.BigInteger"/> on success, or an error message on failure.
    /// </returns>
    public static Result<string, System.Numerics.BigInteger> ParseSignedInteger(
        PineValue value,
        bool rejectLeadingZero,
        bool rejectNegativeZero)
    {
        if (value is not PineValue.BlobValue blob)
            return "Not a blob.";

        return ParseSignedInteger(
            blob.Bytes.Span,
            rejectLeadingZero: rejectLeadingZero,
            rejectNegativeZero: rejectNegativeZero);
    }

    /// <summary>
    /// Parse a signed integer from a byte sequence using strict validation rules.
    /// <para>
    /// Strict parsing rejects ambiguous encodings (leading zero after the sign byte and negative zero).
    /// </para>
    /// </summary>
    /// <param name="blobValue">Sequence of bytes representing the signed integer.</param>
    /// <returns>
    /// An <see cref="Result{T1, T2}"/> with the parsed <see cref="System.Numerics.BigInteger"/> on success, or an error message on failure.
    /// </returns>
    public static Result<string, System.Numerics.BigInteger> ParseSignedIntegerStrict(ReadOnlySpan<byte> blobValue) =>
        ParseSignedInteger(blobValue, rejectLeadingZero: true, rejectNegativeZero: true);

    /// <summary>
    /// Parse a signed integer from a byte sequence using relaxed validation rules.
    /// <para>
    /// Relaxed parsing accepts non-canonical encodings (such as a leading zero after the sign byte or negative zero).
    /// </para>
    /// </summary>
    /// <param name="blobValue">Sequence of bytes representing the signed integer.</param>
    /// <returns>
    /// An <see cref="Result{T1, T2}"/> with the parsed <see cref="System.Numerics.BigInteger"/> on success, or an error message on failure.
    /// </returns>
    public static Result<string, System.Numerics.BigInteger> ParseSignedIntegerRelaxed(ReadOnlySpan<byte> blobValue) =>
        ParseSignedInteger(blobValue, rejectLeadingZero: false, rejectNegativeZero: false);

    /// <summary>
    /// Try parse the given byte sequence as a signed integer.
    /// </summary>
    /// <param name="blobValue">Sequence of bytes</param>
    /// <param name="rejectLeadingZero">Return an error if the first byte is zero</param>
    /// <param name="rejectNegativeZero">Return an error if the sign is negative and all following bytes are zero</param>
    /// <returns></returns>
    public static Result<string, System.Numerics.BigInteger> ParseSignedInteger(
        ReadOnlySpan<byte> blobValue,
        bool rejectLeadingZero,
        bool rejectNegativeZero)
    {
        if (blobValue.Length < 2)
            return "Not a valid integer because it is shorter than 2 bytes. Did you mean to parse an unsigned integer?";

        var signByte = blobValue[0];

        if (signByte is not 4 && signByte is not 2)
            return "Unexpected value for sign byte of integer: " + signByte;

        if (rejectLeadingZero && 2 < blobValue.Length && blobValue[1] is 0)
            return "Avoid ambiguous leading zero.";

        var absValue = ParseUnsignedInteger(blobValue[1..]);

        return
            signByte is 4
            ?
            absValue
            :
            rejectNegativeZero && absValue.IsZero
            ?
            "Avoid ambiguous negative zero."
            :
            absValue * System.Numerics.BigInteger.MinusOne;
    }

    /// <summary>
    /// Try parse the given value as an unsigned integer.
    /// </summary>
    public static Result<string, System.Numerics.BigInteger> ParseUnsignedInteger(PineValue value) =>
        value switch
        {
            PineValue.BlobValue blob =>
            ParseUnsignedInteger(blob.Bytes.Span),

            _ =>
            "Only a BlobValue can represent an integer."
        };

    /// <summary>
    /// Parse the given byte sequence as an unsigned integer.
    /// </summary>
    public static System.Numerics.BigInteger ParseUnsignedInteger(ReadOnlySpan<byte> blobValue) =>
        new(blobValue, isUnsigned: true, isBigEndian: true);
}
