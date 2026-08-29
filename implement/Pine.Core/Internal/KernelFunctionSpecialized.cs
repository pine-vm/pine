using Pine.Core.CommonEncodings;
using System;
using System.Collections.Generic;
using System.Numerics;

namespace Pine.Core.Internal;

using static Core.KernelFunction;

#pragma warning disable IDE1006

/// <summary>
/// This module is a temporary relay for backward compatibility with programs compiled for the old API.
/// See <see href="https://github.com/pine-vm/pine/commit/bd9dbc52769bb501eb03ee4bd04b7d2a1ee0af43"/> and
/// <see href="https://github.com/pine-vm/pine/commit/26ba7f86bd58659e46868b7de3f45cc99b113b16"/>.
/// This file is to be deleted after migrating the dependent systems to the new API.
/// </summary>
public static class KernelFunctionSpecialized
{
    /// <summary>
    /// Returns Pine's canonical boolean value for structural equality between two Pine values.
    /// </summary>
    public static PineValue equal(
        PineValue left,
        PineValue right)
    {
        return ValueFromBool(left == right);
    }

    /// <summary>
    /// Returns the host boolean result of structural equality between two Pine values.
    /// </summary>
    public static bool equal_as_boolean(
        PineValue left,
        PineValue right)
    {
        return left == right;
    }

    /// <summary>
    /// Returns the number of items in a list or the number of bytes in a blob as a CLR integer.
    /// </summary>
    public static int length_as_int(PineValue value)
    {
        if (value is PineValue.ListValue listValue)
        {
            return listValue.Items.Length;
        }

        if (value is PineValue.BlobValue blobValue)
        {
            return blobValue.Bytes.Length;
        }

        throw new NotImplementedException(
            "Unexpected value type: " + value.GetType().FullName);
    }

    /// <summary>
    /// Decodes the skip count from a Pine integer and removes that many leading items or bytes from the value.
    /// </summary>
    public static PineValue skip(PineValue countValue, PineValue value)
    {
        if (SignedIntegerFromValueRelaxed(countValue) is not { } count)
            return PineValue.EmptyList;

        return skip(count, value);
    }

    /// <summary>
    /// Removes count leading items or bytes from a list or blob.
    /// </summary>
    public static PineValue skip(BigInteger count, PineValue value)
    {
        if (count <= 0)
            return value;

        if (value is PineValue.BlobValue blobValue)
        {
            if (blobValue.Bytes.Length <= count)
                return PineValue.EmptyBlob;

            return PineValue.Blob(blobValue.Bytes[(int)count..]);
        }

        if (value is PineValue.ListValue listValue)
        {
            var listItems = listValue.Items.Span;

            var remainingCount = listItems.Length - (int)count;

            if (remainingCount <= 0)
                return PineValue.EmptyList;

            var skipped = new PineValue[remainingCount];

            listItems[(int)count..].CopyTo(skipped);

            return PineValue.List(skipped);
        }

        throw new NotImplementedException(
            "Unexpected value type: " + value.GetType().FullName);
    }

    /// <summary>
    /// Decodes the take count from a Pine integer and keeps that many leading items or bytes from the value.
    /// </summary>
    public static PineValue take(PineValue countValue, PineValue value)
    {
        if (SignedIntegerFromValueRelaxed(countValue) is not { } count)
            return PineValue.EmptyList;

        return take(count, value);
    }

    /// <summary>
    /// Keeps the first count items or bytes from a list or blob.
    /// </summary>
    public static PineValue take(BigInteger count, PineValue value)
    {
        if (value is PineValue.ListValue listValue)
        {
            var listItems = listValue.Items.Span;

            if (listItems.Length <= count)
                return value;

            if (count <= 0)
                return PineValue.EmptyList;

            var resultingCount =
                count <= listItems.Length
                ?
                (int)count
                :
                listItems.Length;

            var taken = new PineValue[resultingCount];

            listItems[..resultingCount].CopyTo(taken);

            return PineValue.List(taken);
        }

        if (value is PineValue.BlobValue blobValue)
        {
            if (blobValue.Bytes.Length <= count)
                return value;

            if (count <= 0)
                return PineValue.EmptyBlob;

            return PineValue.Blob(blobValue.Bytes[..(int)count]);
        }

        throw new NotImplementedException(
            "Unexpected value type: " + value.GetType().FullName);
    }

    /// <summary>
    /// Concatenates a span of same-kind lists or blobs, skipping leading empty lists and preserving a single remaining value as-is.
    /// </summary>
    public static PineValue concat(ReadOnlySpan<PineValue> listBeforeSkipEmpty)
    {
        // Skip over any empty lists at the start.

        var firstNonEmptyIndex = 0;

        while (firstNonEmptyIndex < listBeforeSkipEmpty.Length)
        {
            var item = listBeforeSkipEmpty[firstNonEmptyIndex];

            if (item is PineValue.ListValue listItem && listItem.Items.Length is 0)
            {
                ++firstNonEmptyIndex;
                continue;
            }

            break;
        }

        var list =
            firstNonEmptyIndex is 0
            ?
            listBeforeSkipEmpty
            :
            listBeforeSkipEmpty[firstNonEmptyIndex..];

        if (list.Length is 0)
        {
            return PineValue.EmptyList;
        }

        var head = list[0];

        if (list.Length is 1)
        {
            return head;
        }

        if (head is PineValue.ListValue)
        {
            var aggregateCount = 0;

            for (var i = 0; i < list.Length; ++i)
            {
                if (list[i] is not PineValue.ListValue listValueElement)
                {
                    return PineValue.EmptyList;
                }

                aggregateCount += listValueElement.Items.Length;
            }

            var concatenated = new PineValue[aggregateCount];

            var destItemIndex = 0;

            for (var i = 0; i < list.Length; ++i)
            {
                if (list[i] is not PineValue.ListValue listValueElement)
                {
                    return PineValue.EmptyList;
                }

                listValueElement.Items.CopyTo(concatenated.AsMemory(start: destItemIndex));

                destItemIndex += listValueElement.Items.Length;
            }

            return PineValue.List(concatenated);
        }

        if (head is PineValue.BlobValue)
        {
            var blobs = new List<ReadOnlyMemory<byte>>(capacity: list.Length);

            for (var i = 0; i < list.Length; ++i)
            {
                var item = list[i];

                if (item is PineValue.ListValue listItem && listItem.Items.Length is 0)
                {
                    // Skip empty lists
                    continue;
                }

                if (list[i] is not PineValue.BlobValue blobValue)
                    return PineValue.EmptyList;

                blobs.Add(blobValue.Bytes);
            }

            return PineValue.Blob(BytesConversions.Concat(blobs));
        }

        throw new NotImplementedException(
            "Unexpected value type: " + head.GetType().FullName);
    }

    /// <summary>
    /// Concatenates two lists, or two blobs when the right operand is also a blob. Under the legacy kernel semantics, a blob left operand is returned unchanged for non-blob right operands.
    /// </summary>
    public static PineValue concat(PineValue valueA, PineValue valueB)
    {
        if (valueA is PineValue.ListValue listA)
        {
            if (valueB is not PineValue.ListValue listB)
            {
                return PineValue.EmptyList;
            }

            if (listA.Items.Length is 0)
            {
                return valueB;
            }

            if (listB.Items.Length is 0)
            {
                return valueA;
            }

            var concatenated =
                new PineValue[listA.Items.Length + listB.Items.Length];

            listA.Items.CopyTo(concatenated);

            listB.Items.CopyTo(concatenated.AsMemory(start: listA.Items.Length));

            return PineValue.List(concatenated);
        }

        if (valueA is PineValue.BlobValue blobA)
        {
            if (valueB is not PineValue.BlobValue blobB)
            {
                return valueA;
            }

            if (blobA.Bytes.Length is 0)
            {
                return valueB;
            }

            if (blobB.Bytes.Length is 0)
            {
                return valueA;
            }

            if (blobA.Bytes.Length is 1 && blobB.Bytes.Length is 1)
            {
                return
                    PineValue.ReusedBlobTupleFromBytes(
                        first: blobA.Bytes.Span[0],
                        second: blobB.Bytes.Span[0]);
            }

            return PineValue.Blob(BytesConversions.Concat(blobA.Bytes.Span, blobB.Bytes.Span));
        }

        throw new NotImplementedException(
            "Unexpected value type: " + valueA.GetType().FullName);
    }

    /// <summary>
    /// Decodes two Pine integers and returns their sum in canonical Pine integer encoding.
    /// </summary>
    public static PineValue int_add(PineValue summandA, PineValue summandB)
    {
        if (SignedIntegerFromValueRelaxed(summandA) is not { } intA)
            return PineValue.EmptyList;

        if (SignedIntegerFromValueRelaxed(summandB) is not { } intB)
            return PineValue.EmptyList;

        return int_add(intA, intB);
    }

    /// <summary>
    /// Adds a known host integer to a Pine-encoded integer and returns the canonical Pine result.
    /// </summary>
    public static PineValue int_add(BigInteger summandA, PineValue summandBValue)
    {
        if (SignedIntegerFromValueRelaxed(summandBValue) is not { } intValue)
            return PineValue.EmptyList;

        return int_add(summandA, intValue);
    }

    /// <summary>
    /// Encodes the sum of two host integers as a Pine integer.
    /// </summary>
    public static PineValue int_add(BigInteger summandA, BigInteger summandB) =>
        IntegerEncoding.EncodeSignedInteger(summandA + summandB);

    /// <summary>
    /// Decodes two Pine integers and returns their product in canonical Pine integer encoding.
    /// </summary>
    public static PineValue int_mul(PineValue factorA, PineValue factorB)
    {
        if (SignedIntegerFromValueRelaxed(factorA) is not { } intA)
            return PineValue.EmptyList;

        if (SignedIntegerFromValueRelaxed(factorB) is not { } intB)
            return PineValue.EmptyList;

        return int_mul(intA, intB);
    }

    /// <summary>
    /// Multiplies a known host integer with a Pine-encoded integer and returns the canonical Pine result.
    /// </summary>
    public static PineValue int_mul(BigInteger factorA, PineValue factorBValue)
    {
        if (SignedIntegerFromValueRelaxed(factorBValue) is not { } intValue)
            return PineValue.EmptyList;

        return int_mul(factorA, intValue);
    }

    /// <summary>
    /// Encodes the product of two host integers as a Pine integer.
    /// </summary>
    public static PineValue int_mul(BigInteger factorA, BigInteger factorB) =>
        IntegerEncoding.EncodeSignedInteger(factorA * factorB);

    /// <summary>
    /// Encodes the product of three host integers as a Pine integer.
    /// </summary>
    public static PineValue int_mul(BigInteger factorA, BigInteger factorB, BigInteger factorC) =>
        IntegerEncoding.EncodeSignedInteger(factorA * factorB * factorC);

    /// <summary>
    /// Decodes two Pine integers, multiplies them with a known third factor, and returns the canonical Pine result.
    /// </summary>
    public static PineValue int_mul(PineValue factorA, PineValue factorB, BigInteger factorC)
    {
        if (SignedIntegerFromValueRelaxed(factorA) is not { } intA)
            return PineValue.EmptyList;

        if (SignedIntegerFromValueRelaxed(factorB) is not { } intB)
            return PineValue.EmptyList;

        return int_mul(intA, intB, factorC);
    }

    /// <summary>
    /// Returns Pine's canonical boolean value for whether two decoded integers are in ascending order.
    /// </summary>
    public static PineValue int_is_sorted_asc(
        PineValue left,
        PineValue right)
    {
        if (SignedIntegerFromValueRelaxed(left) is not { } leftInt)
        {
            return PineValue.EmptyList;
        }

        if (SignedIntegerFromValueRelaxed(right) is not { } rightInt)
        {
            return PineValue.EmptyList;
        }

        return
            ValueFromBool(leftInt <= rightInt);
    }

    /// <summary>
    /// Returns Pine's canonical boolean value for whether a decoded middle integer lies between the given bounds.
    /// </summary>
    public static PineValue int_is_sorted_asc(
        BigInteger left,
        PineValue middle,
        BigInteger right)
    {
        if (SignedIntegerFromValueRelaxed(middle) is not { } middleInt)
        {
            return PineValue.EmptyList;
        }

        return ValueFromBool(left <= middleInt && middleInt <= right);
    }

    /// <summary>
    /// Returns Pine's canonical boolean value for whether three host integers are already in ascending order.
    /// </summary>
    public static PineValue int_is_sorted_asc(
        BigInteger left,
        BigInteger middle,
        BigInteger right)
    {
        return ValueFromBool(left <= middle && middle <= right);
    }

    /// <summary>
    /// Returns the host boolean outcome of the ascending-order check for two Pine-encoded integers. Invalid integer encodings produce false.
    /// </summary>
    public static bool int_is_sorted_asc_as_boolean(
        PineValue left,
        PineValue right)
    {
        // Return type: Would value equal the canonical 'True' value if it were returned?

        if (SignedIntegerFromValueRelaxed(left) is not { } leftInt)
        {
            return false;
        }

        if (SignedIntegerFromValueRelaxed(right) is not { } rightInt)
        {
            return false;
        }

        return leftInt <= rightInt;
    }

    /// <summary>
    /// Returns whether a decoded Pine integer is greater than or equal to the given lower bound. Invalid integer encodings produce false.
    /// </summary>
    public static bool int_is_sorted_asc_as_boolean(
        BigInteger left,
        PineValue right)
    {
        // Return type: Would value equal the canonical 'True' value if it were returned?

        if (SignedIntegerFromValueRelaxed(right) is not { } rightInt)
        {
            return false;
        }

        return left <= rightInt;
    }

    /// <summary>
    /// Returns whether a decoded Pine integer is less than or equal to the given upper bound. Invalid integer encodings produce false.
    /// </summary>
    public static bool int_is_sorted_asc_as_boolean(
        PineValue left,
        BigInteger right)
    {
        // Return type: Would value equal the canonical 'True' value if it were returned?

        if (SignedIntegerFromValueRelaxed(left) is not { } leftInt)
        {
            return false;
        }

        return leftInt <= right;
    }

    /// <summary>
    /// Returns the host boolean outcome of the ascending-order check for three host integers.
    /// </summary>
    public static bool int_is_sorted_asc_as_boolean(
        BigInteger left,
        BigInteger middle,
        BigInteger right)
    {
        // Return type: Would value equal the canonical 'True' value if it were returned?
        return left <= middle && middle <= right;
    }

    /// <summary>
    /// Computes the bitwise AND of two blobs after aligning them at their least-significant bytes.
    /// </summary>
    public static PineValue bit_and(
        PineValue left,
        PineValue right)
    {
        if (left is not PineValue.BlobValue leftBlob)
        {
            return PineValue.EmptyList;
        }

        if (right is not PineValue.BlobValue rightBlob)
        {
            return PineValue.EmptyList;
        }

        var commonLength =
            leftBlob.Bytes.Length < rightBlob.Bytes.Length
            ?
            leftBlob.Bytes.Length
            :
            rightBlob.Bytes.Length;

        var resultArray =
            new byte[commonLength];

        for (var i = 0; i < commonLength; ++i)
        {
            var leftByte =
                leftBlob.Bytes.Span[leftBlob.Bytes.Length - commonLength + i];

            var rightByte =
                rightBlob.Bytes.Span[rightBlob.Bytes.Length - commonLength + i];

            resultArray[i] =
                (byte)(leftByte & rightByte);
        }

        return PineValue.Blob(resultArray);
    }

    /// <summary>
    /// Computes the bitwise OR of two blobs after right-aligning them and zero-extending the shorter input.
    /// </summary>
    public static PineValue bit_or(
        PineValue left,
        PineValue right)
    {
        if (left is not PineValue.BlobValue leftBlob)
        {
            return PineValue.EmptyList;
        }

        if (right is not PineValue.BlobValue rightBlob)
        {
            return PineValue.EmptyList;
        }

        var maxLength =
            leftBlob.Bytes.Length > rightBlob.Bytes.Length
            ?
            leftBlob.Bytes.Length
            :
            rightBlob.Bytes.Length;

        var resultArray =
            new byte[maxLength];

        for (var i = 0; i < maxLength; ++i)
        {
            var leftIndex =
                leftBlob.Bytes.Length - maxLength + i;

            var rightIndex =
                rightBlob.Bytes.Length - maxLength + i;

            var leftByte =
                leftIndex < 0
                ?
                (byte)0
                :
                leftBlob.Bytes.Span[leftIndex];

            var rightByte =
                rightIndex < 0
                ?
                (byte)0
                :
                rightBlob.Bytes.Span[rightIndex];

            resultArray[i] = (byte)(leftByte | rightByte);
        }

        return PineValue.Blob(resultArray);
    }

    /// <summary>
    /// Computes the bitwise XOR of two blobs after right-aligning them and zero-extending the shorter input.
    /// </summary>
    public static PineValue bit_xor(
        PineValue left,
        PineValue right)
    {
        if (left is not PineValue.BlobValue leftBlob)
        {
            return PineValue.EmptyList;
        }

        if (right is not PineValue.BlobValue rightBlob)
        {
            return PineValue.EmptyList;
        }

        var maxLength =
            leftBlob.Bytes.Length > rightBlob.Bytes.Length
            ?
            leftBlob.Bytes.Length
            :
            rightBlob.Bytes.Length;

        var resultArray =
            new byte[maxLength];

        for (var i = 0; i < maxLength; ++i)
        {
            var leftIndex =
                leftBlob.Bytes.Length - maxLength + i;

            var rightIndex =
                rightBlob.Bytes.Length - maxLength + i;

            var leftByte =
                leftIndex < 0
                ?
                (byte)0
                :
                leftBlob.Bytes.Span[leftIndex];

            var rightByte =
                rightIndex < 0
                ?
                (byte)0
                :
                rightBlob.Bytes.Span[rightIndex];

            resultArray[i] = (byte)(leftByte ^ rightByte);
        }

        return PineValue.Blob(resultArray);
    }

    /// <summary>
    /// Shifts a blob left by the given bit count within its existing width, discarding overflow bits.
    /// </summary>
    public static PineValue bit_shift_left(
        BigInteger shiftCount,
        PineValue value)
    {
        if (value is not PineValue.BlobValue blobValue)
        {
            return PineValue.EmptyList;
        }

        var offsetBytes = (int)(shiftCount / 8);
        var offsetBits = (int)(shiftCount % 8);

        var resultArray = new byte[blobValue.Bytes.Length];

        byte previousCarry = 0;

        for (var sourceIndex = blobValue.Bytes.Length - 1; offsetBytes <= sourceIndex; --sourceIndex)
        {
            var sourceByte = blobValue.Bytes.Span[sourceIndex];

            var shifted = (byte)(sourceByte << offsetBits);

            var destinationIndex = sourceIndex - offsetBytes;

            resultArray[destinationIndex] = (byte)(shifted | previousCarry);

            previousCarry = (byte)(sourceByte >> 8 - offsetBits);
        }

        return PineValue.Blob(resultArray);
    }

    /// <summary>
    /// Shifts a blob right by the given bit count within its existing width, discarding overflow bits.
    /// </summary>
    public static PineValue bit_shift_right(
        BigInteger shiftCount,
        PineValue value)
    {
        if (value is not PineValue.BlobValue blobValue)
        {
            return PineValue.EmptyList;
        }

        var offsetBytes = (int)(shiftCount / 8);
        var offsetBits = (int)(shiftCount % 8);

        var resultArray = new byte[blobValue.Bytes.Length];

        byte previousCarry = 0;

        for (var sourceIndex = 0; sourceIndex < blobValue.Bytes.Length - offsetBytes; ++sourceIndex)
        {
            var sourceByte = blobValue.Bytes.Span[sourceIndex];

            var shifted = (byte)(sourceByte >> offsetBits);

            var destinationIndex = sourceIndex + offsetBytes;

            resultArray[destinationIndex] = (byte)(shifted | previousCarry);

            previousCarry = (byte)(sourceByte << 8 - offsetBits);
        }

        return PineValue.Blob(resultArray);
    }
}
