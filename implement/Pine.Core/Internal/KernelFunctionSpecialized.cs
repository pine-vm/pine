using Pine.Core.PopularEncodings;
using System;
using System.Collections.Generic;
using System.Numerics;

namespace Pine.Core.Internal;

using static Core.KernelFunction;

#pragma warning disable IDE1006

public static class KernelFunctionSpecialized
{
    public static PineValue equal(
        PineValue left,
        PineValue right)
    {
        return ValueFromBool(left == right);
    }

    public static bool equal_as_boolean(
        PineValue left,
        PineValue right)
    {
        return left == right;
    }

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

    public static PineValue skip(PineValue countValue, PineValue value)
    {
        if (SignedIntegerFromValueRelaxed(countValue) is not { } count)
            return PineValue.EmptyList;

        return skip(count, value);
    }

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

    public static PineValue take(PineValue countValue, PineValue value)
    {
        if (SignedIntegerFromValueRelaxed(countValue) is not { } count)
            return PineValue.EmptyList;

        return take(count, value);
    }

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

    public static PineValue int_add(PineValue summandA, PineValue summandB)
    {
        if (SignedIntegerFromValueRelaxed(summandA) is not { } intA)
            return PineValue.EmptyList;

        if (SignedIntegerFromValueRelaxed(summandB) is not { } intB)
            return PineValue.EmptyList;

        return int_add(intA, intB);
    }

    public static PineValue int_add(BigInteger summandA, PineValue summandBValue)
    {
        if (SignedIntegerFromValueRelaxed(summandBValue) is not { } intValue)
            return PineValue.EmptyList;

        return int_add(summandA, intValue);
    }

    public static PineValue int_add(BigInteger summandA, BigInteger summandB) =>
        IntegerEncoding.EncodeSignedInteger(summandA + summandB);

    public static PineValue int_mul(PineValue factorA, PineValue factorB)
    {
        if (SignedIntegerFromValueRelaxed(factorA) is not { } intA)
            return PineValue.EmptyList;

        if (SignedIntegerFromValueRelaxed(factorB) is not { } intB)
            return PineValue.EmptyList;

        return int_mul(intA, intB);
    }

    public static PineValue int_mul(BigInteger factorA, PineValue factorBValue)
    {
        if (SignedIntegerFromValueRelaxed(factorBValue) is not { } intValue)
            return PineValue.EmptyList;

        return int_mul(factorA, intValue);
    }

    public static PineValue int_mul(BigInteger factorA, BigInteger factorB) =>
        IntegerEncoding.EncodeSignedInteger(factorA * factorB);

    public static PineValue int_mul(BigInteger factorA, BigInteger factorB, BigInteger factorC) =>
        IntegerEncoding.EncodeSignedInteger(factorA * factorB * factorC);

    public static PineValue int_mul(PineValue factorA, PineValue factorB, BigInteger factorC)
    {
        if (SignedIntegerFromValueRelaxed(factorA) is not { } intA)
            return PineValue.EmptyList;

        if (SignedIntegerFromValueRelaxed(factorB) is not { } intB)
            return PineValue.EmptyList;

        return int_mul(intA, intB, factorC);
    }

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

    public static PineValue int_is_sorted_asc(
        BigInteger left,
        BigInteger middle,
        BigInteger right)
    {
        return ValueFromBool(left <= middle && middle <= right);
    }

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

    public static bool int_is_sorted_asc_as_boolean(
        BigInteger left,
        BigInteger middle,
        BigInteger right)
    {
        // Return type: Would value equal the canonical 'True' value if it were returned?
        return left <= middle && middle <= right;
    }

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
