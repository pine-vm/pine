using Pine.Core.PopularEncodings;
using System;
using System.Collections.Generic;
using System.Numerics;

namespace Pine.Core.Internal;

using static Pine.Core.KernelFunction;

#pragma warning disable IDE1006

public static class KernelFunctionSpecialized
{
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
            var listItems = listValue.Elements.Span;

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

    public static PineValue take(BigInteger count, PineValue value)
    {
        if (value is PineValue.ListValue listValue)
        {
            var listItems = listValue.Elements.Span;

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

    public static PineValue takeLast(
        int count,
        PineValue value)
    {
        if (value is PineValue.ListValue listValue)
        {
            var listItems = listValue.Elements.Span;

            if (listItems.Length <= count)
                return value;

            if (count <= 0)
                return PineValue.EmptyList;

            var resultingCount =
                count <= listItems.Length
                ?
                count
                :
                listItems.Length;

            var taken = new PineValue[resultingCount];

            listItems[^resultingCount..].CopyTo(taken);

            return PineValue.List(taken);
        }

        if (value is PineValue.BlobValue blobValue)
        {
            if (blobValue.Bytes.Length <= count)
                return value;

            if (count <= 0)
                return PineValue.EmptyBlob;

            return PineValue.Blob(blobValue.Bytes[^count..]);
        }

        throw new NotImplementedException(
            "Unexpected value type: " + value.GetType().FullName);
    }

    public static PineValue concat(ReadOnlySpan<PineValue> list)
    {
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
                if (list[i] is PineValue.ListValue listValueElement)
                {
                    aggregateCount += listValueElement.Elements.Length;
                }
            }

            var concatenated = new PineValue[aggregateCount];

            var destItemIndex = 0;

            for (var i = 0; i < list.Length; ++i)
            {
                if (list[i] is PineValue.ListValue listValueElement)
                {
                    listValueElement.Elements.CopyTo(concatenated.AsMemory(start: destItemIndex));

                    destItemIndex += listValueElement.Elements.Length;
                }
            }

            return PineValue.List(concatenated);
        }

        if (head is PineValue.BlobValue)
        {
            var blobs = new List<ReadOnlyMemory<byte>>(capacity: list.Length);

            for (var i = 0; i < list.Length; ++i)
            {
                if (list[i] is not PineValue.BlobValue blobValue)
                    continue;

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
                return valueA;
            }

            if (listA.Elements.Length is 0)
            {
                return valueB;
            }

            if (listB.Elements.Length is 0)
            {
                return valueA;
            }

            var concatenated =
                new PineValue[listA.Elements.Length + listB.Elements.Length];

            listA.Elements.CopyTo(concatenated);

            listB.Elements.CopyTo(concatenated.AsMemory(start: listA.Elements.Length));

            return PineValue.List(concatenated);
        }

        if (valueA is PineValue.BlobValue blobA)
        {
            if (valueB is not PineValue.BlobValue blobB)
            {
                return valueA;
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

    public static PineValue int_mul(BigInteger factorA, BigInteger factorB) =>
        IntegerEncoding.EncodeSignedInteger(factorA * factorB);

    public static PineValue bit_and_binary(
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

    public static PineValue bit_or_binary(
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

    public static PineValue bit_xor_binary(
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
        PineValue.BlobValue blobValue)
    {
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
        PineValue.BlobValue blobValue)
    {
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
