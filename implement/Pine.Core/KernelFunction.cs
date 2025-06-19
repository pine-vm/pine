using Pine.Core.PopularEncodings;
using Pine.PineVM;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Numerics;

namespace Pine.Core;

#pragma warning disable IDE1006

public static class KernelFunction
{
    public static PineValue equal(PineValue value)
    {
        if (value is PineValue.ListValue listValue)
        {
            /*
            * :: rest pattern seems not implemented yet:
            * https://github.com/dotnet/csharplang/issues/6574
            * */

            var listItems = listValue.Elements.Span;

            if (listItems.Length < 1)
                return PineVMValues.TrueValue;

            var firstItem = listItems[0];

            for (var i = 1; i < listItems.Length; ++i)
            {
                if (!listItems[i].Equals(firstItem))
                    return PineVMValues.FalseValue;
            }

            return PineVMValues.TrueValue;
        }

        if (value is PineValue.BlobValue blobValue)
        {
            return
                BlobAllBytesEqual(blobValue.Bytes)
                ?
                PineVMValues.TrueValue
                :
                PineVMValues.FalseValue;
        }

        throw new NotImplementedException(
            "Unexpected value type: " + value.GetType().FullName);
    }

    private static bool BlobAllBytesEqual(ReadOnlyMemory<byte> readOnlyMemory) =>
        readOnlyMemory.IsEmpty ||
        !readOnlyMemory.Span.ContainsAnyExcept(readOnlyMemory.Span[0]);

    public static PineValue equal(PineValue valueA, PineValue valueB) =>
        ValueFromBool(valueA.Equals(valueB));

    public static PineValue negate(PineValue value) =>
        value switch
        {
            PineValue.BlobValue blobValue
            when 0 < blobValue.Bytes.Length =>
            blobValue.Bytes.Span[0] switch
            {
                4 =>
                PineValue.Blob([2, .. blobValue.Bytes.Span[1..]]),

                2 =>
                PineValue.Blob([4, .. blobValue.Bytes.Span[1..]]),

                _ =>
                PineValue.EmptyList
            },

            _ =>
            PineValue.EmptyList
        };

    public static PineValue length(PineValue value) =>
        IntegerEncoding.EncodeSignedInteger(
            value switch
            {
                PineValue.BlobValue blobValue =>
                blobValue.Bytes.Length,

                PineValue.ListValue listValue =>
                listValue.Elements.Length,

                _ =>
                throw new NotImplementedException(
                    "Unexpected value type: " + value.GetType().FullName)
            });

    public static PineValue skip(PineValue value) =>
        value switch
        {
            PineValue.ListValue listValue =>
            listValue.Elements.Length is 2
            ?
            SignedIntegerFromValueRelaxed(listValue.Elements.Span[0]) switch
            {
                { } count =>
                skip(count, listValue.Elements.Span[1]),

                _ =>
                PineValue.EmptyList
            }
            :
            PineValue.EmptyList,

            PineValue.BlobValue =>
            PineValue.EmptyList,

            _ =>
            throw new NotImplementedException()
        };

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

    public static PineValue take(PineValue value) =>
        value switch
        {
            PineValue.ListValue listValue =>
            listValue.Elements.Length is 2 ?
            SignedIntegerFromValueRelaxed(listValue.Elements.Span[0]) switch
            {
                { } count =>
                take(count, listValue.Elements.Span[1]),

                _ =>
                PineValue.EmptyList
            }
            :
            PineValue.EmptyList,

            PineValue.BlobValue =>
            PineValue.EmptyList,

            _ =>
            throw new NotImplementedException()
        };

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

    public static PineValue reverse(PineValue value)
    {
        if (value is PineValue.ListValue listValue)
        {
            if (listValue.Elements.Length <= 1)
                return value;

            var reversed = listValue.Elements.ToArray();

            Array.Reverse(reversed);

            return PineValue.List(reversed);
        }

        if (value is PineValue.BlobValue blobValue)
        {
            if (blobValue.Bytes.Length <= 1)
                return value;

            var reversed = blobValue.Bytes.ToArray();

            reversed.AsMemory().Span.Reverse();

            return PineValue.Blob(reversed);
        }

        throw new NotImplementedException(
            "Unexpected value type: " + value.GetType().FullName);
    }

    public static PineValue concat(PineValue value)
    {
        if (value is not PineValue.ListValue listValue)
        {
            return PineValue.EmptyList;
        }

        return concat(listValue.Elements.Span);
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

            for (int i = 0; i < list.Length; ++i)
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

    public static PineValue head(PineValue value)
    {
        if (value is PineValue.ListValue listValue)
        {
            if (listValue.Elements.Length is 0)
            {
                return PineValue.EmptyList;
            }

            return listValue.Elements.Span[0];
        }

        return PineValue.EmptyList;
    }

    public static PineValue int_add(PineValue value) =>
        KernelFunctionExpectingListOfBigIntAndProducingBigInt(
            integers =>
            integers.Aggregate(seed: BigInteger.Zero, func: (aggregate, next) => aggregate + next),
            value);

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

    public static PineValue int_mul(PineValue value) =>
        KernelFunctionExpectingListOfBigIntAndProducingBigInt(
            integers =>
            integers.Aggregate(seed: BigInteger.One, func: (aggregate, next) => aggregate * next),
            value);

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

    public static PineValue int_is_sorted_asc(PineValue value)
    {
        if (value is PineValue.BlobValue blobValue)
        {
            var isSorted = true;

            if (blobValue.Bytes.Length > 0)
            {
                var previous = blobValue.Bytes.Span[0];

                foreach (var current in blobValue.Bytes.Span[1..])
                {
                    if (current < previous)
                    {
                        isSorted = false;
                        break;
                    }

                    previous = current;
                }
            }

            return ValueFromBool(isSorted);
        }

        if (value is PineValue.ListValue listValue)
        {
            var listItems = listValue.Elements.Span;

            if (listItems.Length is 0)
                return ValueFromBool(true);

            if (SignedIntegerFromValueRelaxed(listItems[0]) is not { } firstInt)
            {
                return PineValue.EmptyList;
            }

            var previous = firstInt;

            for (var i = 1; i < listItems.Length; ++i)
            {
                var next = listItems[i];

                if (SignedIntegerFromValueRelaxed(next) is not { } nextInt)
                {
                    return PineValue.EmptyList;
                }

                if (nextInt < previous)
                    return ValueFromBool(false);

                previous = nextInt;
            }

            return ValueFromBool(true);
        }

        throw new NotImplementedException(
            "Unexpected value type: " + value.GetType().FullName);
    }

    public static PineValue bit_and(PineValue value)
    {
        if (value is not PineValue.ListValue argumentsList)
        {
            return PineValue.EmptyList;
        }

        var argumentsItems = argumentsList.Elements.Span;

        if (argumentsItems.Length is 0)
        {
            return PineValue.EmptyList;
        }

        if (argumentsItems[0] is not PineValue.BlobValue firstBlob)
        {
            return PineValue.EmptyList;
        }

        if (argumentsItems.Length is 1)
        {
            return firstBlob;
        }

        PineValue merged = firstBlob;

        for (var i = 1; i < argumentsItems.Length; ++i)
        {
            if (argumentsItems[i] is not PineValue.BlobValue blobValue)
            {
                return PineValue.EmptyList;
            }

            merged = bit_and_binary(merged, blobValue);
        }

        return merged;
    }

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

    public static PineValue bit_or(PineValue value)
    {
        if (value is not PineValue.ListValue argumentsList)
        {
            return PineValue.EmptyList;
        }

        var argumentsItems = argumentsList.Elements.Span;

        if (argumentsItems.Length is 0)
        {
            return PineValue.EmptyList;
        }

        if (argumentsItems[0] is not PineValue.BlobValue firstBlob)
        {
            return PineValue.EmptyList;
        }

        if (argumentsItems.Length is 1)
        {
            return firstBlob;
        }

        PineValue merged = firstBlob;

        for (var i = 1; i < argumentsItems.Length; ++i)
        {
            if (argumentsItems[i] is not PineValue.BlobValue blobValue)
            {
                return PineValue.EmptyList;
            }

            merged = bit_or_binary(merged, blobValue);
        }

        return merged;
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

    public static PineValue bit_xor(PineValue value)
    {
        if (value is not PineValue.ListValue argumentsList)
        {
            return PineValue.EmptyList;
        }

        var argumentsItems = argumentsList.Elements.Span;

        if (argumentsItems.Length is 0)
        {
            return PineValue.EmptyList;
        }

        if (argumentsItems[0] is not PineValue.BlobValue firstBlob)
        {
            return PineValue.EmptyList;
        }

        if (argumentsItems.Length is 1)
        {
            return firstBlob;
        }

        PineValue merged = firstBlob;

        for (var i = 1; i < argumentsItems.Length; ++i)
        {
            if (argumentsItems[i] is not PineValue.BlobValue blobValue)
            {
                return PineValue.EmptyList;
            }

            merged = bit_xor_binary(merged, blobValue);
        }

        return merged;
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

    public static PineValue bit_not(PineValue value)
    {
        if (value is not PineValue.BlobValue blobValue)
        {
            return PineValue.EmptyList;
        }

        var resultArray = new byte[blobValue.Bytes.Length];

        for (var i = 0; i < resultArray.Length; ++i)
        {
            resultArray[i] = (byte)~blobValue.Bytes.Span[i];
        }

        return PineValue.Blob(resultArray);
    }

    public static PineValue bit_shift_left(PineValue value)
    {
        if (value is not PineValue.ListValue argumentsList)
        {
            return PineValue.EmptyList;
        }

        var argumentsItems = argumentsList.Elements.Span;

        if (argumentsItems.Length is not 2)
        {
            return PineValue.EmptyList;
        }

        if (SignedIntegerFromValueRelaxed(argumentsItems[0]) is not { } shiftCount)
        {
            return PineValue.EmptyList;
        }

        if (argumentsItems[1] is not PineValue.BlobValue blobValue)
        {
            return PineValue.EmptyList;
        }

        return bit_shift_left(shiftCount, blobValue);
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

            previousCarry = (byte)(sourceByte >> (8 - offsetBits));
        }

        return PineValue.Blob(resultArray);
    }

    public static PineValue bit_shift_right(PineValue value)
    {
        if (value is not PineValue.ListValue argumentsList)
        {
            return PineValue.EmptyList;
        }

        var argumentsItems = argumentsList.Elements.Span;

        if (argumentsItems.Length is not 2)
        {
            return PineValue.EmptyList;
        }

        if (SignedIntegerFromValueRelaxed(argumentsItems[0]) is not { } shiftCount)
        {
            return PineValue.EmptyList;
        }

        if (argumentsItems[1] is not PineValue.BlobValue blobValue)
        {
            return PineValue.EmptyList;
        }

        return bit_shift_right(shiftCount, blobValue);
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

            previousCarry = (byte)(sourceByte << (8 - offsetBits));
        }

        return PineValue.Blob(resultArray);
    }

    private static PineValue KernelFunctionExpectingListOfBigIntAndProducingBigInt(
        Func<IReadOnlyList<BigInteger>, BigInteger> aggregate,
        PineValue value) =>
        KernelFunctionExpectingListOfBigInt(
            aggregate:
            listOfIntegers => IntegerEncoding.EncodeSignedInteger(aggregate(listOfIntegers)),
            value);

    private static PineValue KernelFunctionExpectingListOfBigInt(
        Func<IReadOnlyList<BigInteger>, PineValue> aggregate,
        PineValue value) =>
        KernelFunctionExpectingList(
            value,
            list =>
            {
                var asIntegers = new BigInteger[list.Length];

                for (var i = 0; i < list.Length; ++i)
                {
                    if (SignedIntegerFromValueRelaxed(list.Span[i]) is not { } intResult)
                        return PineValue.EmptyList;

                    asIntegers[i] = intResult;
                }

                return aggregate(asIntegers);
            });

    public static BigInteger? SignedIntegerFromValueRelaxed(PineValue value)
    {
        if (value is not PineValue.BlobValue blobValue)
            return null;

        int leadingSpaces = 0;

        var blobBytes = blobValue.Bytes;

        for (var i = 0; i < blobBytes.Length; ++i)
        {
            if (blobBytes.Span[i] is 0)
            {
                ++leadingSpaces;
            }
            else
            {
                break;
            }
        }

        var firstValueByteIndex = leadingSpaces + 1;

        var valueBytesCount =
            blobBytes.Length - firstValueByteIndex;

        if (valueBytesCount is < 1)
            return null;

        var abs =
            valueBytesCount is 1
            ?
            (BigInteger)blobBytes.Span[firstValueByteIndex]
            :
            valueBytesCount is 2
            ?
            new BigInteger(blobBytes.Span[firstValueByteIndex] << 8 | blobBytes.Span[firstValueByteIndex + 1])
            :
            new BigInteger(blobBytes.Span[firstValueByteIndex..], isUnsigned: true, isBigEndian: true);

        return
            blobBytes.Span[leadingSpaces] switch
            {
                4 => abs,
                2 => -abs,
                _ => null
            };
    }

    public static PineValue KernelFunctionExpectingList(
        PineValue value,
        Func<ReadOnlyMemory<PineValue>, PineValue> continueWithList) =>
        value switch
        {
            PineValue.ListValue list =>
            continueWithList(list.Elements),

            _ =>
            PineValue.EmptyList
        };

    public static Result<string, bool> ParseBoolFromValue(PineValue value) =>
        value == PineVMValues.TrueValue
        ?
        true
        :
        value == PineVMValues.FalseValue
        ?
        false
        :
        "Value is neither True nor False";

    public static PineValue ValueFromBool(bool b) =>
        b ?
        PineVMValues.TrueValue :
        PineVMValues.FalseValue;
}