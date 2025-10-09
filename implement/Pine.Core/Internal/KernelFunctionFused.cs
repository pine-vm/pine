using System;

namespace Pine.Core.Internal;

public class KernelFunctionFused
{
    public static PineValue ListAppendItem(
        PineValue prefix,
        PineValue itemToAppend)
    {
        if (prefix is not PineValue.ListValue prefixList)
        {
            return prefix;
        }

        var newItems = new PineValue[prefixList.Items.Length + 1];

        prefixList.Items.Span.CopyTo(newItems);

        newItems[^1] = itemToAppend;

        return PineValue.List(newItems);
    }

    public static PineValue ListPrependItem(
        PineValue itemToPrepend,
        PineValue suffix)
    {
        if (suffix is not PineValue.ListValue suffixList)
        {
            return PineValue.List([itemToPrepend]);
        }

        var newItems = new PineValue[suffixList.Items.Length + 1];

        newItems[0] = itemToPrepend;

        suffixList.Items.Span.CopyTo(newItems.AsSpan(start: 1));

        return PineValue.List(newItems);
    }

    public static PineValue BlobPrependByte(
        byte byteToPrepend,
        PineValue suffix)
    {
        if (suffix is not PineValue.BlobValue suffixBlob)
        {
            return PineValue.BlobSingleByte(byteToPrepend);
        }

        if (suffixBlob.Bytes.Length is 0)
        {
            return PineValue.BlobSingleByte(byteToPrepend);
        }

        if (suffixBlob.Bytes.Length is 1)
        {
            return
                PineValue.ReusedBlobTupleFromBytes(
                    first: byteToPrepend,
                    second: suffixBlob.Bytes.Span[0]);
        }

        if (suffixBlob.Bytes.Length is 2)
        {
            if (byteToPrepend is 2)
            {
                return
                    PineValue.ReusedBlobInteger3ByteNegativeFromBytes(
                        second: suffixBlob.Bytes.Span[0],
                        third: suffixBlob.Bytes.Span[1]);
            }

            if (byteToPrepend is 4)
            {
                return
                    PineValue.ReusedBlobInteger3BytePositiveFromBytes(
                        second: suffixBlob.Bytes.Span[0],
                        third: suffixBlob.Bytes.Span[1]);
            }
        }

        var newBytes = new byte[suffixBlob.Bytes.Length + 1];

        newBytes[0] = byteToPrepend;

        suffixBlob.Bytes.Span.CopyTo(newBytes.AsSpan(start: 1));

        return PineValue.Blob(newBytes);
    }

    public static PineValue CanonicalIntegerFromUnsigned(
        bool signIsPositive,
        PineValue unsignedValue)
    {
        /*
         * Corresponding to 'int_add' with one argument being zero,
         * and the other argument is prepending a sign byte to the unsigned integer representation.
         * */

        if (unsignedValue is not PineValue.BlobValue unsignedBlob)
        {
            // The 'concat' would only return the single prefix byte, which is not accepted as integer by 'int_add'.
            return PineValue.EmptyList;
        }

        if (unsignedBlob.Bytes.Length is 0)
        {
            // The 'concat' would only return the single prefix byte, which is not accepted as integer by 'int_add'.
            return PineValue.EmptyList;
        }

        if (unsignedBlob.Bytes.Length is 1)
        {
            if (unsignedBlob.Bytes.Span[0] is 0)
            {
                // 'int_add' only returns positive zero.

                return PineValue.ReusedBlobTupleFromBytes(
                    first: 4,
                    second: 0);
            }

            return
                PineValue.ReusedBlobTupleFromBytes(
                    first: signIsPositive ? (byte)4 : (byte)2,
                    second: unsignedBlob.Bytes.Span[0]);
        }

        // 'int_add' would truncate leading zero bytes after sign byte, except the last zero byte.
        var remainingBytesCount = unsignedBlob.Bytes.Length;

        for (var i = 0; i < unsignedBlob.Bytes.Length - 1; i++)
        {
            if (unsignedBlob.Bytes.Span[i] is 0)
            {
                remainingBytesCount--;
            }
            else
            {
                break;
            }
        }

        if (remainingBytesCount is 1)
        {
            var valueByte = unsignedBlob.Bytes.Span[^1];

            if (valueByte is 0)
            {
                // 'int_add' only returns positive zero.

                return PineValue.ReusedBlobTupleFromBytes(
                    first: 4,
                    second: 0);
            }

            return
                PineValue.ReusedBlobTupleFromBytes(
                    first: signIsPositive ? (byte)4 : (byte)2,
                    second: valueByte);
        }

        if (remainingBytesCount is 2)
        {
            var valueUpperByte = unsignedBlob.Bytes.Span[^2];
            var valueLowerByte = unsignedBlob.Bytes.Span[^1];

            if (valueUpperByte is 0 && valueLowerByte is 0)
            {
                // 'int_add' only returns positive zero.

                return PineValue.ReusedBlobTupleFromBytes(
                    first: 4,
                    second: 0);
            }

            if (signIsPositive)
            {
                return
                    PineValue.ReusedBlobInteger3BytePositiveFromBytes(
                        second: valueUpperByte,
                        third: valueLowerByte);
            }
            else
            {
                return
                    PineValue.ReusedBlobInteger3ByteNegativeFromBytes(
                        second: valueUpperByte,
                        third: valueLowerByte);
            }
        }

        var newBytes = new byte[remainingBytesCount + 1];

        newBytes[0] = signIsPositive ? (byte)4 : (byte)2;

        unsignedBlob.Bytes.Span[^remainingBytesCount..].CopyTo(newBytes.AsSpan(start: 1));

        return PineValue.Blob(newBytes);
    }

    public static PineValue SkipAndTake(
        int takeCount,
        PineValue skipCountValue,
        PineValue argument)
    {
        if (KernelFunction.SignedIntegerFromValueRelaxed(skipCountValue) is not { } skipCount)
        {
            return PineValue.EmptyList;
        }

        return SkipAndTake(takeCount, (int)skipCount, argument);
    }

    public static PineValue SkipAndTake(
        int takeCount,
        int skipCount,
        PineValue argument)
    {
        skipCount =
            skipCount < 0 ? 0 : skipCount;

        if (argument is PineValue.ListValue argumentList)
        {
            var takeLimit =
                argumentList.Items.Length - skipCount;

            takeCount =
                takeLimit < takeCount
                ?
                takeLimit
                :
                takeCount;

            if (takeCount < 1)
                return PineValue.EmptyList;

            if (skipCount is 0 && takeCount == argumentList.Items.Length)
                return argument;

            var slicedItems = new PineValue[takeCount];

            for (var i = 0; i < takeCount; ++i)
            {
                slicedItems[i] = argumentList.Items.Span[skipCount + i];
            }

            return PineValue.List(slicedItems);
        }

        if (argument is PineValue.BlobValue argumentBlob)
        {
            var takeLimit =
                argumentBlob.Bytes.Length - skipCount;

            takeCount =
                takeLimit < takeCount
                ?
                takeLimit
                :
                takeCount;

            if (takeCount < 1)
                return PineValue.EmptyBlob;

            if (skipCount is 0 && takeCount == argumentBlob.Bytes.Length)
                return argument;

            var slicedBytes =
                argumentBlob.Bytes.Slice(start: skipCount, length: takeCount);

            return PineValue.Blob(slicedBytes);
        }

        throw new NotImplementedException(
            "Unexpected argument type: " + argument.GetType());
    }

    public static PineValue TakeAndSkip(
        PineValue skipCountValue,
        PineValue takeCountValue,
        PineValue argument)
    {
        if (KernelFunction.SignedIntegerFromValueRelaxed(skipCountValue) is not { } skipCount)
        {
            return PineValue.EmptyList;
        }

        if (KernelFunction.SignedIntegerFromValueRelaxed(takeCountValue) is not { } takeCount)
        {
            return PineValue.EmptyList;
        }

        return TakeAndSkip((int)skipCount, (int)takeCount, argument);
    }

    public static PineValue TakeAndSkip(
        int skipCount,
        int takeCount,
        PineValue argument)
    {
        skipCount =
            skipCount < 0 ? 0 : skipCount;

        if (argument is PineValue.ListValue argumentList)
        {
            var takeLimit =
                argumentList.Items.Length - skipCount;

            takeCount =
                takeLimit < takeCount
                ?
                takeLimit
                :
                takeCount;

            if (takeCount < 1)
                return PineValue.EmptyList;

            if (skipCount is 0 && takeCount == argumentList.Items.Length)
                return argument;

            var slicedItems = new PineValue[takeCount];

            for (var i = 0; i < takeCount; ++i)
            {
                var sourceIndex = i + skipCount;

                slicedItems[i] = argumentList.Items.Span[sourceIndex];
            }

            return PineValue.List(slicedItems);
        }

        if (argument is PineValue.BlobValue argumentBlob)
        {
            var takeLimit =
                argumentBlob.Bytes.Length - skipCount;

            takeCount =
                takeLimit < takeCount
                ?
                takeLimit
                :
                takeCount;

            if (takeCount < 1)
                return PineValue.EmptyBlob;

            if (skipCount is 0 && takeCount == argumentBlob.Bytes.Length)
                return argument;

            var slicedBytes =
                argumentBlob.Bytes.Slice(start: skipCount, length: takeCount);

            return PineValue.Blob(slicedBytes);
        }

        throw new NotImplementedException(
            "Unexpected argument type: " + argument.GetType());
    }

    public static PineValue TakeLast(
        int takeCount,
        PineValue value)
    {
        if (value is PineValue.ListValue listValue)
        {
            var listItems = listValue.Items.Span;

            if (listItems.Length <= takeCount)
                return value;

            if (takeCount <= 0)
                return PineValue.EmptyList;

            var resultingCount =
                takeCount <= listItems.Length
                ?
                takeCount
                :
                listItems.Length;

            var taken = new PineValue[resultingCount];

            listItems[^resultingCount..].CopyTo(taken);

            return PineValue.List(taken);
        }

        if (value is PineValue.BlobValue blobValue)
        {
            if (blobValue.Bytes.Length <= takeCount)
                return value;

            if (takeCount <= 0)
                return PineValue.EmptyBlob;

            return PineValue.Blob(blobValue.Bytes[^takeCount..]);
        }

        throw new NotImplementedException(
            "Unexpected value type: " + value.GetType().FullName);
    }

    public static PineValue SkipLast(
        int skipCount,
        PineValue value)
    {
        if (value is PineValue.ListValue listValue)
        {
            var listItems = listValue.Items.Span;

            if (listItems.Length <= skipCount)
                return PineValue.EmptyList;

            if (skipCount <= 0)
                return value;

            var resultingCount =
                skipCount <= listItems.Length
                ?
                listItems.Length - skipCount
                :
                0;

            var taken = new PineValue[resultingCount];

            listItems[..resultingCount].CopyTo(taken);

            return PineValue.List(taken);
        }

        if (value is PineValue.BlobValue blobValue)
        {
            if (blobValue.Bytes.Length <= skipCount)
                return PineValue.EmptyBlob;

            if (skipCount <= 0)
                return value;

            return PineValue.Blob(blobValue.Bytes[..^skipCount]);
        }

        throw new NotImplementedException(
            "Unexpected value type: " + value.GetType().FullName);
    }
}
