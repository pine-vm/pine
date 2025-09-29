using System;

namespace Pine.Core.Internal;

public class KernelFunctionFused
{
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
}
