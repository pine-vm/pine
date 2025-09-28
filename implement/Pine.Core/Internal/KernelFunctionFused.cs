using System;

namespace Pine.Core.Internal;

public class KernelFunctionFused
{
    public static PineValue FusedSkipAndTake(PineValue argument, PineValue skipCountValue, int takeCount)
    {
        if (KernelFunction.SignedIntegerFromValueRelaxed(skipCountValue) is not { } skipCount)
        {
            return PineValue.EmptyList;
        }

        return FusedSkipAndTake(argument, (int)skipCount, takeCount);
    }

    public static PineValue FusedSkipAndTake(PineValue argument, int skipCount, int takeCount)
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
}
