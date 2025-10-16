using Pine.Core.PineVM;
using Pine.Core.PopularEncodings;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Numerics;

namespace Pine.Core;

#pragma warning disable IDE1006


/// <summary>
/// Pine kernel functions: the primitive operations of the Pine expression language.
/// Pine’s value model has only two forms: blobs (byte sequences) and lists (heterogeneous sequences).
/// All primitives are invoked by name with a single <see cref="PineValue"/> argument that may be a list
/// encoding multiple logical parameters (e.g. <c>[ count, sequence ]</c> for <c>skip</c>/<c>take</c>).
/// </summary>
public static class KernelFunction
{
    /// <summary>
    /// Evaluates a Pine kernel function by name with the given input value.
    /// Dispatches to the corresponding primitive operation based on the function name.
    /// </summary>
    /// <param name="function">The name of the kernel function to evaluate.</param>
    /// <param name="inputValue">The input value to the kernel function.</param>
    /// <returns>The result of evaluating the kernel function with the provided input value.</returns>
    /// <exception cref="InvalidOperationException">Thrown if the function name is unknown.</exception>
    public static PineValue ApplyKernelFunctionGeneric(
        string function,
        PineValue inputValue)
    {
        return function switch
        {
            nameof(equal) =>
            equal(inputValue),

            nameof(length) =>
            length(inputValue),

            nameof(head) =>
            head(inputValue),

            nameof(skip) =>
            skip(inputValue),

            nameof(take) =>
            take(inputValue),

            nameof(concat) =>
            concat(inputValue),

            nameof(reverse) =>
            reverse(inputValue),

            nameof(negate) =>
            negate(inputValue),

            nameof(int_add) =>
            int_add(inputValue),

            nameof(int_mul) =>
            int_mul(inputValue),

            nameof(int_is_sorted_asc) =>
            int_is_sorted_asc(inputValue),

            nameof(bit_and) =>
            bit_and(inputValue),

            nameof(bit_or) =>
            bit_or(inputValue),

            nameof(bit_xor) =>
            bit_xor(inputValue),

            nameof(bit_not) =>
            bit_not(inputValue),

            nameof(bit_shift_left) =>
            bit_shift_left(inputValue),

            nameof(bit_shift_right) =>
            bit_shift_right(inputValue),

            _ =>
            throw new InvalidOperationException(
                "Unknown kernel function '" + function + "'")
        };
    }

    /// <summary>
    /// For a list, reports if all elements are equal. For a blob, reports if all bytes are equal.
    /// </summary>
    /// <param name="value">The input value, which can be a list or a blob.</param>
    /// <returns>A PineValue representing true or false.</returns>
    public static PineValue equal(PineValue value)
    {
        if (value is PineValue.ListValue listValue)
        {
            /*
            * :: rest pattern seems not implemented yet:
            * https://github.com/dotnet/csharplang/issues/6574
            * */

            var listItems = listValue.Items.Span;

            if (listItems.Length < 1)
                return PineKernelValues.TrueValue;

            var firstItem = listItems[0];

            for (var i = 1; i < listItems.Length; ++i)
            {
                if (!listItems[i].Equals(firstItem))
                    return PineKernelValues.FalseValue;
            }

            return PineKernelValues.TrueValue;
        }

        if (value is PineValue.BlobValue blobValue)
        {
            return
                BlobAllBytesEqual(blobValue.Bytes)
                ?
                PineKernelValues.TrueValue
                :
                PineKernelValues.FalseValue;
        }

        throw new NotImplementedException(
            "Unexpected value type: " + value.GetType().FullName);
    }

    public static PineValue negate(PineValue value)
    {
        if (value is not PineValue.BlobValue blobValue)
            return PineValue.EmptyList;

        if (blobValue.Bytes.Length is 0)
            return PineValue.EmptyList;

        if (blobValue.Bytes.Span[0] is 2)
        {
            if (blobValue.Bytes.Length is 1)
                return PineKernelValues.TrueValue;

            return PineValue.Blob([4, .. blobValue.Bytes.Span[1..]]);
        }

        if (blobValue.Bytes.Span[0] is 4)
        {
            if (blobValue.Bytes.Length is 1)
                return PineKernelValues.FalseValue;

            return PineValue.Blob([2, .. blobValue.Bytes.Span[1..]]);
        }

        return PineValue.EmptyList;
    }

    /// <summary>
    /// For a list, returns the number of elements. For a blob, returns the number of bytes.
    /// </summary>
    /// <param name="value">The input value, which can be a list or a blob.</param>
    /// <returns>A PineValue representing the length as a signed integer.</returns>
    public static PineValue length(PineValue value)
    {
        return
            IntegerEncoding.EncodeSignedInteger(
                value switch
                {
                    PineValue.ListValue listValue =>
                    listValue.Items.Length,

                    PineValue.BlobValue blobValue =>
                    blobValue.Bytes.Length,

                    _ =>
                    throw new NotImplementedException(
                        "Unexpected value type: " + value.GetType().FullName)
                });
    }

    /// <summary>
    /// For a list or blob, returns a new sequence with the first 'count' elements/bytes removed.
    /// The argument is a list of two elements: [count, sequence].
    /// </summary>
    /// <param name="value">A list containing the count and the sequence (list or blob).</param>
    /// <returns>A new sequence with the specified number of elements/bytes removed from the beginning, or an empty list on error.</returns>
    public static PineValue skip(PineValue value)
    {
        if (value is not PineValue.ListValue listValue)
        {
            return PineValue.EmptyList;
        }

        if (listValue.Items.Length is not 2)
        {
            return PineValue.EmptyList;
        }

        if (SignedIntegerFromValueRelaxed(listValue.Items.Span[0]) is not { } count)
        {
            return PineValue.EmptyList;
        }

        return Internal.KernelFunctionSpecialized.skip(count, listValue.Items.Span[1]);
    }

    /// <summary>
    /// For a list or blob, returns a new sequence with the first 'count' elements/bytes.
    /// The argument is a list of two elements: [count, sequence].
    /// </summary>
    /// <param name="value">A list containing the count and the sequence (list or blob).</param>
    /// <returns>A new sequence with the first 'count' elements/bytes, or an empty list on error.</returns>
    public static PineValue take(PineValue value) =>
        value switch
        {
            PineValue.ListValue listValue =>
            listValue.Items.Length is 2 ?
            SignedIntegerFromValueRelaxed(listValue.Items.Span[0]) switch
            {
                { } count =>
                Internal.KernelFunctionSpecialized.take(count, listValue.Items.Span[1]),

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

    /// <summary>
    /// For a list, returns a new list with the elements in reverse order.
    /// For a blob, returns a new blob with the bytes in reverse order.
    /// </summary>
    /// <param name="value">The input value, which can be a list or a blob.</param>
    /// <returns>A new sequence with the elements/bytes in reverse order.</returns>
    public static PineValue reverse(PineValue value)
    {
        if (value is PineValue.ListValue listValue)
        {
            if (listValue.Items.Length <= 1)
                return value;

            var reversed = listValue.Items.ToArray();

            Array.Reverse(reversed);

            return PineValue.List(reversed);
        }

        if (value is PineValue.BlobValue blobValue)
        {
            if (blobValue.Bytes.Length <= 1)
                return value;

            if (blobValue.Bytes.Length is 2)
            {
                return PineValue.ReusedBlobTupleFromBytes(
                    blobValue.Bytes.Span[1],
                    blobValue.Bytes.Span[0]);
            }

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

        return Internal.KernelFunctionSpecialized.concat(listValue.Items.Span);
    }

    public static PineValue head(PineValue value)
    {
        if (value is PineValue.ListValue listValue)
        {
            if (listValue.Items.Length is 0)
            {
                return PineValue.EmptyList;
            }

            return listValue.Items.Span[0];
        }

        if (value is PineValue.BlobValue blobValue)
        {
            if (blobValue.Bytes.Length is 0)
            {
                return PineValue.EmptyBlob;
            }

            return PineValue.BlobSingleByte(blobValue.Bytes.Span[0]);
        }

        throw new NotImplementedException(
            "Unexpected value type: " + value.GetType().FullName);
    }

    public static PineValue int_add(PineValue value) =>
        KernelFunctionExpectingListOfBigIntAndProducingBigInt(
            integers =>
            integers.Aggregate(seed: BigInteger.Zero, func: (aggregate, next) => aggregate + next),
            value);

    public static PineValue int_mul(PineValue value) =>
        KernelFunctionExpectingListOfBigIntAndProducingBigInt(
            integers =>
            integers.Aggregate(seed: BigInteger.One, func: (aggregate, next) => aggregate * next),
            value);

    public static PineValue int_is_sorted_asc(PineValue value)
    {
        if (value is PineValue.ListValue listValue)
        {
            var listItems = listValue.Items.Span;

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

        if (value is PineValue.BlobValue)
        {
            return PineValue.EmptyList;
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

        var argumentsItems = argumentsList.Items.Span;

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

            merged = Internal.KernelFunctionSpecialized.bit_and(merged, blobValue);
        }

        return merged;
    }

    public static PineValue bit_or(PineValue value)
    {
        if (value is not PineValue.ListValue argumentsList)
        {
            return PineValue.EmptyList;
        }

        var argumentsItems = argumentsList.Items.Span;

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

            merged = Internal.KernelFunctionSpecialized.bit_or(merged, blobValue);
        }

        return merged;
    }

    public static PineValue bit_xor(PineValue value)
    {
        if (value is not PineValue.ListValue argumentsList)
        {
            return PineValue.EmptyList;
        }

        var argumentsItems = argumentsList.Items.Span;

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

            merged = Internal.KernelFunctionSpecialized.bit_xor(merged, blobValue);
        }

        return merged;
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

    /// <summary>
    /// Expects a list with exactly two elements: [shiftCount, blob].
    /// If the input does not match this structure, returns an empty list.
    /// Applies a left bitwise shift to the blob by the specified count.
    /// The resulting blob has the same length as the input blob,
    /// with zeros filling one end and bits falling off the other end.
    /// </summary>
    public static PineValue bit_shift_left(PineValue value)
    {
        if (value is not PineValue.ListValue argumentsList)
        {
            return PineValue.EmptyList;
        }

        var argumentsItems = argumentsList.Items.Span;

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

        return Internal.KernelFunctionSpecialized.bit_shift_left(shiftCount, blobValue);
    }

    /// <summary>
    /// Expects a list with exactly two elements: [shiftCount, blob].
    /// If the input does not match this structure, returns an empty list.
    /// Applies a right bitwise shift to the blob by the specified count.
    /// The resulting blob has the same length as the input blob,
    /// with zeros filling one end and bits falling off the other end.
    /// </summary>
    public static PineValue bit_shift_right(PineValue value)
    {
        if (value is not PineValue.ListValue argumentsList)
        {
            return PineValue.EmptyList;
        }

        var argumentsItems = argumentsList.Items.Span;

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

        return Internal.KernelFunctionSpecialized.bit_shift_right(shiftCount, blobValue);
    }

    private static bool BlobAllBytesEqual(ReadOnlyMemory<byte> readOnlyMemory) =>
        readOnlyMemory.IsEmpty ||
        !readOnlyMemory.Span.ContainsAnyExcept(readOnlyMemory.Span[0]);

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

    /// <summary>
    /// Same decoding of relaxed integer as in <see cref="IntegerEncoding"/>, but without the detailed error reporting.
    /// </summary>
    public static BigInteger? SignedIntegerFromValueRelaxed(PineValue value)
    {
        if (value is not PineValue.BlobValue blobValue)
            return null;

        var blobBytes = blobValue.Bytes;

        if (blobBytes.Length is < 2)
            return null;

        var signByte = blobBytes.Span[0];

        if (signByte is not 2 and not 4)
            return null;

        var firstValueByteIndex = 1;

        var valueBytesCount =
            blobBytes.Length - firstValueByteIndex;

        if (valueBytesCount is < 1)
            return null;

        var abs =
            valueBytesCount is 1
            ?
            (BigInteger)blobBytes.Span[1]
            :
            valueBytesCount is 2
            ?
            new BigInteger(
                blobBytes.Span[1] << 8 |
                blobBytes.Span[2])
            :
            valueBytesCount is 3
            ?
            new BigInteger(
                blobBytes.Span[1] << 16 |
                blobBytes.Span[2] << 8 |
                blobBytes.Span[3])
            :
            valueBytesCount is 4
            ?
            new BigInteger(
                (uint)
                (blobBytes.Span[1] << 24 |
                blobBytes.Span[2] << 16 |
                blobBytes.Span[3] << 8 |
                blobBytes.Span[4]))
            :
            new BigInteger(blobBytes.Span[firstValueByteIndex..], isUnsigned: true, isBigEndian: true);

        if (signByte is 2)
        {
            return -abs;
        }

        return abs;
    }

    /// <summary>
    /// Same decoding of relaxed integer as in <see cref="IntegerEncoding"/>, but without the detailed error reporting.
    /// </summary>
    public static BigInteger? UnsignedIntegerFromValueRelaxed(PineValue pineValue)
    {
        if (pineValue is not PineValue.BlobValue blobValue)
            return null;

        var blobBytes = blobValue.Bytes.Span;

        if (blobBytes.Length is 0)
            return null;

        if (blobBytes.Length is 1)
            return (BigInteger)blobBytes[0];

        if (blobBytes.Length is 2)
            return new BigInteger(
                blobBytes[0] << 8 |
                blobBytes[1]);

        if (blobBytes.Length is 3)
            return new BigInteger(
                blobBytes[0] << 16 |
                blobBytes[1] << 8 |
                blobBytes[2]);

        if (blobBytes.Length is 4)
        {
            return new BigInteger(
                (uint)
                (blobBytes[0] << 24 |
                blobBytes[1] << 16 |
                blobBytes[2] << 8 |
                blobBytes[3]));
        }

        return
            new BigInteger(blobBytes, isUnsigned: true, isBigEndian: true);
    }

    private static PineValue KernelFunctionExpectingList(
        PineValue value,
        Func<ReadOnlyMemory<PineValue>, PineValue> continueWithList) =>
        value switch
        {
            PineValue.ListValue list =>
            continueWithList(list.Items),

            _ =>
            PineValue.EmptyList
        };

    /// <summary>
    /// Value representing 'true' or 'false' as returned by Pine kernel functions.
    /// </summary>
    public static PineValue ValueFromBool(bool b) =>
        b ?
        PineKernelValues.TrueValue :
        PineKernelValues.FalseValue;
}
