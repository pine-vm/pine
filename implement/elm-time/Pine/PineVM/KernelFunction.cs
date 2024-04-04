using System;
using System.Collections.Generic;
using System.Linq;
using System.Numerics;

namespace Pine.PineVM;

#pragma warning disable IDE1006

public static class KernelFunction
{
    public static PineValue equal(PineValue value) =>
        PineVM.ValueFromBool(
            value switch
            {
                PineValue.ListValue list =>
                list.Elements switch
                {
                [] => true,

                [var first, ..] =>
                /*
                    * :: rest pattern seems not implemented yet:
                    * https://github.com/dotnet/csharplang/issues/6574
                    * */
                list.Elements.All(e => e.Equals(first)),
                },

                PineValue.BlobValue blob =>
                BlobAllBytesEqual(blob.Bytes),

                _ =>
                throw new NotImplementedException()
            }
        );

    private static bool BlobAllBytesEqual(ReadOnlyMemory<byte> readOnlyMemory) =>
        readOnlyMemory.IsEmpty ||
        !readOnlyMemory.Span.ContainsAnyExcept(readOnlyMemory.Span[0]);

    public static PineValue equal(PineValue valueA, PineValue valueB) =>
        PineVM.ValueFromBool(valueA.Equals(valueB));

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
        PineValueAsInteger.ValueFromSignedInteger(
            value switch
            {
                PineValue.BlobValue blobValue => blobValue.Bytes.Length,
                PineValue.ListValue listValue => listValue.Elements.Count,
                _ => throw new NotImplementedException()
            });

    public static PineValue skip(PineValue value) =>
        KernelFunctionExpectingExactlyTwoArguments(
            PineValueAsInteger.SignedIntegerFromValueRelaxed,
            Result<string, PineValue>.ok,
            compose: skip,
            value);

    public static PineValue skip(BigInteger count, PineValue value) =>
        value switch
        {
            PineValue.BlobValue blobValue =>
            blobValue.Bytes.Length <= count ?
            PineValue.EmptyBlob
            :
            PineValue.Blob(blobValue.Bytes[(int)count..]),

            PineValue.ListValue listValue =>
            listValue.Elements.Count <= count ?
            PineValue.EmptyList
            :
            PineValue.List([.. listValue.Elements.Skip((int)count)]),

            _ =>
            throw new NotImplementedException()
        };

    public static PineValue take(PineValue value) =>
        KernelFunctionExpectingExactlyTwoArguments(
            PineValueAsInteger.SignedIntegerFromValueRelaxed,
            Result<string, PineValue>.ok,
            compose: take,
            value);

    public static PineValue take(BigInteger count, PineValue value) =>
        value switch
        {
            PineValue.BlobValue blobValue =>
            blobValue.Bytes.Length <= count ?
            value
            :
            PineValue.Blob(blobValue.Bytes[..(int)count]),

            PineValue.ListValue listValue =>
            listValue.Elements.Count <= count ?
            value
            :
            PineValue.List([.. listValue.Elements.Take((int)count)]),

            _ =>
            throw new NotImplementedException()
        };

    public static PineValue reverse(PineValue value) =>
        value switch
        {
            PineValue.BlobValue blobValue => PineValue.Blob([.. blobValue.Bytes.ToArray().Reverse()]),
            PineValue.ListValue listValue => PineValue.List([.. listValue.Elements.Reverse()]),
            _ => throw new NotImplementedException()
        };

    public static PineValue concat(PineValue value) =>
        KernelFunctionExpectingList(
            value,
            list =>
            list switch
            {
            [] =>
            PineValue.EmptyList,

            [var head, ..] =>
                head switch
                {
                    PineValue.BlobValue =>
                    PineValue.Blob(CommonConversion.Concat([.. list.OfType<PineValue.BlobValue>().Select(b => b.Bytes)])),

                    PineValue.ListValue =>
                    PineValue.List([.. list.OfType<PineValue.ListValue>().SelectMany(l => l.Elements)]),

                    _ =>
                    throw new NotImplementedException()
                }
            });

    public static PineValue list_head(PineValue value) =>
        value switch
        {
            PineValue.ListValue listValue =>
            listValue.Elements switch
            {
            [] => PineValue.EmptyList,
            [var head, ..] => head,
            },

            _ =>
            PineValue.EmptyList
        };

    public static PineValue add_int(PineValue value) =>
        KernelFunctionExpectingListOfBigIntAndProducingBigInt(
            integers =>
            integers.Aggregate(seed: BigInteger.Zero, func: (aggregate, next) => aggregate + next),
            value);

    public static PineValue add_int(PineValue summandA, PineValue summandB)
    {
        if (PineValueAsInteger.SignedIntegerFromValueRelaxed(summandA) is not Result<string, BigInteger>.Ok intA)
            return PineValue.EmptyList;

        if (PineValueAsInteger.SignedIntegerFromValueRelaxed(summandB) is not Result<string, BigInteger>.Ok intB)
            return PineValue.EmptyList;

        return add_int(intA.Value, intB.Value);
    }

    public static PineValue add_int(BigInteger summandA, BigInteger summandB) =>
        PineValueAsInteger.ValueFromSignedInteger(summandA + summandB);

    public static PineValue mul_int(PineValue value) =>
        KernelFunctionExpectingListOfBigIntAndProducingBigInt(
            integers =>
            integers.Aggregate(seed: BigInteger.One, func: (aggregate, next) => aggregate * next),
            value);

    public static PineValue mul_int(PineValue factorA, PineValue factorB)
    {
        if (PineValueAsInteger.SignedIntegerFromValueRelaxed(factorA) is not Result<string, BigInteger>.Ok intA)
            return PineValue.EmptyList;

        if (PineValueAsInteger.SignedIntegerFromValueRelaxed(factorB) is not Result<string, BigInteger>.Ok intB)
            return PineValue.EmptyList;

        return mul_int(intA.Value, intB.Value);
    }

    public static PineValue mul_int(BigInteger factorA, BigInteger factorB) =>
        PineValueAsInteger.ValueFromSignedInteger(factorA * factorB);

    public static PineValue is_sorted_ascending_int(PineValue value) =>
        PineVM.ValueFromBool(sort_int(value) == value);

    public static PineValue sort_int(PineValue value) =>
        value switch
        {
            PineValue.ListValue list =>
                PineValue.List(
                    [.. list.Elements
                        .Select(sort_int)
                        .Order(valueComparerInt)
                        ]),

            _ => value,
        };

    private static readonly BlobValueIntComparer valueComparerInt = new();

    private class BlobValueIntComparer : IComparer<PineValue>
    {
        public int Compare(PineValue? x, PineValue? y) =>
            (x, y) switch
            {
                (PineValue.BlobValue blobX, PineValue.BlobValue blobY) =>
                (PineValueAsInteger.SignedIntegerFromBlobValueRelaxed(blobX.Bytes.Span),
                PineValueAsInteger.SignedIntegerFromBlobValueRelaxed(blobY.Bytes.Span)) switch
                {
                    (Result<string, BigInteger>.Ok intX, Result<string, BigInteger>.Ok intY) =>
                    BigInteger.Compare(intX.Value, intY.Value),

                    (Result<string, BigInteger>.Ok _, _) =>
                    -1,

                    (_, Result<string, BigInteger>.Ok _) =>
                    1,

                    _ => 0
                },

                (PineValue.ListValue listX, PineValue.ListValue listY) =>
                listX.Elements.Count - listY.Elements.Count,

                (PineValue.ListValue _, _) => -1,

                (_, PineValue.ListValue _) => 1,

                _ => 0
            };
    }

    private static PineValue KernelFunctionExpectingListOfBigIntAndProducingBigInt(
        Func<IReadOnlyList<BigInteger>, BigInteger> aggregate,
        PineValue value) =>
        KernelFunctionExpectingListOfBigInt(
            aggregate:
            listOfIntegers => PineValueAsInteger.ValueFromSignedInteger(aggregate(listOfIntegers)),
            value);

    private static PineValue KernelFunctionExpectingListOfBigInt(
        Func<IReadOnlyList<BigInteger>, PineValue> aggregate,
        PineValue value) =>
        KernelFunctionExpectingList(
            value,
            list =>
            PineVM.ResultListMapCombine(list, PineValueAsInteger.SignedIntegerFromValueRelaxed)
            .Map(ints => aggregate(ints))
            .WithDefault(PineValue.EmptyList));

    private static PineValue KernelFunctionExpectingExactlyTwoArguments<ArgA, ArgB>(
        Func<PineValue, Result<string, ArgA>> decodeArgA,
        Func<PineValue, Result<string, ArgB>> decodeArgB,
        Func<ArgA, ArgB, PineValue> compose,
        PineValue value) =>
        KernelFunctionExpectingList(
            value,
            list =>
            list switch
            {
            [var first, var second] =>
            decodeArgA(first)
            .AndThen(argA =>
            decodeArgB(second)
            .Map(argB => compose(argA, argB)))
            .WithDefault(PineValue.EmptyList),

                _ =>
                PineValue.EmptyList
            });

    private static PineValue KernelFunctionExpectingListOfTypeBool(
        Func<IReadOnlyList<bool>, bool> compose,
        PineValue value) =>
        KernelFunctionExpectingList(
            value,
            list => PineVM.ResultListMapCombine(list, PineVM.ParseBoolFromValue)
            .Map(compose)
            .Map(PineVM.ValueFromBool)
            .WithDefault(PineValue.EmptyList));


    public static PineValue KernelFunctionExpectingList(
        PineValue value,
        Func<IReadOnlyList<PineValue>, PineValue> continueWithList) =>
        value switch
        {
            PineValue.ListValue list =>
            continueWithList(list.Elements),

            _ =>
            PineValue.EmptyList
        };
}