using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Numerics;

namespace Pine.PineVM;

public static class KernelFunction
{
    public static Result<string, PineValue> equal(PineValue value) =>
        Result<string, PineValue>.ok(
            PineVM.ValueFromBool(
                value switch
                {
                    PineValue.ListValue list =>
                    list.Elements switch
                    {
                        [] => true,

                        [var first, ..] =>
                        /*
                         * ::rest pattern seems not implemented yet:
                         * https://github.com/dotnet/csharplang/issues/6574
                         * */
                        list.Elements.All(e => e.Equals(first)),
                    },

                    PineValue.BlobValue blob =>
                    BlobAllBytesEqual(blob.Bytes),

                    _ =>
                    throw new NotImplementedException()
                }
            ));

    private static bool BlobAllBytesEqual(ReadOnlyMemory<byte> readOnlyMemory) =>
        readOnlyMemory.IsEmpty ||
        !readOnlyMemory.Span.ContainsAnyExcept(readOnlyMemory.Span[0]);

    public static Result<string, PineValue> equal(PineValue valueA, PineValue valueB) =>
        Result<string, PineValue>.ok(
            PineVM.ValueFromBool(valueA.Equals(valueB)));

    public static Result<string, PineValue> negate(PineValue value) =>
        Result<string, PineValue>.ok(
            value switch
            {
                PineValue.BlobValue blobValue
                when 0 < blobValue.Bytes.Length =>
                blobValue.Bytes.Span[0] switch
                {
                    4 =>
                    PineValue.Blob(CommonConversion.Concat((ReadOnlySpan<byte>)[2], blobValue.Bytes.Span[1..])),

                    2 =>
                    PineValue.Blob(CommonConversion.Concat((ReadOnlySpan<byte>)[4], blobValue.Bytes.Span[1..])),

                    _ =>
                    PineValue.EmptyList
                },

                _ =>
                PineValue.EmptyList
            });

    public static Result<string, PineValue> logical_and(PineValue value) =>
        KernelFunctionExpectingListOfTypeBool(bools => bools.Aggregate(seed: true, func: (a, b) => a && b), value);

    public static Result<string, PineValue> logical_or(PineValue value) =>
        KernelFunctionExpectingListOfTypeBool(bools => bools.Aggregate(seed: false, func: (a, b) => a || b), value);

    public static Result<string, PineValue> length(PineValue value) =>
        Result<string, PineValue>.ok(
            PineValueAsInteger.ValueFromSignedInteger(
                value switch
                {
                    PineValue.BlobValue blobValue => blobValue.Bytes.Length,
                    PineValue.ListValue listValue => listValue.Elements.Count,
                    _ => throw new NotImplementedException()
                }));

    public static Result<string, PineValue> skip(PineValue value) =>
        KernelFunctionExpectingExactlyTwoArguments(
                PineValueAsInteger.SignedIntegerFromValue,
                Result<string, PineValue>.ok,
                compose: skip)
            (value);

    public static PineValue skip(BigInteger count, PineValue value) =>
        value switch
        {
            PineValue.BlobValue blobValue => PineValue.Blob(blobValue.Bytes[(int)count..]),
            PineValue.ListValue listValue => PineValue.List([.. listValue.Elements.Skip((int)count)]),
            _ => throw new NotImplementedException()
        };

    public static Result<string, PineValue> take(PineValue value) =>
        KernelFunctionExpectingExactlyTwoArguments(
                PineValueAsInteger.SignedIntegerFromValue,
                Result<string, PineValue>.ok,
                compose: take)
            (value);

    public static PineValue take(BigInteger count, PineValue value) =>
        value switch
        {
            PineValue.BlobValue blobValue => PineValue.Blob(blobValue.Bytes[..(int)count]),
            PineValue.ListValue listValue => PineValue.List([.. listValue.Elements.Take((int)count)]),
            _ => throw new NotImplementedException()
        };

    public static Result<string, PineValue> reverse(PineValue value) =>
        Result<string, PineValue>.ok(
            value switch
            {
                PineValue.BlobValue blobValue => PineValue.Blob(blobValue.Bytes.ToArray().Reverse().ToArray()),
                PineValue.ListValue listValue => PineValue.List([.. listValue.Elements.Reverse()]),
                _ => throw new NotImplementedException()
            });

    public static Result<string, PineValue> concat(PineValue value) =>
        PineVM.DecodePineListValue(value)
            .Map(list =>
                list.Aggregate(
                    seed: PineValue.EmptyList,
                    func: (aggregate, elem) =>
                        elem switch
                        {
                            PineValue.BlobValue elemBlobValue =>
                                aggregate switch
                                {
                                    PineValue.BlobValue aggregateBlobValue =>
                                        PineValue.Blob(CommonConversion.Concat(
                                            aggregateBlobValue.Bytes.Span, elemBlobValue.Bytes.Span)),
                                    _ => elemBlobValue
                                },
                            PineValue.ListValue elemListValue =>
                                aggregate switch
                                {
                                    PineValue.ListValue aggregateListValue =>
                                        PineValue.List([.. aggregateListValue.Elements, .. elemListValue.Elements]),
                                    _ => elemListValue
                                },
                            _ => throw new NotImplementedException()
                        }));

    public static Result<string, PineValue> list_head(PineValue value) =>
        PineVM.DecodePineListValue(value)
            .Map(list => list.Count < 1 ? PineValue.EmptyList : list[0]);

    public static Result<string, PineValue> add_int(PineValue value) =>
        KernelFunctionExpectingListOfBigIntWithAtLeastOneAndProducingBigInt(
            (firstInt, otherInts) => Result<string, BigInteger>.ok(
                otherInts.Aggregate(seed: firstInt, func: (aggregate, next) => aggregate + next)),
            value);

    public static Result<string, PineValue> sub_int(PineValue value) =>
        KernelFunctionExpectingListOfBigIntWithAtLeastOneAndProducingBigInt(
            (firstInt, otherInts) => Result<string, BigInteger>.ok(
                otherInts.Aggregate(seed: firstInt, func: (aggregate, next) => aggregate - next)),
            value);

    public static Result<string, PineValue> mul_int(PineValue value) =>
        KernelFunctionExpectingListOfBigIntWithAtLeastOneAndProducingBigInt(
            (firstInt, otherInts) => Result<string, BigInteger>.ok(
                otherInts.Aggregate(seed: firstInt, func: (aggregate, next) => aggregate * next)),
            value);

    public static Result<string, PineValue> div_int(PineValue value) =>
        KernelFunctionExpectingListOfBigIntWithAtLeastOneAndProducingBigInt(
            (firstInt, otherInts) =>
                otherInts.Contains(0) ?
                    Result<string, BigInteger>.err("Division by zero")
                    :
                    Result<string, BigInteger>.ok(otherInts.Aggregate(seed: firstInt, func: (aggregate, next) => aggregate / next)),
            value);

    public static Result<string, PineValue> is_sorted_ascending_int(PineValue value) =>
        Result<string, PineValue>.ok(PineVM.ValueFromBool(sort_int(value) == value));

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
                    (PineValueAsInteger.SignedIntegerFromBlobValue(blobX.Bytes.Span),
                            PineValueAsInteger.SignedIntegerFromBlobValue(blobY.Bytes.Span)) switch
                    {
                        (Result<string, BigInteger>.Ok intX, Result<string, BigInteger>.Ok intY) =>
                            BigInteger.Compare(intX.Value, intY.Value),

                        (Result<string, BigInteger>.Ok _, _) => -1,
                        (_, Result<string, BigInteger>.Ok _) => 1,
                        _ => 0
                    },

                (PineValue.ListValue listX, PineValue.ListValue listY) =>
                    listX.Elements.Count - listY.Elements.Count,

                (PineValue.ListValue _, _) => -1,

                (_, PineValue.ListValue _) => 1,

                _ => 0
            };
    }

    private static Result<string, PineValue> KernelFunctionExpectingListOfBigIntWithAtLeastOneAndProducingBigInt(
        Func<BigInteger, IReadOnlyList<BigInteger>, Result<string, BigInteger>> aggregate,
        PineValue value) =>
        KernelFunctionExpectingListOfBigInt(
            aggregate:
            listOfIntegers =>
                (listOfIntegers.Count < 1
                    ?
                    Result<string, BigInteger>.err("List is empty. Expected at least one element")
                    :
                    aggregate(listOfIntegers[0], listOfIntegers.Skip(1).ToImmutableArray()))
                .Map(PineValueAsInteger.ValueFromSignedInteger),
            value);

    private static Result<string, PineValue> KernelFunctionExpectingListOfBigInt(
        Func<IReadOnlyList<BigInteger>, Result<string, PineValue>> aggregate,
        PineValue value) =>
        PineVM.DecodePineListValue(value)
            .AndThen(list => PineVM.ResultListMapCombine(list, PineValueAsInteger.SignedIntegerFromValue))
            .AndThen(ints => aggregate(ints));

    private static Func<PineValue, Result<string, PineValue>> KernelFunctionExpectingExactlyTwoArguments<ArgA, ArgB>(
        Func<PineValue, Result<string, ArgA>> decodeArgA,
        Func<PineValue, Result<string, ArgB>> decodeArgB,
        Func<ArgA, ArgB, PineValue> compose) =>
        value => PineVM.DecodePineListValue(value)
            .AndThen(PineVM.DecodeListWithExactlyTwoElements)
            .AndThen(argsValues =>
                decodeArgA(argsValues.Item1)
                    .AndThen(argA =>
                        decodeArgB(argsValues.Item2)
                            .Map(argB => compose(argA, argB))));

    private static Result<string, PineValue> KernelFunctionExpectingListOfTypeBool(
        Func<IReadOnlyList<bool>, bool> compose,
        PineValue value) =>
        PineVM.DecodePineListValue(value)
            .AndThen(list => PineVM.ResultListMapCombine(list, PineVM.DecodeBoolFromValue))
            .Map(compose)
            .Map(PineVM.ValueFromBool);
}