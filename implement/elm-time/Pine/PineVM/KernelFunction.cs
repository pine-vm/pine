using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Numerics;

namespace Pine.PineVM;

static public class KernelFunction
{
    static public Result<string, PineValue> equal(PineValue value) =>
        Result<string, PineValue>.ok(
            PineVM.ValueFromBool(
                value switch
                {
                    PineValue.ListValue list =>
                        list.Elements.Count < 1 ?
                            true
                            :
                            list.Elements.All(e => e.Equals(list.Elements[0])),

                    PineValue.BlobValue blob =>
                        blob.Bytes.Length < 1 ? true :
                            blob.Bytes.ToArray().All(b => b == blob.Bytes.Span[0]),

                    _ => throw new NotImplementedException()
                }
            ));

    static public Result<string, PineValue> logical_not(PineValue value) =>
        PineVM.DecodeBoolFromValue(value)
            .Map(b => PineVM.ValueFromBool(!b));

    static public Result<string, PineValue> logical_and(PineValue value) =>
        KernelFunctionExpectingListOfTypeBool(bools => bools.Aggregate(seed: true, func: (a, b) => a && b), value);

    static public Result<string, PineValue> logical_or(PineValue value) =>
        KernelFunctionExpectingListOfTypeBool(bools => bools.Aggregate(seed: false, func: (a, b) => a || b), value);

    static public Result<string, PineValue> length(PineValue value) =>
        Result<string, PineValue>.ok(
            PineValueAsInteger.ValueFromSignedInteger(
                value switch
                {
                    PineValue.BlobValue blobComponent => blobComponent.Bytes.Length,
                    PineValue.ListValue listComponent => listComponent.Elements.Count,
                    _ => throw new NotImplementedException()
                }));

    static public Result<string, PineValue> skip(PineValue value) =>
        KernelFunctionExpectingExactlyTwoArguments(
                PineValueAsInteger.SignedIntegerFromValue,
                Result<string, PineValue>.ok,
                compose: skip)
            (value);

    static public Result<string, PineValue> skip(BigInteger count, PineValue value) =>
        Result<string, PineValue>.ok(
            value switch
            {
                PineValue.BlobValue blobComponent => PineValue.Blob(blobComponent.Bytes[(int)count..]),
                PineValue.ListValue listComponent => PineValue.List(listComponent.Elements.Skip((int)count).ToImmutableList()),
                _ => throw new NotImplementedException()
            });

    static public Result<string, PineValue> take(PineValue value) =>
        KernelFunctionExpectingExactlyTwoArguments(
                PineValueAsInteger.SignedIntegerFromValue,
                Result<string, PineValue>.ok,
                compose: take)
            (value);

    static public Result<string, PineValue> take(BigInteger count, PineValue value) =>
        Result<string, PineValue>.ok(
            value switch
            {
                PineValue.BlobValue blobComponent => PineValue.Blob(blobComponent.Bytes[..(int)count]),
                PineValue.ListValue listComponent => PineValue.List(listComponent.Elements
                    .Take((int)count).ToImmutableList()),
                _ => throw new NotImplementedException()
            });

    static public Result<string, PineValue> reverse(PineValue value) =>
        Result<string, PineValue>.ok(
            value switch
            {
                PineValue.BlobValue blobComponent => PineValue.Blob(blobComponent.Bytes.ToArray().Reverse().ToArray()),
                PineValue.ListValue listComponent => PineValue.List(listComponent.Elements.Reverse().ToImmutableList()),
                _ => throw new NotImplementedException()
            });

    static public Result<string, PineValue> concat(PineValue value) =>
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
                                        PineValue.List(aggregateListValue.Elements.Concat(elemListValue.Elements).ToImmutableList()),
                                    _ => elemListValue
                                },
                            _ => throw new NotImplementedException()
                        }));

    static public Result<string, PineValue> list_head(PineValue value) =>
        PineVM.DecodePineListValue(value)
            .Map(list => list.Count < 1 ? PineValue.EmptyList : list[0]);

    static public Result<string, PineValue> neg_int(PineValue value) =>
        PineValueAsInteger.SignedIntegerFromValue(value)
            .Map(i => PineValueAsInteger.ValueFromSignedInteger(-i));

    static public Result<string, PineValue> add_int(PineValue value) =>
        KernelFunctionExpectingListOfBigIntWithAtLeastOneAndProducingBigInt(
            (firstInt, otherInts) => Result<string, BigInteger>.ok(
                otherInts.Aggregate(seed: firstInt, func: (aggregate, next) => aggregate + next)),
            value);

    static public Result<string, PineValue> sub_int(PineValue value) =>
        KernelFunctionExpectingListOfBigIntWithAtLeastOneAndProducingBigInt(
            (firstInt, otherInts) => Result<string, BigInteger>.ok(
                otherInts.Aggregate(seed: firstInt, func: (aggregate, next) => aggregate - next)),
            value);

    static public Result<string, PineValue> mul_int(PineValue value) =>
        KernelFunctionExpectingListOfBigIntWithAtLeastOneAndProducingBigInt(
            (firstInt, otherInts) => Result<string, BigInteger>.ok(
                otherInts.Aggregate(seed: firstInt, func: (aggregate, next) => aggregate * next)),
            value);

    static public Result<string, PineValue> div_int(PineValue value) =>
        KernelFunctionExpectingListOfBigIntWithAtLeastOneAndProducingBigInt(
            (firstInt, otherInts) =>
                otherInts.Contains(0) ?
                    Result<string, BigInteger>.err("Division by zero")
                    :
                    Result<string, BigInteger>.ok(otherInts.Aggregate(seed: firstInt, func: (aggregate, next) => aggregate / next)),
            value);

    static public PineValue is_sorted_ascending_int(PineValue value) =>
        PineVM.ValueFromBool(sort_int(value) == value);

    static public PineValue sort_int(PineValue value) =>
        value switch
        {
            PineValue.ListValue list =>
                new PineValue.ListValue(
                    list.Elements
                        .Select(sort_int)
                        .Order(valueComparerInt)
                        .ToImmutableList()),

            _ => value,
        };

    static readonly BlobValueIntComparer valueComparerInt = new();

    class BlobValueIntComparer : IComparer<PineValue>
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

    static Result<string, PineValue> KernelFunctionExpectingListOfBigIntWithAtLeastOneAndProducingBigInt(
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

    static Result<string, PineValue> KernelFunctionExpectingListOfBigInt(
        Func<IReadOnlyList<BigInteger>, Result<string, PineValue>> aggregate,
        PineValue value) =>
        PineVM.DecodePineListValue(value)
            .AndThen(list => PineVM.ResultListMapCombine(list, PineValueAsInteger.SignedIntegerFromValue))
            .AndThen(ints => aggregate(ints));

    static Func<PineValue, Result<string, PineValue>> KernelFunctionExpectingExactlyTwoArguments<ArgA, ArgB>(
        Func<PineValue, Result<string, ArgA>> decodeArgA,
        Func<PineValue, Result<string, ArgB>> decodeArgB,
        Func<ArgA, ArgB, Result<string, PineValue>> compose) =>
        value => PineVM.DecodePineListValue(value)
            .AndThen(PineVM.DecodeListWithExactlyTwoElements)
            .AndThen(argsValues =>
                decodeArgA(argsValues.Item1)
                    .AndThen(argA =>
                        decodeArgB(argsValues.Item2)
                            .AndThen(argB => compose(argA, argB))));

    static Result<string, PineValue> KernelFunctionExpectingListOfTypeBool(
        Func<IReadOnlyList<bool>, bool> compose,
        PineValue value) =>
        PineVM.DecodePineListValue(value)
            .AndThen(list => PineVM.ResultListMapCombine(list, PineVM.DecodeBoolFromValue))
            .Map(compose)
            .Map(PineVM.ValueFromBool);
}