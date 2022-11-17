using System;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Numerics;

namespace Pine;

public class PineVM
{
    readonly ConcurrentDictionary<(PineValue, PineValue), PineValue> FunctionApplicationCache = new();

    public long FunctionApplicationCacheSize => FunctionApplicationCache.Count;

    public long FunctionApplicationCacheLookupCount { private set; get; }

    public long FunctionApplicationMaxEnvSize { private set; get; }

    public Result<string, PineValue> EvaluateExpression(
        PineValue environment,
        Expression expression)
    {
        if (expression is Expression.LiteralExpression literalExpression)
            return Result<string, PineValue>.ok(literalExpression.Value);

        if (expression is Expression.ListExpression listExpression)
        {
            return
                ResultListMapCombine(
                    listExpression.List,
                    elem => EvaluateExpression(environment, elem))
                .MapError(err => "Failed to evaluate list element: " + err)
                .Map(PineValue.List);
        }

        if (expression is Expression.DecodeAndEvaluateExpression applicationExpression)
        {
            return
                EvaluateDecodeAndEvaluateExpression(environment, applicationExpression)
                .MapError(err => "Failed to evaluate function application: " + err);
        }

        if (expression is Expression.KernelApplicationExpression kernelApplicationExpression)
        {
            return
                EvaluateKernelApplicationExpression(environment, kernelApplicationExpression)
                .MapError(err => "Failed to evaluate kernel function application: " + err);
        }

        if (expression is Expression.ConditionalExpression conditionalExpression)
        {
            return EvaluateConditionalExpression(environment, conditionalExpression);
        }

        if (expression is Expression.EnvironmentExpression)
        {
            return Result<string, PineValue>.ok(environment);
        }

        if (expression is Expression.StringTagExpression stringTagExpression)
        {
            return EvaluateExpression(environment, stringTagExpression.tagged);
        }

        throw new NotImplementedException("Unexpected shape of expression");
    }

    public Result<string, PineValue> EvaluateDecodeAndEvaluateExpression(
        PineValue environment,
        Expression.DecodeAndEvaluateExpression decodeAndEvaluate) =>
        EvaluateExpression(environment, decodeAndEvaluate.expression)
        .MapError(error => "Failed to evaluate function: " + error)
        .AndThen(functionValue => DecodeExpressionFromValue(functionValue)
        .MapError(error => "Failed to decode expression from function value: " + error)
        .AndThen(functionExpression => EvaluateExpression(environment, decodeAndEvaluate.environment)
        .MapError(error => "Failed to evaluate argument: " + error)
        .AndThen(argumentValue =>
        {
            ++FunctionApplicationCacheLookupCount;

            if (FunctionApplicationCache.TryGetValue((functionValue, argumentValue), out var cachedResult))
                return Result<string, PineValue>.ok(cachedResult);

            if (argumentValue is PineValue.ListValue list)
            {
                FunctionApplicationMaxEnvSize =
                FunctionApplicationMaxEnvSize < list.Elements.Count ? list.Elements.Count : FunctionApplicationMaxEnvSize;
            }

            var evalStopwatch = System.Diagnostics.Stopwatch.StartNew();

            var evalResult = EvaluateExpression(environment: argumentValue, expression: functionExpression);

            if (4 <= evalStopwatch.ElapsedMilliseconds && evalResult is Result<string, PineValue>.Ok evalOk)
            {
                FunctionApplicationCache[(functionValue, argumentValue)] = evalOk.Value;
            }

            return evalResult;
        })));

    public Result<string, PineValue> EvaluateKernelApplicationExpression(
        PineValue environment,
        Expression.KernelApplicationExpression application) =>
        EvaluateExpression(environment, application.argument)
        .MapError(error => "Failed to evaluate argument: " + error)
        .AndThen(argument =>
        {
            if (!NamedKernelFunctions.TryGetValue(application.functionName, out var function))
                return Result<string, PineValue>.err("Did not find kernel function '" + application.functionName + "'");

            return function(argument);
        });

    public Result<string, PineValue> EvaluateConditionalExpression(
        PineValue environment,
        Expression.ConditionalExpression conditional) =>
        EvaluateExpression(environment, conditional.condition)
        .MapError(error => "Failed to evaluate condition: " + error)
        .AndThen(conditionValue =>
        EvaluateExpression(environment, conditionValue == TrueValue ? conditional.ifTrue : conditional.ifFalse));

    static readonly IReadOnlyDictionary<string, Func<PineValue, Result<string, PineValue>>> NamedKernelFunctions =
        ImmutableDictionary<string, Func<PineValue, Result<string, PineValue>>>.Empty
        .SetItem(nameof(KernelFunction.equal), KernelFunction.equal)
        .SetItem(nameof(KernelFunction.logical_not), KernelFunction.logical_not)
        .SetItem(nameof(KernelFunction.logical_and), KernelFunction.logical_and)
        .SetItem(nameof(KernelFunction.logical_or), KernelFunction.logical_or)
        .SetItem(nameof(KernelFunction.length), KernelFunction.length)
        .SetItem(nameof(KernelFunction.skip), KernelFunction.skip)
        .SetItem(nameof(KernelFunction.take), KernelFunction.take)
        .SetItem(nameof(KernelFunction.reverse), KernelFunction.reverse)
        .SetItem(nameof(KernelFunction.concat), KernelFunction.concat)
        .SetItem(nameof(KernelFunction.list_head), KernelFunction.list_head)
        .SetItem(nameof(KernelFunction.neg_int), KernelFunction.neg_int)
        .SetItem(nameof(KernelFunction.add_int), KernelFunction.add_int)
        .SetItem(nameof(KernelFunction.sub_int), KernelFunction.sub_int)
        .SetItem(nameof(KernelFunction.mul_int), KernelFunction.mul_int)
        .SetItem(nameof(KernelFunction.div_int), KernelFunction.div_int)
        .SetItem(nameof(KernelFunction.sort_int), value => Result<string, PineValue>.ok(KernelFunction.sort_int(value)));

    static public PineValue ValueFromBool(bool b) => b ? TrueValue : FalseValue;

    static readonly PineValue TrueValue = PineValue.Blob(new byte[] { 4 });

    static readonly PineValue FalseValue = PineValue.Blob(new byte[] { 2 });

    static public Result<string, bool> DecodeBoolFromValue(PineValue value) =>
        value == TrueValue
        ?
        Result<string, bool>.ok(true)
        :
        (value == FalseValue ? Result<string, bool>.ok(false)
        :
        Result<string, bool>.err("Value is neither True nor False"));

    static public Result<string, Expression> DecodeExpressionFromValue(PineValue value) =>
        DecodeUnionFromPineValue(ExpressionDecoders, value);

    static readonly IImmutableDictionary<string, Func<PineValue, Result<string, Expression>>> ExpressionDecoders =
        ImmutableDictionary<string, Func<PineValue, Result<string, Expression>>>.Empty
        .SetItem(
            "Literal",
            literal => Result<string, Expression>.ok(new Expression.LiteralExpression(literal)))
        .SetItem(
            "List",
            listValue =>
            DecodePineListValue(listValue)
            .AndThen(list => ResultListMapCombine(list, DecodeExpressionFromValue))
            .Map(expressionList => (Expression)new Expression.ListExpression(expressionList.ToImmutableArray())))
        .SetItem(
            "DecodeAndEvaluate",
            value => DecodeDecodeAndEvaluateExpression(value)
            .Map(application => (Expression)application))
        .SetItem(
            "KernelApplication",
            value => DecodeKernelApplicationExpression(value)
            .Map(application => (Expression)application))
        .SetItem(
            "Conditional",
            value => DecodeConditionalExpression(value)
            .Map(conditional => (Expression)conditional))
        .SetItem(
            "Environment",
            _ => Result<string, Expression>.ok(new Expression.EnvironmentExpression()))
        .SetItem(
            "StringTag",
            value => DecodeStringTagExpression(value)
            .Map(stringTag => (Expression)stringTag));

    static public Result<string, Expression.DecodeAndEvaluateExpression> DecodeDecodeAndEvaluateExpression(PineValue value) =>
        DecodeRecord2FromPineValue(
            value,
            ("expression", DecodeExpressionFromValue),
            ("environment", DecodeExpressionFromValue),
            (expression, environment) => new Expression.DecodeAndEvaluateExpression(expression: expression, environment: environment));

    static public Result<string, Expression.KernelApplicationExpression> DecodeKernelApplicationExpression(PineValue value) =>
        DecodeRecord2FromPineValue(
            value,
            (nameof(Expression.KernelApplicationExpression.functionName), Composition.StringFromComponent),
            (nameof(Expression.KernelApplicationExpression.argument), DecodeExpressionFromValue),
            (functionName, argument) => new Expression.KernelApplicationExpression(functionName: functionName, argument: argument));

    static public Result<string, Expression.ConditionalExpression> DecodeConditionalExpression(PineValue value) =>
        DecodeRecord3FromPineValue(
            value,
            (nameof(Expression.ConditionalExpression.condition), DecodeExpressionFromValue),
            (nameof(Expression.ConditionalExpression.ifTrue), DecodeExpressionFromValue),
            (nameof(Expression.ConditionalExpression.ifFalse), DecodeExpressionFromValue),
            (condition, ifTrue, ifFalse) => new Expression.ConditionalExpression(condition: condition, ifTrue: ifTrue, ifFalse: ifFalse));

    static public Result<string, Expression.StringTagExpression> DecodeStringTagExpression(PineValue value) =>
        DecodePineListValue(value)
        .AndThen(DecodeListWithExactlyTwoElements)
        .AndThen(tagValueAndTaggedValue =>
        Composition.StringFromComponent(tagValueAndTaggedValue.Item1).MapError(err => "Failed to decode tag: " + err)
        .AndThen(tag => DecodeExpressionFromValue(tagValueAndTaggedValue.Item2).MapError(err => "Failed to decoded tagged expression: " + err)
        .Map(tagged => new Expression.StringTagExpression(tag: tag, tagged: tagged))));

    static public Result<ErrT, IReadOnlyList<MappedOkT>> ResultListMapCombine<ErrT, OkT, MappedOkT>(
        IReadOnlyList<OkT> list,
        Func<OkT, Result<ErrT, MappedOkT>> mapElement) =>
        list.Select(mapElement).ToImmutableList().ListCombine();

    static public Result<string, Record> DecodeRecord3FromPineValue<FieldA, FieldB, FieldC, Record>(
        PineValue value,
        (string name, Func<PineValue, Result<string, FieldA>> decode) fieldA,
        (string name, Func<PineValue, Result<string, FieldB>> decode) fieldB,
        (string name, Func<PineValue, Result<string, FieldC>> decode) fieldC,
        Func<FieldA, FieldB, FieldC, Record> compose) =>
        DecodeRecordFromPineValue(value)
        .AndThen(record =>
        {
            if (!record.TryGetValue(fieldA.name, out var fieldAValue))
                return Result<string, Record>.err("Did not find field " + fieldA.name);

            if (!record.TryGetValue(fieldB.name, out var fieldBValue))
                return Result<string, Record>.err("Did not find field " + fieldB.name);

            if (!record.TryGetValue(fieldC.name, out var fieldCValue))
                return Result<string, Record>.err("Did not find field " + fieldC.name);

            return
            fieldA.decode(fieldAValue)
            .AndThen(fieldADecoded =>
            fieldB.decode(fieldBValue)
            .AndThen(fieldBDecoded =>
            fieldC.decode(fieldCValue)
            .Map(fieldCDecoded => compose(fieldADecoded, fieldBDecoded, fieldCDecoded))));
        });

    static public Result<string, Record> DecodeRecord2FromPineValue<FieldA, FieldB, Record>(
        PineValue value,
        (string name, Func<PineValue, Result<string, FieldA>> decode) fieldA,
        (string name, Func<PineValue, Result<string, FieldB>> decode) fieldB,
        Func<FieldA, FieldB, Record> compose) =>
        DecodeRecordFromPineValue(value)
        .AndThen(record =>
        {
            if (!record.TryGetValue(fieldA.name, out var fieldAValue))
                return Result<string, Record>.err("Did not find field " + fieldA.name);

            if (!record.TryGetValue(fieldB.name, out var fieldBValue))
                return Result<string, Record>.err("Did not find field " + fieldB.name);

            return
            fieldA.decode(fieldAValue)
            .AndThen(fieldADecoded =>
            fieldB.decode(fieldBValue)
            .Map(fieldBDecoded => compose(fieldADecoded, fieldBDecoded)));
        });

    static public Result<string, ImmutableDictionary<string, PineValue>> DecodeRecordFromPineValue(PineValue value) =>
        DecodePineListValue(value)
        .AndThen(list =>
        list
        .Aggregate(
            seed: Result<string, ImmutableDictionary<string, PineValue>>.ok(ImmutableDictionary<string, PineValue>.Empty),
            func: (aggregate, listElement) => aggregate.AndThen(recordFields =>
            DecodePineListValue(listElement)
            .AndThen(DecodeListWithExactlyTwoElements)
            .AndThen(fieldNameValueAndValue =>
            Composition.StringFromComponent(fieldNameValueAndValue.Item1)
            .Map(fieldName => recordFields.SetItem(fieldName, fieldNameValueAndValue.Item2))))));

    static public Result<string, T> DecodeUnionFromPineValue<T>(
        IImmutableDictionary<string, Func<PineValue, Result<string, T>>> variants,
        PineValue value) =>
        DecodePineListValue(value)
        .AndThen(DecodeListWithExactlyTwoElements)
        .AndThen(tagNameValueAndValue =>
        Composition.StringFromComponent(tagNameValueAndValue.Item1)
        .MapError(error => "Failed to decode union tag name: " + error)
        .AndThen(tagName =>
        {
            if (!variants.TryGetValue(tagName, out var variant))
                return Result<string, T>.err("Unexpected tag name: " + tagName);

            return variant(tagNameValueAndValue.Item2)!;
        }));

    static public Result<string, IImmutableList<PineValue>> DecodePineListValue(PineValue value)
    {
        if (value is not PineValue.ListValue listComponent)
            return Result<string, IImmutableList<PineValue>>.err("Not a list");

        return Result<string, IImmutableList<PineValue>>.ok(
            listComponent.Elements as IImmutableList<PineValue> ?? listComponent.Elements.ToImmutableList());
    }

    static public Result<string, (T, T)> DecodeListWithExactlyTwoElements<T>(IImmutableList<T> list)
    {
        if (list.Count != 2)
            return Result<string, (T, T)>.err("Unexpected number of elements in list: Not 2 but " + list.Count);

        return Result<string, (T, T)>.ok((list[0], list[1]));
    }


    static public class KernelFunction
    {
        static public Result<string, PineValue> equal(PineValue value) =>
            Result<string, PineValue>.ok(
                ValueFromBool(
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
            DecodeBoolFromValue(value)
            .Map(b => ValueFromBool(!b));

        static public Result<string, PineValue> logical_and(PineValue value) =>
            KernelFunctionExpectingListOfTypeBool(bools => bools.Aggregate(seed: true, func: (a, b) => a && b), value);

        static public Result<string, PineValue> logical_or(PineValue value) =>
            KernelFunctionExpectingListOfTypeBool(bools => bools.Aggregate(seed: false, func: (a, b) => a || b), value);

        static public Result<string, PineValue> length(PineValue value) =>
            Result<string, PineValue>.ok(
                Composition.ComponentFromSignedInteger(
                    value switch
                    {
                        PineValue.BlobValue blobComponent => blobComponent.Bytes.Length,
                        PineValue.ListValue listComponent => listComponent.Elements.Count,
                        _ => throw new NotImplementedException()
                    }));

        static public Result<string, PineValue> skip(PineValue value) =>
            KernelFunctionExpectingExactlyTwoArguments(
                Composition.SignedIntegerFromComponent,
                Result<string, PineValue>.ok,
                compose: (count, list) =>
                Result<string, PineValue>.ok(
                    list switch
                    {
                        PineValue.BlobValue blobComponent => PineValue.Blob(blobComponent.Bytes[(int)count..]),
                        PineValue.ListValue listComponent => PineValue.List(listComponent.Elements.Skip((int)count).ToImmutableList()),
                        _ => throw new NotImplementedException()
                    }))
            (value);

        static public Result<string, PineValue> take(PineValue value) =>
            KernelFunctionExpectingExactlyTwoArguments(
                Composition.SignedIntegerFromComponent,
                Result<string, PineValue>.ok,
                compose: (count, list) =>
                Result<string, PineValue>.ok(
                    list switch
                    {
                        PineValue.BlobValue blobComponent => PineValue.Blob(blobComponent.Bytes[..(int)count]),
                        PineValue.ListValue listComponent => PineValue.List(listComponent.Elements.Take((int)count).ToImmutableList()),
                        _ => throw new NotImplementedException()
                    }))
            (value);

        static public Result<string, PineValue> reverse(PineValue value) =>
            Result<string, PineValue>.ok(
                value switch
                {
                    PineValue.BlobValue blobComponent => PineValue.Blob(blobComponent.Bytes.ToArray().Reverse().ToArray()),
                    PineValue.ListValue listComponent => PineValue.List(listComponent.Elements.Reverse().ToImmutableList()),
                    _ => throw new NotImplementedException()
                });

        static public Result<string, PineValue> concat(PineValue value) =>
            DecodePineListValue(value)
            .Map(list =>
            list.Aggregate(
                seed: PineValue.EmptyList,
                func: (aggregate, elem) =>
                elem switch
                {
                    PineValue.BlobValue elemBlobComponent =>
                    aggregate switch
                    {
                        PineValue.BlobValue aggregateBlobComponent =>
                        PineValue.Blob(CommonConversion.Concat(
                            aggregateBlobComponent.Bytes.Span, elemBlobComponent.Bytes.Span)),
                        _ => elemBlobComponent
                    },
                    PineValue.ListValue elemListComponent =>
                    aggregate switch
                    {
                        PineValue.ListValue aggregateListComponent =>
                        PineValue.List(aggregateListComponent.Elements.Concat(elemListComponent.Elements).ToImmutableList()),
                        _ => elemListComponent
                    },
                    _ => throw new NotImplementedException()
                }));

        static public Result<string, PineValue> list_head(PineValue value) =>
            DecodePineListValue(value)
            .Map(list => list.Count < 1 ? PineValue.EmptyList : list[0]);

        static public Result<string, PineValue> neg_int(PineValue value) =>
            Composition.SignedIntegerFromComponent(value)
            .Map(i => Composition.ComponentFromSignedInteger(-i));

        static public Result<string, PineValue> add_int(PineValue value) =>
            KernelFunctionExpectingListOfBigIntWithAtLeastOneAndProducingBigInt(
                (firstInt, otherInts) => otherInts.Aggregate(seed: firstInt, func: (aggregate, next) => aggregate + next),
                value);

        static public Result<string, PineValue> sub_int(PineValue value) =>
            KernelFunctionExpectingListOfBigIntWithAtLeastOneAndProducingBigInt(
                (firstInt, otherInts) => otherInts.Aggregate(seed: firstInt, func: (aggregate, next) => aggregate - next),
                value);

        static public Result<string, PineValue> mul_int(PineValue value) =>
            KernelFunctionExpectingListOfBigIntWithAtLeastOneAndProducingBigInt(
                (firstInt, otherInts) => otherInts.Aggregate(seed: firstInt, func: (aggregate, next) => aggregate * next),
                value);

        static public Result<string, PineValue> div_int(PineValue value) =>
            KernelFunctionExpectingListOfBigIntWithAtLeastOneAndProducingBigInt(
                (firstInt, otherInts) => otherInts.Aggregate(seed: firstInt, func: (aggregate, next) => aggregate / next),
                value);

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
                    (Composition.SignedIntegerFromBlobValue(blobX.Bytes.Span),
                    Composition.SignedIntegerFromBlobValue(blobY.Bytes.Span)) switch
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
            Func<BigInteger, IReadOnlyList<BigInteger>, BigInteger> aggregate,
            PineValue value) =>
            KernelFunctionExpectingListOfBigInt(
                aggregate:
                listOfIntegers =>
                listOfIntegers.Count < 1
                ?
                Result<string, PineValue>.err("List is empty. Expected at least one element")
                :
                Result<string, PineValue>.ok(
                    Composition.ComponentFromSignedInteger(aggregate(listOfIntegers[0], listOfIntegers.Skip(1).ToImmutableArray()))),
                value);

        static Result<string, PineValue> KernelFunctionExpectingListOfBigInt(
            Func<IReadOnlyList<BigInteger>, Result<string, PineValue>> aggregate,
            PineValue value) =>
            DecodePineListValue(value)
            .AndThen(list => ResultListMapCombine(list, Composition.SignedIntegerFromComponent))
            .AndThen(ints => aggregate(ints));

        static Func<PineValue, Result<string, PineValue>> KernelFunctionExpectingExactlyTwoArguments<ArgA, ArgB>(
            Func<PineValue, Result<string, ArgA>> decodeArgA,
            Func<PineValue, Result<string, ArgB>> decodeArgB,
            Func<ArgA, ArgB, Result<string, PineValue>> compose) =>
            value => DecodePineListValue(value)
            .AndThen(DecodeListWithExactlyTwoElements)
            .AndThen(argsValues =>
            decodeArgA(argsValues.Item1)
            .AndThen(argA =>
            decodeArgB(argsValues.Item2)
            .AndThen(argB => compose(argA, argB))));

        static Result<string, PineValue> KernelFunctionExpectingListOfTypeBool(
            Func<IReadOnlyList<bool>, bool> compose,
            PineValue value) =>
            DecodePineListValue(value)
            .AndThen(list => ResultListMapCombine(list, DecodeBoolFromValue))
            .Map(compose)
            .Map(ValueFromBool);
    }


    public abstract record Expression()
    {
        public record LiteralExpression(
            PineValue Value)
            : Expression;

        public record ListExpression(
            ImmutableArray<Expression> List)
            : Expression;

        public record DecodeAndEvaluateExpression(
            Expression expression,
            Expression environment)
            : Expression;

        public record KernelApplicationExpression(
            string functionName,
            Expression argument)
            : Expression;

        public record ConditionalExpression(
            Expression condition,
            Expression ifTrue,
            Expression ifFalse)
            : Expression;

        public record EnvironmentExpression() : Expression;

        public record StringTagExpression(
            string tag,
            Expression tagged)
            : Expression;
    }
}
