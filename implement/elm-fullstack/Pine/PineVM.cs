using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Numerics;

namespace Pine;

public static class PineVM
{
    static public Result<string, PineValue> EvaluateExpression(
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
                .mapError(err => "Failed to evaluate list element: " + err)
                .map(list => PineValue.List(list));
        }

        if (expression is Expression.ApplicationExpression applicationExpression)
        {
            return
                EvaluateApplicationExpression(environment, applicationExpression)
                .mapError(err => "Failed to evaluate function application: " + err);
        }

        if (expression is Expression.KernelApplicationExpression kernelApplicationExpression)
        {
            return
                EvaluateKernelApplicationExpression(environment, kernelApplicationExpression)
                .mapError(err => "Failed to evaluate kernel function application: " + err);
        }

        if (expression is Expression.ConditionalExpression conditionalExpression)
        {
            return EvaluateConditionalExpression(environment, conditionalExpression);
        }

        if (expression is Expression.ApplicationArgumentExpression)
        {
            return Result<string, PineValue>.ok(environment);
        }

        if (expression is Expression.StringTagExpression stringTagExpression)
        {
            return EvaluateExpression(environment, stringTagExpression.tagged);
        }

        throw new NotImplementedException("Unexpected shape of expression");
    }

    static public Result<string, PineValue> EvaluateApplicationExpression(
        PineValue environment,
        Expression.ApplicationExpression application) =>
        EvaluateExpression(environment, application.function)
        .mapError(error => "Failed to evaluate function: " + error)
        .andThen(functionValue => DecodeExpressionFromValue(functionValue)
        .mapError(error => "Failed to decode expression from function value: " + error))
        .andThen(functionExpression => EvaluateExpression(environment, application.argument)
        .mapError(error => "Failed to evaluate argument: " + error)
        .andThen(argumentValue => EvaluateExpression(environment: argumentValue, expression: functionExpression)));

    static public Result<string, PineValue> EvaluateKernelApplicationExpression(
        PineValue environment,
        Expression.KernelApplicationExpression application) =>
        EvaluateExpression(environment, application.argument)
        .mapError(error => "Failed to evaluate argument: " + error)
        .andThen(argument =>
        {
            if (!NamedKernelFunctions.TryGetValue(application.functionName, out var function))
                return Result<string, PineValue>.err("Did not find kernel function '" + application.functionName + "'");

            return function(argument);
        });

    static public Result<string, PineValue> EvaluateConditionalExpression(
        PineValue environment,
        Expression.ConditionalExpression conditional) =>
        EvaluateExpression(environment, conditional.condition)
        .mapError(error => "Failed to evaluate condition: " + error)
        .andThen(conditionValue =>
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
        .SetItem(nameof(KernelFunction.sort_int), KernelFunction.sort_int)
        .SetItem(nameof(KernelFunction.look_up_name_in_ListValue), KernelFunction.look_up_name_in_ListValue);

    static public PineValue ValueFromBool(bool b) => b ? TrueValue : FalseValue;

    static readonly PineValue TrueValue = TagValue("True", ImmutableArray<PineValue>.Empty);

    static readonly PineValue FalseValue = TagValue("False", ImmutableArray<PineValue>.Empty);

    static public Result<string, bool?> DecodeBoolFromValue(PineValue value) =>
        value == TrueValue
        ?
        Result<string, bool?>.ok(true)
        :
        (value == FalseValue ? Result<string, bool?>.ok(false)
        :
        Result<string, bool?>.err("Value is neither True nor False"));

    static PineValue TagValue(string tagName, IImmutableList<PineValue> tagArguments) =>
        PineValue.List(
            ImmutableList.Create(Composition.ComponentFromString(tagName), PineValue.List(tagArguments)));

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
            .andThen(list => ResultListMapCombine(list, DecodeExpressionFromValue))
            .map(expressionList => (Expression)new Expression.ListExpression(expressionList.ToImmutableArray())))
        .SetItem(
            "Application",
            value => DecodeApplicationExpression(value)
            .map(application => (Expression)application))
        .SetItem(
            "KernelApplication",
            value => DecodeKernelApplicationExpression(value)
            .map(application => (Expression)application))
        .SetItem(
            "Conditional",
            value => DecodeConditionalExpression(value)
            .map(conditional => (Expression)conditional))
        .SetItem(
            "ApplicationArgument",
            _ => Result<string, Expression>.ok(new Expression.ApplicationArgumentExpression()))
        .SetItem(
            "StringTag",
            value => DecodeStringTagExpression(value)
            .map(stringTag => (Expression)stringTag));

    static public Result<string, Expression.ApplicationExpression> DecodeApplicationExpression(PineValue value) =>
        DecodeRecord2FromPineValue(
            value,
            ("function", DecodeExpressionFromValue),
            ("argument", DecodeExpressionFromValue),
            (function, argument) => new Expression.ApplicationExpression(function: function, argument: argument));

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
        DecodeRecord2FromPineValue(
            value,
            (nameof(Expression.StringTagExpression.tag), Composition.StringFromComponent),
            (nameof(Expression.StringTagExpression.tagged), DecodeExpressionFromValue),
            (tag, tagged) => new Expression.StringTagExpression(tag: tag, tagged: tagged));

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
        .andThen(record =>
        {
            if (!record.TryGetValue(fieldA.name, out var fieldAValue))
                return Result<string, Record>.err("Did not find field " + fieldA.name);

            if (!record.TryGetValue(fieldB.name, out var fieldBValue))
                return Result<string, Record>.err("Did not find field " + fieldB.name);

            if (!record.TryGetValue(fieldC.name, out var fieldCValue))
                return Result<string, Record>.err("Did not find field " + fieldC.name);

            return
            fieldA.decode(fieldAValue)
            .andThen(fieldADecoded =>
            fieldB.decode(fieldBValue)
            .andThen(fieldBDecoded =>
            fieldC.decode(fieldCValue)
            .map(fieldCDecoded => compose(fieldADecoded, fieldBDecoded, fieldCDecoded))));
        });

    static public Result<string, Record> DecodeRecord2FromPineValue<FieldA, FieldB, Record>(
        PineValue value,
        (string name, Func<PineValue, Result<string, FieldA>> decode) fieldA,
        (string name, Func<PineValue, Result<string, FieldB>> decode) fieldB,
        Func<FieldA, FieldB, Record> compose) =>
        DecodeRecordFromPineValue(value)
        .andThen(record =>
        {
            if (!record.TryGetValue(fieldA.name, out var fieldAValue))
                return Result<string, Record>.err("Did not find field " + fieldA.name);

            if (!record.TryGetValue(fieldB.name, out var fieldBValue))
                return Result<string, Record>.err("Did not find field " + fieldB.name);

            return
            fieldA.decode(fieldAValue)
            .andThen(fieldADecoded =>
            fieldB.decode(fieldBValue)
            .map(fieldBDecoded => compose(fieldADecoded, fieldBDecoded)));
        });

    static public Result<string, ImmutableDictionary<string, PineValue>> DecodeRecordFromPineValue(PineValue value) =>
        DecodePineListValue(value)
        .andThen(list =>
        list
        .Aggregate(
            seed: Result<string, ImmutableDictionary<string, PineValue>>.ok(ImmutableDictionary<string, PineValue>.Empty),
            func: (aggregate, listElement) => aggregate.andThen(recordFields =>
            DecodePineListValue(listElement)
            .andThen(DecodeListWithExactlyTwoElements)
            .andThen(fieldNameValueAndValue =>
            Composition.StringFromComponent(fieldNameValueAndValue!.Value.Item1)
            .map(fieldName => recordFields.SetItem(fieldName, fieldNameValueAndValue.Value.Item2))))));

    static public Result<string, T> DecodeUnionFromPineValue<T>(
        IImmutableDictionary<string, Func<PineValue, Result<string, T>>> variants,
        PineValue value) =>
        DecodePineListValue(value)
        .andThen(DecodeListWithExactlyTwoElements)
        .andThen(tagNameValueAndValue =>
        Composition.StringFromComponent(tagNameValueAndValue!.Value.Item1)
        .mapError(error => "Failed to decode union tag name: " + error)
        .andThen(tagName =>
        {
            if (!variants.TryGetValue(tagName, out var variant))
                return Result<string, T>.err("Unexpected tag name: " + tagName);

            return variant(tagNameValueAndValue!.Value.Item2)!;
        }));

    static public Result<string, IImmutableList<PineValue>> DecodePineListValue(PineValue value)
    {
        if (value is not PineValue.ListValue listComponent)
            return Result<string, IImmutableList<PineValue>>.err("Not a list");

        return Result<string, IImmutableList<PineValue>>.ok(
            listComponent.ListContent as IImmutableList<PineValue> ?? listComponent.ListContent.ToImmutableList());
    }

    static public Result<string, (T, T)?> DecodeListWithExactlyTwoElements<T>(IImmutableList<T> list)
    {
        if (list.Count != 2)
            return Result<string, (T, T)?>.err("Unexpected number of elements in list: Not 2 but " + list.Count);

        return Result<string, (T, T)?>.ok((list[0], list[1]));
    }


    static public class KernelFunction
    {
        static public Result<string, PineValue> equal(PineValue value) =>
            DecodePineListValue(value)
            .map(list =>
            list.Count < 1
            ?
            true
            :
            (bool?)list.All(e => e.Equals(list[0])))
            .map(b => ValueFromBool(b!.Value));

        static public Result<string, PineValue> logical_not(PineValue value) =>
            DecodeBoolFromValue(value)
            .map(b => ValueFromBool(!b!.Value));

        static public Result<string, PineValue> logical_and(PineValue value) =>
            KernelFunctionExpectingListOfTypeBool(bools => bools.Aggregate(seed: true, func: (a, b) => a && b), value);

        static public Result<string, PineValue> logical_or(PineValue value) =>
            KernelFunctionExpectingListOfTypeBool(bools => bools.Aggregate(seed: false, func: (a, b) => a || b), value);

        static public Result<string, PineValue> length(PineValue value) =>
            Result<string, PineValue>.ok(
                Composition.ComponentFromSignedInteger(
                    value switch
                    {
                        PineValue.BlobValue blobComponent => blobComponent.BlobContent.Length,
                        PineValue.ListValue listComponent => listComponent.ListContent.Count,
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
                        PineValue.BlobValue blobComponent => PineValue.Blob(blobComponent.BlobContent[(int)count..]),
                        PineValue.ListValue listComponent => PineValue.List(listComponent.ListContent.Skip((int)count).ToImmutableList()),
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
                        PineValue.BlobValue blobComponent => PineValue.Blob(blobComponent.BlobContent[..(int)count]),
                        PineValue.ListValue listComponent => PineValue.List(listComponent.ListContent.Take((int)count).ToImmutableList()),
                        _ => throw new NotImplementedException()
                    }))
            (value);

        static public Result<string, PineValue> reverse(PineValue value) =>
            Result<string, PineValue>.ok(
                value switch
                {
                    PineValue.BlobValue blobComponent => PineValue.Blob(blobComponent.BlobContent.ToArray().Reverse().ToArray()),
                    PineValue.ListValue listComponent => PineValue.List(listComponent.ListContent.Reverse().ToImmutableList()),
                    _ => throw new NotImplementedException()
                });

        static public Result<string, PineValue> concat(PineValue value) =>
            DecodePineListValue(value)
            .map(list =>
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
                            aggregateBlobComponent.BlobContent.Span, elemBlobComponent.BlobContent.Span)),
                        _ => elemBlobComponent
                    },
                    PineValue.ListValue elemListComponent =>
                    aggregate switch
                    {
                        PineValue.ListValue aggregateListComponent =>
                        PineValue.List(aggregateListComponent.ListContent.Concat(elemListComponent.ListContent).ToImmutableList()),
                        _ => elemListComponent
                    },
                    _ => throw new NotImplementedException()
                }));

        static public Result<string, PineValue> list_head(PineValue value) =>
            DecodePineListValue(value)
            .map(list => list.Count < 1 ? PineValue.EmptyList : list[0]);

        static public Result<string, PineValue> neg_int(PineValue value) =>
            Composition.SignedIntegerFromComponent(value)
            .map(i => Composition.ComponentFromSignedInteger(-i));

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

        static public Result<string, PineValue> sort_int(PineValue value) =>
            KernelFunctionExpectingListOfBigInt(
                ints => Result<string, PineValue>.ok(
                    PineValue.List(ints.OrderBy(i => i).Select(Composition.ComponentFromSignedInteger).ToImmutableList())),
                value);

        static public Result<string, PineValue> look_up_name_in_ListValue(PineValue value) =>
            KernelFunctionExpectingExactlyTwoArguments(
                Result<string, PineValue>.ok,
                Result<string, PineValue>.ok,
                (nameValue, listValue) => lookUpNameInListValue(nameValue, listValue))
            (value).map(foundNamed => PineValue.List(ImmutableList.Create(foundNamed)));

        static public Result<string, PineValue> lookUpNameInListValue(
            PineValue nameValue, PineValue listValue) =>
             DecodePineListValue(listValue)
             .andThen(list =>
             {
                 var listNamedEntries =
                 list.SelectMany(element =>
                 element switch
                 {
                     PineValue.ListValue listComponent when listComponent.ListContent.Count == 2 =>
                     new[] { (name: listComponent.ListContent[0], named: listComponent.ListContent[1]) },
                     _ => Array.Empty<(PineValue name, PineValue named)>()
                 });

                 foreach (var namedEntry in listNamedEntries)
                 {
                     if (namedEntry.name == nameValue)
                         return Result<string, PineValue>.ok(namedEntry.named);
                 }

                 var nameAsString = Composition.StringFromComponent(nameValue).withDefault(() => "name_is_not_a_string");

                 var names =
                 listNamedEntries
                 .Select((namedEntry, _) => Composition.StringFromComponent(namedEntry.name).unpack(fromErr: _ => null, fromOk: s => s))
                 .WhereNotNull().ToImmutableArray();

                 return Result<string, PineValue>.err(
                     "Did not find '" + nameAsString + "'. There are " +
                     list.Count + " entries and " +
                     names.Length + " names available in that scope: " +
                     string.Join(", ", names));
             });

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
            .andThen(list => ResultListMapCombine(list, Composition.SignedIntegerFromComponent))
            .andThen(ints => aggregate(ints));

        static Func<PineValue, Result<string, PineValue>> KernelFunctionExpectingExactlyTwoArguments<ArgA, ArgB>(
            Func<PineValue, Result<string, ArgA>> decodeArgA,
            Func<PineValue, Result<string, ArgB>> decodeArgB,
            Func<ArgA, ArgB, Result<string, PineValue>> compose) =>
            value => DecodePineListValue(value)
            .andThen(DecodeListWithExactlyTwoElements)
            .andThen(argsValues =>
            decodeArgA(argsValues!.Value.Item1)
            .andThen(argA =>
            decodeArgB(argsValues.Value.Item2)
            .andThen(argB => compose(argA, argB))));

        static Result<string, PineValue> KernelFunctionExpectingListOfTypeBool(
            Func<IReadOnlyList<bool>, bool?> compose,
            PineValue value) =>
            DecodePineListValue(value)
            .andThen(list => ResultListMapCombine(list, DecodeBoolFromValue))
            .map(bools => compose(bools.Select(b => b!.Value).ToImmutableList()))
            .map(b => ValueFromBool(b!.Value));
    }


    public abstract record Expression()
    {
        public record LiteralExpression(
            PineValue Value)
            : Expression;

        public record ListExpression(
            ImmutableArray<Expression> List)
            : Expression;

        public record ApplicationExpression(
            Expression function,
            Expression argument)
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

        public record ApplicationArgumentExpression() : Expression;

        public record StringTagExpression(
            string tag,
            Expression tagged)
            : Expression;
    }
}
