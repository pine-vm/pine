using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Numerics;

namespace Pine;

public static class PineVM
{
    static public Result<string, Composition.Component> EvaluateExpression(
        Composition.Component environment,
        Expression expression)
    {
        if (expression.LiteralExpression != null)
            return Result<string, Composition.Component>.ok(expression.LiteralExpression);

        if (expression.ListExpression != null)
        {
            return
                ResultListMapCombine(
                    expression.ListExpression.Value,
                    elem => EvaluateExpression(environment, elem))
                .mapError(err => "Failed to evaluate list element: " + err)
                .map(list => Composition.Component.List(list.ToImmutableList()));
        }

        if (expression.ApplicationExpression != null)
        {
            return
                EvaluateApplicationExpression(environment, expression.ApplicationExpression)
                .mapError(err => "Failed to evaluate function application: " + err);
        }

        if (expression.KernelApplicationExpression != null)
        {
            return
                EvaluateKernelApplicationExpression(environment, expression.KernelApplicationExpression)
                .mapError(err => "Failed to evaluate kernel function application: " + err);
        }

        if (expression.ConditionalExpression != null)
        {
            return EvaluateConditionalExpression(environment, expression.ConditionalExpression);
        }

        if (expression.ApplicationArgumentExpression != null)
        {
            return Result<string, Composition.Component>.ok(environment);
        }

        if (expression.StringTagExpression != null)
        {
            return EvaluateExpression(environment, expression.StringTagExpression.tagged);
        }

        throw new NotImplementedException("Unexpected shape of expression");
    }

    static public Result<string, Composition.Component> EvaluateApplicationExpression(
        Composition.Component environment,
        ApplicationExpressionStructure application)
    {
        var evalFunctionResult = EvaluateExpression(environment, application.function);

        if (!evalFunctionResult.IsOk())
            return Result<string, Composition.Component>.err("Failed to evaluate function: " + evalFunctionResult.Err);

        var decodeFunctionResult = DecodeExpressionFromValue(evalFunctionResult.Ok!);

        if (!decodeFunctionResult.IsOk())
            return Result<string, Composition.Component>.err("Failed to decode expression from function value: " + decodeFunctionResult.Err);

        var evalArgumentResult = EvaluateExpression(environment, application.argument);

        if (!evalArgumentResult.IsOk())
            return Result<string, Composition.Component>.err("Failed to evaluate argument: " + evalArgumentResult.Err);

        return EvaluateExpression(evalArgumentResult.Ok!, decodeFunctionResult.Ok!);
    }

    static public Result<string, Composition.Component> EvaluateKernelApplicationExpression(
        Composition.Component environment,
        KernelApplicationExpressionStructure application)
    {
        var evalArgumentResult = EvaluateExpression(environment, application.argument);

        if (!evalArgumentResult.IsOk())
            return Result<string, Composition.Component>.err("Failed to evaluate argument: " + evalArgumentResult.Err);

        if (!NamedKernelFunctions.TryGetValue(application.functionName, out var function))
            return Result<string, Composition.Component>.err("Did not find kernel function '" + application.functionName + "'");

        return function(evalArgumentResult.Ok!);
    }

    static public Result<string, Composition.Component> EvaluateConditionalExpression(
        Composition.Component environment,
        ConditionalExpressionStructure conditional)
    {
        var evalConditionResult = EvaluateExpression(environment, conditional.condition);

        if (!evalConditionResult.IsOk())
            return Result<string, Composition.Component>.err("Failed to evaluate condition: " + evalConditionResult.Err);

        return
            evalConditionResult.Ok == TrueValue
            ?
            EvaluateExpression(environment, conditional.ifTrue)
            :
            EvaluateExpression(environment, conditional.ifFalse);
    }

    static readonly IReadOnlyDictionary<string, Func<Composition.Component, Result<string, Composition.Component>>> NamedKernelFunctions =
        ImmutableDictionary<string, Func<Composition.Component, Result<string, Composition.Component>>>.Empty
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

    static public Composition.Component valueFromBool(bool b) => b ? TrueValue : FalseValue;

    static readonly Composition.Component TrueValue = tagValue("True", ImmutableArray<Composition.Component>.Empty);

    static readonly Composition.Component FalseValue = tagValue("False", ImmutableArray<Composition.Component>.Empty);

    static public Result<string, bool?> decodeBoolFromValue(Composition.Component value) =>
        value == TrueValue
        ?
        Result<string, bool?>.ok(true)
        :
        (value == FalseValue ? Result<string, bool?>.ok(false)
        :
        Result<string, bool?>.err("Value is neither True nor False"));

    static Composition.Component tagValue(string tagName, IImmutableList<Composition.Component> tagArguments) =>
        Composition.Component.List(
            ImmutableList.Create(Composition.ComponentFromString(tagName), Composition.Component.List(tagArguments)));
    static public Result<string, Expression> DecodeExpressionFromValue(Composition.Component value) =>
        DecodeUnionFromPineValue(ExpressionDecoders, value);

    static readonly IImmutableDictionary<string, Func<Composition.Component, Result<string, Expression>>> ExpressionDecoders =
        ImmutableDictionary<string, Func<Composition.Component, Result<string, Expression>>>.Empty
        .SetItem(
            "Literal",
            literal => Result<string, Expression>.ok(new Expression { LiteralExpression = literal }))
        .SetItem(
            "List",
            listValue =>
            DecodePineListValue(listValue)
            .andThen(list => ResultListMapCombine(list, DecodeExpressionFromValue))
            .map(expressionList => new Expression { ListExpression = expressionList.ToImmutableArray() }))
        .SetItem(
            "Application",
            value => DecodeApplicationExpression(value)
            .map(application => new Expression { ApplicationExpression = application }))
        .SetItem(
            "KernelApplication",
            value => DecodeKernelApplicationExpression(value)
            .map(application => new Expression { KernelApplicationExpression = application }))
        .SetItem(
            "Conditional",
            value => DecodeConditionalExpression(value)
            .map(conditional => new Expression { ConditionalExpression = conditional }))
        .SetItem(
            "ApplicationArgument",
            _ => Result<string, Expression>.ok(new Expression { ApplicationArgumentExpression = new object() }))
        .SetItem(
            "StringTag",
            value => DecodeStringTagExpression(value)
            .map(stringTag => new Expression { StringTagExpression = stringTag }));

    static public Result<string, ApplicationExpressionStructure> DecodeApplicationExpression(Composition.Component value) =>
        DecodeRecord2FromPineValue(
            value,
            ("function", DecodeExpressionFromValue),
            ("argument", DecodeExpressionFromValue),
            (function, argument) => new ApplicationExpressionStructure(function: function, argument: argument));

    static public Result<string, KernelApplicationExpressionStructure> DecodeKernelApplicationExpression(Composition.Component value) =>
        DecodeRecord2FromPineValue(
            value,
            (nameof(KernelApplicationExpressionStructure.functionName), Composition.StringFromComponent),
            (nameof(KernelApplicationExpressionStructure.argument), DecodeExpressionFromValue),
            (functionName, argument) => new KernelApplicationExpressionStructure(functionName: functionName, argument: argument));

    static public Result<string, ConditionalExpressionStructure> DecodeConditionalExpression(Composition.Component value) =>
        DecodeRecord3FromPineValue(
            value,
            (nameof(ConditionalExpressionStructure.condition), DecodeExpressionFromValue),
            (nameof(ConditionalExpressionStructure.ifTrue), DecodeExpressionFromValue),
            (nameof(ConditionalExpressionStructure.ifFalse), DecodeExpressionFromValue),
            (condition, ifTrue, ifFalse) => new ConditionalExpressionStructure(condition: condition, ifTrue: ifTrue, ifFalse: ifFalse));

    static public Result<string, StringTagExpressionStructure> DecodeStringTagExpression(Composition.Component value) =>
        DecodeRecord2FromPineValue(
            value,
            (nameof(StringTagExpressionStructure.tag), Composition.StringFromComponent),
            (nameof(StringTagExpressionStructure.tagged), DecodeExpressionFromValue),
            (tag, tagged) => new StringTagExpressionStructure(tag: tag, tagged: tagged));

    static public Result<ErrT, IReadOnlyList<MappedOkT>> ResultListMapCombine<ErrT, OkT, MappedOkT>(
        IReadOnlyList<OkT> list,
        Func<OkT, Result<ErrT, MappedOkT>> mapElement)
    {
        var results = list.Select(mapElement).ToImmutableArray();

        var errors = results.Where(r => !r.IsOk()).ToImmutableArray();

        if (errors.Any())
            return Result<ErrT, IReadOnlyList<MappedOkT>>.err(errors.First().Err!);

        return Result<ErrT, IReadOnlyList<MappedOkT>>.ok(results.Select(e => e.Ok).ToImmutableArray());
    }

    static public Result<string, Record> DecodeRecord3FromPineValue<FieldA, FieldB, FieldC, Record>(
        Composition.Component value,
        (string name, Func<Composition.Component, Result<string, FieldA>> decode) fieldA,
        (string name, Func<Composition.Component, Result<string, FieldB>> decode) fieldB,
        (string name, Func<Composition.Component, Result<string, FieldC>> decode) fieldC,
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
        Composition.Component value,
        (string name, Func<Composition.Component, Result<string, FieldA>> decode) fieldA,
        (string name, Func<Composition.Component, Result<string, FieldB>> decode) fieldB,
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

    static public Result<string, ImmutableDictionary<string, Composition.Component>> DecodeRecordFromPineValue(Composition.Component value) =>
        DecodePineListValue(value)
        .andThen(list =>
        list
        .Aggregate(
            seed: Result<string, ImmutableDictionary<string, Composition.Component>>.ok(ImmutableDictionary<string, Composition.Component>.Empty),
            func: (aggregate, listElement) => aggregate.andThen(recordFields =>
            DecodePineListValue(listElement)
            .andThen(DecodeListWithExactlyTwoElements)
            .andThen(fieldNameValueAndValue =>
            Composition.StringFromComponent(fieldNameValueAndValue!.Value.Item1)
            .map(fieldName => recordFields.SetItem(fieldName, fieldNameValueAndValue.Value.Item2))))));

    static public Result<string, T> DecodeUnionFromPineValue<T>(
        IImmutableDictionary<string, Func<Composition.Component, Result<string, T>>> variants,
        Composition.Component value) =>
        DecodePineListValue(value)
        .andThen(DecodeListWithExactlyTwoElements)
        .andThen(tagNameValueAndValue =>
        {
            var tagNameResult = Composition.StringFromComponent(tagNameValueAndValue!.Value.Item1);

            if (tagNameResult.Ok == null)
                return Result<string, T>.err("Failed to decode union tag name: " + tagNameResult.Err);

            if (!variants.TryGetValue(tagNameResult.Ok, out var variant))
                return Result<string, T>.err("Unexpected tag name: " + tagNameResult.Ok);

            return variant(tagNameValueAndValue!.Value.Item2)!;
        });

    static public Result<string, IImmutableList<Composition.Component>> DecodePineListValue(Composition.Component value)
    {
        if (value.ListContent == null)
            return Result<string, IImmutableList<Composition.Component>>.err("Not a list");

        return Result<string, IImmutableList<Composition.Component>>.ok(value.ListContent);
    }

    static public Result<string, (T, T)?> DecodeListWithExactlyTwoElements<T>(IImmutableList<T> list)
    {
        if (list.Count != 2)
            return Result<string, (T, T)?>.err("Unexpected number of elements in list: Not 2 but " + list.Count);

        return Result<string, (T, T)?>.ok((list[0], list[1]));
    }


    static public class KernelFunction
    {
        static public Result<string, Composition.Component> equal(Composition.Component value) =>
            DecodePineListValue(value)
            .map(list =>
            list.Count < 1
            ?
            true
            :
            (bool?)list.All(e => e.Equals(list[0])))
            .map(b => valueFromBool(b!.Value));

        static public Result<string, Composition.Component> logical_not(Composition.Component value) =>
            decodeBoolFromValue(value)
            .map(b => valueFromBool(!b!.Value));

        static public Result<string, Composition.Component> logical_and(Composition.Component value) =>
            KernelFunctionExpectingListOfTypeBool(bools => bools.Aggregate(seed: true, func: (a, b) => a && b), value);

        static public Result<string, Composition.Component> logical_or(Composition.Component value) =>
            KernelFunctionExpectingListOfTypeBool(bools => bools.Aggregate(seed: false, func: (a, b) => a || b), value);

        static public Result<string, Composition.Component> length(Composition.Component value) =>
            Result<string, Composition.Component>.ok(
                Composition.ComponentFromSignedInteger(
                    value.BlobContent.HasValue
                    ?
                    value.BlobContent.Value.Length
                    :
                    value.ListContent!.Count));

        static public Result<string, Composition.Component> skip(Composition.Component value) =>
            KernelFunctionExpectingExactlyTwoArguments(
                Composition.SignedIntegerFromComponent,
                Result<string, Composition.Component>.ok,
                compose: (count, list) =>
                Result<string, Composition.Component>.ok(
                    list.BlobContent != null
                    ?
                    Composition.Component.Blob(list.BlobContent.Value[(int)count!.Value..])
                    :
                    Composition.Component.List(list.ListContent!.Skip((int)count!.Value).ToImmutableList())))
            (value);

        static public Result<string, Composition.Component> take(Composition.Component value) =>
            KernelFunctionExpectingExactlyTwoArguments(
                Composition.SignedIntegerFromComponent,
                Result<string, Composition.Component>.ok,
                compose: (count, list) =>
                Result<string, Composition.Component>.ok(
                    list.BlobContent != null
                    ?
                    Composition.Component.Blob(list.BlobContent.Value[..(int)count!.Value])
                    :
                    Composition.Component.List(list.ListContent!.Take((int)count!.Value).ToImmutableList())))
            (value);

        static public Result<string, Composition.Component> reverse(Composition.Component value) =>
            Result<string, Composition.Component>.ok(
                value.BlobContent != null
                ?
                Composition.Component.Blob(value.BlobContent.Value.ToArray().Reverse().ToArray())
                :
                Composition.Component.List(value.ListContent!.Reverse().ToImmutableList()));

        static public Result<string, Composition.Component> concat(Composition.Component value) =>
            DecodePineListValue(value)
            .map(list =>
            list.Aggregate(
                seed: Composition.Component.EmptyList,
                func: (aggregate, elem) =>
                elem.BlobContent != null
                ?
                (aggregate.BlobContent != null
                ?
                Composition.Component.Blob(CommonConversion.Concat(aggregate.BlobContent.Value.Span, elem.BlobContent.Value.Span))
                :
                elem)
                :
                (aggregate.ListContent != null
                ?
                Composition.Component.List(aggregate.ListContent.AddRange(elem.ListContent!))
                :
                elem)));

        static public Result<string, Composition.Component> list_head(Composition.Component value) =>
            DecodePineListValue(value)
            .map(list => list.Count < 1 ? Composition.Component.EmptyList : list[0]);

        static public Result<string, Composition.Component> neg_int(Composition.Component value) =>
            Composition.SignedIntegerFromComponent(value)
            .map(i => Composition.ComponentFromSignedInteger(-i!.Value));

        static public Result<string, Composition.Component> add_int(Composition.Component value) =>
            KernelFunctionExpectingListOfBigIntWithAtLeastOneAndProducingBigInt(
                (firstInt, otherInts) => otherInts.Aggregate(seed: firstInt, func: (aggregate, next) => aggregate + next),
                value);

        static public Result<string, Composition.Component> sub_int(Composition.Component value) =>
            KernelFunctionExpectingListOfBigIntWithAtLeastOneAndProducingBigInt(
                (firstInt, otherInts) => otherInts.Aggregate(seed: firstInt, func: (aggregate, next) => aggregate - next),
                value);

        static public Result<string, Composition.Component> mul_int(Composition.Component value) =>
            KernelFunctionExpectingListOfBigIntWithAtLeastOneAndProducingBigInt(
                (firstInt, otherInts) => otherInts.Aggregate(seed: firstInt, func: (aggregate, next) => aggregate * next),
                value);

        static public Result<string, Composition.Component> div_int(Composition.Component value) =>
            KernelFunctionExpectingListOfBigIntWithAtLeastOneAndProducingBigInt(
                (firstInt, otherInts) => otherInts.Aggregate(seed: firstInt, func: (aggregate, next) => aggregate / next),
                value);

        static public Result<string, Composition.Component> sort_int(Composition.Component value) =>
            KernelFunctionExpectingListOfBigInt(
                ints => Result<string, Composition.Component>.ok(
                    Composition.Component.List(ints.OrderBy(i => i).Select(Composition.ComponentFromSignedInteger).ToImmutableList())),
                value);

        static public Result<string, Composition.Component> look_up_name_in_ListValue(Composition.Component value) =>
            KernelFunctionExpectingExactlyTwoArguments(
                Result<string, Composition.Component>.ok,
                Result<string, Composition.Component>.ok,
                (nameValue, listValue) => lookUpNameInListValue(nameValue, listValue))
            (value).map(foundNamed => Composition.Component.List(ImmutableList.Create(foundNamed)));

        static public Result<string, Composition.Component> lookUpNameInListValue(
            Composition.Component nameValue, Composition.Component listValue) =>
             DecodePineListValue(listValue)
             .andThen(list =>
             {
                 var listNamedEntries =
                 list.SelectMany(element =>
                 element.ListContent?.Count == 2
                 ?
                 new[] { (name: element.ListContent[0], named: element.ListContent[1]) }
                 :
                 Array.Empty<(Composition.Component name, Composition.Component named)>());

                 foreach (var namedEntry in listNamedEntries)
                 {
                     if (namedEntry.name == nameValue)
                         return Result<string, Composition.Component>.ok(namedEntry.named);
                 }

                 var nameAsString = Composition.StringFromComponent(nameValue)?.Ok ?? "name_is_not_a_string";

                 var names =
                 listNamedEntries
                 .Select((namedEntry, _) => Composition.StringFromComponent(namedEntry.name)?.Ok)
                 .WhereNotNull().ToImmutableArray();

                 return Result<string, Composition.Component>.err(
                     "Did not find '" + nameAsString + "'. There are " +
                     list.Count + " entries and " +
                     names.Length + " names available in that scope: " +
                     string.Join(", ", names));
             });

        static Result<string, Composition.Component> KernelFunctionExpectingListOfBigIntWithAtLeastOneAndProducingBigInt(
            Func<BigInteger, IReadOnlyList<BigInteger>, BigInteger> aggregate,
            Composition.Component value) =>
            KernelFunctionExpectingListOfBigInt(
                aggregate:
                listOfIntegers =>
                listOfIntegers.Count < 1
                ?
                Result<string, Composition.Component>.err("List is empty. Expected at least one element")
                :
                Result<string, Composition.Component>.ok(
                    Composition.ComponentFromSignedInteger(aggregate(listOfIntegers[0], listOfIntegers.Skip(1).ToImmutableArray()))),
                value);

        static Result<string, Composition.Component> KernelFunctionExpectingListOfBigInt(
            Func<IReadOnlyList<BigInteger>, Result<string, Composition.Component>> aggregate,
            Composition.Component value) =>
            DecodePineListValue(value)
            .andThen(list => ResultListMapCombine(list, Composition.SignedIntegerFromComponent))
            .andThen(ints => aggregate(ints.Select(n => n!.Value).ToImmutableArray()));

        static Func<Composition.Component, Result<string, Composition.Component>> KernelFunctionExpectingExactlyTwoArguments<ArgA, ArgB>(
            Func<Composition.Component, Result<string, ArgA>> decodeArgA,
            Func<Composition.Component, Result<string, ArgB>> decodeArgB,
            Func<ArgA, ArgB, Result<string, Composition.Component>> compose) =>
            value => DecodePineListValue(value)
            .andThen(DecodeListWithExactlyTwoElements)
            .andThen(argsValues =>
            decodeArgA(argsValues!.Value.Item1)
            .andThen(argA =>
            decodeArgB(argsValues.Value.Item2)
            .andThen(argB => compose(argA, argB))));

        static Result<string, Composition.Component> KernelFunctionExpectingListOfTypeBool(
            Func<IReadOnlyList<bool>, bool> compose,
            Composition.Component value) =>
            DecodePineListValue(value)
            .andThen(list => ResultListMapCombine(list, decodeBoolFromValue))
            .map(bools => compose(bools.Select(b => b!.Value).ToImmutableList()))
            .map(valueFromBool);
    }
}

public record Expression(
    Composition.Component? LiteralExpression = null,
    ImmutableArray<Expression>? ListExpression = null,
    ApplicationExpressionStructure? ApplicationExpression = null,
    KernelApplicationExpressionStructure? KernelApplicationExpression = null,
    ConditionalExpressionStructure? ConditionalExpression = null,
    object? ApplicationArgumentExpression = null,
    StringTagExpressionStructure? StringTagExpression = null);

public record ApplicationExpressionStructure(
    Expression function,
    Expression argument);

public record KernelApplicationExpressionStructure(
    string functionName,
    Expression argument);

public record ConditionalExpressionStructure(
    Expression condition,
    Expression ifTrue,
    Expression ifFalse);

public record StringTagExpressionStructure(
    string tag,
    Expression tagged);
